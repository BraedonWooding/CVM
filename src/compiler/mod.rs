pub mod ast;
pub mod lexer;
pub mod parser;
pub mod scope;
pub mod type_check;
pub mod type_infer;

use std::collections::HashMap;

pub type Ident = Spanned<String>;
pub type IdentList = Vec<Ident>;

pub fn string_to_fresh_id(text: &str) -> usize {
    // go from right to left
    // i.e. a => 0, b => 1, ... z => 25
    //      aa => 26, az =>
    let mut num = 0;
    let mut count = 0;

    for c in text.chars() {
        num *= 26;
        count += 1;
        if c > 'z' || c < 'a' {
            panic!("Invalid string to convert to fresh id!");
        }
        num += c as usize - 'a' as usize;
        if count == text.len() {
            break;
        }
        num += 1;
    }

    num
}

pub fn fresh_id_to_string(id: usize) -> String {
    let mut text = String::new();
    let mut num = id;
    loop {
        let c = std::char::from_u32((num % 26) as u32 + b'a' as u32).unwrap();
        num /= 26;
        text.insert(0, c);
        if num == 0 {
            break;
        }
        num -= 1;
    }
    text
}

#[macro_export]
macro_rules! create_type {
    (Var $id:tt $(< $(($($inner: tt)*)),* >)?) => {
        // This monstrosity allows you to optionally pass a series of gen args
        // i.e. create_type!(Var "List" <(Fresh a)>)
        ast::ParsedType::Var{id: Spanned::new($id.to_string(), Span::default()), gen_args: vec![$($(create_type!($($inner)*)),*)?]}
    };
    (Pointer ($($inner:tt)+)) => {
        ast::ParsedType::Pointer(Box::new(create_type!($($inner) +)))
    };
    (Array [$len:expr] ($($inner:tt)+)) => {
        ast::ParsedType::Array{inner: Box::new(create_type!($($inner) +)), len: Box::new($len)}
    };
    // Allows (Fresh a) instead of forcing (Fresh 0)
    // just makes names a tad nicer
    // converts the identifiers to integers (if purely consisting of a-z)
    (Fresh $name:ident) => {
        ast::ParsedType::Fresh { id:string_to_fresh_id(stringify!($name)) }
    };
    (Fresh $id:tt) => {
        ast::ParsedType::Fresh { id:$id }
    };
    (Func $(($($args:tt)+)),* -> ($($ret:tt)+)) => {
        ast::ParsedType::Func{args: vec![$(create_type!($($args)+)),*], ret: Box::new(create_type!($($ret)+))}
    };
}

/// Represents a definition of a primative type or alias
///
/// Weird cases:
/// - *void -> *u8, this is because *void is almost 100% a mistake in C
///   that should have always been *u8 (there is no difference between the types
///   except that *u8 makes the presumption of 8 bit char bit sizes)
///   We allow *void to keep consistency with C but it isn't an actual type
///   (void is though but that is a type with no values and is purely used
///   to allow you to have fields that are removed in some cases).
#[derive(Debug)]
pub enum TypeDefinition {
    // I think alignment has a reasonable maximum of 8 bytes
    // but I'm just gonna be safe and do usize for now :) TODO: This is a waste
    // fix this.
    // Technically integral types only go to like 128 bytes
    // so we could use a u8 as well for size...
    Integral {
        align: usize,
        size: usize,
        signedness: bool,
        c_name: &'static str,
    },
    FloatingPt {
        size: usize,
        align: usize,
        c_name: &'static str,
    },
    /// NOTE: This may have to be an Ident later...
    Alias(String),
}

impl TypeDefinition {
    pub fn type_is_numeric(&self) -> bool {
        if let TypeDefinition::Integral { .. } | TypeDefinition::FloatingPt { .. } = self {
            true
        } else {
            false
        }
    }

    pub fn type_size(&self) -> usize {
        if let TypeDefinition::Integral { size, .. } | TypeDefinition::FloatingPt { size, .. } =
            self
        {
            *size
        } else {
            panic!("Non expanded alias");
        }
    }

    pub fn type_alignment(&self) -> usize {
        if let TypeDefinition::Integral { align, .. } | TypeDefinition::FloatingPt { align, .. } =
            self
        {
            *align
        } else {
            panic!("Non expanded alias");
        }
    }

    // Only valid on integral types
    pub fn type_signedness(&self) -> bool {
        if let TypeDefinition::Integral { signedness, .. } = self {
            *signedness
        } else {
            panic!("Alias is not expanded or you are trying to find signedness on floating point");
        }
    }

    pub fn type_is_integral(&self) -> bool {
        if let TypeDefinition::Integral { .. } = self {
            true
        } else {
            false
        }
    }

    pub fn type_is_float(&self) -> bool {
        if let TypeDefinition::FloatingPt { .. } = self {
            true
        } else {
            false
        }
    }
}

pub struct TypeDefinitionTable(pub HashMap<ast::ParsedType, TypeDefinition>);

impl TypeDefinitionTable {
    fn add_integral<T: num::PrimInt>(
        &mut self,
        id: &'static str,
        aliases: &'static [&'static str],
    ) {
        let align = std::mem::align_of::<T>();
        let size = std::mem::size_of::<T>();
        let signedness = impls!(T: num::Signed);

        self.0.insert(
            create_type!(Var id),
            TypeDefinition::Integral {
                align,
                size,
                signedness,
                c_name: id,
            },
        );

        for alias in aliases {
            self.0.insert(
                create_type!(Var alias),
                TypeDefinition::Alias(id.to_string()),
            );
        }
    }

    fn add_float<T: num::Float>(&mut self, id: &'static str, aliases: &'static [&'static str]) {
        let align = std::mem::align_of::<T>();
        let size = std::mem::size_of::<T>();

        self.0.insert(
            create_type!(Var id),
            TypeDefinition::FloatingPt {
                align,
                size,
                c_name: id,
            },
        );

        for alias in aliases {
            self.0.insert(
                create_type!(Var alias),
                TypeDefinition::Alias(id.to_string()),
            );
        }
    }

    pub fn load_type_definition_table() -> TypeDefinitionTable {
        let mut table = TypeDefinitionTable(HashMap::new());
        table.add_float::<libc::c_float>("float", &["f32"]);
        table.add_float::<libc::c_double>("double", &["f64"]);

        // We allow 'signed_char' just to match (kinda) 'signed char'
        table.add_integral::<libc::c_char>("char", &[]);
        // But we want to produce valid C so we ues int8_t
        // NOTE: We could chuck in signed char here as the C name
        //       so then when it gets used it will expand the name to have the
        //       space but... that just feels hacky and awful
        // TODO: Maybe add an experimental option to do ^^
        table.add_integral::<libc::c_schar>("int8_t", &["schar", "signed_char", "i8"]);
        table.add_integral::<libc::c_uchar>("uint8_t", &["uchar", "unsigned_char", "u8"]);

        table.add_integral::<libc::c_int>("int", &["signed_int"]);
        table.add_integral::<libc::c_long>("long", &["signed_long"]);
        table.add_integral::<libc::c_longlong>("longlong", &["signed_long_long", "long_long"]);
        table.add_integral::<libc::c_short>("short", &["signed_short"]);

        table.add_integral::<libc::c_int>("uint", &["unsigned_int"]);
        table.add_integral::<libc::c_long>("ulong", &["unsigned_long"]);
        table.add_integral::<libc::c_longlong>("ulonglong", &["unsigned_long_long", "ulong_long"]);
        table.add_integral::<libc::c_short>("ushort", &["unsigned_short"]);

        table.add_integral::<libc::uintmax_t>("uintmax_t", &[]);
        table.add_integral::<libc::intmax_t>("intmax_t", &[]);

        // NOTE: Technically this isn't correct!!
        // TODO: Verify if we are fine with this...
        // The reason it isn't correct is because on non linear memory model
        // systems (i.e. pretty old systems with memory region memory)
        // size_t may be 2 bytes (16 bit) but a uintptr_t may be 4 bytes(32 bit)
        // because there are two regions and memory can't cross a region
        // technically size_t is closer to uintmax_t
        table.add_integral::<libc::uintptr_t>("uintptr_t", &["size_t", "usize"]);
        table.add_integral::<libc::intptr_t>("intptr_t", &["ssize_t", "isize"]);

        table.add_integral::<libc::ptrdiff_t>("ptrdiff_t", &[]);

        table.add_integral::<i16>("int16_t", &["i16"]);
        table.add_integral::<i32>("int32_t", &["i32"]);
        table.add_integral::<i64>("int64_t", &["i64"]);
        table.add_integral::<u16>("uint16_t", &["u16"]);
        table.add_integral::<u32>("uint32_t", &["u32"]);
        table.add_integral::<u64>("uint64_t", &["u64"]);

        table
    }
}

impl std::ops::Deref for TypeDefinitionTable {
    type Target = HashMap<ast::ParsedType, TypeDefinition>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Clone, Default, Eq, PartialEq, Hash)]
pub struct Spanned<T> {
    pub span: Span,
    pub inner: T,
}

impl<T> Spanned<T> {
    pub fn new(inner: T, span: Span) -> Spanned<T> {
        Spanned::<T> {
            span: span,
            inner: inner,
        }
    }

    pub fn transform<U, F: FnOnce(T) -> U>(self, capture: F) -> Spanned<U> {
        Spanned::new(capture(self.inner), self.span)
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?} at {:?}", self.inner, self.span)
    }
}

impl<T> std::ops::Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

#[derive(Clone, Default)]
pub struct Span {
    /// Represents the line point of this span
    pub line: usize,
    /// Represents the col point of this
    pub col: usize,
    /// Represents the byte offset range of this span
    pub byte_offset: (usize, usize),
}

impl Span {
    pub fn join(a: &Span, b: &Span) -> Span {
        use std::cmp;
        Span {
            line: cmp::min(a.line, b.line),
            col: cmp::min(a.col, b.col),
            byte_offset: (
                cmp::min(a.byte_offset.0, b.byte_offset.0),
                cmp::max(a.byte_offset.1, b.byte_offset.1),
            ),
        }
    }
}

impl std::cmp::PartialEq for Span {
    /*
     * Note: Span's are always equatable to each other irregardless of value
     *       this is for the benefit of Spanned.
     */
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl std::cmp::Eq for Span {}

impl std::hash::Hash for Span {
    /*
     * Note: Span's shouldn't be used in a hash algorithm since they always
     *       result in an identical hash value.
     *       This is to enable 2 spanned's to be equal
     */
    fn hash<H>(&self, state: &mut H)
    where
        H: std::hash::Hasher,
    {
        state.finish();
    }
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Line {}:{}", self.line, self.col)
    }
}
