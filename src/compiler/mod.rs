pub mod ast;
pub mod lexer;
pub mod parser;
pub mod scope;
pub mod type_check;
pub mod type_infer;

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
        num += (c as usize - 'a' as usize);
        if count == text.len() { break; }
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
    (Var $id:tt) => {
        ParsedType::new_simple_var_type($id)
    };
    (Pointer ($($inner:tt)+)) => {
        ParsedType::Pointer(Box::new(create_type!($($inner) +)))
    };
    (Array [$len:expr] ($($inner:tt)+)) => {
        ParsedType::Array{inner: Box::new(create_type!($($inner) +)), len: Box::new($len)}
    };
    (Fresh $name:ident) => {
        ParsedType::Fresh { id:string_to_fresh_id(stringify!($name)) }
    };
    (Fresh $id:tt) => {
        ParsedType::Fresh { id:$id }
    };
    (Func $(($($args:tt)+)),* -> ($($ret:tt)+)) => {
        ParsedType::Func{args: vec![$(create_type!($($args)+)),*], ret: Box::new(create_type!($($ret)+)), gen_args: vec![]}
    };
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
