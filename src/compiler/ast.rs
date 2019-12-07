/*
 * The AST representation for CVM
 * Based (semi loosely) of the EBNF
 * Note how I've effectively flattened everything,
   this makes analysis much easier since there is less
   redirection!  Downside being it is entirely on the parser
   to ensure the expressions are valid we get less safety
 */

pub type Ident = String;
pub type IdentList = Vec<Ident>;
pub type DeclList = Vec<Decl>;

pub struct Program {
    statements: Vec<TopLevel>,
}

pub struct Block {
    exprs: Vec<Expr>
}

pub struct Function {
    gen_args: IdentList,
    id: Option<Ident>,
    args: DeclList,
    ret_type : Option<Type>,
    // function lambdas are removed
    // as they are parsed so => is converted
    block: Block,
}

pub struct Struct {
    id: Ident,
    gen_args: IdentList,
    is_list: IdentList,
    decls: DeclList,
}

pub struct Decl {
    id: Ident,
    decl_type: Type,
    val: Option<Expr>
}

pub struct Expr {
    is_return: bool,
    kind: ExprKind,
    type_annot: Type
}

pub enum ExprKind {
    Assign{lhs: Box<Expr>, rhs: Box<Expr>, kind: AssignmentKind},
    // TODO: Support initialisation in declarations
    //       or force people to use 'new'
    // TODO: Add uninitialisers
    Decl{lhs: Box<Expr>, lhs_type: Type, rhs: Option<Box<Expr>>},
    New(Type, Initialisation),
    Unary(Vec<UnaryKind>, Box<Expr>),
    Paren(Box<Expr>),
    FuncCall(Box<Expr>, Vec<Expr>),
    Index(Box<Expr>, Box<Expr>),
    Sizeof(Vec<Type>, Option<Box<Expr>>),
    Binop(Box<Expr>, BinopKind, Box<Expr>),
    Ternary{cond: Box<Expr>, if_true: Box<Expr>, if_false: Box<Expr>},
    Constant(ConstantKind),
    // honestly we could probably remove this...
    Let(Box<Expr>),
    Func(Function),
    If{if_cond: Box<Expr>, if_block: Block, else_if: Vec<(Expr, Block)>, else_block: Option<Block>},
    While(Box<Expr>, Block),
    For(Option<Box<Expr>>, Option<Box<Expr>>, Option<Box<Expr>>),
    Defer(Block),
}

pub enum ConstantKind {
    // only one supported so far
    Int32(i32),
    // add more...
    Flt64(f64),
    // more...
    Str(String),
    Char(char),
    Null(),
    Bool(bool),
}

pub enum Type {
    Pointer(Box<Type>),
    Array {inner: Box<Type>, len: Box<Expr>},
    Var {id: Ident, gen_args: Vec<Type>},
    Fresh {id: usize},

    // we don't care about the ids for function types
    // but we do care about the function name.
    Func {name: Ident, args: Vec<Type>, ret: Box<Type>}
}

pub enum BinopKind {
    BitOr,
    BitAnd,
    BitXor,
    BoolAnd,
    BoolOr,
    LessThan,
    GreaterThan,
    LessEqual,
    GreaterEqual,
    ShiftRight,
    ShiftLeft,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

pub struct Initialisation {
    id: Ident,
    val: Conditional,
}

pub enum UnaryKind {
    Not,
    Deref,
    Address,
    Pos,
    Neg,
    Cast(Type)
}

pub enum AssignmentKind {
    MulAssign,
    DivAssign,
    ModAssign,
    AddAssign,
    SubAssign,
    ShiftLeftAssign,
    ShiftRightAssign,
    BitAndAssign,
    BitXorAssign,
    BitOrAssign
}

pub enum TopLevel {
    StructDecl(Box<Struct>),
    FuncDecl(Box<Function>),
}

