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

#[derive(Default, Debug)]
pub struct Program {
    pub top_level: Vec<TopLevel>,
}

#[derive(Default, Debug)]
pub struct Block {
    pub exprs: Vec<Statement>
}

#[derive(Debug)]
pub struct Function {
    pub gen_args: IdentList,
    pub name: Ident,
    pub args: DeclList,
    pub ret: Option<Type>,
    // short hand => is converted to a return block
    pub block: Block,
}

#[derive(Debug)]
pub struct Lambda {
    pub args: DeclList,
    pub block: Block,
}

#[derive(Default, Debug)]
pub struct Struct {
    pub id: Ident,
    pub gen_args: IdentList,
    pub is_list: IdentList,
    pub decls: DeclList,
}

#[derive(Debug)]
pub struct Decl {
    pub id: Ident,
    pub decl_type: Option<Type>,
    pub val: Option<Expr>
}

#[derive(Debug)]
pub enum Statement {
    If{if_cond: Box<Expr>, if_block: Block, else_if: Vec<(Expr, Block)>, else_block: Option<Block>},
    While(Box<Expr>, Block),
    For(Option<Box<Expr>>, Option<Box<Expr>>, Option<Box<Expr>>, Block),
    Defer(Block),
    Expr(Expr),
    Return(Expr)
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub type_annot: Option<Type>
}

impl Expr {
    pub fn is_unary(&self) -> bool {
        match self.kind {
            ExprKind::Unary(..) => true,
            _ => self.is_atom(),
        }
    }

    pub fn is_atom(&self) -> bool {
        match self.kind {
            ExprKind::Paren(..) => true,
            ExprKind::Var(..) => true,
            ExprKind::New(..) => true,
            ExprKind::Cast{..} => true,
            ExprKind::FuncCall(..) => true,
            ExprKind::Index(..) => true,
            ExprKind::Sizeof(..) => true,
            ExprKind::Constant(..) => true,
            ExprKind::Uninitialiser => true,
            ExprKind::Let(..) => true,
            _ => false
        }
    }
}

#[derive(Debug, EnumAsInner)]
pub enum Initialiser {
    Key{key: Ident, val: Vec<Initialiser>},
    Val{val: Expr}
}

#[derive(Debug, EnumAsInner)]
pub enum ExprKind {
    Assign{lhs: Box<Expr>, rhs: Box<Expr>, kind: AssignmentKind},
    Decl(Box<Decl>),
    New(Option<Type>, Option<Box<Expr>>, Vec<Initialiser>),
    Unary(Vec<UnaryKind>, Box<Expr>),
    Paren(Box<Expr>),
    Var(Ident),
    Member(Box<Expr>, Ident),
    GenFuncCall(Ident, Vec<Type>, Vec<Expr>),
    FuncCall(Box<Expr>, Vec<Expr>),
    Cast{to: Option<Type>, from: Option<Type>, obj: Box<Expr>},
    Index(Box<Expr>, Box<Expr>),
    Sizeof(Vec<Type>, Option<Box<Expr>>),
    Binop(Box<Expr>, BinopKind, Box<Expr>),
    Ternary{cond: Box<Expr>, if_true: Box<Expr>, if_false: Box<Expr>},
    Constant(ConstantKind),
    // honestly we could probably remove this...
    Let(Box<Expr>),
    Lambda(Lambda),
    Uninitialiser,
}

#[derive(Debug, EnumAsInner)]
pub enum ConstantKind {
    // only one supported so far
    Int32(i32),
    // add more...
    Flt64(f64),
    // more...
    Str(String),
    Char(char),
    Null,
    Bool(bool),
}

#[derive(Debug, EnumAsInner)]
pub enum Type {
    Pointer(Box<Type>),
    Array {inner: Box<Type>, len: Box<Expr>},
    Var {id: Ident, gen_args: Vec<Type>},
    Fresh {id: usize},

    // we don't care about the ids for function types
    // but we do care about the function name.
    Func {args: Vec<Type>, ret: Option<Box<Type>>, gen_args: Vec<Ident>}
}

#[derive(Debug)]
pub enum BinopKind {
    BitOr,
    BitAnd,
    BitXor,
    BoolAnd,
    BoolOr,
    Equal,
    NotEqual,
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

#[derive(Debug)]
pub enum UnaryKind {
    BitNot,
    Not,
    Deref,
    Address,
    Pos,
    Neg
}

#[derive(Debug)]
pub enum AssignmentKind {
    Assign,
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

#[derive(Debug, EnumAsInner)]
pub enum TopLevel {
    StructDecl(Box<Struct>),
    FuncDecl(Box<Function>),
}

