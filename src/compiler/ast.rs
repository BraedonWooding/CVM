/*
 * The AST representation for CVM
 * Based (semi loosely) of the EBNF
 * Note how I've effectively flattened everything,
   this makes analysis much easier since there is less
   redirection!  Downside being it is entirely on the parser
   to ensure the expressions are valid we get less safety
 */

use crate::compiler::*;
use scope::Scope;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

pub type DeclList = Vec<Decl>;

#[derive(Debug, Clone)]
pub struct Program {
    pub functions: HashMap<Ident, Function>,
    pub structs: HashMap<Ident, Struct>,
    pub top_scope: Rc<RefCell<Scope>>,
    pub filename: String
}

#[derive(Debug, Clone, Default)]
pub struct Block {
    pub scope: Rc<RefCell<Scope>>,
    pub statements: Vec<Statement>
}

#[derive(Debug, Clone)]
pub struct Function {
    pub gen_args: IdentList,
    pub name: Ident,
    pub args: DeclList,
    pub ret: ParsedType,
    // short hand => is converted to a return block
    pub block: Block,
}

#[derive(Debug, Clone)]
pub struct Lambda {
    pub args: DeclList,
    pub block: Block,
    pub ret: ParsedType
}

#[derive(Default, Debug, Clone)]
pub struct Struct {
    pub id: Ident,
    pub gen_args: IdentList,
    pub is_list: IdentList,
    pub decls: DeclList,
}

#[derive(Debug, Clone)]
pub struct Decl {
    pub name: Ident,
    pub decl_type: Option<ParsedType>,
    pub val: Option<Expr>
}

#[derive(Debug, Clone)]
pub enum Statement {
    If{if_cond: Box<Expr>, if_block: Block, else_if: Vec<(Expr, Block)>, else_block: Option<Block>},
    While(Box<Expr>, Block),
    For(Option<Box<Expr>>, Option<Box<Expr>>, Option<Box<Expr>>, Block),
    Expr(Expr),
    Return(Expr),
    Defer // not used really...
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub type_annot: Option<ParsedType>
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

#[derive(Debug, EnumAsInner, Clone)]
pub enum Initialiser {
    Key{key: Ident, val: Vec<Initialiser>},
    Val{val: Expr}
}

#[derive(Debug, EnumAsInner, Clone)]
pub enum ExprKind {
    Assign{lhs: Box<Expr>, rhs: Box<Expr>, kind: AssignmentKind},
    Decl(Box<Decl>),
    New(ParsedType, Option<Box<Expr>>, Vec<Initialiser>),
    Unary(Vec<UnaryKind>, Box<Expr>),
    Paren(Box<Expr>),
    Var(Ident),
    Member(Box<Expr>, Ident),
    // NOTE: you can't call lambdas this way
    GenFuncCall(Ident, Vec<ParsedType>, Vec<Expr>),
    FuncCall(Box<Expr>, Vec<Expr>),
    Cast{to: ParsedType, from: ParsedType, obj: Box<Expr>},
    Index(Box<Expr>, Box<Expr>),
    Sizeof(ParsedType, Option<Box<Expr>>),
    Binop(Box<Expr>, BinopKind, Box<Expr>),
    Ternary{cond: Box<Expr>, if_true: Box<Expr>, if_false: Box<Expr>},
    Constant(ConstantKind),
    // honestly we could probably remove this...
    Let(Box<Expr>),
    Lambda(Lambda),
    Uninitialiser,
}

#[derive(Debug, EnumAsInner, Clone)]
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

#[derive(Debug, EnumAsInner, Clone)]
pub enum ParsedType {
    Pointer(Box<ParsedType>),
    Array {inner: Box<ParsedType>, len: Box<Expr>},
    Var {id: Ident, gen_args: Vec<ParsedType>},
    Fresh {id: usize},
    // we don't care about the ids for function types
    // but we do care about the function name.
    Func {args: Vec<ParsedType>, ret: Box<ParsedType>, gen_args: Vec<Ident>}
}

impl ParsedType {
    pub fn new_simple_var_type(id: &str) -> ParsedType {
        ParsedType::Var{id: Ident::new(id.to_string(), Span::default()), gen_args: vec![]}
    }
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum UnaryKind {
    BitNot,
    Not,
    Deref,
    Address,
    Pos,
    Neg
}

#[derive(Debug, Clone)]
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

