/*
* The AST representation for CVM
* Based (semi loosely) of the EBNF
* Note how I've effectively flattened everything,
  this makes analysis much easier since there is less
  redirection!  Downside being it is entirely on the parser
  to ensure the expressions are valid we get less safety
*/

use crate::*;
use compiler::*;
use lexer::{Postfix, Token, TokenKind};
use scope::Scope;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub type DeclList = Vec<Decl>;

#[derive(Debug, Clone)]
pub struct Program {
    pub functions: HashMap<Ident, Function>,
    pub structs: HashMap<Ident, Struct>,
    pub top_scope: Rc<RefCell<Scope>>,
    pub filename: String,
}

#[derive(Debug, Clone, Default)]
pub struct Block {
    pub scope: Rc<RefCell<Scope>>,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub gen_args: IdentList,
    pub id: Ident,
    pub args: DeclList,
    pub ret: ParsedType,
    // short hand => is converted to a return block
    pub block: Block,
}

#[derive(Debug, Clone)]
pub struct Lambda {
    pub args: DeclList,
    pub block: Block,
    pub ret: ParsedType,
}

#[derive(Default, Debug, Clone)]
pub struct Struct {
    pub id: Ident,
    pub gen_args: IdentList,
    pub is_list: IdentList,
    pub decls: DeclList,
}

impl Struct {
    pub fn find_member(&self, id: &Ident) -> Option<&Decl> {
        self.decls.iter().find(|&x| x.id == *id)
    }
}

#[derive(Debug, Clone)]
pub struct Decl {
    pub id: Ident,
    pub decl_type: ParsedType,
    pub val: Option<Expr>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    If {
        if_cond: Box<Expr>,
        if_block: Block,
        else_if: Vec<(Expr, Block)>,
        else_block: Option<Block>,
    },
    While(Box<Expr>, Block),
    For(
        Option<Box<Expr>>,
        Option<Box<Expr>>,
        Option<Box<Expr>>,
        Block,
    ),
    Expr(Expr),
    Return(Expr),
    Defer, // not used really...
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: Spanned<ExprKind>,
    pub type_annot: ParsedType,
}

impl Expr {
    pub fn is_unary(&self) -> bool {
        match *self.kind {
            ExprKind::Unary(..) => true,
            _ => self.is_atom(),
        }
    }

    pub fn is_atom(&self) -> bool {
        match *self.kind {
            ExprKind::Paren(..) => true,
            ExprKind::Var(..) => true,
            ExprKind::New(..) => true,
            ExprKind::Cast { .. } => true,
            ExprKind::FuncCall(..) => true,
            ExprKind::Index(..) => true,
            ExprKind::Sizeof(..) => true,
            ExprKind::Constant(..) => true,
            ExprKind::Uninitialiser => true,
            ExprKind::Let(..) => true,
            _ => false,
        }
    }
}

#[derive(Debug, EnumAsInner, Clone)]
pub enum Initialiser {
    Key { key: Ident, val: Vec<Initialiser> },
    Val { val: Expr },
}

#[derive(Debug, EnumAsInner, Clone)]
pub enum ExprKind {
    Assign {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        kind: AssignmentKind,
    },
    Decl(Box<Decl>),
    New(ParsedType, Option<Box<Expr>>, Vec<Initialiser>),
    Unary(Vec<UnaryKind>, Box<Expr>),
    Paren(Box<Expr>),
    Var(Ident),
    Member(Box<Expr>, Ident),
    // NOTE: you can't call lambdas this way
    GenFuncCall(Ident, Vec<ParsedType>, Vec<Expr>),
    FuncCall(Box<Expr>, Vec<Expr>),
    Cast {
        to: ParsedType,
        from: ParsedType,
        obj: Box<Expr>,
    },
    Index(Box<Expr>, Box<Expr>),
    Sizeof(ParsedType, Option<Box<Expr>>),
    Binop(Box<Expr>, BinopKind, Box<Expr>),
    Ternary {
        cond: Box<Expr>,
        if_true: Box<Expr>,
        if_false: Box<Expr>,
    },
    Constant(ConstantKind),
    // honestly we could probably remove this...
    Let(Box<Expr>),
    Lambda(Lambda),
    Uninitialiser,
}

impl ConstantKind {
    pub fn from_token(tok: Token) -> Expr {
        let (kind, ty) = match tok.inner {
            TokenKind::Number(num, postfix) =>
            // support all postfixes
            {
                (
                    ExprKind::Constant(match postfix {
                        Postfix::i32 => ConstantKind::Int32(num.parse().unwrap()),
                        Postfix::f64 => ConstantKind::Flt64(num.parse().unwrap()),
                        _ => panic!("Unhandled option"),
                    }),
                    match postfix {
                        Postfix::i32 => create_type!(Var "int"),
                        Postfix::f64 => create_type!(Var "double"),
                        _ => panic!("Unhandled option"),
                    },
                )
            }
            TokenKind::Bool(val) => (
                ExprKind::Constant(ConstantKind::Bool(val)),
                create_type!(Var "bool"),
            ),
            TokenKind::Str(val) =>
            // TODO: This would benefit from our testing macro
            {
                (
                    ExprKind::Constant(ConstantKind::Str(val)),
                    ParsedType::Pointer(Box::new(create_type!(Var "char"))),
                )
            }
            TokenKind::Null =>
            // This should be a generic pointer literal
            {
                (
                    ExprKind::Constant(ConstantKind::Null),
                    ParsedType::Pointer(Box::new(create_type!(Var "u8"))),
                )
            }
            TokenKind::Character(c) => (
                ExprKind::Constant(ConstantKind::Char(c)),
                create_type!(Var "char"),
            ),
            _ => panic!("Unhandled option"),
        };
        Expr {
            type_annot: ty,
            kind: Spanned::new(kind, tok.span),
        }
    }
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
    /// This is used in the cases of type annotations for expressions
    /// (and a few others) basically it means that we'll assign a proper
    /// type later on and don't need to give it a specific fresh id.
    Unknown,
    Pointer(Box<ParsedType>),
    Array {
        inner: Box<ParsedType>,
        len: Box<Expr>,
    },
    Var {
        id: Ident,
        gen_args: Vec<ParsedType>,
    },
    Fresh {
        id: usize,
    },
    // we don't care about the ids for function types
    // but we do care about the function name.
    Func {
        args: Vec<ParsedType>,
        ret: Box<ParsedType>
    },
}

impl std::hash::Hash for ParsedType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            ParsedType::Unknown => panic!("Can't hash an unknown type"),
            ParsedType::Array { inner, .. } | ParsedType::Pointer(inner) => {
                inner.hash(state);
            }
            ParsedType::Var { id, .. } => (*id).hash(state),
            ParsedType::Fresh { id } => id.hash(state),
            ParsedType::Func { args, ret, .. } => {
                args.hash(state);
                ret.hash(state);
            }
        }
    }
}

/// This isn't entirely true...
/// i.e. Unknown =/= Unknown in any context (similar to how f32/f64 can't
/// be hashed in Rust) however to property implement type definitions
/// it would be nice to have this... for now
///
/// TODO: We shouldn't do this... instead we should just handle the special
///       cases as special cases i.e. handle *void as *u8 specially
///       (just use idents for typedefs tbh...)
impl std::cmp::Eq for ParsedType {}

impl std::cmp::PartialEq for ParsedType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ParsedType::Pointer(a), ParsedType::Pointer(b)) => a == b,
            (
                ParsedType::Array { inner: inner_a, .. },
                ParsedType::Array { inner: inner_b, .. },
            ) => inner_a == inner_b,
            // NOTE: Generic arguments are ignored for comparisons
            (ParsedType::Var { id: id_a, .. }, ParsedType::Var { id: id_b, .. }) => id_a == id_b,
            (ParsedType::Fresh { id: id_a }, ParsedType::Fresh { id: id_b }) => id_a == id_b,
            (
                ParsedType::Func {
                    args: args_a,
                    ret: ret_a,
                    ..
                },
                ParsedType::Func {
                    args: args_b,
                    ret: ret_b,
                    ..
                },
            ) => ret_a == ret_b && args_a == args_b,
            // i.e. Unknown != Unknown
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryKind {
    BitNot,
    Not,
    Deref,
    Address,
    Pos,
    Neg,
}

#[derive(Debug, Clone, PartialEq)]
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
    BitOrAssign,
}
