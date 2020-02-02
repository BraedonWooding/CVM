use crate::logger::*;
use crate::compiler::*;

use lexer::*;
use ast::*;
use scope::*;

use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

extern crate log;
use log::{warn};

pub struct Parser<'a> {
    it: std::iter::Peekable<Lexer<'a>>,
    stack: ScopeStack
}

macro_rules! expect {
    ($it:expr, $wanted:pat, $err:expr) => {
        match $it.peek() {
            Some(Ok(Token { inner: $wanted, .. })) => {
                if let Some(Ok(ret_token)) = $it.next() {
                    Some(ret_token)
                } else {
                    None
                }
            },
            Some(Ok(Token { inner, .. })) => {
                log_expected_token($err, &inner);
                None
            }
            Some(Err(other)) => panic!("Weird error {:?}", other),
            None => {
                log_expected_token_but_eof($err);
                None
            }
        }
    };
}

macro_rules! parse_or_token {
    ($self:ident, $parse:ident, $or:pat, $err:expr) => {
        if try_expect!($self.it, $or).is_some() {
            None
        } else {
            let expr = $self.$parse()?;
            eat!($self.it, $or, $err);
            Some(expr)
        }
    };
}

macro_rules! peek_expect {
    ($it:expr, $wanted:pat) => {
        match $it.peek() {
            Some(Ok(Token { inner: $wanted, .. })) => true,
            Some(Err(other)) => panic!("Weird error {:?}", other),
            _ => false
        }
    };
}

macro_rules! try_expect {
    ($it:expr, $wanted:pat) => {
        if peek_expect!($it, $wanted) {
            if let Some(Ok(ret_token)) = $it.next() {
                Some(ret_token)
            } else {
                None
            }
        } else {
            None
        }
    };
}

macro_rules! parse_list {
    ($self: ident, $parse:ident, $wanted:pat, $end:pat, $err:expr) => {{
        let mut list = vec![];

        // NOTE: This supports an extra trailing comma
        while !peek_expect!($self.it, $end) {
            list.push($self.$parse()?);
            if try_expect!($self.it, $wanted).is_none() {
                break;
            }
        }

        eat!($self.it, $end, $err);
        list
    }};
}

macro_rules! eat {
    ($it:expr, $wanted:pat, $err:expr) => {
        if expect!($it, $wanted, $err).is_none() { return None; }
    };
}

macro_rules! expect_match {
    ($it:expr, $capture:ident, { $($($key:pat)|* => $action:expr),+; _ => $else_action:expr } ) => {
        match $it.peek() {
            $(
                $(Some(Ok(Token { inner: $key, .. })))|* => match $it.next() {
                    #[allow(unused)]
                    $(Some(Ok($capture @ Token { inner: $key, .. })))|* => $action,
                    // can't occur
                    _ => panic!("Unreachable"),
                },
            )+
            Some(Err(err)) => panic!("Weird error {:?}", err),
            _ => $else_action
        }
    };
}

macro_rules! parse_binop {
    ($self:ident, $lhs_parse:ident, $rhs_parse:ident => $($key:pat)|+) => {{
        let lhs = $self.$rhs_parse()?;
        expect_match!($self.it, tok, {
            $($key)|+ => {
                // a == b || c == 2
                let rhs = $self.$lhs_parse()?;
                let kind = match token_kind_to_binop(&tok) {
                    Some(op) => op,
                    None => {
                        warn!("No binop matching {:?} this is probably internal error", tok);
                        return None;
                    }
                };
                let span = Span::join(&lhs.kind.span, &rhs.kind.span);
                Some(Expr {
                    type_annot: ParsedType::Unknown,
                    kind: Spanned::new(ExprKind::Binop(Box::new(lhs), kind, Box::new(rhs)), span)
                })
            };
            _ => {
                Some(lhs)
            }
        })
    }};
}

fn token_kind_to_assignment(tok: &TokenKind) -> Option<AssignmentKind> {
    match tok {
        TokenKind::Assign => Some(AssignmentKind::Assign),
        TokenKind::MulAssign => Some(AssignmentKind::MulAssign),
        TokenKind::DivAssign => Some(AssignmentKind::DivAssign),
        TokenKind::ModAssign => Some(AssignmentKind::ModAssign),
        TokenKind::AddAssign => Some(AssignmentKind::AddAssign),
        TokenKind::SubAssign => Some(AssignmentKind::SubAssign),
        TokenKind::LShiftAssign => Some(AssignmentKind::ShiftLeftAssign),
        TokenKind::RShiftAssign => Some(AssignmentKind::ShiftRightAssign),
        TokenKind::BitAndAssign => Some(AssignmentKind::BitAndAssign),
        TokenKind::BitXorAssign => Some(AssignmentKind::BitXorAssign),
        TokenKind::BitOrAssign => Some(AssignmentKind::BitOrAssign),
        _ => None
    }
}

fn token_kind_to_binop(tok: &TokenKind) -> Option<BinopKind> {
    match tok {
        TokenKind::BitOr => Some(BinopKind::BitOr),
        TokenKind::BitAnd => Some(BinopKind::BitAnd),
        TokenKind::BitXor => Some(BinopKind::BitXor),
        TokenKind::BoolAnd => Some(BinopKind::BoolAnd),
        TokenKind::BoolOr => Some(BinopKind::BoolOr),
        TokenKind::LAngle => Some(BinopKind::LessThan),
        TokenKind::RAngle => Some(BinopKind::GreaterThan),
        TokenKind::LessEqual => Some(BinopKind::LessEqual),
        TokenKind::GreaterEqual => Some(BinopKind::GreaterEqual),
        TokenKind::RShift => Some(BinopKind::ShiftRight),
        TokenKind::LShift => Some(BinopKind::ShiftLeft),
        TokenKind::Add => Some(BinopKind::Add),
        TokenKind::Sub => Some(BinopKind::Sub),
        TokenKind::Asterix => Some(BinopKind::Mul),
        TokenKind::Div => Some(BinopKind::Div),
        TokenKind::Mod => Some(BinopKind::Mod),
        TokenKind::Equal => Some(BinopKind::Equal),
        TokenKind::NotEqual => Some(BinopKind::NotEqual),
        _ => None,
    }
}

fn token_kind_to_unary(tok: &TokenKind) -> Option<UnaryKind> {
    match tok {
        TokenKind::Not => Some(UnaryKind::Not),
        TokenKind::Asterix => Some(UnaryKind::Deref),
        TokenKind::BitAnd => Some(UnaryKind::Address),
        TokenKind::Add => Some(UnaryKind::Pos),
        TokenKind::Sub => Some(UnaryKind::Neg),
        TokenKind::BitNot => Some(UnaryKind::BitNot),
        _ => None
    }
}

#[derive(Debug)]
enum TopLevel {
    StructDecl(Struct),
    FuncDecl(Function)
}

impl<'a> Parser<'a> {
    pub fn parse_program(stream: Lexer<'a>, filename: &str) -> (Option<Program>, ScopeStack) {
        let mut program = Program {
            top_scope: Rc::new(RefCell::new(Scope::default())),
            structs: HashMap::new(),
            functions: HashMap::new(),
            filename: filename.to_string()
        };
        let mut parser = Parser::<'a> {
            it: stream.peekable(),
            stack: ScopeStack::new(&program.top_scope)
        };
        while parser.it.peek().is_some() {
            match parser.parse_top_level() {
                Some(TopLevel::StructDecl(decl)) => { program.structs.insert(decl.id.clone(), decl); },
                Some(TopLevel::FuncDecl(decl)) => { program.functions.insert(decl.id.clone(), decl); },
                None => return (None, parser.stack)
            }
        }
        (Some(program), parser.stack)
    }

    fn parse_top_level(&mut self) -> Option<TopLevel> {
        expect_match!(self.it, _tok, {
            TokenKind::Function => Some(TopLevel::FuncDecl(self.parse_function()?)),
            TokenKind::Struct => Some(TopLevel::StructDecl(self.parse_struct()?));
            _ => {
                warn!("Unknown top level... {:?}", self.it.peek());
                return None;
            }
        })
    }

    fn parse_if(&mut self) -> Option<Statement> {
        self.stack.push_new();
        let if_cond = Box::new(self.parse_conditional()?);
        let if_block = self.parse_block(false)?;
        self.stack.pop();
        let mut else_if = vec![];
        let mut else_block = None;

        while try_expect!(self.it, TokenKind::Else).is_some() {
            if try_expect!(self.it, TokenKind::If).is_some() {
                self.stack.push_new();
                let cond = self.parse_conditional()?;
                let block = self.parse_block(false)?;
                else_if.push((cond, block));
                self.stack.pop();
            } else {
                else_block = Some(self.parse_block(true)?);
                break;
            }
        };

        Some(Statement::If{if_cond, if_block, else_if, else_block})
    }

    fn parse_while(&mut self) -> Option<Statement> {
        self.stack.push_new();
        let cond = self.parse_conditional()?;
        let block = self.parse_block(false)?;
        self.stack.pop();

        Some(Statement::While(Box::new(cond), block))
    }

    fn parse_defer(&mut self) -> Option<Statement> {
        match self.parse_block(true) {
            Some(block) => {
                self.stack.cur().borrow_mut().defer_exprs.push(block);
                Some(Statement::Defer)
            }
            None => None
        }
    }

    fn parse_for(&mut self) -> Option<Statement> {
        self.stack.push_new();
        let init = parse_or_token!(self, parse_expr, TokenKind::SemiColon, "';'").map(Box::new);
        let cond = parse_or_token!(self, parse_conditional, TokenKind::SemiColon, "';'").map(Box::new);
        let step = if peek_expect!(self.it, TokenKind::LBrace) { None }
                   else { Some(Box::new(self.parse_expr()?)) };
        let block = self.parse_block(false)?;
        self.stack.pop();

        Some(Statement::For(init, cond, step, block))
    }

    fn parse_uninit_or_expr(&mut self) -> Option<Expr> {
        Some(match try_expect!(self.it, TokenKind::Uninitialised) {
            Some(tok) => Expr {
                // This isn't a special type it'll be given a fresh one
                // in the type_infer
                type_annot: ParsedType::Unknown,
                kind: Spanned::new(ExprKind::Uninitialiser, tok.span)
            },
            None => self.parse_conditional()?,
        })
    }

    fn parse_expr(&mut self) -> Option<Expr> {
        let lhs = self.parse_conditional()?;
        if lhs.is_unary() {
            // possible declaration
            if try_expect!(self.it, TokenKind::Colon).is_some() {
                let id = match lhs.kind {
                    Spanned { inner: ExprKind::Var(id), .. } => id,
                    kind => {
                        warn!("You can't declare a non variable {:?}", kind);
                        return None;
                    }
                };

                let (mut ty, has_type) = if !peek_expect!(self.it, TokenKind::Assign) {
                    (self.parse_type()?, true)
                } else {
                    (ParsedType::Unknown, false)
                };

                // we have to write an the variable here
                // because else the value can't refer to itself
                // which blocks recursive lambdas
                ty = self.stack.cur().borrow_mut().new_var(id.clone(), Some(ty))
                                                  .expect("Multiple variables with the same name {{TODO}} better msg");
               
                let rhs = if try_expect!(self.it, TokenKind::Assign).is_some() {
                    Some(self.parse_uninit_or_expr()?)
                } else if has_type {
                    None
                } else {
                    warn!("Was expecting and/or type/value i.e. a := 2 or a : int for name {:?}", id);
                    return None;
                };

                // TODO: @TypeSpan this should use the type span
                //       but type spans aren't currently implemented
                let span = match rhs {
                    Some(ref rhs) => Span::join(&id.span, &rhs.kind.span),
                    None => id.span.clone()
                };
                Some(Expr { type_annot: ParsedType::Unknown,
                    kind: Spanned::new(ExprKind::Decl(Box::new(Decl {
                        id: id,
                        decl_type: ty,
                        val: rhs
                    })), span)})
            } else {
                // assignment i.e. +=
                let kind = match self.it.peek() {
                    Some(Ok(tok)) if tok.is_assignment() => {
                        match token_kind_to_assignment(&self.it.next()?.unwrap()) {
                            Some(tok) => tok,
                            None => {
                                warn!("Invalid assignment token... this is an internal error");
                                return None;
                            }
                        }
                    },
                    _ => return Some(lhs)
                };

                // note: you can't put '---' here
                //       that is only for declarations
                let rhs = Box::new(self.parse_conditional()?);

                let span = Span::join(&lhs.kind.span, &rhs.kind.span);
                Some(Expr {
                    type_annot: ParsedType::Unknown,
                    kind: Spanned::new(ExprKind::Assign {
                        lhs: Box::new(lhs),
                        rhs: rhs,
                        kind: kind
                    }, span)
                })
            }
        } else {
            Some(lhs)
        }
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        expect_match!(self.it, _tok, {
            TokenKind::If => return self.parse_if(),
            TokenKind::While => return self.parse_while(),
            TokenKind::Defer => return self.parse_defer(),
            TokenKind::For => return self.parse_for();
            _ => {}
        });

        let is_return = try_expect!(self.it, TokenKind::Return).is_some();
        let inner = self.parse_expr()?;
        if is_return {
            // TODO: Fix error to be nicer
            if let ExprKind::Assign{..} = *inner.kind {
                warn!("Can't return a non conditional (i.e. don't return assignments)");
                return None;
            }
        }

        // all statements that aren't if/for/while/defer... require a semicolon
        eat!(self.it, TokenKind::SemiColon, "';'");
        Some(if is_return { Statement::Return(inner) } else { Statement::Expr(inner) })
    }

    fn parse_block(&mut self, create_scope: bool) -> Option<Block> {
        eat!(self.it, TokenKind::LBrace, "'{'");
        let mut list = vec![];

        if create_scope {
            self.stack.push_new();
        }

        // NOTE: This supports an extra trailing comma
        while !peek_expect!(self.it, TokenKind::RBrace) {
            list.push(self.parse_statement()?);
        }

        eat!(self.it, TokenKind::RBrace, "'}'");
        let scope = Rc::clone(&self.stack.cur());
        if create_scope {
            self.stack.pop();
        }
        Some(Block { scope: scope, statements: list })
    }

    fn parse_id(&mut self) -> Option<Ident> {
        if let Some(tok) = try_expect!(self.it, TokenKind::Ident(_)) {
            Some(tok.transform(|x| x.into_ident().unwrap()))
        } else {
            None
        }
    }

    fn parse_type(&mut self) -> Option<ParsedType> {
        let mut ty = expect_match!(self.it, tok, {
            TokenKind::Asterix => {
                // pointer type
                ParsedType::Pointer(Box::new(self.parse_type()?))
            },
            TokenKind::LParen => {
                let ty = self.parse_type()?;
                eat!(self.it, TokenKind::RParen, "')'");
                // @NOTE: we remove the parentheses in AST
                //        this is because to transpile it to a C type
                //        the parentheses are really annoying...
                ty
            },
            TokenKind::Ident(_) => {
                let gen_args = if try_expect!(self.it, TokenKind::LAngle).is_some() {
                    parse_list!(self, parse_type, TokenKind::Comma, TokenKind::RAngle, "'>'")
                } else {
                    vec![]
                };
                ParsedType::Var{id: tok.transform(|x| x.into_ident().unwrap()), gen_args}
            },
            TokenKind::Function => {
                let gen_args = if try_expect!(self.it, TokenKind::LAngle).is_some() {
                    parse_list!(self, parse_id, TokenKind::Comma, TokenKind::RAngle, "'>'")
                } else {
                    vec![]
                };

                eat!(self.it, TokenKind::LParen, "'('");
                let args = parse_list!(self, parse_type, TokenKind::Comma, TokenKind::RParen, "')'");
                let ret = if try_expect!(self.it, TokenKind::Arrow).is_some() {
                    Box::new(self.parse_type()?)
                } else {
                    Box::new(ScopeStack::new_fresh_type())
                };

                ParsedType::Func{args, ret, gen_args}
            };
            _ => {
                warn!("Was expecting a type but instead found (TODO: put token found here)");
                return None;
            }
        });

        while try_expect!(self.it, TokenKind::LBracket).is_some() {
            let len = self.parse_conditional()?;
            eat!(self.it, TokenKind::RBracket, "']'");
            ty = ParsedType::Array{inner: Box::new(ty), len: Box::new(len)};
        }

        Some(ty)
    }

    // NOTE: Unlike in the EBNF we remove all ideas of expression nesting
    //       It makes analysis much easier
    fn parse_conditional(&mut self) -> Option<Expr> {
        let lhs = self.parse_logical_or()?;
        // TODO: Ternary
        Some(lhs)
    }

    fn parse_logical_or(&mut self) -> Option<Expr> {
        parse_binop!(self, parse_logical_or, parse_logical_and
            => TokenKind::BoolOr)
    }

    fn parse_logical_and(&mut self) -> Option<Expr> {
        parse_binop!(self, parse_logical_and, parse_equality
            => TokenKind::BoolAnd)
    }

    fn parse_equality(&mut self) -> Option<Expr> {
        parse_binop!(self, parse_equality, parse_relational
            => TokenKind::Equal | TokenKind::NotEqual)
    }

    fn parse_relational(&mut self) -> Option<Expr> {
        parse_binop!(self, parse_relational, parse_bitwise_or
            => TokenKind::LessEqual | TokenKind::LAngle | TokenKind::RAngle |
               TokenKind::GreaterEqual)
    }

    fn parse_bitwise_or(&mut self) -> Option<Expr> {
        parse_binop!(self, parse_bitwise_or, parse_bitwise_xor
            => TokenKind::BitOr)
    }

    fn parse_bitwise_xor(&mut self) -> Option<Expr> {
        parse_binop!(self, parse_bitwise_xor, parse_bitwise_and
            => TokenKind::BitXor)
    }

    fn parse_bitwise_and(&mut self) -> Option<Expr> {
        parse_binop!(self, parse_bitwise_and, parse_shift
            => TokenKind::BitAnd)
    }

    fn parse_shift(&mut self) -> Option<Expr> {
        parse_binop!(self, parse_shift, parse_additive
            => TokenKind::LShift | TokenKind::RShift)
    }

    fn parse_additive(&mut self) -> Option<Expr> {
        parse_binop!(self, parse_additive, parse_multiplicative
            => TokenKind::Add | TokenKind::Sub)
    }

    fn parse_multiplicative(&mut self) -> Option<Expr> {
        parse_binop!(self, parse_multiplicative, parse_unary
            => TokenKind::Asterix | TokenKind::Div | TokenKind::Mod)
    }

    fn parse_unary(&mut self) -> Option<Expr> {
        let mut unary_ops = vec![];
        let mut first = None;

        loop {
            match self.it.peek() {
                Some(Ok(c)) if c.is_unary() => {
                    // TODO: This will probably be a bit prone to bugs...
                    if let None = first { first = Some(c.span.clone()); }
                    unary_ops.push(token_kind_to_unary(&self.it.next()?.unwrap())?);
                }
                _ => break
            }
        }

        let rhs = self.parse_atom()?;
        if unary_ops.len() > 0 {
            let span = Span::join(&first.unwrap(), &rhs.kind.span);
            Some(Expr {
                type_annot: ParsedType::Unknown,
                kind: Spanned::new(ExprKind::Unary(unary_ops, Box::new(rhs)),
                                   span)
            })
        } else {
            Some(rhs)
        }
    }

    fn parse_initialiser(&mut self) -> Option<Initialiser> {
        if try_expect!(self.it, TokenKind::Period).is_some() {
            let key = self.parse_id()?;
            eat!(self.it, TokenKind::Assign, "'='");
            let val = if try_expect!(self.it, TokenKind::LBrace).is_some() {
                parse_list!(self, parse_initialiser, TokenKind::Comma, TokenKind::RBrace, "'}'")
            } else {
                vec![Initialiser::Val{val: self.parse_conditional()?}]
            };

            Some(Initialiser::Key{key, val})
        } else {
            Some(Initialiser::Val{val: self.parse_conditional()?})
        }
    }

    fn parse_gentype_list(&mut self) -> Option<Vec<ParsedType>> {
        Some(if try_expect!(self.it, TokenKind::Period).is_some() {
            eat!(self.it, TokenKind::LAngle, "'<'");
            parse_list!(self, parse_type, TokenKind::Comma, TokenKind::RAngle, "'>'")
        } else {
            vec![]
        })
    }

    fn parse_atom(&mut self) -> Option<Expr> {
        let lhs = expect_match!(self.it, tok, {
            TokenKind::LParen => {
                let inner = Box::new(self.parse_conditional()?);
                eat!(self.it, TokenKind::RParen, "')'");
                let span = inner.kind.span.clone();
                // There is no point really finding the full range including
                // parentheses since any errors that we'll present will be about
                // the inner object not the parenthesised object
                // (excluding missing parentheses which is handled elsewhere)
                Expr {
                    type_annot: ParsedType::Unknown,
                    kind: Spanned::new(ExprKind::Paren(inner), span)
                }
            },
            TokenKind::Ident(_) => {
                let id = tok.transform(|x| x.into_ident().unwrap());
                let span = id.span.clone();
                Expr {
                    type_annot: ParsedType::Unknown,
                    kind: Spanned::new(ExprKind::Var(id), span)
                }
            },
            TokenKind::Sizeof => {
                // sizeof.<T>(t)
                let mut gen_args = self.parse_gentype_list()?;
                let ty = gen_args.pop().unwrap_or_else(ScopeStack::new_fresh_type);
                if gen_args.len() > 0 {
                    warn!("'sizeof' takes up to 1 type argument (the type of it's object)");
                    return None;
                }

                eat!(self.it, TokenKind::LParen, "'('");
                let mut args = parse_list!(self, parse_conditional, TokenKind::Comma, TokenKind::RParen, "')'");
                if args.len() > 1 {
                    warn!("'sizeof' takes up to a single function argument (object to get sizeof)");
                    return None;
                }
                let obj = args.pop().map(Box::new);
                // TODO: @BUILTIN FUNC SPAN: This is probably wrong
                //       sizeof.<T>(t) should contain the entire length
                //       maybe we need an easier way to handle this
                //       when we parse a list?  (i.e. also return the span?)
                Expr {
                    type_annot: ParsedType::Unknown,
                    kind: Spanned::new(ExprKind::Sizeof(ty, obj), tok.span)
                }
            },
            TokenKind::New => {
                let mut gen_args = self.parse_gentype_list()?;
                let ty = gen_args.pop().unwrap_or_else(ScopeStack::new_fresh_type);
                if gen_args.len() > 0 {
                    warn!("'new' only takes a single type argument (type to allocate)");
                    return None;
                }

                eat!(self.it, TokenKind::LParen, "'('");
                let mut args = parse_list!(self, parse_conditional, TokenKind::Comma, TokenKind::RParen, "')'");
                if args.len() > 1 {
                    warn!("'new' only takes a single function argument (allocator)");
                    return None;
                }
                let arg = args.pop().map(Box::new);

                let init = if try_expect!(self.it, TokenKind::LBrace).is_some() {
                    parse_list!(self, parse_initialiser, TokenKind::Comma, TokenKind::RBrace, "'}'")
                } else {
                    vec![]
                };

                // TODO: @BUILTIN FUNC SPAN: This is probably wrong
                // @SEE Sizeof
                //       sizeof.<T>(t) should contain the entire length
                //       maybe we need an easier way to handle this
                //       when we parse a list?  (i.e. also return the span?)
                Expr {
                    type_annot: ParsedType::Unknown,
                    kind: Spanned::new(ExprKind::New(ty, arg, init), tok.span)
                }
            },
            TokenKind::Cast => {
                // just a function ... like cast.<Out, In>(in: In)
                let mut gen_args = self.parse_gentype_list()?;
                let to = gen_args.pop().unwrap_or_else(ScopeStack::new_fresh_type);
                let from = gen_args.pop().unwrap_or_else(ScopeStack::new_fresh_type);
                if gen_args.len() > 0 {
                    warn!("'cast' takes up to 2 type arguments (to and from)");
                    return None;
                }

                eat!(self.it, TokenKind::LParen, "'('");
                let mut args = parse_list!(self, parse_conditional, TokenKind::Comma, TokenKind::RParen, "')'");
                if args.len() != 1 {
                    warn!("'cast' requires a single function argument (object to cast)");
                    return None;
                }
                let obj = Box::new(args.pop().unwrap());

                // TODO: @BUILTIN FUNC SPAN: This is probably wrong
                // @SEE Sizeof
                //       sizeof.<T>(t) should contain the entire length
                //       maybe we need an easier way to handle this
                //       when we parse a list?  (i.e. also return the span?)
                Expr {
                    type_annot: ParsedType::Unknown,
                    kind: Spanned::new(ExprKind::Cast{ to, from, obj }, tok.span)
                }
            },
            TokenKind::Number(..) | TokenKind::Bool(..) | TokenKind::Str(..) |
            TokenKind::Character(..) | TokenKind::Null => {
                ConstantKind::from_token(tok)
            },
            TokenKind::Let => {
                let inner = Box::new(self.parse_expr()?);
                let span = inner.kind.span.clone();
                Expr {
                    type_annot: ParsedType::Unknown,
                    kind: Spanned::new(ExprKind::Let(inner), span)
                }
            },
            TokenKind::Lambda => {
                // idk instead of 'name' ill use the 'fn' keyword for pointing
                // at a lambda
                let span = tok.span.clone();
                Expr {
                    type_annot: ParsedType::Unknown,
                    kind: Spanned::new(ExprKind::Lambda(self.parse_lambda()?), span)
                }
            };
            _ => {
                warn!("Unknown Token... tried to parse an atom {:?}", self.it.peek());
                return None;
            }
        });

        // all of our atom op other can be determined based on op
        // so we'll just write a loop that performs ops over and over again
        // till it is done.
        let mut res = lhs;

        res = loop {
            let tmp = Expr { type_annot: ParsedType::Unknown, kind: res.kind };
            res.kind = expect_match!(self.it, tok, {
                TokenKind::Period => {
                    // member access or function call
                    expect_match!(self.it, tok, {
                        TokenKind::Ident(..) => {
                            let ident = tok.transform(|x| x.into_ident().unwrap());
                            let joint = Span::join(&tmp.kind.span, &ident.span);
                            Spanned::new(ExprKind::Member(Box::new(tmp), ident), joint)
                        },
                        TokenKind::LAngle => {
                            let id = match tmp.kind.inner {
                                ExprKind::Var(id) => id,
                                _ => {
                                    warn!("You can only use a generic call through an identifier {:?}", tok);
                                    return None;
                                }
                            };
                            let gen_args = self.parse_gentype_list()?;
                            eat!(self.it, TokenKind::LParen, "'('");
                            let args = parse_list!(self, parse_conditional, TokenKind::Comma, TokenKind::RParen, "')'");
                            let span = id.span.clone();
                            Spanned::new(ExprKind::GenFuncCall(id, gen_args, args), span)
                        };
                        _ => {
                            warn!("Invalid member access / function call {:?}", self.it.peek());
                            return None;
                        }
                    })
                },
                TokenKind::LParen => {
                    // just a function call
                    // TODO: Probably should expand to whole length
                    let mut args = parse_list!(self, parse_conditional, TokenKind::Comma, TokenKind::RParen, "')'");
                    let span = tmp.kind.span.clone();
                    Spanned::new(ExprKind::FuncCall(Box::new(tmp), args), span)
                },
                TokenKind::LBracket => {
                    // index
                    let expr = self.parse_conditional()?;
                    eat!(self.it, TokenKind::RBracket, "']'");
                    let span = tmp.kind.span.clone();
                    Spanned::new(ExprKind::Index(Box::new(tmp), Box::new(expr)), span)
                };
                _ => break tmp
            });
        };

        Some(res)
    }

    fn parse_decl(&mut self) -> Option<Decl> {
        let id = self.parse_id()?;
        // TODO: Should we force it to be a single token
        //       in the case of infer assignment i.e. don't allow id ': =' 2
        eat!(self.it, TokenKind::Colon, "':'");
        let decl_type = if !peek_expect!(self.it, TokenKind::Assign) {
            self.parse_type()?
        } else {
            ParsedType::Unknown
        };

        let val = if try_expect!(self.it, TokenKind::Assign).is_some() {
            Some(self.parse_conditional()?)
        } else {
            None
        };

        return Some(Decl {
            id: id,
            decl_type: decl_type,
            val: val
        })
    }

    fn parse_struct(&mut self) -> Option<Struct> {
        let id = if let Some(id) = expect!(self.it, TokenKind::Ident(_), "id") {
            id.transform(|x| x.into_ident().unwrap())
        } else {
            return None;
        };

        let gen_args = if try_expect!(self.it, TokenKind::LAngle).is_some() {
            parse_list!(self, parse_id, TokenKind::Comma, TokenKind::RAngle, "'>'")
        } else {
            vec![]
        };

        let is_list = if try_expect!(self.it, TokenKind::Is).is_some() {
            // maybe don't force it to be 'is (...)' allow the () to be optional
            eat!(self.it, TokenKind::LParen, "'('");
            parse_list!(self, parse_id, TokenKind::Comma, TokenKind::RParen, "')'")
        } else {
            vec![]
        };

        eat!(self.it, TokenKind::LBrace, "'{'");
        let decls = parse_list!(self, parse_decl, TokenKind::Comma, TokenKind::RBrace, "'}'");
        Some(Struct { id, gen_args, decls, is_list })
    }

    fn parse_opt_decl(&mut self) -> Option<Decl> {
        let id = self.parse_id()?;
        let (decl_type, val) = if try_expect!(self.it, TokenKind::Colon).is_some() {
            let decl_type = if !peek_expect!(self.it, TokenKind::Assign) {
                self.parse_type()?
            } else {
                ParsedType::Unknown
            };

            let val = if try_expect!(self.it, TokenKind::Assign).is_some() {
                Some(self.parse_conditional()?)
            } else {
                None
            };

            (decl_type, val)
        } else {
            (ParsedType::Unknown, None)
        };

        return Some(Decl { id, decl_type, val })
    }

    fn parse_lambda(&mut self) -> Option<Lambda> {
        // lambdas can be recursive (including in arguments)
        // butttt the declaration always pushes a temporary fresh
        // before evaluating the value so we don't have to worry
        self.stack.push_new();
        // reserve ret type
        // the type checker will unify this with all the actual return types
        let ret = ScopeStack::new_fresh_type();

        let mut args = if try_expect!(self.it, TokenKind::LParen).is_some() {
            // arg list
            parse_list!(self, parse_opt_decl, TokenKind::Comma, TokenKind::RParen, "')'")
        } else {
            // single arg
            vec![self.parse_opt_decl()?]
        };

        for arg in args.iter_mut() {
            self.stack.cur().borrow_mut()
                .new_var(arg.id.clone(), Some(arg.decl_type.clone()))
                .expect("Variable already exists with variable name {TODO}");
        }
        
        let block = if try_expect!(self.it, TokenKind::FatArrow).is_some() {
            let cond = self.parse_conditional()?;
            Block { statements: vec![Statement::Return(cond)], scope: Rc::clone(&self.stack.cur()) }
        } else {
            self.parse_block(false)?
        };

        self.stack.pop();
        Some(Lambda { args, block, ret })
    }

    fn parse_function(&mut self) -> Option<Function> {
        let gen_args = if try_expect!(self.it, TokenKind::LAngle).is_some() {
            parse_list!(self, parse_id, TokenKind::Comma, TokenKind::RAngle, "'>'")
        } else {
            vec![]
        };

        let id = self.parse_id()?;

        // since arguments could have default values relating to the function itself
        // i.e. fn fib(n: usize, inner := fib) => n <= 1 ? 1 : inner(n - 1) + inner(n - 2)
        // we have to put it in the public scope as a type variable 'a'
        // then we can concrete it down better afterwards
        // This does introduce type variable pollution so we probably want to at one point
        // introduce a new type variable class called TempTypeVar that is not intended to last longer than
        // a declaration so that we don't have to waste a ton of letters on functions
        let (fresh, fresh_id) = if let ParsedType::Fresh{id} = ScopeStack::new_fresh_type() {
            (ParsedType::Fresh{id}, id)
        } else {
            warn!("Internal error new_fresh_type returned a non fresh type");
            return None
        };
        self.stack.top().borrow_mut().new_var(id.clone(), Some(fresh));

        // push a new scope
        self.stack.push_new();

        eat!(self.it, TokenKind::LParen, "'('");

        // do note that unlike the function id
        // the arguments can't be self referential (i.e. you they can't refer to themselves)
        // so you can't have something like fn fib(n: usize, actual: \n => n <= 1 ? 1 : actual(n - 1) + actual(n - 2) ) => actual(n)
        // this may change in the future but currently isn't allowed
        let mut args = parse_list!(self, parse_decl, TokenKind::Comma, TokenKind::RParen, "')'");
        let ret = if try_expect!(self.it, TokenKind::Arrow).is_some() {
            self.parse_type()?
        } else {
            ScopeStack::new_fresh_type()
        };

        // We instead push them here onto their scope
        // we probably want to just do this inside a unique parse_decl
        // ...
        let mut arg_types = vec![];
        for arg in args.iter_mut() {
            arg_types.push(self.stack.cur().borrow_mut()
                .new_var(arg.id.clone(), Some(arg.decl_type.clone()))
                .expect("Variable already exists with variable id {TODO}"));
        }

        let block = if try_expect!(self.it, TokenKind::FatArrow).is_some() {
            let cond = self.parse_conditional()?;
            Block { statements: vec![Statement::Return(cond)], scope: Rc::clone(&self.stack.cur()) }
        } else {
            self.parse_block(false)?
        };

        self.stack.pop();

        let func = ParsedType::Func { args: arg_types, ret: Box::new(ret.clone()), gen_args: gen_args.clone() };
        self.stack.set_fresh(fresh_id, func);
        Some(Function { gen_args, id, args, block, ret })
    }
}

