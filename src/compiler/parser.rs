use super::lexer::*;
use super::ast::*;
use crate::logger::*;

extern crate log;
use log::{info, trace, warn};

pub struct Parser<'a> {
    it: std::iter::Peekable<Lexer<'a>>,
}

macro_rules! expect {
    ($it:expr, $wanted:pat, $err:expr) => {
        match $it.peek() {
            Some(Ok(Token { kind: $wanted, .. })) => {
                if let Some(Ok(ret_token)) = $it.next() {
                    Some(ret_token)
                } else {
                    None
                }
            },
            Some(Ok(Token { kind, .. })) => {
                log_expected_token($err, &kind);
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
            Some(Ok(Token { kind: $wanted, .. })) => true,
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
                $(Some(Ok(Token { kind: $key, .. })))|* => match $it.next() {
                    #[allow(unused)]
                    $(Some(Ok($capture @ Token { kind: $key, .. })))|* => $action,
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
                let kind = match token_kind_to_binop(&tok.kind) {
                    Some(op) => op,
                    None => {
                        warn!("No binop matching {:?} this is probably internal error", tok.kind);
                        return None;
                    }
                };
                Some(Expr {
                    is_return: false, type_annot: None,
                    kind: ExprKind::Binop(Box::new(lhs), kind, Box::new(rhs))
                })
            };
            _ => {
                info!("No binop {:?} :(... token was {:?}", stringify!($lhs_parse), $self.it.peek());
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
        _ => None
    }
}

impl<'a> Parser<'a> {
    pub fn parse_program(stream: Lexer<'a>) -> Option<Program> {
        let mut parser = Parser::<'a> {
            it: stream.peekable(),
        };
        let mut program = Program::default();
        while parser.it.peek().is_some() {
            program.statements.push(parser.parse_top_level()?);
        }

        Some(program)
    }

    fn parse_top_level(&mut self) -> Option<TopLevel> {
        expect_match!(self.it, _tok, {
            TokenKind::Function => Some(TopLevel::FuncDecl(Box::new(self.parse_function()?))),
            TokenKind::Struct => Some(TopLevel::StructDecl(Box::new(self.parse_struct()?)));
            _ => {
                warn!("Unknown top level... {:?}", self.it.peek());
                return None;
            }
        })
    }

    fn parse_if(&mut self) -> Option<Expr> {
        warn!("IF START");
        let if_cond = Box::new(self.parse_conditional()?);
        warn!("IF: {:?}", if_cond);
        let if_block = self.parse_block()?;
        warn!("IF: {:?}", if_block);
        let mut else_if = vec![];
        let mut else_block = None;

        while try_expect!(self.it, TokenKind::Else).is_some() {
            if try_expect!(self.it, TokenKind::If).is_some() {
                let cond = self.parse_conditional()?;
                let block = self.parse_block()?;
                else_if.push((cond, block));
            } else {
                else_block = Some(self.parse_block()?);
                break;
            }
        };

        Some(Expr { is_return: false, type_annot: None,
            kind: ExprKind::If{if_cond, if_block, else_if, else_block} })
    }

    fn parse_while(&mut self) -> Option<Expr> {
        let cond = self.parse_conditional()?;
        warn!("Cond is: {:?}", cond);
        let block = self.parse_block()?;
        warn!("Blcok is {:?}", block);

        Some(Expr { is_return: false, type_annot: None,
            kind: ExprKind::While(Box::new(cond), block) })
    }

    fn parse_defer(&mut self) -> Option<Expr> {
        Some(Expr { is_return: false, type_annot: None, kind: ExprKind::Defer(self.parse_block()?) })
    }

    fn parse_for(&mut self) -> Option<Expr> {
        let init = parse_or_token!(self, parse_expr, TokenKind::SemiColon, "';'")
            .map(|x| Box::new(x));
        let cond = parse_or_token!(self, parse_conditional, TokenKind::SemiColon, "';'")
            .map(|x| Box::new(x));
        let step = if peek_expect!(self.it, TokenKind::LBrace) { None }
                   else { Some(Box::new(self.parse_expr()?)) };

        Some(Expr { is_return: false, type_annot: None,
            kind: ExprKind::For(init, cond, step, self.parse_block()?) })
    }

    fn parse_expr(&mut self) -> Option<Expr> {
        let lhs = self.parse_conditional()?;
        if lhs.is_unary() {
            // possible declaration
            if try_expect!(self.it, TokenKind::Colon).is_some() {
                let ty = if !peek_expect!(self.it, TokenKind::Assign) {
                    Some(self.parse_type()?)
                } else {
                    None
                };
                let rhs = if try_expect!(self.it, TokenKind::Assign).is_some() {
                    Some(Box::new(self.parse_conditional()?))
                } else if ty.is_some() {
                    None
                } else {
                    warn!("Was expecting and/or type/value i.e. a := 2 or a : int");
                    return None;
                };
                Some(Expr { is_return: false, type_annot: None,
                    kind: ExprKind::Decl {
                        lhs: Box::new(lhs),
                        lhs_type: ty,
                        rhs: rhs
                    }
                })
            } else {
                // assignment i.e. +=
                let kind = match self.it.peek() {
                    Some(Ok(tok)) if tok.is_assignment() => {
                        match token_kind_to_assignment(&self.it.next()?.unwrap().kind) {
                            Some(tok) => tok,
                            None => {
                                warn!("Invalid assignment token... this is an internal error");
                                return None;
                            }
                        }
                    },
                    _ => return Some(lhs)
                };
                let rhs = Box::new(self.parse_conditional()?);
                Some(Expr {
                    is_return: false,
                    type_annot: None,
                    kind: ExprKind::Assign {
                        lhs: Box::new(lhs),
                        rhs: rhs,
                        kind: kind
                    }
                })
            }
        } else {
            Some(lhs)
        }
    }

    fn parse_statement(&mut self) -> Option<Expr> {
        expect_match!(self.it, _tok, {
            TokenKind::If => return self.parse_if(),
            TokenKind::While => return self.parse_while(),
            TokenKind::Defer => return self.parse_defer(),
            TokenKind::For => return self.parse_for();
            _ => {}
        });

        let is_return = try_expect!(self.it, TokenKind::Return).is_some();
        let mut inner = self.parse_expr()?;
        if is_return {
            match inner.kind {
                ExprKind::Assign{..} => {
                    warn!("Can't return a non conditional (i.e. don't return assignments)");
                    return None;
                },
                _ => {}
            }
        }

        // all statements taht aren't if/for/while/defer... require a semicolon
        eat!(self.it, TokenKind::SemiColon, "';'");
        inner.is_return = is_return;
        Some(inner)
    }

    fn parse_block(&mut self) -> Option<Block> {
        warn!("BLK: {:?}", self.it.peek());
        eat!(self.it, TokenKind::LBrace, "'{'");
        let mut list = vec![];

        // NOTE: This supports an extra trailing comma
        while !peek_expect!(self.it, TokenKind::RBrace) {
            info!("ONe more loop... {:?}", self.it.peek());
            list.push(self.parse_statement()?);
        }

        eat!(self.it, TokenKind::RBrace, "'}'");
        Some(Block { exprs: list })
    }

    fn parse_id(&mut self) -> Option<Ident> {
        if let Some(tok) = try_expect!(self.it, TokenKind::Ident(_)) {
            Some(tok.kind.into_ident().unwrap())
        } else {
            None
        }
    }

    fn parse_type(&mut self) -> Option<Type> {
        let mut ty = expect_match!(self.it, tok, {
            TokenKind::Asterix => {
                // pointer type
                Type::Pointer(Box::new(self.parse_type()?))
            },
            TokenKind::LParen => {
                let ty = self.parse_type()?;
                eat!(self.it, TokenKind::RParen, "')'");
                Type::Paren(Box::new(ty))
            },
            TokenKind::Ident(_) => {
                let gen_args = if try_expect!(self.it, TokenKind::LAngle).is_some() {
                    parse_list!(self, parse_type, TokenKind::Comma, TokenKind::RAngle, "'>'")
                } else {
                    vec![]
                };
                Type::Var{id: tok.kind.into_ident().unwrap(), gen_args}
            },
            TokenKind::Function => {
                let gen_args = if try_expect!(self.it, TokenKind::LAngle).is_some() {
                    parse_list!(self, parse_id, TokenKind::Comma, TokenKind::RAngle, "'>'")
                } else {
                    vec![]
                };

                let name = if !peek_expect!(self.it, TokenKind::LParen) {
                    // NOTE: we have to propagate the failing here
                    Some(self.parse_id()?)
                } else {
                    None
                };

                eat!(self.it, TokenKind::LParen, "'('");
                let args = parse_list!(self, parse_type, TokenKind::Comma, TokenKind::RParen, "')'");
                let ret = if try_expect!(self.it, TokenKind::Arrow).is_some() {
                    Some(Box::new(self.parse_type()?))
                } else {
                    None
                };

                Type::Func{name, args, ret, gen_args}
            };
            _ => {
                warn!("Was expecting a type but instead found (TODO: put token found here)");
                return None;
            }
        });

        while try_expect!(self.it, TokenKind::LBracket).is_some() {
            let len = self.parse_conditional()?;
            eat!(self.it, TokenKind::RBracket, "']'");
            ty = Type::Array{inner: Box::new(ty), len: Box::new(len)};
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
        loop {
            match self.it.peek() {
                Some(Ok(c)) if c.is_unary() => {
                    // TODO: This will probably be a bit prone to bugs...
                    unary_ops.push(token_kind_to_unary(&self.it.next()?.unwrap().kind)?);
                }
                _ => break
            }
        }

        let rhs = self.parse_atom()?;
        if unary_ops.len() > 0 {
            Some(Expr {
                is_return: false, type_annot: None,
                kind: ExprKind::Unary(unary_ops, Box::new(rhs))
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

    fn parse_gentype_list(&mut self) -> Option<Vec<Type>> {
        Some(if try_expect!(self.it, TokenKind::Period).is_some() {
            eat!(self.it, TokenKind::LAngle, "'<'");
            parse_list!(self, parse_type, TokenKind::Comma, TokenKind::RAngle, "'>'")
        } else {
            vec![]
        })
    }

    fn parse_atom(&mut self) -> Option<Expr> {
        info!("Should be here... {:?}", self.it.peek());
        let lhs = expect_match!(self.it, tok, {
            TokenKind::LParen => {
                let inner = Box::new(self.parse_conditional()?);
                eat!(self.it, TokenKind::RParen, "')'");
                Expr { is_return: false, type_annot: None, kind: ExprKind::Paren(inner) }
            },
            TokenKind::Ident(_) => {
                let id = tok.kind.into_ident().unwrap();
                Expr { is_return: false, type_annot: None, kind: ExprKind::Var(id) }
            },
            TokenKind::New => {
                let mut gen_args = self.parse_gentype_list()?;
                let ty = gen_args.pop();
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
                let arg = args.pop().map(|x| Box::new(x));

                let init = if try_expect!(self.it, TokenKind::LBrace).is_some() {
                    parse_list!(self, parse_initialiser, TokenKind::Comma, TokenKind::RBrace, "'}'")
                } else {
                    vec![]
                };

                Expr { is_return: false, type_annot: None, kind: ExprKind::New(ty, arg, init) }
            },
            TokenKind::Cast => {
                // just a function ... like cast.<Out, In>(in: In)
                let mut gen_args = self.parse_gentype_list()?;
                let to = gen_args.pop();
                let from = gen_args.pop();
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
                Expr { is_return: false, type_annot: None, kind: ExprKind::Cast{ to, from, obj } }
            },
            TokenKind::Number(..) => {
                let (num, postfix) = tok.kind.into_number().unwrap();
                // currently only supports int32 and flt32
                Expr { is_return: false, type_annot: None,
                    kind: ExprKind::Constant(match postfix {
                        Postfix::i32 => ConstantKind::Int32(num.parse::<i32>().unwrap()),
                        Postfix::f64 => ConstantKind::Flt64(num.parse::<f64>().unwrap()),
                        _ => {
                            panic!("Unhandled case");
                        }
                    })
                }
            },
            TokenKind::Bool(..) => {
                let value = tok.kind.into_bool().unwrap();
                Expr { is_return: false, type_annot: None,
                       kind: ExprKind::Constant(ConstantKind::Bool(value)) }
            },
            TokenKind::Str(..) => {
                let value = tok.kind.into_str().unwrap();
                Expr { is_return: false, type_annot: None,
                       kind: ExprKind::Constant(ConstantKind::Str(value)) }
            },
            TokenKind::Null => {
                Expr { is_return: false, type_annot: None,
                    kind: ExprKind::Constant(ConstantKind::Null) }
            },
            TokenKind::Character(..) => {
                let value = tok.kind.into_character().unwrap();
                Expr { is_return: false, type_annot: None,
                    kind: ExprKind::Constant(ConstantKind::Char(value)) }
            },
            TokenKind::Uninitialised => {
                Expr { is_return: false, type_annot: None, kind: ExprKind::Uninitialiser }
            },
            TokenKind::Let => {
                Expr { is_return: false, type_annot: None, kind: ExprKind::Let(Box::new(self.parse_expr()?)) }
            },
            TokenKind::Function => {
                Expr { is_return: false, type_annot: None, kind: ExprKind::Lambda(self.parse_lambda()?) }
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
            let tmp = Expr { is_return: false, type_annot: None, kind: res.kind };
            res.kind = expect_match!(self.it, tok, {
                TokenKind::Period => {
                    // member access or function call
                    expect_match!(self.it, tok, {
                        TokenKind::Ident(..) => {
                            ExprKind::Member(Box::new(tmp), tok.kind.into_ident().unwrap())
                        },
                        TokenKind::LAngle => {
                            let id = match tmp.kind {
                                ExprKind::Var(id) => id,
                                _ => {
                                    warn!("You can only use a generic call through an identifier {:?}", tok);
                                    return None;
                                }
                            };
                            let gen_args = self.parse_gentype_list()?;
                            eat!(self.it, TokenKind::LParen, "'('");
                            let args = parse_list!(self, parse_conditional, TokenKind::Comma, TokenKind::RParen, "')'");
                            ExprKind::GenFuncCall(id, gen_args, args)
                        };
                        _ => {
                            warn!("Invalid member access / function call {:?}", self.it.peek());
                            return None;
                        }
                    })
                },
                TokenKind::LParen => {
                    // just a function call
                    let mut args = parse_list!(self, parse_conditional, TokenKind::Comma, TokenKind::RParen, "')'");
                    ExprKind::FuncCall(Box::new(tmp), args)
                },
                TokenKind::LBracket => {
                    // index
                    let expr = self.parse_conditional()?;
                    eat!(self.it, TokenKind::RBracket, "']'");
                    ExprKind::Index(Box::new(tmp), Box::new(expr))
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
            // NOTE: we have to propagate the failing here
            Some(self.parse_type()?)
        } else {
            None
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
        let id = match expect!(self.it, TokenKind::Ident(_), "identifier") {
            Some(Token{kind: TokenKind::Ident(id), ..}) => id,
            Some(_) | None => return None,
        };

        let id_list = if try_expect!(self.it, TokenKind::LAngle).is_some() {
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
        let decl_list = parse_list!(self, parse_decl, TokenKind::Comma, TokenKind::RBrace, "'}'");

        Some(Struct {
            id: id,
            gen_args: id_list,
            decls: decl_list,
            is_list: is_list
        })
    }

    fn parse_opt_decl(&mut self) -> Option<Decl> {
        let id = self.parse_id()?;
        let (decl_type, val) = if try_expect!(self.it, TokenKind::Colon).is_some() {
            let decl_type = if !peek_expect!(self.it, TokenKind::Assign) {
                // NOTE: we have to propagate the failing here
                Some(self.parse_type()?)
            } else {
                None
            };

            let val = if try_expect!(self.it, TokenKind::Assign).is_some() {
                Some(self.parse_conditional()?)
            } else {
                None
            };

            (decl_type, val)
        } else {
            (None, None)
        };

        return Some(Decl {
            id: id,
            decl_type: decl_type,
            val: val
        })
    }

    fn parse_lambda(&mut self) -> Option<Lambda> {
        eat!(self.it, TokenKind::LParen, "'('");
        let args = parse_list!(self, parse_opt_decl, TokenKind::Comma, TokenKind::RParen, "')'");
        let ret = if try_expect!(self.it, TokenKind::Arrow).is_some() {
            Some(self.parse_type()?)
        } else {
            None
        };

        let block = if try_expect!(self.it, TokenKind::FatArrow).is_some() {
            let mut cond = self.parse_conditional()?;
            cond.is_return = true;
            Block { exprs: vec![cond] }
        } else {
            self.parse_block()?
        };

        Some(Lambda {
            args,
            ret,
            block
        })
    }

    fn parse_function(&mut self) -> Option<Function> {
        let gen_args = if try_expect!(self.it, TokenKind::LAngle).is_some() {
            parse_list!(self, parse_id, TokenKind::Comma, TokenKind::RAngle, "'>'")
        } else {
            vec![]
        };

        let name = if !peek_expect!(self.it, TokenKind::LParen) {
            // NOTE: we have to propagate the failing here
            Some(self.parse_id()?)
        } else {
            None
        };

        eat!(self.it, TokenKind::LParen, "'('");
        let args = parse_list!(self, parse_decl, TokenKind::Comma, TokenKind::RParen, "')'");
        let ret = if try_expect!(self.it, TokenKind::Arrow).is_some() {
            Some(self.parse_type()?)
        } else {
            None
        };

        let block = if try_expect!(self.it, TokenKind::FatArrow).is_some() {
            let mut cond = self.parse_conditional()?;
            cond.is_return = true;
            Block { exprs: vec![cond] }
        } else {
            self.parse_block()?
        };

        Some(Function {
            gen_args,
            name,
            args,
            ret,
            block
        })
    }
}

