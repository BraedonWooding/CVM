use crate::compiler::scope::*;
use crate::compiler::ast::*;

extern crate log;
use log::{warn};

pub struct TypeInfer<'a> {
    stack: &'a mut ScopeStack,
}

impl<'a> TypeInfer<'a> {
    pub fn type_infer_program(program: &mut Program, stack: &'a mut ScopeStack) {
        let mut inferer = TypeInfer { stack };

        for value in program.structs.values_mut() {
            for decl in value.decls.iter_mut() {
                inferer.type_infer_decl(decl);
            }
        }

        // type check all functions and structs...
        for function in program.functions.values_mut() {
            inferer.type_infer_function(function);
        }
    }

    fn deref_type(&mut self, ty: ParsedType) -> Option<ParsedType> {
        match ty {
            // This is the easy case, clearly we can just extract inner
            ParsedType::Pointer(inner) | ParsedType::Array{inner, ..} => Some(*inner),
            ParsedType::Fresh{id} => {
                let ty_obj = self.stack.lookup_fresh(id);
                let mut ty = ty_obj.borrow_mut();

                let new_obj = match *ty {
                    ParsedType::Fresh{id: new_id} if new_id == id => true,
                    _ => false,
                };

                if new_obj {
                    let new_id = ScopeStack::new_fresh_type();
                    *ty = ParsedType::Pointer(Box::new(new_id.clone()));
                    Some(new_id)
                } else {
                    self.deref_type(ty.clone())
                }
            },
            _ => {
                warn!("Attempt to dereference a type that isn't a pointer nor an array");
                None
            }
        }
    }

    fn apply_unary(&mut self, kinds: &[UnaryKind], ty: ParsedType) -> Option<ParsedType> {
        let mut res = ty;
        for kind in kinds {
            res = match kind {
                UnaryKind::BitNot | UnaryKind::Neg | UnaryKind::Pos | UnaryKind::Not => res,
                UnaryKind::Address => ParsedType::Pointer(Box::new(res)),
                UnaryKind::Deref => match self.deref_type(res) {
                    Some(new_ty) => new_ty,
                    None => return None
                }
            }
        }
        Some(res)
    }

    fn type_infer_decl(&mut self, decl: &mut Decl) -> Option<ParsedType> {
        match decl.val {
            Some(ref mut expr) => {
                self.type_infer_expr(expr);
                if let ParsedType::Unknown = decl.decl_type {
                    decl.decl_type = expr.type_annot.clone();
                    let cpy = self.stack.cur().borrow_mut().lookup_var_cond(&decl.id).unwrap();
                    std::mem::replace(&mut *cpy.borrow_mut(), decl.decl_type.clone());
                }
            },
            None =>
                if let ParsedType::Unknown = decl.decl_type {
                    warn!("Internal error decl {:?} has missing type/value", decl);
                    return None;
                },
        }
        return Some(decl.decl_type.clone())
    }

    fn type_infer_expr(&mut self, expr: &mut Expr) {
        let ty = match &mut expr.kind.inner {
            ExprKind::Assign{ref mut lhs, ref mut rhs, ..} => {
                self.type_infer_expr(lhs);
                self.type_infer_expr(rhs);
                Some(lhs.type_annot.clone())
            },
            ExprKind::Decl(ref mut decl) => {
                self.type_infer_decl(decl)
            },
            // TODO: allocator should be of a given type
            ExprKind::New(given_type, _allocator, _init) => {
                // TODO: We should verify that init
                //       is valid...
                //       PostHook pipeline would be nice...
                Some(given_type.clone())
            },
            ExprKind::Unary(args, ref mut expr) => {
                // type check inner
                self.type_infer_expr(expr);
                self.apply_unary(&args, expr.type_annot.clone())
            },
            ExprKind::Paren(ref mut inner) => {
                self.type_infer_expr(inner);
                Some(inner.type_annot.clone())
            },
            ExprKind::Var(id) => {
                match self.stack.cur().borrow_mut().lookup_var_cond(&id) {
                    Some(ty) => Some(ty.borrow().clone()),
                    None => {
                        warn!("Error: No variable found with name {:?}", id);
                        None
                    }
                }
            },
            ExprKind::Member(ref mut expr, id) => {
                // requires struct lookup
                // TODO:
                self.type_infer_expr(expr);
                // there is a reasonable chance that we don't actually have
                // a type for the member...
                None
            },
            ExprKind::GenFuncCall(id, gen_args, args) => {
                // TODO: Generic functions
                for arg in args.iter_mut() {
                    self.type_infer_expr(arg);
                }
                None
            },
            ExprKind::FuncCall(ref mut func, ref mut args) => {
                // the type is just going to be the func
                self.type_infer_expr(func);
                for arg in args.iter_mut() {
                    self.type_infer_expr(arg);
                }
                match &func.type_annot {
                    ParsedType::Func{ret, ..} => Some((**ret).clone()),
                    ParsedType::Fresh{id} => {
                        let ty_obj = self.stack.lookup_fresh(*id);
                        let ty = ty_obj.borrow_mut();
                        // TODO: Make this recursive to handle a -> b -> func
                        if let ParsedType::Func{ref ret, ..} = *ty {
                            Some((**ret).clone())
                        } else {
                            // just create a new fresh type
                            // for now, the type checker will fix it
                            Some(ScopeStack::new_fresh_type())
                        }
                    },
                    _ => {
                        // TODO: Better error...
                        // Especially if the thing is just a 'var'
                        // then it means we don't know the type (potentially)
                        warn!("Can't perform a call on a pointer, array, vec on object {:?}", func);
                        None
                    }
                }
            },
            ExprKind::Cast{to, ref mut obj, ..} => {
                self.type_infer_expr(obj);
                // cast's don't have any othe reliance
                Some(to.clone())
            },
            ExprKind::Index(ref mut expr, ref mut index) => {
                self.type_infer_expr(expr);
                self.type_infer_expr(index);
                self.deref_type(expr.type_annot.clone())
            },
            ExprKind::Sizeof(_ty, ref mut expr) => {
                match expr {
                    Some(ref mut inner) => self.type_infer_expr(inner),
                    _ => {}
                }

                // sizeof always returns size_t
                Some(ParsedType::new_simple_var_type("size_t"))
            },
            ExprKind::Binop(ref mut lhs, op, ref mut rhs) => {
                // We'll check they are the same type in the type check phase...
                self.type_infer_expr(lhs);
                self.type_infer_expr(rhs);
                Some(match op {
                    BinopKind::BitOr | BinopKind::BitAnd | BinopKind::BitXor |
                    BinopKind::ShiftRight | BinopKind::ShiftLeft | BinopKind::Add |
                    BinopKind::Sub | BinopKind::Mul | BinopKind::Div | BinopKind::Mod => {
                        // will produce a type equivalent to the types given
                        // the lhs or rhs may not have a specified type yet...
                        // so we can't really infer a proper type so we'll give it a type variable
                        ScopeStack::new_fresh_type()
                    },
                    BinopKind::BoolAnd | BinopKind::BoolOr | BinopKind::Equal |
                    BinopKind::NotEqual | BinopKind::LessThan | BinopKind::GreaterThan |
                    BinopKind::LessEqual | BinopKind::GreaterEqual => {
                        // will always produce a boolean
                        ParsedType::new_simple_var_type("bool")
                    }
                })
            },
            ExprKind::Ternary{..} => {
                // TODO:
                None
            },
            ExprKind::Constant(kind) => Some(match kind {
                // TODO: Constants should not convert to int32/flt64
                //       instead they should convert to a int / flt literal
                //       that is convertible to all types.
                //       otherwise it requires ew casts / prefixes
                // (on second thought im not sure requiring prefixes is bad)
                ConstantKind::Int32(..) => ParsedType::new_simple_var_type("int32_t"),
                ConstantKind::Flt64(..) => ParsedType::new_simple_var_type("double"),
                ConstantKind::Str(..) => ParsedType::Pointer(Box::new(ParsedType::new_simple_var_type("char"))),
                ConstantKind::Char(..) => ParsedType::new_simple_var_type("char"),
                ConstantKind::Null => ParsedType::Pointer(Box::new(ScopeStack::new_fresh_type())),
                ConstantKind::Bool(..) => ParsedType::new_simple_var_type("bool"),
            }),
            ExprKind::Let(ref mut inner) => {
                self.type_infer_expr(inner);
                Some(inner.type_annot.clone())
            },
            ExprKind::Lambda(Lambda{ref mut args, ref mut block, ..}) => {
                // TODO: support lambdas
                None
            },
            // doesn't really have a 'type' is kinda like 'null'
            // for example `x := null` is an error and so is `x := ---`
            // because both don't know what type is 'x'
            ExprKind::Uninitialiser => Some(ScopeStack::new_fresh_type())
        };
        if let Some(ty) = ty {
            expr.type_annot = ty;
        }
    }

    fn type_infer_block(&mut self, block: &mut Block) {
        for expr in block.statements.iter_mut() {
            self.type_infer_statement(expr);
        }
    }

    fn type_infer_expr_and_block(&mut self, expr: &mut Expr, block: &mut Block) {
        self.stack.push(&block.scope);
        self.type_infer_expr(expr);
        self.type_infer_block(block);
        self.stack.pop();
    }

    fn type_infer_statement(&mut self, statement: &mut Statement) {
        match statement {
            Statement::If{ref mut if_cond, ref mut if_block, ref mut else_if, ref mut else_block} => {
                self.type_infer_expr_and_block(if_cond, if_block);

                for (ref mut expr, ref mut block) in else_if.iter_mut() {
                    self.type_infer_expr_and_block(expr, block);
                }

                if let Some(ref mut block) = else_block { self.type_infer_block(block); }
            },
            Statement::While(ref mut expr, ref mut block) => {
                self.type_infer_expr_and_block(expr, block);
            },
            Statement::For(ref mut start, ref mut cond, ref mut end, ref mut block) => {
                self.stack.push(&block.scope);
                if let Some(ref mut expr) = start { self.type_infer_expr(expr); }
                if let Some(ref mut expr) = cond { self.type_infer_expr(expr); }
                if let Some(ref mut expr) = end { self.type_infer_expr(expr); }
                self.type_infer_block(block);
                self.stack.pop();
            },
            Statement::Expr(ref mut inner) | Statement::Return(ref mut inner) => self.type_infer_expr(inner),
            Statement::Defer => {}
        }
    }

    fn type_infer_function(&mut self, func: &mut Function) {
        self.stack.push(&func.block.scope);

        for arg in func.args.iter_mut() {
            self.type_infer_decl(arg);
        }

        self.type_infer_block(&mut func.block);
        self.stack.pop();
    }
}
