use crate::compiler::scope::*;
use crate::compiler::ast::*;
use std::rc::Rc;

extern crate log;
use log::{info, warn};

pub struct TypeInfer<'a> {
    stack: &'a mut ScopeStack,
}

impl<'a> TypeInfer<'a> {
    pub fn type_infer_program(program: &mut Program, stack: &'a mut ScopeStack) {
        let mut checker = TypeInfer { stack };

        // type check all functions and structs...
        for function in program.functions.values_mut() {
            checker.type_infer_function(function);
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
                UnaryKind::BitNot | UnaryKind::Neg | UnaryKind::Pos | UnaryKind::Neg | UnaryKind::Not => res,
                UnaryKind::Address => ParsedType::Pointer(Box::new(res)),
                UnaryKind::Deref => match self.deref_type(res) {
                    Some(new_ty) => new_ty,
                    None => return None
                }
            }
        }
        Some(res)
    }

    fn type_infer_expr(&mut self, expr: &mut Expr) {
        expr.type_annot = match &mut expr.kind {
            ExprKind::Assign{ref mut lhs, ref mut rhs, ..} => {
                self.type_infer_expr(lhs);
                self.type_infer_expr(rhs);
                lhs.type_annot.clone()
            },
            ExprKind::Decl(ref mut decl) => {
                match decl.val {
                    Some(ref mut expr) => {
                        self.type_infer_expr(expr);
                        match &decl.decl_type {
                            // NOTE: This would happen anyways in type_checking
                            //       but it's fine to do it here.
                            //       If the type is fresh for a declaration
                            //       then it must be auto assigned so we can remove it
                            None | Some(ParsedType::Fresh{..}) => {
                                decl.decl_type = expr.type_annot.clone();
                                expr.type_annot.clone()
                            }
                            Some(ty) => Some(ty.clone()),
                        }
                    },
                    None => match &decl.decl_type {
                        Some(ty) => Some(ty.clone()),
                        None => {
                            warn!("Internal error decl {:?} has missing type/value", decl);
                            None
                        }
                    }
                }
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
                match &expr.type_annot {
                    None => None,
                    Some(ty) => self.apply_unary(&args, ty.clone())
                }
            },
            ExprKind::Paren(ref mut inner) => {
                self.type_infer_expr(inner);
                inner.type_annot.clone()
            },
            ExprKind::Var(id) => {
                match self.stack.cur().borrow_mut().lookup_var_cond(&id) {
                    Some(ty) => Some(ty.borrow().clone()),
                    None => {
                        warn!("Error: No variable found with name {}", id);
                        None
                    }
                }
            },
            ExprKind::Member(ref mut expr, id) => {
                // requires struct lookup
                // TODO:
                self.type_infer_expr(expr);
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
                    None => None,
                    Some(ParsedType::Func{ret, ..}) => Some((**ret).clone()),
                    Some(ParsedType::Fresh{id}) => {
                        let ty_obj = self.stack.lookup_fresh(*id);
                        let mut ty = ty_obj.borrow_mut();
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
                        warn!("Can't perform a call on a pointer, array, vec");
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
                // index always has to be an integral type
                // but that is enforced in type checking
                // we can infer the type overall for this
                // by deref'ing the expr
                match &expr.type_annot {
                    Some(ty) => self.deref_type(ty.clone()),
                    None => None
                }
            },
            ExprKind::Sizeof(ty, ref mut expr) => {
                match expr {
                    Some(ref mut inner) => self.type_infer_expr(inner),
                    _ => {}
                }
                // sizeof always returns size_t
                Some(ParsedType::Var{id: "size_t".to_string(), gen_args: vec![]})
            },
            ExprKind::Binop(ref mut lhs, op, ref mut rhs) => {
                // We'll check they are the same type in the type check phase...
                self.type_infer_expr(lhs);
                self.type_infer_expr(rhs);
                match op {
                    BinopKind::BitOr | BinopKind::BitAnd | BinopKind::BitXor |
                    BinopKind::ShiftRight | BinopKind::ShiftLeft | BinopKind::Add |
                    BinopKind::Sub | BinopKind::Mul | BinopKind::Div | BinopKind::Mod => {
                        // will produce a type equivalent to the types given
                        // the lhs or rhs may not have a specified type yet...
                        // so we can't really infer a proper type so we'll give it a type variable
                        Some(ScopeStack::new_fresh_type())
                    },
                    BinopKind::BoolAnd | BinopKind::BoolOr | BinopKind::Equal |
                    BinopKind::NotEqual | BinopKind::LessThan | BinopKind::GreaterThan |
                    BinopKind::LessEqual | BinopKind::GreaterEqual => {
                        // will always produce a boolean
                        Some(ParsedType::Var{id: "bool".to_string(), gen_args: vec![]})
                    }
                }
            },
            ExprKind::Ternary{..} => {
                // TODO:
                None
            },
            ExprKind::Constant(kind) => match kind {
                ConstantKind::Int32(..) => Some(ParsedType::Var{id: "int32_t".to_string(), gen_args: vec![]}),
                ConstantKind::Flt64(..) => Some(ParsedType::Var{id: "double".to_string(), gen_args: vec![]}),
                ConstantKind::Str(..) => Some(ParsedType::Pointer(Box::new(ParsedType::Var{id: "char".to_string(), gen_args: vec![]}))),
                ConstantKind::Char(..) => Some(ParsedType::Var{id: "char".to_string(), gen_args: vec![]}),
                ConstantKind::Null => Some(ParsedType::Pointer(Box::new(ScopeStack::new_fresh_type()))),
                ConstantKind::Bool(..) => Some(ParsedType::Var{id: "bool".to_string(), gen_args: vec![]}),
            },
            ExprKind::Let(ref mut inner) => {
                self.type_infer_expr(inner);
                inner.type_annot.clone()
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
    }

    fn type_infer_block(&mut self, block: &mut Block) {
        for expr in block.exprs.iter_mut() {
            self.type_infer_statement(expr);
        }
    }

    fn type_infer_expr_and_block(&mut self, expr: &mut Expr, block: &mut Block) {
        self.stack.push(&block.scope);
        self.type_infer_expr(expr);
        self.type_infer_block(block);
        self.stack.pop_scope();
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
                self.stack.pop_scope();
            },
            Statement::Expr(ref mut inner) | Statement::Return(ref mut inner) => self.type_infer_expr(inner),
            Statement::Defer => {}
        }
    }

    fn type_infer_function(&mut self, func: &mut Function) {
        self.stack.push(&func.block.scope);

        for arg in func.args.iter_mut() {
            if let Some(ref mut val) = arg.val {
                self.type_infer_expr(val);
                // and now we can set our type to the inferred type
                arg.decl_type = val.type_annot.clone();
            }
        }

        for expr in func.block.exprs.iter_mut() {
            // type checking for return types are done later
            self.type_infer_statement(expr);
        }

        self.stack.pop_scope();
    }
}
