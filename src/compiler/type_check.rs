use crate::compiler::*;
use ast::*;
use scope::*;

use std::collections::HashMap;

extern crate log;
use log::{info, warn};

pub struct TypeCheck<'a> {
    stack: &'a mut ScopeStack,
    ctx: Option<ParsedType>,
    struct_type_info: HashMap<String, ParsedType>,
    type_definition_table: &'a TypeDefinitionTable,
}

/*
 * TypeCheck should remove all placeholder types and resolve all potential
 * type errors but won't setup the type references.
 *
 * We are separating type inference into a type infer and type check
 * just to make it simpler and easier to debug (arguably we take a perf
 * hit since we traverse the AST one more time then necessary but it's fine
 * since it does give us the benefits of being able to concrete all types
 * making future AST transformations cheaper -- and we don't run optimisations
 * so our compiler is already fast)
 */

impl<'a> TypeCheck<'a> {
    pub fn try_unify<'b>(a: &'b ParsedType, b: &'b ParsedType) -> Vec<(usize, ParsedType)> {
        let mut checker = TypeCheck {
            stack: &mut ScopeStack::empty(),
            ctx: None,
            struct_type_info: HashMap::default(),
            type_definition_table: &TypeDefinitionTable::load_type_definition_table(),
        };
        checker.unify(a, b)
    }

    pub fn type_check_program(program: &'a mut Program, stack: &'a mut ScopeStack, type_definition_table: &'a TypeDefinitionTable) {
        let mut checker = TypeCheck {
            stack,
            ctx: None,
            struct_type_info: HashMap::default(),
            type_definition_table
        };

        for ref mut value in program.structs.values_mut() {
            checker.type_check_struct(value);
        }

        for ref mut function in program.functions.values_mut() {
            checker.type_check_function(function);
        }
    }

    fn type_check_struct(&mut self, obj: &mut Struct) {
        for ref mut decl in obj.decls.iter_mut() {
            self.type_check_decl(decl);
            self.struct_type_info
                .insert(format!("{}.{}", *obj.id, *decl.id), decl.decl_type.clone());
        }
    }

    fn resolve_type(&self, ty: &ParsedType) -> ParsedType {
        match ty {
            ParsedType::Unknown => {
                warn!("No type should ever resolve to unknown");
                ParsedType::Unknown
            }
            ParsedType::Fresh { id } => match self.stack.lookup_fresh_immut(*id) {
                None => {
                    warn!("Non resolved type for {:?}", id);
                    ParsedType::Fresh { id: *id }
                }
                Some(other) => self.resolve_type(&other.borrow()),
            },
            other => other.clone(),
        }
    }

    fn type_check_function(&mut self, func: &mut Function) {
        // TODO: Maybe force args/ret to have concrete type
        let tmp = std::mem::replace(&mut self.ctx, Some(func.ret.clone()));

        self.stack.push(&mut func.block.scope);
        self.ctx = Some(func.ret.clone());
        self.type_check_block(&mut func.block);
        self.stack.pop();

        self.ctx = tmp;
    }

    fn type_check_block(&mut self, block: &mut Block) {
        for ref mut statement in block.statements.iter_mut() {
            self.type_check_statement(statement);
        }

        // defers get type checked after the block
        // this technically shouldn't matter but often defers are stuff
        // like 'free' or 'close' that are generic calls and don't provide
        // much type information so it's almost always more efficient
        // to do them after.
        // Especially since you can't leak variables out of them.
        for ref mut defer in block.scope.borrow_mut().defer_exprs.iter_mut() {
            self.stack.push(&defer.scope);
            self.type_check_block(defer);
            self.stack.pop();
        }
    }

    fn type_check_block_and_unify_and_set(
        &mut self,
        block: &mut Block,
        expr: &mut Expr,
        unify_with: &ParsedType,
    ) {
        self.stack.push(&block.scope);
        self.type_check_expr(expr);
        self.unify_and_set(&mut expr.type_annot, unify_with);
        self.type_check_block(block);
        self.stack.pop();
    }

    fn type_check_decl(&mut self, decl: &mut Decl) {
        if let Some(ref mut val) = decl.val {
            self.type_check_expr(val);
            self.unify_and_set(&mut decl.decl_type, &val.type_annot);
        }
    }

    fn type_check_statement(&mut self, statement: &mut Statement) {
        match statement {
            Statement::If {
                ref mut if_cond,
                ref mut if_block,
                ref mut else_if,
                ref mut else_block,
            } => {
                self.type_check_block_and_unify_and_set(
                    if_block,
                    if_cond,
                    &ParsedType::new_simple_var_type("bool"),
                );
                for (ref mut cond, ref mut block) in else_if.iter_mut() {
                    self.type_check_block_and_unify_and_set(
                        block,
                        cond,
                        &ParsedType::new_simple_var_type("bool"),
                    );
                }

                if let Some(ref mut block) = else_block {
                    self.stack.push(&block.scope);
                    self.type_check_block(block);
                    self.stack.pop();
                }
            }
            Statement::While(ref mut cond, ref mut block) => {
                self.type_check_block_and_unify_and_set(
                    block,
                    cond,
                    &ParsedType::new_simple_var_type("bool"),
                );
            }
            Statement::For(ref mut start, ref mut stop, ref mut step, ref mut block) => {
                self.stack.push(&block.scope);

                if let Some(ref mut expr) = start {
                    self.type_check_expr(expr);
                }

                if let Some(ref mut expr) = stop {
                    self.type_check_expr(expr);
                    self.unify_and_set(
                        &mut expr.type_annot,
                        &ParsedType::new_simple_var_type("bool"),
                    );
                }

                if let Some(ref mut expr) = step {
                    self.type_check_expr(expr);
                }

                self.type_check_block(block);

                self.stack.pop();
            }
            Statement::Expr(ref mut inner) => self.type_check_expr(inner),
            Statement::Return(ref mut inner) => {
                self.type_check_expr(inner);
                // unify with the context!!
                if let Some(ctx) = self.ctx.clone() {
                    self.unify_and_set(&mut inner.type_annot, &ctx);
                } else {
                    warn!("No context, this is probably a bug");
                }
            }
            Statement::Defer => {}
        }
    }

    fn type_check_arithmetic(&self, left: &ParsedType, right: &ParsedType) {
        // In C all arithmetic is defined such that if one operator is allowed
        // all are technically supported.
        // i.e. no '+' operator for strings (but you can add the pointers)
        // TODO: If we ever support operator overloading clearly this will have
        //       to change...
        // NOTE: we currently don't support you doing arithmetic
    }

    fn try_deref_type(&self, ty: &ParsedType) -> Option<ParsedType> {
        let ty = self.resolve_type(&ty);
        match ty {
            ParsedType::Pointer(inner) | ParsedType::Array { inner, .. } => Some(*inner),
            _ => {
                // TODO: We should probably carry location of deref
                //       that probably should occur with unary kind
                //       and with []
                warn!("Can't deref mut type {:?}", ty);
                None
            }
        }
    }

    fn type_check_unary(&self, kinds: &[UnaryKind], ty: &ParsedType) -> Option<ParsedType> {
        let mut res = ty.clone();
        for kind in kinds {
            res = match kind {
                // TODO: we should still check if its an integral type
                UnaryKind::BitNot | UnaryKind::Not => res,
                // TODO: should be a number type (float/int)
                UnaryKind::Neg | UnaryKind::Pos => res,
                UnaryKind::Address => ParsedType::Pointer(Box::new(res)),
                UnaryKind::Deref => match self.try_deref_type(&res) {
                    Some(res) => res,
                    None => return None,
                },
            }
        }
        Some(res)
    }

    fn type_check_expr(&mut self, expr: &mut Expr) {
        // To alleviate the weird borrow checker being cranky
        // we are using this to extend the scope of variables
        // so that we can then set it to unify with as a reference
        // means we can save on a lot of clones :)
        let tmp;
        let unify_with = match &mut expr.kind.inner {
            ExprKind::Assign {
                ref mut lhs,
                ref mut rhs,
                ref mut kind,
            } => {
                self.type_check_expr(lhs);
                self.type_check_expr(rhs);
                self.unify_and_set(&mut lhs.type_annot, &rhs.type_annot);
                // TODO: The 'kind' won't be valid for all types
                //       i.e. += isn't valid for a struct/enum
                &lhs.type_annot
            }
            ExprKind::Decl(ref mut decl) => {
                self.type_check_decl(decl);
                &decl.decl_type
            }
            ExprKind::New(ref mut ty, ref mut alloc, ref mut init) => {
                // NOTE: alloc should be unified with the
                //       allocator type here
                // TODO: ^^
                // TODO: We should also do some type checking on init...
                // but that'll probably have to come after this
                // (which kinda sucks but atleast init can't
                // determine a new type which would suck more)

                // We don't even have to do a unification here...
                // but we will.
                ty
            }
            ExprKind::Unary(ref mut kinds, ref mut inner) => {
                self.type_check_expr(inner);
                tmp = match self.type_check_unary(kinds, &inner.type_annot) {
                    Some(ty) => ty,
                    None => return,
                };
                &tmp
            }
            ExprKind::Paren(ref mut inner) => {
                // easy one
                self.type_check_expr(inner);
                &inner.type_annot
            }
            ExprKind::Var(..) => {
                // interestingly enough this doesn't do anything
                // we have already grabbed the type of id from infer
                // so we'll get automatic type checking without needing
                // to redo that or check anything...
                // (Probably -- I really should verify TODO: verify)
                // I could unify with itself but come on...
                return;
            }
            ExprKind::Member(ref mut inner, ref mut member_id) => {
                self.type_check_expr(inner);

                // relatively complicated to make it handle all cases
                let id_ty = self.resolve_type(&inner.type_annot);
                if let ParsedType::Var { ref id, .. } = id_ty {
                    match self
                        .struct_type_info
                        .get(&format!("{}.{}", **id, **member_id))
                    {
                        Some(member) => {
                            tmp = member.clone();
                            &tmp
                        }
                        None => {
                            warn!(
                                "No struct/member exists for struct type {:?} with id {:?}",
                                id, member_id
                            );
                            return;
                        }
                    }
                } else {
                    warn!(
                        "Can't access a member of a non struct {:?}.{:?}",
                        inner, member_id
                    );
                    return;
                }
            }
            ExprKind::GenFuncCall(..) => {
                // TODO: Generic function calls
                return;
            }
            ExprKind::FuncCall(ref mut func, ref mut exprs) => {
                // we need the function type in a nice format
                self.type_check_expr(func);
                let fn_type = self.resolve_type(&func.type_annot);
                // Generics don't actually come into effect here
                // since you can't make a generic call from a function pointer
                if let ParsedType::Func { args, ret, .. } = fn_type {
                    if args.len() != exprs.len() {
                        warn!("TODO: Better error not enough args supplied");
                        return;
                    }
                    for i in 0..exprs.len() {
                        self.type_check_expr(&mut exprs[i]);
                        self.unify_and_set(&mut exprs[i].type_annot, &args[i]);
                    }
                    tmp = *ret;
                    &tmp
                } else {
                    warn!(
                        "You tried to call a non function, the type is {:?}; here {:?}",
                        fn_type, func
                    );
                    return;
                }
            }
            ExprKind::Cast {
                ref to,
                ref mut from,
                ref mut obj,
            } => {
                self.type_check_expr(obj);
                self.unify_and_set(&mut obj.type_annot, from);
                &to
            }
            ExprKind::Index(ref mut expr, ref mut index) => {
                self.type_check_expr(expr);
                self.type_check_expr(index);

                // TODO: Unify index with integer
                tmp = match self.try_deref_type(&expr.type_annot) {
                    Some(ty) => ty,
                    None => return,
                };
                &tmp
            }
            ExprKind::Sizeof(ref mut ty, ref mut expr) => {
                if let Some(ref mut expr) = expr {
                    self.type_check_expr(expr);
                    self.unify_and_set(&mut expr.type_annot, ty);
                }
                tmp = ParsedType::new_simple_var_type("size_t");
                &tmp
            }
            ExprKind::Binop(ref mut lhs, ref mut op, ref mut rhs) => {
                self.type_check_expr(lhs);
                self.type_check_expr(rhs);
                // TODO: This unification should go away
                //       since the types don't have to unify
                //       they just have to be upcastable
                self.unify_and_set(&mut lhs.type_annot, &rhs.type_annot);
                self.type_check_arithmetic(&lhs.type_annot, &rhs.type_annot);
                &lhs.type_annot
            }
            ExprKind::Ternary {
                ref mut cond,
                ref mut if_true,
                ref mut if_false,
            } => {
                // TODO:
                return;
            }
            ExprKind::Constant(..) => {
                // Constants literally need no work since they
                // and already concreted in terms of type and value
                return;
            }
            ExprKind::Let(ref mut inner) => {
                self.type_check_expr(inner);
                &inner.type_annot
            }
            ExprKind::Lambda(..) => {
                // TODO:
                return;
            }
            ExprKind::Uninitialiser => {
                // Give uninit a fresh type
                // in reality we should just ignore it's type
                // TODO: ^^
                expr.type_annot = ScopeStack::new_fresh_type();
                return;
            }
        };

        self.unify_and_set(&mut expr.type_annot, unify_with);
    }

    /* == Unification Algorithm == */

    fn occurs(&mut self, id: usize, other: &ParsedType) -> bool {
        match other {
            // TODO: GEN ARGS in var and func
            ParsedType::Pointer(ref inner) => self.occurs(id, inner),
            ParsedType::Array { ref inner, .. } => self.occurs(id, inner), // TODO: Expr occurs
            ParsedType::Var { .. } => false,
            ParsedType::Fresh { id: ref other } => {
                id == *other
                    || match &*self.stack.lookup_fresh(*other).borrow() {
                        // if our inner == other and id != other
                        // then we have something like a := (b := b)
                        // which is clearly fine
                        // TODO: We no longer do this ugly goddamn hack
                        //       in our type infer so let's make this simpler
                        //       we can just do a resolve type ontop of this function
                        ParsedType::Fresh { id: ref inner } if *inner == *other => false,
                        ty => self.occurs(id, &ty),
                    }
            }
            ParsedType::Func { args, ret, .. } => {
                args.iter().any(|x| self.occurs(id, x)) || self.occurs(id, ret)
            }
            ParsedType::Unknown => {
                warn!("Unknown shouldn't occur in types past type_infer");
                // we can't say whether or not this type exists in it or not
                true
            }
        }
    }

    /// NOTE: If you are getting infinite loops
    ///       this could be a culprit if you aren't
    ///       handling your types like a := b
    ///       if in some unification you do b := a
    ///       it could end up looping in weird cases
    fn set_type(&mut self, id: usize, mut other: ParsedType) {
        // a := b
        let old = self.stack.set_fresh(id, other.clone());
        // now we have to make sure that we aren't just removing a concreted type
        match old {
            ParsedType::Fresh { .. } => { /* No unification required */ }
            old => self.unify_and_set(&mut other, &old),
        }
    }

    fn unify_and_set<'b>(&mut self, a: &'b mut ParsedType, b: &'b ParsedType) {
        if let ParsedType::Unknown = a {
            *a = b.clone();
        } else {
            for item in self.unify(a, b) {
                self.set_type(item.0, item.1);
            }
        }
    }

    /// Perform unification of a and b
    /// The result should be a substitution that can be performed
    /// giving the result that a := b
    ///
    /// If the types can't be unified it logs an error and continues
    pub fn unify<'b>(&mut self, a: &'b ParsedType, b: &'b ParsedType) -> Vec<(usize, ParsedType)> {
        let mut ret = vec![];
        match (a, b) {
            // TODO: Care about generic args
            (ParsedType::Var { id: ref a_id, .. }, ParsedType::Var { id: ref b_id, .. }) => {
                // @TYPEDEF: TODO: When typedefs come around be smarter here
                if *a_id == *b_id {
                    // Do nothing no unification
                } else {
                    // TODO: Coercian for example int => bool
                    // Type error!
                    warn!("Type Error: Can't unify {:?} and {:?}", a_id, b_id);
                }
            }
            (ParsedType::Fresh { id: ref a_id }, ParsedType::Fresh { id: ref b_id }) => {
                if *a_id != *b_id {
                    ret.push((*a_id, ParsedType::Fresh { id: *b_id }));
                } else {
                    // already unified
                }
            }
            (ParsedType::Fresh { ref id }, ref other)
            | (ref other, ParsedType::Fresh { ref id }) => {
                if self.occurs(*id, other) {
                    // error
                    warn!("Occurs check failed, infinite type in {}", id);
                } else {
                    // id := other
                    ret.push((*id, (*other).clone()));
                }
            }
            (ParsedType::Pointer(ref a), ParsedType::Pointer(ref b)) => ret = self.unify(a, b),
            (
                ParsedType::Func {
                    args: ref a_args,
                    ret: ref a_ret,
                    ..
                },
                ParsedType::Func {
                    args: ref b_args,
                    ret: ref b_ret,
                    ..
                },
            ) => {
                if a_args.len() != b_args.len() {
                    warn!("Types counts don't match up");
                } else {
                    for i in 0..a_args.len() {
                        ret.extend(self.unify(&a_args[i], &b_args[i]));
                    }
                    ret.extend(self.unify(a_ret, b_ret));
                }
            }
            // Missing arrays, TODO:
            // Because eh array unification is hard
            // (it's not just pointers)
            _ => {
                warn!("No unification of {:?} and {:?}", a, b);
            }
        }
        ret
    }
}
