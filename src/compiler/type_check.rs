use crate::compiler::scope::*;
use crate::compiler::ast::*;

extern crate log;
use log::{info, warn};

pub struct TypeCheck<'a> {
    stack: &'a mut ScopeStack,
    ctx: Option<ParsedType>
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
    pub fn type_check_program(program: &mut Program, stack: &'a mut ScopeStack) {
        let mut checker = TypeCheck { stack, ctx: None };

        for mut function in program.functions.values_mut() {
            checker.type_check_function(&mut function);
        }
    }

    fn type_check_function(&mut self, func: &mut Function) {
        // TODO: Maybe force args/ret to have concrete type
        let tmp = std::mem::replace(&mut self.ctx, Some(func.ret.clone()));

        self.stack.push(&func.block.scope);
        self.ctx = Some(func.ret.clone());
        self.type_check_block(&mut func.block);
        self.stack.pop();

        self.ctx = tmp;
    }

    fn type_check_block(&mut self, block: &mut Block) {
        for mut statement in block.statements.iter_mut() {
            self.type_check_statement(&mut statement);
        }
        // defers get type checked after the block
        for mut defer in block.scope.borrow_mut().defer_exprs.iter_mut() {
            self.stack.push(&defer.scope);
            self.type_check_block(&mut defer);
            self.stack.pop();
        }
    }

    fn type_check_block_and_unify(&mut self, block: &mut Block, expr: &mut Expr, unify_with: &ParsedType) {
        self.stack.push(&block.scope);
        self.type_check_expr(expr);
        if let Some(ref mut inner) = &mut expr.type_annot {
            self.unify(inner, unify_with);
        }
        self.type_check_block(block);
        self.stack.pop();
    }

    fn type_check_statement(&mut self, statement: &mut Statement) {
        match statement {
            Statement::If{ref mut if_cond, ref mut if_block, ref mut else_if, ref mut else_block} => {
                self.type_check_block_and_unify(if_block, if_cond, &ParsedType::new_simple_var_type("bool"));
                for (ref mut cond, ref mut block) in else_if.iter_mut() {
                    self.type_check_block_and_unify(block, cond, &ParsedType::new_simple_var_type("bool"));
                }

                if let Some(ref mut block) = else_block {
                    self.stack.push(&block.scope);
                    self.type_check_block(block);
                    self.stack.pop();
                }
            },
            Statement::While(ref mut cond, ref mut block) => {
                self.type_check_block_and_unify(block, cond, &ParsedType::new_simple_var_type("bool"));
            },
            Statement::For(ref mut start, ref mut stop, ref mut step, ref mut block) => {
                self.stack.push(&block.scope);

                if let Some(ref mut expr) = start {
                    self.type_check_expr(expr);
                }

                if let Some(ref mut expr) = stop {
                    self.type_check_expr(expr);
                    if let Some(ref mut inner) = &mut expr.type_annot {
                        self.unify(inner, &ParsedType::new_simple_var_type("bool"));
                    }
                }

                if let Some(ref mut expr) = step {
                    self.type_check_expr(expr);
                }

                self.type_check_block(block);

                self.stack.pop();
            },
            Statement::Expr(ref mut inner) => self.type_check_expr(inner),
            Statement::Return(ref mut inner) => {
                self.type_check_expr(inner);
                // unify with the context!!
                if let Some(ctx) = self.ctx.clone() {
                    if let Some(ref mut ty) = &mut inner.type_annot {
                        self.unify(&ctx, ty);
                    }
                }
            },
            Statement::Defer => {}
        }
    }

    fn type_check_expr(&mut self, expr: &mut Expr) {
        
    }

    /* == Unification Algorithm == */

    fn occurs(&mut self, id: usize, other: &ParsedType) -> bool {
        match other {
            // TODO: GEN ARGS in var and func
            ParsedType::Pointer(ref inner) => self.occurs(id, inner),
            ParsedType::Array{ref inner, ..} => self.occurs(id, inner), // TODO: Expr occurs
            ParsedType::Var{..} => false,
            ParsedType::Fresh{id: ref other} => id == *other || match &*self.stack.lookup_fresh(*other).borrow() {
                // if our inner == other and id != other
                // then we have something like a := (b := b)
                // which is clearly fine
                ParsedType::Fresh{id: ref inner} if *inner == *other => false,
                ty => self.occurs(id, &ty),
            },
            ParsedType::Func{args, ret, ..} => args.iter().any(|x| self.occurs(id, x)) || self.occurs(id, ret),
        }
    }

    fn set_type(&mut self, id: usize, other: ParsedType) {
        // a := b
        let old = self.stack.set_fresh(id, other.clone());
        // now we have to make sure that we aren't just removing a concreted type
        match old {
            ParsedType::Fresh{..} => { /* No unification required */ },
            old => self.unify(&other, &old),
        }
    }

    fn unify<'b>(&mut self, a: &'b ParsedType, b: &'b ParsedType) {
        match (a, b) {
            // TODO: Care about generic args
            (ParsedType::Var{id: ref a_id, ..}, ParsedType::Var{id: ref b_id, ..}) => {
                // @TYPEDEF: TODO: When typedefs come around be smarter here
                if *a_id == *b_id {
                    // Do nothing no unification
                } else {
                    // TODO: Coercian for example int => bool
                    // Type error!
                    warn!("Type Error: Can't unify {:?} and {:?}", a_id, b_id);
                }
            },
            (ParsedType::Fresh{id: ref a_id}, ParsedType::Fresh{id: ref b_id}) => {
                if *a_id != *b_id {
                    self.set_type(*a_id, ParsedType::Fresh{id: *b_id});
                } else {
                    // already unified
                }
            },
            (ParsedType::Fresh{ref id}, ref other) | (ref other, ParsedType::Fresh{ref id}) => {
                if self.occurs(*id, other) {
                    // error
                    warn!("Occurs check failed, infinite type in {}", id);
                } else {
                    // id := other
                    self.set_type(*id, (*other).clone());
                }
            },
            (ParsedType::Pointer(ref a), ParsedType::Pointer(ref b)) => self.unify(a, b),
            (ParsedType::Func{args: ref a_args, ret: ref a_ret, ..},
             ParsedType::Func{args: ref b_args, ret: ref b_ret, ..}) => {
                if a_args.len() != b_args.len() {
                    warn!("Types counts don't match up");
                } else {
                    for i in 0..a_args.len() {
                        self.unify(&a_args[i], &b_args[i]);
                    }
                    self.unify(a_ret, b_ret);
                }
            },
            // Missing arrays
            _ => warn!("No unification of {:?} and {:?}", a, b),
        }
    }
}
