use crate::compiler::ast::*;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};

static FRESH_GENERATOR: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug, Default)]
pub struct Scope {
    pub parent: Option<Rc<RefCell<Scope>>>,
    pub env: Environment,
    // NOTE: we can't collapse this because each defer
    //       is a sepearate scope
    // i.e. defer { i := 0; } defer { i := 2; } is 2 different i's
    pub defer_exprs: Vec<Block>
}

#[derive(Debug, Default)]
pub struct Environment {
    pub fresh_type_env: HashMap<usize, Rc<RefCell<ParsedType>>>,
    pub var_env: HashMap<Ident, Rc<RefCell<ParsedType>>>
}

#[derive(Debug)]
pub struct ScopeStack {
    cur_scope: Rc<RefCell<Scope>>,
    top_scope: Rc<RefCell<Scope>>
}

impl ScopeStack {
    pub fn new(top: &Rc<RefCell<Scope>>) -> ScopeStack {
        ScopeStack {
            top_scope: Rc::clone(&top),
            cur_scope: Rc::clone(&top),
        }
    }

    /// Precondition: scope's parent must be cur_scope
    pub fn push(&mut self, scope: &Rc<RefCell<Scope>>) {
        {
            let scope_borrow = scope.borrow();
            if scope_borrow.parent.is_none() || Rc::ptr_eq(&self.cur_scope, scope_borrow.parent.as_ref().unwrap()) {
                panic!("Precondition of push not fulfilled pushed scope's parent must be current scope");
            }
        }
        self.cur_scope = Rc::clone(&scope);
    }

    pub fn cur(&mut self) -> &mut Rc<RefCell<Scope>> {
        &mut self.cur_scope
    }

    pub fn top(&mut self) -> &mut Rc<RefCell<Scope>> {
        &mut self.top_scope
    }

    pub fn push_new(&mut self) {
        let scope = Scope {
            parent: Some(Rc::clone(&self.cur_scope)),
            env: Environment::default(),
            defer_exprs: vec![]
        };
        self.cur_scope = Rc::new(RefCell::new(scope));
    }

    pub fn pop_scope(&mut self) -> Rc<RefCell<Scope>> {
        let copy = Rc::clone(&self.cur_scope);
        self.cur_scope = Rc::clone(copy.borrow_mut().parent.as_ref()
                                   .expect("All inner scopes must have a parent"));
        copy
    }
}

impl Scope {
    pub fn new_fresh_type() -> ParsedType {
        let id = FRESH_GENERATOR.fetch_add(1, Ordering::Relaxed);
        ParsedType::Fresh{ id }
    }

    pub fn new_var(&mut self, name: Ident, var_type: Option<ParsedType>) -> Option<ParsedType> {
        if self.env.var_env.get(&name).is_none() {
            let ty = match var_type {
                Some(c) => c,
                None => Scope::new_fresh_type()
            };
            self.env.var_env.insert(name, Rc::new(RefCell::new(ty.clone())));
            Some(ty)
        } else {
            None
        }
    }

    pub fn lookup_var_cond(&mut self, name: &Ident) -> Option<Rc<RefCell<ParsedType>>> {
        match self.env.var_env.get(name) {
            Some(ty) => Some(Rc::clone(ty)),
            None => match &self.parent {
                Some(parent) => parent.borrow_mut().lookup_var_cond(name),
                None => None
            }
        }
    }

    pub fn lookup_var(&mut self, name: Ident) -> Rc<RefCell<ParsedType>> {
        self.lookup_var_cond(&name).unwrap_or_else(|| {
            let value = Rc::new(RefCell::new(Scope::new_fresh_type()));
            self.env.var_env.insert(name, Rc::clone(&value));
            value
        })
    }

    pub fn set_fresh(&mut self, id: usize, new_ty: ParsedType) {
        // note if a -> b -> c
        // then we don't want to set 'a' we want to go to the end of the chain
        // and set 'c'
        // this will make types easier on our end
        let outer = self.lookup_fresh(id);
        let ty = outer.borrow();
        match *ty {
            ParsedType::Fresh{id: ref new_id} if *new_id != id => {
                self.set_fresh(*new_id, new_ty);
            },
            _ => {
                // this is overriding the type TypeName = i32;
                // probably should be careful here because we could unintentionally
                // create bugs... so we should do a check of the 2 types to see
                // if they can be unified (i.e. if we have (a -> b) and (int -> c))
                // they could be unified iff b <> c
                // TODO:
                drop(ty);
                let _old = outer.replace(new_ty);
            }
        }
    }

    pub fn lookup_fresh(&mut self, id: usize) -> Rc<RefCell<ParsedType>> {
        match self.env.fresh_type_env.get(&id) {
            Some(ty) => Rc::clone(ty),
            None => match &self.parent {
                Some(parent) => parent.borrow_mut().lookup_fresh(id),
                None => {
                    self.env.fresh_type_env.insert(id, Rc::new(RefCell::new(ParsedType::Fresh{id})));
                    Rc::clone(self.env.fresh_type_env.get(&id).expect("Previously inserted type wasn't inserted"))
                }
            }
        }
    }
}
