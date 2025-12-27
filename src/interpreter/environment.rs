use std::cell::RefCell;
use std::rc::Rc;
use crate::interpreter::value::Value;

#[derive(Clone, Debug)]
pub struct Binding {
    pub value: Value,
    pub is_mutable: bool,
}

#[derive(Clone, Debug)]
pub struct Environment {
    // we used to maintain environment as a stack of bindings.
    // this created a problem when we start having to do stack arithmetic
    // for closure support... at least I couldn't make it work.
    // trying with a different approach, rather than a stack of vectors,
    // we're doing this as parent-child chains of environments
    // So enclosing is the parent scope, None if global
    enclosing: Option<Rc<Environment>>,
    // Values are only values in this scope only, hence only a Vec<Binding>
    values: RefCell<Vec<Binding>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            enclosing: None,
            values: RefCell::new(Vec::new()),
        }
    }

    pub fn new_with_enclosing(enclosing: Rc<Environment>) -> Self {
        Self {
            enclosing: Some(enclosing),
            values: RefCell::new(Vec::new()),
        }
    }

    pub fn define(&self, value: Value, is_mutable: bool) {
        self.values.borrow_mut().push(Binding { value, is_mutable });
    }

    // Helper to walk up the chain 'depth' times
    fn ancestor(&self, depth: usize) -> Option<&Environment> {
        let mut current = self;
        for _ in 0..depth {
            current = current.enclosing.as_ref().expect("Resolved depth exceeds environment chain");
        }
        Some(current)
    }

    pub fn get_at(&self, depth: usize, slot: usize) -> Value {
        let env = self.ancestor(depth).unwrap();
        env.values.borrow()[slot].value.clone()
    }

    pub fn set_at(&self, depth: usize, slot: usize, value: Value) {
        let env = self.ancestor(depth).unwrap();
        env.values.borrow_mut()[slot].value = value;
    }

    pub fn assign_at(&self, depth: usize, slot: usize, value: Value) -> Result<(), String> {
        let env = self.ancestor(depth).unwrap();
        let mut values = env.values.borrow_mut();
        let binding = &mut values[slot];

        if binding.is_mutable {
            binding.value = value;
            Ok(())
        } else {
            Err("Cannot assign to immutable variable".to_string())
        }
    }

    pub fn current_scope_len(&self) -> usize {
        self.values.borrow().len()
    }
}
