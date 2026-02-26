use crate::interpreter::value::Value;
use smallvec::SmallVec;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct Binding {
    pub value: Value,
    pub is_mutable: bool,
}

// Most functions have ≤4 params, so inline storage avoids a heap alloc
type Bindings = SmallVec<[Binding; 4]>;

#[derive(Clone, Debug, Default)]
pub struct Environment {
    enclosing: Option<Rc<Environment>>,
    values: RefCell<Bindings>,
}

impl Environment {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn new_with_enclosing(enclosing: Rc<Environment>) -> Self {
        Self {
            enclosing: Some(enclosing),
            values: RefCell::new(SmallVec::new()),
        }
    }

    /// Create a function-call environment pre-populated with arguments.
    /// Avoids separate define() calls and the extra borrow_mut per arg.
    pub fn new_for_call(enclosing: Rc<Environment>, args: SmallVec<[Value; 4]>) -> Self {
        let bindings: Bindings = args
            .into_iter()
            .map(|value| Binding {
                value,
                is_mutable: false,
            })
            .collect();
        Self {
            enclosing: Some(enclosing),
            values: RefCell::new(bindings),
        }
    }

    /// Reinitialize this environment for reuse (avoids heap allocation).
    pub fn reinit(&mut self, enclosing: Rc<Environment>, args: SmallVec<[Value; 4]>) {
        self.enclosing = Some(enclosing);
        let values = self.values.get_mut(); // &mut access, no RefCell overhead
        values.clear();
        for value in args {
            values.push(Binding {
                value,
                is_mutable: false,
            });
        }
    }

    pub fn define(&self, value: Value, is_mutable: bool) {
        self.values.borrow_mut().push(Binding { value, is_mutable });
    }

    // Helper to walk up the chain 'depth' times
    fn ancestor(&self, depth: usize) -> Option<&Environment> {
        let mut current = self;
        for _ in 0..depth {
            current = current
                .enclosing
                .as_ref()
                .expect("Resolved depth exceeds environment chain");
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
