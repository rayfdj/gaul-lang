use crate::interpreter::value::Value;

#[derive(Clone, Debug)]
pub struct Binding {
    pub value: Value,
    pub is_mutable: bool,
}

#[derive(Clone, Debug)]
pub struct Environment {
    scopes: Vec<Vec<Binding>>,
}

impl Default for Environment {
    fn default() -> Self {
        Self {
            scopes: vec![Vec::new()],
        }
    }
}

impl Environment {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn define(&mut self, value: Value, is_mutable: bool) {
        self.scopes
            .last_mut()
            .unwrap()
            .push(Binding { value, is_mutable });
    }

    pub fn get_at(&self, depth: usize, slot: usize) -> Value {
        //println!("INTERPRET get_at depth={}, slot={}, scopes.len()={}", depth, slot, self.scopes.len());
        let scope_index = self.scopes.len() - 1 - depth;
        self.scopes[scope_index][slot].value.clone()
    }

    pub fn set_at(&mut self, depth: usize, slot: usize, value: Value) {
        let scope_index = self.scopes.len() - 1 - depth;
        self.scopes[scope_index][slot].value = value;
    }

    pub fn assign_at(&mut self, depth: usize, slot: usize, value: Value) -> Result<(), String> {
        let scope_index = self.scopes.len() - 1 - depth;
        let binding = &mut self.scopes[scope_index][slot];
        if binding.is_mutable {
            binding.value = value;
            Ok(())
        } else {
            Err("Cannot assign to immutable variable".to_string())
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Vec::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn current_scope_len(&self) -> usize {
        self.scopes.last().unwrap().len()
    }
}
