use std::collections::HashMap;
use std::collections::hash_map::Entry;
use crate::interpreter::value::Value;

#[derive(Clone)]
pub struct Binding {
    pub value: Value,
    pub is_mutable: bool,
}

#[derive(Clone)]
pub struct Environment {
    scopes: Vec<HashMap<String, Binding>>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            scopes: vec![HashMap::new()],
        }
    }

    pub fn define(&mut self, name: &str, value: Value, is_mutable: bool) -> Result<(), String> {
        let current_scope = self.scopes.last_mut().expect("environment has no scope");

        match current_scope.entry(name.to_string()) {
            Entry::Vacant(e) => {
                e.insert(Binding { value, is_mutable });
                Ok(())
            }
            Entry::Occupied(e) => Err(format!(
                "Variable '{}' already defined in this scope",
                e.key()
            )),
        }
    }

    pub fn get(&self, name: &str) -> Result<Value, String> {
        for scope in self.scopes.iter().rev() {
            if let Some(binding) = scope.get(name) {
                return Ok(binding.value.clone());
            }
        }
        Err(format!("Undefined variable '{}'", name))
    }

    pub fn assign(&mut self, name: &str, value: Value) -> Result<(), String> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(binding) = scope.get_mut(name) {
                if binding.is_mutable {
                    binding.value = value;
                    return Ok(());
                } else {
                    return Err(format!("Cannot assign to immutable variable '{}'", name));
                }
            }
        }
        Err(format!("Undefined variable '{}'", name))
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }
}
