use super::value::Value;
use std::collections::HashMap;
use std::collections::HashSet;

#[derive(Clone)]
pub struct Env {
    scopes: Vec<HashMap<String, Value>>,
    pub pub_names: HashSet<String>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            pub_names: HashSet::new(),
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(v) = scope.get(name) {
                return Some(v.clone());
            }
        }
        None
    }

    pub fn set(&mut self, name: String, value: Value) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, value);
        }
    }

    /// Set in the global (bottom) scope — used for defn
    pub fn set_global(&mut self, name: String, value: Value) {
        self.scopes[0].insert(name, value);
    }

    /// Merge global scope from another env (so closures see later defns)
    pub fn merge_globals(&mut self, other: &Env) {
        for (k, v) in &other.scopes[0] {
            if !self.scopes[0].contains_key(k) {
                self.scopes[0].insert(k.clone(), v.clone());
            }
        }
    }

    /// Get all global bindings (for module exports)
    pub fn globals(&self) -> &HashMap<String, Value> {
        &self.scopes[0]
    }
}

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}
