use super::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;

/// SAFETY: Env contains Rc<RefCell<..>> which is not Send/Sync.
/// However, in practice:
/// - WASM: single-threaded, Env never crosses threads
/// - CLI thread spawn: uses deep_clone() to create independent Envs
///   before moving to threads, so the Rc is never actually shared cross-thread.
unsafe impl Send for Env {}
unsafe impl Sync for Env {}

/// Shared global scope — cloning an Env only clones the local scopes,
/// the global scope is shared via Rc.
#[derive(Clone)]
pub struct Env {
    global: Rc<RefCell<HashMap<String, Value>>>,
    local_scopes: Vec<HashMap<String, Value>>,
    pub pub_names: HashSet<String>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            global: Rc::new(RefCell::new(HashMap::new())),
            local_scopes: Vec::new(),
            pub_names: HashSet::new(),
        }
    }

    pub fn push_scope(&mut self) {
        self.local_scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.local_scopes.pop();
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        // Check local scopes first (innermost to outermost)
        for scope in self.local_scopes.iter().rev() {
            if let Some(v) = scope.get(name) {
                return Some(v.clone());
            }
        }
        // Then check global
        self.global.borrow().get(name).cloned()
    }

    pub fn set(&mut self, name: String, value: Value) {
        if let Some(scope) = self.local_scopes.last_mut() {
            scope.insert(name, value);
        } else {
            // No local scopes — set in global
            self.global.borrow_mut().insert(name, value);
        }
    }

    /// Set in the global (bottom) scope — used for defn
    pub fn set_global(&mut self, name: String, value: Value) {
        self.global.borrow_mut().insert(name, value);
    }

    /// Merge global scope from another env (so closures see later defns).
    /// With Rc-shared globals, this is a no-op if they share the same global.
    pub fn merge_globals(&mut self, other: &Env) {
        if Rc::ptr_eq(&self.global, &other.global) {
            return; // Same global — nothing to do
        }
        let other_global = other.global.borrow();
        let mut self_global = self.global.borrow_mut();
        for (k, v) in other_global.iter() {
            if !self_global.contains_key(k) {
                self_global.insert(k.clone(), v.clone());
            }
        }
    }

    /// Get all global bindings (for module exports)
    pub fn globals(&self) -> HashMap<String, Value> {
        self.global.borrow().clone()
    }

    /// Get a reference to the shared global Rc (for sync_global_env)
    pub fn global_rc(&self) -> &Rc<RefCell<HashMap<String, Value>>> {
        &self.global
    }

    /// Create an Env from a shared global Rc (for get_global_env)
    pub fn from_global_rc(global: Rc<RefCell<HashMap<String, Value>>>) -> Self {
        Self {
            global,
            local_scopes: Vec::new(),
            pub_names: HashSet::new(),
        }
    }

    /// Deep clone — creates a fully independent Env with its own global.
    /// Used when spawning threads to avoid sharing Rc cross-thread.
    pub fn deep_clone(&self) -> Self {
        Self {
            global: Rc::new(RefCell::new(self.global.borrow().clone())),
            local_scopes: self.local_scopes.clone(),
            pub_names: self.pub_names.clone(),
        }
    }
}

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}
