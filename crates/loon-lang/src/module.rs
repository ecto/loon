use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::interp::{Env, Value};

/// Module loading state for cycle detection
#[derive(Debug, Clone)]
enum ModuleState {
    Loading,
    Loaded(ModuleExports),
}

/// Exported values from a module
#[derive(Debug, Clone)]
pub struct ModuleExports {
    pub values: HashMap<String, Value>,
}

/// Cache for loaded modules
pub struct ModuleCache {
    modules: HashMap<PathBuf, ModuleState>,
}

impl ModuleCache {
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
        }
    }

    /// Resolve a dotted module path to a file path.
    /// `"http.server"` with base `/src` → `/src/http/server.loon`
    pub fn resolve_path(module_path: &str, base_dir: &Path) -> PathBuf {
        let parts: Vec<&str> = module_path.split('.').collect();
        let mut path = base_dir.to_path_buf();
        for part in &parts {
            path = path.join(part);
        }
        path.with_extension("loon")
    }

    /// Load a module, returning its exports. Uses cache and detects cycles.
    pub fn load_module(
        &mut self,
        module_path: &str,
        base_dir: &Path,
    ) -> Result<ModuleExports, String> {
        let file_path = Self::resolve_path(module_path, base_dir);
        let canonical = file_path
            .canonicalize()
            .unwrap_or_else(|_| file_path.clone());

        // Check cache
        if let Some(state) = self.modules.get(&canonical) {
            return match state {
                ModuleState::Loading => {
                    Err(format!("circular dependency: {module_path}"))
                }
                ModuleState::Loaded(exports) => Ok(exports.clone()),
            };
        }

        // Mark as loading for cycle detection
        self.modules.insert(canonical.clone(), ModuleState::Loading);

        // Read and parse
        let source = std::fs::read_to_string(&file_path)
            .map_err(|e| format!("cannot read module '{module_path}' at {}: {e}", file_path.display()))?;

        let exprs = crate::parser::parse(&source)
            .map_err(|e| format!("parse error in module '{module_path}': {}", e.message))?;

        // Eval in a fresh env
        let mut env = Env::new();
        crate::interp::register_builtins_pub(&mut env);
        // Load prelude
        if let Ok(prelude_exprs) = crate::parser::parse(crate::prelude::PRELUDE) {
            for expr in &prelude_exprs {
                let _ = crate::interp::eval(expr, &mut env);
            }
        }

        let module_dir = file_path.parent().unwrap_or(base_dir);
        for expr in &exprs {
            // Handle [use ...] inside imported modules too
            if let crate::ast::ExprKind::List(items) = &expr.kind {
                if !items.is_empty() {
                    if let crate::ast::ExprKind::Symbol(s) = &items[0].kind {
                        if s == "use" {
                            crate::interp::eval_use_with_cache(&items[1..], &mut env, module_dir, self)
                                .map_err(|e| format!("in module '{module_path}': {e}"))?;
                            continue;
                        }
                    }
                }
            }
            crate::interp::eval(expr, &mut env)
                .map_err(|e| format!("in module '{module_path}': {e}"))?;
        }

        // Extract pub exports
        let mut values = HashMap::new();
        for name in &env.pub_names {
            if let Some(val) = env.get(name) {
                values.insert(name.clone(), val);
            }
        }

        // If no pub names declared, export all global user-defined names
        // (excluding builtins) — this makes simple modules work without pub
        if values.is_empty() {
            let builtin_env = {
                let mut e = Env::new();
                crate::interp::register_builtins_pub(&mut e);
                e
            };
            for (name, val) in env.globals() {
                if builtin_env.get(&name).is_none() {
                    values.insert(name, val);
                }
            }
        }

        let exports = ModuleExports { values };
        self.modules
            .insert(canonical, ModuleState::Loaded(exports.clone()));
        Ok(exports)
    }
}

impl Default for ModuleCache {
    fn default() -> Self {
        Self::new()
    }
}
