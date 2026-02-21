use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::interp::{Env, Value};
use crate::pkg::lockfile::Lockfile;
use crate::pkg::manifest::Manifest;

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
    /// The root manifest (pkg.oo), if present.
    manifest: Option<Manifest>,
    /// The lockfile (lock.oo), if present.
    lockfile: Option<Lockfile>,
    /// The project root directory (where pkg.oo lives).
    #[allow(dead_code)]
    project_root: Option<PathBuf>,
}

impl ModuleCache {
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
            manifest: None,
            lockfile: None,
            project_root: None,
        }
    }

    /// Create a module cache with a manifest for package-aware resolution.
    pub fn with_manifest(manifest: Manifest) -> Self {
        Self {
            modules: HashMap::new(),
            manifest: Some(manifest),
            lockfile: None,
            project_root: None,
        }
    }

    /// Create a module cache with manifest, lockfile, and project root.
    pub fn with_manifest_and_lockfile(
        manifest: Manifest,
        lockfile: Option<Lockfile>,
        project_root: PathBuf,
    ) -> Self {
        Self {
            modules: HashMap::new(),
            manifest: Some(manifest),
            lockfile,
            project_root: Some(project_root),
        }
    }

    pub fn manifest(&self) -> Option<&Manifest> {
        self.manifest.as_ref()
    }

    pub fn set_manifest(&mut self, manifest: Manifest) {
        self.manifest = Some(manifest);
    }

    pub fn lockfile(&self) -> Option<&Lockfile> {
        self.lockfile.as_ref()
    }

    /// Resolve a dotted module path to a file path.
    /// `"http.server"` with base `/src` → `/src/http/server.oo`
    pub fn resolve_path(module_path: &str, base_dir: &Path) -> PathBuf {
        let parts: Vec<&str> = module_path.split('.').collect();
        let mut path = base_dir.to_path_buf();
        for part in &parts {
            path = path.join(part);
        }
        let oo = path.with_extension("oo");
        if oo.exists() { oo } else { path.with_extension("loon") }
    }

    /// Check if a module path corresponds to a package dependency.
    /// Returns the resolved file path if it's a path dep.
    fn resolve_dep_path(&self, module_path: &str) -> Option<PathBuf> {
        let manifest = self.manifest.as_ref()?;
        let dep = manifest.deps.get(module_path)?;
        let dep_dir = dep.path.as_ref()?;

        // Look for src/lib.oo, src/main.oo, <name>.oo (then .loon fallbacks) in the dep dir
        let candidates = [
            dep_dir.join("src").join("lib.oo"),
            dep_dir.join("src").join("main.oo"),
            dep_dir.join(format!("{module_path}.oo")),
            dep_dir.join("src").join("lib.loon"),
            dep_dir.join("src").join("main.loon"),
            dep_dir.join(format!("{module_path}.loon")),
        ];

        for c in &candidates {
            if c.exists() {
                return Some(c.clone());
            }
        }

        None
    }

    /// Resolve a domain-qualified dependency to a cached file path.
    /// Checks lockfile → cache → fetch (if pkg-fetch feature enabled).
    #[allow(unused_variables)]
    fn resolve_remote_dep(&self, module_path: &str) -> Result<Option<PathBuf>, String> {
        let manifest = self.manifest.as_ref();

        // Check lockfile first — if we have a hash, look in cache
        if let Some(ref lockfile) = self.lockfile {
            if let Some(locked) = lockfile.get(module_path) {
                if let Some(cached) = crate::pkg::fetch::cached_path(&locked.hash) {
                    // Verify cache integrity: re-hash and compare against lockfile
                    #[cfg(feature = "pkg-fetch")]
                    {
                        let actual_hash = crate::pkg::fetch::normalize_and_hash(&cached)
                            .map_err(|e| format!("cache integrity check for '{}': {e}", module_path))?;
                        if actual_hash != locked.hash {
                            return Err(format!(
                                "cache integrity check failed for '{}': expected hash {}, got {}. \
                                 Run `loon cache clean && loon cache warm` to re-fetch.",
                                module_path, &locked.hash[..12.min(locked.hash.len())],
                                &actual_hash[..12.min(actual_hash.len())]
                            ));
                        }
                    }

                    // Find entry point within the cached directory
                    let (_, subpath) = crate::pkg::fetch::parse_source_name(module_path);
                    let dep_dir = if let Some(sub) = subpath.or(locked.subpath.as_deref()) {
                        cached.join(sub)
                    } else {
                        cached
                    };
                    return crate::pkg::fetch::find_entry_point(&dep_dir)
                        .ok_or_else(|| {
                            format!(
                                "no entry point (src/lib.oo or src/main.oo) in cached package '{module_path}'"
                            )
                        })
                        .map(Some);
                }
            }
        }

        // Check manifest deps — if it's a known dep, try to fetch
        #[cfg(feature = "pkg-fetch")]
        if let Some(manifest) = manifest {
            if let Some(dep) = manifest.deps.get(module_path) {
                if !dep.is_path_dep() {
                    let (cached_path, _hash) =
                        crate::pkg::fetch::fetch_and_cache(module_path, dep)?;
                    let (_, subpath) = crate::pkg::fetch::parse_source_name(module_path);
                    let dep_dir = if let Some(sub) = subpath {
                        cached_path.join(sub)
                    } else {
                        cached_path
                    };
                    return crate::pkg::fetch::find_entry_point(&dep_dir)
                        .ok_or_else(|| {
                            format!(
                                "no entry point (src/lib.oo or src/main.oo) in fetched package '{module_path}'"
                            )
                        })
                        .map(Some);
                }
            }
        }

        #[cfg(not(feature = "pkg-fetch"))]
        if let Some(manifest) = manifest {
            if let Some(dep) = manifest.deps.get(module_path) {
                if !dep.is_path_dep() {
                    return Err(format!(
                        "remote dependency '{module_path}' requires the 'pkg-fetch' feature (use loon-cli)"
                    ));
                }
            }
        }

        Ok(None)
    }

    /// Load a module, returning its exports. Uses cache and detects cycles.
    pub fn load_module(
        &mut self,
        module_path: &str,
        base_dir: &Path,
    ) -> Result<ModuleExports, String> {
        // Check if this is a package dependency first (path deps)
        let file_path = if let Some(dep_path) = self.resolve_dep_path(module_path) {
            dep_path
        } else if crate::pkg::fetch::is_domain_qualified(module_path) {
            // Try remote dep resolution
            match self.resolve_remote_dep(module_path)? {
                Some(path) => path,
                None => Self::resolve_path(module_path, base_dir),
            }
        } else {
            Self::resolve_path(module_path, base_dir)
        };

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
