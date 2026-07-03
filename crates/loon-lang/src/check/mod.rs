pub mod ownership;

use crate::ast::{Expr, ExprKind, NodeId};
use crate::errors::codes::ErrorCode;
use crate::errors::LoonDiagnostic;
use crate::module::ModuleCache as ResolveHelper;
use crate::syntax::Span;
use crate::types::*;

use std::cell::RefCell;
use std::collections::{BTreeSet, HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::rc::Rc;

// ── Type-level module cache ──────────────────────────────────────────

/// Exports from a type-checked module.
#[derive(Debug, Clone)]
pub struct TypeModuleExports {
    pub schemes: HashMap<String, Scheme>,
    pub constructors: HashMap<String, Scheme>,
}

#[derive(Debug, Clone)]
enum TypeModuleState {
    Loading,
    Loaded(TypeModuleExports),
}

/// Cache for type-checked modules (shared via Rc<RefCell<>>).
#[derive(Debug)]
pub struct TypeModuleCache {
    modules: HashMap<PathBuf, TypeModuleState>,
}

impl TypeModuleCache {
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
        }
    }
}

impl Default for TypeModuleCache {
    fn default() -> Self {
        Self::new()
    }
}

// ── LSP support types ────────────────────────────────────────────────

/// Information about a definition site (function, let binding, type, constructor).
#[derive(Debug, Clone)]
pub struct DefInfo {
    pub name_span: Span,
    pub file: Option<PathBuf>,
    pub form_span: Span,
}

/// Information about a reference to a name.
#[derive(Debug, Clone)]
pub struct RefInfo {
    pub span: Span,
    pub name: String,
    pub node_id: NodeId,
}

// ── Checker ──────────────────────────────────────────────────────────

pub struct Checker {
    pub subst: Subst,
    pub env: TypeEnv,
    pub errors: Vec<LoonDiagnostic>,
    /// ADT constructor types: name → scheme
    pub constructors: HashMap<String, Scheme>,
    /// ADT type → constructor names (for exhaustiveness checking)
    pub type_constructors: HashMap<String, Vec<String>>,
    /// Type of each expression node (side-table)
    pub type_of: HashMap<NodeId, Type>,
    /// Trait declarations
    pub traits: HashMap<String, TraitDecl>,
    /// Trait implementations: (trait_name, type_name) → method schemes
    pub trait_impls: HashMap<(String, String), HashMap<String, Scheme>>,
    /// Pending [sig] declarations: name → (type, span)
    pub pending_sigs: HashMap<String, (Type, Span)>,
    /// Inferred effect row for each function: name → EffectRow
    /// (resolved at the end of the function's definition)
    pub fn_effects: HashMap<String, EffectRow>,
    /// Effect row of the currently-checked function body (the "ambient"
    /// row). Kept open during inference; effects are added by unifying
    /// through the tail so every row in a body shares one chain.
    current_fn_effects: EffectRow,
    /// Append-only log of every directly performed effect label, in
    /// inference order. `infer_handle` slices it (by index marks) to tell
    /// labels genuinely performed by handler clauses apart from labels the
    /// handle boundary forced into an aliased row (see the scrub step).
    perform_log: Vec<String>,
    /// Registry of declared effects (built-in + user-defined)
    pub effect_registry: crate::effects::EffectRegistry,
    /// Base directory for module resolution (None = no file-system access)
    base_dir: Option<PathBuf>,
    /// Names declared as `pub` in this module
    pub pub_names: HashSet<String>,
    /// Shared cache for type-checked modules
    module_cache: Rc<RefCell<TypeModuleCache>>,
    /// Scoped definition map (for go-to-definition)
    pub definitions: Vec<HashMap<String, DefInfo>>,
    /// All references to names (for go-to-definition lookups)
    pub references: Vec<RefInfo>,
    /// Types with `[derive Copy]` — automatically copy instead of move
    pub derived_copy_types: HashSet<String>,
    /// Expanded program after macro expansion (available after check_program)
    pub expanded_program: Vec<Expr>,
}

impl Checker {
    pub fn new() -> Self {
        let mut checker = Self {
            subst: Subst::new(),
            env: TypeEnv::new(),
            errors: Vec::new(),
            constructors: HashMap::new(),
            type_constructors: HashMap::new(),
            type_of: HashMap::new(),
            traits: HashMap::new(),
            trait_impls: HashMap::new(),
            pending_sigs: HashMap::new(),
            fn_effects: HashMap::new(),
            current_fn_effects: EffectRow::pure(),
            perform_log: Vec::new(),
            effect_registry: crate::effects::EffectRegistry::new(),
            base_dir: None,
            pub_names: HashSet::new(),
            module_cache: Rc::new(RefCell::new(TypeModuleCache::new())),
            definitions: vec![HashMap::new()],
            references: Vec::new(),
            derived_copy_types: HashSet::new(),
            expanded_program: Vec::new(),
        };
        checker.register_builtins();
        checker.register_dom_builtins();
        checker.register_prelude();
        checker.register_physics_builtins();
        checker.effect_polymorphize_builtins();
        // The top-level "ambient" effect row: open so any effect can be
        // performed (and absorbed) at the top level of a program.
        let top_tail = checker.subst.fresh_var();
        checker.current_fn_effects = EffectRow::open(top_tail);
        checker
    }

    /// Make every registered builtin scheme effect-polymorphic: give all
    /// function types in a scheme ONE shared, quantified effect-row tail.
    ///
    /// This is what lets higher-order builtins propagate the effects of
    /// their function arguments: `map : ∀a b e. ((a → b | e), Vec a) → Vec b | e`
    /// — using `map` with an IO lambda makes the call site perform IO, while
    /// using it with a pure lambda leaves the caller pure. First-order
    /// builtins simply become callable in any effect context.
    fn effect_polymorphize_builtins(&mut self) {
        fn open_pure_fn_rows(ty: &Type, tail: TypeVar, changed: &mut bool) -> Type {
            match ty {
                Type::Fn(params, ret, row) => {
                    let new_row = if row.is_pure() {
                        *changed = true;
                        EffectRow::open(tail)
                    } else {
                        row.clone()
                    };
                    Type::Fn(
                        params
                            .iter()
                            .map(|p| open_pure_fn_rows(p, tail, changed))
                            .collect(),
                        Box::new(open_pure_fn_rows(ret, tail, changed)),
                        new_row,
                    )
                }
                Type::Con(name, args) => Type::Con(
                    name.clone(),
                    args.iter()
                        .map(|a| open_pure_fn_rows(a, tail, changed))
                        .collect(),
                ),
                Type::Tuple(items) => Type::Tuple(
                    items
                        .iter()
                        .map(|t| open_pure_fn_rows(t, tail, changed))
                        .collect(),
                ),
                Type::Record(inner) => {
                    Type::Record(Box::new(open_pure_fn_rows(inner, tail, changed)))
                }
                Type::Row(fields, rest) => Type::Row(
                    fields
                        .iter()
                        .map(|(n, t)| (n.clone(), open_pure_fn_rows(t, tail, changed)))
                        .collect(),
                    *rest,
                ),
                _ => ty.clone(),
            }
        }

        let entries: Vec<(String, Scheme)> = self
            .env
            .global_scope()
            .map(|scope| scope.iter().map(|(n, s)| (n.clone(), s.clone())).collect())
            .unwrap_or_default();
        for (name, mut scheme) in entries {
            let tail = self.subst.fresh_var();
            let mut changed = false;
            let new_ty = open_pure_fn_rows(&scheme.ty, tail, &mut changed);
            if changed {
                scheme.ty = new_ty;
                scheme.vars.push(tail);
                self.env.set_global(name, scheme);
            }
        }
    }

    /// Create a checker that can resolve `[use ...]` against the file system.
    pub fn with_base_dir(base_dir: &Path) -> Self {
        let mut c = Self::new();
        c.base_dir = Some(base_dir.to_path_buf());
        c
    }

    /// The base directory used to resolve `[use ...]`, if any.
    pub fn base_dir(&self) -> Option<&Path> {
        self.base_dir.as_deref()
    }

    /// Internal: create a checker for a sub-module that shares the module cache.
    fn for_module(base_dir: &Path, cache: Rc<RefCell<TypeModuleCache>>) -> Self {
        let mut c = Self::new();
        c.base_dir = Some(base_dir.to_path_buf());
        c.module_cache = cache;
        c
    }

    /// Look up the inferred type for a given node.
    pub fn get_type_of(&self, id: NodeId) -> Option<&Type> {
        self.type_of.get(&id)
    }

    /// Record that the current function body performs `label`.
    ///
    /// The label is pushed through the ambient row's tail by unification
    /// (never inserted into the local concrete part) so that every row that
    /// has been linked with the ambient row observes it too.
    fn perform_effect(&mut self, label: &str) {
        self.perform_log.push(label.to_string());
        let want = EffectRow {
            labels: std::iter::once(label.to_string()).collect(),
            tail: Some(self.subst.fresh_var()),
        };
        let ambient = self.current_fn_effects.clone();
        // Cannot fail: the ambient row is always open and `want`'s tail is
        // fresh, so there is no closed side and no shared tail.
        let _ = unify_effect_rows(&mut self.subst, &ambient, &want);
    }

    /// Absorb a callee's effect row into the ambient row of the current
    /// function body.
    ///
    /// - A CLOSED row's labels are definite: the call is implicitly
    ///   "opened" (its labels join the ambient effects without further
    ///   constraining the callee — a pure function may be called anywhere).
    /// - An OPEN row is unified with the ambient row, linking their tails:
    ///   this is what makes a higher-order function's effect depend on its
    ///   argument's effect (`twice` inherits `f`'s row).
    fn absorb_effect_row(&mut self, row: &EffectRow) {
        let resolved = self.subst.resolve_effect_row(row);
        if resolved.tail.is_some() {
            let ambient = self.current_fn_effects.clone();
            if unify_effect_rows(&mut self.subst, &ambient, &resolved).is_ok() {
                return;
            }
            // Defensive fallback: if linking failed (shared tail with
            // differing labels), still record the concrete labels.
        }
        if resolved.labels.is_empty() {
            return;
        }
        let want = EffectRow {
            labels: resolved.labels,
            tail: Some(self.subst.fresh_var()),
        };
        let ambient = self.current_fn_effects.clone();
        let _ = unify_effect_rows(&mut self.subst, &ambient, &want);
    }

    fn register_builtins(&mut self) {
        // Arithmetic: ∀a. Add a => a → a → a
        {
            let a = self.subst.fresh();
            let tv = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            self.subst.add_constraint(
                tv,
                TraitBound {
                    trait_name: "Add".to_string(),
                },
            );
            let add_scheme = Scheme {
                bounds: vec![(
                    tv,
                    vec![TraitBound {
                        trait_name: "Add".to_string(),
                    }],
                )],
                vars: vec![tv],
                ty: Type::Fn(
                    vec![Type::Var(tv), Type::Var(tv)],
                    Box::new(Type::Var(tv)),
                    EffectRow::pure(),
                ),
            };
            for op in ["+", "-", "*"] {
                self.env.set_global(op.to_string(), add_scheme.clone());
            }
        }

        // Comparison: ∀a. Ord a => a → a → Bool
        {
            let a = self.subst.fresh();
            let tv = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            self.subst.add_constraint(
                tv,
                TraitBound {
                    trait_name: "Ord".to_string(),
                },
            );
            let ord_scheme = Scheme {
                bounds: vec![(
                    tv,
                    vec![TraitBound {
                        trait_name: "Ord".to_string(),
                    }],
                )],
                vars: vec![tv],
                ty: Type::Fn(
                    vec![Type::Var(tv), Type::Var(tv)],
                    Box::new(Type::Bool),
                    EffectRow::pure(),
                ),
            };
            for op in [">", "<", ">=", "<="] {
                self.env.set_global(op.to_string(), ord_scheme.clone());
            }
        }

        // Equality / inequality: ∀a. Eq a => a → a → Bool
        for op in ["=", "!="] {
            let a = self.subst.fresh();
            let tv = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            self.subst.add_constraint(
                tv,
                TraitBound {
                    trait_name: "Eq".to_string(),
                },
            );
            self.env.set_global(
                op.to_string(),
                Scheme {
                    bounds: vec![(
                        tv,
                        vec![TraitBound {
                            trait_name: "Eq".to_string(),
                        }],
                    )],
                    vars: vec![tv],
                    ty: Type::Fn(
                        vec![Type::Var(tv), Type::Var(tv)],
                        Box::new(Type::Bool),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // not: Bool → Bool
        self.env.set_global(
            "not".to_string(),
            Scheme::mono(Type::Fn(
                vec![Type::Bool],
                Box::new(Type::Bool),
                EffectRow::pure(),
            )),
        );

        // str: ∀a b. a → b → Str (variadic, approximate as polymorphic)
        {
            let a = self.subst.fresh();
            let b = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            let tvb = if let Type::Var(v) = b {
                v
            } else {
                unreachable!()
            };
            self.env.set_global(
                "str".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tva, tvb],
                    ty: Type::Fn(
                        vec![Type::Var(tva), Type::Var(tvb)],
                        Box::new(Type::Str),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // println: ∀a. a → ()
        {
            let a = self.subst.fresh();
            let tv = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            self.env.set_global(
                "println".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tv],
                    ty: Type::Fn(vec![Type::Var(tv)], Box::new(Type::Unit), EffectRow::pure()),
                },
            );
        }

        // len: ∀a. Vec a → Int
        {
            let a = self.subst.fresh();
            let tv = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            self.env.set_global(
                "len".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tv],
                    ty: Type::Fn(
                        vec![Type::Con("Vec".to_string(), vec![Type::Var(tv)])],
                        Box::new(Type::Int),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // nth: ∀a. Vec a → Int → a
        {
            let a = self.subst.fresh();
            let tv = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            self.env.set_global(
                "nth".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tv],
                    ty: Type::Fn(
                        vec![Type::Con("Vec".to_string(), vec![Type::Var(tv)]), Type::Int],
                        Box::new(Type::Var(tv)),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // range: Int → Int → Vec Int
        self.env.set_global(
            "range".to_string(),
            Scheme::mono(Type::Fn(
                vec![Type::Int, Type::Int],
                Box::new(Type::Con("Vec".to_string(), vec![Type::Int])),
                EffectRow::pure(),
            )),
        );

        // empty?: ∀a. a → Bool (works on Vec, Str, Map, Set)
        {
            let a = self.subst.fresh();
            let tv = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            self.env.set_global(
                "empty?".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tv],
                    ty: Type::Fn(vec![Type::Var(tv)], Box::new(Type::Bool), EffectRow::pure()),
                },
            );
        }

        // contains?: ∀a. Set a → a → Bool
        {
            let a = self.subst.fresh();
            let tv = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            self.env.set_global(
                "contains?".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tv],
                    ty: Type::Fn(
                        vec![
                            Type::Con("Set".to_string(), vec![Type::Var(tv)]),
                            Type::Var(tv),
                        ],
                        Box::new(Type::Bool),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // conj: ∀a. Vec a → a → Vec a
        {
            let a = self.subst.fresh();
            let tv = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            self.env.set_global(
                "conj".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tv],
                    ty: Type::Fn(
                        vec![
                            Type::Con("Vec".to_string(), vec![Type::Var(tv)]),
                            Type::Var(tv),
                        ],
                        Box::new(Type::Con("Vec".to_string(), vec![Type::Var(tv)])),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // get: ∀v. Map Keyword v → Keyword → v
        {
            let v = self.subst.fresh();
            let tv = if let Type::Var(vv) = v {
                vv
            } else {
                unreachable!()
            };
            self.env.set_global(
                "get".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tv],
                    ty: Type::Fn(
                        vec![
                            Type::Con("Map".to_string(), vec![Type::Keyword, Type::Var(tv)]),
                            Type::Keyword,
                        ],
                        Box::new(Type::Var(tv)),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // assoc: ∀v. Map Keyword v → Keyword → v → Map Keyword v
        {
            let v = self.subst.fresh();
            let tv = if let Type::Var(vv) = v {
                vv
            } else {
                unreachable!()
            };
            let map_t = Type::Con("Map".to_string(), vec![Type::Keyword, Type::Var(tv)]);
            self.env.set_global(
                "assoc".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tv],
                    ty: Type::Fn(
                        vec![map_t.clone(), Type::Keyword, Type::Var(tv)],
                        Box::new(map_t),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // map: ∀a b. (a → b) → Vec a → Vec b
        {
            let a = self.subst.fresh();
            let b = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            let tvb = if let Type::Var(v) = b {
                v
            } else {
                unreachable!()
            };
            self.env.set_global(
                "map".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tva, tvb],
                    ty: Type::Fn(
                        vec![
                            Type::Fn(
                                vec![Type::Var(tva)],
                                Box::new(Type::Var(tvb)),
                                EffectRow::pure(),
                            ),
                            Type::Con("Vec".to_string(), vec![Type::Var(tva)]),
                        ],
                        Box::new(Type::Con("Vec".to_string(), vec![Type::Var(tvb)])),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // filter: ∀a. (a → Bool) → Vec a → Vec a
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            self.env.set_global(
                "filter".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tva],
                    ty: Type::Fn(
                        vec![
                            Type::Fn(
                                vec![Type::Var(tva)],
                                Box::new(Type::Bool),
                                EffectRow::pure(),
                            ),
                            Type::Con("Vec".to_string(), vec![Type::Var(tva)]),
                        ],
                        Box::new(Type::Con("Vec".to_string(), vec![Type::Var(tva)])),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // fold: ∀a b. b → (b → a → b) → Vec a → b
        {
            let a = self.subst.fresh();
            let b = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            let tvb = if let Type::Var(v) = b {
                v
            } else {
                unreachable!()
            };
            self.env.set_global(
                "fold".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tva, tvb],
                    ty: Type::Fn(
                        vec![
                            Type::Var(tvb),
                            Type::Fn(
                                vec![Type::Var(tvb), Type::Var(tva)],
                                Box::new(Type::Var(tvb)),
                                EffectRow::pure(),
                            ),
                            Type::Con("Vec".to_string(), vec![Type::Var(tva)]),
                        ],
                        Box::new(Type::Var(tvb)),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // each: ∀a. (a → ()) → Vec a → ()
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            self.env.set_global(
                "each".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tva],
                    ty: Type::Fn(
                        vec![
                            Type::Fn(
                                vec![Type::Var(tva)],
                                Box::new(Type::Unit),
                                EffectRow::pure(),
                            ),
                            Type::Con("Vec".to_string(), vec![Type::Var(tva)]),
                        ],
                        Box::new(Type::Unit),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // collect: ∀a. Vec a → Vec a
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            let vec_a = Type::Con("Vec".to_string(), vec![Type::Var(tva)]);
            self.env.set_global(
                "collect".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tva],
                    ty: Type::Fn(vec![vec_a.clone()], Box::new(vec_a), EffectRow::pure()),
                },
            );
        }

        // assert-eq: ∀a. a → a → ()
        {
            let a = self.subst.fresh();
            let tv = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            self.env.set_global(
                "assert-eq".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tv],
                    ty: Type::Fn(
                        vec![Type::Var(tv), Type::Var(tv)],
                        Box::new(Type::Unit),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // / and %: Int → Int → Int
        let int_bin = Scheme::mono(Type::Fn(
            vec![Type::Int, Type::Int],
            Box::new(Type::Int),
            EffectRow::pure(),
        ));
        for op in ["/", "%"] {
            self.env.set_global(op.to_string(), int_bin.clone());
        }

        // or, and: Bool → Bool → Bool
        let bool_bin = Scheme::mono(Type::Fn(
            vec![Type::Bool, Type::Bool],
            Box::new(Type::Bool),
            EffectRow::pure(),
        ));
        for op in ["or", "and"] {
            self.env.set_global(op.to_string(), bool_bin.clone());
        }

        // print: ∀a. a → ()
        {
            let a = self.subst.fresh();
            let tv = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            self.env.set_global(
                "print".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tv],
                    ty: Type::Fn(vec![Type::Var(tv)], Box::new(Type::Unit), EffectRow::pure()),
                },
            );
        }

        // split: Str → Str → Vec Str
        self.env.set_global(
            "split".to_string(),
            Scheme::mono(Type::Fn(
                vec![Type::Str, Type::Str],
                Box::new(Type::Con("Vec".to_string(), vec![Type::Str])),
                EffectRow::pure(),
            )),
        );

        // join: Str → Vec Str → Str
        self.env.set_global(
            "join".to_string(),
            Scheme::mono(Type::Fn(
                vec![Type::Str, Type::Con("Vec".to_string(), vec![Type::Str])],
                Box::new(Type::Str),
                EffectRow::pure(),
            )),
        );

        // trim: Str → Str
        self.env.set_global(
            "trim".to_string(),
            Scheme::mono(Type::Fn(
                vec![Type::Str],
                Box::new(Type::Str),
                EffectRow::pure(),
            )),
        );

        // starts-with?, ends-with?: Str → Str → Bool
        let str_str_bool = Scheme::mono(Type::Fn(
            vec![Type::Str, Type::Str],
            Box::new(Type::Bool),
            EffectRow::pure(),
        ));
        for op in ["starts-with?", "ends-with?"] {
            self.env.set_global(op.to_string(), str_str_bool.clone());
        }

        // replace: Str → Str → Str → Str
        self.env.set_global(
            "replace".to_string(),
            Scheme::mono(Type::Fn(
                vec![Type::Str, Type::Str, Type::Str],
                Box::new(Type::Str),
                EffectRow::pure(),
            )),
        );

        // uppercase, lowercase: Str → Str
        let str_to_str = Scheme::mono(Type::Fn(
            vec![Type::Str],
            Box::new(Type::Str),
            EffectRow::pure(),
        ));
        for op in ["uppercase", "lowercase"] {
            self.env.set_global(op.to_string(), str_to_str.clone());
        }

        // sort-by: ∀a b. (a → b) → Keyword → Vec a → Vec a
        {
            let a = self.subst.fresh();
            let b = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            let tvb = if let Type::Var(v) = b {
                v
            } else {
                unreachable!()
            };
            let vec_a = Type::Con("Vec".to_string(), vec![Type::Var(tva)]);
            self.env.set_global(
                "sort-by".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tva, tvb],
                    ty: Type::Fn(
                        vec![
                            Type::Fn(
                                vec![Type::Var(tva)],
                                Box::new(Type::Var(tvb)),
                                EffectRow::pure(),
                            ),
                            Type::Keyword,
                            vec_a.clone(),
                        ],
                        Box::new(vec_a),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // take: ∀a. Int → Vec a → Vec a
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            let vec_a = Type::Con("Vec".to_string(), vec![Type::Var(tva)]);
            self.env.set_global(
                "take".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tva],
                    ty: Type::Fn(
                        vec![Type::Int, vec_a.clone()],
                        Box::new(vec_a),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // drop: ∀a. Int → Vec a → Vec a
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            let vec_a = Type::Con("Vec".to_string(), vec![Type::Var(tva)]);
            self.env.set_global(
                "drop".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tva],
                    ty: Type::Fn(
                        vec![Type::Int, vec_a.clone()],
                        Box::new(vec_a),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // reverse: ∀a. Vec a → Vec a
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            let vec_a = Type::Con("Vec".to_string(), vec![Type::Var(tva)]);
            self.env.set_global(
                "reverse".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tva],
                    ty: Type::Fn(vec![vec_a.clone()], Box::new(vec_a), EffectRow::pure()),
                },
            );
        }

        // flatten: ∀a. Vec (Vec a) → Vec a
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            let vec_a = Type::Con("Vec".to_string(), vec![Type::Var(tva)]);
            self.env.set_global(
                "flatten".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tva],
                    ty: Type::Fn(
                        vec![Type::Con("Vec".to_string(), vec![vec_a.clone()])],
                        Box::new(vec_a),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // chunk: ∀a. Int → Vec a → Vec (Vec a)
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            let vec_a = Type::Con("Vec".to_string(), vec![Type::Var(tva)]);
            self.env.set_global(
                "chunk".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tva],
                    ty: Type::Fn(
                        vec![Type::Int, vec_a.clone()],
                        Box::new(Type::Con("Vec".to_string(), vec![vec_a])),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // zip: ∀a b. Vec a → Vec b → Vec (a, b)
        {
            let a = self.subst.fresh();
            let b = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            let tvb = if let Type::Var(v) = b {
                v
            } else {
                unreachable!()
            };
            self.env.set_global(
                "zip".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tva, tvb],
                    ty: Type::Fn(
                        vec![
                            Type::Con("Vec".to_string(), vec![Type::Var(tva)]),
                            Type::Con("Vec".to_string(), vec![Type::Var(tvb)]),
                        ],
                        Box::new(Type::Con(
                            "Vec".to_string(),
                            vec![Type::Tuple(vec![Type::Var(tva), Type::Var(tvb)])],
                        )),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // find: ∀a. (a → Bool) → Vec a → Option a
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            self.env.set_global(
                "find".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tva],
                    ty: Type::Fn(
                        vec![
                            Type::Fn(
                                vec![Type::Var(tva)],
                                Box::new(Type::Bool),
                                EffectRow::pure(),
                            ),
                            Type::Con("Vec".to_string(), vec![Type::Var(tva)]),
                        ],
                        Box::new(Type::Con("Option".to_string(), vec![Type::Var(tva)])),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // any?: ∀a. (a → Bool) → Vec a → Bool
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            self.env.set_global(
                "any?".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tva],
                    ty: Type::Fn(
                        vec![
                            Type::Fn(
                                vec![Type::Var(tva)],
                                Box::new(Type::Bool),
                                EffectRow::pure(),
                            ),
                            Type::Con("Vec".to_string(), vec![Type::Var(tva)]),
                        ],
                        Box::new(Type::Bool),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // all?: ∀a. (a → Bool) → Vec a → Bool
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            self.env.set_global(
                "all?".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tva],
                    ty: Type::Fn(
                        vec![
                            Type::Fn(
                                vec![Type::Var(tva)],
                                Box::new(Type::Bool),
                                EffectRow::pure(),
                            ),
                            Type::Con("Vec".to_string(), vec![Type::Var(tva)]),
                        ],
                        Box::new(Type::Bool),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // update: ∀v. Map Keyword v → Keyword → (v → v) → Map Keyword v
        {
            let v = self.subst.fresh();
            let tv = if let Type::Var(vv) = v {
                vv
            } else {
                unreachable!()
            };
            let map_t = Type::Con("Map".to_string(), vec![Type::Keyword, Type::Var(tv)]);
            self.env.set_global(
                "update".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tv],
                    ty: Type::Fn(
                        vec![
                            map_t.clone(),
                            Type::Keyword,
                            Type::Fn(
                                vec![Type::Var(tv)],
                                Box::new(Type::Var(tv)),
                                EffectRow::pure(),
                            ),
                        ],
                        Box::new(map_t),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // entries: ∀k v. Map k v → Vec (k, v)
        {
            let k = self.subst.fresh();
            let v = self.subst.fresh();
            let tvk = if let Type::Var(vv) = k {
                vv
            } else {
                unreachable!()
            };
            let tvv = if let Type::Var(vv) = v {
                vv
            } else {
                unreachable!()
            };
            self.env.set_global(
                "entries".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tvk, tvv],
                    ty: Type::Fn(
                        vec![Type::Con(
                            "Map".to_string(),
                            vec![Type::Var(tvk), Type::Var(tvv)],
                        )],
                        Box::new(Type::Con(
                            "Vec".to_string(),
                            vec![Type::Tuple(vec![Type::Var(tvk), Type::Var(tvv)])],
                        )),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // keys: ∀k v. Map k v → Vec k
        {
            let k = self.subst.fresh();
            let v = self.subst.fresh();
            let tvk = if let Type::Var(vv) = k {
                vv
            } else {
                unreachable!()
            };
            let tvv = if let Type::Var(vv) = v {
                vv
            } else {
                unreachable!()
            };
            self.env.set_global(
                "keys".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tvk, tvv],
                    ty: Type::Fn(
                        vec![Type::Con(
                            "Map".to_string(),
                            vec![Type::Var(tvk), Type::Var(tvv)],
                        )],
                        Box::new(Type::Con("Vec".to_string(), vec![Type::Var(tvk)])),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // values: ∀k v. Map k v → Vec v
        {
            let k = self.subst.fresh();
            let v = self.subst.fresh();
            let tvk = if let Type::Var(vv) = k {
                vv
            } else {
                unreachable!()
            };
            let tvv = if let Type::Var(vv) = v {
                vv
            } else {
                unreachable!()
            };
            self.env.set_global(
                "values".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tvk, tvv],
                    ty: Type::Fn(
                        vec![Type::Con(
                            "Map".to_string(),
                            vec![Type::Var(tvk), Type::Var(tvv)],
                        )],
                        Box::new(Type::Con("Vec".to_string(), vec![Type::Var(tvv)])),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // merge: ∀v. Map Keyword v → Map Keyword v → Map Keyword v
        {
            let v = self.subst.fresh();
            let tv = if let Type::Var(vv) = v {
                vv
            } else {
                unreachable!()
            };
            let map_t = Type::Con("Map".to_string(), vec![Type::Keyword, Type::Var(tv)]);
            self.env.set_global(
                "merge".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tv],
                    ty: Type::Fn(
                        vec![map_t.clone(), map_t.clone()],
                        Box::new(map_t),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // remove: ∀v. Map Keyword v → Keyword → Map Keyword v
        {
            let v = self.subst.fresh();
            let tv = if let Type::Var(vv) = v {
                vv
            } else {
                unreachable!()
            };
            let map_t = Type::Con("Map".to_string(), vec![Type::Keyword, Type::Var(tv)]);
            self.env.set_global(
                "remove".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tv],
                    ty: Type::Fn(
                        vec![map_t.clone(), Type::Keyword],
                        Box::new(map_t),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // push!: ∀a. Vec a → a → Vec a
        {
            let a = self.subst.fresh();
            let tv = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            let vec_a = Type::Con("Vec".to_string(), vec![Type::Var(tv)]);
            self.env.set_global(
                "push!".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tv],
                    ty: Type::Fn(
                        vec![vec_a.clone(), Type::Var(tv)],
                        Box::new(vec_a),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // int: Str → Int
        self.env.set_global(
            "int".to_string(),
            Scheme::mono(Type::Fn(
                vec![Type::Str],
                Box::new(Type::Int),
                EffectRow::pure(),
            )),
        );

        // float: Str → Float
        self.env.set_global(
            "float".to_string(),
            Scheme::mono(Type::Fn(
                vec![Type::Str],
                Box::new(Type::Float),
                EffectRow::pure(),
            )),
        );

        // char-at: Str → Int → Str
        self.env.set_global(
            "char-at".to_string(),
            Scheme::mono(Type::Fn(
                vec![Type::Str, Type::Int],
                Box::new(Type::Str),
                EffectRow::pure(),
            )),
        );

        // substring: Str → Int → Int → Str
        self.env.set_global(
            "substring".to_string(),
            Scheme::mono(Type::Fn(
                vec![Type::Str, Type::Int, Type::Int],
                Box::new(Type::Str),
                EffectRow::pure(),
            )),
        );

        // contains?: Str → Str → Bool
        self.env.set_global(
            "contains?".to_string(),
            Scheme::mono(Type::Fn(
                vec![Type::Str, Type::Str],
                Box::new(Type::Bool),
                EffectRow::pure(),
            )),
        );

        // index-of: Str → Str → Int
        self.env.set_global(
            "index-of".to_string(),
            Scheme::mono(Type::Fn(
                vec![Type::Str, Type::Str],
                Box::new(Type::Int),
                EffectRow::pure(),
            )),
        );

        // group-by: ∀a k. (a → k) → Vec a → Map k (Vec a)
        {
            let a = self.subst.fresh();
            let k = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            let tvk = if let Type::Var(v) = k {
                v
            } else {
                unreachable!()
            };
            self.env.set_global(
                "group-by".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tva, tvk],
                    ty: Type::Fn(
                        vec![
                            Type::Fn(
                                vec![Type::Var(tva)],
                                Box::new(Type::Var(tvk)),
                                EffectRow::pure(),
                            ),
                            Type::Con("Vec".to_string(), vec![Type::Var(tva)]),
                        ],
                        Box::new(Type::Con(
                            "Map".to_string(),
                            vec![
                                Type::Var(tvk),
                                Type::Con("Vec".to_string(), vec![Type::Var(tva)]),
                            ],
                        )),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // flat-map: ∀a b. (a → Vec b) → Vec a → Vec b
        {
            let a = self.subst.fresh();
            let b = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            let tvb = if let Type::Var(v) = b {
                v
            } else {
                unreachable!()
            };
            self.env.set_global(
                "flat-map".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tva, tvb],
                    ty: Type::Fn(
                        vec![
                            Type::Fn(
                                vec![Type::Var(tva)],
                                Box::new(Type::Con("Vec".to_string(), vec![Type::Var(tvb)])),
                                EffectRow::pure(),
                            ),
                            Type::Con("Vec".to_string(), vec![Type::Var(tva)]),
                        ],
                        Box::new(Type::Con("Vec".to_string(), vec![Type::Var(tvb)])),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // sort: ∀a. Vec a → Vec a
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            let vec_a = Type::Con("Vec".to_string(), vec![Type::Var(tva)]);
            self.env.set_global(
                "sort".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tva],
                    ty: Type::Fn(vec![vec_a.clone()], Box::new(vec_a), EffectRow::pure()),
                },
            );
        }

        // min, max: ∀a. Vec a → a
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            let vec_a = Type::Con("Vec".to_string(), vec![Type::Var(tva)]);
            for name in ["min", "max"] {
                self.env.set_global(
                    name.to_string(),
                    Scheme {
                        bounds: vec![],
                        vars: vec![tva],
                        ty: Type::Fn(
                            vec![vec_a.clone()],
                            Box::new(Type::Var(tva)),
                            EffectRow::pure(),
                        ),
                    },
                );
            }
        }

        // sum: Vec Int → Int (approximate)
        self.env.set_global(
            "sum".to_string(),
            Scheme::mono(Type::Fn(
                vec![Type::Con("Vec".to_string(), vec![Type::Int])],
                Box::new(Type::Int),
                EffectRow::pure(),
            )),
        );

        // str: ∀a. a → Str
        {
            let a = self.subst.fresh();
            let tv = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            self.env.set_global(
                "str".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tv],
                    ty: Type::Fn(vec![Type::Var(tv)], Box::new(Type::Str), EffectRow::pure()),
                },
            );
        }

        // into-map: ∀k v. Vec (k, v) → Map k v
        {
            let k = self.subst.fresh();
            let v = self.subst.fresh();
            let tvk = if let Type::Var(vv) = k {
                vv
            } else {
                unreachable!()
            };
            let tvv = if let Type::Var(vv) = v {
                vv
            } else {
                unreachable!()
            };
            self.env.set_global(
                "into-map".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tvk, tvv],
                    ty: Type::Fn(
                        vec![Type::Con(
                            "Vec".to_string(),
                            vec![Type::Tuple(vec![Type::Var(tvk), Type::Var(tvv)])],
                        )],
                        Box::new(Type::Con(
                            "Map".to_string(),
                            vec![Type::Var(tvk), Type::Var(tvv)],
                        )),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // channel: () → (Tx a, Rx a)
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            self.env.set_global(
                "channel".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tva],
                    ty: Type::Fn(
                        vec![],
                        Box::new(Type::Tuple(vec![
                            Type::Con("Tx".to_string(), vec![Type::Var(tva)]),
                            Type::Con("Rx".to_string(), vec![Type::Var(tva)]),
                        ])),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // send: Tx a → a → ()
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            self.env.set_global(
                "send".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tva],
                    ty: Type::Fn(
                        vec![
                            Type::Con("Tx".to_string(), vec![Type::Var(tva)]),
                            Type::Var(tva),
                        ],
                        Box::new(Type::Unit),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // recv: Rx a → a
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            self.env.set_global(
                "recv".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tva],
                    ty: Type::Fn(
                        vec![Type::Con("Rx".to_string(), vec![Type::Var(tva)])],
                        Box::new(Type::Var(tva)),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // name: Keyword → Str
        self.env.set_global(
            "name".to_string(),
            Scheme::mono(Type::Fn(
                vec![Type::Keyword],
                Box::new(Type::Str),
                EffectRow::pure(),
            )),
        );

        // keyword: Str → Keyword
        self.env.set_global(
            "keyword".to_string(),
            Scheme::mono(Type::Fn(
                vec![Type::Str],
                Box::new(Type::Keyword),
                EffectRow::pure(),
            )),
        );

        // keywordize-keys: ∀a. Map<Str,a> → Map<Keyword,a>
        // Simplified: ∀a. a → a (maps aren't parameterized in the checker yet)
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            self.env.set_global(
                "keywordize-keys".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tva],
                    ty: Type::Fn(
                        vec![Type::Var(tva)],
                        Box::new(Type::Var(tva)),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // map?: ∀a. a → Bool
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            self.env.set_global(
                "map?".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tva],
                    ty: Type::Fn(
                        vec![Type::Var(tva)],
                        Box::new(Type::Bool),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // vec?: ∀a. a → Bool
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            self.env.set_global(
                "vec?".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tva],
                    ty: Type::Fn(
                        vec![Type::Var(tva)],
                        Box::new(Type::Bool),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // cons: ∀a. a → Vec a → Vec a
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            let vec_a = Type::Con("Vec".to_string(), vec![Type::Var(tva)]);
            self.env.set_global(
                "cons".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tva],
                    ty: Type::Fn(
                        vec![Type::Var(tva), vec_a.clone()],
                        Box::new(vec_a),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // HashMap.new: ∀k v. () → Map k v
        {
            let k = self.subst.fresh();
            let v = self.subst.fresh();
            let tvk = if let Type::Var(vv) = k {
                vv
            } else {
                unreachable!()
            };
            let tvv = if let Type::Var(vv) = v {
                vv
            } else {
                unreachable!()
            };
            self.env.set_global(
                "HashMap.new".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tvk, tvv],
                    ty: Type::Fn(
                        vec![],
                        Box::new(Type::Con(
                            "Map".to_string(),
                            vec![Type::Var(tvk), Type::Var(tvv)],
                        )),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // try-recv: ∀a. Rx a → Option a
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            self.env.set_global(
                "try-recv".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tva],
                    ty: Type::Fn(
                        vec![Type::Con("Rx".to_string(), vec![Type::Var(tva)])],
                        Box::new(Type::Con("Option".to_string(), vec![Type::Var(tva)])),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // sqrt: Float → Float
        self.env.set_global(
            "sqrt".to_string(),
            Scheme::mono(Type::Fn(
                vec![Type::Float],
                Box::new(Type::Float),
                EffectRow::pure(),
            )),
        );

        // pow: Float → Float → Float
        self.env.set_global(
            "pow".to_string(),
            Scheme::mono(Type::Fn(
                vec![Type::Float, Type::Float],
                Box::new(Type::Float),
                EffectRow::pure(),
            )),
        );

        // abs: Float → Float
        self.env.set_global(
            "abs".to_string(),
            Scheme::mono(Type::Fn(
                vec![Type::Float],
                Box::new(Type::Float),
                EffectRow::pure(),
            )),
        );

        // first: ∀a. Vec a → a
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            self.env.set_global(
                "first".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tva],
                    ty: Type::Fn(
                        vec![Type::Con("Vec".to_string(), vec![Type::Var(tva)])],
                        Box::new(Type::Var(tva)),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // last: ∀a. Vec a → a
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            self.env.set_global(
                "last".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tva],
                    ty: Type::Fn(
                        vec![Type::Con("Vec".to_string(), vec![Type::Var(tva)])],
                        Box::new(Type::Var(tva)),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // some?: ∀a. a → Bool
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            self.env.set_global(
                "some?".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tva],
                    ty: Type::Fn(
                        vec![Type::Var(tva)],
                        Box::new(Type::Bool),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // none?: ∀a. a → Bool (complement of some?)
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            self.env.set_global(
                "none?".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tva],
                    ty: Type::Fn(
                        vec![Type::Var(tva)],
                        Box::new(Type::Bool),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // nil?: ∀a. a → Bool
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            self.env.set_global(
                "nil?".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tva],
                    ty: Type::Fn(
                        vec![Type::Var(tva)],
                        Box::new(Type::Bool),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // type-of: ∀a. a → Str
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            self.env.set_global(
                "type-of".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tva],
                    ty: Type::Fn(vec![Type::Var(tva)], Box::new(Type::Str), EffectRow::pure()),
                },
            );
        }
    }

    /// Register type signatures for all DOM builtins.
    pub fn register_dom_builtins(&mut self) {
        // dom/create-element: Str → Int
        self.env.set_global(
            "dom.create-element".to_string(),
            Scheme::mono(Type::Fn(
                vec![Type::Str],
                Box::new(Type::Int),
                EffectRow::pure(),
            )),
        );

        // dom/create-text: Str → Int
        self.env.set_global(
            "dom.create-text".to_string(),
            Scheme::mono(Type::Fn(
                vec![Type::Str],
                Box::new(Type::Int),
                EffectRow::pure(),
            )),
        );

        // dom/set-attribute: Int → Str → Str → ()
        self.env.set_global(
            "dom.set-attribute".to_string(),
            Scheme::mono(Type::Fn(
                vec![Type::Int, Type::Str, Type::Str],
                Box::new(Type::Unit),
                EffectRow::pure(),
            )),
        );

        // dom/set-style: Int → Str → Str → ()
        self.env.set_global(
            "dom.set-style".to_string(),
            Scheme::mono(Type::Fn(
                vec![Type::Int, Type::Str, Type::Str],
                Box::new(Type::Unit),
                EffectRow::pure(),
            )),
        );

        // dom/append-child: Int → Int → ()
        self.env.set_global(
            "dom.append-child".to_string(),
            Scheme::mono(Type::Fn(
                vec![Type::Int, Type::Int],
                Box::new(Type::Unit),
                EffectRow::pure(),
            )),
        );

        // dom/remove-child: Int → Int → ()
        self.env.set_global(
            "dom.remove-child".to_string(),
            Scheme::mono(Type::Fn(
                vec![Type::Int, Type::Int],
                Box::new(Type::Unit),
                EffectRow::pure(),
            )),
        );

        // dom/replace-child: Int → Int → Int → ()
        self.env.set_global(
            "dom.replace-child".to_string(),
            Scheme::mono(Type::Fn(
                vec![Type::Int, Type::Int, Type::Int],
                Box::new(Type::Unit),
                EffectRow::pure(),
            )),
        );

        // dom/set-text: Int → Str → ()
        self.env.set_global(
            "dom.set-text".to_string(),
            Scheme::mono(Type::Fn(
                vec![Type::Int, Type::Str],
                Box::new(Type::Unit),
                EffectRow::pure(),
            )),
        );

        // dom/query-selector: Str → Int
        self.env.set_global(
            "dom.query-selector".to_string(),
            Scheme::mono(Type::Fn(
                vec![Type::Str],
                Box::new(Type::Int),
                EffectRow::pure(),
            )),
        );

        // dom/set-inner-html: Int → Str → ()
        self.env.set_global(
            "dom.set-inner-html".to_string(),
            Scheme::mono(Type::Fn(
                vec![Type::Int, Type::Str],
                Box::new(Type::Unit),
                EffectRow::pure(),
            )),
        );

        // dom/add-listener: ∀a. Int → Str → (a → ()) → Int
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            self.env.set_global(
                "dom.add-listener".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tva],
                    ty: Type::Fn(
                        vec![
                            Type::Int,
                            Type::Str,
                            Type::Fn(
                                vec![Type::Var(tva)],
                                Box::new(Type::Unit),
                                EffectRow::pure(),
                            ),
                        ],
                        Box::new(Type::Int),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // dom/remove-listener: Int → ()
        self.env.set_global(
            "dom.remove-listener".to_string(),
            Scheme::mono(Type::Fn(
                vec![Type::Int],
                Box::new(Type::Unit),
                EffectRow::pure(),
            )),
        );

        // dom/get-value: Int → Str
        self.env.set_global(
            "dom.get-value".to_string(),
            Scheme::mono(Type::Fn(
                vec![Type::Int],
                Box::new(Type::Str),
                EffectRow::pure(),
            )),
        );

        // dom/set-value: Int → Str → ()
        self.env.set_global(
            "dom.set-value".to_string(),
            Scheme::mono(Type::Fn(
                vec![Type::Int, Type::Str],
                Box::new(Type::Unit),
                EffectRow::pure(),
            )),
        );

        // dom/eval-loon: Str → Str
        self.env.set_global(
            "dom.eval-loon".to_string(),
            Scheme::mono(Type::Fn(
                vec![Type::Str],
                Box::new(Type::Str),
                EffectRow::pure(),
            )),
        );

        // dom/set-title: Str → ()
        self.env.set_global(
            "dom.set-title".to_string(),
            Scheme::mono(Type::Fn(
                vec![Type::Str],
                Box::new(Type::Unit),
                EffectRow::pure(),
            )),
        );

        // dom/push-state: Str → ()
        self.env.set_global(
            "dom.push-state".to_string(),
            Scheme::mono(Type::Fn(
                vec![Type::Str],
                Box::new(Type::Unit),
                EffectRow::pure(),
            )),
        );

        // dom/location: () → Str
        self.env.set_global(
            "dom.location".to_string(),
            Scheme::mono(Type::Fn(vec![], Box::new(Type::Str), EffectRow::pure())),
        );

        // dom/request-animation-frame: ∀a. (a → ()) → ()
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            self.env.set_global(
                "dom.request-animation-frame".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tva],
                    ty: Type::Fn(
                        vec![Type::Fn(
                            vec![Type::Var(tva)],
                            Box::new(Type::Unit),
                            EffectRow::pure(),
                        )],
                        Box::new(Type::Unit),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // dom/set-timeout: ∀a. (a → ()) → Int → ()
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            self.env.set_global(
                "dom.set-timeout".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tva],
                    ty: Type::Fn(
                        vec![
                            Type::Fn(
                                vec![Type::Var(tva)],
                                Box::new(Type::Unit),
                                EffectRow::pure(),
                            ),
                            Type::Int,
                        ],
                        Box::new(Type::Unit),
                        EffectRow::pure(),
                    ),
                },
            );
        }
    }

    fn register_prelude(&mut self) {
        // Parse and check the prelude to register Option/Result types
        if let Ok(exprs) = crate::parser::parse(crate::prelude::PRELUDE) {
            for expr in &exprs {
                self.infer(expr);
            }
        }

        // Register built-in trait declarations
        self.traits.insert(
            "Add".to_string(),
            TraitDecl {
                name: "Add".to_string(),
                type_params: vec![],
                methods: vec![TraitMethod {
                    name: "add".to_string(),
                    param_types: vec![
                        Type::Con("Self".to_string(), vec![]),
                        Type::Con("Self".to_string(), vec![]),
                    ],
                    ret_type: Type::Con("Self".to_string(), vec![]),
                }],
            },
        );
        self.traits.insert(
            "Eq".to_string(),
            TraitDecl {
                name: "Eq".to_string(),
                type_params: vec![],
                methods: vec![TraitMethod {
                    name: "eq".to_string(),
                    param_types: vec![
                        Type::Con("Self".to_string(), vec![]),
                        Type::Con("Self".to_string(), vec![]),
                    ],
                    ret_type: Type::Bool,
                }],
            },
        );
        self.traits.insert(
            "Ord".to_string(),
            TraitDecl {
                name: "Ord".to_string(),
                type_params: vec![],
                methods: vec![TraitMethod {
                    name: "lt".to_string(),
                    param_types: vec![
                        Type::Con("Self".to_string(), vec![]),
                        Type::Con("Self".to_string(), vec![]),
                    ],
                    ret_type: Type::Bool,
                }],
            },
        );
        self.traits.insert(
            "Display".to_string(),
            TraitDecl {
                name: "Display".to_string(),
                type_params: vec![],
                methods: vec![TraitMethod {
                    name: "display".to_string(),
                    param_types: vec![Type::Con("Self".to_string(), vec![])],
                    ret_type: Type::Str,
                }],
            },
        );

        // Register primitive trait impls
        let empty = std::collections::HashMap::new();
        for ty in ["Int", "Float"] {
            self.trait_impls
                .insert(("Add".to_string(), ty.to_string()), empty.clone());
            self.trait_impls
                .insert(("Ord".to_string(), ty.to_string()), empty.clone());
        }
        for ty in ["Int", "Float", "Bool", "String", "Keyword"] {
            self.trait_impls
                .insert(("Eq".to_string(), ty.to_string()), empty.clone());
        }
        for ty in ["Int", "Float", "Bool", "String", "Keyword"] {
            self.trait_impls
                .insert(("Display".to_string(), ty.to_string()), empty.clone());
        }
    }

    fn register_physics_builtins(&mut self) {
        use crate::types::Dimension;

        // unit: special-cased in infer_list, but registered here for parity checks
        // Signature is approximate — actual inference is done in infer_list
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            self.env.set_global(
                "unit".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tva],
                    ty: Type::Fn(
                        vec![Type::Var(tva), Type::Keyword],
                        Box::new(Type::Var(tva)),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // magnitude: special-cased in infer_list, registered for parity
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a {
                v
            } else {
                unreachable!()
            };
            self.env.set_global(
                "magnitude".to_string(),
                Scheme {
                    bounds: vec![],
                    vars: vec![tva],
                    ty: Type::Fn(
                        vec![Type::Var(tva)],
                        Box::new(Type::Float),
                        EffectRow::pure(),
                    ),
                },
            );
        }

        // scalar: Float → Dim(Scalar)
        self.env.set_global(
            "scalar".to_string(),
            Scheme::mono(Type::Fn(
                vec![Type::Float],
                Box::new(Type::Dim(Dimension::SCALAR)),
                EffectRow::pure(),
            )),
        );

        // Physics constants (namespaced via dot access — registered as qualified names)
        // Speed of light: Velocity
        self.env.set_global(
            "Const.c".to_string(),
            Scheme::mono(Type::Dim(Dimension {
                length: 1,
                time: -1,
                ..Dimension::SCALAR
            })),
        );
        // Gravitational constant: m³/(kg·s²)
        self.env.set_global(
            "Const.G".to_string(),
            Scheme::mono(Type::Dim(Dimension {
                mass: -1,
                length: 3,
                time: -2,
                ..Dimension::SCALAR
            })),
        );
        // Planck's constant: Energy·Time = kg·m²/s
        self.env.set_global(
            "Const.h".to_string(),
            Scheme::mono(Type::Dim(Dimension {
                mass: 1,
                length: 2,
                time: -1,
                ..Dimension::SCALAR
            })),
        );
        // Boltzmann constant: Energy/Temperature = kg·m²/(s²·K)
        self.env.set_global(
            "Const.k-B".to_string(),
            Scheme::mono(Type::Dim(Dimension {
                mass: 1,
                length: 2,
                time: -2,
                temperature: -1,
                ..Dimension::SCALAR
            })),
        );
        // Elementary charge: Charge = A·s
        self.env.set_global(
            "Const.e-charge".to_string(),
            Scheme::mono(Type::Dim(Dimension {
                current: 1,
                time: 1,
                ..Dimension::SCALAR
            })),
        );

        // Register trait impls for Dim
        let empty = std::collections::HashMap::new();
        for trait_name in ["Add", "Ord", "Eq", "Display"] {
            self.trait_impls
                .insert((trait_name.to_string(), "Dim".to_string()), empty.clone());
        }
    }

    /// Infer type of dimensional arithmetic operations.
    fn infer_dim_arithmetic(&mut self, op: &str, lhs: &Type, rhs: &Type, span: Span) -> Type {
        use crate::types::Dimension;
        match op {
            "+" | "-" => match (lhs, rhs) {
                (Type::Dim(d1), Type::Dim(d2)) => {
                    if d1 == d2 {
                        Type::Dim(d1.clone())
                    } else {
                        let result_dim = d1.div(d2);
                        let hint = if result_dim.name() != "Dim" {
                            format!(
                                "\n    = hint: did you mean {}? try [/ a b]",
                                result_dim.name()
                            )
                        } else {
                            let mul_dim = d1.mul(d2);
                            if mul_dim.name() != "Dim" {
                                format!(
                                    "\n    = hint: did you mean {}? try [* a b]",
                                    mul_dim.name()
                                )
                            } else {
                                String::new()
                            }
                        };
                        self.errors.push(
                            LoonDiagnostic::new(
                                ErrorCode::E0208,
                                format!(
                                    "cannot {} {} and {}",
                                    if op == "+" { "add" } else { "subtract" },
                                    d1.name(),
                                    d2.name()
                                ),
                            )
                            .with_why(format!(
                                "{} ({}) and {} ({}) are incompatible dimensions{}",
                                d1.name(),
                                d1,
                                d2.name(),
                                d2,
                                hint
                            ))
                            .with_label(
                                span,
                                "dimension mismatch",
                                true,
                            ),
                        );
                        self.subst.fresh()
                    }
                }
                (Type::Dim(d), _) => {
                    self.errors.push(
                            LoonDiagnostic::new(
                                ErrorCode::E0208,
                                format!("cannot {} {} and non-dimensional type", if op == "+" { "add" } else { "subtract" }, d.name()),
                            )
                            .with_why("cannot mix dimensional and non-dimensional values in addition/subtraction")
                            .with_fix("use [magnitude x] to extract the numeric value, or [scalar n] to enter the physics world")
                            .with_label(span, "dimension mismatch", true),
                        );
                    self.subst.fresh()
                }
                (_, Type::Dim(d)) => {
                    self.errors.push(
                            LoonDiagnostic::new(
                                ErrorCode::E0208,
                                format!("cannot {} non-dimensional type and {}", if op == "+" { "add" } else { "subtract" }, d.name()),
                            )
                            .with_why("cannot mix dimensional and non-dimensional values in addition/subtraction")
                            .with_fix("use [magnitude x] to extract the numeric value, or [scalar n] to enter the physics world")
                            .with_label(span, "dimension mismatch", true),
                        );
                    self.subst.fresh()
                }
                _ => unreachable!(),
            },
            "*" => {
                match (lhs, rhs) {
                    (Type::Dim(d1), Type::Dim(d2)) => Type::Dim(d1.mul(d2)),
                    (Type::Dim(d), Type::Float | Type::Int)
                    | (Type::Float | Type::Int, Type::Dim(d)) => Type::Dim(d.clone()),
                    (Type::Dim(d), Type::Var(_)) | (Type::Var(_), Type::Dim(d)) => {
                        // Polymorphic: Dim * unknown → Dim (assume scalar multiplier)
                        Type::Dim(d.clone())
                    }
                    _ => unreachable!(),
                }
            }
            "/" => {
                match (lhs, rhs) {
                    (Type::Dim(d1), Type::Dim(d2)) => {
                        // No-dimensionless rule: always returns Dim, even if d1==d2 → Scalar
                        Type::Dim(d1.div(d2))
                    }
                    (Type::Dim(d), Type::Float | Type::Int) => Type::Dim(d.clone()),
                    (Type::Float | Type::Int, Type::Dim(d)) => Type::Dim(Dimension::SCALAR.div(d)),
                    (Type::Dim(d), Type::Var(_)) => Type::Dim(d.clone()),
                    (Type::Var(_), Type::Dim(d)) => Type::Dim(Dimension::SCALAR.div(d)),
                    _ => unreachable!(),
                }
            }
            ">" | "<" | ">=" | "<=" => match (lhs, rhs) {
                (Type::Dim(d1), Type::Dim(d2)) => {
                    if d1 == d2 {
                        Type::Bool
                    } else {
                        self.errors.push(
                            LoonDiagnostic::new(
                                ErrorCode::E0208,
                                format!("cannot compare {} and {}", d1.name(), d2.name()),
                            )
                            .with_why(format!(
                                "{} and {} have incompatible dimensions",
                                d1.name(),
                                d2.name()
                            ))
                            .with_label(
                                span,
                                "dimension mismatch",
                                true,
                            ),
                        );
                        Type::Bool
                    }
                }
                (Type::Dim(d), _) | (_, Type::Dim(d)) => {
                    self.errors.push(
                        LoonDiagnostic::new(
                            ErrorCode::E0208,
                            format!("cannot compare {} with non-dimensional type", d.name()),
                        )
                        .with_label(span, "dimension mismatch", true),
                    );
                    Type::Bool
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    /// Convert a TypeError from unify() into a LoonDiagnostic and push it.
    fn push_unify_error(&mut self, e: TypeError, span: Span) {
        let code = if e.message.contains("infinite type") {
            ErrorCode::E0203
        } else if e.message.contains("arity mismatch") {
            ErrorCode::E0202
        } else if e.message.contains("field mismatch") || e.message.contains("missing fields") {
            ErrorCode::E0207
        } else if e.message.contains("effect mismatch") || e.message.contains("infinite effect row")
        {
            ErrorCode::E0403
        } else {
            ErrorCode::E0200
        };
        let mut diag = LoonDiagnostic::new(code, &e.message).with_label(span, &e.message, true);
        if code == ErrorCode::E0200 {
            diag = diag
                .with_why("the types are incompatible")
                .with_fix("ensure the types match");
        } else if code == ErrorCode::E0202 {
            diag = diag
                .with_why("the function expects a different number of arguments")
                .with_fix("pass the correct number of arguments");
        } else if code == ErrorCode::E0203 {
            diag = diag
                .with_why("a type variable refers to itself, creating an infinite loop")
                .with_fix("break the cycle by adding a type annotation or restructuring");
        } else if code == ErrorCode::E0207 {
            diag = diag
                .with_why("the record fields do not match")
                .with_fix("add or remove fields to make the records compatible");
        } else if code == ErrorCode::E0403 {
            diag = diag
                .with_why("the effect rows are incompatible: one side performs an effect the other does not allow")
                .with_fix("handle the effect with a `handle` block, or widen the effect annotation (e.g. add the effect to the `#{...}` set)");
        }
        self.errors.push(diag);
    }

    fn push_scope(&mut self) {
        self.env.push_scope();
        self.definitions.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.env.pop_scope();
        if self.definitions.len() > 1 {
            self.definitions.pop();
        }
    }

    fn add_definition(&mut self, name: &str, name_span: Span, form_span: Span) {
        let info = DefInfo {
            name_span,
            file: None,
            form_span,
        };
        if let Some(scope) = self.definitions.last_mut() {
            scope.insert(name.to_string(), info);
        }
    }

    /// Look up definition info for a name across all scopes (innermost first).
    pub fn lookup_definition(&self, name: &str) -> Option<&DefInfo> {
        for scope in self.definitions.iter().rev() {
            if let Some(info) = scope.get(name) {
                return Some(info);
            }
        }
        None
    }

    /// Infer the type of an expression, recording it in the type side-table.
    pub fn infer(&mut self, expr: &Expr) -> Type {
        let ty = self.infer_inner(expr);
        self.type_of.insert(expr.id, ty.clone());
        ty
    }

    fn infer_inner(&mut self, expr: &Expr) -> Type {
        match &expr.kind {
            ExprKind::Int(_) => Type::Int,
            ExprKind::Float(_) => Type::Float,
            ExprKind::Bool(_) => Type::Bool,
            ExprKind::Str(_) => Type::Str,
            ExprKind::Keyword(_) => Type::Keyword,

            ExprKind::Symbol(name) => {
                if let Some(scheme) = self.env.get(name) {
                    self.references.push(RefInfo {
                        span: expr.span,
                        name: name.clone(),
                        node_id: expr.id,
                    });
                    instantiate(&mut self.subst, scheme)
                } else {
                    self.errors.push(
                        LoonDiagnostic::new(ErrorCode::E0201, format!("unbound symbol '{name}'"))
                            .with_why(format!("'{name}' is not defined in this scope"))
                            .with_fix("check the spelling or add a definition")
                            .with_label(expr.span, format!("'{name}' not found"), true),
                    );
                    self.subst.fresh()
                }
            }

            ExprKind::Vec(items) => {
                let elem = self.subst.fresh();
                for item in items {
                    let t = self.infer(item);
                    if let Err(e) = unify(&mut self.subst, &elem, &t) {
                        self.push_unify_error(e, expr.span);
                    }
                }
                Type::Con("Vec".to_string(), vec![elem])
            }

            ExprKind::Set(items) => {
                let elem = self.subst.fresh();
                for item in items {
                    let t = self.infer(item);
                    if let Err(e) = unify(&mut self.subst, &elem, &t) {
                        self.push_unify_error(e, expr.span);
                    }
                }
                Type::Con("Set".to_string(), vec![elem])
            }

            ExprKind::Map(pairs) => {
                // Check if all keys are keywords — if so, infer a Record type
                let all_keywords = !pairs.is_empty()
                    && pairs
                        .iter()
                        .all(|(k, _)| matches!(&k.kind, ExprKind::Keyword(_)));
                if all_keywords {
                    self.infer_record_literal(pairs, expr.span)
                } else {
                    let key_t = self.subst.fresh();
                    let val_t = self.subst.fresh();
                    for (k, v) in pairs {
                        let kt = self.infer(k);
                        let vt = self.infer(v);
                        if let Err(e) = unify(&mut self.subst, &key_t, &kt) {
                            self.push_unify_error(e, expr.span);
                        }
                        if let Err(e) = unify(&mut self.subst, &val_t, &vt) {
                            self.push_unify_error(e, expr.span);
                        }
                    }
                    Type::Con("Map".to_string(), vec![key_t, val_t])
                }
            }

            ExprKind::Tuple(items) => {
                let types: Vec<Type> = items.iter().map(|e| self.infer(e)).collect();
                Type::Tuple(types)
            }

            ExprKind::DotAccess(_, _) => {
                // Try qualified name lookup first (e.g. math.double from [use math])
                if let Some(path) = expr.as_dotted_path() {
                    if let Some(scheme) = self.env.get(&path) {
                        self.references.push(RefInfo {
                            span: expr.span,
                            name: path,
                            node_id: expr.id,
                        });
                        return instantiate(&mut self.subst, scheme);
                    }
                }
                // Fall back to record field access
                if let ExprKind::DotAccess(inner, field) = &expr.kind {
                    let inner_ty = self.infer(inner);
                    self.infer_record_get(&inner_ty, field, expr.span)
                } else {
                    unreachable!()
                }
            }

            // Quasiquote nodes should be expanded before type checking
            ExprKind::Quote(_) | ExprKind::Unquote(_) | ExprKind::UnquoteSplice(_) => Type::Unit,

            ExprKind::List(items) if items.is_empty() => Type::Unit,

            ExprKind::List(items) => self.infer_list(items, expr.span),
        }
    }

    /// Infer a record literal from a map where all keys are keywords.
    /// Produces Record(Row([("field1", T1), ("field2", T2), ...], None))
    fn infer_record_literal(&mut self, pairs: &[(Expr, Expr)], _span: Span) -> Type {
        let fields: Vec<(String, Type)> = pairs
            .iter()
            .map(|(k, v)| {
                let name = if let ExprKind::Keyword(s) = &k.kind {
                    s.clone()
                } else {
                    unreachable!("infer_record_literal called with non-keyword key")
                };
                let vt = self.infer(v);
                (name, vt)
            })
            .collect();
        Type::Record(Box::new(Type::Row(fields, None)))
    }

    /// Infer the type of accessing a field on a record via `get`.
    /// Uses row unification to support structural subtyping.
    fn infer_record_get(&mut self, rec_ty: &Type, field_name: &str, span: Span) -> Type {
        let field_ty = self.subst.fresh();
        let rest = self.subst.fresh_var();
        // Build an open row type that requires the given field
        let expected_row = Type::Row(vec![(field_name.to_string(), field_ty.clone())], Some(rest));
        let expected_record = Type::Record(Box::new(expected_row));
        if let Err(e) = unify(&mut self.subst, rec_ty, &expected_record) {
            self.push_unify_error(e, span);
        }
        self.subst.resolve(&field_ty)
    }

    fn infer_list(&mut self, items: &[Expr], span: Span) -> Type {
        let head = &items[0];
        if let ExprKind::Symbol(s) = &head.kind {
            match s.as_str() {
                "fn" => return self.infer_fn(&items[1..], span),
                "let" => return self.infer_let(&items[1..]),
                "if" => return self.infer_if(&items[1..], span),
                "do" => return self.infer_do(&items[1..]),
                "match" => return self.infer_match(&items[1..], span),
                "pipe" => return self.infer_pipe(&items[1..], span),
                "type" => return self.infer_type_def(&items[1..]),
                "effect" => return self.infer_effect_def(&items[1..], span),
                "trait" => return self.infer_trait_def(&items[1..]),
                "impl" => return self.infer_impl_def(&items[1..], span),
                "sig" => return self.infer_sig(&items[1..], span),
                "handle" => return self.infer_handle(&items[1..], span),
                "try" => {
                    // [try body on-fail] — returns type of body
                    if items.len() >= 3 {
                        let body_ty = self.infer(&items[1]);
                        self.infer(&items[2]); // on-fail handler
                        return body_ty;
                    }
                    return Type::Unit;
                }
                // str is variadic: any number of args → Str
                "str" => {
                    for a in &items[1..] {
                        self.infer(a);
                    }
                    return Type::Str;
                }
                "use" => return self.infer_use(&items[1..], span),
                "pub" => {
                    // Track the name being made public
                    if items.len() > 2 {
                        if let ExprKind::Symbol(kind) = &items[1].kind {
                            if matches!(kind.as_str(), "fn" | "let" | "type") {
                                if let ExprKind::Symbol(name) = &items[2].kind {
                                    self.pub_names.insert(name.clone());
                                }
                            }
                        }
                    }
                    if items.len() > 1 {
                        return self.infer_list(&items[1..], span);
                    }
                    return Type::Unit;
                }
                "test" | "mut" => {
                    if items.len() > 1 {
                        return self.infer_list(&items[1..], span);
                    }
                    return Type::Unit;
                }
                "derive" => return self.infer_derive(&items[1..], span),
                "macro" | "macro+" | "macroexpand" => return Type::Unit,
                "catch-errors" => {
                    // [catch-errors expr] — arg should be Str, returns Vec of error maps
                    if items.len() >= 2 {
                        let arg_ty = self.infer(&items[1]);
                        if let Err(e) = unify(&mut self.subst, &arg_ty, &Type::Str) {
                            self.push_unify_error(e, items[1].span);
                        }
                    }
                    let elem = self.subst.fresh();
                    return Type::Con("Vec".to_string(), vec![elem]);
                }
                // Record field access: [get record :key]
                "get" if items.len() == 3 => {
                    if let ExprKind::Keyword(field_name) = &items[2].kind {
                        let rec_ty = self.infer(&items[1]);
                        let resolved = self.subst.resolve(&rec_ty);
                        // Use row-based get for Record types or unresolved type vars
                        if matches!(resolved, Type::Record(_) | Type::Var(_)) {
                            return self.infer_record_get(&rec_ty, field_name, span);
                        }
                    }
                }
                _ => {}
            }
        }

        // Check for Effect.op pattern via DotAccess (e.g. IO.read-file)
        if let ExprKind::DotAccess(obj, op) = &head.kind {
            if let ExprKind::Symbol(effect) = &obj.kind {
                if effect.starts_with(char::is_uppercase) {
                    self.perform_effect(effect);
                    // Look up operation in registry for type checking
                    if let Some(op_def) = self.effect_registry.get_op(effect, op).cloned() {
                        // Type-check arguments against declared param types
                        let arg_types: Vec<Type> =
                            items[1..].iter().map(|a| self.infer(a)).collect();
                        if arg_types.len() != op_def.params.len() {
                            self.errors.push(
                                LoonDiagnostic::new(
                                    ErrorCode::E0202,
                                    format!(
                                        "`{effect}.{op}` expects {} argument(s), got {}",
                                        op_def.params.len(),
                                        arg_types.len()
                                    ),
                                )
                                .with_label(
                                    span,
                                    "effect operation call",
                                    true,
                                ),
                            );
                        } else {
                            for (i, ((_pname, pty), arg_ty)) in
                                op_def.params.iter().zip(arg_types.iter()).enumerate()
                            {
                                if let Some(ty_name) = pty {
                                    let expected = self.resolve_type_name(ty_name);
                                    if let Err(e) = unify(&mut self.subst, arg_ty, &expected) {
                                        self.push_unify_error(e, items[1 + i].span);
                                    }
                                }
                            }
                        }
                        // Return declared return type or fresh
                        return match &op_def.return_type {
                            Some(ty_name) => self.resolve_type_name(ty_name),
                            None => self.subst.fresh(),
                        };
                    } else if self.effect_registry.has_effect(effect) {
                        // Effect exists but operation unknown
                        self.errors.push(
                            LoonDiagnostic::new(
                                ErrorCode::E0402,
                                format!("effect `{effect}` has no operation `{op}`"),
                            )
                            .with_label(
                                span,
                                "unknown operation",
                                true,
                            ),
                        );
                        for a in &items[1..] {
                            self.infer(a);
                        }
                        return self.subst.fresh();
                    } else {
                        // Unknown effect — still allow it (might be defined later or external)
                        for a in &items[1..] {
                            self.infer(a);
                        }
                        return self.subst.fresh();
                    }
                }
            }
        }

        // Special-case: [unit value :keyword] — physics unit constructor
        if let ExprKind::Symbol(s) = &head.kind {
            if s == "unit" && items.len() == 3 {
                let val_ty = self.infer(&items[1]);
                // Value must be numeric
                let resolved_val = self.subst.resolve(&val_ty);
                if !matches!(resolved_val, Type::Float | Type::Int | Type::Var(_)) {
                    self.errors.push(
                        LoonDiagnostic::new(
                            ErrorCode::E0200,
                            "unit: first argument must be a number".to_string(),
                        )
                        .with_label(
                            items[1].span,
                            "expected Float or Int",
                            true,
                        ),
                    );
                }
                // Second arg must be a keyword
                if let ExprKind::Keyword(unit_name) = &items[2].kind {
                    use crate::types::Dimension;
                    const D: Dimension = Dimension::SCALAR;
                    let dim = match unit_name.as_str() {
                        // Base SI
                        "m" => Some(Dimension::length()),
                        "s" => Some(Dimension::time()),
                        "kg" => Some(Dimension::mass()),
                        "A" => Some(Dimension::current()),
                        "K" => Some(Dimension::temperature()),
                        // Derived
                        "N" => Some(Dimension {
                            mass: 1,
                            length: 1,
                            time: -2,
                            ..D
                        }),
                        "J" => Some(Dimension {
                            mass: 1,
                            length: 2,
                            time: -2,
                            ..D
                        }),
                        "W" => Some(Dimension {
                            mass: 1,
                            length: 2,
                            time: -3,
                            ..D
                        }),
                        "Pa" => Some(Dimension {
                            mass: 1,
                            length: -1,
                            time: -2,
                            ..D
                        }),
                        "Hz" => Some(Dimension { time: -1, ..D }),
                        "C" => Some(Dimension {
                            current: 1,
                            time: 1,
                            ..D
                        }),
                        "V" => Some(Dimension {
                            mass: 1,
                            length: 2,
                            time: -3,
                            current: -1,
                            ..D
                        }),
                        "ohm" => Some(Dimension {
                            mass: 1,
                            length: 2,
                            time: -3,
                            current: -2,
                            ..D
                        }),
                        // Prefixed length
                        "km" | "cm" | "mm" => Some(Dimension::length()),
                        // Prefixed time
                        "ms" | "us" | "ns" => Some(Dimension::time()),
                        // Prefixed mass
                        "g" | "mg" => Some(Dimension::mass()),
                        // Prefixed force
                        "kN" => Some(Dimension {
                            mass: 1,
                            length: 1,
                            time: -2,
                            ..D
                        }),
                        // Prefixed pressure
                        "kPa" | "MPa" | "GPa" => Some(Dimension {
                            mass: 1,
                            length: -1,
                            time: -2,
                            ..D
                        }),
                        // Prefixed power
                        "kW" => Some(Dimension {
                            mass: 1,
                            length: 2,
                            time: -3,
                            ..D
                        }),
                        // Prefixed current
                        "mA" => Some(Dimension::current()),
                        // Area/Volume helpers
                        "m2" => Some(Dimension { length: 2, ..D }),
                        "m3" => Some(Dimension { length: 3, ..D }),
                        _ => None,
                    };
                    if let Some(d) = dim {
                        return Type::Dim(d);
                    } else {
                        self.errors.push(
                            LoonDiagnostic::new(
                                ErrorCode::E0201,
                                format!("unknown unit :{unit_name}"),
                            )
                            .with_label(
                                items[2].span,
                                "unknown unit",
                                true,
                            ),
                        );
                        return self.subst.fresh();
                    }
                } else {
                    self.errors.push(
                        LoonDiagnostic::new(
                            ErrorCode::E0200,
                            "unit: second argument must be a keyword (e.g. :m, :kg, :s)"
                                .to_string(),
                        )
                        .with_label(
                            items[2].span,
                            "expected keyword",
                            true,
                        ),
                    );
                    return self.subst.fresh();
                }
            }

            // Special-case: [magnitude expr] — explicit exit from Dim world
            if s == "magnitude" && items.len() == 2 {
                let arg_ty = self.infer(&items[1]);
                let resolved = self.subst.resolve(&arg_ty);
                if !matches!(resolved, Type::Dim(_) | Type::Var(_)) {
                    self.errors.push(
                        LoonDiagnostic::new(
                            ErrorCode::E0200,
                            "magnitude: argument must be a dimensional type".to_string(),
                        )
                        .with_label(
                            items[1].span,
                            "expected dimensional type",
                            true,
                        ),
                    );
                }
                return Type::Float;
            }

            // Dimensional arithmetic intercept
            if matches!(s.as_str(), "+" | "-" | "*" | "/" | ">" | "<" | ">=" | "<=")
                && items.len() == 3
            {
                let lhs_ty = self.infer(&items[1]);
                let rhs_ty = self.infer(&items[2]);
                let lhs = self.subst.resolve(&lhs_ty);
                let rhs = self.subst.resolve(&rhs_ty);
                if matches!(&lhs, Type::Dim(_)) || matches!(&rhs, Type::Dim(_)) {
                    return self.infer_dim_arithmetic(s, &lhs, &rhs, span);
                }
                // Dimensional polymorphism: Float * type_var → preserve type_var
                if matches!(s.as_str(), "*" | "/") {
                    let lhs_is_scalar = matches!(&lhs, Type::Float | Type::Int);
                    let rhs_is_scalar = matches!(&rhs, Type::Float | Type::Int);
                    if lhs_is_scalar && matches!(&rhs, Type::Var(_)) {
                        return rhs;
                    }
                    if rhs_is_scalar && matches!(&lhs, Type::Var(_)) {
                        return lhs;
                    }
                }
            }
        }

        // Function application
        let func_ty = self.infer(head);
        let arg_types: Vec<Type> = items[1..].iter().map(|a| self.infer(a)).collect();
        let ret = self.subst.fresh();

        // The application's effect row: a fresh open row that unification
        // will bind to the callee's (instantiated) row.
        let app_row = EffectRow::open(self.subst.fresh_var());
        let expected_fn = Type::Fn(arg_types, Box::new(ret.clone()), app_row.clone());
        if let Err(e) = unify(&mut self.subst, &func_ty, &expected_fn) {
            self.push_unify_error(e, span);
        }

        // Propagate the callee's effects into the ambient row.
        self.absorb_effect_row(&app_row);

        // Belt-and-braces: also union the definition-time concrete labels
        // recorded for named functions (covers paths where the row is not
        // threaded through the type, e.g. multi-arity functions).
        if let ExprKind::Symbol(callee_name) = &head.kind {
            if let Some(callee_effects) = self.fn_effects.get(callee_name) {
                let labels = callee_effects.labels.clone();
                if !labels.is_empty() {
                    self.absorb_effect_row(&EffectRow::closed(labels));
                }
            }
        }

        ret
    }

    fn infer_fn(&mut self, args: &[Expr], span: Span) -> Type {
        if args.is_empty() {
            return self.subst.fresh();
        }

        // If first arg is a symbol, treat as named function
        if let ExprKind::Symbol(name) = &args[0].kind {
            return self.infer_named_fn(name.clone(), &args[1..], args[0].span, span);
        }

        // Otherwise anonymous lambda: [fn [params] body...]
        if let ExprKind::List(params) = &args[0].kind {
            // The lambda gets its own ambient effect row: defining an
            // effectful lambda does not perform its effects — calling it
            // does. The row travels on the lambda's function type.
            let lam_tail = self.subst.fresh_var();
            let saved_effects =
                std::mem::replace(&mut self.current_fn_effects, EffectRow::open(lam_tail));

            self.push_scope();
            let param_types = self.infer_params(params);

            let mut body_ty = Type::Unit;
            for expr in &args[1..] {
                body_ty = self.infer(expr);
            }
            self.pop_scope();

            let row = std::mem::replace(&mut self.current_fn_effects, saved_effects);
            return Type::Fn(param_types, Box::new(body_ty), row);
        }

        self.subst.fresh()
    }

    fn infer_named_fn(&mut self, name: String, args: &[Expr], name_span: Span, span: Span) -> Type {
        if args.is_empty() {
            return Type::Unit;
        }

        // Record definition
        self.add_definition(&name, name_span, span);

        // Save the enclosing ambient row; this function body gets its own
        // fresh open row (its effects live on its type, not the caller's).
        let fn_tail = self.subst.fresh_var();
        let saved_effects =
            std::mem::replace(&mut self.current_fn_effects, EffectRow::open(fn_tail));

        // Multi-arity check
        if matches!(args[0].kind, ExprKind::Tuple(_)) {
            let ret = self.subst.fresh();
            for clause_expr in &args[0..] {
                if let ExprKind::Tuple(clause_items) = &clause_expr.kind {
                    if clause_items.len() >= 2 {
                        let clause_ret = self.infer_fn_clause(&clause_items[0], &clause_items[1..]);
                        if let Err(e) = unify(&mut self.subst, &ret, &clause_ret) {
                            self.push_unify_error(e, span);
                        }
                    }
                }
            }
            let scheme = generalize(&self.env, &self.subst, &ret);
            self.env.set_global(name.clone(), scheme);
            // Store inferred effects and restore
            let inferred = std::mem::replace(&mut self.current_fn_effects, saved_effects);
            let inferred = self.subst.resolve_effect_row(&inferred);
            self.fn_effects.insert(name, inferred);
            return Type::Unit;
        }

        // Single-arity
        if let ExprKind::List(params) = &args[0].kind {
            // Parse effect annotation: #{IO Fail}
            let mut body_start = 1;
            let mut declared_effects: Option<EffectSet> = None;
            if body_start < args.len() {
                if matches!(&args[body_start].kind, ExprKind::Set(_) | ExprKind::Map(_)) {
                    declared_effects = Some(self.parse_effect_set(&args[body_start]));
                    body_start += 1;
                }
            }

            self.push_scope();
            let param_types = self.infer_params(params);

            let temp_ret = self.subst.fresh();
            // The self-reference for recursion carries the ambient row, so
            // recursive calls unify the row with itself (a no-op). It also
            // keeps the ambient tail free in the env during the body, which
            // stops inner `let`s from generalizing over it.
            let temp_fn_ty = Type::Fn(
                param_types.clone(),
                Box::new(temp_ret.clone()),
                self.current_fn_effects.clone(),
            );
            self.env.set(name.clone(), Scheme::mono(temp_fn_ty));

            let mut body_ty = Type::Unit;
            for body_expr in &args[body_start..] {
                body_ty = self.infer(body_expr);
            }

            if let Err(e) = unify(&mut self.subst, &temp_ret, &body_ty) {
                self.push_unify_error(e, span);
            }

            self.pop_scope();

            // The function's type carries its body's effect row. An
            // unresolved tail generalizes with the type variables below,
            // giving effect polymorphism: each use instantiates it fresh.
            let fn_ty = Type::Fn(
                param_types,
                Box::new(body_ty),
                self.current_fn_effects.clone(),
            );

            // Check against pending sig if present
            if let Some((sig_ty, sig_span)) = self.pending_sigs.remove(&name) {
                if let Err(_e) = unify(&mut self.subst, &fn_ty, &sig_ty) {
                    let resolved_fn = self.subst.resolve(&fn_ty);
                    let resolved_sig = self.subst.resolve(&sig_ty);
                    self.errors.push(
                        LoonDiagnostic::new(
                            ErrorCode::E0204,
                            format!(
                                "inferred type `{}` does not match declared signature `{}`",
                                resolved_fn, resolved_sig
                            ),
                        )
                        .with_why("the function body infers a different type than declared")
                        .with_fix("update the signature or the function body to match")
                        .with_label(
                            sig_span,
                            "signature declared here",
                            true,
                        ),
                    );
                }
            }

            // Store inferred effects (resolved: concrete labels + any
            // still-open tail)
            let inferred = std::mem::replace(&mut self.current_fn_effects, saved_effects);
            let inferred = self.subst.resolve_effect_row(&inferred);

            // Check declared effects if present (assertion mode: every
            // inferred concrete label must be declared)
            if let Some(ref declared) = declared_effects {
                {
                    for eff in &inferred.labels {
                        if !declared.contains(eff) {
                            self.errors.push(
                                LoonDiagnostic::new(
                                    ErrorCode::E0401,
                                    format!("function `{name}` performs undeclared effect `{eff}`"),
                                )
                                .with_why(format!("effect `{eff}` is used but not listed in the effect annotation"))
                                .with_fix(format!("add `{eff}` to the effect set: / #{{{eff} ...}}"))
                                .with_label(span, "function with missing effect", true),
                            );
                        }
                    }
                }
            }

            self.fn_effects.insert(name.clone(), inferred);

            let scheme = generalize(&self.env, &self.subst, &fn_ty);
            self.env.set_global(name, scheme);
        } else {
            // Restore effects if params weren't a list
            self.current_fn_effects = saved_effects;
        }
        Type::Unit
    }

    /// Infer [effect Name [op-name [ParamType ...] ReturnType] ...]
    fn infer_effect_def(&mut self, args: &[Expr], span: Span) -> Type {
        if args.is_empty() {
            return Type::Unit;
        }
        let name = match &args[0].kind {
            ExprKind::Symbol(s) => s.clone(),
            _ => return Type::Unit,
        };
        // Effect names must start with uppercase
        if !name.starts_with(char::is_uppercase) {
            self.errors.push(
                LoonDiagnostic::new(
                    ErrorCode::E0200,
                    format!("effect name `{name}` must start with an uppercase letter"),
                )
                .with_label(span, "effect declaration", true),
            );
            return Type::Unit;
        }
        let mut operations = Vec::new();
        // Each remaining arg is [op-name [ParamType ...] ReturnType]
        for op_expr in &args[1..] {
            if let ExprKind::List(op_items) = &op_expr.kind {
                if op_items.is_empty() {
                    continue;
                }
                let op_name = match &op_items[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => continue,
                };
                let mut params = Vec::new();
                let mut return_type = None;
                // Parse [ParamType ...] and ReturnType
                // Format: [op-name [Type1 Type2 ...] RetType]
                if op_items.len() >= 2 {
                    if let ExprKind::List(param_types) = &op_items[1].kind {
                        for pt in param_types {
                            if let ExprKind::Symbol(ty_name) = &pt.kind {
                                params.push((ty_name.clone(), Some(ty_name.clone())));
                            }
                        }
                    }
                }
                if op_items.len() >= 3 {
                    if let ExprKind::Symbol(ret) = &op_items[2].kind {
                        return_type = Some(ret.clone());
                    }
                }
                operations.push(crate::effects::EffectOp {
                    name: op_name,
                    params,
                    return_type,
                });
            }
        }
        let decl = crate::effects::EffectDecl {
            name: name.clone(),
            operations,
        };
        self.effect_registry.register(decl);
        Type::Unit
    }

    fn infer_handle(&mut self, args: &[Expr], _span: Span) -> Type {
        if args.is_empty() {
            return Type::Unit;
        }
        // Save the enclosing ambient row. The handled body gets its own
        // fresh open row so the handled labels can be subtracted before the
        // residual is merged back into the enclosing row.
        let saved_effects = self.current_fn_effects.clone();
        // Labels already in the enclosing row BEFORE the handled body: they
        // came from performs outside this handle and must survive the scrub
        // step below.
        let ambient_labels_pre = self.subst.resolve_effect_row(&saved_effects).labels;
        let body_tail = self.subst.fresh_var();
        self.current_fn_effects = EffectRow::open(body_tail);
        let body_ty = self.infer(&args[0]);
        // Restore the enclosing row before checking the handler clauses:
        // clause bodies run at the handler, outside the handled region.
        let body_row = std::mem::replace(&mut self.current_fn_effects, saved_effects);
        // Track what the handler CLAUSES add to the enclosing row (clause
        // bodies run outside the handled region, so their performs are
        // genuine): a resolved-labels snapshot around the clause loop plus
        // a slice of the perform log (which also catches a clause
        // re-performing a label the body already forced into the chain).
        let ambient_labels_post_body = self
            .subst
            .resolve_effect_row(&self.current_fn_effects)
            .labels;
        let perform_mark = self.perform_log.len();

        // The handler's ANSWER type: what every clause (and the `return` clause)
        // produces, and what the whole `handle` evaluates to. For a plain
        // tail-resumptive handler with no `return` clause it coincides with the
        // body's type; for the escaping/answer-passing style (clauses return
        // functions) it is that function type. `resume : op-result -> answer`.
        let answer = self.subst.fresh();
        let mut handled = EffectSet::empty();
        let handler_args = &args[1..];
        let mut has_return = false;
        let mut i = 0;
        while i + 1 < handler_args.len() {
            let ExprKind::List(pattern) = &handler_args[i].kind else {
                i += 1;
                continue;
            };
            if pattern.is_empty() {
                i += 1;
                continue;
            }
            let clause_body = &handler_args[i + 1];
            match &pattern[0].kind {
                // [return x] body — maps the body's normal-completion value to
                // the answer type. x is bound to the body's type.
                ExprKind::Symbol(s) if s == "return" => {
                    has_return = true;
                    self.push_scope();
                    if let Some(ExprKind::Symbol(name)) = pattern.get(1).map(|e| &e.kind) {
                        self.env.set(name.clone(), Scheme::mono(body_ty.clone()));
                    }
                    let rty = self.infer(clause_body);
                    if let Err(e) = unify(&mut self.subst, &answer, &rty) {
                        self.push_unify_error(e, clause_body.span);
                    }
                    self.pop_scope();
                }
                // [Effect.op params…] body — an operation clause.
                ExprKind::DotAccess(obj, _op) => {
                    if let ExprKind::Symbol(effect) = &obj.kind {
                        if effect.starts_with(char::is_uppercase) {
                            handled.insert(effect.to_string());
                        }
                    }
                    self.push_scope();
                    for p in &pattern[1..] {
                        if let ExprKind::Symbol(name) = &p.kind {
                            let t = self.subst.fresh();
                            self.env.set(name.clone(), Scheme::mono(t));
                        }
                    }
                    // resume : op-result -> answer. Argument and result are
                    // INDEPENDENT (tying them — the old `a -> a` — made the
                    // escaping encoding an infinite type). The result is the
                    // shared answer, so a clause that returns a function checks.
                    let resume_arg = self.subst.fresh();
                    let resume_row = EffectRow::open(self.subst.fresh_var());
                    self.env.set(
                        "resume".to_string(),
                        Scheme::mono(Type::Fn(
                            vec![resume_arg],
                            Box::new(answer.clone()),
                            resume_row,
                        )),
                    );
                    let cty = self.infer(clause_body);
                    if let Err(e) = unify(&mut self.subst, &answer, &cty) {
                        self.push_unify_error(e, clause_body.span);
                    }
                    self.pop_scope();
                }
                _ => {}
            }
            i += 2;
        }

        // With no `return` clause, the body's value IS the answer.
        if !has_return {
            if let Err(e) = unify(&mut self.subst, &answer, &body_ty) {
                self.push_unify_error(e, args[0].span);
            }
        }

        // Labels the clauses themselves contributed to the enclosing row.
        let mut clause_labels: BTreeSet<String> = self
            .subst
            .resolve_effect_row(&self.current_fn_effects)
            .labels
            .difference(&ambient_labels_post_body)
            .cloned()
            .collect();
        clause_labels.extend(self.perform_log[perform_mark..].iter().cloned());

        // Effect subtraction: the handle expression's row is the body's row
        // minus the handled labels.
        //
        // Open-tail semantics (the evidence-passing-compatible choice): a
        // handled label must not escape through the body row's tail, so we
        // CONSTRAIN the tail at the handle boundary by unifying the body row
        // with `{handled… | fresh}`. This forces every handled label into
        // the concrete part of the row — and of every row linked to it, such
        // as a function parameter called inside the body. When a caller
        // later passes an effectful argument, its labels match those
        // concrete labels instead of being absorbed by the residual tail, so
        // the handled effect never reappears at the call site. The residual
        // row keeps the (now constrained) open tail: effects the handler
        // does not handle still flow out.
        if !handled.0.is_empty() {
            let want = EffectRow {
                labels: handled.0.clone(),
                tail: Some(self.subst.fresh_var()),
            };
            let _ = unify_effect_rows(&mut self.subst, &body_row, &want);
        }
        let resolved_body_row = self.subst.resolve_effect_row(&body_row);
        let residual = EffectRow {
            labels: resolved_body_row
                .labels
                .difference(&handled.0)
                .cloned()
                .collect(),
            tail: resolved_body_row.tail,
        };
        // Merge the residual (unhandled) effects into the enclosing row.
        self.absorb_effect_row(&residual);

        // Scrub step: if the body row became ALIASED with the enclosing
        // row during body inference (a recursive self-call inside the
        // handle, or a parameter that was already linked to the enclosing
        // row), the boundary constraint above forced the handled labels
        // into the enclosing function's own row too — where the subtraction
        // cannot see them, so the function (and every caller) would falsely
        // report the handled effect. A handled label found in the enclosing
        // row is genuine only if it was there before the body (performed
        // outside the handle) or was added by a handler clause; anything
        // else is boundary contamination and is removed, in place, from the
        // whole chain so aliased rows (e.g. a parameter's row) are cleaned
        // consistently. Known over-approximation kept: a parameter whose
        // row was NOT aliased with the enclosing row here keeps the handled
        // label concretely (that is the no-leak guarantee), so calling that
        // parameter again outside the handle re-reports the label even for
        // pure arguments (see effect_row_param_called_after_handle test).
        if !handled.0.is_empty() {
            let scrub: BTreeSet<String> = handled
                .0
                .iter()
                .filter(|l| !ambient_labels_pre.contains(*l) && !clause_labels.contains(*l))
                .cloned()
                .collect();
            if !scrub.is_empty() {
                self.current_fn_effects
                    .labels
                    .retain(|l| !scrub.contains(l));
                self.subst
                    .scrub_effect_labels(self.current_fn_effects.tail, &scrub);
            }
        }
        answer
    }

    /// Resolve a type name string to a Type (for effect declarations and annotations).
    fn resolve_type_name(&mut self, name: &str) -> Type {
        use crate::types::Dimension;
        const D: Dimension = Dimension::SCALAR;
        match name {
            "Int" => Type::Int,
            "Float" => Type::Float,
            "Bool" => Type::Bool,
            "String" | "Str" => Type::Str,
            "Keyword" => Type::Keyword,
            "Unit" => Type::Unit,
            // Named physical quantities → Dim types
            "Length" => Type::Dim(Dimension::length()),
            "Time" => Type::Dim(Dimension::time()),
            "Mass" => Type::Dim(Dimension::mass()),
            "Current" => Type::Dim(Dimension::current()),
            "Temperature" => Type::Dim(Dimension::temperature()),
            "Scalar" => Type::Dim(D),
            "Velocity" => Type::Dim(Dimension {
                length: 1,
                time: -1,
                ..D
            }),
            "Acceleration" => Type::Dim(Dimension {
                length: 1,
                time: -2,
                ..D
            }),
            "Force" => Type::Dim(Dimension {
                mass: 1,
                length: 1,
                time: -2,
                ..D
            }),
            "Pressure" => Type::Dim(Dimension {
                mass: 1,
                length: -1,
                time: -2,
                ..D
            }),
            "Energy" => Type::Dim(Dimension {
                mass: 1,
                length: 2,
                time: -2,
                ..D
            }),
            "Power" => Type::Dim(Dimension {
                mass: 1,
                length: 2,
                time: -3,
                ..D
            }),
            "Frequency" => Type::Dim(Dimension { time: -1, ..D }),
            "Area" => Type::Dim(Dimension { length: 2, ..D }),
            "Volume" => Type::Dim(Dimension { length: 3, ..D }),
            "Density" => Type::Dim(Dimension {
                mass: 1,
                length: -3,
                ..D
            }),
            "Momentum" => Type::Dim(Dimension {
                mass: 1,
                length: 1,
                time: -1,
                ..D
            }),
            "Charge" => Type::Dim(Dimension {
                current: 1,
                time: 1,
                ..D
            }),
            "Voltage" => Type::Dim(Dimension {
                mass: 1,
                length: 2,
                time: -3,
                current: -1,
                ..D
            }),
            "Resistance" => Type::Dim(Dimension {
                mass: 1,
                length: 2,
                time: -3,
                current: -2,
                ..D
            }),
            "ThermalConductivity" => Type::Dim(Dimension {
                mass: 1,
                length: 1,
                time: -3,
                temperature: -1,
                ..D
            }),
            _ => {
                // Check if it's a known type constructor
                if let Some(scheme) = self.env.get(name) {
                    instantiate(&mut self.subst, &scheme)
                } else {
                    // Unknown type name — use fresh var
                    self.subst.fresh()
                }
            }
        }
    }

    fn parse_effect_set(&self, expr: &Expr) -> EffectSet {
        let mut effects = EffectSet::empty();
        match &expr.kind {
            ExprKind::Set(items) => {
                for item in items {
                    if let ExprKind::Symbol(name) = &item.kind {
                        effects.insert(name.clone());
                    }
                }
            }
            // Also support {IO Fail} parsed as a map (parser quirk)
            ExprKind::Map(pairs) => {
                for (k, v) in pairs {
                    if let ExprKind::Symbol(name) = &k.kind {
                        effects.insert(name.clone());
                    }
                    if let ExprKind::Symbol(name) = &v.kind {
                        effects.insert(name.clone());
                    }
                }
            }
            _ => {}
        }
        effects
    }

    fn infer_fn_clause(&mut self, params_expr: &Expr, body: &[Expr]) -> Type {
        if let ExprKind::List(params) = &params_expr.kind {
            self.push_scope();
            let _param_types = self.infer_params(params);

            let mut body_ty = Type::Unit;
            for expr in body {
                body_ty = self.infer(expr);
            }

            self.pop_scope();
            body_ty
        } else {
            self.subst.fresh()
        }
    }

    fn infer_let(&mut self, args: &[Expr]) -> Type {
        if args.len() < 2 {
            return Type::Unit;
        }
        let (binding, val_idx) = if matches!(&args[0].kind, ExprKind::Symbol(s) if s == "mut") {
            (&args[1], 2)
        } else {
            (&args[0], 1)
        };

        if val_idx >= args.len() {
            return Type::Unit;
        }

        let val_ty = self.infer(&args[val_idx]);

        match &binding.kind {
            ExprKind::Symbol(name) if name != "_" => {
                let scheme = generalize(&self.env, &self.subst, &val_ty);
                self.env.set(name.clone(), scheme);
                self.type_of.insert(binding.id, val_ty.clone());
                // Compute the full let-form span from binding to value
                let form_span = Span::new(binding.span.start, args[val_idx].span.end);
                self.add_definition(name, binding.span, form_span);
            }
            _ => {}
        }

        val_ty
    }

    fn infer_if(&mut self, args: &[Expr], span: Span) -> Type {
        if args.len() < 2 {
            return Type::Unit;
        }
        // A condition may be ANY type — truthiness is a runtime property (the
        // falsy set is {false, (), None}), so we do NOT unify with Bool. But
        // if the inferred type can NEVER be falsy, one branch is dead: warn
        // and teach the rule (E0209).
        let cond_ty = self.infer(&args[0]);
        // Skip the lint for compiler-generated conditions (the gensym temps
        // produced by the and/or and if-let/when-let desugars): the user
        // never wrote that `if`, so there is nothing to teach at its span.
        let is_gensym_cond =
            matches!(&args[0].kind, ExprKind::Symbol(s) if s.starts_with("__gensym_"));
        if !is_gensym_cond {
            self.warn_if_never_falsy(&cond_ty, args[0].span);
        }
        let then_ty = self.infer(&args[1]);
        if args.len() > 2 {
            let else_ty = self.infer(&args[2]);
            if let Err(e) = unify(&mut self.subst, &then_ty, &else_ty) {
                self.push_unify_error(e, span);
            }
        }
        then_ty
    }

    /// E0209: warn when an `if` condition's inferred type can NEVER be
    /// falsy, so one branch is dead. Fires only on concrete types that cannot
    /// produce `false`, `()`, or `None`: Int, Float, String, Keyword,
    /// functions, tuples, records, dimensions, and non-Option constructors.
    /// It must NOT fire on Bool, Unit, Option, type variables (could resolve
    /// to anything), or anything still unknown.
    fn warn_if_never_falsy(&mut self, cond_ty: &Type, span: Span) {
        let resolved = self.subst.resolve(cond_ty);
        let type_name = match &resolved {
            Type::Int => "Int",
            Type::Float => "Float",
            Type::Str => "String",
            Type::Keyword => "Keyword",
            Type::Fn(..) => "a function",
            Type::Tuple(_) => "a tuple",
            Type::Record(_) | Type::Row(..) => "a record",
            Type::Dim(_) => "a dimensional value",
            // Option can be None (falsy). The builtin containers and Result
            // are heap values — always truthy — so they warn. User ADTs are
            // skipped conservatively: a user type could (re)define a `None`
            // constructor, and we don't track ctor names here.
            Type::Con(name, _) => match name.as_str() {
                "Option" => return,
                "Vec" | "Map" | "Set" | "String" | "Result" => name.as_str(),
                _ => return, // user ADT: conservative, no warning
            },
            // Bool/Unit can be falsy; vars and effect rows are unknown.
            Type::Bool | Type::Unit | Type::Var(_) | Type::Effects(_) => return,
        };
        self.errors.push(
            LoonDiagnostic::new(
                ErrorCode::E0209,
                format!("this condition is always truthy: its type is {type_name}, which can never be falsy"),
            )
            .with_why(format!(
                "the falsy set is exactly {{false, (), None}}; {type_name} can produce none of them, so the else branch (or the non-run arm) is dead — 0, \"\", and empty collections are truthy in Loon"
            ))
            .with_fix(
                "a value is truthy unless it says no (false) or says nothing ((), None) — test the property you mean explicitly, e.g. [> n 0], [empty? v], or [some? x]".to_string(),
            )
            .with_label(span, "always-truthy condition", true),
        );
    }

    fn infer_do(&mut self, args: &[Expr]) -> Type {
        let mut last = Type::Unit;
        for expr in args {
            last = self.infer(expr);
        }
        last
    }

    fn infer_match(&mut self, args: &[Expr], span: Span) -> Type {
        if args.is_empty() {
            return Type::Unit;
        }
        let scrutinee_ty = self.infer(&args[0]);
        let result_ty = self.subst.fresh();

        let arms = &args[1..];
        let mut i = 0;
        let mut covered_ctors: Vec<String> = Vec::new();
        let mut has_wildcard = false;
        while i < arms.len() {
            let pattern = &arms[i];

            // Track covered constructors and wildcards
            match &pattern.kind {
                ExprKind::Symbol(s) if s == "_" => {
                    has_wildcard = true;
                }
                ExprKind::Symbol(s) if !s.starts_with(char::is_uppercase) => {
                    has_wildcard = true;
                }
                ExprKind::Symbol(s) if s.starts_with(char::is_uppercase) => {
                    covered_ctors.push(s.clone());
                }
                ExprKind::List(items) if !items.is_empty() => {
                    if let ExprKind::Symbol(s) = &items[0].kind {
                        if s.starts_with(char::is_uppercase) {
                            covered_ctors.push(s.clone());
                        }
                    }
                }
                _ => {}
            }

            // Guard: pattern [when guard] body → i += 3
            if i + 2 < arms.len() {
                if let ExprKind::List(guard_form) = &arms[i + 1].kind {
                    if !guard_form.is_empty() {
                        if let ExprKind::Symbol(s) = &guard_form[0].kind {
                            if s == "when" {
                                self.push_scope();
                                self.bind_pattern_vars(pattern, &scrutinee_ty);
                                let _guard_ty = self.infer(&guard_form[1]);
                                let body_ty = self.infer(&arms[i + 2]);
                                self.pop_scope();
                                if let Err(e) = unify(&mut self.subst, &result_ty, &body_ty) {
                                    self.push_unify_error(e, arms[i + 2].span);
                                }
                                i += 3;
                                continue;
                            }
                        }
                    }
                }
            }

            // Simple: pattern body → i += 2
            if i + 1 < arms.len() {
                self.push_scope();
                self.bind_pattern_vars(pattern, &scrutinee_ty);
                let body_ty = self.infer(&arms[i + 1]);
                self.pop_scope();
                if let Err(e) = unify(&mut self.subst, &result_ty, &body_ty) {
                    self.push_unify_error(e, arms[i + 1].span);
                }
                i += 2;
                continue;
            }

            i += 1;
        }

        let resolved = self.subst.resolve(&scrutinee_ty);
        if let Type::Con(ref type_name, _) = resolved {
            if let Some(all_ctors) = self.type_constructors.get(type_name).cloned() {
                let caught: Vec<&String> = all_ctors
                    .iter()
                    .filter(|c| !covered_ctors.contains(c))
                    .collect();
                if !has_wildcard {
                    // Error: non-exhaustive match
                    if !caught.is_empty() {
                        let missing_str = caught
                            .iter()
                            .map(|s| s.as_str())
                            .collect::<Vec<_>>()
                            .join(", ");
                        self.errors.push(
                            LoonDiagnostic::new(
                                ErrorCode::E0206,
                                format!(
                                    "non-exhaustive match on {type_name}: missing {missing_str}"
                                ),
                            )
                            .with_why(format!("not all constructors of `{type_name}` are covered"))
                            .with_fix(format!(
                                "add arms for: {missing_str}, or add a wildcard `_` arm"
                            ))
                            .with_label(
                                span,
                                "non-exhaustive match",
                                true,
                            ),
                        );
                    }
                } else if !caught.is_empty()
                    && !matches!(&args[0].kind, ExprKind::Symbol(s) if s.starts_with("__gensym_"))
                {
                    // Warning: transparent wildcard. (Skipped for matches on
                    // compiler-generated gensym temps — e.g. the if-let
                    // desugar's unwrap match — which the user never wrote.)
                    let caught_str = caught
                        .iter()
                        .map(|s| s.as_str())
                        .collect::<Vec<_>>()
                        .join(", ");
                    self.errors.push(
                        LoonDiagnostic::new(
                            ErrorCode::W0100,
                            format!(
                                "_ catches {} constructor{} of {type_name}: {caught_str}",
                                caught.len(),
                                if caught.len() == 1 { "" } else { "s" },
                            ),
                        )
                        .with_why(format!("adding a variant to `{type_name}` will silently fall into this arm"))
                        .with_fix("add explicit arms for each constructor, or keep _ if the fallback is intentional".to_string())
                        .with_label(span, "wildcard match", true),
                    );
                }
            }
        }

        result_ty
    }

    fn infer_pipe(&mut self, args: &[Expr], _span: Span) -> Type {
        if args.is_empty() {
            return Type::Unit;
        }
        let mut current = self.infer(&args[0]);
        for step in &args[1..] {
            match &step.kind {
                ExprKind::List(items) if !items.is_empty() => {
                    let func_ty = self.infer(&items[0]);
                    let explicit_args: Vec<Type> =
                        items[1..].iter().map(|a| self.infer(a)).collect();

                    let arg_tys = if explicit_args.is_empty() {
                        vec![current]
                    } else {
                        let mut tys = explicit_args;
                        tys.push(current);
                        tys
                    };

                    let ret = self.subst.fresh();
                    let app_row = EffectRow::open(self.subst.fresh_var());
                    let expected = Type::Fn(arg_tys, Box::new(ret.clone()), app_row.clone());
                    if let Err(e) = unify(&mut self.subst, &func_ty, &expected) {
                        self.push_unify_error(e, step.span);
                    }
                    self.absorb_effect_row(&app_row);
                    current = ret;
                }
                ExprKind::Symbol(_) => {
                    let func_ty = self.infer(step);
                    let ret = self.subst.fresh();
                    let app_row = EffectRow::open(self.subst.fresh_var());
                    let expected = Type::Fn(vec![current], Box::new(ret.clone()), app_row.clone());
                    if let Err(e) = unify(&mut self.subst, &func_ty, &expected) {
                        self.push_unify_error(e, step.span);
                    }
                    self.absorb_effect_row(&app_row);
                    current = ret;
                }
                _ => {}
            }
        }
        current
    }

    fn infer_type_def(&mut self, args: &[Expr]) -> Type {
        if args.is_empty() {
            return Type::Unit;
        }
        let type_name = match &args[0].kind {
            ExprKind::Symbol(s) => s.clone(),
            _ => return Type::Unit,
        };

        // Record type name definition
        let type_form_span = if let Some(last) = args.last() {
            Span::new(args[0].span.start, last.span.end)
        } else {
            args[0].span
        };
        self.add_definition(&type_name, args[0].span, type_form_span);

        let mut type_params = Vec::new();
        let mut ctor_start = 1;
        let mut ctor_names = Vec::new();
        // Distinguish a leading type PARAMETER (a symbol used as a field type,
        // e.g. `T` in `[type Option T [Some T] None]`) from a leading nullary
        // CONSTRUCTOR (e.g. `None`, or `Red` in `[type Color Red Green Blue]`).
        // Type parameters are exactly the leading symbols that appear in some
        // constructor's field positions — works for both lowercase (`a`) and
        // uppercase (`T`, the prelude convention) parameter names.
        fn collect_field_syms(e: &Expr, out: &mut std::collections::HashSet<String>) {
            if let ExprKind::List(items) = &e.kind {
                for f in items.iter().skip(1) {
                    match &f.kind {
                        ExprKind::Symbol(s) => {
                            out.insert(s.clone());
                        }
                        ExprKind::List(_) => collect_field_syms(f, out),
                        _ => {}
                    }
                }
            }
        }
        let mut field_syms: std::collections::HashSet<String> = std::collections::HashSet::new();
        for arg in &args[1..] {
            collect_field_syms(arg, &mut field_syms);
        }
        for arg in &args[1..] {
            if let ExprKind::Symbol(s) = &arg.kind {
                if field_syms.contains(s) {
                    let tv = self.subst.fresh();
                    if let Type::Var(v) = tv {
                        type_params.push((s.clone(), v));
                    }
                    ctor_start += 1;
                } else {
                    // A leading bare symbol not used as a field is a nullary
                    // constructor, not a parameter — stop collecting params.
                    break;
                }
            } else {
                break;
            }
        }

        let result_ty = if type_params.is_empty() {
            Type::Con(type_name.clone(), vec![])
        } else {
            Type::Con(
                type_name.clone(),
                type_params.iter().map(|(_, v)| Type::Var(*v)).collect(),
            )
        };

        for arg in &args[ctor_start..] {
            match &arg.kind {
                ExprKind::List(items) if !items.is_empty() => {
                    if let ExprKind::Symbol(ctor_name) = &items[0].kind {
                        let field_types: Vec<Type> = items[1..]
                            .iter()
                            .map(|f| {
                                if let ExprKind::Symbol(s) = &f.kind {
                                    if let Some((_, tv)) = type_params.iter().find(|(n, _)| n == s)
                                    {
                                        Type::Var(*tv)
                                    } else {
                                        self.name_to_type(s)
                                    }
                                } else {
                                    self.subst.fresh()
                                }
                            })
                            .collect();

                        // Constructors are effect-polymorphic: give them an open,
                        // quantified tail (like builtins get in
                        // effect_polymorphize_builtins) so a constructor can be
                        // passed to any higher-order function regardless of the
                        // effect row its function parameter carries.
                        let effect_tail = self.subst.fresh_var();
                        let ctor_ty = Type::Fn(
                            field_types,
                            Box::new(result_ty.clone()),
                            EffectRow::open(effect_tail),
                        );
                        let mut vars: Vec<TypeVar> = type_params.iter().map(|(_, v)| *v).collect();
                        vars.push(effect_tail);
                        let scheme = Scheme {
                            bounds: vec![],
                            vars,
                            ty: ctor_ty,
                        };
                        self.constructors.insert(ctor_name.clone(), scheme.clone());
                        self.env.set_global(ctor_name.clone(), scheme);
                        self.add_definition(ctor_name, items[0].span, arg.span);
                        ctor_names.push(ctor_name.clone());
                    }
                }
                ExprKind::Symbol(ctor_name) if ctor_name.starts_with(char::is_uppercase) => {
                    let vars: Vec<TypeVar> = type_params.iter().map(|(_, v)| *v).collect();
                    let scheme = Scheme {
                        bounds: vec![],
                        vars,
                        ty: result_ty.clone(),
                    };
                    self.constructors.insert(ctor_name.clone(), scheme.clone());
                    self.env.set_global(ctor_name.clone(), scheme);
                    self.add_definition(ctor_name, arg.span, arg.span);
                    ctor_names.push(ctor_name.clone());
                }
                _ => {}
            }
        }

        if !ctor_names.is_empty() {
            self.type_constructors.insert(type_name, ctor_names);
        }

        Type::Unit
    }

    fn infer_trait_def(&mut self, args: &[Expr]) -> Type {
        if args.is_empty() {
            return Type::Unit;
        }
        let trait_name = match &args[0].kind {
            ExprKind::Symbol(s) => s.clone(),
            _ => return Type::Unit,
        };

        let mut methods = Vec::new();
        for arg in &args[1..] {
            if let ExprKind::List(items) = &arg.kind {
                if items.len() >= 2 {
                    if let ExprKind::Symbol(ref kw) = items[0].kind {
                        if kw == "fn" {
                            if let ExprKind::Symbol(ref method_name) = items[1].kind {
                                let mut param_types = Vec::new();
                                let mut ret_type = Type::Unit;
                                let mut i = 2;

                                if i < items.len() {
                                    if let ExprKind::List(ref params) = items[i].kind {
                                        for p in params {
                                            if let ExprKind::Symbol(ref s) = p.kind {
                                                if s == "self" {
                                                    param_types.push(Type::Con(
                                                        "Self".to_string(),
                                                        vec![],
                                                    ));
                                                } else {
                                                    param_types.push(self.name_to_type(s));
                                                }
                                            }
                                        }
                                        i += 1;
                                    }
                                }

                                // Skip → and parse return type
                                while i < items.len() {
                                    if let ExprKind::Symbol(ref s) = items[i].kind {
                                        if s == "\u{2192}" || s == "->" {
                                            i += 1;
                                            if i < items.len() {
                                                if let ExprKind::Symbol(ref ret) = items[i].kind {
                                                    if ret == "Self" {
                                                        ret_type =
                                                            Type::Con("Self".to_string(), vec![]);
                                                    } else {
                                                        ret_type = self.name_to_type(ret);
                                                    }
                                                }
                                            }
                                            break;
                                        }
                                    }
                                    i += 1;
                                }

                                methods.push(TraitMethod {
                                    name: method_name.clone(),
                                    param_types,
                                    ret_type,
                                });
                            }
                        }
                    }
                }
            }
        }

        self.traits.insert(
            trait_name.clone(),
            TraitDecl {
                name: trait_name,
                type_params: vec![],
                methods,
            },
        );
        Type::Unit
    }

    fn infer_impl_def(&mut self, args: &[Expr], span: Span) -> Type {
        if args.len() < 2 {
            return Type::Unit;
        }
        let trait_name = match &args[0].kind {
            ExprKind::Symbol(s) => s.clone(),
            _ => return Type::Unit,
        };
        let type_name = match &args[1].kind {
            ExprKind::Symbol(s) => s.clone(),
            _ => return Type::Unit,
        };

        let trait_decl = match self.traits.get(&trait_name) {
            Some(t) => t.clone(),
            None => {
                self.errors.push(
                    LoonDiagnostic::new(ErrorCode::E0205, format!("unknown trait '{trait_name}'"))
                        .with_why(format!("trait `{trait_name}` has not been declared"))
                        .with_fix("declare the trait before implementing it")
                        .with_label(span, "unknown trait", true),
                );
                return Type::Unit;
            }
        };

        let impl_type = self.name_to_type(&type_name);
        let mut method_schemes = std::collections::HashMap::new();

        for arg in &args[2..] {
            if let ExprKind::List(items) = &arg.kind {
                if items.len() >= 3 {
                    if let ExprKind::Symbol(ref kw) = items[0].kind {
                        if kw == "fn" {
                            if let ExprKind::Symbol(ref method_name) = items[1].kind {
                                let trait_method =
                                    trait_decl.methods.iter().find(|m| m.name == *method_name);

                                if let ExprKind::List(ref params) = items[2].kind {
                                    self.push_scope();

                                    let mut param_types = Vec::new();
                                    for p in params {
                                        if let ExprKind::Symbol(ref s) = p.kind {
                                            if s == "self" {
                                                self.env.set(
                                                    "self".to_string(),
                                                    Scheme::mono(impl_type.clone()),
                                                );
                                                param_types.push(impl_type.clone());
                                            } else {
                                                let t = self.subst.fresh();
                                                self.env.set(s.clone(), Scheme::mono(t.clone()));
                                                param_types.push(t);
                                            }
                                        }
                                    }

                                    let mut body_ty = Type::Unit;
                                    for body_expr in &items[3..] {
                                        body_ty = self.infer(body_expr);
                                    }

                                    if let Some(tm) = trait_method {
                                        let expected_ret = if tm.ret_type
                                            == Type::Con("Self".to_string(), vec![])
                                        {
                                            impl_type.clone()
                                        } else {
                                            tm.ret_type.clone()
                                        };
                                        if let Err(e) =
                                            unify(&mut self.subst, &body_ty, &expected_ret)
                                        {
                                            self.push_unify_error(e, span);
                                        }
                                    }

                                    self.pop_scope();

                                    let fn_ty = Type::Fn(
                                        param_types,
                                        Box::new(body_ty),
                                        EffectRow::open(self.subst.fresh_var()),
                                    );
                                    let scheme = generalize(&self.env, &self.subst, &fn_ty);
                                    method_schemes.insert(method_name.clone(), scheme.clone());

                                    self.env
                                        .set_global(format!("{type_name}.{method_name}"), scheme);
                                }
                            }
                        }
                    }
                }
            }
        }

        self.trait_impls
            .insert((trait_name, type_name), method_schemes);
        Type::Unit
    }

    fn infer_sig(&mut self, args: &[Expr], span: Span) -> Type {
        if args.is_empty() {
            return Type::Unit;
        }
        let name = match &args[0].kind {
            ExprKind::Symbol(s) => s.clone(),
            _ => return Type::Unit,
        };

        // Skip the : symbol
        let type_args = if args.len() > 1 {
            if let ExprKind::Symbol(ref s) = args[1].kind {
                if s == ":" {
                    &args[2..]
                } else {
                    &args[1..]
                }
            } else {
                &args[1..]
            }
        } else {
            return Type::Unit;
        };

        let sig_type = self.parse_sig_type(type_args);
        self.pending_sigs.insert(name, (sig_type, span));
        Type::Unit
    }

    /// Parse a function type from sig args: Type1 → Type2 → ... → RetType
    fn parse_sig_type(&mut self, args: &[Expr]) -> Type {
        let mut types = Vec::new();
        for arg in args {
            match &arg.kind {
                ExprKind::Symbol(s) if s == "\u{2192}" || s == "->" => continue,
                _ => types.push(self.parse_type_expr(arg)),
            }
        }

        if types.len() <= 1 {
            return types.into_iter().next().unwrap_or(Type::Unit);
        }

        let ret = types.pop().unwrap();
        // Sigs say nothing about effects; an open row means the assertion
        // constrains only the value types, never the effect row.
        Type::Fn(
            types,
            Box::new(ret),
            EffectRow::open(self.subst.fresh_var()),
        )
    }

    /// Convert an AST expression into a Type
    fn parse_type_expr(&mut self, expr: &Expr) -> Type {
        match &expr.kind {
            ExprKind::Symbol(s) => {
                if s.len() == 1 && s.chars().next().is_some_and(|c| c.is_lowercase()) {
                    self.subst.fresh()
                } else {
                    self.name_to_type(s)
                }
            }
            ExprKind::List(items) if !items.is_empty() => {
                if let ExprKind::Symbol(ref name) = items[0].kind {
                    let type_args: Vec<Type> =
                        items[1..].iter().map(|a| self.parse_type_expr(a)).collect();
                    Type::Con(name.clone(), type_args)
                } else {
                    self.subst.fresh()
                }
            }
            ExprKind::Tuple(items) => {
                let types: Vec<Type> = items.iter().map(|e| self.parse_type_expr(e)).collect();
                Type::Tuple(types)
            }
            _ => self.subst.fresh(),
        }
    }

    /// Handle [derive Copy [type Name ...]]
    fn infer_derive(&mut self, args: &[Expr], span: Span) -> Type {
        if args.is_empty() {
            return Type::Unit;
        }
        // First arg should be the trait to derive (e.g., Copy)
        let trait_name = match &args[0].kind {
            ExprKind::Symbol(s) => s.clone(),
            _ => return Type::Unit,
        };

        if trait_name == "Copy" {
            // The inner form should be a type definition: [type Name ...]
            if args.len() >= 2 {
                if let ExprKind::List(inner) = &args[1].kind {
                    if !inner.is_empty() {
                        if let ExprKind::Symbol(ref kw) = inner[0].kind {
                            if kw == "type" && inner.len() >= 2 {
                                if let ExprKind::Symbol(ref type_name) = inner[1].kind {
                                    self.derived_copy_types.insert(type_name.clone());
                                }
                            }
                        }
                    }
                    // Infer the inner type definition
                    return self.infer_list(inner, span);
                }
            }
        }

        Type::Unit
    }

    /// Post-inference pass: check that all trait constraints are satisfied.
    pub fn check_trait_constraints(&mut self) {
        let constraints: Vec<(TypeVar, Vec<TraitBound>)> = self
            .subst
            .constraints
            .iter()
            .map(|(v, bs)| (*v, bs.clone()))
            .collect();

        for (tv, bounds) in constraints {
            let resolved = self.subst.resolve(&Type::Var(tv));
            let type_name = match &resolved {
                Type::Int => Some("Int".to_string()),
                Type::Float => Some("Float".to_string()),
                Type::Bool => Some("Bool".to_string()),
                Type::Str => Some("String".to_string()),
                Type::Con(name, _) => Some(name.clone()),
                Type::Dim(_) => Some("Dim".to_string()),
                Type::Var(_) => None, // still polymorphic, OK
                _ => None,
            };

            if let Some(type_name) = type_name {
                for bound in &bounds {
                    let key = (bound.trait_name.clone(), type_name.clone());
                    if !self.trait_impls.contains_key(&key) {
                        self.errors.push(
                            LoonDiagnostic::new(
                                ErrorCode::E0205,
                                format!(
                                    "no `{}` implementation for type `{}`",
                                    bound.trait_name, type_name
                                ),
                            )
                            .with_why(format!(
                                "type `{type_name}` does not implement `{}`",
                                bound.trait_name
                            ))
                            .with_fix(format!(
                                "add an impl block: [impl {} {type_name} ...]",
                                bound.trait_name
                            )),
                        );
                    }
                }
            }
        }
    }

    /// Infer param types, binding names into the current scope.
    fn infer_params(&mut self, params_expr: &[Expr]) -> Vec<Type> {
        params_expr
            .iter()
            .map(|p| self.infer_single_param(p))
            .collect()
    }

    fn infer_single_param(&mut self, expr: &Expr) -> Type {
        match &expr.kind {
            ExprKind::Symbol(s) => {
                let t = self.subst.fresh();
                self.env.set(s.clone(), Scheme::mono(t.clone()));
                self.type_of.insert(expr.id, t.clone());
                t
            }
            ExprKind::List(items) => {
                let elem_types: Vec<Type> =
                    items.iter().map(|p| self.infer_single_param(p)).collect();
                Type::Tuple(elem_types)
            }
            ExprKind::Map(pairs) => {
                let val_t = self.subst.fresh();
                for (k, _) in pairs {
                    if let ExprKind::Symbol(s) = &k.kind {
                        self.env.set(s.clone(), Scheme::mono(val_t.clone()));
                    }
                }
                Type::Con("Map".to_string(), vec![Type::Keyword, val_t])
            }
            _ => self.subst.fresh(),
        }
    }

    /// Bind variables from a match pattern into the current scope.
    fn bind_pattern_vars(&mut self, pattern: &Expr, _scrutinee_ty: &Type) {
        match &pattern.kind {
            ExprKind::Symbol(s) if s != "_" && !s.starts_with(char::is_uppercase) => {
                let t = self.subst.fresh();
                self.env.set(s.clone(), Scheme::mono(t));
            }
            ExprKind::List(items) if !items.is_empty() => {
                // Constructor pattern: [Ok x] — bind the field vars
                if let ExprKind::Symbol(ctor) = &items[0].kind {
                    if ctor.starts_with(char::is_uppercase) {
                        for field in &items[1..] {
                            self.bind_pattern_vars(field, _scrutinee_ty);
                        }
                    }
                }
            }
            _ => {}
        }
    }

    fn name_to_type(&self, name: &str) -> Type {
        match name {
            "i64" | "Int" => Type::Int,
            "f64" | "Float" => Type::Float,
            "Bool" => Type::Bool,
            "String" | "Str" => Type::Str,
            "Keyword" => Type::Keyword,
            _ => Type::Con(name.to_string(), vec![]),
        }
    }

    // ── Module / use resolution ────────────────────────────────────

    fn infer_use(&mut self, args: &[Expr], span: Span) -> Type {
        if args.is_empty() {
            self.errors.push(
                LoonDiagnostic::new(ErrorCode::E0500, "use requires a module path").with_label(
                    span,
                    "missing module path",
                    true,
                ),
            );
            return Type::Unit;
        }

        let module_path = match args[0].as_dotted_path() {
            Some(s) => s,
            None => {
                self.errors.push(
                    LoonDiagnostic::new(ErrorCode::E0500, "use module path must be a symbol")
                        .with_label(span, "expected symbol", true),
                );
                return Type::Unit;
            }
        };

        let base_dir = match &self.base_dir {
            Some(d) => d.clone(),
            None => {
                // No base_dir — silently skip (e.g. WASM/REPL context)
                return Type::Unit;
            }
        };

        let file_path = ResolveHelper::resolve_path(&module_path, &base_dir);
        let canonical = file_path
            .canonicalize()
            .unwrap_or_else(|_| file_path.clone());

        // Check cache (cycle detection + memoisation)
        {
            let cached = {
                let cache = self.module_cache.borrow();
                cache.modules.get(&canonical).cloned()
            };
            if let Some(state) = cached {
                match state {
                    TypeModuleState::Loading => {
                        self.errors.push(
                            LoonDiagnostic::new(
                                ErrorCode::E0502,
                                format!("circular module dependency: {module_path}"),
                            )
                            .with_why("this module is already being loaded, creating a cycle")
                            .with_fix("break the cycle by restructuring module dependencies")
                            .with_label(span, "circular use", true),
                        );
                        return Type::Unit;
                    }
                    TypeModuleState::Loaded(exports) => {
                        self.import_exports(&module_path, &args[1..], &exports, span);
                        return Type::Unit;
                    }
                }
            }
        }

        // Mark as loading
        self.module_cache
            .borrow_mut()
            .modules
            .insert(canonical.clone(), TypeModuleState::Loading);

        // Read and parse the module file
        let source = match std::fs::read_to_string(&file_path) {
            Ok(s) => s,
            Err(e) => {
                self.errors.push(
                    LoonDiagnostic::new(
                        ErrorCode::E0500,
                        format!(
                            "cannot read module '{}' at {}: {e}",
                            module_path,
                            file_path.display()
                        ),
                    )
                    .with_label(span, "module not found", true),
                );
                self.module_cache.borrow_mut().modules.remove(&canonical);
                return Type::Unit;
            }
        };

        let exprs = match crate::parser::parse(&source) {
            Ok(e) => e,
            Err(e) => {
                self.errors.push(
                    LoonDiagnostic::new(
                        ErrorCode::E0500,
                        format!("parse error in module '{module_path}': {}", e.message),
                    )
                    .with_label(span, "parse error in module", true),
                );
                self.module_cache.borrow_mut().modules.remove(&canonical);
                return Type::Unit;
            }
        };

        // Type-check the module with a fresh checker sharing our cache
        let module_dir = file_path.parent().unwrap_or(&base_dir).to_path_buf();
        let cache_ref = Rc::clone(&self.module_cache);
        let mut mod_checker = Checker::for_module(&module_dir, cache_ref);
        for expr in &exprs {
            mod_checker.infer(expr);
        }

        // Collect exported schemes
        let exports = mod_checker.collect_exports();

        // Store in cache
        self.module_cache
            .borrow_mut()
            .modules
            .insert(canonical, TypeModuleState::Loaded(exports.clone()));

        // Capability enforcement: if this module is declared as a dependency
        // in pkg.oo, its inferred effect rows must stay within its :grant.
        self.enforce_dep_grants(&module_path, &mod_checker, &base_dir, span);

        // Propagate any diagnostics from the module
        for mut e in mod_checker.errors {
            e.what = format!("in module '{module_path}': {}", e.what);
            self.errors.push(e);
        }

        self.import_exports(&module_path, &args[1..], &exports, span);
        Type::Unit
    }

    /// Effects that carry ambient authority — they reach outside the program
    /// through VM builtins when unhandled (real filesystem, network, clock,
    /// environment). These are what a pkg.oo `:grant` governs. User-declared
    /// effects need a caller-supplied handler to mean anything (no ambient
    /// escape, and an unhandled one is a loud VM error), and `Fail` is pure
    /// control flow — neither is subject to grants.
    const AMBIENT_EFFECTS: &'static [&'static str] = &["IO", "Net", "Process", "Env", "Async"];

    /// Enforce pkg.oo capability grants on an imported module: if `module_path`
    /// is declared as a dependency in the importing project's manifest, every
    /// ambient effect inferred anywhere in the module must appear in the dep's
    /// `:grant` list (an absent/empty grant means the dep is declared pure).
    /// Static supply-chain security: the checker proves a dependency cannot
    /// touch the network/filesystem unless the manifest says so.
    fn enforce_dep_grants(
        &mut self,
        module_path: &str,
        mod_checker: &Checker,
        base_dir: &std::path::Path,
        span: Span,
    ) {
        // Grants only apply to modules declared as manifest dependencies;
        // plain project-local modules are unrestricted.
        let Ok(Some(manifest)) = crate::pkg::Manifest::load(base_dir) else {
            return;
        };
        let Some(dep) = manifest.deps.get(module_path) else {
            return;
        };
        let granted: std::collections::HashSet<&str> =
            dep.grant.iter().map(|s| s.as_str()).collect();

        // One diagnostic per ungranted effect, citing one offending function
        // (BTreeMap for deterministic output order).
        let mut violations: std::collections::BTreeMap<&String, &String> =
            std::collections::BTreeMap::new();
        for (fname, effects) in &mod_checker.fn_effects {
            for eff in &effects.labels {
                if Self::AMBIENT_EFFECTS.contains(&eff.as_str()) && !granted.contains(eff.as_str())
                {
                    let entry = violations.entry(eff).or_insert(fname);
                    // keep the lexicographically first fn for determinism
                    if fname < *entry {
                        *entry = fname;
                    }
                }
            }
        }

        let grant_desc = if dep.grant.is_empty() {
            "declares it pure (no :grant)".to_string()
        } else {
            format!("grants it only {:?}", dep.grant)
        };
        for (eff, fname) in violations {
            self.errors.push(
                LoonDiagnostic::new(
                    ErrorCode::E0404,
                    format!(
                        "dependency '{module_path}' performs effect `{eff}` \
                         (e.g. in `{fname}`) but pkg.oo {grant_desc}"
                    ),
                )
                .with_why(
                    "a dependency's effect row is its capability set: code whose \
                     grant lacks an effect must not be able to perform it",
                )
                .with_fix(format!(
                    "add \"{eff}\" to the :grant list for \"{module_path}\" in \
                     pkg.oo — or use a dependency that doesn't need it"
                ))
                .with_label(span, "capability violation", true),
            );
        }
    }

    /// Collect the exported type schemes from a checked module.
    fn collect_exports(&self) -> TypeModuleExports {
        let mut schemes = HashMap::new();

        if self.pub_names.is_empty() {
            // No explicit pub — export all non-builtin globals
            let builtin_checker = Checker::new();
            if let Some(global_scope) = self.env.global_scope() {
                for (name, scheme) in global_scope {
                    if builtin_checker.env.get(name).is_none() {
                        schemes.insert(name.clone(), scheme.clone());
                    }
                }
            }
        } else {
            for name in &self.pub_names {
                if let Some(scheme) = self.env.get(name) {
                    schemes.insert(name.clone(), scheme.clone());
                }
            }
        }

        // Also export constructors for pub ADTs
        let mut constructors = HashMap::new();
        for (name, scheme) in &self.constructors {
            // Export if the constructor's type is for a pub ADT, or if no pub names
            if self.pub_names.is_empty() || self.pub_names.contains(name) {
                constructors.insert(name.clone(), scheme.clone());
            }
        }

        TypeModuleExports {
            schemes,
            constructors,
        }
    }

    /// Import schemes from module exports into the current checker's env.
    fn import_exports(
        &mut self,
        module_path: &str,
        import_args: &[Expr],
        exports: &TypeModuleExports,
        span: Span,
    ) {
        // [use mod :as alias]
        if import_args.len() >= 2 {
            if let ExprKind::Keyword(k) = &import_args[0].kind {
                if k == "as" {
                    if let ExprKind::Symbol(alias) = &import_args[1].kind {
                        for (name, scheme) in &exports.schemes {
                            self.env
                                .set_global(format!("{alias}.{name}"), scheme.clone());
                            // The runtime (lower.rs collect_imports) splices all
                            // module forms in, so unqualified names resolve too —
                            // mirror that here or the checker rejects programs
                            // that run fine.
                            self.env.set_global(name.clone(), scheme.clone());
                        }
                        for (name, scheme) in &exports.constructors {
                            self.constructors
                                .insert(format!("{alias}.{name}"), scheme.clone());
                            self.constructors.insert(name.clone(), scheme.clone());
                        }
                        return;
                    }
                }
            }
        }

        // [use mod {name1 name2}] or [use mod [name1 name2]]
        if !import_args.is_empty() {
            let names: Vec<String> = match &import_args[0].kind {
                ExprKind::Map(pairs) => pairs
                    .iter()
                    .filter_map(|(k, _)| {
                        if let ExprKind::Symbol(s) = &k.kind {
                            Some(s.clone())
                        } else {
                            None
                        }
                    })
                    .collect(),
                ExprKind::Vec(items) | ExprKind::List(items) => items
                    .iter()
                    .filter_map(|i| {
                        if let ExprKind::Symbol(s) = &i.kind {
                            Some(s.clone())
                        } else {
                            None
                        }
                    })
                    .collect(),
                _ => vec![],
            };

            if !names.is_empty() {
                for name in &names {
                    if let Some(scheme) = exports.schemes.get(name) {
                        self.env.set_global(name.clone(), scheme.clone());
                        self.add_definition(name, span, span);
                    } else if let Some(scheme) = exports.constructors.get(name) {
                        self.constructors.insert(name.clone(), scheme.clone());
                        self.add_definition(name, span, span);
                    } else {
                        self.errors.push(
                            LoonDiagnostic::new(
                                ErrorCode::E0501,
                                format!("module '{module_path}' does not export '{name}'"),
                            )
                            .with_why(format!("'{name}' is not a public export of this module"))
                            .with_fix("check the module's public declarations")
                            .with_label(span, "not exported", true),
                        );
                    }
                }
                return;
            }
        }

        // Default: qualified (mod.name) AND unqualified. The runtime
        // (lower.rs collect_imports) splices every module form into the
        // program, so `[use mod]` makes `name` itself resolvable — the checker
        // must match, or `loon check` rejects programs `loon run` accepts.
        for (name, scheme) in &exports.schemes {
            self.env
                .set_global(format!("{module_path}.{name}"), scheme.clone());
            self.env.set_global(name.clone(), scheme.clone());
        }
        for (name, scheme) in &exports.constructors {
            self.constructors
                .insert(format!("{module_path}.{name}"), scheme.clone());
            self.constructors.insert(name.clone(), scheme.clone());
        }
    }

    /// Check an entire program. Returns list of diagnostics.
    ///
    /// Pipeline: parse → expand(macro) → typecheck → expand(macro+) → re-typecheck
    pub fn check_program(&mut self, exprs: &[Expr]) -> Vec<LoonDiagnostic> {
        // Phase 1: Regular macro expansion
        let mut expander = crate::macros::MacroExpander::new();
        let expanded = match expander.expand_program(exprs) {
            Ok(e) => e,
            Err(msg) => {
                self.errors.push(LoonDiagnostic::new(ErrorCode::E0100, msg));
                return std::mem::take(&mut self.errors);
            }
        };

        // Phase 2: Type check
        for expr in &expanded {
            self.infer(expr);
        }

        // Phase 3: Type-aware macro expansion (macro+)
        let final_exprs = if expander.has_type_aware_macros() {
            match expander.expand_type_aware(&expanded) {
                Ok(re_expanded) => {
                    // Re-typecheck the expanded expressions
                    for expr in &re_expanded {
                        self.infer(expr);
                    }
                    re_expanded
                }
                Err(msg) => {
                    self.errors.push(LoonDiagnostic::new(ErrorCode::E0100, msg));
                    expanded
                }
            }
        } else {
            expanded
        };

        self.expanded_program = final_exprs;
        self.check_trait_constraints();
        std::mem::take(&mut self.errors)
    }

    /// Get the resolved type of an expression after checking.
    pub fn resolve(&self, ty: &Type) -> Type {
        self.subst.resolve(ty)
    }
}

impl Default for Checker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;

    fn infer_type(src: &str) -> (Type, Vec<LoonDiagnostic>) {
        let exprs = parse(src).unwrap();
        let mut checker = Checker::new();
        let mut ty = Type::Unit;
        for expr in &exprs {
            ty = checker.infer(expr);
        }
        let resolved = checker.resolve(&ty);
        let errors = std::mem::take(&mut checker.errors);
        (resolved, errors)
    }

    fn check_errors(src: &str) -> Vec<LoonDiagnostic> {
        let exprs = parse(src).unwrap();
        let mut checker = Checker::new();
        checker.check_program(&exprs)
    }

    #[test]
    fn infer_int() {
        let (ty, errors) = infer_type("42");
        assert!(errors.is_empty());
        assert_eq!(ty, Type::Int);
    }

    #[test]
    fn escaping_handler_type_checks() {
        // Regression: the escaping/answer-passing style (a handler clause returns
        // a function and uses `resume` non-tail) must type-check. The pure State
        // effect is the canonical case; it used to fail with E0203 (infinite
        // type) and then E0300 (false move of the reused state).
        let errors = check_errors(
            "[effect State [get [] Int] [put [Int] Unit]] \
             [fn run-state [thunk init] \
               [[handle [thunk] \
                   [return x]    [fn [s] x] \
                   [State.get]   [fn [s] [[resume s] s]] \
                   [State.put n] [fn [s] [[resume 0] n]]] \
                 init]] \
             [fn prog [] [let a [State.get]] [State.put [+ a 1]] [State.get]] \
             [fn main [] [run-state prog 0]]",
        );
        assert!(errors.is_empty(), "errors: {errors:?}");
        // Tail-resumptive handlers still check, and a clause that aborts (no
        // resume) unifies its value with the handle's answer type.
        let errors = check_errors(
            "[effect E [op [Int] Int]] [effect F [fail [] Int]] \
             [fn body [] [+ [E.op 1] [F.fail]]] \
             [fn main [] [handle [body] [E.op v] [+ 1 [resume v]] [F.fail] 0]]",
        );
        assert!(errors.is_empty(), "errors: {errors:?}");
    }

    #[test]
    fn generic_adt_construction() {
        // Regression: an uppercase type parameter (the prelude convention) must
        // be recognized so a generic constructor instantiates per use rather
        // than pinning the parameter to the first concrete type seen.
        let errors = check_errors(
            "[type Option T [Some T] None] \
             [fn pick [b] [if b [Some 42] None]] \
             [fn name [b] [if b [Some \"x\"] None]] \
             [fn main [] [pick true] [name false]]",
        );
        assert!(errors.is_empty(), "errors: {errors:?}");
        // Result with two parameters, and nullary-only enums still parse.
        let errors = check_errors(
            "[type Result T E [Ok T] [Err E]] [type Color Red Green Blue] \
             [fn main [] [Ok 7] [Err \"e\"] Green]",
        );
        assert!(errors.is_empty(), "errors: {errors:?}");
    }

    #[test]
    fn infer_addition() {
        let (ty, errors) = infer_type("[+ 1 2]");
        assert!(errors.is_empty());
        assert_eq!(ty, Type::Int);
    }

    #[test]
    fn infer_comparison() {
        let (ty, errors) = infer_type("[> 1 2]");
        assert!(errors.is_empty());
        assert_eq!(ty, Type::Bool);
    }

    #[test]
    fn infer_if() {
        let (ty, errors) = infer_type("[if true 1 2]");
        assert!(errors.is_empty());
        assert_eq!(ty, Type::Int);
    }

    #[test]
    fn infer_defn() {
        let (ty, errors) = infer_type(
            "[fn add [x y] [+ x y]]
             [add 3 4]",
        );
        assert!(errors.is_empty(), "errors: {:?}", errors);
        assert_eq!(ty, Type::Int);
    }

    #[test]
    fn infer_lambda() {
        let (ty, errors) = infer_type("[fn [x] [+ x 1]]");
        assert!(errors.is_empty());
        let resolved = ty;
        assert!(
            matches!(resolved, Type::Fn(params, ret, _) if params.len() == 1 && *ret == Type::Int)
        );
    }

    #[test]
    fn infer_vector() {
        let (ty, errors) = infer_type("#[1 2 3]");
        assert!(errors.is_empty());
        assert_eq!(ty, Type::Con("Vec".to_string(), vec![Type::Int]));
    }

    #[test]
    fn type_error_if_mismatch() {
        let (_, errors) = infer_type("[if 42 1 2]");
        assert!(
            !errors.is_empty(),
            "should have type error for non-bool condition"
        );
    }

    #[test]
    fn type_error_add_string() {
        let (_, errors) = infer_type(r#"[+ "hello" 1]"#);
        assert!(!errors.is_empty(), "should have type error");
    }

    #[test]
    fn infer_fib() {
        let (ty, errors) = infer_type(
            r#"
            [fn fib [n]
              [match n
                0 0
                1 1
                n [+ [fib [- n 1]] [fib [- n 2]]]]]
            [fib 10]
        "#,
        );
        assert!(errors.is_empty(), "errors: {:?}", errors);
        assert_eq!(ty, Type::Int);
    }

    #[test]
    fn type_error_has_span() {
        let errors = check_errors("[if 42 1 2]");
        assert!(!errors.is_empty());
        assert!(errors[0].span().is_some(), "type error should have span");
    }

    #[test]
    fn type_side_table_populated() {
        let exprs = parse("42").unwrap();
        let mut checker = Checker::new();
        for expr in &exprs {
            checker.infer(expr);
        }
        assert!(
            !checker.type_of.is_empty(),
            "type side-table should be populated"
        );
    }

    #[test]
    fn trait_decl_type_checks() {
        let errors = check_errors(
            r#"
            [trait Display [fn display [self] -> String]]
        "#,
        );
        assert!(errors.is_empty(), "errors: {:?}", errors);
    }

    #[test]
    fn sig_matching_passes() {
        let errors = check_errors(
            r#"
            [sig add Int -> Int -> Int]
            [fn add [x y] [+ x y]]
        "#,
        );
        assert!(errors.is_empty(), "errors: {:?}", errors);
    }

    #[test]
    fn sig_mismatch_errors() {
        let errors = check_errors(
            r#"
            [sig add Int -> String -> Int]
            [fn add [x y] [+ x y]]
        "#,
        );
        assert!(!errors.is_empty(), "should have sig mismatch error");
        assert!(
            errors[0].message().contains("does not match"),
            "error: {}",
            errors[0].message()
        );
    }

    #[test]
    fn add_float_works() {
        let (ty, errors) = infer_type("[+ 1.0 2.0]");
        assert!(errors.is_empty(), "errors: {:?}", errors);
        assert_eq!(ty, Type::Float);
    }

    #[test]
    fn polymorphic_add_in_defn() {
        let (ty, errors) = infer_type(
            "[fn double [x] [+ x x]]
             [double 5]",
        );
        assert!(errors.is_empty(), "errors: {:?}", errors);
        assert_eq!(ty, Type::Int);
    }

    #[test]
    fn add_bool_errors() {
        let errors = check_errors("[+ true false]");
        assert!(!errors.is_empty(), "should error: no Add impl for Bool");
        assert!(
            errors.iter().any(|e| e.message().contains("Add")),
            "error should mention Add: {:?}",
            errors
        );
    }

    #[test]
    fn eq_works_on_strings() {
        let (ty, errors) = infer_type(r#"[= "a" "b"]"#);
        assert!(errors.is_empty(), "errors: {:?}", errors);
        assert_eq!(ty, Type::Bool);
    }

    // --- Effect inference tests ---

    fn infer_effects(
        src: &str,
    ) -> (
        std::collections::HashMap<String, EffectRow>,
        Vec<LoonDiagnostic>,
    ) {
        let exprs = parse(src).unwrap();
        let mut checker = Checker::new();
        for expr in &exprs {
            checker.infer(expr);
        }
        let errors = std::mem::take(&mut checker.errors);
        (checker.fn_effects, errors)
    }

    #[test]
    fn effect_infer_io_read_file() {
        let (effects, errors) = infer_effects(r#"[fn load [p] [IO.read-file p]]"#);
        assert!(errors.is_empty(), "errors: {:?}", errors);
        let load_effects = effects.get("load").unwrap();
        assert!(load_effects.contains("IO"), "load should have IO effect");
    }

    #[test]
    fn effect_propagation() {
        let (effects, errors) = infer_effects(
            r#"
            [fn load [p] [IO.read-file p]]
            [fn main [] [load "x"]]
        "#,
        );
        assert!(errors.is_empty(), "errors: {:?}", errors);
        let main_effects = effects.get("main").unwrap();
        assert!(
            main_effects.contains("IO"),
            "main should have IO effect via propagation"
        );
    }

    #[test]
    fn effect_handle_subtracts() {
        let (effects, errors) = infer_effects(
            r#"
            [fn safe []
              [handle [IO.read-file "x"]
                [IO.read-file p] [resume "y"]]]
        "#,
        );
        assert!(errors.is_empty(), "errors: {:?}", errors);
        let safe_effects = effects.get("safe").unwrap();
        assert!(
            !safe_effects.contains("IO"),
            "handle should subtract IO effect"
        );
    }

    #[test]
    fn effect_annotation_passes() {
        let errors = check_errors(
            r#"
            [fn load [path] #{IO} [IO.read-file path]]
        "#,
        );
        assert!(errors.is_empty(), "errors: {:?}", errors);
    }

    #[test]
    fn effect_annotation_extra_ok() {
        let errors = check_errors(
            r#"
            [fn load [path] #{IO Fail} [IO.read-file path]]
        "#,
        );
        assert!(errors.is_empty(), "errors: {:?}", errors);
    }

    #[test]
    fn effect_annotation_missing_errors() {
        let errors = check_errors(
            r#"
            [fn load [path] #{Fail} [IO.read-file path]]
        "#,
        );
        assert!(!errors.is_empty(), "should error for undeclared IO effect");
        assert!(
            errors[0].message().contains("undeclared effect"),
            "error: {}",
            errors[0].message()
        );
    }

    #[test]
    fn effect_question_infers_fail() {
        let (effects, errors) = infer_effects(
            r#"
            [fn try-it [x] [do x]?]
        "#,
        );
        assert!(errors.is_empty(), "errors: {:?}", errors);
        let eff = effects.get("try-it").unwrap();
        assert!(eff.contains("Fail"), "? should infer Fail effect");
    }

    #[test]
    fn effect_annotation_pure_passes() {
        let errors = check_errors(
            r#"
            [fn pure [x] #{} [+ x 1]]
        "#,
        );
        assert!(errors.is_empty(), "errors: {:?}", errors);
    }

    // --- Effect row polymorphism tests ---

    #[test]
    fn effect_row_twice_generalizes_per_use() {
        // ONE definition of `twice` serves a pure and an effectful use in the
        // SAME program: the pure use stays pure, the IO use gets IO. This is
        // the core of effect row polymorphism — the row tail generalizes with
        // let-polymorphism and instantiates fresh per use.
        let (effects, errors) = infer_effects(
            r#"
            [fn twice [f x] [f [f x]]]
            [fn pure-use [] [twice [fn [x] [+ x 1]] 0]]
            [fn io-use [] [twice [fn [p] [IO.read-file p]] "f"]]
        "#,
        );
        assert!(errors.is_empty(), "errors: {:?}", errors);
        let twice = effects.get("twice").unwrap();
        assert!(
            twice.labels.is_empty(),
            "twice has no concrete effects of its own, got: {}",
            twice.render()
        );
        let pure_use = effects.get("pure-use").unwrap();
        assert!(
            !pure_use.contains("IO"),
            "pure use of twice must stay pure, got: {}",
            pure_use.render()
        );
        let io_use = effects.get("io-use").unwrap();
        assert!(
            io_use.contains("IO"),
            "IO use of twice must carry IO, got: {}",
            io_use.render()
        );
    }

    #[test]
    fn effect_row_map_propagates_lambda_effects() {
        let (effects, errors) = infer_effects(
            r#"
            [fn load-all [v] [map [fn [p] [IO.read-file p]] v]]
            [fn squares [v] [map [fn [x] [* x x]] v]]
        "#,
        );
        assert!(errors.is_empty(), "errors: {:?}", errors);
        let load_all = effects.get("load-all").unwrap();
        assert!(
            load_all.contains("IO"),
            "map with an IO lambda must carry IO, got: {}",
            load_all.render()
        );
        let squares = effects.get("squares").unwrap();
        assert!(
            !squares.contains("IO"),
            "map with a pure lambda must stay pure, got: {}",
            squares.render()
        );
    }

    #[test]
    fn effect_row_filter_and_fold_propagate() {
        let (effects, errors) = infer_effects(
            r#"
            [fn keep [v]
              [filter [fn [x] [IO.read-file "f"] [> x 0]] v]]
            [fn total [v]
              [fold 0 [fn [a b] [IO.read-file "f"] [+ a b]] v]]
        "#,
        );
        assert!(errors.is_empty(), "errors: {:?}", errors);
        let keep = effects.get("keep").unwrap();
        assert!(
            keep.contains("IO"),
            "filter with an IO predicate must carry IO, got: {}",
            keep.render()
        );
        let total = effects.get("total").unwrap();
        assert!(
            total.contains("IO"),
            "fold with an IO reducer must carry IO, got: {}",
            total.render()
        );
    }

    #[test]
    fn effect_row_handle_with_open_tail_does_not_leak() {
        // `run-io` handles IO around a call to its function ARGUMENT — the
        // body row is open (the argument's effects flow through the tail).
        // Handling must constrain that tail so IO cannot leak through it:
        // callers passing an IO function must NOT get IO.
        let (effects, errors) = infer_effects(
            r#"
            [fn run-io [f]
              [handle [f "p"]
                [IO.read-file p] [resume "data"]]]
            [fn main [] [run-io [fn [p] [IO.read-file p]]]]
        "#,
        );
        assert!(errors.is_empty(), "errors: {:?}", errors);
        let run_io = effects.get("run-io").unwrap();
        assert!(
            !run_io.contains("IO"),
            "run-io handles IO, got: {}",
            run_io.render()
        );
        let main = effects.get("main").unwrap();
        assert!(
            !main.contains("IO"),
            "IO must not leak through the handled body's tail, got: {}",
            main.render()
        );
    }

    #[test]
    fn effect_row_handle_leaves_unhandled_effects() {
        // Handling one effect leaves the other flowing out through the row.
        let (effects, errors) = infer_effects(
            r#"
            [effect A [a [] Int]]
            [effect B [b [] Int]]
            [fn partial []
              [handle [+ [A.a] [B.b]]
                [A.a] [resume 1]]]
        "#,
        );
        assert!(errors.is_empty(), "errors: {:?}", errors);
        let partial = effects.get("partial").unwrap();
        assert!(
            !partial.contains("A"),
            "A is handled, got: {}",
            partial.render()
        );
        assert!(
            partial.contains("B"),
            "B is unhandled and must remain, got: {}",
            partial.render()
        );
    }

    #[test]
    fn effect_row_recursive_call_inside_handle_stays_clean() {
        // A recursive self-call INSIDE the handled body aliases the
        // function's own (monomorphic) row with the body row; the handle
        // boundary must not bake the handled label into the function's row.
        let (effects, errors) = infer_effects(
            r#"
            [effect A [a [] Int]]
            [fn weird [n]
              [if [= n 0]
                0
                [handle [+ [A.a] [weird [- n 1]]]
                  [A.a] [resume 1]]]]
            [fn main [] #{} [println [weird 3]]]
        "#,
        );
        assert!(errors.is_empty(), "errors: {:?}", errors);
        let weird = effects.get("weird").unwrap();
        assert!(
            !weird.contains("A"),
            "weird fully handles A, got: {}",
            weird.render()
        );
    }

    #[test]
    fn effect_row_param_called_before_handle_stays_clean() {
        // Calling a parameter before the handle links its row to the
        // enclosing row; handling around a second call must not force the
        // handled label into the function's own row (callers passing a PURE
        // argument must stay clean).
        let (effects, errors) = infer_effects(
            r#"
            [effect A [a [] Int]]
            [fn h [f]
              [f 1]
              [handle [f 2]
                [A.a] [resume 1]]]
            [fn caller [] #{} [h [fn [x] [* x 2]]]]
        "#,
        );
        assert!(errors.is_empty(), "errors: {:?}", errors);
        let caller = effects.get("caller").unwrap();
        assert!(
            !caller.contains("A"),
            "caller passes a pure argument, got: {}",
            caller.render()
        );
    }

    #[test]
    fn effect_row_scrub_keeps_performs_from_outside_the_handle() {
        // The scrub at the handle boundary must NOT remove a handled label
        // that the function genuinely performs outside the handle, even when
        // recursion aliases the body row with the function's own row.
        let (effects, errors) = infer_effects(
            r#"
            [effect A [a [] Int]]
            [fn g [n]
              [A.a]
              [if [= n 0]
                0
                [handle [g [- n 1]]
                  [A.a] [resume 1]]]]
        "#,
        );
        assert!(errors.is_empty(), "errors: {:?}", errors);
        let g = effects.get("g").unwrap();
        assert!(
            g.contains("A"),
            "g performs A before the handle — must keep it, got: {}",
            g.render()
        );
    }

    #[test]
    fn effect_row_scrub_keeps_clause_performs() {
        // A handler clause runs OUTSIDE the handled region; a clause that
        // re-performs the handled effect (re-raise) keeps it in the
        // function's row even when recursion aliases the rows.
        let (effects, errors) = infer_effects(
            r#"
            [effect A [a [] Int]]
            [fn h [n]
              [if [= n 0]
                0
                [handle [h [- n 1]]
                  [A.a] [do [A.a] [resume 1]]]]]
        "#,
        );
        assert!(errors.is_empty(), "errors: {:?}", errors);
        let h = effects.get("h").unwrap();
        assert!(
            h.contains("A"),
            "the clause re-performs A unhandled — must keep it, got: {}",
            h.render()
        );
    }

    #[test]
    fn effect_row_param_called_after_handle_overapproximates() {
        // PINNED over-approximation (deliberate, Koka-style): when a
        // parameter is called inside a handle and again AFTER it, the
        // boundary forces the handled label into the parameter's row (that
        // is the no-leak guarantee), so the later call re-absorbs the label
        // into the function's row — even for callers passing a pure
        // argument. Precision here would need subsumption-based (not
        // equality-based) row unification. See DESIGN.md "Effect Rows".
        let (effects, errors) = infer_effects(
            r#"
            [effect A [a [] Int]]
            [fn run-both [f]
              [handle [f 1]
                [A.a] [resume 0]]
              [f 2]]
        "#,
        );
        assert!(errors.is_empty(), "errors: {:?}", errors);
        let run_both = effects.get("run-both").unwrap();
        assert!(
            run_both.contains("A"),
            "current (pinned) behavior: the post-handle call re-reports A, got: {}",
            run_both.render()
        );
    }

    #[test]
    fn effect_row_occurs_check() {
        // Unifying two rows that share a tail but disagree on labels would
        // need the tail to absorb a label into itself (`e ~ {IO | e}`) — an
        // infinite row. The occurs check must reject it.
        let mut subst = Subst::new();
        let tail = subst.fresh_var();
        let a = EffectRow {
            labels: std::iter::once("IO".to_string()).collect(),
            tail: Some(tail),
        };
        let b = EffectRow {
            labels: std::collections::BTreeSet::new(),
            tail: Some(tail),
        };
        assert!(
            unify_effect_rows(&mut subst, &a, &b).is_err(),
            "occurs check must reject e ~ {{IO | e}}"
        );
        // Sanity: the same rows with distinct tails unify fine.
        let mut subst = Subst::new();
        let t1 = subst.fresh_var();
        let t2 = subst.fresh_var();
        let a = EffectRow {
            labels: std::iter::once("IO".to_string()).collect(),
            tail: Some(t1),
        };
        let b = EffectRow {
            labels: std::collections::BTreeSet::new(),
            tail: Some(t2),
        };
        assert!(unify_effect_rows(&mut subst, &a, &b).is_ok());
        assert!(
            subst.resolve_effect_row(&b).contains("IO"),
            "the open tail must absorb the missing label"
        );
    }

    #[test]
    fn effect_row_closed_rows_mismatch_errors() {
        let mut subst = Subst::new();
        let a = EffectRow::closed(std::iter::once("IO".to_string()).collect());
        let b = EffectRow::pure();
        let err = unify_effect_rows(&mut subst, &a, &b).unwrap_err();
        let msg = format!("{err:?}");
        assert!(msg.contains("IO"), "error should name the effect: {msg}");
    }

    #[test]
    fn effect_row_render_readable() {
        let pure = EffectRow::pure();
        assert_eq!(pure.render(), "pure");
        let io = EffectRow::closed(std::iter::once("IO".to_string()).collect());
        assert_eq!(io.render(), "IO");
        let mut subst = Subst::new();
        let open = EffectRow {
            labels: ["Fail".to_string(), "IO".to_string()].into_iter().collect(),
            tail: Some(subst.fresh_var()),
        };
        // Rows render without internal variable ids.
        assert_eq!(open.render(), "Fail + IO + e");
    }

    #[test]
    fn effect_row_sig_on_effectful_fn_passes() {
        // A [sig] constrains value types only; it must not conflict with the
        // function's inferred effect row.
        let errors = check_errors(
            r#"
            [sig load : String -> String]
            [fn load [path] #{IO} [IO.read-file path]]
        "#,
        );
        assert!(errors.is_empty(), "errors: {:?}", errors);
    }

    #[test]
    fn effect_row_annotation_still_asserts_with_hof() {
        // Assertion mode survives rows: an effect annotation on a function
        // that gets its effect FROM a higher-order call still asserts.
        let errors = check_errors(
            r#"
            [fn twice [f x] [f [f x]]]
            [fn io-use [] #{} [twice [fn [p] [IO.read-file p]] "f"]]
        "#,
        );
        assert!(
            !errors.is_empty(),
            "declared pure but performs IO through twice — must error"
        );
        assert!(
            errors[0].message().contains("undeclared effect"),
            "error: {}",
            errors[0].message()
        );
    }

    #[test]
    fn effect_row_ctor_passes_to_effectful_hof() {
        // User ADT constructors get an open, quantified effect-row tail
        // (like builtins), so passing one to a higher-order function whose
        // parameter row carries concrete labels must not produce a false
        // effect mismatch.
        let errors = check_errors(
            r#"
            [type Box [MkBox Int]]
            [fn do-both [f x]
              [IO.println "hi"]
              [f x]]
            [fn main [] [println [do-both MkBox 1]]]
        "#,
        );
        assert!(errors.is_empty(), "errors: {:?}", errors);
    }

    #[test]
    fn effect_row_ctor_passes_through_handle_boundary() {
        // A handle boundary forces the handled label into f's parameter row;
        // an effect-polymorphic constructor must still unify with it.
        let errors = check_errors(
            r#"
            [effect Ask [ask [] Int]]
            [type Box [MkBox Int]]
            [fn call-it [f]
              [handle [f 1]
                [Ask.ask] [resume 42]]]
            [fn main [] [println [call-it MkBox]]]
        "#,
        );
        assert!(errors.is_empty(), "errors: {:?}", errors);
    }

    #[test]
    fn effect_row_mismatch_classified_as_e0403() {
        // Effect-row unification failures must land in the E04xx effect
        // family with effects-specific why/fix text, not fall through to the
        // generic E0200 value-type mismatch.
        let mut checker = Checker::new();
        let a = EffectRow::closed(std::iter::once("IO".to_string()).collect());
        let b = EffectRow::pure();
        let err = unify_effect_rows(&mut checker.subst, &a, &b).unwrap_err();
        checker.push_unify_error(err, Span::ZERO);
        assert_eq!(checker.errors.len(), 1);
        let diag = &checker.errors[0];
        assert_eq!(diag.code, ErrorCode::E0403, "got: {diag:?}");
        assert_eq!(diag.code.category(), "effect");
        assert!(
            diag.why.contains("effect"),
            "why-text should talk about effects, got: {:?}",
            diag.why
        );
    }

    #[test]
    fn infinite_effect_row_classified_as_e0403() {
        // The row occurs check (`e ~ {IO | e}`) is also an effect error.
        let mut checker = Checker::new();
        let tail = checker.subst.fresh_var();
        let a = EffectRow {
            labels: std::iter::once("IO".to_string()).collect(),
            tail: Some(tail),
        };
        let b = EffectRow {
            labels: std::collections::BTreeSet::new(),
            tail: Some(tail),
        };
        let err = unify_effect_rows(&mut checker.subst, &a, &b).unwrap_err();
        checker.push_unify_error(err, Span::ZERO);
        assert_eq!(checker.errors.len(), 1);
        assert_eq!(checker.errors[0].code, ErrorCode::E0403);
    }

    // --- Row polymorphism / Record tests ---

    #[test]
    fn record_literal_has_record_row_type() {
        let (ty, errors) = infer_type("{:x 1 :y 2}");
        assert!(errors.is_empty(), "errors: {:?}", errors);
        match ty {
            Type::Record(inner) => match *inner {
                Type::Row(fields, None) => {
                    assert_eq!(fields.len(), 2);
                    assert!(fields.iter().any(|(n, t)| n == "x" && *t == Type::Int));
                    assert!(fields.iter().any(|(n, t)| n == "y" && *t == Type::Int));
                }
                other => panic!("expected closed Row, got {:?}", other),
            },
            other => panic!("expected Record, got {:?}", other),
        }
    }

    #[test]
    fn record_structural_subtyping_via_get() {
        // A function that accesses :x should accept a wider record with extra fields
        let (ty, errors) = infer_type(
            r#"
            [fn get-x [r] [get r :x]]
            [get-x {:x 42 :y "hello"}]
        "#,
        );
        assert!(errors.is_empty(), "errors: {:?}", errors);
        assert_eq!(ty, Type::Int);
    }

    #[test]
    fn record_field_type_mismatch_errors() {
        // Passing a record where :x is a String to a function expecting :x as Int
        let errors = check_errors(
            r#"
            [fn add-x [r] [+ [get r :x] 1]]
            [add-x {:x "oops"}]
        "#,
        );
        assert!(
            !errors.is_empty(),
            "should have type error for field type mismatch"
        );
    }

    #[test]
    fn definitions_populated_for_defn() {
        let exprs = parse("[fn add [x y] [+ x y]]").unwrap();
        let mut checker = Checker::new();
        checker.check_program(&exprs);
        let def = checker.lookup_definition("add");
        assert!(def.is_some(), "should have definition for 'add'");
    }

    #[test]
    fn definitions_populated_for_let() {
        let exprs = parse("[let x 42]").unwrap();
        let mut checker = Checker::new();
        checker.check_program(&exprs);
        let def = checker.lookup_definition("x");
        assert!(def.is_some(), "should have definition for 'x'");
    }

    #[test]
    fn definitions_populated_for_type_def() {
        let exprs = parse("[type Color Red Green Blue]").unwrap();
        let mut checker = Checker::new();
        checker.check_program(&exprs);
        let def = checker.lookup_definition("Color");
        assert!(def.is_some(), "should have definition for 'Color'");
        let red_def = checker.lookup_definition("Red");
        assert!(red_def.is_some(), "should have definition for 'Red'");
    }

    #[test]
    fn references_populated_for_symbols() {
        let exprs = parse("[let x 42]\n[+ x 1]").unwrap();
        let mut checker = Checker::new();
        checker.check_program(&exprs);
        let x_refs: Vec<_> = checker
            .references
            .iter()
            .filter(|r| r.name == "x")
            .collect();
        assert!(!x_refs.is_empty(), "should have references to 'x'");
    }

    // --- Trait bounds in schemes ---

    #[test]
    fn scheme_has_add_bound_for_double() {
        let exprs = parse("[fn double [x] [+ x x]]").unwrap();
        let mut checker = Checker::new();
        checker.check_program(&exprs);
        let scheme = checker.env.get("double").unwrap();
        assert!(
            !scheme.bounds.is_empty(),
            "double's scheme should have bounds, got: {:?}",
            scheme.bounds
        );
        assert!(
            scheme
                .bounds
                .iter()
                .any(|(_, bs)| bs.iter().any(|b| b.trait_name == "Add")),
            "double should have Add bound, got: {:?}",
            scheme.bounds
        );
    }

    #[test]
    fn scheme_display_shows_bounds() {
        let exprs = parse("[fn double [x] [+ x x]]").unwrap();
        let mut checker = Checker::new();
        checker.check_program(&exprs);
        let scheme = checker.env.get("double").unwrap();
        let display = format!("{}", scheme);
        assert!(
            display.contains("Add"),
            "display should show Add bound: {}",
            display
        );
    }

    // --- derive Copy ---

    #[test]
    fn derive_copy_type_checks() {
        let errors = check_errors(
            r#"
            [derive Copy [type Point [Point Int Int]]]
        "#,
        );
        assert!(errors.is_empty(), "errors: {:?}", errors);
    }

    #[test]
    fn derive_copy_registered() {
        let exprs = parse(r#"[derive Copy [type Point [Point Int Int]]]"#).unwrap();
        let mut checker = Checker::new();
        checker.check_program(&exprs);
        assert!(
            checker.derived_copy_types.contains("Point"),
            "Point should be in derived_copy_types"
        );
    }

    // --- catch-errors ---

    #[test]
    fn catch_errors_type_checks() {
        let errors = check_errors(
            r#"
            [catch-errors "[+ 1 2]"]
        "#,
        );
        assert!(errors.is_empty(), "errors: {:?}", errors);
    }

    #[test]
    fn builtin_parity_interp_subset_of_checker() {
        // Checker builtins
        let checker = Checker::new();
        let checker_names: std::collections::HashSet<String> = checker
            .env
            .global_scope()
            .unwrap()
            .keys()
            .cloned()
            .collect();

        // Interpreter builtins (core + DOM)
        let mut interp_env = crate::interp::Env::new();
        crate::interp::builtins::register_builtins(&mut interp_env);
        crate::interp::dom_builtins::register_dom_builtins(&mut interp_env);
        let interp_names: std::collections::HashSet<String> =
            interp_env.globals().keys().cloned().collect();

        let missing: Vec<&String> = interp_names
            .iter()
            .filter(|name| !checker_names.contains(*name))
            .collect();

        assert!(
            missing.is_empty(),
            "Interpreter builtins missing from checker: {:?}\n\
             Add type signatures for these in register_builtins() or register_dom_builtins().",
            {
                let mut sorted = missing.clone();
                sorted.sort();
                sorted
            }
        );
    }

    // ── Transparent wildcard warnings (W0100) ─────────────────

    // ── Always-truthy condition warnings (E0209) ─────────────
    fn e0209_count(src: &str) -> usize {
        check_errors(src)
            .iter()
            .filter(|e| e.code == ErrorCode::E0209)
            .count()
    }

    #[test]
    fn never_falsy_condition_warns() {
        // Int, Float, String, Keyword: can never be falsy — one warning each.
        assert_eq!(e0209_count("[if 0 1 2]"), 1);
        assert_eq!(e0209_count("[if 1.5 1 2]"), 1);
        assert_eq!(e0209_count("[if \"\" 1 2]"), 1);
        assert_eq!(e0209_count("[if :kw 1 2]"), 1);
    }

    #[test]
    fn never_falsy_fix_states_the_rule() {
        let errors = check_errors("[if 0 1 2]");
        let w = errors
            .iter()
            .find(|e| e.code == ErrorCode::E0209)
            .expect("E0209 should fire on an Int condition");
        assert!(w.code.is_warning(), "E0209 must be warning severity");
        assert!(
            w.fix.contains("a value is truthy unless it says no (false) or says nothing ((), None)"),
            "fix text must state the one-sentence rule verbatim: {}", w.fix
        );
    }

    #[test]
    fn never_falsy_does_not_warn_on_falsifiable_types() {
        // Bool, Option, and type variables can all be falsy — no warning.
        assert_eq!(e0209_count("[if true 1 2]"), 0);
        assert_eq!(e0209_count("[if [Some 1] 1 2]"), 0);
        assert_eq!(e0209_count("[if None 1 2]"), 0);
        assert_eq!(e0209_count("[fn f [x] [if x 1 2]]"), 0);
    }

    #[test]
    fn wildcard_warning_lists_caught_constructors() {
        let errors = check_errors(
            "[type Color Red Green Blue]\n\
             [let c Red]\n\
             [match c Red \"red\" _ \"other\"]",
        );
        let warnings: Vec<_> = errors
            .iter()
            .filter(|e| e.code == ErrorCode::W0100)
            .collect();
        assert_eq!(warnings.len(), 1);
        assert!(
            warnings[0].what.contains("Green"),
            "should list Green: {}",
            warnings[0].what
        );
        assert!(
            warnings[0].what.contains("Blue"),
            "should list Blue: {}",
            warnings[0].what
        );
        assert!(
            warnings[0].what.contains("2 constructors"),
            "should say 2: {}",
            warnings[0].what
        );
    }

    #[test]
    fn wildcard_warning_not_emitted_for_full_coverage() {
        let errors = check_errors(
            "[type Color Red Green Blue]\n\
             [let c Red]\n\
             [match c Red \"red\" Green \"green\" Blue \"blue\" _ \"unreachable\"]",
        );
        let warnings: Vec<_> = errors
            .iter()
            .filter(|e| e.code == ErrorCode::W0100)
            .collect();
        assert!(
            warnings.is_empty(),
            "all constructors covered, _ catches nothing: {:?}",
            warnings
        );
    }

    #[test]
    fn variable_binding_also_warns() {
        let errors = check_errors(
            "[type Dir North South]\n\
             [let d North]\n\
             [match d other \"fallback\"]",
        );
        let warnings: Vec<_> = errors
            .iter()
            .filter(|e| e.code == ErrorCode::W0100)
            .collect();
        assert_eq!(
            warnings.len(),
            1,
            "lowercase var should trigger warning: {:?}",
            errors
        );
        assert!(warnings[0].what.contains("North"), "{}", warnings[0].what);
        assert!(warnings[0].what.contains("South"), "{}", warnings[0].what);
    }

    #[test]
    fn no_warning_for_non_adt_match() {
        let errors = check_errors("[match 1 0 \"zero\" _ \"other\"]");
        let warnings: Vec<_> = errors
            .iter()
            .filter(|e| e.code == ErrorCode::W0100)
            .collect();
        assert!(
            warnings.is_empty(),
            "int match should not warn: {:?}",
            warnings
        );
    }

    #[test]
    fn no_warning_without_wildcard() {
        let errors = check_errors(
            "[type Bool2 True2 False2]\n\
             [let b True2]\n\
             [match b True2 \"yes\" False2 \"no\"]",
        );
        assert!(errors.is_empty(), "exhaustive explicit match: {:?}", errors);
    }

    // --- User-defined effects ---

    #[test]
    fn user_effect_declaration_no_error() {
        let errors = check_errors(
            r#"
            [effect Fs [read-file [String] String]]
        "#,
        );
        assert!(errors.is_empty(), "errors: {:?}", errors);
    }

    #[test]
    fn user_effect_infer_propagates() {
        let (effects, errors) = infer_effects(
            r#"
            [effect Fs [read-file [String] String]]
            [fn load [p] [Fs.read-file p]]
        "#,
        );
        assert!(errors.is_empty(), "errors: {:?}", errors);
        let load_eff = effects.get("load").unwrap();
        assert!(
            load_eff.contains("Fs"),
            "load should infer Fs effect, got {:?}",
            load_eff
        );
    }

    #[test]
    fn user_effect_return_type_flows() {
        let (_ty, errors) = infer_type(
            r#"
            [effect Fs [read-file [String] String]]
            [fn load [p] [Fs.read-file p]]
        "#,
        );
        assert!(errors.is_empty(), "errors: {:?}", errors);
        // load's return type should be String — verify via handle expression
        let (ty2, errors2) = infer_type(
            r#"
            [effect Fs [read-file [String] String]]
            [handle [Fs.read-file "a"]
                [Fs.read-file p] [resume "hi"]]
        "#,
        );
        assert!(errors2.is_empty(), "errors: {:?}", errors2);
        assert_eq!(ty2, Type::Str, "handle should return String");
    }

    #[test]
    fn user_effect_arg_type_mismatch() {
        let errors = check_errors(
            r#"
            [effect Fs [read-file [String] String]]
            [Fs.read-file 42]
        "#,
        );
        assert!(!errors.is_empty(), "should have type error for Int arg");
        assert!(
            errors.iter().any(|e| e.code == ErrorCode::E0200),
            "should be a type mismatch error, got: {:?}",
            errors
        );
    }

    #[test]
    fn user_effect_arity_mismatch() {
        let errors = check_errors(
            r#"
            [effect Fs [read-file [String] String]]
            [Fs.read-file "a" "b"]
        "#,
        );
        assert!(!errors.is_empty(), "should have arity error");
        assert!(
            errors.iter().any(|e| e.code == ErrorCode::E0202),
            "should be arity mismatch, got: {:?}",
            errors
        );
    }

    #[test]
    fn user_effect_unknown_op() {
        let errors = check_errors(
            r#"
            [effect Fs [read-file [String] String]]
            [Fs.write-file "a"]
        "#,
        );
        assert!(!errors.is_empty(), "should have unknown op error");
        assert!(
            errors.iter().any(|e| e.code == ErrorCode::E0402),
            "should be E0402 unknown op, got: {:?}",
            errors
        );
    }

    #[test]
    fn user_effect_multi_ops() {
        let errors = check_errors(
            r#"
            [effect Fs
                [read-file [String] String]
                [write-file [String String] Unit]]
            [Fs.read-file "test.txt"]
            [Fs.write-file "out.txt" "data"]
        "#,
        );
        assert!(errors.is_empty(), "errors: {:?}", errors);
    }

    // ── Physics type system tests ─────────────────────────────────

    #[test]
    fn dim_unit_constructor_length() {
        let (ty, errors) = infer_type("[unit 5.0 :m]");
        assert!(errors.is_empty(), "errors: {:?}", errors);
        assert_eq!(ty, Type::Dim(Dimension::length()));
    }

    #[test]
    fn dim_literal_suffix_desugars() {
        // 5.0m should desugar to [unit 5.0 :m]
        let (ty, errors) = infer_type("5.0m");
        assert!(errors.is_empty(), "errors: {:?}", errors);
        assert_eq!(ty, Type::Dim(Dimension::length()));
    }

    #[test]
    fn dim_int_literal_suffix() {
        let (ty, errors) = infer_type("10kg");
        assert!(errors.is_empty(), "errors: {:?}", errors);
        assert_eq!(ty, Type::Dim(Dimension::mass()));
    }

    #[test]
    fn dim_add_same_dimension() {
        let (ty, errors) = infer_type("[+ [unit 1.0 :m] [unit 2.0 :m]]");
        assert!(errors.is_empty(), "errors: {:?}", errors);
        assert_eq!(ty, Type::Dim(Dimension::length()));
    }

    #[test]
    fn dim_add_different_dimensions_error() {
        let errors = check_errors("[+ [unit 1.0 :m] [unit 2.0 :s]]");
        assert!(!errors.is_empty(), "should error on Length + Time");
        assert!(errors.iter().any(|e| e.code == ErrorCode::E0208));
    }

    #[test]
    fn dim_divide_velocity() {
        let (ty, errors) = infer_type("[/ [unit 10.0 :m] [unit 2.0 :s]]");
        assert!(errors.is_empty(), "errors: {:?}", errors);
        assert_eq!(
            ty,
            Type::Dim(Dimension {
                length: 1,
                time: -1,
                ..Dimension::SCALAR
            })
        );
    }

    #[test]
    fn dim_multiply_force() {
        // Force = mass * acceleration = kg * (m/s²)
        let (ty, errors) = infer_type("[* 3.0kg [/ 10.0m [* 2.0s 2.0s]]]");
        assert!(errors.is_empty(), "errors: {:?}", errors);
        // mass(1) * (length(1) / time(2)) = mass(1)*length(1)*time(-2) = Force
        assert_eq!(
            ty,
            Type::Dim(Dimension {
                mass: 1,
                length: 1,
                time: -2,
                ..Dimension::SCALAR
            })
        );
    }

    #[test]
    fn dim_scalar_multiply_preserves_dimension() {
        let (ty, errors) = infer_type("[* 2.0 [unit 5.0 :m]]");
        assert!(errors.is_empty(), "errors: {:?}", errors);
        assert_eq!(ty, Type::Dim(Dimension::length()));
    }

    #[test]
    fn dim_no_dimensionless_divide_same() {
        // m / m → Scalar (NOT Float)
        let (ty, errors) = infer_type("[/ [unit 10.0 :m] [unit 5.0 :m]]");
        assert!(errors.is_empty(), "errors: {:?}", errors);
        assert_eq!(ty, Type::Dim(Dimension::SCALAR));
    }

    #[test]
    fn dim_scalar_plus_float_error() {
        // Scalar + Float should be an error
        let errors = check_errors("[+ [/ 10.0m 5.0m] 1.0]");
        assert!(!errors.is_empty(), "Scalar + Float should be E0208");
        assert!(errors.iter().any(|e| e.code == ErrorCode::E0208));
    }

    #[test]
    fn dim_magnitude_returns_float() {
        let (ty, errors) = infer_type("[magnitude [unit 5.0 :m]]");
        assert!(errors.is_empty(), "errors: {:?}", errors);
        assert_eq!(ty, Type::Float);
    }

    #[test]
    fn dim_scalar_entry() {
        let (ty, errors) = infer_type("[scalar 2.0]");
        assert!(errors.is_empty(), "errors: {:?}", errors);
        assert_eq!(ty, Type::Dim(Dimension::SCALAR));
    }

    #[test]
    fn dim_comparison_same() {
        let (ty, errors) = infer_type("[> [unit 10.0 :m] [unit 5.0 :m]]");
        assert!(errors.is_empty(), "errors: {:?}", errors);
        assert_eq!(ty, Type::Bool);
    }

    #[test]
    fn dim_comparison_different_error() {
        let errors = check_errors("[> [unit 10.0 :m] [unit 5.0 :s]]");
        assert!(
            !errors.is_empty(),
            "should error on comparing Length and Time"
        );
        assert!(errors.iter().any(|e| e.code == ErrorCode::E0208));
    }

    #[test]
    fn dim_polymorphism_double() {
        let (ty, errors) = infer_type(
            r#"
            [fn double [x] [* 2.0 x]]
            [double [unit 5.0 :m]]
        "#,
        );
        assert!(errors.is_empty(), "errors: {:?}", errors);
        assert_eq!(ty, Type::Dim(Dimension::length()));
    }

    #[test]
    fn dim_prefixed_units() {
        let (ty, errors) = infer_type("[unit 5.0 :km]");
        assert!(errors.is_empty(), "errors: {:?}", errors);
        assert_eq!(ty, Type::Dim(Dimension::length()));
    }

    #[test]
    fn dim_derived_unit_newton() {
        let (ty, errors) = infer_type("[unit 10.0 :N]");
        assert!(errors.is_empty(), "errors: {:?}", errors);
        assert_eq!(
            ty,
            Type::Dim(Dimension {
                mass: 1,
                length: 1,
                time: -2,
                ..Dimension::SCALAR
            })
        );
    }

    #[test]
    fn dim_physics_effect_gravity() {
        let errors = check_errors(
            r#"
            [fn get-g []
              [Physics.gravity]]
            [handle [get-g]
              [Physics.gravity] [resume [unit 9.81 :m]]]
        "#,
        );
        // Note: resume provides Length, but Physics.gravity expects Acceleration.
        // Since the handle mechanism doesn't yet unify resume types vs declared return types,
        // this should still pass. The key is that Physics.gravity returns Acceleration type.
        assert!(errors.is_empty(), "errors: {:?}", errors);
    }

    #[test]
    fn dim_physics_effect_returns_typed() {
        let (ty, errors) = infer_type(
            r#"
            [Physics.yield-strength]
        "#,
        );
        assert!(errors.is_empty(), "errors: {:?}", errors);
        // Should be Pressure type
        assert_eq!(
            ty,
            Type::Dim(Dimension {
                mass: 1,
                length: -1,
                time: -2,
                ..Dimension::SCALAR
            })
        );
    }

    #[test]
    fn dim_unknown_unit_error() {
        let errors = check_errors("[unit 5.0 :foobar]");
        assert!(!errors.is_empty(), "should error on unknown unit");
    }

    #[test]
    fn dim_display_named() {
        assert_eq!(Dimension::length().to_string(), "Length");
        assert_eq!(Dimension::SCALAR.to_string(), "Scalar");
        assert_eq!(
            Dimension {
                length: 1,
                time: -1,
                ..Dimension::SCALAR
            }
            .to_string(),
            "Velocity"
        );
        assert_eq!(
            Dimension {
                mass: 1,
                length: 1,
                time: -2,
                ..Dimension::SCALAR
            }
            .to_string(),
            "Force"
        );
    }

    #[test]
    fn dim_const_c_velocity_type() {
        let (ty, errors) = infer_type("Const.c");
        assert!(errors.is_empty(), "errors: {:?}", errors);
        assert_eq!(
            ty,
            Type::Dim(Dimension {
                length: 1,
                time: -1,
                ..Dimension::SCALAR
            })
        );
    }

    #[test]
    fn dim_f64_suffix_still_works() {
        let (ty, errors) = infer_type("5.0f64");
        assert!(errors.is_empty(), "errors: {:?}", errors);
        assert_eq!(ty, Type::Float);
    }

    #[test]
    fn dim_i32_suffix_still_works() {
        let (ty, errors) = infer_type("42i32");
        assert!(errors.is_empty(), "errors: {:?}", errors);
        assert_eq!(ty, Type::Int);
    }
}
