pub mod ownership;

use crate::ast::{Expr, ExprKind, NodeId};
use crate::syntax::Span;
use crate::types::*;

pub struct Checker {
    pub subst: Subst,
    pub env: TypeEnv,
    pub errors: Vec<TypeError>,
    /// ADT constructor types: name → scheme
    pub constructors: std::collections::HashMap<String, Scheme>,
    /// ADT type → constructor names (for exhaustiveness checking)
    pub type_constructors: std::collections::HashMap<String, Vec<String>>,
    /// Type of each expression node (side-table)
    pub type_of: std::collections::HashMap<NodeId, Type>,
    /// Trait declarations
    pub traits: std::collections::HashMap<String, TraitDecl>,
    /// Trait implementations: (trait_name, type_name) → method schemes
    pub trait_impls: std::collections::HashMap<(String, String), std::collections::HashMap<String, Scheme>>,
    /// Pending [sig] declarations: name → (type, span)
    pub pending_sigs: std::collections::HashMap<String, (Type, Span)>,
}

impl Checker {
    pub fn new() -> Self {
        let mut checker = Self {
            subst: Subst::new(),
            env: TypeEnv::new(),
            errors: Vec::new(),
            constructors: std::collections::HashMap::new(),
            type_constructors: std::collections::HashMap::new(),
            type_of: std::collections::HashMap::new(),
            traits: std::collections::HashMap::new(),
            trait_impls: std::collections::HashMap::new(),
            pending_sigs: std::collections::HashMap::new(),
        };
        checker.register_builtins();
        checker.register_prelude();
        checker
    }

    /// Look up the inferred type for a given node.
    pub fn get_type_of(&self, id: NodeId) -> Option<&Type> {
        self.type_of.get(&id)
    }

    fn register_builtins(&mut self) {
        // Arithmetic: ∀a. Add a => a → a → a
        {
            let a = self.subst.fresh();
            let tv = if let Type::Var(v) = a { v } else { unreachable!() };
            self.subst.add_constraint(tv, TraitBound { trait_name: "Add".to_string() });
            let add_scheme = Scheme {
                vars: vec![tv],
                ty: Type::Fn(vec![Type::Var(tv), Type::Var(tv)], Box::new(Type::Var(tv))),
            };
            for op in ["+", "-", "*"] {
                self.env.set_global(op.to_string(), add_scheme.clone());
            }
        }

        // Comparison: ∀a. Ord a => a → a → Bool
        {
            let a = self.subst.fresh();
            let tv = if let Type::Var(v) = a { v } else { unreachable!() };
            self.subst.add_constraint(tv, TraitBound { trait_name: "Ord".to_string() });
            let ord_scheme = Scheme {
                vars: vec![tv],
                ty: Type::Fn(vec![Type::Var(tv), Type::Var(tv)], Box::new(Type::Bool)),
            };
            for op in [">", "<", ">=", "<="] {
                self.env.set_global(op.to_string(), ord_scheme.clone());
            }
        }

        // Equality: ∀a. Eq a => a → a → Bool
        {
            let a = self.subst.fresh();
            let tv = if let Type::Var(v) = a { v } else { unreachable!() };
            self.subst.add_constraint(tv, TraitBound { trait_name: "Eq".to_string() });
            self.env.set_global(
                "=".to_string(),
                Scheme {
                    vars: vec![tv],
                    ty: Type::Fn(vec![Type::Var(tv), Type::Var(tv)], Box::new(Type::Bool)),
                },
            );
        }

        // not: Bool → Bool
        self.env.set_global(
            "not".to_string(),
            Scheme::mono(Type::Fn(vec![Type::Bool], Box::new(Type::Bool))),
        );

        // str: String → ... → String (variadic, approximate as String → String)
        self.env.set_global(
            "str".to_string(),
            Scheme::mono(Type::Fn(vec![Type::Str, Type::Str], Box::new(Type::Str))),
        );

        // println: ∀a. a → ()
        {
            let a = self.subst.fresh();
            let tv = if let Type::Var(v) = a { v } else { unreachable!() };
            self.env.set_global(
                "println".to_string(),
                Scheme {
                    vars: vec![tv],
                    ty: Type::Fn(vec![Type::Var(tv)], Box::new(Type::Unit)),
                },
            );
        }

        // len: ∀a. Vec a → Int
        {
            let a = self.subst.fresh();
            let tv = if let Type::Var(v) = a { v } else { unreachable!() };
            self.env.set_global(
                "len".to_string(),
                Scheme {
                    vars: vec![tv],
                    ty: Type::Fn(
                        vec![Type::Con("Vec".to_string(), vec![Type::Var(tv)])],
                        Box::new(Type::Int),
                    ),
                },
            );
        }

        // nth: ∀a. Vec a → Int → a
        {
            let a = self.subst.fresh();
            let tv = if let Type::Var(v) = a { v } else { unreachable!() };
            self.env.set_global(
                "nth".to_string(),
                Scheme {
                    vars: vec![tv],
                    ty: Type::Fn(
                        vec![
                            Type::Con("Vec".to_string(), vec![Type::Var(tv)]),
                            Type::Int,
                        ],
                        Box::new(Type::Var(tv)),
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
            )),
        );

        // empty?: ∀a. Vec a → Bool
        {
            let a = self.subst.fresh();
            let tv = if let Type::Var(v) = a { v } else { unreachable!() };
            self.env.set_global(
                "empty?".to_string(),
                Scheme {
                    vars: vec![tv],
                    ty: Type::Fn(
                        vec![Type::Con("Vec".to_string(), vec![Type::Var(tv)])],
                        Box::new(Type::Bool),
                    ),
                },
            );
        }

        // contains?: ∀a. Set a → a → Bool
        {
            let a = self.subst.fresh();
            let tv = if let Type::Var(v) = a { v } else { unreachable!() };
            self.env.set_global(
                "contains?".to_string(),
                Scheme {
                    vars: vec![tv],
                    ty: Type::Fn(
                        vec![
                            Type::Con("Set".to_string(), vec![Type::Var(tv)]),
                            Type::Var(tv),
                        ],
                        Box::new(Type::Bool),
                    ),
                },
            );
        }

        // conj: ∀a. Vec a → a → Vec a
        {
            let a = self.subst.fresh();
            let tv = if let Type::Var(v) = a { v } else { unreachable!() };
            self.env.set_global(
                "conj".to_string(),
                Scheme {
                    vars: vec![tv],
                    ty: Type::Fn(
                        vec![
                            Type::Con("Vec".to_string(), vec![Type::Var(tv)]),
                            Type::Var(tv),
                        ],
                        Box::new(Type::Con("Vec".to_string(), vec![Type::Var(tv)])),
                    ),
                },
            );
        }

        // get: ∀v. Map Keyword v → Keyword → v
        {
            let v = self.subst.fresh();
            let tv = if let Type::Var(vv) = v { vv } else { unreachable!() };
            self.env.set_global(
                "get".to_string(),
                Scheme {
                    vars: vec![tv],
                    ty: Type::Fn(
                        vec![
                            Type::Con("Map".to_string(), vec![Type::Keyword, Type::Var(tv)]),
                            Type::Keyword,
                        ],
                        Box::new(Type::Var(tv)),
                    ),
                },
            );
        }

        // assoc: ∀v. Map Keyword v → Keyword → v → Map Keyword v
        {
            let v = self.subst.fresh();
            let tv = if let Type::Var(vv) = v { vv } else { unreachable!() };
            let map_t = Type::Con("Map".to_string(), vec![Type::Keyword, Type::Var(tv)]);
            self.env.set_global(
                "assoc".to_string(),
                Scheme {
                    vars: vec![tv],
                    ty: Type::Fn(
                        vec![map_t.clone(), Type::Keyword, Type::Var(tv)],
                        Box::new(map_t),
                    ),
                },
            );
        }

        // map: ∀a b. (a → b) → Vec a → Vec b
        {
            let a = self.subst.fresh();
            let b = self.subst.fresh();
            let tva = if let Type::Var(v) = a { v } else { unreachable!() };
            let tvb = if let Type::Var(v) = b { v } else { unreachable!() };
            self.env.set_global(
                "map".to_string(),
                Scheme {
                    vars: vec![tva, tvb],
                    ty: Type::Fn(
                        vec![
                            Type::Fn(vec![Type::Var(tva)], Box::new(Type::Var(tvb))),
                            Type::Con("Vec".to_string(), vec![Type::Var(tva)]),
                        ],
                        Box::new(Type::Con("Vec".to_string(), vec![Type::Var(tvb)])),
                    ),
                },
            );
        }

        // filter: ∀a. (a → Bool) → Vec a → Vec a
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a { v } else { unreachable!() };
            self.env.set_global(
                "filter".to_string(),
                Scheme {
                    vars: vec![tva],
                    ty: Type::Fn(
                        vec![
                            Type::Fn(vec![Type::Var(tva)], Box::new(Type::Bool)),
                            Type::Con("Vec".to_string(), vec![Type::Var(tva)]),
                        ],
                        Box::new(Type::Con("Vec".to_string(), vec![Type::Var(tva)])),
                    ),
                },
            );
        }

        // fold: ∀a b. b → (b → a → b) → Vec a → b
        {
            let a = self.subst.fresh();
            let b = self.subst.fresh();
            let tva = if let Type::Var(v) = a { v } else { unreachable!() };
            let tvb = if let Type::Var(v) = b { v } else { unreachable!() };
            self.env.set_global(
                "fold".to_string(),
                Scheme {
                    vars: vec![tva, tvb],
                    ty: Type::Fn(
                        vec![
                            Type::Var(tvb),
                            Type::Fn(
                                vec![Type::Var(tvb), Type::Var(tva)],
                                Box::new(Type::Var(tvb)),
                            ),
                            Type::Con("Vec".to_string(), vec![Type::Var(tva)]),
                        ],
                        Box::new(Type::Var(tvb)),
                    ),
                },
            );
        }

        // each: ∀a. (a → ()) → Vec a → ()
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a { v } else { unreachable!() };
            self.env.set_global(
                "each".to_string(),
                Scheme {
                    vars: vec![tva],
                    ty: Type::Fn(
                        vec![
                            Type::Fn(vec![Type::Var(tva)], Box::new(Type::Unit)),
                            Type::Con("Vec".to_string(), vec![Type::Var(tva)]),
                        ],
                        Box::new(Type::Unit),
                    ),
                },
            );
        }

        // collect: ∀a. Vec a → Vec a
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a { v } else { unreachable!() };
            let vec_a = Type::Con("Vec".to_string(), vec![Type::Var(tva)]);
            self.env.set_global(
                "collect".to_string(),
                Scheme {
                    vars: vec![tva],
                    ty: Type::Fn(vec![vec_a.clone()], Box::new(vec_a)),
                },
            );
        }

        // assert-eq: ∀a. a → a → ()
        {
            let a = self.subst.fresh();
            let tv = if let Type::Var(v) = a { v } else { unreachable!() };
            self.env.set_global(
                "assert-eq".to_string(),
                Scheme {
                    vars: vec![tv],
                    ty: Type::Fn(vec![Type::Var(tv), Type::Var(tv)], Box::new(Type::Unit)),
                },
            );
        }

        // / and %: Int → Int → Int
        let int_bin = Scheme::mono(Type::Fn(vec![Type::Int, Type::Int], Box::new(Type::Int)));
        for op in ["/", "%"] {
            self.env.set_global(op.to_string(), int_bin.clone());
        }

        // or, and: Bool → Bool → Bool
        let bool_bin = Scheme::mono(Type::Fn(vec![Type::Bool, Type::Bool], Box::new(Type::Bool)));
        for op in ["or", "and"] {
            self.env.set_global(op.to_string(), bool_bin.clone());
        }

        // print: ∀a. a → ()
        {
            let a = self.subst.fresh();
            let tv = if let Type::Var(v) = a { v } else { unreachable!() };
            self.env.set_global(
                "print".to_string(),
                Scheme {
                    vars: vec![tv],
                    ty: Type::Fn(vec![Type::Var(tv)], Box::new(Type::Unit)),
                },
            );
        }

        // split: Str → Str → Vec Str
        self.env.set_global(
            "split".to_string(),
            Scheme::mono(Type::Fn(
                vec![Type::Str, Type::Str],
                Box::new(Type::Con("Vec".to_string(), vec![Type::Str])),
            )),
        );

        // join: Str → Vec Str → Str
        self.env.set_global(
            "join".to_string(),
            Scheme::mono(Type::Fn(
                vec![Type::Str, Type::Con("Vec".to_string(), vec![Type::Str])],
                Box::new(Type::Str),
            )),
        );

        // trim: Str → Str
        self.env.set_global(
            "trim".to_string(),
            Scheme::mono(Type::Fn(vec![Type::Str], Box::new(Type::Str))),
        );

        // starts-with?, ends-with?: Str → Str → Bool
        let str_str_bool = Scheme::mono(Type::Fn(vec![Type::Str, Type::Str], Box::new(Type::Bool)));
        for op in ["starts-with?", "ends-with?"] {
            self.env.set_global(op.to_string(), str_str_bool.clone());
        }

        // replace: Str → Str → Str → Str
        self.env.set_global(
            "replace".to_string(),
            Scheme::mono(Type::Fn(vec![Type::Str, Type::Str, Type::Str], Box::new(Type::Str))),
        );

        // uppercase, lowercase: Str → Str
        let str_to_str = Scheme::mono(Type::Fn(vec![Type::Str], Box::new(Type::Str)));
        for op in ["uppercase", "lowercase"] {
            self.env.set_global(op.to_string(), str_to_str.clone());
        }

        // sort-by: ∀a. (a → a → Int) → Vec a → Vec a
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a { v } else { unreachable!() };
            let vec_a = Type::Con("Vec".to_string(), vec![Type::Var(tva)]);
            self.env.set_global(
                "sort-by".to_string(),
                Scheme {
                    vars: vec![tva],
                    ty: Type::Fn(
                        vec![
                            Type::Fn(vec![Type::Var(tva), Type::Var(tva)], Box::new(Type::Int)),
                            vec_a.clone(),
                        ],
                        Box::new(vec_a),
                    ),
                },
            );
        }

        // take: ∀a. Int → Vec a → Vec a
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a { v } else { unreachable!() };
            let vec_a = Type::Con("Vec".to_string(), vec![Type::Var(tva)]);
            self.env.set_global(
                "take".to_string(),
                Scheme {
                    vars: vec![tva],
                    ty: Type::Fn(vec![Type::Int, vec_a.clone()], Box::new(vec_a)),
                },
            );
        }

        // drop: ∀a. Int → Vec a → Vec a
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a { v } else { unreachable!() };
            let vec_a = Type::Con("Vec".to_string(), vec![Type::Var(tva)]);
            self.env.set_global(
                "drop".to_string(),
                Scheme {
                    vars: vec![tva],
                    ty: Type::Fn(vec![Type::Int, vec_a.clone()], Box::new(vec_a)),
                },
            );
        }

        // reverse: ∀a. Vec a → Vec a
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a { v } else { unreachable!() };
            let vec_a = Type::Con("Vec".to_string(), vec![Type::Var(tva)]);
            self.env.set_global(
                "reverse".to_string(),
                Scheme {
                    vars: vec![tva],
                    ty: Type::Fn(vec![vec_a.clone()], Box::new(vec_a)),
                },
            );
        }

        // flatten: ∀a. Vec (Vec a) → Vec a
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a { v } else { unreachable!() };
            let vec_a = Type::Con("Vec".to_string(), vec![Type::Var(tva)]);
            self.env.set_global(
                "flatten".to_string(),
                Scheme {
                    vars: vec![tva],
                    ty: Type::Fn(
                        vec![Type::Con("Vec".to_string(), vec![vec_a.clone()])],
                        Box::new(vec_a),
                    ),
                },
            );
        }

        // chunk: ∀a. Int → Vec a → Vec (Vec a)
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a { v } else { unreachable!() };
            let vec_a = Type::Con("Vec".to_string(), vec![Type::Var(tva)]);
            self.env.set_global(
                "chunk".to_string(),
                Scheme {
                    vars: vec![tva],
                    ty: Type::Fn(
                        vec![Type::Int, vec_a.clone()],
                        Box::new(Type::Con("Vec".to_string(), vec![vec_a])),
                    ),
                },
            );
        }

        // zip: ∀a b. Vec a → Vec b → Vec (a, b)
        {
            let a = self.subst.fresh();
            let b = self.subst.fresh();
            let tva = if let Type::Var(v) = a { v } else { unreachable!() };
            let tvb = if let Type::Var(v) = b { v } else { unreachable!() };
            self.env.set_global(
                "zip".to_string(),
                Scheme {
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
                    ),
                },
            );
        }

        // find: ∀a. (a → Bool) → Vec a → Option a
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a { v } else { unreachable!() };
            self.env.set_global(
                "find".to_string(),
                Scheme {
                    vars: vec![tva],
                    ty: Type::Fn(
                        vec![
                            Type::Fn(vec![Type::Var(tva)], Box::new(Type::Bool)),
                            Type::Con("Vec".to_string(), vec![Type::Var(tva)]),
                        ],
                        Box::new(Type::Con("Option".to_string(), vec![Type::Var(tva)])),
                    ),
                },
            );
        }

        // any?: ∀a. (a → Bool) → Vec a → Bool
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a { v } else { unreachable!() };
            self.env.set_global(
                "any?".to_string(),
                Scheme {
                    vars: vec![tva],
                    ty: Type::Fn(
                        vec![
                            Type::Fn(vec![Type::Var(tva)], Box::new(Type::Bool)),
                            Type::Con("Vec".to_string(), vec![Type::Var(tva)]),
                        ],
                        Box::new(Type::Bool),
                    ),
                },
            );
        }

        // all?: ∀a. (a → Bool) → Vec a → Bool
        {
            let a = self.subst.fresh();
            let tva = if let Type::Var(v) = a { v } else { unreachable!() };
            self.env.set_global(
                "all?".to_string(),
                Scheme {
                    vars: vec![tva],
                    ty: Type::Fn(
                        vec![
                            Type::Fn(vec![Type::Var(tva)], Box::new(Type::Bool)),
                            Type::Con("Vec".to_string(), vec![Type::Var(tva)]),
                        ],
                        Box::new(Type::Bool),
                    ),
                },
            );
        }

        // update: ∀v. Map Keyword v → Keyword → (v → v) → Map Keyword v
        {
            let v = self.subst.fresh();
            let tv = if let Type::Var(vv) = v { vv } else { unreachable!() };
            let map_t = Type::Con("Map".to_string(), vec![Type::Keyword, Type::Var(tv)]);
            self.env.set_global(
                "update".to_string(),
                Scheme {
                    vars: vec![tv],
                    ty: Type::Fn(
                        vec![
                            map_t.clone(),
                            Type::Keyword,
                            Type::Fn(vec![Type::Var(tv)], Box::new(Type::Var(tv))),
                        ],
                        Box::new(map_t),
                    ),
                },
            );
        }

        // entries: ∀v. Map Keyword v → Vec (Keyword, v)
        {
            let v = self.subst.fresh();
            let tv = if let Type::Var(vv) = v { vv } else { unreachable!() };
            self.env.set_global(
                "entries".to_string(),
                Scheme {
                    vars: vec![tv],
                    ty: Type::Fn(
                        vec![Type::Con("Map".to_string(), vec![Type::Keyword, Type::Var(tv)])],
                        Box::new(Type::Con(
                            "Vec".to_string(),
                            vec![Type::Tuple(vec![Type::Keyword, Type::Var(tv)])],
                        )),
                    ),
                },
            );
        }

        // keys: ∀v. Map Keyword v → Vec Keyword
        {
            let v = self.subst.fresh();
            let tv = if let Type::Var(vv) = v { vv } else { unreachable!() };
            self.env.set_global(
                "keys".to_string(),
                Scheme {
                    vars: vec![tv],
                    ty: Type::Fn(
                        vec![Type::Con("Map".to_string(), vec![Type::Keyword, Type::Var(tv)])],
                        Box::new(Type::Con("Vec".to_string(), vec![Type::Keyword])),
                    ),
                },
            );
        }

        // values: ∀v. Map Keyword v → Vec v
        {
            let v = self.subst.fresh();
            let tv = if let Type::Var(vv) = v { vv } else { unreachable!() };
            self.env.set_global(
                "values".to_string(),
                Scheme {
                    vars: vec![tv],
                    ty: Type::Fn(
                        vec![Type::Con("Map".to_string(), vec![Type::Keyword, Type::Var(tv)])],
                        Box::new(Type::Con("Vec".to_string(), vec![Type::Var(tv)])),
                    ),
                },
            );
        }

        // merge: ∀v. Map Keyword v → Map Keyword v → Map Keyword v
        {
            let v = self.subst.fresh();
            let tv = if let Type::Var(vv) = v { vv } else { unreachable!() };
            let map_t = Type::Con("Map".to_string(), vec![Type::Keyword, Type::Var(tv)]);
            self.env.set_global(
                "merge".to_string(),
                Scheme {
                    vars: vec![tv],
                    ty: Type::Fn(vec![map_t.clone(), map_t.clone()], Box::new(map_t)),
                },
            );
        }

        // remove: ∀v. Map Keyword v → Keyword → Map Keyword v
        {
            let v = self.subst.fresh();
            let tv = if let Type::Var(vv) = v { vv } else { unreachable!() };
            let map_t = Type::Con("Map".to_string(), vec![Type::Keyword, Type::Var(tv)]);
            self.env.set_global(
                "remove".to_string(),
                Scheme {
                    vars: vec![tv],
                    ty: Type::Fn(vec![map_t.clone(), Type::Keyword], Box::new(map_t)),
                },
            );
        }

        // push!: ∀a. Vec a → a → Vec a
        {
            let a = self.subst.fresh();
            let tv = if let Type::Var(v) = a { v } else { unreachable!() };
            let vec_a = Type::Con("Vec".to_string(), vec![Type::Var(tv)]);
            self.env.set_global(
                "push!".to_string(),
                Scheme {
                    vars: vec![tv],
                    ty: Type::Fn(vec![vec_a.clone(), Type::Var(tv)], Box::new(vec_a)),
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
        self.traits.insert("Add".to_string(), TraitDecl {
            name: "Add".to_string(),
            type_params: vec![],
            methods: vec![TraitMethod {
                name: "add".to_string(),
                param_types: vec![Type::Con("Self".to_string(), vec![]), Type::Con("Self".to_string(), vec![])],
                ret_type: Type::Con("Self".to_string(), vec![]),
            }],
        });
        self.traits.insert("Eq".to_string(), TraitDecl {
            name: "Eq".to_string(),
            type_params: vec![],
            methods: vec![TraitMethod {
                name: "eq".to_string(),
                param_types: vec![Type::Con("Self".to_string(), vec![]), Type::Con("Self".to_string(), vec![])],
                ret_type: Type::Bool,
            }],
        });
        self.traits.insert("Ord".to_string(), TraitDecl {
            name: "Ord".to_string(),
            type_params: vec![],
            methods: vec![TraitMethod {
                name: "lt".to_string(),
                param_types: vec![Type::Con("Self".to_string(), vec![]), Type::Con("Self".to_string(), vec![])],
                ret_type: Type::Bool,
            }],
        });
        self.traits.insert("Display".to_string(), TraitDecl {
            name: "Display".to_string(),
            type_params: vec![],
            methods: vec![TraitMethod {
                name: "display".to_string(),
                param_types: vec![Type::Con("Self".to_string(), vec![])],
                ret_type: Type::Str,
            }],
        });

        // Register primitive trait impls
        let empty = std::collections::HashMap::new();
        for ty in ["Int", "Float"] {
            self.trait_impls.insert(("Add".to_string(), ty.to_string()), empty.clone());
            self.trait_impls.insert(("Ord".to_string(), ty.to_string()), empty.clone());
        }
        for ty in ["Int", "Float", "Bool", "String", "Keyword"] {
            self.trait_impls.insert(("Eq".to_string(), ty.to_string()), empty.clone());
        }
        for ty in ["Int", "Float", "Bool", "String", "Keyword"] {
            self.trait_impls.insert(("Display".to_string(), ty.to_string()), empty.clone());
        }
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
                    instantiate(&mut self.subst, scheme)
                } else {
                    self.errors.push(TypeError::at(
                        format!("unbound symbol '{name}'"),
                        expr.span,
                    ));
                    self.subst.fresh()
                }
            }

            ExprKind::Vec(items) => {
                let elem = self.subst.fresh();
                for item in items {
                    let t = self.infer(item);
                    if let Err(e) = unify(&mut self.subst, &elem, &t) {
                        self.errors.push(e.with_span(expr.span));
                    }
                }
                Type::Con("Vec".to_string(), vec![elem])
            }

            ExprKind::Set(items) => {
                let elem = self.subst.fresh();
                for item in items {
                    let t = self.infer(item);
                    if let Err(e) = unify(&mut self.subst, &elem, &t) {
                        self.errors.push(e.with_span(expr.span));
                    }
                }
                Type::Con("Set".to_string(), vec![elem])
            }

            ExprKind::Map(pairs) => {
                let key_t = self.subst.fresh();
                let val_t = self.subst.fresh();
                for (k, v) in pairs {
                    let kt = self.infer(k);
                    let vt = self.infer(v);
                    if let Err(e) = unify(&mut self.subst, &key_t, &kt) {
                        self.errors.push(e.with_span(expr.span));
                    }
                    if let Err(e) = unify(&mut self.subst, &val_t, &vt) {
                        self.errors.push(e.with_span(expr.span));
                    }
                }
                Type::Con("Map".to_string(), vec![key_t, val_t])
            }

            ExprKind::Tuple(items) => {
                let types: Vec<Type> = items.iter().map(|e| self.infer(e)).collect();
                Type::Tuple(types)
            }

            ExprKind::List(items) if items.is_empty() => Type::Unit,

            ExprKind::List(items) => self.infer_list(items, expr.span),
        }
    }

    fn infer_list(&mut self, items: &[Expr], span: Span) -> Type {
        let head = &items[0];
        if let ExprKind::Symbol(s) = &head.kind {
            match s.as_str() {
                "defn" => return self.infer_defn(&items[1..], span),
                "let" => return self.infer_let(&items[1..]),
                "fn" => return self.infer_lambda(&items[1..]),
                "if" => return self.infer_if(&items[1..], span),
                "do" => return self.infer_do(&items[1..]),
                "match" => return self.infer_match(&items[1..], span),
                "|>" => return self.infer_pipe(&items[1..], span),
                "type" => return self.infer_type_def(&items[1..]),
                "trait" => return self.infer_trait_def(&items[1..]),
                "impl" => return self.infer_impl_def(&items[1..], span),
                "sig" => return self.infer_sig(&items[1..], span),
                "test" | "pub" | "mut" => {
                    if items.len() > 1 {
                        return self.infer_list(&items[1..], span);
                    }
                    return Type::Unit;
                }
                _ => {}
            }
        }

        // Function application
        let func_ty = self.infer(head);
        let arg_types: Vec<Type> = items[1..].iter().map(|a| self.infer(a)).collect();
        let ret = self.subst.fresh();

        let expected_fn = Type::Fn(arg_types, Box::new(ret.clone()));
        if let Err(e) = unify(&mut self.subst, &func_ty, &expected_fn) {
            self.errors.push(e.with_span(span));
        }
        ret
    }

    fn infer_defn(&mut self, args: &[Expr], span: Span) -> Type {
        if args.len() < 2 {
            return Type::Unit;
        }
        let name = match &args[0].kind {
            ExprKind::Symbol(s) => s.clone(),
            _ => return Type::Unit,
        };

        // Multi-arity check
        if matches!(args[1].kind, ExprKind::Tuple(_)) {
            let ret = self.subst.fresh();
            for clause_expr in &args[1..] {
                if let ExprKind::Tuple(clause_items) = &clause_expr.kind {
                    if clause_items.len() >= 2 {
                        let clause_ret = self.infer_fn_clause(&clause_items[0], &clause_items[1..]);
                        if let Err(e) = unify(&mut self.subst, &ret, &clause_ret) {
                            self.errors.push(e.with_span(span));
                        }
                    }
                }
            }
            let scheme = generalize(&self.env, &self.subst, &ret);
            self.env.set_global(name, scheme);
            return Type::Unit;
        }

        // Single-arity
        if let ExprKind::List(params) = &args[1].kind {
            let mut body_start = 2;
            if body_start < args.len() {
                if let ExprKind::Symbol(s) = &args[body_start].kind {
                    if s == "/" {
                        body_start += 2;
                    }
                }
            }

            self.env.push_scope();
            let param_types = self.infer_params(params);

            let temp_ret = self.subst.fresh();
            let temp_fn_ty = Type::Fn(
                param_types.clone(),
                Box::new(temp_ret.clone()),
            );
            self.env.set(name.clone(), Scheme::mono(temp_fn_ty));

            let mut body_ty = Type::Unit;
            for body_expr in &args[body_start..] {
                body_ty = self.infer(body_expr);
            }

            if let Err(e) = unify(&mut self.subst, &temp_ret, &body_ty) {
                self.errors.push(e.with_span(span));
            }

            self.env.pop_scope();

            let fn_ty = Type::Fn(param_types, Box::new(body_ty));

            // Check against pending sig if present
            if let Some((sig_ty, sig_span)) = self.pending_sigs.remove(&name) {
                if let Err(_e) = unify(&mut self.subst, &fn_ty, &sig_ty) {
                    let resolved_fn = self.subst.resolve(&fn_ty);
                    let resolved_sig = self.subst.resolve(&sig_ty);
                    self.errors.push(TypeError::at(
                        format!(
                            "inferred type `{}` does not match declared signature `{}`",
                            resolved_fn, resolved_sig
                        ),
                        sig_span,
                    ));
                }
            }

            let scheme = generalize(&self.env, &self.subst, &fn_ty);
            self.env.set_global(name, scheme);
        }
        Type::Unit
    }

    fn infer_fn_clause(&mut self, params_expr: &Expr, body: &[Expr]) -> Type {
        if let ExprKind::List(params) = &params_expr.kind {
            self.env.push_scope();
            let _param_types = self.infer_params(params);

            let mut body_ty = Type::Unit;
            for expr in body {
                body_ty = self.infer(expr);
            }

            self.env.pop_scope();
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
            }
            _ => {}
        }

        val_ty
    }

    fn infer_lambda(&mut self, args: &[Expr]) -> Type {
        if args.is_empty() {
            return self.subst.fresh();
        }

        if let ExprKind::List(params) = &args[0].kind {
            self.env.push_scope();
            let param_types = self.infer_params(params);

            let mut body_ty = Type::Unit;
            for expr in &args[1..] {
                body_ty = self.infer(expr);
            }
            self.env.pop_scope();

            Type::Fn(param_types, Box::new(body_ty))
        } else {
            self.subst.fresh()
        }
    }

    fn infer_if(&mut self, args: &[Expr], span: Span) -> Type {
        if args.len() < 2 {
            return Type::Unit;
        }
        let cond_ty = self.infer(&args[0]);
        if let Err(e) = unify(&mut self.subst, &cond_ty, &Type::Bool) {
            self.errors.push(e.with_span(args[0].span));
        }
        let then_ty = self.infer(&args[1]);
        if args.len() > 2 {
            let else_ty = self.infer(&args[2]);
            if let Err(e) = unify(&mut self.subst, &then_ty, &else_ty) {
                self.errors.push(e.with_span(span));
            }
        }
        then_ty
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
            match &arms[i].kind {
                ExprKind::Symbol(s) if s == "_" => has_wildcard = true,
                ExprKind::Symbol(s) if !s.starts_with(char::is_uppercase) && s != "=>" => {
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

            if let ExprKind::Symbol(s) = &arms[i].kind {
                if s == "=>"
                    && i + 1 < arms.len() {
                        let body_ty = self.infer(&arms[i + 1]);
                        if let Err(e) = unify(&mut self.subst, &result_ty, &body_ty) {
                            self.errors.push(e.with_span(arms[i + 1].span));
                        }
                        i += 2;
                        continue;
                }
            }
            i += 1;
        }

        if !has_wildcard {
            let resolved = self.subst.resolve(&scrutinee_ty);
            if let Type::Con(ref type_name, _) = resolved {
                if let Some(all_ctors) = self.type_constructors.get(type_name).cloned() {
                    let missing: Vec<&String> = all_ctors
                        .iter()
                        .filter(|c| !covered_ctors.contains(c))
                        .collect();
                    if !missing.is_empty() {
                        self.errors.push(TypeError::at(
                            format!(
                                "non-exhaustive match on {}: missing {}",
                                type_name,
                                missing.iter().map(|s| s.as_str()).collect::<Vec<_>>().join(", ")
                            ),
                            span,
                        ));
                    }
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
                    let explicit_args: Vec<Type> = items[1..].iter().map(|a| self.infer(a)).collect();

                    let arg_tys = if explicit_args.is_empty() {
                        vec![current]
                    } else {
                        let mut tys = explicit_args;
                        tys.push(current);
                        tys
                    };

                    let ret = self.subst.fresh();
                    let expected = Type::Fn(arg_tys, Box::new(ret.clone()));
                    if let Err(e) = unify(&mut self.subst, &func_ty, &expected) {
                        self.errors.push(e.with_span(step.span));
                    }
                    current = ret;
                }
                ExprKind::Symbol(_) => {
                    let func_ty = self.infer(step);
                    let ret = self.subst.fresh();
                    let expected = Type::Fn(vec![current], Box::new(ret.clone()));
                    if let Err(e) = unify(&mut self.subst, &func_ty, &expected) {
                        self.errors.push(e.with_span(step.span));
                    }
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

        let mut type_params = Vec::new();
        let mut ctor_start = 1;
        let mut ctor_names = Vec::new();
        for arg in &args[1..] {
            if let ExprKind::Symbol(s) = &arg.kind {
                if s.chars().next().is_some_and(|c| c.is_uppercase()) {
                    break;
                }
                let tv = self.subst.fresh();
                if let Type::Var(v) = tv {
                    type_params.push((s.clone(), v));
                }
                ctor_start += 1;
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
                                    if let Some((_, tv)) =
                                        type_params.iter().find(|(n, _)| n == s)
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

                        let ctor_ty = Type::Fn(field_types, Box::new(result_ty.clone()));
                        let vars: Vec<TypeVar> =
                            type_params.iter().map(|(_, v)| *v).collect();
                        let scheme = Scheme {
                            vars,
                            ty: ctor_ty,
                        };
                        self.constructors
                            .insert(ctor_name.clone(), scheme.clone());
                        self.env.set_global(ctor_name.clone(), scheme);
                        ctor_names.push(ctor_name.clone());
                    }
                }
                ExprKind::Symbol(ctor_name)
                    if ctor_name.starts_with(char::is_uppercase) =>
                {
                    let vars: Vec<TypeVar> =
                        type_params.iter().map(|(_, v)| *v).collect();
                    let scheme = Scheme {
                        vars,
                        ty: result_ty.clone(),
                    };
                    self.constructors
                        .insert(ctor_name.clone(), scheme.clone());
                    self.env.set_global(ctor_name.clone(), scheme);
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
                                                    param_types.push(Type::Con("Self".to_string(), vec![]));
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
                                                        ret_type = Type::Con("Self".to_string(), vec![]);
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

        self.traits.insert(trait_name.clone(), TraitDecl {
            name: trait_name,
            type_params: vec![],
            methods,
        });
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
                self.errors.push(TypeError::at(
                    format!("unknown trait '{trait_name}'"),
                    span,
                ));
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
                                let trait_method = trait_decl.methods.iter().find(|m| m.name == *method_name);

                                if let ExprKind::List(ref params) = items[2].kind {
                                    self.env.push_scope();

                                    let mut param_types = Vec::new();
                                    for p in params {
                                        if let ExprKind::Symbol(ref s) = p.kind {
                                            if s == "self" {
                                                self.env.set("self".to_string(), Scheme::mono(impl_type.clone()));
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
                                        let expected_ret = if tm.ret_type == Type::Con("Self".to_string(), vec![]) {
                                            impl_type.clone()
                                        } else {
                                            tm.ret_type.clone()
                                        };
                                        if let Err(e) = unify(&mut self.subst, &body_ty, &expected_ret) {
                                            self.errors.push(e.with_span(span));
                                        }
                                    }

                                    self.env.pop_scope();

                                    let fn_ty = Type::Fn(param_types, Box::new(body_ty));
                                    let scheme = generalize(&self.env, &self.subst, &fn_ty);
                                    method_schemes.insert(method_name.clone(), scheme.clone());

                                    self.env.set_global(
                                        format!("{type_name}.{method_name}"),
                                        scheme,
                                    );
                                }
                            }
                        }
                    }
                }
            }
        }

        self.trait_impls.insert((trait_name, type_name), method_schemes);
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
                if s == ":" { &args[2..] } else { &args[1..] }
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
        Type::Fn(types, Box::new(ret))
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
                    let type_args: Vec<Type> = items[1..]
                        .iter()
                        .map(|a| self.parse_type_expr(a))
                        .collect();
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

    /// Post-inference pass: check that all trait constraints are satisfied.
    pub fn check_trait_constraints(&mut self) {
        let constraints: Vec<(TypeVar, Vec<TraitBound>)> =
            self.subst.constraints.iter().map(|(v, bs)| (*v, bs.clone())).collect();

        for (tv, bounds) in constraints {
            let resolved = self.subst.resolve(&Type::Var(tv));
            let type_name = match &resolved {
                Type::Int => Some("Int".to_string()),
                Type::Float => Some("Float".to_string()),
                Type::Bool => Some("Bool".to_string()),
                Type::Str => Some("String".to_string()),
                Type::Con(name, _) => Some(name.clone()),
                Type::Var(_) => None, // still polymorphic, OK
                _ => None,
            };

            if let Some(type_name) = type_name {
                for bound in &bounds {
                    let key = (bound.trait_name.clone(), type_name.clone());
                    if !self.trait_impls.contains_key(&key) {
                        self.errors.push(TypeError::bare(format!(
                            "no `{}` implementation for type `{}`",
                            bound.trait_name, type_name
                        )));
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
                t
            }
            ExprKind::List(items) => {
                let elem_types: Vec<Type> = items.iter().map(|p| self.infer_single_param(p)).collect();
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

    /// Check an entire program. Returns list of type errors.
    pub fn check_program(&mut self, exprs: &[Expr]) -> Vec<TypeError> {
        for expr in exprs {
            self.infer(expr);
        }
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

    fn infer_type(src: &str) -> (Type, Vec<TypeError>) {
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

    fn check_errors(src: &str) -> Vec<TypeError> {
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
            "[defn add [x y] [+ x y]]
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
        assert!(matches!(resolved, Type::Fn(params, ret) if params.len() == 1 && *ret == Type::Int));
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
        assert!(!errors.is_empty(), "should have type error for non-bool condition");
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
            [defn fib [n]
              [match n
                0 => 0
                1 => 1
                n => [+ [fib [- n 1]] [fib [- n 2]]]]]
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
        assert!(errors[0].span.is_some(), "type error should have span");
    }

    #[test]
    fn type_side_table_populated() {
        let exprs = parse("42").unwrap();
        let mut checker = Checker::new();
        for expr in &exprs {
            checker.infer(expr);
        }
        assert!(!checker.type_of.is_empty(), "type side-table should be populated");
    }

    #[test]
    fn trait_decl_type_checks() {
        let errors = check_errors(r#"
            [trait Display [fn display [self] -> String]]
        "#);
        assert!(errors.is_empty(), "errors: {:?}", errors);
    }

    #[test]
    fn sig_matching_passes() {
        let errors = check_errors(r#"
            [sig add Int -> Int -> Int]
            [defn add [x y] [+ x y]]
        "#);
        assert!(errors.is_empty(), "errors: {:?}", errors);
    }

    #[test]
    fn sig_mismatch_errors() {
        let errors = check_errors(r#"
            [sig add Int -> String -> Int]
            [defn add [x y] [+ x y]]
        "#);
        assert!(!errors.is_empty(), "should have sig mismatch error");
        assert!(errors[0].message.contains("does not match"), "error: {}", errors[0].message);
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
            "[defn double [x] [+ x x]]
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
            errors.iter().any(|e| e.message.contains("Add")),
            "error should mention Add: {:?}", errors
        );
    }

    #[test]
    fn eq_works_on_strings() {
        let (ty, errors) = infer_type(r#"[= "a" "b"]"#);
        assert!(errors.is_empty(), "errors: {:?}", errors);
        assert_eq!(ty, Type::Bool);
    }
}
