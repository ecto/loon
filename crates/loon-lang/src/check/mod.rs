pub mod ownership;

use crate::ast::{Expr, ExprKind};
use crate::types::*;

pub struct Checker {
    pub subst: Subst,
    pub env: TypeEnv,
    pub errors: Vec<TypeError>,
    /// ADT constructor types: name → scheme
    pub constructors: std::collections::HashMap<String, Scheme>,
}

impl Checker {
    pub fn new() -> Self {
        let mut checker = Self {
            subst: Subst::new(),
            env: TypeEnv::new(),
            errors: Vec::new(),
            constructors: std::collections::HashMap::new(),
        };
        checker.register_builtins();
        checker
    }

    fn register_builtins(&mut self) {
        // Arithmetic: Int → Int → Int
        let int_bin = Scheme::mono(Type::Fn(vec![Type::Int, Type::Int], Box::new(Type::Int)));
        for op in ["+", "-", "*"] {
            self.env.set_global(op.to_string(), int_bin.clone());
        }

        // Comparison: Int → Int → Bool
        let int_cmp = Scheme::mono(Type::Fn(vec![Type::Int, Type::Int], Box::new(Type::Bool)));
        for op in [">", "<", ">=", "<="] {
            self.env.set_global(op.to_string(), int_cmp.clone());
        }

        // Equality: ∀a. a → a → Bool
        {
            let a = self.subst.fresh();
            let tv = if let Type::Var(v) = a { v } else { unreachable!() };
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
    }

    /// Infer the type of an expression.
    pub fn infer(&mut self, expr: &Expr) -> Type {
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
                    self.errors.push(TypeError {
                        message: format!("unbound symbol '{name}'"),
                    });
                    self.subst.fresh()
                }
            }

            ExprKind::Vec(items) => {
                let elem = self.subst.fresh();
                for item in items {
                    let t = self.infer(item);
                    if let Err(e) = unify(&mut self.subst, &elem, &t) {
                        self.errors.push(e);
                    }
                }
                Type::Con("Vec".to_string(), vec![elem])
            }

            ExprKind::Set(items) => {
                let elem = self.subst.fresh();
                for item in items {
                    let t = self.infer(item);
                    if let Err(e) = unify(&mut self.subst, &elem, &t) {
                        self.errors.push(e);
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
                        self.errors.push(e);
                    }
                    if let Err(e) = unify(&mut self.subst, &val_t, &vt) {
                        self.errors.push(e);
                    }
                }
                Type::Con("Map".to_string(), vec![key_t, val_t])
            }

            ExprKind::Tuple(items) => {
                let types: Vec<Type> = items.iter().map(|e| self.infer(e)).collect();
                Type::Tuple(types)
            }

            ExprKind::List(items) if items.is_empty() => Type::Unit,

            ExprKind::List(items) => self.infer_list(items),
        }
    }

    fn infer_list(&mut self, items: &[Expr]) -> Type {
        let head = &items[0];
        if let ExprKind::Symbol(s) = &head.kind {
            match s.as_str() {
                "defn" => return self.infer_defn(&items[1..]),
                "let" => return self.infer_let(&items[1..]),
                "fn" => return self.infer_lambda(&items[1..]),
                "if" => return self.infer_if(&items[1..]),
                "do" => return self.infer_do(&items[1..]),
                "match" => return self.infer_match(&items[1..]),
                "|>" => return self.infer_pipe(&items[1..]),
                "type" => return self.infer_type_def(&items[1..]),
                "test" | "pub" | "mut" => {
                    // Pass through
                    if items.len() > 1 {
                        return self.infer_list(&items[1..]);
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
            self.errors.push(e);
        }
        ret
    }

    fn infer_defn(&mut self, args: &[Expr]) -> Type {
        if args.len() < 2 {
            return Type::Unit;
        }
        let name = match &args[0].kind {
            ExprKind::Symbol(s) => s.clone(),
            _ => return Type::Unit,
        };

        // Multi-arity check
        if matches!(args[1].kind, ExprKind::Tuple(_)) {
            // Multi-arity: infer each clause, unify return types
            let ret = self.subst.fresh();
            for clause_expr in &args[1..] {
                if let ExprKind::Tuple(clause_items) = &clause_expr.kind {
                    if clause_items.len() >= 2 {
                        let clause_ret = self.infer_fn_clause(&clause_items[0], &clause_items[1..]);
                        if let Err(e) = unify(&mut self.subst, &ret, &clause_ret) {
                            self.errors.push(e);
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
            let param_types: Vec<(String, Type)> = params
                .iter()
                .filter_map(|p| {
                    if let ExprKind::Symbol(s) = &p.kind {
                        Some((s.clone(), self.subst.fresh()))
                    } else {
                        None
                    }
                })
                .collect();

            // Skip effect annotation
            let mut body_start = 2;
            if body_start < args.len() {
                if let ExprKind::Symbol(s) = &args[body_start].kind {
                    if s == "/" {
                        body_start += 2;
                    }
                }
            }

            self.env.push_scope();
            for (pname, ptype) in &param_types {
                self.env.set(pname.clone(), Scheme::mono(ptype.clone()));
            }

            // For recursive calls, add a temporary type for the function
            let temp_ret = self.subst.fresh();
            let temp_fn_ty = Type::Fn(
                param_types.iter().map(|(_, t)| t.clone()).collect(),
                Box::new(temp_ret.clone()),
            );
            self.env.set(name.clone(), Scheme::mono(temp_fn_ty));

            let mut body_ty = Type::Unit;
            for body_expr in &args[body_start..] {
                body_ty = self.infer(body_expr);
            }

            if let Err(e) = unify(&mut self.subst, &temp_ret, &body_ty) {
                self.errors.push(e);
            }

            self.env.pop_scope();

            let fn_ty = Type::Fn(
                param_types.iter().map(|(_, t)| t.clone()).collect(),
                Box::new(body_ty),
            );
            let scheme = generalize(&self.env, &self.subst, &fn_ty);
            self.env.set_global(name, scheme);
        }
        Type::Unit
    }

    fn infer_fn_clause(&mut self, params_expr: &Expr, body: &[Expr]) -> Type {
        if let ExprKind::List(params) = &params_expr.kind {
            let param_types: Vec<(String, Type)> = params
                .iter()
                .filter_map(|p| {
                    if let ExprKind::Symbol(s) = &p.kind {
                        Some((s.clone(), self.subst.fresh()))
                    } else {
                        None
                    }
                })
                .collect();

            self.env.push_scope();
            for (pname, ptype) in &param_types {
                self.env.set(pname.clone(), Scheme::mono(ptype.clone()));
            }

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
        // Handle [let mut name val]
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
            let param_types: Vec<(String, Type)> = params
                .iter()
                .filter_map(|p| {
                    if let ExprKind::Symbol(s) = &p.kind {
                        Some((s.clone(), self.subst.fresh()))
                    } else {
                        None
                    }
                })
                .collect();

            self.env.push_scope();
            for (pname, ptype) in &param_types {
                self.env.set(pname.clone(), Scheme::mono(ptype.clone()));
            }

            let mut body_ty = Type::Unit;
            for expr in &args[1..] {
                body_ty = self.infer(expr);
            }
            self.env.pop_scope();

            Type::Fn(
                param_types.into_iter().map(|(_, t)| t).collect(),
                Box::new(body_ty),
            )
        } else {
            self.subst.fresh()
        }
    }

    fn infer_if(&mut self, args: &[Expr]) -> Type {
        if args.len() < 2 {
            return Type::Unit;
        }
        let cond_ty = self.infer(&args[0]);
        if let Err(e) = unify(&mut self.subst, &cond_ty, &Type::Bool) {
            self.errors.push(e);
        }
        let then_ty = self.infer(&args[1]);
        if args.len() > 2 {
            let else_ty = self.infer(&args[2]);
            if let Err(e) = unify(&mut self.subst, &then_ty, &else_ty) {
                self.errors.push(e);
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

    fn infer_match(&mut self, args: &[Expr]) -> Type {
        if args.is_empty() {
            return Type::Unit;
        }
        let _scrutinee_ty = self.infer(&args[0]);
        let result_ty = self.subst.fresh();

        // Walk arms: pattern => body (skip => tokens)
        let arms = &args[1..];
        let mut i = 0;
        while i < arms.len() {
            // Skip patterns and =>, just type the body exprs
            if let ExprKind::Symbol(s) = &arms[i].kind {
                if s == "=>" {
                    if i + 1 < arms.len() {
                        let body_ty = self.infer(&arms[i + 1]);
                        if let Err(e) = unify(&mut self.subst, &result_ty, &body_ty) {
                            self.errors.push(e);
                        }
                        i += 2;
                        continue;
                    }
                }
            }
            i += 1;
        }
        result_ty
    }

    fn infer_pipe(&mut self, args: &[Expr]) -> Type {
        if args.is_empty() {
            return Type::Unit;
        }
        let mut current = self.infer(&args[0]);
        for step in &args[1..] {
            match &step.kind {
                ExprKind::List(items) if !items.is_empty() => {
                    let func_ty = self.infer(&items[0]);
                    let mut arg_tys = vec![current];
                    for a in &items[1..] {
                        arg_tys.push(self.infer(a));
                    }
                    let ret = self.subst.fresh();
                    let expected = Type::Fn(arg_tys, Box::new(ret.clone()));
                    if let Err(e) = unify(&mut self.subst, &func_ty, &expected) {
                        self.errors.push(e);
                    }
                    current = ret;
                }
                ExprKind::Symbol(_) => {
                    let func_ty = self.infer(step);
                    let ret = self.subst.fresh();
                    let expected = Type::Fn(vec![current], Box::new(ret.clone()));
                    if let Err(e) = unify(&mut self.subst, &func_ty, &expected) {
                        self.errors.push(e);
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

        // Collect type params
        let mut type_params = Vec::new();
        let mut ctor_start = 1;
        for arg in &args[1..] {
            if let ExprKind::Symbol(s) = &arg.kind {
                if s.chars().next().map_or(false, |c| c.is_uppercase()) {
                    break;
                }
                // Lowercase symbol = type parameter
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

        // Register constructors
        for arg in &args[ctor_start..] {
            match &arg.kind {
                ExprKind::List(items) if !items.is_empty() => {
                    if let ExprKind::Symbol(ctor_name) = &items[0].kind {
                        // Constructor with fields
                        let field_types: Vec<Type> = items[1..]
                            .iter()
                            .map(|f| {
                                if let ExprKind::Symbol(s) = &f.kind {
                                    // Check if it's a type param
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
                    }
                }
                ExprKind::Symbol(ctor_name)
                    if ctor_name.starts_with(char::is_uppercase) =>
                {
                    // Nullary constructor
                    let vars: Vec<TypeVar> =
                        type_params.iter().map(|(_, v)| *v).collect();
                    let scheme = Scheme {
                        vars,
                        ty: result_ty.clone(),
                    };
                    self.constructors
                        .insert(ctor_name.clone(), scheme.clone());
                    self.env.set_global(ctor_name.clone(), scheme);
                }
                _ => {}
            }
        }

        Type::Unit
    }

    fn name_to_type(&self, name: &str) -> Type {
        match name {
            "i64" | "Int" => Type::Int,
            "f64" | "Float" => Type::Float,
            "Bool" => Type::Bool,
            "String" => Type::Str,
            "Keyword" => Type::Keyword,
            _ => Type::Con(name.to_string(), vec![]),
        }
    }

    /// Check an entire program. Returns list of type errors.
    pub fn check_program(&mut self, exprs: &[Expr]) -> Vec<TypeError> {
        for expr in exprs {
            self.infer(expr);
        }
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
}
