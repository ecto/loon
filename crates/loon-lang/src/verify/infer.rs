//! A small Hindley-Milner inference for the reference prelude and laws.
//!
//! Untrusted: it only decides which ADT a law parameter ranges over (for the
//! kernel's `induction`/`cases` and for the bounded generator). The language
//! checker is not used for this because it does not generalize top-level fns
//! and shares one instantiation of a constructor across a function, so a list
//! of instructions in `compile` fixes the element type of every list law.
//!
//! Supported forms: integer/bool literals, variables, constructor
//! applications, calls to reference fns (let-polymorphic, in definition
//! order), calls of function-typed variables, `match`, `+ - *`, and `fn`
//! lambdas. Anything else makes inference fail (`None`), and callers fall
//! back to the checker.

use super::kernel::{head_sym, Ty};
use super::Defs;
use crate::ast::{Expr, ExprKind};
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub enum T {
    Var(usize),
    Con(String, Vec<T>),
}

const ARROW: &str = "->";

struct Inf<'a> {
    defs: &'a Defs,
    subst: Vec<Option<T>>,
    /// Generalized fn schemes: (number of quantified vars, type over Var(0..n)).
    schemes: HashMap<String, (usize, T)>,
}

impl Inf<'_> {
    fn fresh(&mut self) -> T {
        self.subst.push(None);
        T::Var(self.subst.len() - 1)
    }

    fn resolve(&self, t: &T) -> T {
        match t {
            T::Var(v) => match &self.subst[*v] {
                Some(t) => self.resolve(t),
                None => t.clone(),
            },
            T::Con(n, a) => T::Con(n.clone(), a.iter().map(|x| self.resolve(x)).collect()),
        }
    }

    fn occurs(&self, v: usize, t: &T) -> bool {
        match self.resolve(t) {
            T::Var(w) => v == w,
            T::Con(_, a) => a.iter().any(|x| self.occurs(v, x)),
        }
    }

    fn unify(&mut self, a: &T, b: &T) -> Option<()> {
        match (self.resolve(a), self.resolve(b)) {
            (T::Var(x), T::Var(y)) if x == y => Some(()),
            (T::Var(x), t) | (t, T::Var(x)) => {
                if self.occurs(x, &t) {
                    return None;
                }
                self.subst[x] = Some(t);
                Some(())
            }
            (T::Con(n, xs), T::Con(m, ys)) if n == m && xs.len() == ys.len() => {
                for (x, y) in xs.iter().zip(&ys) {
                    self.unify(x, y)?;
                }
                Some(())
            }
            _ => None,
        }
    }

    fn instantiate(&mut self, scheme: &(usize, T)) -> T {
        let vars: Vec<T> = (0..scheme.0).map(|_| self.fresh()).collect();
        fn go(t: &T, vars: &[T]) -> T {
            match t {
                T::Var(i) => vars[*i].clone(),
                T::Con(n, a) => T::Con(n.clone(), a.iter().map(|x| go(x, vars)).collect()),
            }
        }
        go(&scheme.1, &vars)
    }

    /// Constructor `c`: (field types, result type), freshly instantiated.
    fn ctor(&mut self, c: &str) -> Option<(Vec<T>, T)> {
        let ty = self.defs.type_of_ctor(c)?;
        let params = self.defs.type_params.get(&ty).cloned().unwrap_or_default();
        let inst: Vec<T> = params.iter().map(|_| self.fresh()).collect();
        let info = self.defs.types[&ty].iter().find(|k| k.name == c)?.clone();
        let fields = info
            .fields
            .iter()
            .map(|f| self.field(f, &params, &inst))
            .collect::<Option<Vec<_>>>()?;
        Some((fields, T::Con(ty, inst)))
    }

    fn field(&mut self, f: &Expr, params: &[String], inst: &[T]) -> Option<T> {
        match &f.kind {
            ExprKind::Symbol(s) => Some(match params.iter().position(|p| p == s) {
                Some(i) => inst[i].clone(),
                None => T::Con(s.clone(), vec![]),
            }),
            ExprKind::List(v) | ExprKind::Tuple(v) => {
                let Some(ExprKind::Symbol(h)) = v.first().map(|e| &e.kind) else {
                    return None;
                };
                let args = v[1..]
                    .iter()
                    .map(|a| self.field(a, params, inst))
                    .collect::<Option<Vec<_>>>()?;
                Some(T::Con(h.clone(), args))
            }
            _ => None,
        }
    }

    fn pattern(&mut self, p: &Expr, t: &T, env: &mut HashMap<String, T>) -> Option<()> {
        match &p.kind {
            ExprKind::Symbol(s) if s == "_" => Some(()),
            ExprKind::Symbol(s) if self.defs.is_ctor(s) => {
                let (fs, r) = self.ctor(s)?;
                if !fs.is_empty() {
                    return None;
                }
                self.unify(&r, t)
            }
            ExprKind::Symbol(s) => {
                env.insert(s.clone(), t.clone());
                Some(())
            }
            ExprKind::Int(_) => self.unify(t, &T::Con("Int".into(), vec![])),
            ExprKind::List(v) => {
                let c = head_sym(p)?;
                let (fs, r) = self.ctor(c)?;
                if fs.len() + 1 != v.len() {
                    return None;
                }
                self.unify(&r, t)?;
                for (sp, ft) in v[1..].iter().zip(&fs) {
                    self.pattern(sp, ft, env)?;
                }
                Some(())
            }
            _ => None,
        }
    }

    fn expr(
        &mut self,
        e: &Expr,
        env: &HashMap<String, T>,
        current: &HashMap<String, T>,
    ) -> Option<T> {
        let int = T::Con("Int".into(), vec![]);
        match &e.kind {
            ExprKind::Int(_) => Some(int),
            ExprKind::Bool(_) => Some(T::Con("Bool".into(), vec![])),
            ExprKind::Symbol(s) => {
                if let Some(t) = env.get(s) {
                    return Some(t.clone());
                }
                if let Some(t) = current.get(s) {
                    return Some(t.clone());
                }
                if self.defs.is_ctor(s) {
                    let (fs, r) = self.ctor(s)?;
                    return fs.is_empty().then_some(r);
                }
                let sc = self.schemes.get(s)?.clone();
                Some(self.instantiate(&sc))
            }
            ExprKind::List(items) => {
                let head = head_sym(e);
                match head {
                    Some("match") => {
                        let st = self.expr(items.get(1)?, env, current)?;
                        let out = self.fresh();
                        for pair in items[2..].chunks(2) {
                            let [pat, body] = pair else { return None };
                            let mut inner = env.clone();
                            self.pattern(pat, &st, &mut inner)?;
                            let bt = self.expr(body, &inner, current)?;
                            self.unify(&out, &bt)?;
                        }
                        Some(out)
                    }
                    Some("fn") => {
                        let ExprKind::List(ps) = &items.get(1)?.kind else {
                            return None;
                        };
                        if items.len() != 3 {
                            return None;
                        }
                        let mut inner = env.clone();
                        let mut pts = Vec::new();
                        for p in ps {
                            let ExprKind::Symbol(s) = &p.kind else {
                                return None;
                            };
                            let t = self.fresh();
                            inner.insert(s.clone(), t.clone());
                            pts.push(t);
                        }
                        let r = self.expr(&items[2], &inner, current)?;
                        pts.push(r);
                        Some(T::Con(ARROW.into(), pts))
                    }
                    Some("+") | Some("-") | Some("*") if items.len() == 3 => {
                        for a in &items[1..] {
                            let t = self.expr(a, env, current)?;
                            self.unify(&t, &int)?;
                        }
                        Some(int)
                    }
                    Some(c) if self.defs.is_ctor(c) => {
                        let (fs, r) = self.ctor(c)?;
                        if fs.len() + 1 != items.len() {
                            return None;
                        }
                        for (a, ft) in items[1..].iter().zip(&fs) {
                            let t = self.expr(a, env, current)?;
                            self.unify(&t, ft)?;
                        }
                        Some(r)
                    }
                    _ => {
                        let ft = self.expr(items.first()?, env, current)?;
                        let mut args = Vec::new();
                        for a in &items[1..] {
                            args.push(self.expr(a, env, current)?);
                        }
                        let r = self.fresh();
                        args.push(r.clone());
                        self.unify(&ft, &T::Con(ARROW.into(), args))?;
                        Some(r)
                    }
                }
            }
            _ => None,
        }
    }

    /// Generalize every unresolved variable (top level: the env is empty).
    fn generalize(&self, t: &T) -> (usize, T) {
        let mut map: HashMap<usize, usize> = HashMap::new();
        fn go(t: &T, map: &mut HashMap<usize, usize>) -> T {
            match t {
                T::Var(v) => {
                    let n = map.len();
                    T::Var(*map.entry(*v).or_insert(n))
                }
                T::Con(n, a) => T::Con(n.clone(), a.iter().map(|x| go(x, map)).collect()),
            }
        }
        let g = go(&self.resolve(t), &mut map);
        (map.len(), g)
    }
}

/// Infer the types of `params` in the law `lhs = rhs`. `fn_order` lists the
/// reference fns in definition order. `None` if anything fails to type.
pub fn law_param_types(
    defs: &Defs,
    fn_order: &[String],
    params: &[String],
    lhs: &Expr,
    rhs: &Expr,
) -> Option<Vec<T>> {
    law_param_types_eqs(defs, fn_order, params, &[(lhs, rhs)])
}

/// `law_param_types` for a law with hypotheses: every equation (conclusion
/// and `:given` hypotheses) is typed in one environment, both sides unified.
pub fn law_param_types_eqs(
    defs: &Defs,
    fn_order: &[String],
    params: &[String],
    eqs: &[(&Expr, &Expr)],
) -> Option<Vec<T>> {
    let mut inf = with_schemes(defs, fn_order);
    let mut env = HashMap::new();
    let pts: Vec<T> = params.iter().map(|_| inf.fresh()).collect();
    for (p, t) in params.iter().zip(&pts) {
        env.insert(p.clone(), t.clone());
    }
    for (lhs, rhs) in eqs {
        let l = inf.expr(lhs, &env, &HashMap::new())?;
        let r = inf.expr(rhs, &env, &HashMap::new())?;
        inf.unify(&l, &r)?;
    }

    Some(pts.iter().map(|t| inf.resolve(t)).collect())
}

/// An inference state with every typable fn of defs.oo generalized.
fn with_schemes<'a>(defs: &'a Defs, fn_order: &[String]) -> Inf<'a> {
    let mut inf = Inf {
        defs,
        subst: Vec::new(),
        schemes: HashMap::new(),
    };
    // Mutually recursive groups (callees first) are typed together: every
    // member is monomorphic inside the group and generalized after it. A
    // group with a member that fails to type gets no schemes.
    for group in super::kernel::fn_sccs(defs, fn_order) {
        let mut current: HashMap<String, T> = HashMap::new();
        let mut sigs = Vec::new();
        for name in &group {
            let f = &defs.fns[name];
            let mut env = HashMap::new();
            let mut pts = Vec::new();
            for p in &f.params {
                let t = inf.fresh();
                env.insert(p.clone(), t.clone());
                pts.push(t);
            }
            let r = inf.fresh();
            pts.push(r.clone());
            let ft = T::Con(ARROW.into(), pts);
            current.insert(name.clone(), ft.clone());
            sigs.push((name, env, r, ft));
        }
        let ok = sigs.iter().all(|(name, env, r, _)| {
            inf.expr(&defs.fns[*name].body, env, &current)
                .and_then(|bt| inf.unify(&bt, r))
                .is_some()
        });
        if !ok {
            continue;
        }
        for (name, _, _, ft) in &sigs {
            let sc = inf.generalize(ft);
            inf.schemes.insert((*name).clone(), sc);
        }
    }
    inf
}

fn from_ty(inf: &mut Inf, t: &Ty) -> T {
    if t.name == "?" {
        return inf.fresh();
    }
    T::Con(
        t.name.clone(),
        t.args.iter().map(|a| from_ty(inf, a)).collect(),
    )
}

/// The type of an arbitrary term `e` whose variables have the (kernel)
/// types in `env`; other free symbols get fresh type variables. Used by the
/// kernel's `cases EXPR :as NAME` to find the ADT to split over. `None` if
/// it does not type.
pub fn expr_type(defs: &Defs, env: &HashMap<String, Ty>, e: &Expr) -> Option<Ty> {
    let mut inf = with_schemes(defs, &defs.fn_order);
    let mut syms = std::collections::HashSet::new();
    super::kernel::symbols(e, &mut syms);
    let mut tenv = HashMap::new();
    for s in syms {
        if defs.is_ctor(&s) || defs.fns.contains_key(&s) {
            continue;
        }
        let t = match env.get(&s) {
            Some(t) => from_ty(&mut inf, t),
            None => inf.fresh(),
        };
        tenv.insert(s, t);
    }
    let t = inf.expr(e, &tenv, &HashMap::new())?;
    Some(to_ty(&inf.resolve(&t)))
}

/// The kernel's view: unresolved variables become `?`.
pub fn to_ty(t: &T) -> Ty {
    match t {
        T::Var(_) => Ty::unknown(),
        T::Con(n, a) => Ty::app(n, a.iter().map(to_ty).collect()),
    }
}

/// The checker's view, for the bounded generator: unresolved variables are
/// instantiated at `Int`, as the generator always did.
pub fn to_checker(t: &T) -> crate::types::Type {
    use crate::types::{EffectRow, Type};
    match t {
        T::Var(_) => Type::Int,
        T::Con(n, a) if n == "Int" && a.is_empty() => Type::Int,
        T::Con(n, a) if n == "Bool" && a.is_empty() => Type::Bool,
        T::Con(n, a) if n == ARROW && !a.is_empty() => {
            let ps = a[..a.len() - 1].iter().map(to_checker).collect();
            Type::Fn(ps, Box::new(to_checker(&a[a.len() - 1])), EffectRow::pure())
        }
        T::Con(n, a) => Type::Con(n.clone(), a.iter().map(to_checker).collect()),
    }
}
