//! The trusted rewrite kernel.
//!
//! Everything here operates on untyped `Expr` terms. The trusted surface is:
//! substitution, one-step unfolding of a structurally recursive definition,
//! first-order syntactic matching, constructor-`match` reduction (plus
//! constant folding of literal integer arithmetic), and the structural
//! induction scheme for the ADTs declared in `defs.oo`.
//!
//! A case closes when both sides are syntactically equal after *evaluation
//! normalization*: constructor-match reduction plus unfolding of total
//! definitions whose scrutinized argument is a constructor application. Since
//! every unfolded definition is structurally recursive, each unfold consumes a
//! constructor and normalization terminates (a fuel bound backs this up).
//!
//! Both normalizations also canonicalize associative-commutative operators:
//! the builtin `+` always, and each user fn listed in `Defs::ac` (an `ac`
//! declaration whose cited assoc/comm/unit laws were checked for shape and
//! are proven earlier in dependency order; see `verify/mod.rs`). Operands of
//! a nested application are flattened, unit operands dropped, sorted by their
//! printed form, and rebuilt right-nested. `rewrite` then matches against
//! that canonical form, so operand order in a goal is printed-form order.

use super::{Defs, FnDef};
use crate::ast::{Expr, ExprKind};
use crate::syntax::Span;
use std::collections::{HashMap, HashSet};

// ─── Term helpers ──────────────────────────────────────────────────────────

pub fn sym(s: &str) -> Expr {
    Expr::new(ExprKind::Symbol(s.to_string()), Span::ZERO)
}

pub fn list(items: Vec<Expr>) -> Expr {
    Expr::new(ExprKind::List(items), Span::ZERO)
}

pub fn int(n: i64) -> Expr {
    Expr::new(ExprKind::Int(n), Span::ZERO)
}

fn with_kind(e: &Expr, kind: ExprKind) -> Expr {
    Expr::new(kind, e.span)
}

/// Structural equality, ignoring spans and node ids.
pub fn term_eq(a: &Expr, b: &Expr) -> bool {
    use ExprKind::*;
    match (&a.kind, &b.kind) {
        (Int(x), Int(y)) => x == y,
        (Float(x), Float(y)) => x.to_bits() == y.to_bits(),
        (Bool(x), Bool(y)) => x == y,
        (Str(x), Str(y)) | (Keyword(x), Keyword(y)) | (Symbol(x), Symbol(y)) => x == y,
        (List(x), List(y)) | (Vec(x), Vec(y)) | (Set(x), Set(y)) | (Tuple(x), Tuple(y)) => {
            x.len() == y.len() && x.iter().zip(y).all(|(p, q)| term_eq(p, q))
        }
        (Map(x), Map(y)) => {
            x.len() == y.len()
                && x
                    .iter()
                    .zip(y)
                    .all(|((k1, v1), (k2, v2))| term_eq(k1, k2) && term_eq(v1, v2))
        }
        (Quote(x), Quote(y)) | (Unquote(x), Unquote(y)) | (UnquoteSplice(x), UnquoteSplice(y)) => {
            term_eq(x, y)
        }
        (DotAccess(x, f), DotAccess(y, g)) => f == g && term_eq(x, y),
        _ => false,
    }
}

/// Immediate children, mutable, in preorder order.
fn children(e: &Expr) -> Vec<&Expr> {
    use ExprKind::*;
    match &e.kind {
        List(v) | Vec(v) | Set(v) | Tuple(v) => v.iter().collect(),
        Map(ps) => ps.iter().flat_map(|(k, v)| [k, v]).collect(),
        Quote(x) | Unquote(x) | UnquoteSplice(x) | DotAccess(x, _) => vec![x],
        _ => vec![],
    }
}

/// Rebuild `e` with its children mapped through `f`.
fn map_children(e: &Expr, f: &mut dyn FnMut(&Expr) -> Expr) -> Expr {
    use ExprKind::*;
    let kind = match &e.kind {
        List(v) => List(v.iter().map(&mut *f).collect()),
        Vec(v) => Vec(v.iter().map(&mut *f).collect()),
        Set(v) => Set(v.iter().map(&mut *f).collect()),
        Tuple(v) => Tuple(v.iter().map(&mut *f).collect()),
        Map(ps) => Map(ps.iter().map(|(k, v)| (f(k), f(v))).collect()),
        Quote(x) => Quote(Box::new(f(x))),
        Unquote(x) => Unquote(Box::new(f(x))),
        UnquoteSplice(x) => UnquoteSplice(Box::new(f(x))),
        DotAccess(x, s) => DotAccess(Box::new(f(x)), s.clone()),
        k => k.clone(),
    };
    with_kind(e, kind)
}

pub fn head_sym(e: &Expr) -> Option<&str> {
    match &e.kind {
        ExprKind::List(v) => match v.first().map(|h| &h.kind) {
            Some(ExprKind::Symbol(s)) => Some(s),
            _ => None,
        },
        _ => None,
    }
}

/// Every symbol occurring anywhere in `e`.
pub fn symbols(e: &Expr, out: &mut HashSet<String>) {
    if let ExprKind::Symbol(s) = &e.kind {
        out.insert(s.clone());
    }
    for c in children(e) {
        symbols(c, out);
    }
}

/// Variables bound by a match pattern (symbols that are not constructors,
/// `_`, or literals).
pub fn pattern_vars(p: &Expr, defs: &Defs, out: &mut Vec<String>) {
    match &p.kind {
        ExprKind::Symbol(s) if s != "_" && !defs.is_ctor(s) => out.push(s.clone()),
        ExprKind::List(v) => {
            for (i, c) in v.iter().enumerate() {
                if i == 0 && matches!(c.kind, ExprKind::Symbol(_)) {
                    continue; // constructor head
                }
                pattern_vars(c, defs, out);
            }
        }
        ExprKind::Vec(v) | ExprKind::Tuple(v) => {
            for c in v {
                pattern_vars(c, defs, out);
            }
        }
        _ => {}
    }
}

/// Binders introduced by `fn` params: `[fn [a b] body]` or `[fn name [a b] body]`.
fn fn_binders(items: &[Expr]) -> Option<(usize, Vec<String>)> {
    let idx = match items.get(1).map(|e| &e.kind) {
        Some(ExprKind::List(_)) => 1,
        Some(ExprKind::Symbol(_)) if matches!(items.get(2).map(|e| &e.kind), Some(ExprKind::List(_))) => 2,
        _ => return None,
    };
    let ExprKind::List(ps) = &items[idx].kind else {
        return None;
    };
    Some((
        idx,
        ps.iter()
            .filter_map(|p| match &p.kind {
                ExprKind::Symbol(s) => Some(s.clone()),
                _ => None,
            })
            .collect(),
    ))
}

/// Capture-respecting substitution of free symbols. Binders (`match` arm
/// patterns, `fn` params) shadow the substitution inside their scope.
pub fn subst(e: &Expr, map: &HashMap<String, Expr>, defs: &Defs) -> Expr {
    if map.is_empty() {
        return e.clone();
    }
    match &e.kind {
        ExprKind::Symbol(s) => map.get(s).cloned().unwrap_or_else(|| e.clone()),
        ExprKind::List(items) if head_sym(e) == Some("match") && items.len() >= 2 => {
            let mut out = vec![items[0].clone(), subst(&items[1], map, defs)];
            for pair in items[2..].chunks(2) {
                out.push(pair[0].clone());
                if let Some(body) = pair.get(1) {
                    let mut bound = Vec::new();
                    pattern_vars(&pair[0], defs, &mut bound);
                    let inner: HashMap<String, Expr> = map
                        .iter()
                        .filter(|(k, _)| !bound.contains(k))
                        .map(|(k, v)| (k.clone(), v.clone()))
                        .collect();
                    out.push(subst(body, &inner, defs));
                }
            }
            with_kind(e, ExprKind::List(out))
        }
        ExprKind::List(items) if head_sym(e) == Some("fn") => {
            if let Some((idx, bound)) = fn_binders(items) {
                let inner: HashMap<String, Expr> = map
                    .iter()
                    .filter(|(k, _)| !bound.contains(k))
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect();
                let mut out: Vec<Expr> = items[..=idx].to_vec();
                out.extend(items[idx + 1..].iter().map(|b| subst(b, &inner, defs)));
                with_kind(e, ExprKind::List(out))
            } else {
                map_children(e, &mut |c| subst(c, map, defs))
            }
        }
        _ => map_children(e, &mut |c| subst(c, map, defs)),
    }
}

/// Rename every pattern-bound variable in `body` to a name not in `used`.
fn freshen_binders(body: &Expr, defs: &Defs, used: &mut HashSet<String>) -> Expr {
    match &body.kind {
        ExprKind::List(items) if head_sym(body) == Some("match") && items.len() >= 2 => {
            let mut out = vec![items[0].clone(), freshen_binders(&items[1], defs, used)];
            for pair in items[2..].chunks(2) {
                let mut bound = Vec::new();
                pattern_vars(&pair[0], defs, &mut bound);
                let mut ren = HashMap::new();
                for v in bound {
                    let nv = fresh_name(&v, used);
                    ren.insert(v, sym(&nv));
                }
                out.push(rename_pattern(&pair[0], &ren));
                if let Some(b) = pair.get(1) {
                    let b = subst(b, &ren, defs);
                    out.push(freshen_binders(&b, defs, used));
                }
            }
            with_kind(body, ExprKind::List(out))
        }
        _ => map_children(body, &mut |c| freshen_binders(c, defs, used)),
    }
}

fn rename_pattern(p: &Expr, ren: &HashMap<String, Expr>) -> Expr {
    match &p.kind {
        ExprKind::Symbol(s) => ren.get(s).cloned().unwrap_or_else(|| p.clone()),
        _ => map_children(p, &mut |c| rename_pattern(c, ren)),
    }
}

pub fn fresh_name(base: &str, used: &mut HashSet<String>) -> String {
    let base = base.trim_end_matches(|c: char| c.is_ascii_digit());
    let base = if base.is_empty() { "v" } else { base };
    let mut cand = base.to_string();
    let mut i = 1;
    while used.contains(&cand) {
        cand = format!("{base}{i}");
        i += 1;
    }
    used.insert(cand.clone());
    cand
}

// ─── Normalization ─────────────────────────────────────────────────────────

/// Is `e` a constructor value head: a nullary ctor symbol or `[Ctor ...]`?
fn ctor_of<'a>(e: &'a Expr, defs: &Defs) -> Option<(&'a str, &'a [Expr])> {
    match &e.kind {
        ExprKind::Symbol(s) if defs.is_ctor(s) => Some((s, &[])),
        ExprKind::List(v) => match v.first().map(|h| &h.kind) {
            Some(ExprKind::Symbol(s)) if defs.is_ctor(s) => Some((s, &v[1..])),
            _ => None,
        },
        _ => None,
    }
}

enum PatResult {
    Match(HashMap<String, Expr>),
    NoMatch,
    Undecided,
}

fn match_pattern(p: &Expr, v: &Expr, defs: &Defs) -> PatResult {
    match &p.kind {
        ExprKind::Symbol(s) if s == "_" => PatResult::Match(HashMap::new()),
        ExprKind::Symbol(s) if defs.is_ctor(s) => match ctor_of(v, defs) {
            Some((c, args)) if c == s && args.is_empty() => PatResult::Match(HashMap::new()),
            Some(_) => PatResult::NoMatch,
            None => PatResult::Undecided,
        },
        ExprKind::Symbol(s) => PatResult::Match(HashMap::from([(s.clone(), v.clone())])),
        ExprKind::Int(n) => match v.kind {
            ExprKind::Int(m) if m == *n => PatResult::Match(HashMap::new()),
            ExprKind::Int(_) => PatResult::NoMatch,
            _ => PatResult::Undecided,
        },
        ExprKind::List(ps) if head_sym(p).is_some_and(|h| defs.is_ctor(h)) => {
            let pc = head_sym(p).unwrap();
            match ctor_of(v, defs) {
                Some((c, args)) if c == pc && args.len() == ps.len() - 1 => {
                    let mut binds = HashMap::new();
                    for (sp, sv) in ps[1..].iter().zip(args) {
                        match match_pattern(sp, sv, defs) {
                            PatResult::Match(b) => binds.extend(b),
                            other => return other,
                        }
                    }
                    PatResult::Match(binds)
                }
                Some(_) => PatResult::NoMatch,
                None => PatResult::Undecided,
            }
        }
        _ => PatResult::Undecided,
    }
}

/// Reduce a `match` node whose scrutinee is decided by a constructor.
fn reduce_match(e: &Expr, defs: &Defs) -> Option<Expr> {
    let ExprKind::List(items) = &e.kind else {
        return None;
    };
    if head_sym(e) != Some("match") || items.len() < 4 {
        return None;
    }
    let scrut = &items[1];
    for pair in items[2..].chunks(2) {
        let [pat, body] = pair else { return None };
        match match_pattern(pat, scrut, defs) {
            PatResult::Match(b) => return Some(subst(body, &b, defs)),
            PatResult::NoMatch => continue,
            PatResult::Undecided => return None,
        }
    }
    None
}

fn fold_arith(e: &Expr) -> Option<Expr> {
    let ExprKind::List(items) = &e.kind else {
        return None;
    };
    if items.len() != 3 {
        return None;
    }
    let (ExprKind::Int(a), ExprKind::Int(b)) = (&items[1].kind, &items[2].kind) else {
        return None;
    };
    let r = match head_sym(e)? {
        "+" => a.checked_add(*b)?,
        "-" => a.checked_sub(*b)?,
        "*" => a.checked_mul(*b)?,
        _ => return None,
    };
    Some(int(r))
}

/// Flatten a `+` tree (children already normalized) into its operands.
fn plus_operands(e: &Expr, out: &mut Vec<Expr>) {
    if let ExprKind::List(items) = &e.kind {
        if head_sym(e) == Some("+") && items.len() == 3 {
            plus_operands(&items[1], out);
            plus_operands(&items[2], out);
            return;
        }
    }
    out.push(e.clone());
}

/// Canonical form for `+`: associativity, commutativity and the zero law,
/// all at once. Literals are summed into one constant placed first; the
/// remaining operands are sorted by their printed form; the result is
/// right-nested. `[+ 1 [+ a b]]`, `[+ [+ 1 a] b]` and `[+ b [+ a 1]]` all
/// become `[+ 1 [+ a b]]`; `[+ 0 n]` becomes `n`.
fn normalize_plus(e: &Expr) -> Option<Expr> {
    let ExprKind::List(items) = &e.kind else {
        return None;
    };
    if head_sym(e) != Some("+") || items.len() != 3 {
        return None;
    }
    let mut ops = Vec::new();
    plus_operands(e, &mut ops);
    let mut constant: i64 = 0;
    let mut rest: Vec<Expr> = Vec::new();
    for op in ops {
        match op.kind {
            ExprKind::Int(n) => constant = constant.checked_add(n)?,
            _ => rest.push(op),
        }
    }
    rest.sort_by_key(|x| x.to_string());
    let mut terms: Vec<Expr> = Vec::new();
    if constant != 0 || rest.is_empty() {
        terms.push(int(constant));
    }
    terms.extend(rest);
    let mut acc = terms.pop()?;
    while let Some(t) = terms.pop() {
        acc = Expr::new(
            ExprKind::List(vec![Expr::new(ExprKind::Symbol("+".into()), e.span), t, acc]),
            e.span,
        );
    }
    Some(acc)
}

/// Flatten a user AC-operator tree into its operands.
fn ac_operands(e: &Expr, op: &str, out: &mut Vec<Expr>) {
    if let ExprKind::List(items) = &e.kind {
        if head_sym(e) == Some(op) && items.len() == 3 {
            ac_operands(&items[1], op, out);
            ac_operands(&items[2], op, out);
            return;
        }
    }
    out.push(e.clone());
}

/// Canonical form for an AC operator application (children already
/// normalized): `+` via `normalize_plus`; an active user operator OP by
/// flattening, dropping its declared unit, sorting operands by printed form,
/// and rebuilding right-nested. One operand is returned bare; none gives the
/// unit. `None` if `e` is not an application of an active AC operator.
fn normalize_ac(e: &Expr, defs: &Defs) -> Option<Expr> {
    let h = head_sym(e)?;
    if h == "+" {
        return normalize_plus(e);
    }
    let ac = defs.ac.iter().find(|a| a.op == h)?;
    let ExprKind::List(items) = &e.kind else { return None };
    if items.len() != 3 {
        return None;
    }
    let mut ops = Vec::new();
    ac_operands(e, &ac.op, &mut ops);
    if let Some(u) = &ac.unit {
        ops.retain(|x| !term_eq(x, u));
    }
    ops.sort_by_key(|x| x.to_string());
    let Some(mut acc) = ops.pop() else {
        return ac.unit.clone();
    };
    while let Some(t) = ops.pop() {
        acc = Expr::new(ExprKind::List(vec![items[0].clone(), t, acc]), e.span);
    }
    Some(acc)
}

/// Constructor-match normalization plus literal constant folding and AC
/// canonicalization, bottom-up.
pub fn normalize(e: &Expr, defs: &Defs) -> Expr {
    let e = map_children(e, &mut |c| normalize(c, defs));
    if let Some(r) = reduce_match(&e, defs) {
        return normalize(&r, defs);
    }
    if let Some(r) = normalize_ac(&e, defs) {
        return r;
    }
    if let Some(r) = fold_arith(&e) {
        return r;
    }
    e
}

/// Instantiate `f`'s body at `args`, freshening its binders against `used`.
fn instantiate(f: &FnDef, args: &[Expr], defs: &Defs, used: &mut HashSet<String>) -> Expr {
    for a in args {
        symbols(a, used);
    }
    let body = freshen_binders(&f.body, defs, used);
    let map: HashMap<String, Expr> = f
        .params
        .iter()
        .cloned()
        .zip(args.iter().cloned())
        .collect();
    subst(&body, &map, defs)
}

/// Is `e` an application of total fn `name` whose scrutinized argument is a
/// constructor (so unfolding it fires a match)?
fn is_redex(e: &Expr, name: &str, defs: &Defs) -> bool {
    let Some(f) = defs.fns.get(name) else {
        return false;
    };
    let ExprKind::List(items) = &e.kind else {
        return false;
    };
    if head_sym(e) != Some(name) || items.len() != f.params.len() + 1 {
        return false;
    }
    match f.scrutinee {
        Some(i) => ctor_of(&items[i + 1], defs).is_some(),
        None => true,
    }
}

/// Evaluation normalization used to close goals: unfold every total
/// definition applied to a constructor in its scrutinized position.
pub fn eval_normalize(e: &Expr, defs: &Defs, used: &mut HashSet<String>, fuel: &mut u32) -> Expr {
    let e = map_children(e, &mut |c| eval_normalize(c, defs, used, fuel));
    if let Some(r) = reduce_match(&e, defs) {
        return eval_normalize(&r, defs, used, fuel);
    }
    // A canonical user-AC application may still be a redex (its sorted first
    // operand a constructor), so fall through to unfolding with it.
    let e = match normalize_ac(&e, defs) {
        Some(r) if head_sym(&e) == Some("+") => return r,
        Some(r) if head_sym(&r) != head_sym(&e) => return r,
        Some(r) => r,
        None => e,
    };
    if let Some(r) = fold_arith(&e) {
        return r;
    }
    if *fuel == 0 {
        return e;
    }
    if let Some(name) = head_sym(&e) {
        if defs.total.contains(name) && defs.fns[name].scrutinee.is_some() && is_redex(&e, name, defs) {
            *fuel -= 1;
            let ExprKind::List(items) = &e.kind else { unreachable!() };
            let r = instantiate(&defs.fns[name], &items[1..], defs, used);
            return eval_normalize(&r, defs, used, fuel);
        }
    }
    e
}

// ─── Totality (structural recursion) ───────────────────────────────────────

/// Compute the set of total functions: structurally recursive on one fixed
/// parameter, calling only total definitions, with no mutual recursion.
pub fn total_fns(defs: &Defs) -> HashSet<String> {
    let mut total = HashSet::new();
    let mut state: HashMap<String, u8> = HashMap::new(); // 1 visiting, 2 done
    let names: Vec<String> = defs.fns.keys().cloned().collect();
    for n in names {
        check_total(&n, defs, &mut state, &mut total);
    }
    total
}

fn check_total(name: &str, defs: &Defs, state: &mut HashMap<String, u8>, total: &mut HashSet<String>) -> bool {
    match state.get(name) {
        Some(2) => return total.contains(name),
        Some(1) => return false, // mutual recursion
        _ => {}
    }
    state.insert(name.to_string(), 1);
    let f = &defs.fns[name];
    let mut callees = HashSet::new();
    symbols(&f.body, &mut callees);
    let mut ok = callees
        .iter()
        .filter(|c| c.as_str() != name && defs.fns.contains_key(c.as_str()))
        .filter(|c| !f.params.contains(c))
        .all(|c| check_total(c, defs, state, total));
    ok = ok && structural(f, name, defs);
    state.insert(name.to_string(), 2);
    if ok {
        total.insert(name.to_string());
    }
    ok
}

fn structural(f: &FnDef, name: &str, defs: &Defs) -> bool {
    if f.params.iter().any(|p| p == name) {
        return false;
    }
    let mut calls = Vec::new();
    if !collect_self_calls(&f.body, name, &HashMap::new(), defs, &mut calls) {
        return false;
    }
    if calls.is_empty() {
        return true;
    }
    // Some fixed parameter index i such that every self-call's i-th arg is a
    // strict subterm of parameter i.
    (0..f.params.len()).any(|i| {
        calls.iter().all(|args| {
            args.len() == f.params.len()
                && matches!(&args[i], Some(p) if *p == i)
        })
    })
}

/// Walk `e`, tracking which variables are strict subterms of which param
/// (`env`: var -> param index). Each self-call records, per argument, the
/// param it is a strict subterm of. Returns false on a non-call self-reference
/// or on rebinding that would make tracking unsound.
fn collect_self_calls(
    e: &Expr,
    name: &str,
    env: &HashMap<String, usize>,
    defs: &Defs,
    calls: &mut Vec<Vec<Option<usize>>>,
) -> bool {
    let f = &defs.fns[name];
    match &e.kind {
        ExprKind::Symbol(s) => s != name,
        ExprKind::List(items) if head_sym(e) == Some(name) => {
            calls.push(
                items[1..]
                    .iter()
                    .map(|a| match &a.kind {
                        ExprKind::Symbol(s) => env.get(s).copied(),
                        _ => None,
                    })
                    .collect(),
            );
            items[1..].iter().all(|a| collect_self_calls(a, name, env, defs, calls))
        }
        ExprKind::List(items) if head_sym(e) == Some("match") && items.len() >= 2 => {
            if !collect_self_calls(&items[1], name, env, defs, calls) {
                return false;
            }
            let scrut_param = match &items[1].kind {
                ExprKind::Symbol(s) => f.params.iter().position(|p| p == s).filter(|_| !env.contains_key(s)),
                _ => None,
            };
            // A scrutinee that is itself a tracked subterm stays tracked.
            let scrut_sub = match &items[1].kind {
                ExprKind::Symbol(s) => env.get(s).copied(),
                _ => None,
            };
            for pair in items[2..].chunks(2) {
                let mut bound = Vec::new();
                pattern_vars(&pair[0], defs, &mut bound);
                let is_ctor_pat = matches!(pair[0].kind, ExprKind::List(_));
                let mut inner = env.clone();
                for v in &bound {
                    inner.remove(v);
                    if f.params.contains(v) {
                        return false; // shadowing a param: refuse
                    }
                    if is_ctor_pat {
                        if let Some(i) = scrut_param.or(scrut_sub) {
                            inner.insert(v.clone(), i);
                        }
                    }
                }
                if let Some(b) = pair.get(1) {
                    if !collect_self_calls(b, name, &inner, defs, calls) {
                        return false;
                    }
                }
            }
            true
        }
        ExprKind::List(items) if matches!(head_sym(e), Some("let") | Some("fn")) => {
            // Conservative: any rebinding of a param or tracked var rejects.
            let mut bs = HashSet::new();
            if let Some(b) = items.get(1) {
                symbols(b, &mut bs);
            }
            if bs.iter().any(|b| f.params.contains(b) || env.contains_key(b)) {
                return false;
            }
            items.iter().all(|c| collect_self_calls(c, name, env, defs, calls))
        }
        _ => children(e).into_iter().all(|c| collect_self_calls(c, name, env, defs, calls)),
    }
}

// ─── Rewriting ─────────────────────────────────────────────────────────────

/// A rewrite rule: universally quantified `params` act as pattern variables.
#[derive(Clone, Debug)]
pub struct Rule {
    pub params: Vec<String>,
    pub lhs: Expr,
    pub rhs: Expr,
}

/// First-order matching. At an application of an active user AC operator
/// the two operands are also tried swapped (matching modulo commutativity),
/// since a pattern's variables sort by name rather than by what they bind.
fn match_term(pat: &Expr, t: &Expr, vars: &[String], b: &mut HashMap<String, Expr>, defs: &Defs) -> bool {
    let saved = b.clone();
    if match_plain(pat, t, vars, b, defs) {
        return true;
    }
    *b = saved;
    let (ExprKind::List(ps), ExprKind::List(ts)) = (&pat.kind, &t.kind) else { return false };
    let is_ac = |e: &Expr| head_sym(e).is_some_and(|h| defs.ac.iter().any(|a| a.op == h));
    if ps.len() != 3 || ts.len() != 3 || !is_ac(pat) || head_sym(pat) != head_sym(t) {
        return false;
    }
    let saved = b.clone();
    if match_term(&ps[1], &ts[2], vars, b, defs) && match_term(&ps[2], &ts[1], vars, b, defs) {
        return true;
    }
    *b = saved;
    false
}

fn match_plain(pat: &Expr, t: &Expr, vars: &[String], b: &mut HashMap<String, Expr>, defs: &Defs) -> bool {
    if let ExprKind::Symbol(s) = &pat.kind {
        if vars.contains(s) {
            if let Some(prev) = b.get(s) {
                return term_eq(prev, t);
            }
            b.insert(s.clone(), t.clone());
            return true;
        }
    }
    let (pc, tc) = (children(pat), children(t));
    let same_kind = std::mem::discriminant(&pat.kind) == std::mem::discriminant(&t.kind);
    if !same_kind || pc.len() != tc.len() {
        return false;
    }
    if pc.is_empty() {
        return term_eq(pat, t);
    }
    if let (ExprKind::DotAccess(_, f), ExprKind::DotAccess(_, g)) = (&pat.kind, &t.kind) {
        if f != g {
            return false;
        }
    }
    pc.iter().zip(tc).all(|(p, x)| match_term(p, x, vars, b, defs))
}

/// Preorder walk replacing the `target`-th match of `rule`. Returns the new
/// term if a replacement happened.
fn rewrite_nth(t: &Expr, rule: &Rule, target: usize, seen: &mut usize, defs: &Defs) -> Option<Expr> {
    let mut b = HashMap::new();
    if match_term(&rule.lhs, t, &rule.params, &mut b, defs) {
        if *seen == target {
            return Some(subst(&rule.rhs, &b, defs));
        }
        *seen += 1;
    }
    let mut done = false;
    let out = map_children(t, &mut |c| {
        if done {
            return c.clone();
        }
        match rewrite_nth(c, rule, target, seen, defs) {
            Some(r) => {
                done = true;
                r
            }
            None => c.clone(),
        }
    });
    done.then_some(out)
}

pub fn rewrite(t: &Expr, rule: &Rule, at: usize, defs: &Defs) -> Result<Expr, String> {
    let mut lhs_vars = HashSet::new();
    symbols(&rule.lhs, &mut lhs_vars);
    let mut rhs_vars = HashSet::new();
    symbols(&rule.rhs, &mut rhs_vars);
    if let Some(v) = rule
        .params
        .iter()
        .find(|p| rhs_vars.contains(*p) && !lhs_vars.contains(*p))
    {
        return Err(format!("rule would introduce unbound variable `{v}`"));
    }
    rewrite_nth(t, rule, at, &mut 0, defs).ok_or_else(|| "rule does not match".to_string())
}

/// Replace the leftmost-outermost redex of `name` (an application whose
/// scrutinized argument is a constructor); failing that, the leftmost-outermost
/// application at all.
pub fn unfold(t: &Expr, name: &str, defs: &Defs, used: &mut HashSet<String>) -> Result<Expr, String> {
    let f = defs
        .fns
        .get(name)
        .ok_or_else(|| format!("`{name}` is not defined in defs.oo"))?;
    if !defs.total.contains(name) {
        return Err(format!("`{name}` is not structurally recursive; cannot unfold"));
    }
    let arity = f.params.len();
    let is_app = |e: &Expr| matches!(&e.kind, ExprKind::List(v) if v.len() == arity + 1) && head_sym(e) == Some(name);
    for pred in [true, false] {
        let mut done = false;
        let out = replace_first(t, &mut |e| {
            let hit = if pred { is_redex(e, name, defs) } else { is_app(e) };
            if !hit {
                return None;
            }
            let ExprKind::List(items) = &e.kind else { return None };
            Some(instantiate(f, &items[1..], defs, used))
        }, &mut done);
        if done {
            return Ok(out);
        }
    }
    Err(format!("no application of `{name}`"))
}

fn replace_first(t: &Expr, f: &mut dyn FnMut(&Expr) -> Option<Expr>, done: &mut bool) -> Expr {
    if *done {
        return t.clone();
    }
    if let Some(r) = f(t) {
        *done = true;
        return r;
    }
    map_children(t, &mut |c| replace_first(c, f, done))
}

// ─── Proof checking ────────────────────────────────────────────────────────

#[derive(Clone, Debug, PartialEq)]
pub enum Outcome {
    Proven,
    Admitted,
    Stuck { case: String, term: String, reason: String },
}

/// A type as far as the kernel needs it: a head name plus argument types.
/// `?` stands for an argument the checker could not determine; a field whose
/// type is such an unknown parameter gets no type (so it cannot be split).
#[derive(Clone, Debug, PartialEq)]
pub struct Ty {
    pub name: String,
    pub args: Vec<Ty>,
}

impl Ty {
    pub fn named(n: &str) -> Ty {
        Ty { name: n.to_string(), args: vec![] }
    }

    pub fn app(n: &str, args: Vec<Ty>) -> Ty {
        Ty { name: n.to_string(), args }
    }

    pub fn unknown() -> Ty {
        Ty::named("?")
    }
}

/// The type of each field of `ctor` (a constructor of `ty`), instantiating
/// the ADT's type parameters with `ty.args`. `None` where unknown.
pub fn field_tys(defs: &Defs, ty: &Ty, fields: &[Expr]) -> Vec<Option<Ty>> {
    let params = defs.type_params.get(&ty.name).cloned().unwrap_or_default();
    fn conv(f: &Expr, params: &[String], ty: &Ty) -> Option<Ty> {
        match &f.kind {
            ExprKind::Symbol(s) => match params.iter().position(|p| p == s) {
                Some(i) => ty.args.get(i).filter(|t| t.name != "?").cloned(),
                None => Some(Ty::named(s)),
            },
            ExprKind::List(v) | ExprKind::Tuple(v) => {
                let Some(ExprKind::Symbol(h)) = v.first().map(|e| &e.kind) else {
                    return None;
                };
                let args = v[1..].iter().map(|a| conv(a, params, ty).unwrap_or_else(Ty::unknown)).collect();
                Some(Ty::app(h, args))
            }
            _ => None,
        }
    }
    fields.iter().map(|f| conv(f, &params, ty)).collect()
}

/// Is a declared field type exactly the ADT `name` itself (`name` or
/// `(name ...)`)? Only such fields get an induction hypothesis: a field like
/// `(List (Tree T))` mentions the ADT but is not a structural subterm of it.
fn is_self_field(f: &Expr, name: &str) -> bool {
    match &f.kind {
        ExprKind::Symbol(s) => s == name,
        ExprKind::List(v) | ExprKind::Tuple(v) => matches!(v.first().map(|e| &e.kind), Some(ExprKind::Symbol(h)) if h == name),
        _ => false,
    }
}

pub struct Goal<'a> {
    pub law_name: &'a str,
    pub params: &'a [String],
    /// param name -> ADT type, where known
    pub param_types: &'a HashMap<String, Ty>,
    pub lhs: &'a Expr,
    pub rhs: &'a Expr,
}

/// Look up a citable law by name: returns (rule, admitted?).
pub type Lemmas<'a> = dyn Fn(&str) -> Option<(Rule, bool)> + 'a;

struct Ctx<'a> {
    defs: &'a Defs,
    lemmas: &'a Lemmas<'a>,
    admitted: bool,
}

#[derive(Clone)]
struct State {
    l: Expr,
    r: Expr,
    /// Law params still universally quantified in this goal.
    params: Vec<String>,
    /// var -> type (law params and constructor fields introduced by splits)
    types: HashMap<String, Ty>,
    /// Induction hypotheses in scope, keyed by the recursive field they are
    /// about. Cited as `ih-FIELD`; plain `ih` names the only one.
    ihs: Vec<(String, Rule)>,
    case: String,
}

pub fn check_proof(goal: &Goal, steps: &[Expr], defs: &Defs, lemmas: &Lemmas) -> Outcome {
    let mut ctx = Ctx { defs, lemmas, admitted: false };
    let st = State {
        l: normalize(goal.lhs, defs),
        r: normalize(goal.rhs, defs),
        params: goal.params.to_vec(),
        types: goal.param_types.clone(),
        ihs: Vec::new(),
        case: "root".into(),
    };
    let _ = goal.law_name;
    match run_steps(&mut ctx, st, steps) {
        Ok(()) if ctx.admitted => Outcome::Admitted,
        Ok(()) => Outcome::Proven,
        Err(o) => o,
    }
}

fn stuck(st: &State, reason: impl Into<String>) -> Outcome {
    let mut reason = reason.into();
    if !st.ihs.is_empty() {
        let names: Vec<String> = st.ihs.iter().map(|(f, _)| format!("ih-{f}")).collect();
        reason = format!("{reason} [hypotheses: {}]", names.join(" "));
    }
    Outcome::Stuck {
        case: st.case.clone(),
        term: format!("{}", st.l),
        reason,
    }
}

/// Resolve `ih` / `ih-FIELD` to a hypothesis in scope. `None` if `name` is
/// not hypothesis syntax (so it is looked up as a law).
fn lookup_ih(st: &State, name: &str) -> Option<Result<Rule, Outcome>> {
    if name == "ih" {
        return Some(match st.ihs.as_slice() {
            [] => Err(stuck(st, "no induction hypothesis in scope")),
            [(_, r)] => Ok(r.clone()),
            _ => Err(stuck(st, "`ih` is ambiguous here; name one with ih-FIELD")),
        });
    }
    let field = name.strip_prefix("ih-")?;
    Some(
        st.ihs
            .iter()
            .find(|(f, _)| f == field)
            .map(|(_, r)| r.clone())
            .ok_or_else(|| stuck(st, format!("no hypothesis `{name}` in scope"))),
    )
}

fn closes(st: &State, defs: &Defs) -> bool {
    let mut used = HashSet::new();
    symbols(&st.l, &mut used);
    symbols(&st.r, &mut used);
    let mut fuel = 10_000;
    let l = eval_normalize(&st.l, defs, &mut used, &mut fuel);
    let r = eval_normalize(&st.r, defs, &mut used, &mut fuel);
    term_eq(&l, &r)
}

fn used_symbols(st: &State) -> HashSet<String> {
    let mut used = HashSet::new();
    symbols(&st.l, &mut used);
    symbols(&st.r, &mut used);
    used.extend(st.params.iter().cloned());
    used.extend(st.types.keys().cloned());
    for (f, ih) in &st.ihs {
        used.insert(f.clone());
        symbols(&ih.lhs, &mut used);
        symbols(&ih.rhs, &mut used);
    }
    used
}

fn run_steps(ctx: &mut Ctx, mut st: State, steps: &[Expr]) -> Result<(), Outcome> {
    for (i, step) in steps.iter().enumerate() {
        let ExprKind::List(items) = &step.kind else {
            return Err(stuck(&st, format!("malformed step `{step}`")));
        };
        let op = head_sym(step).unwrap_or("");
        match op {
            "unfold" => {
                let Some(ExprKind::Symbol(f)) = items.get(1).map(|e| &e.kind) else {
                    return Err(stuck(&st, "unfold needs a function name"));
                };
                let mut used = used_symbols(&st);
                st.l = unfold(&st.l, f, ctx.defs, &mut used)
                    .map(|l| normalize(&l, ctx.defs))
                    .map_err(|e| stuck(&st, format!("unfold {f}: {e}")))?;
            }
            "rewrite" => {
                let Some(ExprKind::Symbol(name)) = items.get(1).map(|e| &e.kind) else {
                    return Err(stuck(&st, "rewrite needs a rule name"));
                };
                let (mut rev, mut at) = (false, 0usize);
                let mut j = 2;
                while j < items.len() {
                    match &items[j].kind {
                        ExprKind::Keyword(k) if k == "rev" => rev = true,
                        ExprKind::Keyword(k) if k == "at" => {
                            j += 1;
                            match items.get(j).map(|e| &e.kind) {
                                Some(ExprKind::Int(n)) if *n >= 0 => at = *n as usize,
                                _ => return Err(stuck(&st, "`:at` needs a non-negative integer")),
                            }
                        }
                        _ => return Err(stuck(&st, format!("unknown rewrite option `{}`", items[j]))),
                    }
                    j += 1;
                }
                let rule = if let Some(ih) = lookup_ih(&st, name) {
                    ih?
                } else {
                    let (rule, adm) = (ctx.lemmas)(name).ok_or_else(|| {
                        stuck(&st, format!("`{name}` is not a law proven earlier"))
                    })?;
                    ctx.admitted |= adm;
                    rule
                };
                // Rules are matched in the same canonical form as the goal.
                let (lhs, rhs) = (normalize(&rule.lhs, ctx.defs), normalize(&rule.rhs, ctx.defs));
                let rule = if rev {
                    Rule { params: rule.params, lhs: rhs, rhs: lhs }
                } else {
                    Rule { params: rule.params, lhs, rhs }
                };
                st.l = rewrite(&st.l, &rule, at, ctx.defs)
                    .map(|l| normalize(&l, ctx.defs))
                    .map_err(|e| stuck(&st, format!("rewrite {name}: {e}")))?;
            }
            "refl" => {
                if !closes(&st, ctx.defs) {
                    return Err(stuck(&st, "refl: sides differ"));
                }
                return Ok(());
            }
            "admit" => {
                ctx.admitted = true;
                return Ok(());
            }
            "induction" | "cases" => {
                if i + 1 != steps.len() {
                    return Err(stuck(&st, format!("steps after `{op}` are not allowed; put them in its cases")));
                }
                return split(ctx, st, items, op == "induction");
            }
            _ => return Err(stuck(&st, format!("unknown step `{step}`"))),
        }
    }
    if closes(&st, ctx.defs) {
        Ok(())
    } else {
        Err(stuck(&st, "no step applies; sides differ"))
    }
}

fn split(ctx: &mut Ctx, st: State, items: &[Expr], induction: bool) -> Result<(), Outcome> {
    let Some(ExprKind::Symbol(var)) = items.get(1).map(|e| &e.kind) else {
        return Err(stuck(&st, "induction/cases needs a variable"));
    };
    let var = var.clone();
    if induction && !st.params.contains(&var) {
        return Err(stuck(&st, format!("`{var}` is not a law parameter of this goal")));
    }
    let Some(ty) = st.types.get(&var).cloned() else {
        return Err(stuck(&st, format!("type of `{var}` is not a known ADT")));
    };
    let Some(ctors) = ctx.defs.types.get(&ty.name) else {
        return Err(stuck(&st, format!("`{}` is not an ADT declared in defs.oo", ty.name)));
    };
    let ty_name = ty.name.clone();
    if induction {
        let mut fs = HashSet::new();
        symbols(&st.l, &mut fs);
        symbols(&st.r, &mut fs);
        if let Some(bad) = fs
            .iter()
            .find(|s| ctx.defs.fns.contains_key(s.as_str()) && !ctx.defs.total.contains(s.as_str()))
        {
            return Err(stuck(&st, format!("induction needs total functions; `{bad}` is not structurally recursive")));
        }
    }
    let cases: Vec<&Expr> = items[2..].iter().collect();
    for ctor in ctors {
        let case = cases.iter().find(|c| {
            head_sym(c).is_some_and(|h| h == ctor.name || h.eq_ignore_ascii_case(&ctor.name))
        });
        let mut used = used_symbols(&st);
        let mut fields = Vec::new();
        let mut types = st.types.clone();
        types.remove(&var);
        let ftys = field_tys(ctx.defs, &ty, &ctor.fields);
        // Fields that are structural subterms of the same ADT.
        let mut rec_fields = Vec::new();
        for (k, rec) in ctor.recursive.iter().enumerate() {
            let is_self = ctor.fields.get(k).is_some_and(|f| is_self_field(f, &ty_name));
            let base = if *rec { "rest" } else { "x" };
            let n = fresh_name(base, &mut used);
            if let Some(Some(t)) = ftys.get(k) {
                types.insert(n.clone(), t.clone());
            }
            if is_self {
                rec_fields.push(n.clone());
            }
            fields.push(sym(&n));
        }
        let value = if fields.is_empty() {
            sym(&ctor.name)
        } else {
            let mut v = vec![sym(&ctor.name)];
            v.extend(fields);
            list(v)
        };
        let m = HashMap::from([(var.clone(), value)]);
        let params: Vec<String> = st.params.iter().filter(|p| **p != var).cloned().collect();
        // Induction: one hypothesis per recursive field, each quantified over
        // the remaining law params. Cases: hypotheses already in scope carry
        // over, specialized at the split value unless they quantify `var`.
        let ihs: Vec<(String, Rule)> = if induction {
            rec_fields
                .iter()
                .map(|rf| {
                    let im = HashMap::from([(var.clone(), sym(rf))]);
                    (
                        rf.clone(),
                        Rule {
                            params: params.clone(),
                            lhs: subst(&st.l, &im, ctx.defs),
                            rhs: subst(&st.r, &im, ctx.defs),
                        },
                    )
                })
                .collect()
        } else {
            st.ihs
                .iter()
                .map(|(f, ih)| {
                    let ih = if ih.params.contains(&var) {
                        ih.clone()
                    } else {
                        Rule {
                            lhs: subst(&ih.lhs, &m, ctx.defs),
                            rhs: subst(&ih.rhs, &m, ctx.defs),
                            params: ih.params.clone(),
                        }
                    };
                    (f.clone(), ih)
                })
                .collect()
        };
        let child = State {
            l: normalize(&subst(&st.l, &m, ctx.defs), ctx.defs),
            r: normalize(&subst(&st.r, &m, ctx.defs), ctx.defs),
            params,
            types,
            ihs,
            case: if st.case == "root" { ctor.name.clone() } else { format!("{}/{}", st.case, ctor.name) },
        };
        let Some(case) = case else {
            return Err(stuck(&child, format!("missing case {}", ctor.name)));
        };
        let ExprKind::List(cs) = &case.kind else { unreachable!() };
        run_steps(ctx, child, &cs[1..])?;
    }
    if let Some(extra) = cases.iter().find(|c| {
        !ctors
            .iter()
            .any(|k| head_sym(c).is_some_and(|h| h.eq_ignore_ascii_case(&k.name)))
    }) {
        return Err(stuck(&st, format!("case `{extra}` is not a constructor of {ty_name}")));
    }
    Ok(())
}
