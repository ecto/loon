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
                && x.iter()
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
        Some(ExprKind::Symbol(_))
            if matches!(items.get(2).map(|e| &e.kind), Some(ExprKind::List(_))) =>
        {
            2
        }
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
            ExprKind::List(vec![
                Expr::new(ExprKind::Symbol("+".into()), e.span),
                t,
                acc,
            ]),
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
    let ExprKind::List(items) = &e.kind else {
        return None;
    };
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
    let map: HashMap<String, Expr> = f.params.iter().cloned().zip(args.iter().cloned()).collect();
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
        if defs.total.contains(name)
            && defs.fns[name].scrutinee.is_some()
            && is_redex(&e, name, defs)
        {
            *fuel -= 1;
            let ExprKind::List(items) = &e.kind else {
                unreachable!()
            };
            let r = instantiate(&defs.fns[name], &items[1..], defs, used);
            return eval_normalize(&r, defs, used, fuel);
        }
    }
    e
}

// ─── Totality (structural recursion) ───────────────────────────────────────

/// Fns of defs.oo mentioned in `f`'s body (other than names its params shadow).
fn callees(f: &FnDef, defs: &Defs) -> Vec<String> {
    let mut s = HashSet::new();
    symbols(&f.body, &mut s);
    let mut v: Vec<String> = s
        .into_iter()
        .filter(|c| defs.fns.contains_key(c) && !f.params.contains(c))
        .collect();
    v.sort();
    v
}

/// Strongly connected components of the call graph of defs.oo, callees
/// first (Tarjan). Members of a component are listed in `order` order.
pub fn fn_sccs(defs: &Defs, order: &[String]) -> Vec<Vec<String>> {
    struct T<'a> {
        defs: &'a Defs,
        index: HashMap<String, usize>,
        low: HashMap<String, usize>,
        stack: Vec<String>,
        on: HashSet<String>,
        out: Vec<Vec<String>>,
    }
    fn visit(t: &mut T, v: &str) {
        let i = t.index.len();
        t.index.insert(v.to_string(), i);
        t.low.insert(v.to_string(), i);
        t.stack.push(v.to_string());
        t.on.insert(v.to_string());
        for w in callees(&t.defs.fns[v], t.defs) {
            if !t.index.contains_key(&w) {
                visit(t, &w);
                let lw = t.low[&w];
                let lv = t.low.get_mut(v).unwrap();
                *lv = (*lv).min(lw);
            } else if t.on.contains(&w) {
                let iw = t.index[&w];
                let lv = t.low.get_mut(v).unwrap();
                *lv = (*lv).min(iw);
            }
        }
        if t.low[v] == t.index[v] {
            let mut comp = Vec::new();
            while let Some(w) = t.stack.pop() {
                t.on.remove(&w);
                let done = w == v;
                comp.push(w);
                if done {
                    break;
                }
            }
            t.out.push(comp);
        }
    }
    let mut t = T {
        defs,
        index: HashMap::new(),
        low: HashMap::new(),
        stack: Vec::new(),
        on: HashSet::new(),
        out: Vec::new(),
    };
    let mut roots: Vec<String> = order
        .iter()
        .filter(|n| defs.fns.contains_key(*n))
        .cloned()
        .collect();
    let mut rest: Vec<String> = defs
        .fns
        .keys()
        .filter(|n| !roots.contains(n))
        .cloned()
        .collect();
    rest.sort();
    roots.extend(rest);
    for r in &roots {
        if !t.index.contains_key(r) {
            visit(&mut t, r);
        }
    }
    let pos = |n: &String| roots.iter().position(|r| r == n);
    for comp in t.out.iter_mut() {
        comp.sort_by_key(pos);
    }
    t.out
}

/// Compute the set of total functions. Each strongly connected component
/// of the call graph (a single fn, or a group of mutually recursive fns) is
/// total when every fn it calls outside itself is total and it is
/// structurally recursive as a group (`structural_group`).
pub fn total_fns(defs: &Defs) -> HashSet<String> {
    let mut total: HashSet<String> = HashSet::new();
    for group in fn_sccs(defs, &defs.fn_order) {
        let ext_ok = group.iter().all(|n| {
            callees(&defs.fns[n], defs)
                .iter()
                .all(|c| group.contains(c) || total.contains(c))
        });
        if ext_ok && structural_group(&group, defs) {
            total.extend(group);
        }
    }
    total
}

/// The structural rule for a recursive group (one fn, or several mutually
/// recursive fns, e.g. `subst` on a term and `subst-list` on its argument
/// list). The group is accepted when there is a choice of one *measure*
/// parameter per member such that every call from member f to member g
/// passes, in g's measure position, a pattern-bound strict subterm of f's
/// measure parameter (a variable bound by a constructor pattern of a `match`
/// on that parameter, or on such a variable, transitively).
///
/// Soundness: values are finite constructor trees and a strict subterm has
/// strictly fewer constructor nodes, so along any chain of calls inside the
/// group the size of the measure argument strictly decreases; every chain
/// is finite, and calls leaving the group go to fns already known total.
/// For a single fn this is exactly "every self-call passes a strict subterm
/// of one fixed parameter".
fn structural_group(group: &[String], defs: &Defs) -> bool {
    let mut calls: Vec<(usize, usize, Vec<Option<usize>>)> = Vec::new();
    for (i, n) in group.iter().enumerate() {
        let f = &defs.fns[n];
        if f.params.iter().any(|p| group.contains(p)) {
            return false;
        }
        let mut cs = Vec::new();
        if !collect_group_calls(&f.body, f, group, &HashMap::new(), defs, &mut cs) {
            return false;
        }
        for (callee, args) in cs {
            let j = group.iter().position(|g| *g == callee).unwrap();
            if args.len() != defs.fns[&callee].params.len() {
                return false;
            }
            calls.push((i, j, args));
        }
    }
    if calls.is_empty() {
        return true;
    }
    let arities: Vec<usize> = group.iter().map(|n| defs.fns[n].params.len()).collect();
    if arities.contains(&0) {
        return false;
    }
    // Try every measure assignment (odometer), up to a small cap.
    let combos = arities
        .iter()
        .try_fold(1usize, |acc, a| acc.checked_mul(*a));
    if combos.is_none_or(|c| c > 1 << 16) {
        return false;
    }
    let mut d = vec![0usize; group.len()];
    loop {
        if calls.iter().all(|(i, j, args)| args[d[*j]] == Some(d[*i])) {
            return true;
        }
        let mut k = 0;
        loop {
            if k == d.len() {
                return false;
            }
            d[k] += 1;
            if d[k] < arities[k] {
                break;
            }
            d[k] = 0;
            k += 1;
        }
    }
}

/// Walk `e` (part of `f`'s body), tracking which variables are strict
/// subterms of which param of `f` (`env`: var -> param index). Each call to
/// a member of `group` records the callee and, per argument, the param it
/// is a strict subterm of. Returns false on a non-call reference to a group
/// member or on rebinding that would make tracking unsound.
fn collect_group_calls(
    e: &Expr,
    f: &FnDef,
    group: &[String],
    env: &HashMap<String, usize>,
    defs: &Defs,
    calls: &mut Vec<(String, Vec<Option<usize>>)>,
) -> bool {
    let rec = |c: &Expr, env: &HashMap<String, usize>, calls: &mut Vec<_>| {
        collect_group_calls(c, f, group, env, defs, calls)
    };
    match &e.kind {
        ExprKind::Symbol(s) => !group.contains(s),
        ExprKind::List(items) if head_sym(e).is_some_and(|h| group.iter().any(|g| g == h)) => {
            calls.push((
                head_sym(e).unwrap().to_string(),
                items[1..]
                    .iter()
                    .map(|a| match &a.kind {
                        ExprKind::Symbol(s) => env.get(s).copied(),
                        _ => None,
                    })
                    .collect(),
            ));
            items[1..].iter().all(|a| rec(a, env, calls))
        }
        ExprKind::List(items) if head_sym(e) == Some("match") && items.len() >= 2 => {
            if !rec(&items[1], env, calls) {
                return false;
            }
            let scrut_param = match &items[1].kind {
                ExprKind::Symbol(s) => f
                    .params
                    .iter()
                    .position(|p| p == s)
                    .filter(|_| !env.contains_key(s)),
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
                    if f.params.contains(v) || group.contains(v) {
                        return false; // shadowing a param or a group fn: refuse
                    }
                    if is_ctor_pat {
                        if let Some(i) = scrut_param.or(scrut_sub) {
                            inner.insert(v.clone(), i);
                        }
                    }
                }
                if let Some(b) = pair.get(1) {
                    if !rec(b, &inner, calls) {
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
            if bs
                .iter()
                .any(|b| f.params.contains(b) || env.contains_key(b))
            {
                return false;
            }
            items.iter().all(|c| rec(c, env, calls))
        }
        _ => children(e).into_iter().all(|c| rec(c, env, calls)),
    }
}

// ─── Rewriting ─────────────────────────────────────────────────────────────

/// A rewrite rule: universally quantified `params` act as pattern variables.
/// `hyps` are the rule's conditions (a conditional law's `:given`): each,
/// instantiated under the match, is an obligation discharged at the use site
/// (see `discharge`). Unconditional rules have none.
#[derive(Clone, Debug)]
pub struct Rule {
    pub params: Vec<String>,
    pub lhs: Expr,
    pub rhs: Expr,
    pub hyps: Vec<(Expr, Expr)>,
}

/// First-order matching. At an application of an active user AC operator
/// the two operands are also tried swapped (matching modulo commutativity),
/// since a pattern's variables sort by name rather than by what they bind.
fn match_term(
    pat: &Expr,
    t: &Expr,
    vars: &[String],
    b: &mut HashMap<String, Expr>,
    defs: &Defs,
) -> bool {
    let saved = b.clone();
    if match_plain(pat, t, vars, b, defs) {
        return true;
    }
    *b = saved;
    let (ExprKind::List(ps), ExprKind::List(ts)) = (&pat.kind, &t.kind) else {
        return false;
    };
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

fn match_plain(
    pat: &Expr,
    t: &Expr,
    vars: &[String],
    b: &mut HashMap<String, Expr>,
    defs: &Defs,
) -> bool {
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
    pc.iter()
        .zip(tc)
        .all(|(p, x)| match_term(p, x, vars, b, defs))
}

/// Preorder walk replacing the `target`-th match of `rule`. Returns the new
/// term if a replacement happened.
fn rewrite_nth(
    t: &Expr,
    rule: &Rule,
    target: usize,
    seen: &mut usize,
    defs: &Defs,
) -> Option<Expr> {
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

type OnMatch<'a> = dyn FnMut(&HashMap<String, Expr>) -> Result<Expr, String> + 'a;

/// Like `rewrite_nth`, but hands the match's bindings to `on_match`, which
/// builds the replacement (or refuses it).
fn rewrite_nth_by(
    t: &Expr,
    rule: &Rule,
    target: usize,
    seen: &mut usize,
    defs: &Defs,
    on_match: &mut OnMatch,
) -> Option<Result<Expr, String>> {
    let mut b = HashMap::new();
    if match_term(&rule.lhs, t, &rule.params, &mut b, defs) {
        if *seen == target {
            return Some(on_match(&b));
        }
        *seen += 1;
    }
    let mut res: Option<Result<Expr, String>> = None;
    let out = map_children(t, &mut |c| {
        if res.is_some() {
            return c.clone();
        }
        match rewrite_nth_by(c, rule, target, seen, defs, on_match) {
            Some(Ok(r)) => {
                res = Some(Ok(r.clone()));
                r
            }
            Some(Err(e)) => {
                res = Some(Err(e));
                c.clone()
            }
            None => c.clone(),
        }
    });
    match res {
        Some(Ok(_)) => Some(Ok(out)),
        r => r,
    }
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
pub fn unfold(
    t: &Expr,
    name: &str,
    defs: &Defs,
    used: &mut HashSet<String>,
) -> Result<Expr, String> {
    let f = defs
        .fns
        .get(name)
        .ok_or_else(|| format!("`{name}` is not defined in defs.oo"))?;
    if !defs.total.contains(name) {
        return Err(format!(
            "`{name}` is not structurally recursive; cannot unfold"
        ));
    }
    let arity = f.params.len();
    let is_app = |e: &Expr| {
        matches!(&e.kind, ExprKind::List(v) if v.len() == arity + 1) && head_sym(e) == Some(name)
    };
    for pred in [true, false] {
        let mut done = false;
        let out = replace_first(
            t,
            &mut |e| {
                let hit = if pred {
                    is_redex(e, name, defs)
                } else {
                    is_app(e)
                };
                if !hit {
                    return None;
                }
                let ExprKind::List(items) = &e.kind else {
                    return None;
                };
                Some(instantiate(f, &items[1..], defs, used))
            },
            &mut done,
        );
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
    Stuck {
        case: String,
        term: String,
        reason: String,
    },
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
        Ty {
            name: n.to_string(),
            args: vec![],
        }
    }

    pub fn app(n: &str, args: Vec<Ty>) -> Ty {
        Ty {
            name: n.to_string(),
            args,
        }
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
                let args = v[1..]
                    .iter()
                    .map(|a| conv(a, params, ty).unwrap_or_else(Ty::unknown))
                    .collect();
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
        ExprKind::List(v) | ExprKind::Tuple(v) => {
            matches!(v.first().map(|e| &e.kind), Some(ExprKind::Symbol(h)) if h == name)
        }
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
    /// The law's `:given` hypotheses, over its params.
    pub hyps: &'a [(Expr, Expr)],
}

/// Look up a citable law by name: returns (rule, admitted?).
pub type Lemmas<'a> = dyn Fn(&str) -> Option<(Rule, bool)> + 'a;

struct Ctx<'a> {
    defs: &'a Defs,
    lemmas: &'a Lemmas<'a>,
    /// Every citable law (name, rule, admitted?), for discharging obligations
    /// by an instance of a proven law.
    known: &'a [(String, Rule, bool)],
    /// The other laws of this proof's `:mutual` group (empty if none).
    partners: &'a [Mutual],
    admitted: bool,
}

/// Another law of a `:mutual` group, as a source of induction hypotheses.
///
/// Mutual induction: laws P1..Pk, each proven by a proof that is a single
/// `[induction Vi ...]` on a param Vi of type Ti, are proven together. In
/// the induction cases of Pi, every constructor field whose type is exactly
/// Tj gets the hypothesis "Pj at that field" (Pj's other params stay
/// quantified), cited as `ih-FIELD` (`ih-FIELD-PJ` when the field already
/// has Pi's own hypothesis); fields of type Ti get Pi's own `ih` as usual.
/// Soundness: by strong induction on the number of constructor nodes n,
/// prove "every Pi holds at every value of size n": each case of each proof
/// uses some Pj only at a constructor field of the value being split, which
/// is strictly smaller. Hence all of P1..Pk hold, and only together: the
/// driver (`verify/mod.rs`) makes no member citable until every member's
/// proof has closed, and none if any is stuck.
#[derive(Clone, Debug)]
pub struct Mutual {
    pub name: String,
    pub params: Vec<String>,
    /// The param the partner's proof does induction on, and its type.
    pub var: String,
    pub var_ty: Ty,
    pub lhs: Expr,
    pub rhs: Expr,
    /// The partner's `:given` hypotheses: obligations wherever its
    /// hypothesis is cited, exactly like a conditional `ih`.
    pub hyps: Vec<(Expr, Expr)>,
}

/// Is `t` fully known (no `?` anywhere)?
fn ty_known(t: &Ty) -> bool {
    t.name != "?" && t.args.iter().all(ty_known)
}

/// The variable of a proof that is exactly one `[induction V ...]` step.
pub fn sole_induction_var(steps: &[Expr]) -> Option<String> {
    let [step] = steps else { return None };
    let ExprKind::List(items) = &step.kind else {
        return None;
    };
    if head_sym(step) != Some("induction") {
        return None;
    }
    match items.get(1).map(|e| &e.kind) {
        Some(ExprKind::Symbol(v)) => Some(v.clone()),
        _ => None,
    }
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
    /// The law's own hypotheses `h1`, `h2`, ..., with the splits so far
    /// substituted in. Their variables are the law's params as fixed values,
    /// never pattern variables. Constructor components are derived on demand
    /// by `hyps_in_scope`.
    hyps: Vec<(String, Expr, Expr)>,
    case: String,
}

pub fn check_proof(goal: &Goal, steps: &[Expr], defs: &Defs, lemmas: &Lemmas) -> Outcome {
    check_proof_with(goal, steps, defs, lemmas, &[])
}

/// `check_proof`, with `known` the laws an obligation may be discharged by.
pub fn check_proof_with(
    goal: &Goal,
    steps: &[Expr],
    defs: &Defs,
    lemmas: &Lemmas,
    known: &[(String, Rule, bool)],
) -> Outcome {
    check_proof_mutual(goal, steps, defs, lemmas, known, &[])
}

/// `check_proof_with` for one law of a `:mutual` group (see `Mutual`):
/// `partners` are the group's other laws. With partners, the proof must be
/// a single `induction` on a law param of fully known type. Members may
/// have `:given` hypotheses: a partner's hypothesis `ih-FIELD` carries them
/// as obligations (instantiated at the field), like a conditional `ih`.
pub fn check_proof_mutual(
    goal: &Goal,
    steps: &[Expr],
    defs: &Defs,
    lemmas: &Lemmas,
    known: &[(String, Rule, bool)],
    partners: &[Mutual],
) -> Outcome {
    if !partners.is_empty() {
        let bad = |reason: String| Outcome::Stuck {
            case: "root".into(),
            term: format!("{}", goal.lhs),
            reason,
        };
        let Some(v) = sole_induction_var(steps) else {
            return bad("a :mutual proof must be exactly one [induction VAR ...] step".into());
        };
        if !goal.params.contains(&v) || !goal.param_types.get(&v).is_some_and(ty_known) {
            return bad(format!(
                ":mutual induction variable `{v}` must be a law param of known type"
            ));
        }
    }
    let mut ctx = Ctx {
        defs,
        lemmas,
        known,
        partners,
        admitted: false,
    };
    let st = State {
        l: normalize(goal.lhs, defs),
        r: normalize(goal.rhs, defs),
        params: goal.params.to_vec(),
        types: goal.param_types.clone(),
        ihs: Vec::new(),
        hyps: goal
            .hyps
            .iter()
            .enumerate()
            .map(|(i, (a, b))| (format!("h{}", i + 1), a.clone(), b.clone()))
            .collect(),
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
    let mut names: Vec<String> = st.ihs.iter().map(|(f, _)| format!("ih-{f}")).collect();
    names.extend(st.hyps.iter().map(|(n, a, b)| format!("{n}: [= {a} {b}]")));
    if !names.is_empty() {
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

// ─── Hypotheses and obligations ────────────────────────────────────────────
//
// A conditional law `[law N [ps] :given [[= A1 B1] ...] [= L R]]` reads
// "for all ps, A1 = B1 ∧ ... → L = R".
//
// Inside N's own proof each hypothesis is a ground rewrite rule `hK` (no
// pattern variables: its free symbols are the law's params, i.e. fixed
// unknown values), substituted by `induction`/`cases` exactly like the goal.
// `hN` rewrites in the goal's canonical form (`normalize`). Constructor
// injectivity: when a hypothesis's two sides, evaluation-normalized (total
// fns applied to constructors unfolded; meaning-preserving), are the same
// constructor applied to the same number of arguments, its K-th argument
// pair is the hypothesis `hN.K` (recursively, `hN.K.J`). Distinct
// constructors are not used (no reasoning by contradiction yet).
//
// Citing a conditional rule elsewhere (a law, or `ih` of a conditional law)
// matches its LHS as usual; each hypothesis instantiated under the match is
// an obligation, discharged right there (`discharge`). Rule params that the
// LHS does not bind may be bound by discharging against an in-scope
// hypothesis (`ih` of `[= ys zs]` given `[= [f ys] [f zs]]` picks `zs` from
// the hypothesis); anything still unbound is an error.

/// A cited rule's name. `h1.2` parses as a dot access; spell it back.
fn rule_name(e: &Expr) -> Option<String> {
    match &e.kind {
        ExprKind::Symbol(s) => Some(s.clone()),
        ExprKind::DotAccess(inner, f) => Some(format!("{}.{f}", rule_name(inner)?)),
        _ => None,
    }
}

/// Is `name` hypothesis syntax: `h` followed by `N(.K)*`?
fn is_hyp_name(name: &str) -> bool {
    name.strip_prefix('h').is_some_and(|r| {
        !r.is_empty()
            && r.split('.')
                .all(|p| !p.is_empty() && p.bytes().all(|c| c.is_ascii_digit()))
    })
}

/// The hypotheses in scope: each `hN` in canonical form, then its
/// evaluation-normalized form under the same name (used only when
/// discharging obligations, since lookup takes the first), then its
/// constructor components (`hN.K`).
fn hyps_in_scope(st: &State, defs: &Defs) -> Vec<(String, Expr, Expr)> {
    fn add(out: &mut Vec<(String, Expr, Expr)>, n: String, a: Expr, b: Expr, defs: &Defs) {
        if let (Some((ca, xa)), Some((cb, xb))) = (ctor_of(&a, defs), ctor_of(&b, defs)) {
            if ca == cb && xa.len() == xb.len() {
                let comps: Vec<(Expr, Expr)> = xa.iter().cloned().zip(xb.iter().cloned()).collect();
                for (k, (x, y)) in comps.into_iter().enumerate() {
                    add(out, format!("{n}.{}", k + 1), x, y, defs);
                }
            }
        }
        out.push((n, a, b));
    }
    let mut used = used_symbols(st);
    let mut out = Vec::new();
    for (n, a, b) in &st.hyps {
        let (a, b) = (normalize(a, defs), normalize(b, defs));
        out.push((n.clone(), a.clone(), b.clone()));
        let mut fuel = 10_000;
        let a = eval_normalize(&a, defs, &mut used, &mut fuel);
        let b = eval_normalize(&b, defs, &mut used, &mut fuel);
        add(&mut out, n.clone(), a, b, defs);
    }

    out
}

/// Does ground `pat` occur in `t` (up to the matcher's AC handling)?
fn occurs(pat: &Expr, t: &Expr, defs: &Defs) -> bool {
    match_term(pat, t, &[], &mut HashMap::new(), defs)
        || children(t).into_iter().any(|c| occurs(pat, c, defs))
}

/// Resolve `hN` / `hN.K` to a ground rule. `None` if not hypothesis syntax.
/// `hN` has a canonical and an evaluated form; the first with a side
/// occurring in the goal is used (either is the same equation).
fn lookup_hyp(st: &State, name: &str, defs: &Defs) -> Option<Result<Rule, Outcome>> {
    let base = name.split('.').next().unwrap_or(name);
    if !is_hyp_name(name) && !st.hyps.iter().any(|(n, _, _)| n == base) {
        return None;
    }
    let cands: Vec<_> = hyps_in_scope(st, defs)
        .into_iter()
        .filter(|(n, _, _)| n == name)
        .collect();
    let pick = cands
        .iter()
        .position(|(_, a, b)| occurs(a, &st.l, defs) || occurs(b, &st.l, defs))
        .unwrap_or(0);
    Some(
        cands
            .into_iter()
            .nth(pick)
            .map(|(_, a, b)| Rule {
                params: vec![],
                lhs: a,
                rhs: b,
                hyps: vec![],
            })
            .ok_or_else(|| stuck(st, format!("no hypothesis `{name}` in scope"))),
    )
}

/// Discharge one obligation `[= a b]`, whose symbols in `free` are still
/// unbound rule params (pattern variables, bound into `b2` on success).
/// In order: (a) no free variables and both sides evaluate to the same
/// normal form; (b) it is an instance of an in-scope hypothesis or one of
/// its constructor components, either orientation (this is the only rule
/// that may bind free variables); (c) no free variables and it is an
/// instance of a known unconditional law, either orientation (citing an
/// admitted law taints the proof). No nested proofs: anything else fails.
fn discharge(
    ctx: &mut Ctx,
    hyps: &[(String, Expr, Expr)],
    a: &Expr,
    b: &Expr,
    free: &[String],
    b2: &mut HashMap<String, Expr>,
) -> bool {
    let defs = ctx.defs;
    let (a, b) = (subst(a, b2, defs), subst(b, b2, defs));
    let mut used = HashSet::new();
    symbols(&a, &mut used);
    symbols(&b, &mut used);
    let mut fuel = 10_000;
    let a = eval_normalize(&normalize(&a, defs), defs, &mut used, &mut fuel);
    let b = eval_normalize(&normalize(&b, defs), defs, &mut used, &mut fuel);
    let open: Vec<String> = free
        .iter()
        .filter(|v| used.contains(*v) && !b2.contains_key(*v))
        .cloned()
        .collect();
    if open.is_empty() && term_eq(&a, &b) {
        return true;
    }
    for (_, l, r) in hyps {
        for (x, y) in [(l, r), (r, l)] {
            let mut m = b2.clone();
            if match_term(&a, x, &open, &mut m, defs) && match_term(&b, y, &open, &mut m, defs) {
                *b2 = m;
                return true;
            }
        }
    }
    if !open.is_empty() {
        return false;
    }
    for (_, rule, adm) in ctx.known {
        if !rule.hyps.is_empty() {
            continue;
        }
        let (l, r) = (normalize(&rule.lhs, defs), normalize(&rule.rhs, defs));
        for (x, y) in [(&l, &r), (&r, &l)] {
            let mut m = HashMap::new();
            if match_term(x, &a, &rule.params, &mut m, defs)
                && match_term(y, &b, &rule.params, &mut m, defs)
            {
                ctx.admitted |= *adm;
                return true;
            }
        }
    }
    false
}

/// Rewrite with a conditional rule: the `at`-th match, then discharge each
/// instantiated hypothesis in order.
fn rewrite_cond(
    ctx: &mut Ctx,
    st: &State,
    target: &Expr,
    rule: &Rule,
    at: usize,
) -> Result<Expr, String> {
    let hyps = hyps_in_scope(st, ctx.defs);
    let mut used = used_symbols(st);
    for (a, b) in &rule.hyps {
        symbols(a, &mut used);
        symbols(b, &mut used);
    }
    symbols(&rule.lhs, &mut used);
    symbols(&rule.rhs, &mut used);
    let defs = ctx.defs;
    let mut on_match = |bind: &HashMap<String, Expr>| -> Result<Expr, String> {
        // Unbound params get fresh names so they cannot be confused with
        // goal symbols of the same spelling.
        let mut bind = bind.clone();
        let mut fresh = Vec::new();
        let mut back = HashMap::new();
        for p in &rule.params {
            if !bind.contains_key(p) {
                let f = fresh_name(p, &mut used);
                bind.insert(p.clone(), sym(&f));
                back.insert(f.clone(), p.clone());
                fresh.push(f);
            }
        }
        let mut b2 = HashMap::new();
        for (a, b) in &rule.hyps {
            let (a, b) = (subst(a, &bind, defs), subst(b, &bind, defs));
            if !discharge(ctx, &hyps, &a, &b, &fresh, &mut b2) {
                let (a, b) = (subst(&a, &b2, defs), subst(&b, &b2, defs));
                return Err(format!("obligation [= {a} {b}] not discharged"));
            }
        }
        let mut rv = HashSet::new();
        symbols(&rule.rhs, &mut rv);
        for f in &fresh {
            if !b2.contains_key(f) && rv.contains(&back[f]) {
                return Err(format!(
                    "rule would introduce unbound variable `{}`",
                    back[f]
                ));
            }
        }
        let bind: HashMap<String, Expr> = bind
            .into_iter()
            .map(|(k, v)| (k, subst(&v, &b2, defs)))
            .collect();
        Ok(subst(&rule.rhs, &bind, defs))
    };
    rewrite_nth_by(target, rule, at, &mut 0, defs, &mut on_match)
        .unwrap_or_else(|| Err("rule does not match".to_string()))
}

/// Ex falso: some hypothesis in scope (or a constructor component of one),
/// evaluation-normalized, equates two distinct constructors. Distinct
/// constructor values are never equal, so the hypotheses are inconsistent
/// and the case holds vacuously.
fn contradictory(st: &State, defs: &Defs) -> bool {
    hyps_in_scope(st, defs).iter().any(|(_, a, b)| {
        matches!((ctor_of(a, defs), ctor_of(b, defs)),
            (Some((ca, xa)), Some((cb, xb))) if ca != cb || xa.len() != xb.len())
    })
}

fn closes(st: &State, defs: &Defs) -> bool {
    if contradictory(st, defs) {
        return true;
    }
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
        for (a, b) in &ih.hyps {
            symbols(a, &mut used);
            symbols(b, &mut used);
        }
    }
    for (_, a, b) in &st.hyps {
        symbols(a, &mut used);
        symbols(b, &mut used);
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
                let Some(name) = items.get(1).and_then(rule_name) else {
                    return Err(stuck(&st, "rewrite needs a rule name"));
                };
                let name = &name;
                let (mut rev, mut at) = (false, 0usize);
                let mut into: Option<String> = None;
                let mut j = 2;
                while j < items.len() {
                    match &items[j].kind {
                        ExprKind::Keyword(k) if k == "rev" => rev = true,
                        ExprKind::Keyword(k) if k == "in" => {
                            j += 1;
                            match items.get(j).map(|e| &e.kind) {
                                Some(ExprKind::Symbol(h))
                                    if st.hyps.iter().any(|(n, _, _)| n == h) =>
                                {
                                    into = Some(h.clone())
                                }
                                _ => {
                                    return Err(stuck(
                                        &st,
                                        "`:in` needs the name of a hypothesis in scope",
                                    ))
                                }
                            }
                        }
                        ExprKind::Keyword(k) if k == "at" => {
                            j += 1;
                            match items.get(j).map(|e| &e.kind) {
                                Some(ExprKind::Int(n)) if *n >= 0 => at = *n as usize,
                                _ => return Err(stuck(&st, "`:at` needs a non-negative integer")),
                            }
                        }
                        _ => {
                            return Err(stuck(
                                &st,
                                format!("unknown rewrite option `{}`", items[j]),
                            ))
                        }
                    }
                    j += 1;
                }
                let rule = if let Some(ih) = lookup_ih(&st, name) {
                    ih?
                } else if let Some(h) = lookup_hyp(&st, name, ctx.defs) {
                    h?
                } else {
                    let (rule, adm) = (ctx.lemmas)(name).ok_or_else(|| {
                        stuck(&st, format!("`{name}` is not a law proven earlier"))
                    })?;
                    ctx.admitted |= adm;
                    rule
                };
                // Rules are matched in the same canonical form as the goal.
                let (lhs, rhs) = (
                    normalize(&rule.lhs, ctx.defs),
                    normalize(&rule.rhs, ctx.defs),
                );
                let rule = if rev {
                    Rule {
                        params: rule.params,
                        lhs: rhs,
                        rhs: lhs,
                        hyps: rule.hyps,
                    }
                } else {
                    Rule {
                        params: rule.params,
                        lhs,
                        rhs,
                        hyps: rule.hyps,
                    }
                };
                if let Some(h) = into {
                    if let Err(e) = rewrite_hyp(ctx, &mut st, &h, &rule, at) {
                        return Err(stuck(&st, format!("rewrite {name} :in {h}: {e}")));
                    }
                    continue;
                }
                let res = if rule.hyps.is_empty() {
                    rewrite(&st.l, &rule, at, ctx.defs)
                } else {
                    let l = st.l.clone();
                    rewrite_cond(ctx, &st, &l, &rule, at)
                };
                st.l = res
                    .map(|l| normalize(&l, ctx.defs))
                    .map_err(|e| stuck(&st, format!("rewrite {name}: {e}")))?;
            }
            "sym" => {
                std::mem::swap(&mut st.l, &mut st.r);
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
                    return Err(stuck(
                        &st,
                        format!("steps after `{op}` are not allowed; put them in its cases"),
                    ));
                }
                if op == "cases" && matches!(items.get(1).map(|e| &e.kind), Some(ExprKind::List(_)))
                {
                    return split_expr(ctx, st, items);
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
        return Err(stuck(
            &st,
            format!("`{var}` is not a law parameter of this goal"),
        ));
    }
    let Some(ty) = st.types.get(&var).cloned() else {
        return Err(stuck(&st, format!("type of `{var}` is not a known ADT")));
    };
    let Some(ctors) = ctx.defs.types.get(&ty.name) else {
        return Err(stuck(
            &st,
            format!("`{}` is not an ADT declared in defs.oo", ty.name),
        ));
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
            return Err(stuck(
                &st,
                format!("induction needs total functions; `{bad}` is not structurally recursive"),
            ));
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
            let is_self = ctor
                .fields
                .get(k)
                .is_some_and(|f| is_self_field(f, &ty_name));
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
            v.extend(fields.iter().cloned());
            list(v)
        };
        let m = HashMap::from([(var.clone(), value)]);
        let params: Vec<String> = st.params.iter().filter(|p| **p != var).cloned().collect();
        // Induction: one hypothesis per recursive field, each quantified over
        // the remaining law params. Cases: hypotheses already in scope carry
        // over, specialized at the split value unless they quantify `var`.
        // For a conditional law, `ih` is the whole law at the smaller
        // argument: its hypotheses (the current `h*`, at the recursive field)
        // are generalized over the other params together with the conclusion,
        // and become obligations wherever `ih` is cited.
        let mut ihs: Vec<(String, Rule)> = if induction {
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
                            hyps: st
                                .hyps
                                .iter()
                                .map(|(_, a, b)| (subst(a, &im, ctx.defs), subst(b, &im, ctx.defs)))
                                .collect(),
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
                            hyps: ih
                                .hyps
                                .iter()
                                .map(|(a, b)| (subst(a, &m, ctx.defs), subst(b, &m, ctx.defs)))
                                .collect(),
                        }
                    };
                    (f.clone(), ih)
                })
                .collect()
        };
        // The root induction of a `:mutual` proof: partner hypotheses.
        if induction && st.case == "root" {
            for (k, f) in fields.iter().enumerate() {
                let (ExprKind::Symbol(fname), Some(Some(fty))) = (&f.kind, ftys.get(k)) else {
                    continue;
                };
                for pt in ctx.partners.iter().filter(|pt| &pt.var_ty == fty) {
                    let mut im = HashMap::from([(pt.var.clone(), sym(fname))]);
                    let mut ps = Vec::new();
                    for q in pt.params.iter().filter(|q| **q != pt.var) {
                        let nq = fresh_name(q, &mut used);
                        im.insert(q.clone(), sym(&nq));
                        ps.push(nq);
                    }
                    let key = if ihs.iter().any(|(k, _)| k == fname) {
                        format!("{fname}-{}", pt.name)
                    } else {
                        fname.clone()
                    };
                    ihs.push((
                        key,
                        Rule {
                            params: ps,
                            lhs: normalize(&subst(&pt.lhs, &im, ctx.defs), ctx.defs),
                            rhs: normalize(&subst(&pt.rhs, &im, ctx.defs), ctx.defs),
                            hyps: pt
                                .hyps
                                .iter()
                                .map(|(a, b)| (subst(a, &im, ctx.defs), subst(b, &im, ctx.defs)))
                                .collect(),
                        },
                    ));
                }
            }
        }
        let child = State {
            l: normalize(&subst(&st.l, &m, ctx.defs), ctx.defs),
            r: normalize(&subst(&st.r, &m, ctx.defs), ctx.defs),
            params,
            types,
            ihs,
            hyps: st
                .hyps
                .iter()
                .map(|(n, a, b)| (n.clone(), subst(a, &m, ctx.defs), subst(b, &m, ctx.defs)))
                .collect(),
            case: if st.case == "root" {
                ctor.name.clone()
            } else {
                format!("{}/{}", st.case, ctor.name)
            },
        };
        let Some(case) = case else {
            return Err(stuck(&child, format!("missing case {}", ctor.name)));
        };
        let ExprKind::List(cs) = &case.kind else {
            unreachable!()
        };
        run_steps(ctx, child, &cs[1..])?;
    }
    if let Some(extra) = cases.iter().find(|c| {
        !ctors
            .iter()
            .any(|k| head_sym(c).is_some_and(|h| h.eq_ignore_ascii_case(&k.name)))
    }) {
        return Err(stuck(
            &st,
            format!("case `{extra}` is not a constructor of {ty_name}"),
        ));
    }
    Ok(())
}

/// `[rewrite RULE :in NAME]`: rewrite inside hypothesis NAME instead of the
/// goal. The hypothesis is first replaced by its evaluation-normalized form
/// (meaning-preserving), then the `at`-th match of RULE, counting through its
/// left side and then its right side in preorder, is rewritten. Sound: the
/// rule instance holds in this context (its obligations are discharged from
/// the hypotheses in scope), so the new hypothesis follows from the old one;
/// the old one is dropped, which only weakens the context.
fn rewrite_hyp(
    ctx: &mut Ctx,
    st: &mut State,
    name: &str,
    rule: &Rule,
    at: usize,
) -> Result<(), String> {
    let defs = ctx.defs;
    let k = st.hyps.iter().position(|(n, _, _)| n == name).unwrap();
    let mut used = used_symbols(st);
    let mut fuel = 10_000;
    let a = eval_normalize(&normalize(&st.hyps[k].1, defs), defs, &mut used, &mut fuel);
    let b = eval_normalize(&normalize(&st.hyps[k].2, defs), defs, &mut used, &mut fuel);
    let pair = list(vec![sym("="), a, b]);
    let ExprKind::List(v) = &pair.kind else {
        unreachable!()
    };
    // Matches in `a` are counted first; `at` beyond them falls through to `b`.
    let count = |t: &Expr| -> usize {
        fn go(t: &Expr, r: &Rule, defs: &Defs) -> usize {
            let mut b = HashMap::new();
            let me = usize::from(match_term(&r.lhs, t, &r.params, &mut b, defs));
            me + children(t)
                .into_iter()
                .map(|c| go(c, r, defs))
                .sum::<usize>()
        }
        go(t, rule, defs)
    };
    let na = count(&v[1]);
    let (side, at2) = if at < na { (1, at) } else { (2, at - na) };
    let res = if rule.hyps.is_empty() {
        rewrite(&v[side], rule, at2, defs)
    } else {
        rewrite_cond(ctx, st, &v[side], rule, at2)
    };
    match res {
        Ok(t) => {
            let t = normalize(&t, defs);
            let (na, nb) = if side == 1 {
                (t, v[2].clone())
            } else {
                (v[1].clone(), t)
            };
            st.hyps[k] = (name.to_string(), na, nb);
            Ok(())
        }
        Err(e) => Err(e),
    }
}

/// Replace every occurrence of `pat` in `t` by `by`.
fn replace_all(t: &Expr, pat: &Expr, by: &Expr) -> Expr {
    if term_eq(t, pat) {
        return by.clone();
    }
    map_children(t, &mut |c| replace_all(c, pat, by))
}

/// `[cases EXPR :as NAME CASE*]`: case analysis on an arbitrary term.
///
/// EXPR's type is inferred (`infer::expr_type`, from the types of the goal's
/// variables) and must be an ADT of defs.oo. For each constructor K, with
/// fresh field names f1.., the case gets hypothesis NAME `[= EXPR [K f1 ..]]`
/// and every occurrence of EXPR in the goal and in the (evaluation-
/// normalized) hypotheses is replaced by `[K f1 ..]`.
///
/// Soundness: every fn in EXPR is total (checked: list heads must be
/// constructors, `match`, or total fns of defs.oo, and no symbol may name a
/// non-total fn), so EXPR evaluates to a value of its ADT type, which is one
/// of the constructors applied to some field values. The cases are
/// exhaustive, each with that equation as an assumption, and replacing EXPR
/// by an equal term preserves the goal and the hypotheses.
fn split_expr(ctx: &mut Ctx, st: State, items: &[Expr]) -> Result<(), Outcome> {
    let defs = ctx.defs;
    let expr = normalize(&items[1], defs);
    let name = match (items.get(2).map(|e| &e.kind), items.get(3).map(|e| &e.kind)) {
        (Some(ExprKind::Keyword(k)), Some(ExprKind::Symbol(n))) if k == "as" => n.clone(),
        _ => return Err(stuck(&st, "cases on a term needs `:as NAME`")),
    };
    if name == "ih" || name.starts_with("ih-") || st.hyps.iter().any(|(n, _, _)| *n == name) {
        return Err(stuck(&st, format!("`{name}` is already a hypothesis name")));
    }
    fn total_only(e: &Expr, defs: &Defs) -> Result<(), String> {
        if let ExprKind::Symbol(s) = &e.kind {
            if defs.fns.contains_key(s) && !defs.total.contains(s) {
                return Err(format!("`{s}` is not structurally recursive"));
            }
        }
        if let ExprKind::List(v) = &e.kind {
            match v.first().map(|h| &h.kind) {
                Some(ExprKind::Symbol(h))
                    if h == "match" || defs.is_ctor(h) || defs.total.contains(h) => {}
                _ => {
                    return Err(format!(
                        "`{e}` is not an application of a total fn of defs.oo"
                    ))
                }
            }
        }
        children(e)
            .into_iter()
            .try_for_each(|c| total_only(c, defs))
    }
    total_only(&expr, defs).map_err(|e| stuck(&st, format!("cases on a term: {e}")))?;
    let Some(ty) = super::infer::expr_type(defs, &st.types, &expr) else {
        return Err(stuck(&st, format!("cannot infer the type of `{expr}`")));
    };
    let Some(ctors) = defs.types.get(&ty.name) else {
        return Err(stuck(
            &st,
            format!(
                "`{expr}` has type `{}`, not an ADT declared in defs.oo",
                ty.name
            ),
        ));
    };
    let cases: Vec<&Expr> = items[4..].iter().collect();
    let mut used0 = used_symbols(&st);
    used0.insert(name.clone());
    for ctor in ctors {
        let case = cases.iter().find(|c| {
            head_sym(c).is_some_and(|h| h == ctor.name || h.eq_ignore_ascii_case(&ctor.name))
        });
        let mut used = used0.clone();
        let mut types = st.types.clone();
        let ftys = field_tys(defs, &ty, &ctor.fields);
        let mut fields = Vec::new();
        for (k, rec) in ctor.recursive.iter().enumerate() {
            let n = fresh_name(if *rec { "rest" } else { "x" }, &mut used);
            if let Some(Some(t)) = ftys.get(k) {
                types.insert(n.clone(), t.clone());
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
        let rp = |t: &Expr| normalize(&replace_all(t, &expr, &value), defs);
        let mut hyps = Vec::new();
        for (n, a, b) in &st.hyps {
            let mut fuel = 10_000;
            let a = eval_normalize(&normalize(a, defs), defs, &mut used, &mut fuel);
            let b = eval_normalize(&normalize(b, defs), defs, &mut used, &mut fuel);
            hyps.push((n.clone(), rp(&a), rp(&b)));
        }
        hyps.push((name.clone(), expr.clone(), value.clone()));
        let child = State {
            l: rp(&st.l),
            r: rp(&st.r),
            params: st.params.clone(),
            types,
            ihs: st.ihs.clone(),
            hyps,
            case: if st.case == "root" {
                ctor.name.clone()
            } else {
                format!("{}/{}", st.case, ctor.name)
            },
        };
        let Some(case) = case else {
            return Err(stuck(&child, format!("missing case {}", ctor.name)));
        };
        let ExprKind::List(cs) = &case.kind else {
            unreachable!()
        };
        run_steps(ctx, child, &cs[1..])?;
    }
    if let Some(extra) = cases.iter().find(|c| {
        !ctors
            .iter()
            .any(|k| head_sym(c).is_some_and(|h| h.eq_ignore_ascii_case(&k.name)))
    }) {
        return Err(stuck(
            &st,
            format!("case `{extra}` is not a constructor of {}", ty.name),
        ));
    }
    Ok(())
}
