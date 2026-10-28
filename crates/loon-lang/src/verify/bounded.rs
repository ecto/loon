//! Bounded falsification: evaluate both sides of a law on small inputs in the
//! EIR VM and report the smallest counterexample.
//!
//! Both sides of every input are evaluated in one generated program by the
//! same backend (`eval_eir`), printed with `str`, and compared as text. The
//! reference definitions are renamed `vref-NAME` in that program because the
//! checker resolves builtin names (`map`, `reverse`, ...) ahead of user fns;
//! `builtin-NAME` in a bridge law is rewritten to the builtin `NAME`.

use super::kernel::{head_sym, list, subst, sym};
use super::Defs;
use crate::ast::{Expr, ExprKind};
use crate::syntax::Span;
use crate::types::Type;
use std::collections::HashMap;

/// Cap on the number of inputs evaluated per law.
pub const MAX_INPUTS: usize = 1500;
const ELEMS: [i64; 3] = [0, 1, 2];

#[derive(Clone, Debug)]
pub enum Kind {
    /// A list-shaped ADT: (type name, nil ctor, cons ctor)
    List(String, String, String),
    /// Any other ADT from defs.oo: (type name, [(ctor, recursive-field flags)]).
    /// Values are enumerated as trees of increasing node count.
    Adt(String, Vec<(String, Vec<bool>)>),
    /// An ADT whose type arguments are themselves ADTs (e.g. `List Instr`):
    /// the checker's type plus its values grouped by size, precomputed.
    Typed(Type, Vec<Vec<Expr>>),
    Int,
    Bool,
    Fn1,
    Unsupported(String),
}

#[derive(Debug, Default)]
pub struct Bounded {
    pub checked: usize,
    /// (assignment, lhs value, rhs value)
    pub counterexample: Option<(Vec<(String, Expr)>, String, String)>,
    pub error: Option<String>,
    /// Inputs skipped because they violate a hypothesis.
    pub vacuous: usize,
}

/// Rename reference fns to `vref-NAME`, and `builtin-NAME` to `NAME`.
pub fn mangle(e: &Expr, defs: &Defs) -> Expr {
    match &e.kind {
        ExprKind::Symbol(s) if defs.fns.contains_key(s) => sym(&format!("vref-{s}")),
        ExprKind::Symbol(s) if s.starts_with("builtin-") => sym(&s["builtin-".len()..]),
        _ => {
            let mut e2 = e.clone();
            mangle_children(&mut e2, defs);
            e2
        }
    }
}

fn mangle_children(e: &mut Expr, defs: &Defs) {
    use ExprKind::*;
    match &mut e.kind {
        List(v) | Vec(v) | Set(v) | Tuple(v) => {
            for c in v.iter_mut() {
                *c = mangle(c, defs);
            }
        }
        Map(ps) => {
            for (k, v) in ps.iter_mut() {
                *k = mangle(k, defs);
                *v = mangle(v, defs);
            }
        }
        Quote(x) | Unquote(x) | UnquoteSplice(x) | DotAccess(x, _) => {
            **x = mangle(x, defs);
        }
        _ => {}
    }
}

/// Infer each law parameter's kind by checking `[fn [params] #[lhs rhs]]`
/// against the evaluation prelude.
pub fn param_kinds(
    prelude_src: &str,
    params: &[String],
    eqs: &[(&Expr, &Expr)],
    defs: &Defs,
) -> Vec<Kind> {
    // `eqs[0]` is the conclusion; any further equations are hypotheses.
    let (lhs, rhs) = eqs[0];
    // Prefer verify's own inference (generalizes top-level fns); fall back
    // to the language checker when it cannot type the law (which then sees
    // only the conclusion).
    if let Some(ts) = super::infer::law_param_types_eqs(defs, &defs.fn_order, params, eqs) {
        return ts
            .iter()
            .map(|t| kind_of(&super::infer::to_checker(t), defs))
            .collect();
    }
    let Ok(prefix) = crate::parser::parse(prelude_src) else {
        return params
            .iter()
            .map(|_| Kind::Unsupported("prelude parse error".into()))
            .collect();
    };
    // Only the fns the law reaches: the checker does not generalize top-level
    // fns, so an unrelated use (`append` on instructions in `compile`) would
    // otherwise fix the element type of every list law.
    let prefix = reachable_forms(prefix, &[mangle(lhs, defs), mangle(rhs, defs)]);
    let mut checker = crate::check::Checker::new();
    checker.check_program(&prefix);
    let lam = list(vec![
        sym("fn"),
        list(params.iter().map(|p| sym(p)).collect()),
        Expr::new(
            ExprKind::Vec(vec![mangle(lhs, defs), mangle(rhs, defs)]),
            Span::ZERO,
        ),
    ]);
    // Round-trip through text so the checker sees fresh node ids and spans.
    let lam = crate::parser::parse(&format!("{lam}"))
        .ok()
        .and_then(|mut v| v.pop())
        .unwrap_or(lam);
    let ty = checker.infer(&lam);
    let ty = checker.resolve(&ty);
    let Type::Fn(ps, _, _) = ty else {
        return params
            .iter()
            .map(|_| Kind::Unsupported("could not infer types".into()))
            .collect();
    };
    let scrut: Vec<Option<String>> = params
        .iter()
        .map(|p| scrutinized_type(p, lhs, defs).or_else(|| scrutinized_type(p, rhs, defs)))
        .collect();
    // A param passed to a reference fn only in a non-scrutinized position
    // (e.g. `p` in `[mul [add a b] p]`) takes the law's ADT, when the law's
    // scrutinized params agree on exactly one non-list ADT.
    let mut adts: Vec<&String> = scrutinized_adts(&scrut, defs);
    adts.dedup();
    params
        .iter()
        .zip(&ps)
        .zip(&scrut)
        .map(|((p, t), sc)| match checker.resolve(t) {
            // Unconstrained by the checker: if the law scrutinizes it through
            // a reference fn, it has that fn's ADT type.
            Type::Var(_) => match sc {
                Some(ty) => kind_of(&Type::Con(ty.clone(), vec![Type::Int]), defs),
                None if adts.len() == 1 && (fn_arg(p, lhs, defs) || fn_arg(p, rhs, defs)) => {
                    kind_of(&Type::Con(adts[0].clone(), vec![]), defs)
                }
                None => Kind::Int,
            },
            t => kind_of(&t, defs),
        })
        .collect()
}

fn scrutinized_adts<'a>(scrut: &'a [Option<String>], defs: &Defs) -> Vec<&'a String> {
    let mut v: Vec<&String> = scrut
        .iter()
        .flatten()
        .filter(|t| {
            !matches!(
                kind_of(&Type::Con((*t).clone(), vec![]), defs),
                Kind::List(..)
            )
        })
        .collect();
    v.sort();
    v
}

/// Does `p` appear as a direct argument of a reference fn application?
fn fn_arg(p: &str, e: &Expr, defs: &Defs) -> bool {
    let ExprKind::List(items) = &e.kind else {
        return false;
    };
    if head_sym(e).is_some_and(|h| defs.fns.contains_key(h))
        && items[1..]
            .iter()
            .any(|a| matches!(&a.kind, ExprKind::Symbol(s) if s == p))
    {
        return true;
    }
    items.iter().any(|a| fn_arg(p, a, defs))
}

/// `type` forms, plus the `fn` forms transitively referenced from `roots`.
fn reachable_forms(forms: Vec<Expr>, roots: &[Expr]) -> Vec<Expr> {
    use std::collections::HashSet;
    let fn_name = |e: &Expr| -> Option<String> {
        match (&e.kind, head_sym(e)) {
            (ExprKind::List(v), Some("fn")) => match v.get(1).map(|x| &x.kind) {
                Some(ExprKind::Symbol(s)) => Some(s.clone()),
                _ => None,
            },
            _ => None,
        }
    };
    let mut want = HashSet::new();
    for r in roots {
        super::kernel::symbols(r, &mut want);
    }
    let mut keep: HashSet<String> = HashSet::new();
    loop {
        let mut grew = false;
        for f in &forms {
            if let Some(n) = fn_name(f) {
                if want.contains(&n) && keep.insert(n) {
                    super::kernel::symbols(f, &mut want);
                    grew = true;
                }
            }
        }
        if !grew {
            break;
        }
    }
    forms
        .into_iter()
        .filter(|f| fn_name(f).map_or(true, |n| keep.contains(&n)))
        .collect()
}

/// The ADT of `p` if it appears as the scrutinized argument of a reference fn.
fn scrutinized_type(p: &str, e: &Expr, defs: &Defs) -> Option<String> {
    if let (Some(h), ExprKind::List(items)) = (head_sym(e), &e.kind) {
        if let Some(f) = defs.fns.get(h) {
            if let Some(i) = f.scrutinee {
                if matches!(items.get(i + 1).map(|a| &a.kind), Some(ExprKind::Symbol(s)) if s == p)
                {
                    if let ExprKind::List(m) = &f.body.kind {
                        for pat in m[2..].iter().step_by(2) {
                            let c = match &pat.kind {
                                ExprKind::Symbol(c) => Some(c.as_str()),
                                _ => head_sym(pat),
                            };
                            if let Some(t) = c.and_then(|c| defs.type_of_ctor(c)) {
                                return Some(t);
                            }
                        }
                    }
                }
            }
        }
        for a in &items[1..] {
            if let Some(t) = scrutinized_type(p, a, defs) {
                return Some(t);
            }
        }
    }
    None
}

fn is_intish(t: &Type) -> bool {
    matches!(t, Type::Int | Type::Var(_))
}

fn kind_of(t: &Type, defs: &Defs) -> Kind {
    match t {
        Type::Int | Type::Var(_) => Kind::Int,
        Type::Bool => Kind::Bool,
        Type::Fn(ps, r, _) if ps.len() == 1 && is_intish(&ps[0]) && is_intish(r) => Kind::Fn1,
        Type::Con(name, args) if defs.types.contains_key(name) => {
            if args.iter().any(|a| !is_intish(a)) {
                let gs: Vec<Vec<Expr>> = (0..=TYPED_MAX_SIZE)
                    .map(|n| typed_values(t, n, defs, 0))
                    .collect();
                if gs.iter().all(|g| g.is_empty()) {
                    return Kind::Unsupported(format!("{name} of non-Int elements"));
                }
                return Kind::Typed(t.clone(), gs);
            }
            let ctors = &defs.types[name];
            let nil = ctors.iter().find(|c| c.recursive.is_empty());
            let cons = ctors
                .iter()
                .find(|c| c.recursive.len() == 2 && !c.recursive[0] && c.recursive[1]);
            match (ctors.len(), nil, cons) {
                (2, Some(n), Some(c)) => Kind::List(name.clone(), n.name.clone(), c.name.clone()),
                _ if nil.is_some() => Kind::Adt(
                    name.clone(),
                    ctors
                        .iter()
                        .map(|c| (c.name.clone(), c.recursive.clone()))
                        .collect(),
                ),
                _ => {
                    let gs: Vec<Vec<Expr>> = (0..=TYPED_MAX_SIZE)
                        .map(|n| typed_values(t, n, defs, 0))
                        .collect();
                    Kind::Typed(t.clone(), gs)
                }
            }
        }
        other => Kind::Unsupported(format!("{other:?}")),
    }
}

const TYPED_MAX_SIZE: usize = 6;
const TYPED_CAP: usize = 200;

/// All values (capped) of type `t` with exactly `n` non-nullary constructor
/// nodes. Ints and Bools are leaves of size 0; ints range over `ELEMS`.
/// Generic over any ADT in defs.oo, instantiating type parameters.
pub fn typed_values(t: &Type, n: usize, defs: &Defs, depth: usize) -> Vec<Expr> {
    if depth > 64 {
        return vec![];
    }
    match t {
        Type::Int | Type::Var(_) => {
            if n == 0 {
                ELEMS.iter().map(|e| super::kernel::int(*e)).collect()
            } else {
                vec![]
            }
        }
        Type::Bool => {
            if n == 0 {
                vec![
                    Expr::new(ExprKind::Bool(false), Span::ZERO),
                    Expr::new(ExprKind::Bool(true), Span::ZERO),
                ]
            } else {
                vec![]
            }
        }
        Type::Con(name, args) if defs.types.contains_key(name) => {
            let params = defs.type_params.get(name).cloned().unwrap_or_default();
            fn conv(f: &Expr, params: &[String], args: &[Type]) -> Option<Type> {
                match &f.kind {
                    ExprKind::Symbol(s) => match params.iter().position(|p| p == s) {
                        Some(i) => Some(args.get(i).cloned().unwrap_or(Type::Int)),
                        None if s == "Int" => Some(Type::Int),
                        None if s == "Bool" => Some(Type::Bool),
                        None => Some(Type::Con(s.clone(), vec![])),
                    },
                    ExprKind::List(v) | ExprKind::Tuple(v) => match v.first().map(|e| &e.kind) {
                        Some(ExprKind::Symbol(h)) => Some(Type::Con(
                            h.clone(),
                            v[1..]
                                .iter()
                                .map(|a| conv(a, params, args))
                                .collect::<Option<Vec<_>>>()?,
                        )),
                        _ => None,
                    },
                    _ => None,
                }
            }
            let mut out = Vec::new();
            for c in &defs.types[name] {
                if c.fields.is_empty() {
                    if n == 0 {
                        out.push(sym(&c.name));
                    }
                    continue;
                }
                if n == 0 {
                    continue;
                }
                let Some(ftys) = c
                    .fields
                    .iter()
                    .map(|f| conv(f, &params, args))
                    .collect::<Option<Vec<_>>>()
                else {
                    continue;
                };
                // Distribute n-1 over the fields, in field order.
                let mut partial: Vec<(Vec<Expr>, usize)> = vec![(vec![], n - 1)];
                for (fi, ft) in ftys.iter().enumerate() {
                    let last = fi + 1 == ftys.len();
                    let mut next = Vec::new();
                    'outer: for (vals, left) in &partial {
                        let sizes: Vec<usize> = if last {
                            vec![*left]
                        } else {
                            (0..=*left).collect()
                        };
                        for sz in sizes {
                            for v in typed_values(ft, sz, defs, depth + 1) {
                                let mut v2 = vals.clone();
                                v2.push(v);
                                next.push((v2, left - sz));
                                if next.len() >= TYPED_CAP {
                                    break 'outer;
                                }
                            }
                        }
                    }
                    partial = next;
                }
                for (vals, _) in partial {
                    if out.len() >= TYPED_CAP {
                        break;
                    }
                    let mut v = vec![sym(&c.name)];
                    v.extend(vals);
                    out.push(list(v));
                }
            }
            out
        }
        _ => vec![],
    }
}

/// Values of `kind` grouped by size 0..=bound.
fn groups(kind: &Kind, bound: usize, rng: &mut u64) -> Vec<Vec<Expr>> {
    let mut out = Vec::new();
    match kind {
        Kind::List(_, nil, cons) => {
            let mut layer: Vec<Expr> = vec![sym(nil)];
            out.push(layer.clone());
            for _ in 0..bound {
                let mut next = Vec::new();
                for e in ELEMS {
                    for tail in &layer {
                        next.push(list(vec![sym(cons), super::kernel::int(e), tail.clone()]));
                    }
                }
                out.push(next.clone());
                layer = next;
            }
        }
        Kind::Adt(_, ctors) => {
            // by_size[n] = all trees with exactly n constructor nodes (capped).
            const CAP: usize = 200;
            let mut by_size: Vec<Vec<Expr>> = vec![Vec::new()];
            for n in 1..=bound + 1 {
                let mut layer = Vec::new();
                for (name, flags) in ctors {
                    let recs = flags.iter().filter(|r| **r).count();
                    if recs == 0 {
                        if n != 1 {
                            continue;
                        }
                    } else if n < 1 + recs {
                        continue;
                    }
                    // distribute n-1 nodes over the recursive fields
                    let mut combos: Vec<Vec<Expr>> = vec![Vec::new()];
                    let mut left_opts: Vec<usize> = vec![n - 1];
                    for (fi, rec) in flags.iter().enumerate() {
                        let last_rec = flags[fi + 1..].iter().all(|r| !r);
                        let mut nc = Vec::new();
                        let mut nl = Vec::new();
                        for (c, left) in combos.iter().zip(&left_opts) {
                            if *rec {
                                let sizes: Vec<usize> = if last_rec {
                                    vec![*left]
                                } else {
                                    (1..=*left).collect()
                                };
                                for s in sizes {
                                    for v in by_size.get(s).into_iter().flatten() {
                                        let mut c2 = c.clone();
                                        c2.push(v.clone());
                                        nc.push(c2);
                                        nl.push(left - s);
                                    }
                                }
                            } else {
                                for e in ELEMS {
                                    let mut c2 = c.clone();
                                    c2.push(super::kernel::int(e));
                                    nc.push(c2);
                                    nl.push(*left);
                                }
                            }
                            if nc.len() >= CAP {
                                break;
                            }
                        }
                        combos = nc;
                        left_opts = nl;
                    }
                    for (c, left) in combos.into_iter().zip(left_opts) {
                        if left != 0 || layer.len() >= CAP {
                            continue;
                        }
                        layer.push(if c.is_empty() {
                            sym(name)
                        } else {
                            let mut v = vec![sym(name)];
                            v.extend(c);
                            list(v)
                        });
                    }
                }
                by_size.push(layer);
            }
            out.extend(by_size.into_iter().skip(1).filter(|g| !g.is_empty()));
        }
        Kind::Int => {
            out.push(vec![super::kernel::int(0)]);
            for k in 1..=bound as i64 {
                let r = (splitmix(rng) % 41) as i64 - 20;
                out.push(vec![
                    super::kernel::int(k),
                    super::kernel::int(-k),
                    super::kernel::int(r),
                ]);
            }
        }
        Kind::Bool => {
            out.push(vec![Expr::new(ExprKind::Bool(false), Span::ZERO)]);
            out.push(vec![Expr::new(ExprKind::Bool(true), Span::ZERO)]);
        }
        Kind::Fn1 => {
            let parse = |s: &str| crate::parser::parse(s).unwrap().pop().unwrap();
            out.push(vec![parse("[fn [x] [+ x 1]]")]);
            out.push(vec![parse("[fn [x] [* x 2]]"), parse("[fn [x] 0]")]);
        }
        Kind::Typed(_, gs) => {
            out.extend(gs.iter().take(bound + 1).cloned());
        }
        Kind::Unsupported(_) => {}
    }
    out
}

fn splitmix(state: &mut u64) -> u64 {
    *state = state.wrapping_add(0x9E37_79B9_7F4A_7C15);
    let mut z = *state;
    z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
    z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
    z ^ (z >> 31)
}

/// Inputs in order of increasing total size, capped at `MAX_INPUTS`.
fn inputs(kinds: &[Kind], bound: usize, seed: u64) -> Vec<Vec<Expr>> {
    let mut rng = seed;
    let gs: Vec<Vec<Vec<Expr>>> = kinds.iter().map(|k| groups(k, bound, &mut rng)).collect();
    if kinds.is_empty() {
        return vec![vec![]];
    }
    let max_total: usize = gs.iter().map(|g| g.len().saturating_sub(1)).sum();
    let mut out = Vec::new();
    for total in 0..=max_total {
        let mut sizes = vec![0usize; kinds.len()];
        compositions(&gs, total, 0, &mut sizes, &mut |sizes| {
            product(&gs, sizes, 0, &mut Vec::new(), &mut out);
        });
        if out.len() >= MAX_INPUTS {
            out.truncate(MAX_INPUTS);
            break;
        }
    }
    out
}

fn compositions(
    gs: &[Vec<Vec<Expr>>],
    left: usize,
    i: usize,
    sizes: &mut Vec<usize>,
    f: &mut dyn FnMut(&[usize]),
) {
    if i == gs.len() {
        if left == 0 {
            f(sizes);
        }
        return;
    }
    for s in 0..gs[i].len().min(left + 1) {
        sizes[i] = s;
        compositions(gs, left - s, i + 1, sizes, f);
    }
}

fn product(
    gs: &[Vec<Vec<Expr>>],
    sizes: &[usize],
    i: usize,
    cur: &mut Vec<Expr>,
    out: &mut Vec<Vec<Expr>>,
) {
    if out.len() >= MAX_INPUTS {
        return;
    }
    if i == gs.len() {
        out.push(cur.clone());
        return;
    }
    for v in &gs[i][sizes[i]] {
        cur.push(v.clone());
        product(gs, sizes, i + 1, cur, out);
        cur.pop();
    }
}

/// Evaluate a law on bounded inputs.
#[allow(clippy::too_many_arguments)]
pub fn check_law(
    prelude_src: &str,
    params: &[String],
    kinds: &[Kind],
    lhs: &Expr,
    rhs: &Expr,
    hyps: &[(Expr, Expr)],
    defs: &Defs,
    bound: usize,
    seed: u64,
) -> Bounded {
    if let Some((p, Kind::Unsupported(why))) = params
        .iter()
        .zip(kinds)
        .find(|(_, k)| matches!(k, Kind::Unsupported(_)))
    {
        return Bounded {
            error: Some(format!("cannot generate `{p}`: {why}")),
            ..Default::default()
        };
    }
    let ins = inputs(kinds, bound, seed);
    let (ml, mr) = (mangle(lhs, defs), mangle(rhs, defs));
    // Per input: each hypothesis's two sides, then the conclusion's.
    let mut pairs: Vec<(Expr, Expr)> = hyps
        .iter()
        .map(|(a, b)| (mangle(a, defs), mangle(b, defs)))
        .collect();
    pairs.push((ml, mr));
    let width = pairs.len() * 2;
    // Each chunk returns its printed values joined by `|` (values here are
    // ints, bools, vectors and ADTs, none of which print a `|`). The result
    // comes back as main's value, so nothing is written to stdout.
    let mut src = String::from(prelude_src);
    let mut chunk_names = Vec::new();
    for (ci, chunk) in ins.chunks(100).enumerate() {
        let name = format!("__law-chunk-{ci}");
        src.push_str(&format!("\n[fn {name} [] [str"));
        for input in chunk {
            let m: HashMap<String, Expr> =
                params.iter().cloned().zip(input.iter().cloned()).collect();
            for (a, b) in &pairs {
                src.push_str(&format!(
                    "\n  {} \"|\" {} \"|\"",
                    subst(a, &m, defs),
                    subst(b, &m, defs)
                ));
            }
        }
        src.push_str("]]");
        chunk_names.push(name);
    }
    src.push_str("\n[fn main [] [str");
    for n in &chunk_names {
        src.push_str(&format!(" [{n}]"));
    }
    src.push_str("]]\n");
    let run = || -> Result<String, crate::eir::vm::VmError> {
        let mut sess =
            crate::eir::vm::Session::new(&src, std::path::Path::new("."), Default::default())?;
        sess.start()?;
        Ok(sess.show(sess.value()))
    };
    let out = match run() {
        Ok(s) => s,
        Err(e) => {
            return Bounded {
                error: Some(format!("evaluation failed: {e}")),
                ..Default::default()
            }
        }
    };
    let lines: Vec<String> = out.split('|').map(str::to_string).collect();
    let lines = &lines[..lines.len().saturating_sub(1)];
    if lines.len() != ins.len() * width {
        return Bounded {
            error: Some(format!(
                "expected {} values, got {}",
                ins.len() * width,
                lines.len()
            )),
            ..Default::default()
        };
    }
    // An input violating a hypothesis is vacuous: skipped, counted apart.
    let (mut checked, mut vacuous) = (0, 0);
    for (i, input) in ins.iter().enumerate() {
        let vals = &lines[width * i..width * (i + 1)];
        if vals[..width - 2].chunks(2).any(|p| p[0] != p[1]) {
            vacuous += 1;
            continue;
        }
        checked += 1;
        let (l, r) = (&vals[width - 2], &vals[width - 1]);
        if l != r {
            return Bounded {
                checked,
                counterexample: Some((
                    params.iter().cloned().zip(input.iter().cloned()).collect(),
                    l.clone(),
                    r.clone(),
                )),
                error: None,
                vacuous,
            };
        }
    }
    Bounded {
        checked,
        vacuous,
        ..Default::default()
    }
}

/// Render a value term (literal input or printed output) as JSON: list-shaped
/// ADTs and vectors become arrays, ints numbers, bools booleans.
pub fn to_json(e: &Expr, defs: &Defs) -> serde_json::Value {
    use serde_json::Value as J;
    match &e.kind {
        ExprKind::Int(n) => J::from(*n),
        ExprKind::Bool(b) => J::Bool(*b),
        ExprKind::Symbol(s) if defs.is_ctor(s) && is_nil(s, defs) && is_list_type(s, defs) => {
            J::Array(vec![])
        }
        ExprKind::Vec(v) => J::Array(v.iter().map(|x| to_json(x, defs)).collect()),
        ExprKind::List(v) if v.len() == 3 && head_sym(e).is_some_and(|h| is_cons(h, defs)) => {
            let mut arr = vec![to_json(&v[1], defs)];
            match to_json(&v[2], defs) {
                J::Array(rest) => arr.extend(rest),
                _ => return J::String(format!("{e}")),
            }
            J::Array(arr)
        }
        _ => J::String(format!("{e}")),
    }
}

/// Is `ctor` a constructor of a list-shaped ADT (rendered as a JSON array)?
fn is_list_type(ctor: &str, defs: &Defs) -> bool {
    defs.types.values().any(|cs| {
        cs.iter().any(|c| c.name == ctor)
            && cs.len() == 2
            && cs
                .iter()
                .any(|c| c.recursive.len() == 2 && !c.recursive[0] && c.recursive[1])
    })
}

fn is_nil(s: &str, defs: &Defs) -> bool {
    defs.types
        .values()
        .flatten()
        .any(|c| c.name == s && c.recursive.is_empty())
}

fn is_cons(s: &str, defs: &Defs) -> bool {
    defs.types
        .values()
        .flatten()
        .any(|c| c.name == s && c.recursive.len() == 2 && c.recursive[1])
        && is_list_type(s, defs)
}

/// Parse a printed value back to a term for JSON rendering.
pub fn value_json(printed: &str, defs: &Defs) -> serde_json::Value {
    match crate::parser::parse(printed) {
        Ok(mut v) if v.len() == 1 => to_json(&v.pop().unwrap(), defs),
        _ => serde_json::Value::String(printed.to_string()),
    }
}
