//! Syntactic termination analysis backing the `Div` effect.
//!
//! `Div` (possible nontermination) is inferred like any other effect label.
//! This module only answers the syntactic questions the checker needs:
//!
//! - does a recursive body recurse *structurally* (every self-call / `recur`
//!   passes, in one fixed argument position, a variable bound as a strict
//!   subterm of the corresponding parameter by a `match` constructor
//!   pattern)?
//! - which top-level functions are mutually recursive (call-graph cycles of
//!   length ≥ 2)?
//!
//! Everything it cannot prove is reported as diverging; the analysis is
//! deliberately simple and conservative.

use crate::ast::{Expr, ExprKind};
use std::collections::{HashMap, HashSet};

#[derive(Clone, Default)]
struct Ctx {
    /// Unshadowed parameter names → position.
    params: HashMap<String, usize>,
    /// Variables known to be strict subterms of parameter `i`.
    subterm: HashMap<String, usize>,
    /// Inside a nested fn / loop: `recur` no longer targets us.
    inner: bool,
}

impl Ctx {
    fn unbind(&mut self, name: &str) {
        self.params.remove(name);
        self.subterm.remove(name);
    }
}

struct Walk<'a> {
    name: &'a str,
    /// Argument positions decreasing in every recursive call seen so far
    /// (`None` = no recursive call yet).
    candidates: Option<HashSet<usize>>,
    /// A self-reference we cannot reason about (escaping, wrong shape).
    bad: bool,
}

/// True when `body` — the body of a function (or `loop`) named `name` with
/// positional `params` — recurses (by name or via `recur`) in a way that is
/// not provably structural. Non-recursive bodies return false. Pass `""` as
/// `name` for anonymous fns and `loop`, where only `recur` re-enters.
pub fn recursion_diverges(name: &str, params: &[String], body: &[Expr]) -> bool {
    let mut ctx = Ctx::default();
    for (i, p) in params.iter().enumerate() {
        if !p.is_empty() {
            ctx.params.insert(p.clone(), i);
        }
    }
    let mut w = Walk {
        name,
        candidates: None,
        bad: false,
    };
    for e in body {
        w.expr(e, &mut ctx);
    }
    w.bad || w.candidates.is_some_and(|c| c.is_empty())
}

impl Walk<'_> {
    fn call(&mut self, args: &[Expr], ctx: &mut Ctx) {
        let dec: HashSet<usize> = args
            .iter()
            .enumerate()
            .filter(|(i, a)| match &a.kind {
                ExprKind::Symbol(s) => ctx.subterm.get(s) == Some(i),
                _ => false,
            })
            .map(|(i, _)| i)
            .collect();
        self.candidates = Some(match self.candidates.take() {
            None => dec,
            Some(c) => c.intersection(&dec).copied().collect(),
        });
        for a in args {
            self.expr(a, ctx);
        }
    }

    fn expr(&mut self, e: &Expr, ctx: &mut Ctx) {
        match &e.kind {
            ExprKind::Symbol(s) => {
                if !self.name.is_empty() && s == self.name {
                    // The function escapes as a value: its calls are invisible.
                    self.bad = true;
                }
            }
            ExprKind::List(items) if !items.is_empty() => {
                let head = match &items[0].kind {
                    ExprKind::Symbol(h) => h.as_str(),
                    _ => "",
                };
                match head {
                    h if !self.name.is_empty() && h == self.name => self.call(&items[1..], ctx),
                    "recur" if !ctx.inner => self.call(&items[1..], ctx),
                    "match" if items.len() >= 2 => self.match_form(&items[1..], ctx),
                    "fn" => self.fn_form(&items[1..], ctx),
                    "loop" if items.len() >= 2 => {
                        let mut inner = ctx.clone();
                        inner.inner = true;
                        if let ExprKind::List(binds) = &items[1].kind {
                            for pair in binds.chunks(2) {
                                if let Some(v) = pair.get(1) {
                                    self.expr(v, ctx);
                                }
                                for s in pattern_vars(&pair[0]) {
                                    inner.unbind(&s);
                                }
                            }
                        }
                        for b in &items[2..] {
                            self.expr(b, &mut inner);
                        }
                    }
                    "let" => {
                        for it in items.iter().skip(2) {
                            self.expr(it, ctx);
                        }
                        // Conservative: whatever `let` binds stops being a
                        // known param/subterm for the rest of this scope.
                        if let Some(p) = items.get(1) {
                            for s in pattern_vars(p) {
                                ctx.unbind(&s);
                            }
                        }
                    }
                    _ => {
                        for it in items {
                            self.expr(it, ctx);
                        }
                    }
                }
            }
            ExprKind::List(items)
            | ExprKind::Vec(items)
            | ExprKind::Set(items)
            | ExprKind::Tuple(items) => {
                for it in items {
                    self.expr(it, ctx);
                }
            }
            ExprKind::Map(pairs) => {
                for (k, v) in pairs {
                    self.expr(k, ctx);
                    self.expr(v, ctx);
                }
            }
            ExprKind::Quote(x) | ExprKind::Unquote(x) | ExprKind::UnquoteSplice(x) => {
                self.expr(x, ctx)
            }
            ExprKind::DotAccess(x, _) => self.expr(x, ctx),
            _ => {}
        }
    }

    fn fn_form(&mut self, rest: &[Expr], ctx: &Ctx) {
        let mut inner = ctx.clone();
        inner.inner = true;
        let mut rest = rest;
        if let Some(ExprKind::Symbol(n)) = rest.first().map(|e| &e.kind) {
            if n == self.name {
                // A nested definition shadowing us: not our recursion.
                return;
            }
            inner.unbind(n);
            rest = &rest[1..];
        }
        for part in rest {
            match &part.kind {
                ExprKind::List(ps) => {
                    for p in ps {
                        for s in pattern_vars(p) {
                            inner.unbind(&s);
                        }
                    }
                }
                ExprKind::Tuple(cl) => {
                    // multi-arity clause: ([params] body...)
                    let mut c = inner.clone();
                    if let Some(ExprKind::List(ps)) = cl.first().map(|e| &e.kind) {
                        for p in ps {
                            for s in pattern_vars(p) {
                                c.unbind(&s);
                            }
                        }
                    }
                    for b in cl.iter().skip(1) {
                        self.expr(b, &mut c);
                    }
                    continue;
                }
                _ => {}
            }
            self.expr(part, &mut inner);
        }
    }

    fn match_form(&mut self, rest: &[Expr], ctx: &mut Ctx) {
        let scrut = &rest[0];
        self.expr(scrut, ctx);
        let source = match &scrut.kind {
            ExprKind::Symbol(s) => ctx.params.get(s).or(ctx.subterm.get(s)).copied(),
            _ => None,
        };
        let arms = &rest[1..];
        let mut i = 0;
        while i < arms.len() {
            let pat = &arms[i];
            let mut arm = ctx.clone();
            for s in pattern_vars(pat) {
                arm.unbind(&s);
            }
            if let Some(p) = source {
                for s in strict_subterm_vars(pat) {
                    arm.subterm.insert(s, p);
                }
            }
            let guard = arms.get(i + 1).filter(|g| is_when(g)).is_some() && i + 2 < arms.len();
            if guard {
                self.expr(&arms[i + 1], &mut arm);
                self.expr(&arms[i + 2], &mut arm);
                i += 3;
            } else {
                if let Some(b) = arms.get(i + 1) {
                    self.expr(b, &mut arm);
                }
                i += 2;
            }
        }
    }
}

fn is_when(e: &Expr) -> bool {
    matches!(&e.kind, ExprKind::List(xs)
        if matches!(xs.first().map(|h| &h.kind), Some(ExprKind::Symbol(s)) if s == "when"))
}

fn is_binder(s: &str) -> bool {
    s != "_" && s != "&" && s.starts_with(|c: char| c.is_lowercase() || c == '_')
}

/// Every variable a pattern binds.
fn pattern_vars(p: &Expr) -> Vec<String> {
    let mut out = Vec::new();
    collect(p, true, &mut out);
    out
}

/// Variables bound strictly *inside* a constructor / vector pattern.
fn strict_subterm_vars(p: &Expr) -> Vec<String> {
    let mut out = Vec::new();
    match &p.kind {
        ExprKind::List(items) => {
            for it in items.iter().skip(1) {
                collect(it, true, &mut out);
            }
        }
        ExprKind::Vec(items) => {
            for it in items {
                collect(it, true, &mut out);
            }
        }
        _ => {}
    }
    out
}

fn collect(p: &Expr, top: bool, out: &mut Vec<String>) {
    match &p.kind {
        ExprKind::Symbol(s) if is_binder(s) => out.push(s.clone()),
        ExprKind::List(items) => {
            // Head is the constructor name; skip it.
            for it in items.iter().skip(1) {
                collect(it, false, out);
            }
        }
        ExprKind::Vec(items) | ExprKind::Tuple(items) | ExprKind::Set(items) => {
            for it in items {
                collect(it, false, out);
            }
        }
        ExprKind::Map(pairs) => {
            for (_, v) in pairs {
                collect(v, false, out);
            }
        }
        _ => {}
    }
    let _ = top;
}

/// All symbols mentioned anywhere in `exprs` (a cheap call-graph edge set).
pub fn referenced_symbols(exprs: &[Expr]) -> HashSet<String> {
    fn go(e: &Expr, out: &mut HashSet<String>) {
        match &e.kind {
            ExprKind::Symbol(s) => {
                out.insert(s.clone());
            }
            ExprKind::List(xs) | ExprKind::Vec(xs) | ExprKind::Set(xs) | ExprKind::Tuple(xs) => {
                xs.iter().for_each(|x| go(x, out))
            }
            ExprKind::Map(ps) => ps.iter().for_each(|(k, v)| {
                go(k, out);
                go(v, out)
            }),
            ExprKind::Quote(x) | ExprKind::Unquote(x) | ExprKind::UnquoteSplice(x) => go(x, out),
            ExprKind::DotAccess(x, field) => {
                if let ExprKind::Symbol(obj) = &x.kind {
                    out.insert(format!("{obj}.{field}"));
                }
                go(x, out)
            }
            _ => {}
        }
    }
    let mut out = HashSet::new();
    exprs.iter().for_each(|e| go(e, &mut out));
    out
}

/// Top-level function facts used to make forward references conservative.
pub struct ProgramFacts {
    /// Functions on a call-graph cycle through at least one other function.
    pub mutual: HashSet<String>,
    /// Functions that may diverge by a purely syntactic over-approximation
    /// (non-structural self recursion, mutual recursion, a `loop`, or
    /// referencing another such function).
    pub may_diverge: HashSet<String>,
}

/// Analyse the top-level `[fn name ...]` forms of a program.
pub fn program_facts(exprs: &[Expr]) -> ProgramFacts {
    let mut defs: Vec<(String, &[Expr])> = Vec::new();
    for e in exprs {
        if let ExprKind::List(items) = &e.kind {
            if let (Some(ExprKind::Symbol(h)), Some(ExprKind::Symbol(n))) = (
                items.first().map(|x| &x.kind),
                items.get(1).map(|x| &x.kind),
            ) {
                if h == "fn" && items.len() >= 3 {
                    defs.push((n.clone(), &items[2..]));
                }
            }
        }
    }
    let names: HashSet<String> = defs.iter().map(|(n, _)| n.clone()).collect();
    let mut edges: HashMap<String, HashSet<String>> = HashMap::new();
    let mut may_diverge = HashSet::new();
    for (n, rest) in &defs {
        let refs = referenced_symbols(rest);
        if refs.contains("loop") || refs.contains("Async.loop") || fn_self_diverges(n, rest) {
            may_diverge.insert(n.clone());
        }
        edges.insert(
            n.clone(),
            refs.into_iter()
                .filter(|r| names.contains(r) && r != n)
                .collect(),
        );
    }
    let reach = |from: &str| -> HashSet<String> {
        let mut seen = HashSet::new();
        let mut stack = vec![from.to_string()];
        while let Some(x) = stack.pop() {
            for y in edges.get(&x).into_iter().flatten() {
                if seen.insert(y.clone()) {
                    stack.push(y.clone());
                }
            }
        }
        seen
    };
    let mut mutual = HashSet::new();
    for (n, _) in &defs {
        if reach(n).contains(n) {
            mutual.insert(n.clone());
        }
    }
    may_diverge.extend(mutual.iter().cloned());
    loop {
        let before = may_diverge.len();
        for (n, es) in &edges {
            if es.iter().any(|g| may_diverge.contains(g)) {
                may_diverge.insert(n.clone());
            }
        }
        if may_diverge.len() == before {
            break;
        }
    }
    ProgramFacts {
        mutual,
        may_diverge,
    }
}

/// Self-recursion check over the tail of a `[fn name ...]` form (params,
/// optional effect annotation, body — or multi-arity clauses).
pub fn fn_self_diverges(name: &str, rest: &[Expr]) -> bool {
    match rest.first().map(|e| &e.kind) {
        Some(ExprKind::List(params)) => {
            let ps = param_names(params);
            let mut body = &rest[1..];
            if matches!(body.first().map(|e| &e.kind), Some(ExprKind::Set(_))) && body.len() > 1 {
                body = &body[1..];
            }
            recursion_diverges(name, &ps, body)
        }
        // Multi-arity: clauses call each other by name; stay conservative.
        _ => referenced_symbols(rest).contains(name) || referenced_symbols(rest).contains("recur"),
    }
}

/// Positional parameter names (`""` for destructuring params, stopping at `&`).
pub fn param_names(params: &[Expr]) -> Vec<String> {
    params
        .iter()
        .take_while(|p| !matches!(&p.kind, ExprKind::Symbol(s) if s == "&"))
        .map(|p| match &p.kind {
            ExprKind::Symbol(s) => s.clone(),
            _ => String::new(),
        })
        .collect()
}
