//! Source-to-source CPS + handler-passing transform for *escaping* effect
//! handlers (the tier where a handler clause returns a function — e.g. the
//! pure `State` effect in `samples/state.oo`). The tail-resumptive and abort
//! tiers are handled directly in the backend; this pass only fires when an
//! escaping handle is present (`needs_cps`), so it can't affect other programs.
//!
//! It rewrites the program into ordinary Loon (closures + first-class calls,
//! which the backend already compiles), threading two extra arguments through
//! every effectful function:
//!   - `k`: the delimited continuation — a plain function `opresult -> answer`,
//!   - `h`: the handler dispatcher — `(op_tag, arg, resume) -> answer`.
//! An "answer" is whatever the handler clauses produce (for State, a function
//! `state -> result`). Effect operations consume the current continuation as
//! `resume` and invoke `h`; `handle` builds `h`/`return` from its clauses and
//! runs the delimited body. Continuations are ordinary closures, so they may be
//! invoked zero, one, or many times (multi-shot).

use crate::ast::{Expr, ExprKind};
use std::collections::HashMap;
use std::sync::atomic::{AtomicU32, Ordering};

static GENSYM: AtomicU32 = AtomicU32::new(0);

fn gensym(prefix: &str) -> String {
    let n = GENSYM.fetch_add(1, Ordering::Relaxed);
    format!("__cps_{prefix}{n}")
}

fn sym(s: &str) -> Expr {
    Expr::new(ExprKind::Symbol(s.to_string()), crate::syntax::Span::ZERO)
}
fn int(n: i64) -> Expr {
    Expr::new(ExprKind::Int(n), crate::syntax::Span::ZERO)
}
fn list(items: Vec<Expr>) -> Expr {
    Expr::new(ExprKind::List(items), crate::syntax::Span::ZERO)
}
/// `[do [let name val] body]` — bind `name` to `val`, then evaluate `body`.
/// (Loon's `let` is a statement; a bare `[let n v body]` would drop `body`.)
fn let_in(name: &str, val: Expr, body: Expr) -> Expr {
    list(vec![
        sym("do"),
        list(vec![sym("let"), sym(name), val]),
        body,
    ])
}
/// `[fn [params...] body...]`
fn lam(params: Vec<&str>, body: Vec<Expr>) -> Expr {
    let mut v = vec![sym("fn")];
    v.push(list(params.into_iter().map(sym).collect()));
    v.extend(body);
    list(v)
}

/// Is this a `[head ...]` form?
fn head_is(e: &Expr, name: &str) -> bool {
    matches!(&e.kind, ExprKind::List(items)
        if matches!(items.first().map(|x| &x.kind), Some(ExprKind::Symbol(s)) if s == name))
}

/// Whether the program contains an *escaping* handle (a handler clause whose
/// body is a `[fn …]`), which is what this transform handles.
pub fn needs_cps(exprs: &[Expr]) -> bool {
    fn walk(e: &Expr) -> bool {
        if let ExprKind::List(items) = &e.kind {
            if head_is(e, "handle") {
                // clauses are items[2..], in (pattern, body) pairs
                let mut i = 2;
                while i + 1 < items.len() {
                    if head_is(&items[i + 1], "fn") {
                        return true;
                    }
                    i += 2;
                }
            }
            return items.iter().any(walk);
        }
        false
    }
    exprs.iter().any(walk)
}

/// A delimited continuation under construction.
enum Cont<'a> {
    /// A variable already bound to a continuation closure.
    Var(String),
    /// An inlined continuation: given the result expression, build the rest.
    Fn(Box<dyn FnOnce(Expr) -> Expr + 'a>),
}

struct Cps {
    /// "Effect.op" -> small integer tag, shared between op sites and handlers.
    op_tags: HashMap<String, i64>,
}

impl Cps {
    fn op_tag(&mut self, key: &str) -> i64 {
        let n = self.op_tags.len() as i64;
        *self.op_tags.entry(key.to_string()).or_insert(n)
    }

    /// Apply a continuation to a (pure) value expression.
    fn apply(cont: Cont, value: Expr) -> Expr {
        match cont {
            Cont::Var(k) => list(vec![sym(&k), value]),
            Cont::Fn(f) => f(value),
        }
    }

    /// Materialize a continuation as a closure value `[fn [x] …]`.
    fn reify(self_cps: &mut Cps, cont: Cont, h: &str) -> Expr {
        match cont {
            Cont::Var(k) => sym(&k),
            Cont::Fn(_) => {
                let x = gensym("v");
                let body = Self::apply(cont, sym(&x));
                let _ = (self_cps, h);
                lam(vec![&x], vec![body])
            }
        }
    }

    /// CPS-transform expression `e` with continuation `cont` and handler var `h`.
    fn t(&mut self, e: &Expr, cont: Cont, h: &str) -> Expr {
        match &e.kind {
            ExprKind::Int(_)
            | ExprKind::Float(_)
            | ExprKind::Bool(_)
            | ExprKind::Str(_)
            | ExprKind::Keyword(_)
            | ExprKind::Symbol(_) => Self::apply(cont, e.clone()),

            ExprKind::List(items) if items.is_empty() => Self::apply(cont, e.clone()),

            ExprKind::List(items) => {
                let head = &items[0];
                if let ExprKind::Symbol(s) = &head.kind {
                    match s.as_str() {
                        "fn" => {
                            // A lambda value: CPS its body; it gains (k, h).
                            return Self::apply(cont, self.t_fn(items));
                        }
                        "let" => {
                            // [let x e1] is a statement; the rest follows in the
                            // enclosing sequence — handled by t_seq. A bare let
                            // here just binds for the continuation value.
                            // (Sequences route through t_seq, so this is rare.)
                            return self.t_seq(std::slice::from_ref(e), cont, h);
                        }
                        "do" => {
                            return self.t_seq(&items[1..], cont, h);
                        }
                        "if" => {
                            let c = &items[1];
                            let then_e = &items[2];
                            let else_e = items.get(3).cloned().unwrap_or_else(|| int(0));
                            // Reify the continuation once (used by both arms).
                            let kname = gensym("k");
                            let kval = Self::reify(self, cont, h);
                            let then_c = self.t(then_e, Cont::Var(kname.clone()), h);
                            let else_c = self.t(&else_e, Cont::Var(kname.clone()), h);
                            let cv = gensym("c");
                            let inner = list(vec![sym("if"), sym(&cv), then_c, else_c]);
                            let cond_cont = Cont::Fn(Box::new(move |cval: Expr| {
                                let_in(&cv, cval, inner)
                            }));
                            // wrap: [let kname kval] <cond>
                            let body = self.t(c, cond_cont, h);
                            return let_in(&kname, kval, body);
                        }
                        "handle" => {
                            return self.t_handle(items, cont, h);
                        }
                        "resume" => {
                            // In computation land resume shouldn't appear; clause
                            // bodies are emitted verbatim. Treat as a value call.
                        }
                        _ => {}
                    }
                    // Effect operation: Effect.op via DotAccess head is below;
                    // a bare-symbol head is a function/builtin call.
                }
                // DotAccess head => effect operation.
                if let ExprKind::DotAccess(obj, op) = &head.kind {
                    if let ExprKind::Symbol(effect) = &obj.kind {
                        if effect.starts_with(char::is_uppercase) {
                            return self.t_effect(effect, op, &items[1..], cont, h);
                        }
                    }
                }
                // Otherwise: an application. CPS the args, then call.
                self.t_app(items, cont, h)
            }

            _ => Self::apply(cont, e.clone()),
        }
    }

    /// CPS a `[fn [params] body…]` lambda value: it takes the original params
    /// plus a continuation `k` and handler `h`.
    fn t_fn(&mut self, items: &[Expr]) -> Expr {
        let params = match items.get(1).map(|e| &e.kind) {
            Some(ExprKind::List(p)) => p.clone(),
            _ => Vec::new(),
        };
        let k = gensym("k");
        let h = gensym("h");
        let body = self.t_seq(&items[2..], Cont::Var(k.clone()), &h);
        let mut all: Vec<Expr> = params;
        all.push(sym(&k));
        all.push(sym(&h));
        list(vec![sym("fn"), list(all), body])
    }

    /// CPS a statement sequence (function body / do): `let`s scope to the rest.
    fn t_seq(&mut self, stmts: &[Expr], cont: Cont, h: &str) -> Expr {
        if stmts.is_empty() {
            return Self::apply(cont, int(0));
        }
        if stmts.len() == 1 {
            return self.t(&stmts[0], cont, h);
        }
        let first = &stmts[0];
        let rest = &stmts[1..];
        // [let x e1] rest...  => CPS e1 with cont (fn [x] CPS[rest])
        if head_is(first, "let") {
            if let ExprKind::List(li) = &first.kind {
                // [let x v] or [let mut x v]
                let (ni, vi) = if matches!(li.get(1).map(|e| &e.kind), Some(ExprKind::Symbol(s)) if s == "mut")
                {
                    (2, 3)
                } else {
                    (1, 2)
                };
                let name = match li.get(ni).map(|e| &e.kind) {
                    Some(ExprKind::Symbol(s)) => s.clone(),
                    _ => gensym("ignored"),
                };
                let val = li[vi].clone();
                // Bind the rest under `let name = <result of val>`.
                let bound = self.t_seq(rest, cont, h);
                let rest_cont = move |xval: Expr| -> Expr { let_in(&name, xval, bound) };
                return self.t(&val, Cont::Fn(Box::new(rest_cont)), h);
            }
        }
        // Non-let statement: evaluate it (for effect), then the rest. The value
        // is sequenced via `do` so side effects aren't dropped.
        let bound = self.t_seq(rest, cont, h);
        let drop_cont = move |v: Expr| -> Expr { list(vec![sym("do"), v, bound]) };
        self.t(first, Cont::Fn(Box::new(drop_cont)), h)
    }

    /// CPS an effect operation `Effect.op args…`: the continuation becomes
    /// `resume`, and we invoke the dispatcher `h` with (tag, arg, resume).
    fn t_effect(&mut self, effect: &str, op: &str, args: &[Expr], cont: Cont, h: &str) -> Expr {
        let tag = self.op_tag(&format!("{effect}.{op}"));
        let resume = Self::reify(self, cont, h);
        // One-argument ops (get: none, put: one). Evaluate the arg purely first.
        if args.is_empty() {
            list(vec![sym(h), int(tag), int(0), resume])
        } else {
            // Evaluate arg with a continuation that performs the op.
            let h_owned = h.to_string();
            let arg_cont = move |av: Expr| -> Expr {
                list(vec![sym(&h_owned), int(tag), av, resume])
            };
            self.t(&args[0], Cont::Fn(Box::new(arg_cont)), h)
        }
    }

    /// CPS an application `[f args…]`. Evaluates `f` and each arg to atoms, then
    /// either calls a user/CPS function (passing the continuation + handler) or
    /// applies a pure builtin and feeds the result to the continuation.
    fn t_app(&mut self, items: &[Expr], cont: Cont, h: &str) -> Expr {
        let head = items[0].clone();
        // A "direct" call feeds its result straight to the continuation (no
        // resume/handler threading): pure builtins, and applications of a
        // *computed* value-function — e.g. `[[handle …] init]` applies the
        // handler's answer (a plain `state -> result`), and a continuation
        // applied to a value. A bare-symbol head names a CPS function (user fn
        // or thunk param), which takes the continuation + handler.
        let pure_builtin = matches!(&head.kind, ExprKind::Symbol(s) if is_pure_builtin(s))
            || !matches!(&head.kind, ExprKind::Symbol(_));
        // Evaluate all sub-expressions (head + args) to temporaries, innermost
        // continuation builds the actual call.
        let mut atoms: Vec<Expr> = Vec::new();
        self.t_app_args(&head, items, 0, &mut atoms, cont, h, pure_builtin)
    }

    #[allow(clippy::too_many_arguments)]
    fn t_app_args(
        &mut self,
        head: &Expr,
        items: &[Expr],
        idx: usize,
        atoms: &mut Vec<Expr>,
        cont: Cont,
        h: &str,
        pure_builtin: bool,
    ) -> Expr {
        if idx == items.len() {
            // All evaluated: build the call.
            if pure_builtin {
                let call = list(atoms.clone());
                return Self::apply(cont, call);
            }
            // User function (or computed head): pass continuation + handler.
            let resume = Self::reify(self, cont, h);
            let mut call = atoms.clone();
            call.push(resume);
            call.push(sym(h));
            return list(call);
        }
        // Evaluate items[idx] to a temp, recurse.
        let cur = items[idx].clone();
        if is_atom(&cur) {
            atoms.push(cur);
            // can't recurse with atoms borrowed in closure; do it directly
            return self.t_app_args(head, items, idx + 1, atoms, cont, h, pure_builtin);
        }
        let tmp = gensym("a");
        // Build the rest after binding tmp.
        let mut atoms_after = atoms.clone();
        atoms_after.push(sym(&tmp));
        let rest = self.t_app_args_owned(
            head.clone(),
            items.to_vec(),
            idx + 1,
            atoms_after,
            cont,
            h.to_string(),
            pure_builtin,
        );
        let bind = move |val: Expr| -> Expr { let_in(&tmp, val, rest) };
        self.t(&cur, Cont::Fn(Box::new(bind)), h)
    }

    #[allow(clippy::too_many_arguments)]
    fn t_app_args_owned(
        &mut self,
        head: Expr,
        items: Vec<Expr>,
        idx: usize,
        mut atoms: Vec<Expr>,
        cont: Cont,
        h: String,
        pure_builtin: bool,
    ) -> Expr {
        let _ = &head;
        self.t_app_args(&head.clone(), &items, idx, &mut atoms, cont, &h, pure_builtin)
    }

    /// CPS `[handle bodyexpr (pat hbody)…]`: build the dispatcher and return
    /// continuation from the clauses, then run the delimited body with them.
    fn t_handle(&mut self, items: &[Expr], cont: Cont, h_outer: &str) -> Expr {
        let body = &items[1];
        // Parse clauses.
        let mut ret_param: Option<String> = None;
        let mut ret_body: Option<Expr> = None;
        // dispatcher arms: (tag, op_param: Option<String>, clause_body Expr)
        let mut arms: Vec<(i64, Option<String>, Expr)> = Vec::new();
        let mut i = 2;
        while i + 1 < items.len() {
            let pat = &items[i];
            let hbody = &items[i + 1];
            i += 2;
            let parts = match &pat.kind {
                ExprKind::List(p) if !p.is_empty() => p,
                _ => continue,
            };
            if let ExprKind::Symbol(s) = &parts[0].kind {
                if s == "return" {
                    if let Some(ExprKind::Symbol(x)) = parts.get(1).map(|e| &e.kind) {
                        ret_param = Some(x.clone());
                        ret_body = Some(hbody.clone());
                    }
                    continue;
                }
            }
            if let ExprKind::DotAccess(obj, op) = &parts[0].kind {
                if let ExprKind::Symbol(effect) = &obj.kind {
                    let tag = self.op_tag(&format!("{effect}.{op}"));
                    let op_param = match parts.get(1).map(|e| &e.kind) {
                        Some(ExprKind::Symbol(s)) => Some(s.clone()),
                        _ => None,
                    };
                    arms.push((tag, op_param, hbody.clone()));
                }
            }
        }
        // Dispatcher: [fn [op arg resume] (if [= op t0] body0 (if ... 0))].
        // `resume` is renamed away from the reserved word the backend uses for
        // tail-resumptive handlers (it would otherwise be treated as identity).
        let op_v = "__op";
        let arg_v = "__arg";
        let resume_v = "__cps_resume";
        let mut dispatch = int(0);
        for (tag, op_param, clause_body) in arms.into_iter().rev() {
            // Rename `resume` -> resume_v and the op-param -> __arg in the body.
            let cb0 = subst(&clause_body, "resume", &sym(resume_v));
            let cb = match op_param {
                Some(p) => subst(&cb0, &p, &sym(arg_v)),
                None => cb0,
            };
            dispatch = list(vec![
                sym("if"),
                list(vec![sym("="), sym(op_v), int(tag)]),
                cb,
                dispatch,
            ]);
        }
        let h_name = gensym("h");
        let dispatcher = lam(vec![op_v, arg_v, resume_v], vec![dispatch]);
        // return continuation: [fn [x] ret_body]
        let ret_lam = match (ret_param, ret_body) {
            (Some(x), Some(rb)) => lam(vec![&x], vec![rb]),
            _ => {
                let x = gensym("x");
                lam(vec![&x], vec![sym(&x)])
            }
        };
        let ret_name = gensym("ret");
        // Delimited body run with (ret, h_name): CPS the handle body.
        let answer = self.t(body, Cont::Var(ret_name.clone()), &h_name);
        // [let h_name dispatcher] [let ret_name ret_lam] (apply cont answer)
        let _ = h_outer;
        let final_expr = Self::apply(cont, answer);
        let_in(&h_name, dispatcher, let_in(&ret_name, ret_lam, final_expr))
    }

    /// Transform a top-level `[fn name [params] body…]` into CPS form.
    fn t_defn(&mut self, items: &[Expr]) -> Expr {
        // items = [name, [params], body...]
        let name = items[0].clone();
        let params = match items.get(1).map(|e| &e.kind) {
            Some(ExprKind::List(p)) => p.clone(),
            _ => Vec::new(),
        };
        let k = gensym("k");
        let h = gensym("h");
        let body = self.t_seq(&items[2..], Cont::Var(k.clone()), &h);
        let mut all = params;
        all.push(sym(&k));
        all.push(sym(&h));
        list(vec![sym("fn"), name, list(all), body])
    }

    /// Transform `main`: run its body with a top-level identity continuation and
    /// a dummy handler (effects only occur inside `run-state`-style calls, which
    /// install their own handlers).
    fn t_main(&mut self, items: &[Expr]) -> Expr {
        let name = items[0].clone();
        let kid = gensym("k");
        let h = gensym("h");
        // identity continuation [fn [x] x]
        let body = self.t_seq(&items[2..], Cont::Var(kid.clone()), &h);
        let wrapped = let_in(&kid, lam(vec!["x"], vec![sym("x")]), let_in(&h, int(0), body));
        list(vec![sym("fn"), name, list(vec![]), wrapped])
    }
}

/// Pure builtins whose application is value-land (fed straight to the cont).
fn is_pure_builtin(s: &str) -> bool {
    matches!(
        s,
        "+" | "-" | "*" | "/" | "%" | "mod" | "=" | "!=" | "<" | ">" | "<=" | ">="
            | "not" | "and" | "or" | "inc" | "dec" | "abs" | "min" | "max"
            | "str" | "str-concat" | "println" | "print"
    )
}

fn is_atom(e: &Expr) -> bool {
    matches!(
        e.kind,
        ExprKind::Int(_)
            | ExprKind::Float(_)
            | ExprKind::Bool(_)
            | ExprKind::Str(_)
            | ExprKind::Keyword(_)
            | ExprKind::Symbol(_)
    )
}

/// Capture-free-ish substitution of a symbol `name` with `repl` (used only for
/// renaming an op parameter to the dispatcher's `arg`, which is hygienic here).
fn subst(e: &Expr, name: &str, repl: &Expr) -> Expr {
    match &e.kind {
        ExprKind::Symbol(s) if s == name => repl.clone(),
        ExprKind::List(items) => Expr::new(
            ExprKind::List(items.iter().map(|x| subst(x, name, repl)).collect()),
            e.span,
        ),
        ExprKind::Vec(items) => Expr::new(
            ExprKind::Vec(items.iter().map(|x| subst(x, name, repl)).collect()),
            e.span,
        ),
        ExprKind::Tuple(items) => Expr::new(
            ExprKind::Tuple(items.iter().map(|x| subst(x, name, repl)).collect()),
            e.span,
        ),
        _ => e.clone(),
    }
}

/// Transform a whole program, CPS-converting function definitions (and `main`).
/// `[effect …]` declarations are dropped (effect ops are gone after the pass).
pub fn transform(exprs: &[Expr]) -> Vec<Expr> {
    let mut cps = Cps {
        op_tags: HashMap::new(),
    };
    let mut out = Vec::new();
    for e in exprs {
        if let ExprKind::List(items) = &e.kind {
            if let Some(ExprKind::Symbol(s)) = items.first().map(|x| &x.kind) {
                if s == "effect" {
                    continue; // no longer needed
                }
                if s == "fn" && items.len() >= 3 {
                    let is_main = matches!(items.get(1).map(|x| &x.kind), Some(ExprKind::Symbol(n)) if n == "main");
                    if is_main {
                        out.push(cps.t_main(&items[1..]));
                    } else if matches!(items.get(1).map(|x| &x.kind), Some(ExprKind::Symbol(_))) {
                        out.push(cps.t_defn(&items[1..]));
                    } else {
                        out.push(e.clone());
                    }
                    continue;
                }
            }
        }
        out.push(e.clone());
    }
    out
}

