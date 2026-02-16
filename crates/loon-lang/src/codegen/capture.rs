use crate::ast::{Expr, ExprKind};
use std::collections::HashSet;

/// Whether a captured variable is borrowed or moved into the closure.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)]
pub enum CaptureMode {
    /// Variable is only read inside the closure.
    Ref,
    /// Variable is mutated, returned, or stored — ownership must transfer.
    Move,
}

/// Analyze free variables in a lambda body.
/// Returns the set of variables that are referenced but not bound
/// by the lambda's own params or inner let bindings.
pub fn free_vars(params: &[String], body: &[Expr]) -> HashSet<String> {
    let mut free = HashSet::new();
    let bound: HashSet<String> = params.iter().cloned().collect();
    for expr in body {
        collect_free(expr, &bound, &mut free);
    }
    free
}

/// Classify each captured (free) variable as Ref or Move.
///
/// A variable is classified as `Move` if it is:
/// - Mutated (via `set!` or mutation builtins like `push!`)
/// - Returned as the last expression in the closure body
/// - Stored into a data structure
///
/// Otherwise it is `Ref` (read-only borrow).
#[allow(dead_code)]
pub fn classify_captures(params: &[String], body: &[Expr]) -> Vec<(String, CaptureMode)> {
    let fv = free_vars(params, body);
    let mut mutated = HashSet::new();
    let mut escaped = HashSet::new();

    for expr in body {
        collect_mutated(expr, &mut mutated);
    }

    // The last expression in the body is the return value — if it's a free var, it escapes.
    if let Some(last) = body.last() {
        collect_escaped(last, &fv, &mut escaped);
    }

    fv.into_iter()
        .map(|name| {
            let mode = if mutated.contains(&name) || escaped.contains(&name) {
                CaptureMode::Move
            } else {
                CaptureMode::Ref
            };
            (name, mode)
        })
        .collect()
}

/// Collect names that are mutated (set!, push!, etc.).
fn collect_mutated(expr: &Expr, mutated: &mut HashSet<String>) {
    match &expr.kind {
        ExprKind::List(items) if !items.is_empty() => {
            if let ExprKind::Symbol(s) = &items[0].kind {
                match s.as_str() {
                    "set!" => {
                        if items.len() > 1 {
                            if let ExprKind::Symbol(name) = &items[1].kind {
                                mutated.insert(name.clone());
                            }
                        }
                    }
                    "push!" | "pop!" | "swap!" => {
                        if items.len() > 1 {
                            if let ExprKind::Symbol(name) = &items[1].kind {
                                mutated.insert(name.clone());
                            }
                        }
                    }
                    _ => {}
                }
            }
            for item in items {
                collect_mutated(item, mutated);
            }
        }
        ExprKind::Vec(items) | ExprKind::Set(items) | ExprKind::Tuple(items) => {
            for item in items {
                collect_mutated(item, mutated);
            }
        }
        ExprKind::Map(pairs) => {
            for (k, v) in pairs {
                collect_mutated(k, mutated);
                collect_mutated(v, mutated);
            }
        }
        _ => {}
    }
}

/// Collect free variables that "escape" — are returned or stored in a data structure.
fn collect_escaped(expr: &Expr, free: &HashSet<String>, escaped: &mut HashSet<String>) {
    match &expr.kind {
        ExprKind::Symbol(name) if free.contains(name) => {
            escaped.insert(name.clone());
        }
        ExprKind::Vec(items) | ExprKind::Tuple(items) => {
            for item in items {
                if let ExprKind::Symbol(name) = &item.kind {
                    if free.contains(name) {
                        escaped.insert(name.clone());
                    }
                }
            }
        }
        ExprKind::List(items) if !items.is_empty() => {
            // For a call expression, the return value depends on what the function does.
            // Conservatively: if the call is the tail expression, the result escapes,
            // but we can't easily tell which args escape. We focus on direct returns.
            // For `do` blocks, the last sub-expression is the return.
            if let ExprKind::Symbol(s) = &items[0].kind {
                if s == "do" || s == "let" {
                    if let Some(last) = items.last() {
                        collect_escaped(last, free, escaped);
                    }
                } else if s == "if" {
                    // Both branches can return
                    if items.len() >= 3 {
                        collect_escaped(&items[2], free, escaped);
                    }
                    if items.len() >= 4 {
                        collect_escaped(&items[3], free, escaped);
                    }
                }
            }
        }
        _ => {}
    }
}

fn collect_free(expr: &Expr, bound: &HashSet<String>, free: &mut HashSet<String>) {
    match &expr.kind {
        ExprKind::Symbol(name) => {
            if !bound.contains(name) {
                free.insert(name.clone());
            }
        }
        ExprKind::List(items) if !items.is_empty() => {
            // Check for special forms that introduce bindings
            if let ExprKind::Symbol(s) = &items[0].kind {
                match s.as_str() {
                    "let" => {
                        // [let name val body...] or [let mut name val body...]
                        let (name_idx, val_idx) = if items.len() > 2 {
                            if let ExprKind::Symbol(s) = &items[1].kind {
                                if s == "mut" {
                                    (2, 3)
                                } else {
                                    (1, 2)
                                }
                            } else {
                                (1, 2)
                            }
                        } else {
                            (1, 2)
                        };
                        // The val expression is in outer scope
                        if val_idx < items.len() {
                            collect_free(&items[val_idx], bound, free);
                        }
                        // The name is a new binding for subsequent expressions
                        if name_idx < items.len() {
                            if let ExprKind::Symbol(name) = &items[name_idx].kind {
                                let mut inner_bound = bound.clone();
                                inner_bound.insert(name.clone());
                                for item in &items[(val_idx + 1)..] {
                                    collect_free(item, &inner_bound, free);
                                }
                                return;
                            }
                        }
                    }
                    "fn" => {
                        // [fn [params] body...] — inner lambda
                        if items.len() >= 2 {
                            if let ExprKind::List(params) = &items[1].kind {
                                let mut inner_bound = bound.clone();
                                for p in params {
                                    if let ExprKind::Symbol(s) = &p.kind {
                                        inner_bound.insert(s.clone());
                                    }
                                }
                                for item in &items[2..] {
                                    collect_free(item, &inner_bound, free);
                                }
                                return;
                            }
                        }
                    }
                    "defn" => {
                        // Skip defn — it defines a name, not a free var reference
                        return;
                    }
                    _ => {}
                }
            }
            // Default: recurse into all children
            for item in items {
                collect_free(item, bound, free);
            }
        }
        ExprKind::Vec(items) | ExprKind::Set(items) | ExprKind::Tuple(items) => {
            for item in items {
                collect_free(item, bound, free);
            }
        }
        ExprKind::Map(pairs) => {
            for (k, v) in pairs {
                collect_free(k, bound, free);
                collect_free(v, bound, free);
            }
        }
        // Literals: no free vars
        ExprKind::Int(_) | ExprKind::Float(_) | ExprKind::Bool(_) | ExprKind::Str(_)
        | ExprKind::Keyword(_) => {}
        ExprKind::List(_) => {} // empty list handled above
        // Macro quasiquote nodes — recurse into inner expr
        ExprKind::Quote(inner) | ExprKind::Unquote(inner) | ExprKind::UnquoteSplice(inner) => {
            collect_free(inner, bound, free);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;

    #[test]
    fn captures_outer_var() {
        // [fn [x] [+ x offset]] — offset is free
        let src = "[fn [x] [+ x offset]]";
        let exprs = parse(src).unwrap();
        if let ExprKind::List(items) = &exprs[0].kind {
            if let ExprKind::List(params) = &items[1].kind {
                let param_names: Vec<String> = params
                    .iter()
                    .filter_map(|p| {
                        if let ExprKind::Symbol(s) = &p.kind {
                            Some(s.clone())
                        } else {
                            None
                        }
                    })
                    .collect();
                let fv = free_vars(&param_names, &items[2..]);
                assert!(fv.contains("offset"));
                assert!(!fv.contains("x"));
                // + is a builtin but captured as free var at codegen level
                assert!(fv.contains("+"));
            }
        }
    }

    #[test]
    fn no_free_vars() {
        let src = "[fn [x y] [+ x y]]";
        let exprs = parse(src).unwrap();
        if let ExprKind::List(items) = &exprs[0].kind {
            if let ExprKind::List(params) = &items[1].kind {
                let param_names: Vec<String> = params
                    .iter()
                    .filter_map(|p| {
                        if let ExprKind::Symbol(s) = &p.kind {
                            Some(s.clone())
                        } else {
                            None
                        }
                    })
                    .collect();
                let fv = free_vars(&param_names, &items[2..]);
                // Only + should be free (it's a builtin, not a local)
                assert!(!fv.contains("x"));
                assert!(!fv.contains("y"));
            }
        }
    }

    /// Helper: extract params and body from a parsed `[fn [params] body...]`.
    fn parse_lambda(src: &str) -> (Vec<String>, Vec<Expr>) {
        let exprs = parse(src).unwrap();
        if let ExprKind::List(items) = &exprs[0].kind {
            if let ExprKind::List(params) = &items[1].kind {
                let param_names: Vec<String> = params
                    .iter()
                    .filter_map(|p| {
                        if let ExprKind::Symbol(s) = &p.kind {
                            Some(s.clone())
                        } else {
                            None
                        }
                    })
                    .collect();
                return (param_names, items[2..].to_vec());
            }
        }
        panic!("not a lambda");
    }

    #[test]
    fn classify_read_only_capture_is_ref() {
        // [fn [x] [+ x offset]] — offset is only read
        let (params, body) = parse_lambda("[fn [x] [+ x offset]]");
        let captures = classify_captures(&params, &body);
        let offset = captures.iter().find(|(n, _)| n == "offset");
        assert_eq!(offset.map(|(_, m)| *m), Some(CaptureMode::Ref));
    }

    #[test]
    fn classify_returned_capture_is_move() {
        // [fn [x] offset] — offset is returned directly
        let (params, body) = parse_lambda("[fn [x] offset]");
        let captures = classify_captures(&params, &body);
        let offset = captures.iter().find(|(n, _)| n == "offset");
        assert_eq!(offset.map(|(_, m)| *m), Some(CaptureMode::Move));
    }

    #[test]
    fn classify_mutated_capture_is_move() {
        // [fn [] [set! counter [+ counter 1]]] — counter is mutated
        let (params, body) = parse_lambda("[fn [] [set! counter [+ counter 1]]]");
        let captures = classify_captures(&params, &body);
        let counter = captures.iter().find(|(n, _)| n == "counter");
        assert_eq!(counter.map(|(_, m)| *m), Some(CaptureMode::Move));
    }

    #[test]
    fn classify_pushed_capture_is_move() {
        // [fn [x] [push! items x]] — items is mutated via push!
        let (params, body) = parse_lambda("[fn [x] [push! items x]]");
        let captures = classify_captures(&params, &body);
        let items = captures.iter().find(|(n, _)| n == "items");
        assert_eq!(items.map(|(_, m)| *m), Some(CaptureMode::Move));
    }
}
