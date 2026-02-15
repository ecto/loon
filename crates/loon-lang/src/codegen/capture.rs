use crate::ast::{Expr, ExprKind};
use std::collections::HashSet;

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
}
