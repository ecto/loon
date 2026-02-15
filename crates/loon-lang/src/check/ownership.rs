use crate::ast::{Expr, ExprKind, NodeId};
use crate::syntax::Span;
use crate::types::{Subst, Type};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct OwnershipError {
    pub message: String,
    pub span: Span,
    pub why: String,
    pub fix: String,
}

impl std::fmt::Display for OwnershipError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "ownership error at {}..{}: {}\n  why: {}\n  fix: {}",
            self.span.start, self.span.end, self.message, self.why, self.fix
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum BindingState {
    Alive,
    Moved,
    MutBorrowed,
}

#[derive(Debug, Clone)]
struct Binding {
    state: BindingState,
    defined_at: Span,
    moved_at: Option<Span>,
    is_copy: bool,
    is_mut: bool,
}

pub struct OwnershipChecker<'a> {
    scopes: Vec<HashMap<String, Binding>>,
    pub errors: Vec<OwnershipError>,
    /// Functions known to only borrow (not move) their args
    borrow_fns: std::collections::HashSet<String>,
    /// Types known to be Copy
    copy_types: std::collections::HashSet<String>,
    /// Type side-table from the type checker
    type_of: Option<&'a HashMap<NodeId, Type>>,
    /// Substitution for resolving type variables
    subst: Option<&'a Subst>,
}

impl<'a> OwnershipChecker<'a> {
    pub fn new() -> Self {
        let mut borrow_fns = std::collections::HashSet::new();
        // Builtins that only read/borrow their arguments
        for name in [
            "println", "print", "str", "len", "nth", "get", "contains?", "empty?", "+", "-", "*",
            ">", "<", ">=", "<=", "=", "not", "and", "or",
        ] {
            borrow_fns.insert(name.to_string());
        }
        let mut copy_types = std::collections::HashSet::new();
        for ty in ["Int", "Float", "Bool", "Keyword"] {
            copy_types.insert(ty.to_string());
        }
        Self {
            scopes: vec![HashMap::new()],
            errors: Vec::new(),
            borrow_fns,
            copy_types,
            type_of: None,
            subst: None,
        }
    }

    /// Create an ownership checker with type information from the type checker.
    pub fn with_type_info(type_of: &'a HashMap<NodeId, Type>, subst: &'a Subst) -> Self {
        let mut checker = Self::new();
        checker.type_of = Some(type_of);
        checker.subst = Some(subst);
        checker
    }

    /// Check if a type is a Copy type.
    fn is_copy_type(&self, ty: &Type) -> bool {
        match ty {
            Type::Int | Type::Float | Type::Bool | Type::Keyword => true,
            Type::Con(name, _) => self.copy_types.contains(name.as_str()),
            _ => false,
        }
    }

    /// Look up whether the value expression for a binding is a Copy type.
    fn is_value_copy(&self, expr: &Expr) -> bool {
        if let (Some(type_of), Some(subst)) = (self.type_of, self.subst) {
            if let Some(ty) = type_of.get(&expr.id) {
                let resolved = subst.resolve(ty);
                return self.is_copy_type(&resolved);
            }
        }
        // Without type info, conservatively assume non-copy
        false
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn define(&mut self, name: String, span: Span, is_copy: bool, is_mut: bool) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(
                name,
                Binding {
                    state: BindingState::Alive,
                    defined_at: span,
                    moved_at: None,
                    is_copy,
                    is_mut,
                },
            );
        }
    }

    fn get_binding(&self, name: &str) -> Option<&Binding> {
        for scope in self.scopes.iter().rev() {
            if let Some(b) = scope.get(name) {
                return Some(b);
            }
        }
        None
    }

    fn get_binding_mut(&mut self, name: &str) -> Option<&mut Binding> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(b) = scope.get_mut(name) {
                return Some(b);
            }
        }
        None
    }

    fn use_binding(&mut self, name: &str, span: Span) {
        if let Some(binding) = self.get_binding(name) {
            if binding.state == BindingState::Moved {
                let defined = binding.defined_at;
                let moved = binding.moved_at.unwrap_or(defined);
                self.errors.push(OwnershipError {
                    message: format!("use of moved value '{name}'"),
                    span,
                    why: format!(
                        "'{name}' was moved at {}..{} and can no longer be used",
                        moved.start, moved.end
                    ),
                    fix: format!("clone '{name}' before moving, or restructure to avoid the move"),
                });
            }
        }
    }

    fn move_binding(&mut self, name: &str, span: Span) {
        if let Some(binding) = self.get_binding(name) {
            if binding.is_copy {
                return; // Copy types don't move
            }
            if binding.state == BindingState::Moved {
                let defined = binding.defined_at;
                let moved = binding.moved_at.unwrap_or(defined);
                self.errors.push(OwnershipError {
                    message: format!("use of moved value '{name}'"),
                    span,
                    why: format!(
                        "'{name}' was already moved at {}..{}",
                        moved.start, moved.end
                    ),
                    fix: format!("clone '{name}' before the first move"),
                });
                return;
            }
        }
        if let Some(binding) = self.get_binding_mut(name) {
            if !binding.is_copy {
                binding.state = BindingState::Moved;
                binding.moved_at = Some(span);
            }
        }
    }

    fn mut_borrow(&mut self, name: &str, span: Span) {
        if let Some(binding) = self.get_binding(name) {
            if !binding.is_mut {
                self.errors.push(OwnershipError {
                    message: format!("cannot mutably borrow immutable binding '{name}'"),
                    span,
                    why: "only bindings declared with [let mut ...] can be mutably borrowed"
                        .to_string(),
                    fix: format!("declare '{name}' as [let mut {name} ...]"),
                });
                return;
            }
            if binding.state == BindingState::MutBorrowed {
                self.errors.push(OwnershipError {
                    message: format!("cannot borrow '{name}' as mutable more than once"),
                    span,
                    why: "Loon prevents aliased mutable references to eliminate data races"
                        .to_string(),
                    fix: "ensure the first mutable borrow is no longer in use".to_string(),
                });
            }
        }
        if let Some(binding) = self.get_binding_mut(name) {
            binding.state = BindingState::MutBorrowed;
        }
    }

    pub fn check_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Symbol(name) => {
                self.use_binding(name, expr.span);
            }
            ExprKind::List(items) if !items.is_empty() => {
                self.check_list(items, expr.span);
            }
            ExprKind::Vec(items) | ExprKind::Set(items) | ExprKind::Tuple(items) => {
                for item in items {
                    self.check_expr(item);
                }
            }
            ExprKind::Map(pairs) => {
                for (k, v) in pairs {
                    self.check_expr(k);
                    self.check_expr(v);
                }
            }
            _ => {}
        }
    }

    fn check_list(&mut self, items: &[Expr], _span: Span) {
        if items.is_empty() {
            return;
        }
        let head = &items[0];
        if let ExprKind::Symbol(s) = &head.kind {
            match s.as_str() {
                "defn" => {
                    self.check_defn(&items[1..]);
                    return;
                }
                "let" => {
                    self.check_let(&items[1..]);
                    return;
                }
                "fn" => {
                    self.check_fn_body(&items[1..]);
                    return;
                }
                "if" => {
                    for item in &items[1..] {
                        self.check_expr(item);
                    }
                    return;
                }
                "do" => {
                    for item in &items[1..] {
                        self.check_expr(item);
                    }
                    return;
                }
                "match" | "|>" | "type" | "test" | "pub" | "trait" | "impl" | "sig" => {
                    for item in &items[1..] {
                        self.check_expr(item);
                    }
                    return;
                }
                "push!" => {
                    // push! requires mutable borrow of first arg
                    if items.len() > 1 {
                        if let ExprKind::Symbol(name) = &items[1].kind {
                            self.mut_borrow(name, items[1].span);
                        }
                    }
                    for item in &items[2..] {
                        self.check_expr(item);
                    }
                    return;
                }
                name if self.borrow_fns.contains(name) => {
                    // These builtins only borrow
                    for item in &items[1..] {
                        self.check_expr(item);
                    }
                    return;
                }
                _ => {}
            }
        }

        // Generic function call — head is borrowed, args are moved
        self.check_expr(&items[0]);
        for item in &items[1..] {
            if let ExprKind::Symbol(ref name) = item.kind {
                self.move_binding(name, item.span);
            } else {
                self.check_expr(item);
            }
        }
    }

    fn check_defn(&mut self, args: &[Expr]) {
        if args.len() < 2 {
            return;
        }
        // Check function body in a new scope
        let mut body_start = 2;
        if body_start < args.len() {
            if let ExprKind::Symbol(s) = &args[body_start].kind {
                if s == "/" {
                    body_start += 2;
                }
            }
        }

        // Register params with type-based Copy detection
        if let ExprKind::List(params) = &args[1].kind {
            self.push_scope();
            for p in params {
                if let ExprKind::Symbol(name) = &p.kind {
                    let is_copy = self.is_value_copy(p);
                    self.define(name.clone(), p.span, is_copy, false);
                }
            }
            for expr in &args[body_start..] {
                self.check_expr(expr);
            }
            self.pop_scope();
        }
    }

    fn check_let(&mut self, args: &[Expr]) {
        if args.len() < 2 {
            return;
        }
        let (binding, val_idx, is_mut) =
            if matches!(&args[0].kind, ExprKind::Symbol(s) if s == "mut") {
                if args.len() < 3 {
                    return;
                }
                (&args[1], 2, true)
            } else {
                (&args[0], 1, false)
            };

        // Check the value expression first
        if val_idx < args.len() {
            self.check_expr(&args[val_idx]);
        }

        // Register the binding, using type info for Copy detection
        if let ExprKind::Symbol(name) = &binding.kind {
            let is_copy = if val_idx < args.len() {
                self.is_value_copy(&args[val_idx])
            } else {
                false
            };
            self.define(name.clone(), binding.span, is_copy, is_mut);
        }
    }

    fn check_fn_body(&mut self, args: &[Expr]) {
        if args.is_empty() {
            return;
        }
        if let ExprKind::List(params) = &args[0].kind {
            self.push_scope();
            for p in params {
                if let ExprKind::Symbol(name) = &p.kind {
                    let is_copy = self.is_value_copy(p);
                    self.define(name.clone(), p.span, is_copy, false);
                }
            }
            for expr in &args[1..] {
                self.check_expr(expr);
            }
            self.pop_scope();
        }
    }

    pub fn check_program(&mut self, exprs: &[Expr]) -> Vec<OwnershipError> {
        for expr in exprs {
            self.check_expr(expr);
        }
        std::mem::take(&mut self.errors)
    }
}

impl Default for OwnershipChecker<'_> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;

    fn check(src: &str) -> Vec<OwnershipError> {
        let exprs = parse(src).unwrap();
        let mut checker = OwnershipChecker::new();
        checker.check_program(&exprs)
    }

    fn check_with_types(src: &str) -> Vec<OwnershipError> {
        let exprs = parse(src).unwrap();
        let mut type_checker = crate::check::Checker::new();
        for expr in &exprs {
            type_checker.infer(expr);
        }
        let mut checker = OwnershipChecker::with_type_info(&type_checker.type_of, &type_checker.subst);
        checker.check_program(&exprs)
    }

    #[test]
    fn no_error_simple() {
        let errors = check(r#"[let x 42] [println x]"#);
        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
    }

    #[test]
    fn use_after_move() {
        let errors = check(
            r#"
            [defn take [s] s]
            [defn main []
              [let name "alice"]
              [take name]
              [println name]]
        "#,
        );
        // `take` is not in borrow_fns, so `name` gets moved, then used
        assert!(
            errors.iter().any(|e| e.message.contains("moved")),
            "expected use-after-move error, got: {:?}",
            errors
        );
    }

    #[test]
    fn mut_borrow_immutable() {
        let errors = check(
            r#"
            [defn main []
              [let v #[1 2 3]]
              [push! v 4]]
        "#,
        );
        assert!(
            errors.iter().any(|e| e.message.contains("immutable")),
            "expected immutable borrow error, got: {:?}",
            errors
        );
    }

    #[test]
    fn mut_borrow_ok() {
        let errors = check(
            r#"
            [defn main []
              [let mut v #[1 2 3]]
              [push! v 4]]
        "#,
        );
        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
    }

    #[test]
    fn copy_type_no_move_error() {
        // Int is Copy, so using x after passing to a function should be fine
        let errors = check_with_types(
            r#"
            [defn take [s] s]
            [let x 42]
            [take x]
            [println x]
        "#,
        );
        assert!(errors.is_empty(), "Int is Copy, should not error: {:?}", errors);
    }

    #[test]
    fn non_copy_type_move_error() {
        // Vec is not Copy, so using v after passing to a function should error
        let errors = check_with_types(
            r#"
            [defn consume [v] v]
            [let v #[1 2 3]]
            [consume v]
            [println v]
        "#,
        );
        assert!(
            errors.iter().any(|e| e.message.contains("moved")),
            "Vec is not Copy, should error: {:?}",
            errors
        );
    }
}
