use crate::ast::{Expr, ExprKind, NodeId};
use crate::errors::codes::ErrorCode;
use crate::errors::{LoonDiagnostic, OwnershipDiagram};
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

/// How a function uses a particular parameter.
#[derive(Debug, Clone, Copy, PartialEq)]
enum ParamMode {
    /// Parameter is only read — immutable borrow at call site.
    Borrow,
    /// Parameter is mutated (push!, set!) — mutable borrow at call site.
    MutBorrow,
    /// Parameter escapes (returned, stored in data structure) — move at call site.
    Move,
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
    pub errors: Vec<LoonDiagnostic>,
    /// Functions known to only borrow (not move) their args
    borrow_fns: std::collections::HashSet<String>,
    /// Types known to be Copy
    copy_types: std::collections::HashSet<String>,
    /// Type side-table from the type checker
    type_of: Option<&'a HashMap<NodeId, Type>>,
    /// Substitution for resolving type variables
    subst: Option<&'a Subst>,
    /// Per-parameter borrow/move modes for analyzed user-defined functions
    fn_param_modes: HashMap<String, Vec<ParamMode>>,
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
            fn_param_modes: HashMap::new(),
        }
    }

    /// Create an ownership checker with type information from the type checker.
    pub fn with_type_info(type_of: &'a HashMap<NodeId, Type>, subst: &'a Subst) -> Self {
        let mut checker = Self::new();
        checker.type_of = Some(type_of);
        checker.subst = Some(subst);
        checker
    }

    /// Register additional user-defined Copy types (from `[derive Copy ...]`).
    pub fn with_derived_copy_types(mut self, types: &std::collections::HashSet<String>) -> Self {
        for t in types {
            self.copy_types.insert(t.clone());
        }
        self
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

    /// Build an ownership diagram showing the lifecycle of a binding.
    fn make_move_diagram(&self, name: &str, defined: Span, moved: Span, used: Span) -> OwnershipDiagram {
        OwnershipDiagram {
            lines: vec![
                format!("  {name} defined at {}..{}", defined.start, defined.end),
                format!("  {name} moved   at {}..{}  -- ownership transferred", moved.start, moved.end),
                format!("  {name} used    at {}..{}  -- ERROR: value no longer available", used.start, used.end),
            ],
        }
    }

    fn use_binding(&mut self, name: &str, span: Span) {
        if let Some(binding) = self.get_binding(name) {
            if binding.state == BindingState::Moved {
                let defined = binding.defined_at;
                let moved = binding.moved_at.unwrap_or(defined);
                let why_msg = format!(
                    "'{name}' was moved at {}..{} and can no longer be used",
                    moved.start, moved.end
                );
                let fix_msg = format!("clone '{name}' before moving, or restructure to avoid the move");
                let diagram = self.make_move_diagram(name, defined, moved, span);
                self.errors.push(
                    LoonDiagnostic::new(ErrorCode::E0300, format!("use of moved value '{name}'"))
                        .with_why(&why_msg)
                        .with_fix(&fix_msg)
                        .with_label(span, format!("'{name}' used after move"), true)
                        .with_label(moved, format!("'{name}' moved here"), false)
                        .with_ownership_diagram(diagram),
                );
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
                let diagram = self.make_move_diagram(name, defined, moved, span);
                self.errors.push(
                    LoonDiagnostic::new(ErrorCode::E0300, format!("use of moved value '{name}'"))
                        .with_why(format!(
                            "'{name}' was already moved at {}..{}",
                            moved.start, moved.end
                        ))
                        .with_fix(format!("clone '{name}' before the first move"))
                        .with_label(span, format!("'{name}' used after move"), true)
                        .with_label(moved, format!("'{name}' moved here"), false)
                        .with_ownership_diagram(diagram),
                );
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
                self.errors.push(
                    LoonDiagnostic::new(
                        ErrorCode::E0301,
                        format!("cannot mutably borrow immutable binding '{name}'"),
                    )
                    .with_why("only bindings declared with [let mut ...] can be mutably borrowed")
                    .with_fix(format!("declare '{name}' as [let mut {name} ...]"))
                    .with_label(span, "mutable borrow of immutable binding", true),
                );
                return;
            }
            if binding.state == BindingState::MutBorrowed {
                self.errors.push(
                    LoonDiagnostic::new(
                        ErrorCode::E0302,
                        format!("cannot borrow '{name}' as mutable more than once"),
                    )
                    .with_why("Loon prevents aliased mutable references to eliminate data races")
                    .with_fix("ensure the first mutable borrow is no longer in use")
                    .with_label(span, "second mutable borrow", true),
                );
            }
        }
        if let Some(binding) = self.get_binding_mut(name) {
            binding.state = BindingState::MutBorrowed;
        }
    }

    /// Analyze a function body to determine how each parameter is used.
    /// `param_names` is the list of parameter names, `body` is the function body expressions.
    fn analyze_param_modes(&self, param_names: &[String], body: &[Expr]) -> Vec<ParamMode> {
        let mut modes: Vec<ParamMode> = vec![ParamMode::Borrow; param_names.len()];
        for expr in body {
            self.classify_expr(expr, param_names, &mut modes, false);
        }
        // The last expression in the body is in return position
        if let Some(last) = body.last() {
            self.classify_expr(last, param_names, &mut modes, true);
        }
        modes
    }

    /// Walk an expression classifying how each parameter is used.
    /// `in_return_pos` is true when this expression is the tail/return position.
    fn classify_expr(
        &self,
        expr: &Expr,
        param_names: &[String],
        modes: &mut [ParamMode],
        in_return_pos: bool,
    ) {
        match &expr.kind {
            ExprKind::Symbol(name) => {
                if in_return_pos {
                    // A bare symbol in return position means it escapes
                    if let Some(idx) = param_names.iter().position(|p| p == name) {
                        Self::escalate(&mut modes[idx], ParamMode::Move);
                    }
                }
            }
            ExprKind::List(items) if !items.is_empty() => {
                if let ExprKind::Symbol(head) = &items[0].kind {
                    match head.as_str() {
                        "fn" | "type" | "trait" | "impl" | "sig" | "pub" | "test" | "derive" => {
                            // Don't descend into nested definitions
                        }
                        "let" => {
                            // Analyze value expressions but not the binding name
                            // [let name val] or [let mut name val]
                            let val_start = if items.len() > 2 {
                                if matches!(&items[1].kind, ExprKind::Symbol(s) if s == "mut") {
                                    3
                                } else {
                                    2
                                }
                            } else {
                                2
                            };
                            for item in items.iter().skip(val_start) {
                                self.classify_expr(item, param_names, modes, false);
                            }
                        }
                        "if" => {
                            // condition is not in return position
                            if items.len() > 1 {
                                self.classify_expr(&items[1], param_names, modes, false);
                            }
                            // then and else branches inherit return position
                            for item in items.iter().skip(2) {
                                self.classify_expr(item, param_names, modes, in_return_pos);
                            }
                        }
                        "do" => {
                            // All but last are not in return position
                            let body = &items[1..];
                            if !body.is_empty() {
                                for item in &body[..body.len() - 1] {
                                    self.classify_expr(item, param_names, modes, false);
                                }
                                self.classify_expr(
                                    &body[body.len() - 1],
                                    param_names,
                                    modes,
                                    in_return_pos,
                                );
                            }
                        }
                        "push!" | "set!" => {
                            // First arg is mutably borrowed
                            if items.len() > 1 {
                                if let ExprKind::Symbol(name) = &items[1].kind {
                                    if let Some(idx) = param_names.iter().position(|p| p == name) {
                                        Self::escalate(&mut modes[idx], ParamMode::MutBorrow);
                                    }
                                }
                            }
                            // Remaining args: analyze normally
                            for item in items.iter().skip(2) {
                                self.classify_expr(item, param_names, modes, false);
                            }
                        }
                        fname if self.borrow_fns.contains(fname) => {
                            // Builtin borrow function — args are only borrowed
                            for item in items.iter().skip(1) {
                                // Still recurse for nested exprs, but symbols here are just borrowed
                                if !matches!(&item.kind, ExprKind::Symbol(name) if param_names.contains(name))
                                {
                                    self.classify_expr(item, param_names, modes, false);
                                }
                                // If it's a param symbol passed to a borrow fn, mode stays Borrow (no-op)
                            }
                        }
                        fname => {
                            // User-defined or unknown function call.
                            // Check if we have analyzed param modes for this callee.
                            let callee_modes = self.fn_param_modes.get(fname).cloned();
                            for (i, item) in items.iter().skip(1).enumerate() {
                                if let ExprKind::Symbol(name) = &item.kind {
                                    if let Some(idx) =
                                        param_names.iter().position(|p| p == name)
                                    {
                                        // Determine what mode the callee uses for this arg position
                                        let arg_mode = callee_modes
                                            .as_ref()
                                            .and_then(|m| m.get(i).copied())
                                            .unwrap_or(ParamMode::Move);
                                        Self::escalate(&mut modes[idx], arg_mode);
                                        continue;
                                    }
                                }
                                self.classify_expr(item, param_names, modes, false);
                            }
                        }
                    }
                } else {
                    // Head is not a symbol — generic call, treat all args as Move
                    for item in items.iter().skip(1) {
                        if let ExprKind::Symbol(name) = &item.kind {
                            if let Some(idx) = param_names.iter().position(|p| p == name) {
                                Self::escalate(&mut modes[idx], ParamMode::Move);
                                continue;
                            }
                        }
                        self.classify_expr(item, param_names, modes, false);
                    }
                }
            }
            ExprKind::Vec(items) | ExprKind::Set(items) | ExprKind::Tuple(items) => {
                // Stored in a data structure → Move
                for item in items {
                    if let ExprKind::Symbol(name) = &item.kind {
                        if let Some(idx) = param_names.iter().position(|p| p == name) {
                            Self::escalate(&mut modes[idx], ParamMode::Move);
                            continue;
                        }
                    }
                    self.classify_expr(item, param_names, modes, false);
                }
            }
            ExprKind::Map(pairs) => {
                for (k, v) in pairs {
                    for item in [k, v] {
                        if let ExprKind::Symbol(name) = &item.kind {
                            if let Some(idx) = param_names.iter().position(|p| p == name) {
                                Self::escalate(&mut modes[idx], ParamMode::Move);
                                continue;
                            }
                        }
                        self.classify_expr(item, param_names, modes, false);
                    }
                }
            }
            _ => {}
        }
    }

    /// Escalate a param mode: Borrow < MutBorrow < Move
    fn escalate(mode: &mut ParamMode, new: ParamMode) {
        let rank = |m: &ParamMode| match m {
            ParamMode::Borrow => 0,
            ParamMode::MutBorrow => 1,
            ParamMode::Move => 2,
        };
        if rank(&new) > rank(mode) {
            *mode = new;
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
                "fn" if items.len() > 1 && matches!(&items[1].kind, ExprKind::Symbol(_)) => {
                    // Named function: [fn name [params] body...]
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
                "match" | "|>" | "type" | "test" | "pub" | "trait" | "impl" | "sig" | "derive" => {
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

        // Generic function call — head is borrowed, args use per-param modes if available
        let callee_name = if let ExprKind::Symbol(s) = &items[0].kind {
            Some(s.clone())
        } else {
            None
        };
        let callee_modes = callee_name.and_then(|n| self.fn_param_modes.get(&n).cloned());

        self.check_expr(&items[0]);
        for (i, item) in items[1..].iter().enumerate() {
            if let ExprKind::Symbol(ref name) = item.kind {
                let mode = callee_modes
                    .as_ref()
                    .and_then(|m| m.get(i).copied())
                    .unwrap_or(ParamMode::Move);
                match mode {
                    ParamMode::Borrow => {
                        self.use_binding(name, item.span);
                    }
                    ParamMode::MutBorrow => {
                        self.mut_borrow(name, item.span);
                    }
                    ParamMode::Move => {
                        self.move_binding(name, item.span);
                    }
                }
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

        // Extract function name for param mode registration
        let fn_name = if let ExprKind::Symbol(name) = &args[0].kind {
            Some(name.clone())
        } else {
            None
        };

        // Register params with type-based Copy detection
        if let ExprKind::List(params) = &args[1].kind {
            // Analyze param modes before checking the body
            let param_names: Vec<String> = params
                .iter()
                .filter_map(|p| {
                    if let ExprKind::Symbol(name) = &p.kind {
                        Some(name.clone())
                    } else {
                        None
                    }
                })
                .collect();

            if let Some(ref name) = fn_name {
                let body = &args[body_start..];
                let modes = self.analyze_param_modes(&param_names, body);
                self.fn_param_modes.insert(name.clone(), modes);
            }

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

    pub fn check_program(&mut self, exprs: &[Expr]) -> Vec<LoonDiagnostic> {
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

    fn check(src: &str) -> Vec<LoonDiagnostic> {
        let exprs = parse(src).unwrap();
        let mut checker = OwnershipChecker::new();
        checker.check_program(&exprs)
    }

    fn check_with_types(src: &str) -> Vec<LoonDiagnostic> {
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
            [fn take [s] s]
            [fn main []
              [let name "alice"]
              [take name]
              [println name]]
        "#,
        );
        // `take` is not in borrow_fns, so `name` gets moved, then used
        assert!(
            errors.iter().any(|e| e.message().contains("moved")),
            "expected use-after-move error, got: {:?}",
            errors
        );
    }

    #[test]
    fn mut_borrow_immutable() {
        let errors = check(
            r#"
            [fn main []
              [let v #[1 2 3]]
              [push! v 4]]
        "#,
        );
        assert!(
            errors.iter().any(|e| e.message().contains("immutable")),
            "expected immutable borrow error, got: {:?}",
            errors
        );
    }

    #[test]
    fn mut_borrow_ok() {
        let errors = check(
            r#"
            [fn main []
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
            [fn take [s] s]
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
            [fn consume [v] v]
            [let v #[1 2 3]]
            [consume v]
            [println v]
        "#,
        );
        assert!(
            errors.iter().any(|e| e.message().contains("moved")),
            "Vec is not Copy, should error: {:?}",
            errors
        );
    }

    #[test]
    fn param_inference_read_only_no_move() {
        // A function that only reads its param (passes to println) should not move it
        let errors = check(
            r#"
            [fn greet [name] [println name]]
            [let name "alice"]
            [greet name]
            [println name]
        "#,
        );
        assert!(errors.is_empty(), "read-only param should not move: {:?}", errors);
    }

    #[test]
    fn param_inference_returned_param_moves() {
        // A function that returns its param should move it
        let errors = check(
            r#"
            [fn identity [x] x]
            [let name "alice"]
            [identity name]
            [println name]
        "#,
        );
        assert!(
            errors.iter().any(|e| e.message().contains("moved")),
            "returned param should move: {:?}",
            errors
        );
    }

    #[test]
    fn param_inference_mixed_params() {
        // One param is read-only, one is returned — only the returned one should move
        let errors = check(
            r#"
            [fn pick_second [a b] [println a] b]
            [let x "hello"]
            [let y "world"]
            [pick_second x y]
            [println x]
        "#,
        );
        // x should NOT be moved (only read via println inside pick_second)
        assert!(errors.is_empty(), "read-only param x should not move: {:?}", errors);
    }

    #[test]
    fn param_inference_mixed_params_escaped_is_moved() {
        // The returned param should be moved
        let errors = check(
            r#"
            [fn pick_second [a b] [println a] b]
            [let x "hello"]
            [let y "world"]
            [pick_second x y]
            [println y]
        "#,
        );
        assert!(
            errors.iter().any(|e| e.message().contains("moved")),
            "returned param y should move: {:?}",
            errors
        );
    }

    #[test]
    fn derive_copy_no_move_error() {
        // A type with [derive Copy] should not trigger move errors
        let exprs = parse(r#"
            [derive Copy [type Point [Point Int Int]]]
            [fn take [p] p]
            [let p [Point 1 2]]
            [take p]
            [println p]
        "#).unwrap();
        let mut type_checker = crate::check::Checker::new();
        for expr in &exprs {
            type_checker.infer(expr);
        }
        let mut checker = OwnershipChecker::with_type_info(&type_checker.type_of, &type_checker.subst)
            .with_derived_copy_types(&type_checker.derived_copy_types);
        let errors = checker.check_program(&exprs);
        assert!(errors.is_empty(), "derive Copy type should not trigger move error: {:?}", errors);
    }
}
