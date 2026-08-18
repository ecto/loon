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
///
/// This is Loon's answer to what other languages make you write down. Rust
/// spells it `&T` / `&mut T` / `T` at every signature; here it is inferred
/// from the body and then used the same way — to decide what a call site may
/// do with a binding, and (for kernels) which direction data has to move
/// across a placement boundary.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParamMode {
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
            "println",
            "print",
            "str",
            "len",
            "nth",
            "get",
            "contains?",
            "empty?",
            "+",
            "-",
            "*",
            ">",
            "<",
            ">=",
            "<=",
            "=",
            "not",
            "and",
            "or",
            // Persistent-collection builtins: they return NEW collections
            // (structural sharing) and never consume the original, so the
            // argument stays usable afterwards on every backend.
            "map",
            "filter",
            "each",
            "fold",
            "reduce",
            "sum",
            "sort",
            "sort-by",
            "reverse",
            "conj",
            "cons",
            "keys",
            "values",
            "vals",
            "entries",
            "merge",
            "assoc",
            "update",
            "remove",
            "join",
            "split",
            "range",
            // `resume` continues a captured continuation with a value. A
            // multi-shot continuation may be resumed more than once, so resume
            // must NOT consume (move) its argument — treat it as a borrow. This
            // lets the escaping/answer-passing style (e.g. `[[resume s] s]`)
            // reuse a value across the resume without a false move error.
            "resume",
            // Kernel element read. `[at buf i]` observes one element and never
            // consumes the buffer, so a kernel parameter that is only read
            // stays a borrow — which is what makes it an `:in` (host-to-device
            // only) argument at a placement boundary.
            "at",
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
            // Dim values are plain floats at runtime — scalar, hence Copy.
            Type::Int | Type::Float | Type::Bool | Type::Keyword | Type::Dim(_) => true,
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
    fn make_move_diagram(
        &self,
        name: &str,
        defined: Span,
        moved: Span,
        used: Span,
    ) -> OwnershipDiagram {
        OwnershipDiagram {
            lines: vec![
                format!("  {name} defined at {}..{}", defined.start, defined.end),
                format!(
                    "  {name} moved   at {}..{}  -- ownership transferred",
                    moved.start, moved.end
                ),
                format!(
                    "  {name} used    at {}..{}  -- ERROR: value no longer available",
                    used.start, used.end
                ),
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
                let fix_msg =
                    format!("clone '{name}' before moving, or restructure to avoid the move");
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
                    .with_label(
                        span,
                        "mutable borrow of immutable binding",
                        true,
                    ),
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
                        "push!" | "set!" | "put" => {
                            // First arg is mutably borrowed. `put` is the
                            // kernel element write; treating it exactly like
                            // `set!` is what makes a written-to kernel buffer
                            // infer as `:inout` with no annotation.
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
                                    if let Some(idx) = param_names.iter().position(|p| p == name) {
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
                "match" | "pipe" | "type" | "test" | "pub" | "trait" | "impl" | "sig"
                | "derive" => {
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
            if matches!(&args[body_start].kind, ExprKind::Set(_) | ExprKind::Map(_)) {
                body_start += 1;
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
        let (binding, val_idx, is_mut) = if matches!(&args[0].kind, ExprKind::Symbol(s) if s == "mut")
        {
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

    /// Per-parameter modes for every named function seen so far.
    pub fn param_modes(&self) -> &HashMap<String, Vec<ParamMode>> {
        &self.fn_param_modes
    }

    /// Re-run mode analysis over every named function until the answers stop
    /// changing.
    ///
    /// The first pass sees definitions in source order, so a call to a
    /// not-yet-analyzed function falls back to the conservative `Move`. Once
    /// every function has an entry, re-analyzing resolves those calls for
    /// real. Repeating until stable makes the result independent of the order
    /// the definitions were written in, which matters because a spuriously
    /// `Move` parameter reads as "the callee consumed this" — a claim that
    /// costs optimizations downstream and is simply untrue.
    ///
    /// Bounded to a small number of rounds: each round can only replace a
    /// guess with a real answer, so a program that has not settled by then has
    /// a cycle whose conservative reading is the correct one to keep.
    pub fn refine_param_modes(&mut self, exprs: &[Expr]) {
        const MAX_ROUNDS: usize = 8;
        let defns = Self::collect_fn_defns(exprs);
        for _ in 0..MAX_ROUNDS {
            let mut changed = false;
            for (name, param_names, body) in &defns {
                let modes = self.analyze_param_modes(param_names, body);
                match self.fn_param_modes.get(name) {
                    Some(prev) if *prev == modes => {}
                    _ => {
                        self.fn_param_modes.insert(name.clone(), modes);
                        changed = true;
                    }
                }
            }
            if !changed {
                return;
            }
        }
    }

    /// Every `[fn name [params] body...]` in the program, including those
    /// nested inside module-level forms, as (name, param names, body).
    fn collect_fn_defns(exprs: &[Expr]) -> Vec<(String, Vec<String>, Vec<Expr>)> {
        let mut out = Vec::new();
        for expr in exprs {
            Self::collect_fn_defns_into(expr, &mut out);
        }
        out
    }

    fn collect_fn_defns_into(expr: &Expr, out: &mut Vec<(String, Vec<String>, Vec<Expr>)>) {
        let ExprKind::List(items) = &expr.kind else {
            return;
        };
        let Some(head) = items.first() else {
            return;
        };
        if let ExprKind::Symbol(h) = &head.kind {
            if h == "fn" && items.len() >= 3 {
                if let (ExprKind::Symbol(name), ExprKind::List(params)) =
                    (&items[1].kind, &items[2].kind)
                {
                    let param_names: Vec<String> = params
                        .iter()
                        .filter_map(|p| match &p.kind {
                            ExprKind::Symbol(n) => Some(n.clone()),
                            _ => None,
                        })
                        .collect();
                    // Skip an effect-row annotation between params and body,
                    // exactly as `check_defn` does.
                    let mut body_start = 3;
                    if body_start < items.len()
                        && matches!(
                            &items[body_start].kind,
                            ExprKind::Set(_) | ExprKind::Map(_)
                        )
                    {
                        body_start += 1;
                    }
                    out.push((
                        name.clone(),
                        param_names,
                        items[body_start..].to_vec(),
                    ));
                    return;
                }
            }
        }
        // Not a function definition — look inside for nested ones.
        for item in items {
            Self::collect_fn_defns_into(item, out);
        }
    }
}

/// Infer parameter modes for a program without reporting ownership errors.
///
/// The full ownership check runs as a separate frontend pass and its results
/// are thrown away with the checker. The *modes* it computes along the way are
/// useful to the compiler proper — they say, for each parameter, whether a
/// caller's value is read, mutated, or consumed — so this entry point runs the
/// same analysis for its modes alone. Diagnostics are discarded here; the
/// dedicated pass is still what reports them.
///
/// Unlike the diagnostic pass, this one iterates to a fixed point. A single
/// pass in source order has to guess at callees it has not reached yet, and it
/// guesses `Move`; that makes the answer depend on the order two functions
/// happen to be written in, which is not a property anything downstream should
/// inherit. Re-running until nothing changes removes the guess.
pub fn infer_param_modes(exprs: &[Expr]) -> HashMap<String, Vec<ParamMode>> {
    let mut checker = OwnershipChecker::new();
    let _ = checker.check_program(exprs);
    checker.refine_param_modes(exprs);
    checker.fn_param_modes
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
        let mut checker =
            OwnershipChecker::with_type_info(&type_checker.type_of, &type_checker.subst);
        checker.check_program(&exprs)
    }

    #[test]
    fn no_error_simple() {
        let errors = check(r#"[let x 42] [println x]"#);
        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
    }

    #[test]
    fn http_double_send_is_compile_error() {
        // An HTTP response body is MOVED into Http.respond and consumed there;
        // performing the effect a second time with the same value must be a
        // use-after-move error (the "double send is a compile error" property).
        let errors = check(
            "[type Response [Resp Int String]] \
             [effect Http [respond [Response] Unit]] \
             [fn handler [] [let r [Resp 200 \"hi\"]] [Http.respond r] [Http.respond r]]",
        );
        assert!(
            errors.iter().any(|e| e.message().contains("moved")),
            "expected use-after-move, got: {errors:?}"
        );
        // A single send is fine.
        let ok = check(
            "[type Response [Resp Int String]] \
             [effect Http [respond [Response] Unit]] \
             [fn handler [] [let r [Resp 200 \"hi\"]] [Http.respond r]]",
        );
        assert!(ok.is_empty(), "single send should be clean: {ok:?}");
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
        assert!(
            errors.is_empty(),
            "Int is Copy, should not error: {:?}",
            errors
        );
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
        assert!(
            errors.is_empty(),
            "read-only param should not move: {:?}",
            errors
        );
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
        assert!(
            errors.is_empty(),
            "read-only param x should not move: {:?}",
            errors
        );
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
        let exprs = parse(
            r#"
            [derive Copy [type Point [Point Int Int]]]
            [fn take [p] p]
            [let p [Point 1 2]]
            [take p]
            [println p]
        "#,
        )
        .unwrap();
        let mut type_checker = crate::check::Checker::new();
        for expr in &exprs {
            type_checker.infer(expr);
        }
        let mut checker =
            OwnershipChecker::with_type_info(&type_checker.type_of, &type_checker.subst)
                .with_derived_copy_types(&type_checker.derived_copy_types);
        let errors = checker.check_program(&exprs);
        assert!(
            errors.is_empty(),
            "derive Copy type should not trigger move error: {:?}",
            errors
        );
    }
}
