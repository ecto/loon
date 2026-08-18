//! Kernels: functions restricted enough to run somewhere else.
//!
//! A kernel is written like any other function —
//!
//! ```text
//! [kernel saxpy [i a x out]
//!   [put out i [+ [* a [at x i]] [at out i]]]]
//! ```
//!
//! — and is an ordinary function as far as the rest of the compiler is
//! concerned: [`desugar`] rewrites the head to `fn` before type checking, so
//! kernels infer, lower, and run exactly like anything else. What the keyword
//! buys is a *promise*, checked by [`verify`]: the body stays inside a small
//! numeric subset with no closures, no allocation, no strings, and no effects.
//!
//! That restriction is the safety argument. A recent Rust GPU-offload design
//! needs an `unsafe trait` for its partitioning strategies, because a kernel
//! there can be handed a slice and index it however it likes; the invariant
//! that threads touch disjoint elements has to be promised by hand. Here a
//! kernel cannot express the unsafe program in the first place — it receives
//! an index and may only write at that index — so the guarantee comes from
//! what the language declines to compile rather than from a promise attached
//! to a trait impl.
//!
//! The subset is also what makes a kernel portable. Everything admitted here
//! has a direct equivalent in scalar machine code and in WGSL, which is why
//! the same source can run on a CPU core, across threads, or on a GPU without
//! being rewritten for each.

use crate::ast::{Expr, ExprKind};
use crate::errors::codes::ErrorCode;
use crate::errors::LoonDiagnostic;
use std::collections::HashSet;

/// Builtins a kernel body may call.
///
/// Every one of these is a scalar numeric operation or a buffer element
/// access — things a GPU can do per work item. Anything that allocates,
/// inspects a collection, or touches a string is absent on purpose.
const KERNEL_BUILTINS: &[&str] = &[
    // Buffer element access.
    "at", "put", "buf-len", // Math.
    "sqrt", "pow", "floor", "ceil", "round", "sin", "cos", "tan", "asin", "acos", "atan", "atan2",
    "log", "log10", "exp", "abs", "min", "max", // Logic.
    "not",
];

// Deliberately absent: `int` and `float`. In Loon those parse strings, and a
// kernel has no strings to parse. Mixing an integer and a float in kernel
// arithmetic is handled where it belongs — the WGSL emitter inserts the
// conversion, because WGSL will not mix the two silently and neither should
// the language pretend to.

/// Special forms a kernel body may use.
const KERNEL_FORMS: &[&str] = &["let", "if", "do", "and", "or", "recur", "mut"];

/// Rewrite every `[kernel name [params] body...]` into the equivalent `fn`,
/// returning the rewritten program and the set of kernel names.
///
/// Doing this before type checking means kernels are not a second language:
/// they infer types, get ownership modes, and lower through exactly the same
/// path as ordinary functions. The only thing that remains special about them
/// is the promise [`verify`] enforces and the fact that they can be placed.
pub fn desugar(exprs: &[Expr]) -> (Vec<Expr>, HashSet<String>) {
    let mut names = HashSet::new();
    let out = exprs.iter().map(|e| rewrite(e, &mut names)).collect();
    (out, names)
}

fn rewrite(expr: &Expr, names: &mut HashSet<String>) -> Expr {
    let ExprKind::List(items) = &expr.kind else {
        return expr.clone();
    };
    let mut new_items: Vec<Expr> = items.iter().map(|i| rewrite(i, names)).collect();
    if let Some(ExprKind::Symbol(head)) = new_items.first().map(|i| &i.kind) {
        if head == "kernel" && new_items.len() >= 3 {
            if let ExprKind::Symbol(name) = &new_items[1].kind {
                names.insert(name.clone());
                new_items[0] = Expr {
                    kind: ExprKind::Symbol("fn".to_string()),
                    ..new_items[0].clone()
                };
            }
        }
    }
    Expr {
        kind: ExprKind::List(new_items),
        ..expr.clone()
    }
}

/// Check that each kernel body stays inside the placeable subset.
///
/// Runs on the desugared program, so kernels appear as `fn` forms; `names`
/// says which ones were written with the `kernel` keyword.
pub fn verify(exprs: &[Expr], names: &HashSet<String>) -> Vec<LoonDiagnostic> {
    if names.is_empty() {
        return Vec::new();
    }
    let mut v = Verifier {
        kernels: names,
        errors: Vec::new(),
        index_param: None,
    };
    for expr in exprs {
        v.walk_program(expr);
    }
    v.errors
}

struct Verifier<'a> {
    kernels: &'a HashSet<String>,
    errors: Vec<LoonDiagnostic>,
    /// The name of the kernel currently being checked's work index — its first
    /// parameter. A `put` at any other index is a scatter.
    index_param: Option<String>,
}

impl Verifier<'_> {
    fn walk_program(&mut self, expr: &Expr) {
        let ExprKind::List(items) = &expr.kind else {
            return;
        };
        let is_fn =
            matches!(items.first().map(|i| &i.kind), Some(ExprKind::Symbol(h)) if h == "fn");
        if is_fn && items.len() >= 3 {
            if let ExprKind::Symbol(name) = &items[1].kind {
                if self.kernels.contains(name) {
                    self.index_param = first_param(&items[2]);
                    self.check_params(name, &items[2]);
                    for body in &items[3..] {
                        self.check_body(name, body);
                    }
                    return;
                }
            }
        }
        for item in items {
            self.walk_program(item);
        }
    }

    /// A kernel's first parameter is its work index, so it needs at least one.
    fn check_params(&mut self, kernel: &str, params: &Expr) {
        let ExprKind::List(ps) = &params.kind else {
            return;
        };
        if ps.is_empty() {
            self.errors.push(
                LoonDiagnostic::new(
                    ErrorCode::E0601,
                    format!("kernel '{kernel}' has no parameters"),
                )
                .with_why(
                    "a kernel runs once per work item and takes that item's index as its first \
                     parameter, so it cannot be nullary"
                        .to_string(),
                )
                .with_fix(format!(
                    "give '{kernel}' an index parameter, e.g. [kernel {kernel} [i ...] ...]"
                ))
                .with_label(
                    params.span,
                    "expected at least an index parameter",
                    true,
                ),
            );
        }
    }

    fn check_body(&mut self, kernel: &str, expr: &Expr) {
        match &expr.kind {
            // Literals and names are always fine.
            ExprKind::Int(_)
            | ExprKind::Float(_)
            | ExprKind::Bool(_)
            | ExprKind::Symbol(_)
            | ExprKind::Keyword(_) => {}

            ExprKind::Str(_) => self.reject(
                kernel,
                expr,
                "a string literal",
                "kernels have no string support: there is nowhere to put the bytes on a device",
            ),
            ExprKind::Vec(_) | ExprKind::Map(_) | ExprKind::Set(_) | ExprKind::Tuple(_) => self
                .reject(
                    kernel,
                    expr,
                    "a collection literal",
                    "kernels cannot allocate; pass a buffer in and index it instead",
                ),

            ExprKind::List(items) => self.check_call(kernel, expr, items),
            _ => {}
        }
    }

    fn check_call(&mut self, kernel: &str, expr: &Expr, items: &[Expr]) {
        let Some(head) = items.first() else {
            return;
        };
        // `Effect.op` reaches here as a dot access on an uppercase name.
        if let ExprKind::DotAccess(base, field) = &head.kind {
            if let ExprKind::Symbol(b) = &base.kind {
                if b.chars().next().is_some_and(|c| c.is_uppercase()) {
                    self.reject(
                        kernel,
                        expr,
                        &format!("the effect operation '{b}.{field}'"),
                        "a kernel runs where there is no handler tower to perform effects against",
                    );
                    return;
                }
            }
        }

        let ExprKind::Symbol(name) = &head.kind else {
            // An indirect call means a function value, which a kernel has no
            // way to have obtained.
            self.reject(
                kernel,
                expr,
                "an indirect call",
                "kernels can only call other kernels and a fixed set of numeric builtins",
            );
            return;
        };

        // An effect performed inside a kernel would need a handler on the
        // device. Effects are how placement itself is expressed, so a kernel
        // performing one would be circular as well as unimplementable.
        if name.contains('.') && name.chars().next().is_some_and(|c| c.is_uppercase()) {
            self.reject(
                kernel,
                expr,
                &format!("the effect operation '{name}'"),
                "a kernel runs where there is no handler tower to perform effects against",
            );
            return;
        }

        // A work item may write at its own index and nowhere else.
        //
        // This is the disjointness rule the whole design rests on: it is what
        // lets the parallel executor hand each thread a slice and what lets a
        // GPU dispatch run every work item at once. The Rust offload work
        // arrives at the same guarantee by having partitioning strategies
        // promise it in an `unsafe impl`; here the program that would violate
        // it does not compile.
        if name == "put" && items.len() >= 3 {
            let idx = &items[2];
            let ok = match (&idx.kind, &self.index_param) {
                (ExprKind::Symbol(s), Some(p)) => s == p,
                _ => false,
            };
            if !ok {
                let index_name = self.index_param.clone().unwrap_or_else(|| "i".to_string());
                self.errors.push(
                    LoonDiagnostic::new(
                        ErrorCode::E0602,
                        format!("kernel '{kernel}' writes at an index other than its own"),
                    )
                    .with_why(format!(
                        "every work item runs at once, so each may only write element \
                         '{index_name}'; writing elsewhere means two of them can reach the \
                         same element and the result would depend on which got there first"
                    ))
                    .with_fix(format!(
                        "write at '{index_name}', and read whatever else this element needs \
                         with `at`"
                    ))
                    .with_label(idx.span, "not this work item's element", true),
                );
            }
        }

        if name == "fn" || name == "kernel" {
            self.reject(
                kernel,
                expr,
                "a nested function",
                "kernels cannot create closures; there is no heap to capture into",
            );
            return;
        }

        let known = KERNEL_FORMS.contains(&name.as_str())
            || KERNEL_BUILTINS.contains(&name.as_str())
            || self.kernels.contains(name)
            || is_operator(name);

        if !known {
            self.reject(
                kernel,
                expr,
                &format!("a call to '{name}'"),
                "kernels may call other kernels and numeric builtins only",
            );
            return;
        }

        for arg in &items[1..] {
            self.check_body(kernel, arg);
        }
    }

    fn reject(&mut self, kernel: &str, expr: &Expr, what: &str, why: &str) {
        self.errors.push(
            LoonDiagnostic::new(
                ErrorCode::E0600,
                format!("kernel '{kernel}' contains {what}"),
            )
            .with_why(why.to_string())
            .with_fix(
                "move this out of the kernel and pass the result in as a buffer or scalar"
                    .to_string(),
            )
            .with_label(expr.span, "not allowed inside a kernel", true),
        );
    }
}

/// The name of a parameter list's first entry.
fn first_param(params: &Expr) -> Option<String> {
    let ExprKind::List(ps) = &params.kind else {
        return None;
    };
    match ps.first().map(|p| &p.kind) {
        Some(ExprKind::Symbol(name)) => Some(name.clone()),
        _ => None,
    }
}

/// Operators lower to arithmetic instructions rather than builtin calls.
fn is_operator(name: &str) -> bool {
    matches!(
        name,
        "+" | "-" | "*" | "/" | "%" | "=" | "!=" | "<" | ">" | "<=" | ">="
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;

    fn check(src: &str) -> Vec<LoonDiagnostic> {
        let exprs = parse(src).expect("parses");
        let (desugared, names) = desugar(&exprs);
        verify(&desugared, &names)
    }

    #[test]
    fn a_numeric_kernel_is_accepted() {
        let errors = check("[kernel saxpy [i a x out] [put out i [+ [* a [at x i]] [at out i]]]]");
        assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    }

    #[test]
    fn control_flow_and_math_are_accepted() {
        let errors = check(
            "[kernel clamp [i lo hi x out] \
               [let v [at x i]] \
               [put out i [if [< v lo] lo [if [> v hi] hi [sqrt [abs v]]]]]]",
        );
        assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    }

    #[test]
    fn a_kernel_may_call_another_kernel() {
        let errors = check(
            "[kernel scale [i s x out] [put out i [* s [at x i]]]] \
             [kernel twice [i x out] [scale i 2.0 x out]]",
        );
        assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    }

    #[test]
    fn desugaring_makes_a_kernel_an_ordinary_function() {
        let exprs = parse("[kernel k [i x] [put x i 1.0]]").expect("parses");
        let (desugared, names) = desugar(&exprs);
        assert!(names.contains("k"));
        let ExprKind::List(items) = &desugared[0].kind else {
            panic!("expected a list");
        };
        assert!(
            matches!(&items[0].kind, ExprKind::Symbol(h) if h == "fn"),
            "the head should have become `fn`, got {:?}",
            items[0].kind
        );
    }

    #[test]
    fn allocation_is_rejected() {
        let errors = check("[kernel k [i out] [put out i [len #[1 2 3]]]]");
        assert!(!errors.is_empty(), "a vector literal should be rejected");
        assert_eq!(errors[0].code, ErrorCode::E0600);
    }

    #[test]
    fn strings_are_rejected() {
        let errors = check("[kernel k [i out] [put out i [len \"hello\"]]]");
        assert!(!errors.is_empty(), "a string literal should be rejected");
    }

    #[test]
    fn closures_are_rejected() {
        let errors = check("[kernel k [i out] [put out i [[fn [x] x] 1.0]]]");
        assert!(!errors.is_empty(), "a nested fn should be rejected");
    }

    #[test]
    fn effects_are_rejected() {
        // This one is the interesting case: placement *is* an effect, so a
        // kernel performing one would be asking the device to reach back into
        // the handler tower it was dispatched from.
        let errors = check("[kernel k [i out] [do [IO.println 1] [put out i 1.0]]]");
        assert!(!errors.is_empty(), "an effect should be rejected");
        assert!(
            errors[0].what.contains("IO.println"),
            "the message should name the operation: {}",
            errors[0].what
        );
    }

    #[test]
    fn calls_to_ordinary_functions_are_rejected() {
        let errors = check("[fn helper [x] x] [kernel k [i out] [put out i [helper 1.0]]]");
        assert!(
            !errors.is_empty(),
            "a kernel may not call a non-kernel function"
        );
    }

    #[test]
    fn writing_at_another_work_items_index_is_rejected() {
        // The disjointness rule, enforced. Two work items reaching the same
        // element would make the result depend on which arrived first.
        for src in [
            "[kernel k [i s d] [put d [- 99 i] [at s i]]]",
            "[kernel k [i d] [put d 0 1.0]]",
            "[kernel k [i d] [put d [+ i 1] 1.0]]",
        ] {
            let errors = check(src);
            assert!(!errors.is_empty(), "should reject a scatter: {src}");
            assert_eq!(errors[0].code, ErrorCode::E0602, "{src}");
        }
    }

    #[test]
    fn writing_at_your_own_index_is_the_whole_point() {
        let errors = check("[kernel k [i s d] [put d i [+ [at s i] [at s 0]]]]");
        assert!(
            errors.is_empty(),
            "reading anywhere is fine; only writing is restricted: {errors:?}"
        );
    }

    #[test]
    fn the_index_parameter_can_be_called_anything() {
        let errors = check("[kernel k [row d] [put d row 1.0]]");
        assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    }

    #[test]
    fn a_nullary_kernel_is_rejected() {
        let errors = check("[kernel k [] 1]");
        assert!(!errors.is_empty(), "a kernel needs an index parameter");
        assert_eq!(errors[0].code, ErrorCode::E0601);
    }

    #[test]
    fn ordinary_functions_are_left_alone() {
        // The restrictions apply to kernels only; normal code keeps every
        // feature the language has.
        let errors = check("[fn ordinary [] [let v #[1 2 3]] [IO.println \"hi\"] [len v]]");
        assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    }
}
