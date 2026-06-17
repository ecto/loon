//! Backend differential-parity suite.
//!
//! Runs the same program under the EIR VM (the default `loon run` backend) and
//! the legacy tree-walking interpreter, and asserts they produce identical
//! observable output. This is the safety net for unifying the backends: any
//! semantic divergence between them becomes a failing test rather than a
//! surprise discovered later in one backend or the other.
//!
//! Programs that legitimately exercise unimplemented-on-one-backend features are
//! NOT in the corpus yet; as the backends converge, more move into `CORPUS`.

use loon_lang::eir::vm::eval_eir;
use loon_lang::interp::builtins::capture_output;
use loon_lang::interp::eval_program;
use loon_lang::parser::parse;

/// Output of running `src` under the EIR VM (joined stdout lines).
fn eir_output(src: &str) -> Result<String, String> {
    eval_eir(src)
        .map(|r| r.output.join("\n"))
        .map_err(|e| format!("{e}"))
}

/// Output of running `src` under the legacy tree-walking interpreter.
fn interp_output(src: &str) -> Result<String, String> {
    let exprs = parse(src).map_err(|e| format!("parse: {}", e.message))?;
    let (res, out) = capture_output(|| eval_program(&exprs));
    res.map(|_| out).map_err(|e| format!("{e:?}"))
}

/// Programs that both backends should evaluate identically. Each prints its
/// result(s) so the comparison is on observable output.
const CORPUS: &[(&str, &str)] = &[
    ("arith", "[fn main [] [println [+ [* 6 7] [- 10 8]]]]"),
    ("float", "[fn main [] [println [* 2.0 3.5]]]"),
    ("strings", r#"[fn main [] [println [str "a" "-" 42 "-" "b"]]]"#),
    ("bool", "[fn main [] [println [and [> 3 2] [not [< 5 1]]]]]"),
    (
        "vec-map-fold",
        "[fn main [] [println [fold [map #[1 2 3 4] [fn [x] [* x x]]] 0 [fn [a b] [+ a b]]]]]",
    ),
    (
        "pipe-map-each",
        "[fn dbl [x] [* x 2]] \
         [fn main [] [pipe [range 1 4] [map dbl] [each [fn [x] [println x]]]]]",
    ),
    ("filter", "[fn main [] [println [filter #[1 2 3 4 5] [fn [x] [> x 2]]]]]"),
    (
        "recursion",
        "[fn fib [n] [if [< n 2] n [+ [fib [- n 1]] [fib [- n 2]]]]] \
         [fn main [] [println [fib 10]]]",
    ),
    (
        "closure-capture",
        "[fn adder [n] [fn [x] [+ x n]]] \
         [fn main [] [let add5 [adder 5]] [println [add5 10]]]",
    ),
    (
        "adt-nongeneric",
        "[type Shape [Circle f64] [Rect f64 f64] Point] \
         [fn area [s] [match s [Circle r] [* 3.0 [* r r]] [Rect w h] [* w h] Point 0.0]] \
         [fn main [] [println [area [Rect 3.0 4.0]]]]",
    ),
    (
        "adt-generic-option",
        "[type Option T [Some T] None] \
         [fn unwrap-or [o d] [match o [Some x] x None d]] \
         [fn main [] [println [unwrap-or [Some 7] 0]] [println [unwrap-or None 0]]]",
    ),
    (
        "effect-tail-resume",
        "[effect E [op [Int] Int]] \
         [fn body [] [+ 100 [E.op 5]]] \
         [fn main [] [println [handle [body] [E.op v] [resume v]]]]",
    ),
    (
        "effect-multishot",
        "[effect C [pick [] Int]] \
         [fn body [] [* [C.pick] 2]] \
         [fn main [] [println [handle [body] [C.pick] [+ [resume 3] [resume 5]]]]]",
    ),
];

#[test]
fn backends_agree() {
    let mut divergences = Vec::new();
    for (name, src) in CORPUS {
        let eir = eir_output(src);
        let interp = interp_output(src);
        if eir != interp {
            divergences.push(format!("  {name}: EIR={eir:?}  INTERP={interp:?}"));
        }
    }
    assert!(
        divergences.is_empty(),
        "backend divergences ({}/{}):\n{}",
        divergences.len(),
        CORPUS.len(),
        divergences.join("\n")
    );
}

/// Known divergences between the backends, PINNED so any change is noticed.
/// Each records the program and the (eir, interp) outputs observed today, with
/// a note on which backend is correct — a worklist for unification.
///
/// - effect-abort: a handler clause that does NOT resume must DISCARD the
///   continuation (algebraic-effects abort). The EIR VM does (999); the legacy
///   interp wrongly resumes with the clause value (1 + 999 = 1000). EIR correct.
/// - fold-builtin-arg: a binary builtin (`+`) passed as a HOF function. The EIR
///   VM wraps a builtin used as a value in an arity-1 closure, so binary use via
///   fold misfires (0); the interp dispatches variadically (10). interp correct.
#[test]
fn known_divergences_are_pinned() {
    let abort = "[effect F [fail [] Int]] [fn body [] [+ 1 [F.fail]]] \
                 [fn main [] [println [handle [body] [F.fail] 999]]]";
    assert_eq!(eir_output(abort).as_deref(), Ok("999"), "EIR abort (correct)");
    assert_eq!(interp_output(abort).as_deref(), Ok("1000"), "interp abort (wrong)");

    let fold_builtin = "[fn main [] [println [fold #[1 2 3 4] 0 +]]]";
    assert_eq!(eir_output(fold_builtin).as_deref(), Ok("0"), "EIR builtin-as-fold-fn (gap)");
    assert_eq!(interp_output(fold_builtin).as_deref(), Ok("10"), "interp builtin-as-fold-fn");
}
