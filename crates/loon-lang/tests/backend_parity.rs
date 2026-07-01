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
    // strings
    ("str-split-join", r#"[fn main [] [println [join [split "a,b,c" ","] "-"]]]"#),
    ("str-case", r#"[fn main [] [println [str [uppercase "hi"] [lowercase "YO"]]]]"#),
    ("str-trim", r#"[fn main [] [println [str "[" [trim "  x  "] "]"]]]"#),
    ("str-replace", r#"[fn main [] [println [replace "aXbXc" "X" "-"]]]"#),
    ("str-substring", r#"[fn main [] [println [substring "hello" 1 4]]]"#),
    ("str-contains", r#"[fn main [] [println [contains? "hello" "ell"]]]"#),
    // collections
    ("range", "[fn main [] [println [range 0 5]]]"),
    ("len", "[fn main [] [println [len #[10 20 30]]]]"),
    ("sort", "[fn main [] [println [sort #[3 1 2]]]]"),
    ("reverse", "[fn main [] [println [reverse #[1 2 3]]]]"),
    ("take-drop", "[fn main [] [println [take 2 #[1 2 3 4 5]]] [println [drop 2 #[1 2 3 4 5]]]]"),
    ("conj", "[fn main [] [println [conj #[1 2] 3]]]"),
    ("first-nth", "[fn main [] [println [nth #[7 8 9] 1]]]"),
    ("vec-contains", "[fn main [] [println [contains? #[1 2 3] 2]]]"),
    ("sum", "[fn main [] [println [sum #[1 2 3 4]]]]"),
    ("any-all", "[fn main [] [println [any? [fn [x] [> x 2]] #[1 2 3]]] [println [all? [fn [x] [> x 0]] #[1 2 3]]]]"),
    // maps
    ("map-get", "[fn main [] [println [get {:a 1 :b 2} :b]]]"),
    ("map-assoc", "[fn main [] [println [get [assoc {:a 1} :c 9] :c]]]"),
    ("map-keys", "[fn main [] [println [len [keys {:a 1 :b 2 :c 3}]]]]"),
    // control / pattern
    ("when", "[fn main [] [when [> 3 2] [println \"yes\"]]]"),
    ("nested-let", "[fn main [] [let a 2] [let b [* a 3]] [let c [+ a b]] [println c]]"),
    // higher-order with named function
    (
        "map-named-fn",
        "[fn dbl [x] [* x 2]] [fn main [] [println [map #[1 2 3] dbl]]]",
    ),
    // a handler clause that captures an enclosing local (parameterized handler)
    (
        "handler-captures-local",
        "[effect E [op [] Int]] \
         [fn run [n] [handle [E.op] [E.op] [resume n]]] \
         [fn main [] [println [run 42]]]",
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

    // nested-handlers: two effects handled by two stacked handlers. The EIR VM
    // composes them correctly (30); the legacy interp's replay strategy hits its
    // "too many sequential effects" guard and errors. EIR correct — another
    // reason the legacy tree-walker is the one to retire.
    let nested = "[effect A [a [] Int]] [effect B [b [] Int]] \
                  [fn body [] [+ [A.a] [B.b]]] \
                  [fn main [] [println [handle [handle [body] [A.a] [resume 10]] [B.b] [resume 20]]]]";
    assert_eq!(eir_output(nested).as_deref(), Ok("30"), "EIR nested handlers (correct)");
    assert!(interp_output(nested).is_err(), "interp nested handlers (broken)");

    // forward-to-outer: a handler clause re-performing the handled effect runs
    // OUTSIDE its own handle (deep-handler semantics), so the perform reaches
    // the next handler out — the interposition substrate tracing/sandboxing
    // wrappers are built on. The EIR VM used to re-enter its own handler here
    // and loop forever; it now forwards ("w:k:x"). The legacy interp forwards
    // but then LOSES the wrapper clause's continuation, returning the outer
    // handler's value alone ("k:x"). EIR correct.
    let forward = r#"[effect F [read [String] String]]
        [fn body [] [F.read "x"]]
        [fn wrapped [] [handle [body] [F.read p] [resume [str "w:" [F.read p]]]]]
        [fn main [] [println [handle [wrapped] [F.read p] [resume [str "k:" p]]]]]"#;
    assert_eq!(eir_output(forward).as_deref(), Ok("w:k:x"), "EIR forward-to-outer (correct)");
    assert_eq!(interp_output(forward).as_deref(), Ok("k:x"), "interp forward-to-outer (wrong)");

    // inner-handle-survives-resume: an inner handle suspended inside a captured
    // continuation must still handle its effect after the outer handler
    // resumes — its handlers travel with the segment (the EIR VM snapshots and
    // re-establishes them). The legacy interp hits its sequential-effects guard
    // and errors. EIR correct.
    let suspended = "[effect A [geta [] Int]] [effect B [getb [] Int]] \
         [fn inner [] [+ [A.geta] [B.getb]]] \
         [fn body [] [handle [inner] [B.getb] [resume 10]]] \
         [fn main [] [println [handle [body] [A.geta] [resume 1]]]]";
    assert_eq!(eir_output(suspended).as_deref(), Ok("11"), "EIR suspended inner handle (correct)");
    assert!(interp_output(suspended).is_err(), "interp suspended inner handle (broken)");

    // try-on-fail capture: the on-fail closure references enclosing locals
    // (`child`, `n`) and RETRIES after an abort — the supervision pattern.
    // lower_try used to compile the on-fail expression with no captures, so
    // the retry called garbage ("value is not callable" / silent corruption);
    // it now desugars through lower_handle and inherits real free-variable
    // capture. The legacy interp truncates the program silently. EIR correct.
    let sup_retry = r#"
        [effect S [get [] Int]]
        [fn sup [child n]
          [try [child]
               [fn [m] [if [> n 0] [sup child [- n 1]] "gave up"]]]]
        [fn main []
          [let r [[handle
                    [sup [fn [] [if [< [S.get] 2] [Fail.fail "boom"] "ok"]] 5]
                    [return x] [fn [st] x]
                    [S.get] [fn [st] [[resume st] [+ st 1]]]]
                  0]]
          [println r]]"#;
    assert_eq!(eir_output(sup_retry).as_deref(), Ok("ok"), "EIR try-retry captures (correct)");
    assert_ne!(interp_output(sup_retry).as_deref(), Ok("ok"), "interp try-retry captures (broken)");
}
