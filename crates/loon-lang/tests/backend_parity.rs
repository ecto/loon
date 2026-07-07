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
        "match-vec-pattern",
        "[fn main [] [println [match #[1 2] #[a b] [+ a b] _ 0]]]",
    ),
    (
        "match-vec-pattern-exact-length",
        "[fn main [] [println [match #[1 2 3] #[a b] 0 #[a b c] [+ a [+ b c]] _ -1]]]",
    ),
    (
        "match-vec-pattern-nested-literal",
        "[fn main [] [println [match #[1 #[2 3]] #[1 #[b c]] [+ b c] _ 0]]]",
    ),
    (
        "match-vec-pattern-non-sequence-falls-through",
        r#"[fn main [] [println [match "xy" #[a b] "seq" _ "not-seq"]]]"#,
    ),
    (
        "match-empty-vec-pattern",
        r#"[fn main [] [println [match #[] #[] "empty" _ "no"]]]"#,
    ),
    (
        "let-destructure-vec",
        "[fn main [] [let [x y] #[7 8]] [println [+ x y]]]",
    ),
    (
        "let-destructure-nested",
        "[fn main [] [let [a [b c]] #[1 #[2 3]]] [println [+ a [+ b c]]]]",
    ),
    (
        "let-destructure-extra-elements-allowed",
        "[fn main [] [let [x y] #[1 2 3 4]] [println [+ x y]]]",
    ),
    (
        "let-destructure-tuple-from-zip",
        "[fn main [] [let [a b] [first [zip #[1 2] #[10 20]]]] [println [+ a b]]]",
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
    // A handler clause that does NOT call `resume` ABORTS: the captured
    // continuation is discarded and the enclosing `handle` yields the clause
    // value directly. Here the body would compute [+ 1 [F.fail]], but the
    // non-resuming clause 999 wins — the `[+ 1 ...]` is never resumed. Both
    // backends now agree on 999.
    (
        "effect-abort-discards-continuation",
        "[effect F [fail [] Int]] \
         [fn body [] [+ 1 [F.fail]]] \
         [fn main [] [println [handle [body] [F.fail] 999]]]",
    ),
    // and/or: short-circuiting (desugared to nested `if` at macro-expansion
    // time, so both backends inherit it). `[or true X]` must never evaluate X;
    // `[assert-eq 1 2]` would abort the program if it ran.
    (
        "and-or-short-circuit",
        r#"[fn main []
             [println [or true [assert-eq 1 2]]]
             [println [and false [assert-eq 1 2]]]]"#,
    ),
    // and/or: value semantics (first falsy / first truthy, else last) and
    // variadic/nullary forms.
    (
        "and-or-values",
        r#"[fn main []
             [println [and 1 2]] [println [and false 2]]
             [println [or false 5]] [println [or 3 5]]
             [println [and]] [println [or]]
             [println [and true true 9]] [println [or false false 7]]]"#,
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
    // ── map insertion order (guaranteed identical across backends) ─────────
    // A literal iterates/prints in the order keys were written, NOT sorted or
    // hashed. `:c :a :b` would print sorted as `:a :b :c` on a key-ordered map
    // and in hash order on a hash map; insertion order is the only ruling.
    (
        "map-order-keys",
        "[fn main [] [println [keys {:c 3 :a 1 :b 2}]]]",
    ),
    (
        "map-order-display",
        "[fn main [] [println {:c 3 :a 1 :b 2}]]",
    ),
    (
        // values follow key order — read them back via [get] per key so this
        // stays independent of the vals/values builtin-naming split.
        "map-order-values",
        "[fn main [] [let m {:c 3 :a 1 :b 2}] \
         [println [map [keys m] [fn [k] [get m k]]]]]",
    ),
    (
        // assoc of an EXISTING key updates in place: order is unchanged, only
        // the value moves. `:a` stays in slot 2, now 99.
        "map-order-assoc-update",
        "[fn main [] [let m {:c 3 :a 1 :b 2}] \
         [let m2 [assoc m :a 99]] \
         [println [keys m2]] \
         [println [map [keys m2] [fn [k] [get m2 k]]]]]",
    ),
    (
        // assoc of a NEW key appends it to the end.
        "map-order-assoc-append",
        "[fn main [] [println [keys [assoc {:c 3 :a 1 :b 2} :d 4]]]]",
    ),
    (
        // merge is left-biased: existing keys keep their position AND value,
        // new keys from the right append in their order. `:b` keeps slot 2 and
        // value 2 (not 9); `:c` appends.
        "map-order-merge",
        "[fn main [] [let mm [merge {:a 1 :b 2} {:b 9 :c 3}]] \
         [println [keys mm]] \
         [println [map [keys mm] [fn [k] [get mm k]]]]]",
    ),
    (
        // Value equality is ORDER-INDEPENDENT: two maps with the same k/v pairs
        // are equal regardless of insertion order. Ordering must not leak into
        // equality.
        "map-eq-order-independent",
        "[fn main [] [println [= {:a 1 :b 2} {:b 2 :a 1}]]]",
    ),
    // map destructuring in let: `{name name}` shorthand binds a key to its
    // value; both backends must bind the names (the EIR VM used to drop the
    // pattern silently and bind nothing). Present keys read through.
    (
        "let-map-destructure-shorthand",
        "[fn main [] [let m {:name 42 :age 7}] \
         [let {name name age age} m] \
         [println name] [println age]]",
    ),
    // a per-key default expression is used only when the key is ABSENT; a
    // present key keeps its value and the default is not evaluated.
    (
        "let-map-destructure-default",
        "[fn main [] [let m {:name 42}] \
         [let {name name missing 99} m] \
         [println name] [println missing]]",
    ),
    // a missing key with NO default binds unit on both backends (matching the
    // interpreter's `None => Value::Unit` fallthrough) — printed as ().
    (
        "let-map-destructure-missing-no-default",
        "[fn main [] [let m {:a 1}] \
         [let {gone gone} m] \
         [println gone]]",
    ),
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
    // and/or pipe STEPS are thread-last partial application: [pipe v [or a]]
    // ≡ [or a v] (short-circuit now, same values as the old eager builtin)
    (
        "pipe-and-or-steps",
        "[fn main [] \
           [println [pipe false [or 7]]] \
           [println [pipe 5 [and true true]]] \
           [println [pipe 0 [or false]]]]",
    ),
    // operator partials in pipe: +/* are variadic left folds, the rest are
    // strictly binary (extra args, including the piped value, are ignored)
    (
        "pipe-operator-partials",
        "[fn main [] \
           [println [pipe 5 [+ 1 2]]] \
           [println [pipe 2 [* 3 4]]] \
           [println [pipe 5 [- 10 3]]] \
           [println [pipe 5 [+ 1]]]]",
    ),
    // a local binding shadows the prelude Option/Result ctors at call sites,
    // exactly as it shadows builtins (the ctors are pre-registered on the VM)
    (
        "local-shadows-prelude-ctor",
        "[fn main [] [let Some [fn [x] [+ x 1]]] [println [Some 5]]]",
    ),
    // ── try/on-fail hygiene + evaluation order (fixed 2026-07-01) ──────────
    // The on-fail handler is lowered eagerly in the enclosing scope and applied
    // via gensym bindings, so the injected message/continuation names cannot
    // shadow user variables, and the handler expression evaluates once (even on
    // the success path). All four must now agree across backends.
    (
        // enclosing `__fail_msg` is NOT shadowed by the injected message binding
        "try-no-shadow-failmsg",
        r#"[fn main []
             [do [let __fail_msg "USER"]
                 [println [try [Fail.fail "boom"] [fn [m] [str m "/" __fail_msg]]]]]]"#,
    ),
    (
        // enclosing `resume` is NOT shadowed by the handler's implicit continuation
        "try-no-shadow-resume",
        r#"[fn main []
             [do [let resume 42]
                 [println [try [Fail.fail "x"] [fn [m] [str "r=" resume]]]]]]"#,
    ),
    (
        // the handler-producing expression is evaluated eagerly, once — its side
        // effect shows up even when the body succeeds
        "try-onfail-eager",
        r#"[fn mk [] [do [println "MAKING"] [fn [m] "h"]]]
           [fn main [] [println [try [+ 1 2] [mk]]]]"#,
    ),
    (
        // on-fail closes over an enclosing local (the supervision retry pattern)
        "try-onfail-captures-local",
        r#"[fn run [tag]
             [try [Fail.fail "e"] [fn [m] [str tag ":" m]]]]
           [fn main [] [println [run "job7"]]]"#,
    ),
    (
        // a 3-arg try picks the SECOND arg as the handler on both backends
        "try-three-arg-uses-second",
        r#"[fn main [] [println [try [Fail.fail "x"] [fn [m] 99] 7]]]"#,
    ),
    // Canonical truthiness: the falsy set is exactly {false, (), None} — a
    // value is truthy unless it says no (false) or says nothing ((), None).
    // Integer 0, float 0.0, empty string, and empty collections are all
    // TRUTHY and drive the THEN branch. The EIR VM is the reference; the
    // legacy interpreter used to treat Int(0) as falsy and now agrees.
    // (None's falsiness has its own entry below.)
    (
        "truthiness-falsy-set",
        r#"[fn main []
             [println [if 0 "T" "F"]]
             [println [if 0.0 "T" "F"]]
             [println [if "" "T" "F"]]
             [println [if #[] "T" "F"]]
             [println [if {} "T" "F"]]
             [println [if false "T" "F"]]]"#,
    ),
    // None is FALSY on both backends: it "says nothing", like (). Some(x) is
    // truthy for ANY payload — including Some(false) and Some(0) — because
    // the wrapper says something regardless of what's inside. (This flips the
    // pre-#66-stopgap "None is truthy" pin; the falsy set is now closed at
    // {false, (), None}.)
    (
        "truthiness-none-falsy",
        r#"[fn main []
             [println [if None "T" "F"]]
             [println [if [Some false] "T" "F"]]
             [println [if [Some 0] "T" "F"]]
             [println [= None None]]
             [println [= None [do]]]
             [println [= None [Some 1]]]]"#,
    ),
    // if-let/when-let: expr evaluates once; Some v binds the payload, any
    // other truthy value binds the value itself, falsy (None/false/()) takes
    // the else arm (or nothing for when-let). Payload truthiness is
    // irrelevant: Some(false) binds x=false and runs THEN.
    (
        "if-let-when-let",
        r#"[fn main []
             [println [if-let [x [Some 5]] [str "some:" x] "else"]]
             [println [if-let [x None] [str "bad:" x] "none-else"]]
             [println [if-let [x [Some false]] [str "payload:" x] "BAD"]]
             [println [if-let [x 7] [str "plain:" x] "BAD"]]
             [println [if-let [x false] "BAD" "false-else"]]
             [println [if-let [x [Some 1]] [str "no-else:" x]]]
             [when-let [x [Some 2]] [println "wl-first"] [println [str "wl:" x]]]
             [when-let [x None] [println "BAD"]]
             [when-let [x "s"] [println [str "wl-plain:" x]]]]"#,
    ),
    // some?/none? exist as exact complements on both backends: none? is true
    // for the "says nothing" values (None and unit), some? for everything
    // else — including Some(false).
    (
        "some-none-predicates",
        r#"[fn main []
             [println [some? None]] [println [none? None]]
             [println [some? [Some false]]] [println [none? [Some false]]]
             [println [some? [do]]] [println [none? [do]]]
             [println [some? 0]] [println [none? 0]]]"#,
    ),
    // `[or maybe-x default]` is the blessed default-value idiom: None is
    // falsy, so `or` skips it and yields the default; a present Some passes
    // through as the Some itself (unwrap separately).
    (
        "or-none-default-idiom",
        r#"[fn main []
             [println [or None "fallback"]]
             [println [or [Some 5] "fallback"]]
             [println [or false None "last"]]]"#,
    ),
    // and/or thread truthiness through the same test: `[and 0 "x"]` keeps going
    // past 0 (truthy) and yields "x"; `[or false #[]]` skips false and yields
    // the empty vector, which prints truthy.
    (
        "truthiness-and-or",
        r#"[fn main []
             [println [if [and 0 "x"] "T" "F"]]
             [println [if [or false #[]] "T" "F"]]]"#,
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

/// Integer division / modulo by zero must RAISE on BOTH backends — never
/// silently return `()`. The EIR VM used to return unit here (a silent-failure
/// regression); it now raises a "division by zero" / "modulo by zero" runtime
/// error, agreeing with the interpreter. Float division by zero is IEEE (inf),
/// NOT an error, on both backends — verified here so the fix doesn't overreach.
#[test]
fn int_divide_by_zero_raises_on_both_backends() {
    for (src, needle) in [
        ("[fn main [] [println [/ 5 0]]]", "division by zero"),
        ("[fn main [] [println [% 5 0]]]", "modulo by zero"),
    ] {
        let eir = eir_output(src);
        let interp = interp_output(src);
        assert!(eir.is_err(), "EIR VM must error on {src:?}, got {eir:?}");
        assert!(
            interp.is_err(),
            "interp must error on {src:?}, got {interp:?}"
        );
        assert!(
            eir.as_ref().unwrap_err().contains(needle),
            "EIR VM error for {src:?} should mention {needle:?}, got {:?}",
            eir.unwrap_err()
        );
    }

    // Float division by zero stays IEEE (infinity), not an error, on both.
    let finf = "[fn main [] [println [/ 1.0 0.0]]]";
    assert!(eir_output(finf).is_ok(), "float /0.0 must not error on EIR");
    assert!(
        interp_output(finf).is_ok(),
        "float /0.0 must not error on interp"
    );
}

/// Destructuring `let` with too few elements (or a non-sequence value) must
/// RAISE on BOTH backends — never silently bind `()` (issue #17: vector
/// patterns used to no-op entirely on the EIR VM). Extra elements are allowed
/// (Clojure prior); the ruling is documented in DESIGN.md §6.
#[test]
fn destructure_mismatch_raises_on_both_backends() {
    for (src, needle) in [
        (
            "[fn main [] [let [x y z] #[1 2]] [println x]]",
            "destructuring expected at least 3 elements, got 2",
        ),
        (
            "[fn main [] [let [x y] 5] [println x]]",
            "destructuring requires a vector or tuple",
        ),
    ] {
        let eir = eir_output(src);
        let interp = interp_output(src);
        assert!(eir.is_err(), "EIR VM must error on {src:?}, got {eir:?}");
        assert!(
            interp.is_err(),
            "interp must error on {src:?}, got {interp:?}"
        );
        assert!(
            eir.as_ref().unwrap_err().contains(needle),
            "EIR VM error for {src:?} should mention {needle:?}, got {:?}",
            eir.unwrap_err()
        );
        assert!(
            interp.as_ref().unwrap_err().contains(needle),
            "interp error for {src:?} should mention {needle:?}, got {:?}",
            interp.unwrap_err()
        );
    }
}

/// Known divergences between the backends, PINNED so any change is noticed.
/// Each records the program and the (eir, interp) outputs observed today, with
/// a note on which backend is correct — a worklist for unification.
///
/// (The non-resuming-abort divergence has been RETIRED: the legacy interp now
/// discards the continuation like the EIR VM, so it lives in CORPUS as
/// `effect-abort-discards-continuation` and is enforced for agreement.)
///
/// (The fold-builtin-arg divergence has been RETIRED: the EIR VM now wraps a
/// first-class binary operator in an arity-2 closure, so `[fold xs 0 +]`
/// agrees with the interp's variadic dispatch. Asserted below as CONVERGED.)
#[test]
fn known_divergences_are_pinned() {
    // CONVERGED (2026-07-01, phase-2): an uncaught Fail raised in a handler
    // clause — whose enclosing `try` was frozen into the continuation — used to
    // fall through to silent unit on the EIR VM (result collapsed to "()"). It
    // now raises a loud UnhandledEffect error, matching the tree-walker. Both
    // backends error (messages differ in format, so this is asserted as
    // both-error rather than a CORPUS equality entry).
    let frozen_try = "[effect E [op [Int] Int]] \
                      [fn body [] [try [E.op 1] [fn [m] [str \"caught \" m]]]] \
                      [fn main [] [println [handle [body] [E.op x] [Fail.fail \"denied\"]]]]";
    assert!(
        eir_output(frozen_try).is_err(),
        "EIR uncaught clause-Fail now errors"
    );
    assert!(
        interp_output(frozen_try).is_err(),
        "interp uncaught clause-Fail errors"
    );

    // CONVERGED (2026-07-07): a binary operator (`+`) passed as a HOF
    // function. The EIR VM used to wrap it in an arity-1 closure (misfiring
    // to 0); lower_symbol now wraps first-class operators in an arity-2
    // closure, matching the interp's variadic dispatch for the binary case.
    let fold_builtin = "[fn main [] [println [fold #[1 2 3 4] 0 +]]]";
    assert_eq!(
        eir_output(fold_builtin).as_deref(),
        Ok("10"),
        "EIR operator-as-fold-fn"
    );
    assert_eq!(
        interp_output(fold_builtin).as_deref(),
        Ok("10"),
        "interp operator-as-fold-fn"
    );

    // nested-handlers: two effects handled by two stacked handlers. The EIR VM
    // composes them correctly (30); the legacy interp's replay strategy hits its
    // "too many sequential effects" guard and errors. EIR correct — another
    // reason the legacy tree-walker is the one to retire.
    let nested = "[effect A [a [] Int]] [effect B [b [] Int]] \
                  [fn body [] [+ [A.a] [B.b]]] \
                  [fn main [] [println [handle [handle [body] [A.a] [resume 10]] [B.b] [resume 20]]]]";
    assert_eq!(
        eir_output(nested).as_deref(),
        Ok("30"),
        "EIR nested handlers (correct)"
    );
    assert!(
        interp_output(nested).is_err(),
        "interp nested handlers (broken)"
    );

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
    assert_eq!(
        eir_output(forward).as_deref(),
        Ok("w:k:x"),
        "EIR forward-to-outer (correct)"
    );
    assert_eq!(
        interp_output(forward).as_deref(),
        Ok("k:x"),
        "interp forward-to-outer (wrong)"
    );

    // inner-handle-survives-resume: an inner handle suspended inside a captured
    // continuation must still handle its effect after the outer handler
    // resumes — its handlers travel with the segment (the EIR VM snapshots and
    // re-establishes them). The legacy interp hits its sequential-effects guard
    // and errors. EIR correct.
    let suspended = "[effect A [geta [] Int]] [effect B [getb [] Int]] \
         [fn inner [] [+ [A.geta] [B.getb]]] \
         [fn body [] [handle [inner] [B.getb] [resume 10]]] \
         [fn main [] [println [handle [body] [A.geta] [resume 1]]]]";
    assert_eq!(
        eir_output(suspended).as_deref(),
        Ok("11"),
        "EIR suspended inner handle (correct)"
    );
    assert!(
        interp_output(suspended).is_err(),
        "interp suspended inner handle (broken)"
    );

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
    assert_eq!(
        eir_output(sup_retry).as_deref(),
        Ok("ok"),
        "EIR try-retry captures (correct)"
    );
    assert_ne!(
        interp_output(sup_retry).as_deref(),
        Ok("ok"),
        "interp try-retry captures (broken)"
    );
}

/// The EIR VM used to silently produce `()` in several places the interp
/// errors loudly (unbound symbols, binary min/max, assoc on a vector,
/// non-exhaustive match). All are now hard errors on BOTH backends, with the
/// EIR message matching the interp's wording.
#[test]
fn silent_unit_traps_error_loudly() {
    // (program, expected substring in the EIR error)
    let cases: &[(&str, &str)] = &[
        (
            "[fn main [] [println [reduce f 0 #[1 2 3]]]]",
            "unbound symbol 'f'",
        ),
        // (sqrt is now implemented on both backends — see the ok_cases below.)
        ("[fn main [] [println [min 1 2]]]", "min requires a vector"),
        ("[fn main [] [println [max 5 3]]]", "max requires a vector"),
        ("[fn main [] [println [min #[]]]]", "min: empty vector"),
        (
            // Non-numeric elements error rather than being silently skipped
            // (a NaN-keyed skip would return the wrong element). The interp
            // diverges here: its generic value_cmp orders strings.
            r#"[fn main [] [println [min #["b" "a"]]]]"#,
            "min: non-numeric element",
        ),
        (
            "[fn main [] [println [assoc #[1 2 3] 0 9]]]",
            "assoc requires a map",
        ),
        (
            r#"[fn main [] [println [match 5 1 "one" 2 "two"]]]"#,
            "no match arm matched value: 5",
        ),
    ];
    for (src, want) in cases {
        let eir = eir_output(src);
        let msg = eir.expect_err(&format!("EIR must error: {src}"));
        assert!(
            msg.contains(want),
            "EIR error for {src:?} was {msg:?}, expected to contain {want:?}"
        );
        // The interp errors on all of these too — except string min, which
        // its generic value_cmp orders instead of rejecting.
        if !src.contains(r#"#["b" "a"]"#) {
            assert!(interp_output(src).is_err(), "interp must also error: {src}");
        }
    }

    // Loud-but-correct counterparts: the supported forms still work and agree.
    let ok_cases: &[(&str, &str)] = &[
        ("[fn main [] [println [min #[3 1 2]]]]", "1"),
        ("[fn main [] [println [max #[3 1 2]]]]", "3"),
        // sqrt is now implemented on both backends (was an EIR gap).
        ("[fn main [] [println [sqrt 4]]]", "2"),
        (
            r#"[fn main [] [println [get [assoc {:a 1} :b 2] :b]]]"#,
            "2",
        ),
        (r#"[fn main [] [println [match 2 1 "one" 2 "two"]]]"#, "two"),
    ];
    for (src, want) in ok_cases {
        assert_eq!(eir_output(src).as_deref(), Ok(*want), "EIR: {src}");
        assert_eq!(interp_output(src).as_deref(), Ok(*want), "interp: {src}");
    }
}
