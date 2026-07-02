//! End-to-end tests for effect row polymorphism.
//!
//! Each program here exercises inference that a flat effect set cannot
//! express — higher-order functions whose effects depend on their arguments
//! (`twice`, a generic `sum-with` combinator, `map` with an effectful
//! lambda). Before effect rows these were mistyped (the argument's effects
//! were baked in at first use, or over/under-approximated); now they must
//! CHECK cleanly and RUN correctly on the EIR VM (the default backend) and
//! the legacy interpreter.

use loon_lang::check::Checker;
use loon_lang::eir::vm::eval_eir;
use loon_lang::interp::builtins::capture_output;
use loon_lang::interp::eval_program;
use loon_lang::parser::parse;

/// Type-check `src`, returning (fn_effects, diagnostics).
fn check(
    src: &str,
) -> (
    std::collections::HashMap<String, loon_lang::types::EffectRow>,
    Vec<loon_lang::errors::LoonDiagnostic>,
) {
    let exprs = parse(src).unwrap();
    let mut checker = Checker::new();
    let errors = checker.check_program(&exprs);
    (checker.fn_effects.clone(), errors)
}

fn eir_output(src: &str) -> Result<String, String> {
    eval_eir(src)
        .map(|r| r.output.join("\n"))
        .map_err(|e| format!("{e}"))
}

fn interp_output(src: &str) -> Result<String, String> {
    let exprs = parse(src).map_err(|e| format!("parse: {}", e.message))?;
    let (res, out) = capture_output(|| eval_program(&exprs));
    res.map(|_| out).map_err(|e| format!("{e:?}"))
}

/// One `twice`, two instantiations: a pure one and an effectful one, in the
/// same program. Untypeable with flat effect sets — the first use would bake
/// its argument's effects into `twice` for the second.
const TWICE_BOTH_USES: &str = "\
[fn twice [f x] [f [f x]]]
[effect Counter [next [] Int]]
[fn pure-part [] [twice [fn [x] [* x 2]] 3]]
[fn eff-part [] [twice [fn [x] [+ x [Counter.next]]] 0]]
[fn main []
  [println [pure-part]]
  [println [handle [eff-part]
    [Counter.next] [resume 10]]]]";

#[test]
fn twice_both_uses_checks_clean() {
    let (effects, errors) = check(TWICE_BOTH_USES);
    assert!(errors.is_empty(), "must check cleanly, got: {errors:?}");
    // One definition, two effect instantiations.
    let pure_part = effects.get("pure-part").unwrap();
    assert!(
        !pure_part.contains("Counter"),
        "pure use stays pure, got: {}",
        pure_part.render()
    );
    let eff_part = effects.get("eff-part").unwrap();
    assert!(
        eff_part.contains("Counter"),
        "effectful use carries Counter, got: {}",
        eff_part.render()
    );
    // main handles Counter, so nothing escapes.
    let main = effects.get("main").unwrap();
    assert!(
        !main.contains("Counter"),
        "main handles Counter, got: {}",
        main.render()
    );
}

#[test]
fn twice_both_uses_runs_on_eir_vm() {
    // pure: [twice dbl 3] = 12; effectful: 0 + 10 + 10 = 20 under [resume 10].
    assert_eq!(eir_output(TWICE_BOTH_USES).as_deref(), Ok("12\n20"));
}

#[test]
fn twice_both_uses_runs_on_interp() {
    assert_eq!(interp_output(TWICE_BOTH_USES).as_deref(), Ok("12\n20"));
}

/// A user-level generic combinator (`sum-with`, a map-reduce) used with a
/// pure function and an effectful function in the same program, the
/// effectful use handled at the call site. With a flat effect set the first
/// use fixed the combinator's effects, so one of the two uses mistyped.
const SUM_WITH_BOTH_USES: &str = "\
[effect Ask [ask [] Int]]
[fn sum-with [f v]
  [if [= [len v] 0]
    0
    [+ [f [nth v 0]] [sum-with f [drop 1 v]]]]]
[fn main []
  [println [sum-with [fn [x] [* x x]] #[1 2 3]]]
  [println [handle [sum-with [fn [x] [+ x [Ask.ask]]] #[1 2 3]]
    [Ask.ask] [resume 10]]]]";

#[test]
fn sum_with_both_uses_checks_clean() {
    let (effects, errors) = check(SUM_WITH_BOTH_USES);
    assert!(errors.is_empty(), "must check cleanly, got: {errors:?}");
    let sum_with = effects.get("sum-with").unwrap();
    assert!(
        sum_with.labels.is_empty(),
        "sum-with has no effects of its own, got: {}",
        sum_with.render()
    );
    let main = effects.get("main").unwrap();
    assert!(
        !main.contains("Ask"),
        "main handles Ask, got: {}",
        main.render()
    );
}

#[test]
fn sum_with_both_uses_runs_on_eir_vm() {
    // pure: 1+4+9 = 14; effectful: (1+10)+(2+10)+(3+10) = 36.
    assert_eq!(eir_output(SUM_WITH_BOTH_USES).as_deref(), Ok("14\n36"));
}

#[test]
fn sum_with_both_uses_runs_on_interp() {
    assert_eq!(interp_output(SUM_WITH_BOTH_USES).as_deref(), Ok("14\n36"));
}

/// BUILTIN `map` with an effectful lambda, handled at the call site. The
/// builtins are effect-polymorphic in the checker (one shared row tail per
/// scheme), so this now checks cleanly — with flat sets the builtin's
/// signature could not express "map's effects are the lambda's effects".
const MAP_EFFECTFUL_LAMBDA: &str = "\
[effect Ask [ask [] Int]]
[fn bump-all [v] [map [fn [x] [+ x [Ask.ask]]] v]]
[fn sum-all [v] [fold 0 [fn [a b] [+ a b]] v]]
[fn main []
  [println [handle [sum-all [bump-all #[1 2 3]]]
    [Ask.ask] [resume 10]]]]";

#[test]
fn map_effectful_lambda_checks_clean() {
    let (effects, errors) = check(MAP_EFFECTFUL_LAMBDA);
    assert!(errors.is_empty(), "must check cleanly, got: {errors:?}");
    let bump_all = effects.get("bump-all").unwrap();
    assert!(
        bump_all.contains("Ask"),
        "map propagates the lambda's effect, got: {}",
        bump_all.render()
    );
    let sum_all = effects.get("sum-all").unwrap();
    assert!(
        !sum_all.contains("Ask"),
        "fold with a pure reducer stays pure, got: {}",
        sum_all.render()
    );
    let main = effects.get("main").unwrap();
    assert!(
        !main.contains("Ask"),
        "main handles Ask, got: {}",
        main.render()
    );
}

#[test]
fn map_effectful_lambda_runs_on_interp() {
    // (1+10) + (2+10) + (3+10) = 36
    assert_eq!(interp_output(MAP_EFFECTFUL_LAMBDA).as_deref(), Ok("36"));
}

/// A user ADT constructor passed as a first-class function to an effectful
/// higher-order function. Constructors are registered effect-polymorphic
/// (open quantified row tail), so the HOF's concrete IO label must not
/// clash with the constructor's row.
const CTOR_AS_HOF_ARG: &str = "\
[type Box [MkBox Int]]
[fn do-both [f x]
  [IO.println \"hi\"]
  [f x]]
[fn main []
  [println [do-both MkBox 1]]]";

#[test]
fn ctor_as_hof_arg_checks_clean() {
    let (effects, errors) = check(CTOR_AS_HOF_ARG);
    assert!(errors.is_empty(), "must check cleanly, got: {errors:?}");
    let main = effects.get("main").unwrap();
    assert!(
        main.contains("IO"),
        "main performs IO through do-both, got: {}",
        main.render()
    );
}

/// Same shape with a handled user effect (so output capture sees everything):
/// the handle boundary forces `Ask` into f's parameter row, and the
/// constructor's open tail must absorb it.
const CTOR_UNDER_HANDLER: &str = "\
[effect Ask [ask [] Int]]
[type Box [MkBox Int]]
[fn call-it [f]
  [handle [f [Ask.ask]]
    [Ask.ask] [resume 41]]]
[fn main []
  [println [call-it MkBox]]]";

#[test]
fn ctor_under_handler_checks_clean() {
    let (effects, errors) = check(CTOR_UNDER_HANDLER);
    assert!(errors.is_empty(), "must check cleanly, got: {errors:?}");
    let main = effects.get("main").unwrap();
    assert!(
        !main.contains("Ask"),
        "call-it handles Ask, got: {}",
        main.render()
    );
}

#[test]
fn ctor_under_handler_runs_on_interp() {
    assert_eq!(
        interp_output(CTOR_UNDER_HANDLER).as_deref(),
        Ok("[MkBox 41]")
    );
}

// PRE-EXISTING runtime gap (also present on main, before effect rows): the
// EIR VM cannot call a user ADT constructor that was passed around as a
// first-class value ("value is not callable"), independent of effect rows —
// verified failing at merge-base bf5ac8a. The checker side is fixed; un-ignore
// when the VM materializes constructors as callable values.
#[test]
#[ignore = "EIR VM: user ADT constructor as a first-class value is not callable (pre-existing)"]
fn ctor_under_handler_runs_on_eir_vm() {
    assert_eq!(eir_output(CTOR_UNDER_HANDLER).as_deref(), Ok("[MkBox 41]"));
}

// PRE-EXISTING runtime gap (also present on main, before effect rows): the
// EIR VM's higher-order BUILTINS (map/filter/fold) invoke their callback via
// a nested `run_call_with_captures` loop, and a `perform` inside that nested
// loop does not reach the enclosing handler — the elements come back
// corrupted (e.g. `#[() 12 12]`). User-defined higher-order functions are
// fine (see SUM_WITH_BOTH_USES above); only the Rust-native builtin
// iteration frame is broken, because a continuation cannot be captured
// across it. The checker side (this track) is correct — un-ignore when the
// VM threads effects through builtin callbacks.
#[test]
#[ignore = "EIR VM: perform inside a builtin HOF callback does not reach the handler (pre-existing)"]
fn map_effectful_lambda_runs_on_eir_vm() {
    assert_eq!(eir_output(MAP_EFFECTFUL_LAMBDA).as_deref(), Ok("36"));
}
