//! Parameter modes survive the trip from inference into the IR.
//!
//! Loon infers, for every function parameter, whether the callee reads it,
//! writes through it, or consumes it. Other languages make you write that down
//! — Rust spells it `&T` / `&mut T` / `T`, and a GPU offload compiler reads
//! those same sigils to decide which way bytes move across a device boundary.
//! Here the analysis already ran; these tests pin that its answers are correct
//! and that lowering carries them onto `Func::param_modes` instead of throwing
//! them away.

use loon_lang::check::ownership::{infer_param_modes, ParamMode};
use loon_lang::check::Checker;
use loon_lang::eir::lower::lower;
use loon_lang::eir::Mode;
use loon_lang::parser::parse;

/// Infer modes for one named function.
fn modes_of(src: &str, func: &str) -> Vec<ParamMode> {
    let exprs = parse(src).expect("parses");
    infer_param_modes(&exprs)
        .get(func)
        .unwrap_or_else(|| panic!("no modes inferred for `{func}`"))
        .clone()
}

/// Lower a program and read the modes off the named function's IR.
fn lowered_modes(src: &str, func: &str) -> Vec<Mode> {
    let exprs = parse(src).expect("parses");
    let mut checker = Checker::new();
    let errors = checker.check_program(&exprs);
    assert!(errors.is_empty(), "type errors: {errors:?}");
    let module = lower(&checker);
    let f = module
        .funcs
        .iter()
        .find(|f| f.name.as_deref() == Some(func))
        .unwrap_or_else(|| panic!("no lowered function `{func}`"));
    f.param_modes.clone()
}

#[test]
fn a_read_only_parameter_is_a_borrow() {
    // `n` is only added to, so the caller keeps it.
    assert_eq!(
        modes_of("[fn twice [n] [+ n n]]", "twice"),
        vec![ParamMode::Borrow]
    );
}

#[test]
fn a_mutated_parameter_is_a_mutable_borrow() {
    // `push!` writes through its first argument; the rest are read.
    assert_eq!(
        modes_of("[fn add-to [v x] [push! v x]]", "add-to"),
        vec![ParamMode::MutBorrow, ParamMode::Borrow]
    );
}

#[test]
fn a_returned_parameter_is_moved() {
    assert_eq!(
        modes_of("[fn identity [x] x]", "identity"),
        vec![ParamMode::Move]
    );
}

#[test]
fn kernel_accessors_drive_the_direction() {
    // This is the whole trick behind inferring transfer direction: `at` reads
    // an element and `put` writes one, so a buffer that is only read comes out
    // `Borrow` (host-to-device only) while one written through comes out
    // `MutBorrow` (must be synchronized back). Nobody annotated anything.
    assert_eq!(
        modes_of("[fn saxpy [i a x out] [put out i [* a [at x i]]]]", "saxpy"),
        vec![
            ParamMode::Borrow,    // i — index, read
            ParamMode::Borrow,    // a — scalar, read
            ParamMode::Borrow,    // x — input buffer
            ParamMode::MutBorrow, // out — output buffer
        ]
    );
}

#[test]
fn modes_reach_the_lowered_function() {
    // The point of the exercise: what the frontend inferred is visible to the
    // backend, rather than being dropped with the checker.
    assert_eq!(
        lowered_modes("[fn add-to [v x] [push! v x]] [fn main [] []]", "add-to"),
        vec![Mode::InOut, Mode::In]
    );
    // The shape a kernel launch cares about: one argument read, one written
    // through. `nth` reads; `push!` writes. Once buffers land, `at` and `put`
    // take these roles with the same rules.
    assert_eq!(
        lowered_modes(
            "[fn blend [src dst] [push! dst [nth src 0]]] [fn main [] []]",
            "blend"
        ),
        vec![Mode::In, Mode::InOut]
    );
}

#[test]
fn arity_always_matches_the_parameter_list() {
    // A mode vector that disagrees with the parameter count would silently
    // misalign direction with argument, so lowering falls back to the
    // conservative all-`Owned` answer rather than emitting a short vector.
    for (src, func, arity) in [
        ("[fn none [] 1]", "none", 0),
        ("[fn one [a] a]", "one", 1),
        ("[fn three [a b c] [+ a [+ b c]]]", "three", 3),
    ] {
        let src = format!("{src} [fn main [] []]");
        assert_eq!(
            lowered_modes(&src, func).len(),
            arity,
            "`{func}` should have {arity} modes"
        );
    }
}

#[test]
fn a_callee_that_mutates_propagates_to_its_caller() {
    // `caller` hands `x` to something that writes through it, so `x` is
    // mutably borrowed rather than consumed — in placement terms, an argument
    // that has to be synchronized back but need not be surrendered. Reaching
    // this answer requires resolving the callee, which is what the fixed point
    // buys over a single source-order pass.
    let modes = modes_of(
        "[fn caller [x] [later x]] [fn later [y] [push! y 1]]",
        "caller",
    );
    assert_eq!(modes, vec![ParamMode::MutBorrow]);
}

#[test]
fn conservative_when_the_callee_is_unknowable() {
    // A callee that is not a definition in this program — here a parameter
    // holding a function — cannot be analyzed, so the argument is assumed
    // consumed. That is sound: it only costs an optimization, and it must
    // never come out as a plain borrow, which would wrongly tell a backend
    // nothing needs copying back.
    let modes = modes_of("[fn apply-to [f x] [f x]]", "apply-to");
    assert_eq!(modes[1], ParamMode::Move);
}

#[test]
fn modes_do_not_depend_on_definition_order() {
    // Mode inference walks definitions in source order and consults callees it
    // has already seen. Reordering two independent functions must not change
    // either one's answer.
    let forward = modes_of(
        "[fn helper [v] [push! v 1]] [fn user [w] [helper w]]",
        "user",
    );
    let backward = modes_of(
        "[fn user [w] [helper w]] [fn helper [v] [push! v 1]]",
        "user",
    );
    assert_eq!(
        forward, backward,
        "definition order changed the inferred mode: {forward:?} vs {backward:?}"
    );
}
