//! Builtin-registry conformance suite.
//!
//! The registry (`loon_lang::builtins::BUILTINS`) is the single source of
//! truth for the builtin surface. These tests assert every entry is wired
//! on every surface — the type checker's initial environment, the legacy
//! tree-walking interpreter, and the EIR VM/lowering — so a builtin cannot
//! exist on one surface and be missing on another.

use loon_lang::builtins::BUILTINS;
use loon_lang::check::Checker;
use loon_lang::eir::lower::{builtin_const, resolve_builtin_name};
use loon_lang::eir::vm::eval_eir;
use loon_lang::interp::builtins::{capture_output, register_builtins};
use loon_lang::interp::eval_program;
use loon_lang::interp::Env;
use loon_lang::parser::parse;

#[test]
fn checker_types_every_registry_entry() {
    let checker = Checker::new();
    let missing: Vec<&str> = BUILTINS
        .iter()
        .map(|b| b.name)
        .filter(|name| checker.env.get(name).is_none())
        .collect();
    assert!(
        missing.is_empty(),
        "registry builtins missing from the checker's initial environment: {missing:?}"
    );
}

#[test]
fn interp_implements_every_registry_entry() {
    let mut env = Env::new();
    register_builtins(&mut env);
    let missing: Vec<&str> = BUILTINS
        .iter()
        .map(|b| b.name)
        .filter(|name| env.get(name).is_none())
        .collect();
    assert!(
        missing.is_empty(),
        "registry builtins missing from the interpreter: {missing:?}"
    );
}

#[test]
fn vm_lowering_covers_every_registry_entry() {
    let missing: Vec<&str> = BUILTINS
        .iter()
        .map(|b| b.name)
        .filter(|name| resolve_builtin_name(name).is_none() && builtin_const(name).is_none())
        .collect();
    assert!(
        missing.is_empty(),
        "registry builtins not lowered for the EIR VM: {missing:?}"
    );
}

// ── Differential smoke tests: both backends, identical output ──────────

fn eir_output(src: &str) -> String {
    eval_eir(src)
        .unwrap_or_else(|e| panic!("EIR VM failed: {e}\n{src}"))
        .output
        .join("\n")
}

fn interp_output(src: &str) -> String {
    let exprs = parse(src).unwrap_or_else(|e| panic!("parse: {}\n{src}", e.message));
    let (res, out) = capture_output(|| eval_program(&exprs));
    res.unwrap_or_else(|e| panic!("interp failed: {e:?}\n{src}"));
    out
}

fn assert_parity(name: &str, src: &str, expected: &str) {
    let vm = eir_output(src);
    let interp = interp_output(src);
    assert_eq!(vm, interp, "{name}: backend outputs diverge\n{src}");
    assert_eq!(vm, expected, "{name}: unexpected output\n{src}");
}

#[test]
fn math_builtins_parity() {
    assert_parity("sqrt-int", "[fn main [] [println [sqrt 4]]]", "2");
    assert_parity("pow", "[fn main [] [println [pow 2 10]]]", "1024");
    assert_parity(
        "floor-ceil-round",
        "[fn main [] [println [floor -1.5]] [println [ceil 1.2]] [println [round 2.5]]]",
        "-2\n2\n3",
    );
    assert_parity("floor-int", "[fn main [] [println [floor 7]]]", "7");
    assert_parity("exp-zero", "[fn main [] [println [exp 0]]]", "1");
    assert_parity("log-e", "[fn main [] [println [log [exp 1]]]]", "1");
    assert_parity("sin-zero", "[fn main [] [println [sin 0]]]", "0");
    assert_parity("atan2", "[fn main [] [println [atan2 0 1]]]", "0");
    assert_parity(
        "constants",
        "[fn main [] [println [floor [* pi 100]]] [println [floor [* e 100]]]]",
        "314\n271",
    );
}

#[test]
fn parse_builtins_parity() {
    assert_parity(
        "parse-int",
        r#"[fn main []
             [match [parse-int "42"] [Some n] [println [+ n 1]] None [println "no"]]
             [match [parse-int "nope"] [Some n] [println n] None [println "no"]]]"#,
        "43\nno",
    );
    assert_parity(
        "parse-float",
        r#"[fn main []
             [match [parse-float "2.5"] [Some x] [println [* x 2.0]] None [println "no"]]
             [match [parse-float "x"] [Some x] [println x] None [println "no"]]]"#,
        "5\nno",
    );
}

#[test]
fn string_helpers_parity() {
    assert_parity(
        "capitalize",
        r#"[fn main [] [println [capitalize "loon"]] [println [capitalize ""]]]"#,
        "Loon\n",
    );
    assert_parity(
        "pad",
        r#"[fn main [] [println [pad-left "5" 3 "0"]] [println [pad-right "ab" 4 "-"]]]"#,
        "005\nab--",
    );
    assert_parity(
        "repeat",
        r#"[fn main [] [println [repeat "ab" 3]]]"#,
        "ababab",
    );
}

#[test]
fn reduce_is_fold_alias() {
    assert_parity(
        "reduce",
        "[fn main [] [println [reduce 0 [fn [acc x] [+ acc x]] #[1 2 3 4]]]]",
        "10",
    );
    assert_parity(
        "fold-still-works",
        "[fn main [] [println [fold 0 [fn [acc x] [+ acc x]] #[1 2 3 4]]]]",
        "10",
    );
}

#[test]
fn index_of_vector_parity() {
    assert_parity(
        "index-of-vec",
        "[fn main [] [println [index-of #[10 20 30] 20]] [println [index-of #[1 2] 9]]]",
        "1\n-1",
    );
}

#[test]
fn rand_effect_is_seeded_and_deterministic_across_backends() {
    // Randomness flows through the Rand *effect* (record/replay-able), and
    // both backends share the same PRNG, so a seeded program is identical
    // everywhere.
    let src = "[fn main []
                 [Rand.seed 42]
                 [println [Rand.rand-int 0 1000]]
                 [println [Rand.rand-int 0 1000]]
                 [println [< [Rand.rand] 1.0]]
                 [println [>= [Rand.rand] 0.0]]]";
    let vm = eir_output(src);
    let interp = interp_output(src);
    assert_eq!(vm, interp, "seeded Rand diverges between backends\n{src}");
    let lines: Vec<&str> = vm.lines().collect();
    assert_eq!(lines.len(), 4);
    assert_eq!(lines[2], "true");
    assert_eq!(lines[3], "true");

    // Same seed → same sequence on a fresh run.
    assert_eq!(vm, eir_output(src));
}

#[test]
fn rand_effect_can_be_handled_in_language() {
    // A user handler overrides the builtin implementation — the basis for
    // deterministic tests without seeding.
    assert_parity(
        "rand-handled",
        "[fn main []
           [println [handle [Rand.rand-int 0 100] [Rand.rand-int lo hi] [resume 7]]]]",
        "7",
    );
}

#[test]
fn radix_literals_parity() {
    assert_parity(
        "radix",
        "[fn main [] [println [+ 0xFF 0]] [println 0o17] [println 0b1010] [println -0x10] [println 0xdead_beef]]",
        "255\n15\n10\n-16\n3735928559",
    );
}

#[test]
fn sqrt_on_int_literal_typechecks() {
    // Regression: [sqrt 4] used to fail with "cannot unify Float with Int".
    let exprs = parse("[fn main [] [println [sqrt 4]]]").unwrap();
    let mut checker = Checker::new();
    checker.check_program(&exprs);
    assert!(
        checker.errors.is_empty(),
        "sqrt on Int literal should typecheck: {:?}",
        checker
            .errors
            .iter()
            .map(|e| e.message())
            .collect::<Vec<_>>()
    );
}
