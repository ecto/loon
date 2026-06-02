//! Differential execution tests for the wasm backend.
//!
//! Every sample is exercised through *both* execution paths of the real CLI:
//!   - `loon run <file>`          — the EIR register VM (authoritative)
//!   - `loon run <file> --wasm`   — compile to standalone wasm + run on wasmtime
//!
//! For samples the wasm backend supports, the two paths must execute
//! successfully and produce byte-identical stdout. This is true *differential*
//! testing: there are no golden files to drift — the interpreter is the oracle.
//!
//! For samples that hit a documented backend limitation (delimited
//! continuations, floats, or a not-yet-ported stdlib builtin), the wasm path
//! must be *rejected cleanly at compile time* with an error naming the reason —
//! never silently miscompiled. That invariant is what keeps the supported set
//! honest: a sample can only leave the "unsupported" table by actually running.

use std::path::PathBuf;
use std::process::Command;

fn samples_dir() -> PathBuf {
    // crates/loon-cli -> workspace root -> samples
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("..")
        .join("samples")
}

struct Run {
    ok: bool,
    stdout: String,
    stderr: String,
}

fn run(sample: &str, wasm: bool) -> Run {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_loon"));
    // Run from the workspace root so that samples reading workspace-relative
    // input (e.g. `IO.read-file "samples/input.txt"`) resolve correctly,
    // exactly as a user invoking `loon run samples/foo.oo` from the repo root.
    cmd.current_dir(samples_dir().parent().expect("workspace root"));
    cmd.arg("run").arg(samples_dir().join(sample));
    if wasm {
        cmd.arg("--wasm");
    }
    let out = cmd
        .output()
        .unwrap_or_else(|e| panic!("failed to spawn loon for {sample}: {e}"));
    Run {
        ok: out.status.success(),
        stdout: String::from_utf8_lossy(&out.stdout).into_owned(),
        stderr: String::from_utf8_lossy(&out.stderr).into_owned(),
    }
}

/// Samples the wasm backend fully supports: must run on both backends and
/// produce identical stdout.
const SUPPORTED: &[&str] = &[
    "hello.oo",
    "fib-simple.oo",
    "fib.oo",
    "compiled-fib.oo",
    "pipeline.oo",
    "multi-arity.oo",
];

#[test]
fn supported_samples_match_interpreter() {
    for &name in SUPPORTED {
        let interp = run(name, false);
        assert!(
            interp.ok,
            "interpreter failed for {name}:\n{}",
            interp.stderr
        );

        let wasm = run(name, true);
        assert!(
            wasm.ok,
            "wasm backend failed for {name} (it is listed as SUPPORTED):\n{}",
            wasm.stderr
        );

        assert_eq!(
            interp.stdout, wasm.stdout,
            "stdout diverged between interpreter and wasm for {name}"
        );
    }
}

/// Samples that exercise a feature the wasm backend does not implement yet.
/// Each must be rejected by codegen with an error containing the given reason.
/// The interpreter, by contrast, must run them — proving the program itself is
/// valid and the boundary is a backend gap, not a broken sample.
const UNSUPPORTED: &[(&str, &str)] = &[
    // Algebraic effects via delimited continuations.
    ("effects.oo", "delimited continuations"),
    ("user-effects.oo", "delimited continuations"),
    ("tco-stress.oo", "delimited continuations"),
    // `state.oo` is also a continuations program; its handler desugars to a
    // call form the backend does not recognize. (Nicer would be the explicit
    // "delimited continuations" message — see continuations support work.)
    ("state.oo", "unsupported call form"),
    // Floating-point arithmetic (the value model is untagged i64).
    ("physics.oo", "floating-point"),
    ("types.oo", "floating-point"),
    // Collection / string stdlib builtins not yet ported to codegen.
    ("bench-collections.oo", "unknown function 'cons'"),
    ("word-count.oo", "unknown function 'split'"),
    ("word-freq.oo", "unknown function 'split'"),
];

#[test]
fn unsupported_samples_reject_cleanly() {
    for &(name, reason) in UNSUPPORTED {
        // The program is valid: the interpreter runs it.
        let interp = run(name, false);
        assert!(
            interp.ok,
            "interpreter unexpectedly failed for {name}; is it still a valid program?\n{}",
            interp.stderr
        );

        // The wasm backend must refuse it, naming the reason — not miscompile.
        let wasm = run(name, true);
        assert!(
            !wasm.ok,
            "wasm backend unexpectedly *succeeded* for {name}; if the gap is \
             closed, move it to SUPPORTED"
        );
        assert!(
            wasm.stderr.contains(reason),
            "wasm rejection for {name} should mention {reason:?}, got:\n{}",
            wasm.stderr
        );
    }
}

/// KNOWN BUG (tracked, not yet fixed): codegen *accepts* `test-suite.oo` and
/// emits a module that wasmtime rejects at translation time ("WebAssembly
/// translation error") — instead of either running it or rejecting it cleanly
/// at compile time like every other unsupported sample. Surfaced by the
/// differential sweep. Remove `#[ignore]` once codegen either compiles it
/// correctly (then it belongs in SUPPORTED) or rejects it cleanly (then it
/// belongs in UNSUPPORTED).
#[test]
#[ignore = "known miscompile: emits invalid wasm; see comment"]
fn test_suite_should_not_miscompile() {
    let interp = run("test-suite.oo", false);
    assert!(interp.ok, "interpreter failed for test-suite.oo");

    let wasm = run("test-suite.oo", true);
    assert!(
        wasm.ok,
        "wasm backend failed for test-suite.oo:\n{}",
        wasm.stderr
    );
    assert_eq!(interp.stdout, wasm.stdout, "stdout diverged for test-suite.oo");
}
