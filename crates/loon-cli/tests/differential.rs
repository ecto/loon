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

/// Run an inline program through a chosen backend, returning trimmed stdout.
fn run_src(src: &str, args: &[&str]) -> String {
    use std::io::Write;
    let dir = std::env::temp_dir().join(format!("loon-diff-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("prog.oo");
    std::fs::File::create(&path)
        .unwrap()
        .write_all(src.as_bytes())
        .unwrap();
    let out = Command::new(env!("CARGO_BIN_EXE_loon"))
        .arg("run")
        .arg(&path)
        .args(args)
        .output()
        .expect("spawn loon");
    String::from_utf8_lossy(&out.stdout).trim().to_string()
}

/// Documents a *known EIR VM bug* uncovered while building the wasm map stdlib:
/// the VM (`loon run`, the differential oracle) keys maps by object handle, so
/// two structurally-equal strings built separately count as distinct keys. The
/// legacy tree-walking interpreter and the wasm backend both treat them as
/// equal (the correct behaviour). This is why `word-count.oo` cannot reach
/// output parity on wasm: the VM does not aggregate split-produced words.
///
/// When the VM is fixed to use structural key equality, this test will fail and
/// should be updated (the `vm` value will become "1").
#[test]
fn vm_map_keys_are_handle_identity_not_structural() {
    let src = r#"[fn main []
        [let m [assoc [assoc {} [str "a" "b"] 1] [str "a" "b"] 2]]
        [println [len m]]]"#;
    let vm = run_src(src, &[]);
    let legacy = run_src(src, &["--legacy"]);
    let wasm = run_src(src, &["--wasm"]);
    assert_eq!(legacy, "1", "legacy interpreter should dedup equal string keys");
    assert_eq!(wasm, "1", "wasm backend should dedup equal string keys");
    assert_eq!(
        vm, "2",
        "KNOWN BUG: the EIR VM keys maps by handle, not structural equality; \
         if this now reports 1 the VM was fixed — update this test and revisit \
         word-count.oo parity"
    );
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
    // word-count compiles further now (split/filter/fold/update/take all land);
    // its next codegen gap is `sort-by`. Note: even once that lands, word-count
    // cannot reach *output* parity, because the EIR VM (the differential oracle)
    // keys maps by object handle rather than structural equality, so
    // split-produced duplicate words are not aggregated there. See the
    // `vm_map_keys_are_handle_identity_not_structural` regression test.
    ("word-count.oo", "unknown function 'sort-by'"),
    ("word-freq.oo", "lowercase"),
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

/// `test-suite.oo` is a definitions-only file: top-level `fn`s plus `test`
/// blocks, but no `main`. The wasm backend's only entry point is `main`, so it
/// is not runnable as wasm — but it must still compile to a *valid* module and
/// fail cleanly, not miscompile.
///
/// Regression guard: codegen used to leave the (unreachable) functions in the
/// module with stale provisional indices when there was no `main`, emitting a
/// module wasmtime rejected at translation time. Now tree-shaking prunes them
/// to a valid empty module. Because `loon run --wasm` validates the module via
/// `wasmtime::Module::new` *before* looking for an entry, reaching the
/// "no _start or main" error proves the emitted wasm is valid.
#[test]
fn definitions_only_file_compiles_valid_but_has_no_entry() {
    // The interpreter accepts it (defines fns/tests, runs nothing at top level).
    let interp = run("test-suite.oo", false);
    assert!(interp.ok, "interpreter failed for test-suite.oo:\n{}", interp.stderr);

    let wasm = run("test-suite.oo", true);
    assert!(!wasm.ok, "expected no-entry failure, but wasm run succeeded");
    assert!(
        wasm.stderr.contains("no _start or main"),
        "expected a clean no-entry error (which implies a valid module), got:\n{}",
        wasm.stderr
    );
    // Specifically must NOT be the old miscompile or a codegen rejection.
    assert!(
        !wasm.stderr.contains("translation") && !wasm.stderr.contains("codegen"),
        "test-suite.oo should compile to valid wasm, not crash/reject:\n{}",
        wasm.stderr
    );
}
