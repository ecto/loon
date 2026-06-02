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
    use std::sync::atomic::{AtomicU64, Ordering};
    // Unique per call so parallel tests (and the three backends within a test)
    // never share a source file.
    static SEQ: AtomicU64 = AtomicU64::new(0);
    let n = SEQ.fetch_add(1, Ordering::Relaxed);
    let dir = std::env::temp_dir().join(format!("loon-diff-{}-{}", std::process::id(), n));
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

/// Regression: all three backends treat structurally-equal strings as the same
/// map key. The EIR VM used to key maps by object handle, so two separately
/// built equal strings (e.g. words from `split`) counted as distinct keys and
/// were not aggregated; the VM now interns string objects by content, matching
/// the legacy interpreter and the wasm backend. This is what makes
/// `word-count.oo` aggregate correctly on every backend.
#[test]
fn map_keys_use_structural_equality_on_all_backends() {
    let src = r#"[fn main []
        [let m [assoc [assoc {} [str "a" "b"] 1] [str "a" "b"] 2]]
        [println [len m]]]"#;
    assert_eq!(run_src(src, &[]), "1", "EIR VM should dedup equal string keys");
    assert_eq!(run_src(src, &["--legacy"]), "1", "legacy should dedup");
    assert_eq!(run_src(src, &["--wasm"]), "1", "wasm should dedup");
}

/// Regression: the `IO.read-file` host import reads a file into the guest heap
/// and returns a loon string identical to the VM's.
#[test]
fn io_read_file_matches_on_wasm() {
    use std::io::Write;
    let dir = std::env::temp_dir().join(format!("loon-rf-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let file = dir.join("data.txt");
    std::fs::File::create(&file)
        .unwrap()
        .write_all(b"hello loon file io")
        .unwrap();
    let path = file.to_string_lossy().replace('\\', "/");
    let src = format!(r#"[fn main [] [println [str [IO.read-file "{path}"]]]]"#);
    let vm = run_src(&src, &[]);
    let wasm = run_src(&src, &["--wasm"]);
    assert_eq!(vm, "hello loon file io", "unexpected VM output: {vm}");
    assert_eq!(vm, wasm, "IO.read-file diverged between VM and wasm");
}

/// Regression: `take`/`drop` clamp their count to the collection length instead
/// of panicking when it exceeds the length (the VM previously panicked inside
/// `imbl::Vector::split_off`).
#[test]
fn take_drop_clamp_past_end_on_vm() {
    let take = r#"[fn main [] [println [len [take 99 [range 0 4]]]]]"#;
    let drop = r#"[fn main [] [println [len [drop 99 [range 0 4]]]]]"#;
    assert_eq!(run_src(take, &[]), "4");
    assert_eq!(run_src(drop, &[]), "0");
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
    "word-count.oo",
    // Full pipeline: split/filter/map lowercase/group-by/entries/tuples/
    // sort-by/take/each + fmt interpolation + IO.read-file (host bridge) +
    // IO.println.
    "word-freq.oo",
    // No `main` and only `[test]` blocks: a no-op under `loon run`, and now a
    // no-op (empty main) on wasm too.
    "test-suite.oo",
    // Floating-point: f64 literals/arithmetic, ADT f64 fields, float-returning
    // functions, and VM-matching float formatting.
    "types.oo",
    // Tail-resumptive algebraic effect handlers (`handle`/`resume`).
    "effects.oo",
    "user-effects.oo",
    // Dimensional units (compile-time; `unit`/`magnitude` are runtime
    // identities), a `Const` value, and a tail-resumptive Physics handler.
    "physics.oo",
    // TCO stress: proper tail calls (named/mutual/through if-do-when-match) +
    // `try` (Fail handler) + keyword/bool `str` formatting; runs on a large
    // stack with a 1 GiB heap for the O(n²) vector build.
    "tco-stress.oo",
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
    // `state.oo` resumes a captured continuation *after* `handle` returns
    // (escaping, multi-shot) — its handler desugars to a call form the
    // tail-resumptive backend does not recognize.
    ("state.oo", "unsupported call form"),
    // Collection / string stdlib builtins not yet ported to codegen.
    // (bench-collections additionally builds 100K-element vectors, which the
    // copy-on-write vector representation cannot do in reasonable time/space.)
    ("bench-collections.oo", "unknown function 'cons'"),
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
/// blocks, but no `main`. Under `loon run` this is a no-op (functions/tests are
/// defined, nothing runs at top level); the wasm backend synthesizes an empty
/// `main` so it is a no-op there too — running cleanly with no output.
///
/// Regression guard: codegen used to leave the (unreachable) functions in the
/// module with stale provisional indices when there was no `main`, emitting a
/// module wasmtime rejected at translation time. Tree-shaking prunes them so
/// the module is valid; this test ensures it runs (not "translation"/"codegen"
/// rejected) and produces no output, matching the interpreter.
#[test]
fn definitions_only_file_runs_as_noop_on_wasm() {
    let interp = run("test-suite.oo", false);
    assert!(interp.ok, "interpreter failed for test-suite.oo:\n{}", interp.stderr);

    let wasm = run("test-suite.oo", true);
    assert!(
        wasm.ok,
        "test-suite.oo should run as a no-op on wasm, got:\n{}",
        wasm.stderr
    );
    assert_eq!(
        wasm.stdout.trim(),
        interp.stdout.trim(),
        "definitions-only file should produce the same (empty) output on both backends"
    );
    // Must not be the old miscompile or a codegen rejection.
    assert!(
        !wasm.stderr.contains("translation") && !wasm.stderr.contains("codegen"),
        "test-suite.oo should compile to valid wasm, not crash/reject:\n{}",
        wasm.stderr
    );
}
