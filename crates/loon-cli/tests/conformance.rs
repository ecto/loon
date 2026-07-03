//! Cross-backend semantic conformance suite.
//!
//! Every program in `tests/conformance/` runs through the REAL CLI on all
//! three backends:
//!
//!   - `loon run <file>`           — the EIR VM (the default backend, and the
//!                                   ORACLE the others are compared against)
//!   - `loon run <file> --legacy`  — the tree-walking interpreter
//!   - `loon run <file> --wasm`    — codegen to standalone wasm + wasmtime
//!
//! A backend CONFORMS on a program when it produces byte-identical stdout and
//! the same success/failure exit class as the EIR VM. There are no golden
//! files to drift: the oracle is executable.
//!
//! Known divergences are not skipped — they are DECLARED in the program
//! header and enforced in both directions:
//!
//!   ; expect-fail: legacy — <reason>
//!   ; expect-fail: wasm — <reason>
//!
//! An annotated backend MUST diverge from the oracle; if it starts
//! conforming, the suite fails so the annotation gets retired. That is what
//! keeps the divergence list honest.
//!
//! The deterministic subset of `samples/` is held to the same rule for the
//! VM/legacy pair (the wasm side of samples is covered by differential.rs),
//! with the expected-fail list kept in `SAMPLE_EXPECT_FAIL_LEGACY` below.

use std::path::{Path, PathBuf};
use std::process::Command;

fn manifest_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn corpus_dir() -> PathBuf {
    manifest_dir().join("tests").join("conformance")
}

fn workspace_root() -> PathBuf {
    manifest_dir()
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf()
}

#[derive(Debug, PartialEq)]
struct Outcome {
    ok: bool,
    stdout: String,
}

/// Run `loon run <file> [flag]` with a fresh scratch directory as cwd (so
/// programs that write relative files are isolated per backend run).
fn run_in_scratch(file: &Path, flag: Option<&str>) -> Outcome {
    static SEQ: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
    let n = SEQ.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    let cwd = std::env::temp_dir().join(format!("loon-conf-{}-{n}", std::process::id()));
    std::fs::create_dir_all(&cwd).unwrap();
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_loon"));
    cmd.current_dir(&cwd).arg("run").arg(file);
    if let Some(f) = flag {
        cmd.arg(f);
    }
    let out = cmd
        .output()
        .unwrap_or_else(|e| panic!("spawn loon for {file:?}: {e}"));
    let _ = std::fs::remove_dir_all(&cwd);
    Outcome {
        ok: out.status.success(),
        stdout: String::from_utf8_lossy(&out.stdout).into_owned(),
    }
}

/// Run `loon run <file> [flag]` from the workspace root (samples read
/// workspace-relative input like `samples/input.txt`).
fn run_from_root(file: &Path, flag: Option<&str>) -> Outcome {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_loon"));
    cmd.current_dir(workspace_root()).arg("run").arg(file);
    if let Some(f) = flag {
        cmd.arg(f);
    }
    let out = cmd
        .output()
        .unwrap_or_else(|e| panic!("spawn loon for {file:?}: {e}"));
    Outcome {
        ok: out.status.success(),
        stdout: String::from_utf8_lossy(&out.stdout).into_owned(),
    }
}

/// Backends a corpus program declares as expected-fail, parsed from leading
/// `; expect-fail: <backend> — <reason>` comment lines.
#[derive(Default)]
struct ExpectFail {
    legacy: bool,
    wasm: bool,
}

fn parse_directives(src: &str) -> ExpectFail {
    let mut ef = ExpectFail::default();
    for line in src.lines() {
        let t = line.trim();
        if !t.starts_with(';') {
            // Directives live in the leading comment block only.
            if !t.is_empty() {
                break;
            }
            continue;
        }
        let t = t.trim_start_matches(';').trim();
        if let Some(rest) = t.strip_prefix("expect-fail:") {
            let backend = rest.trim().split_whitespace().next().unwrap_or("");
            match backend {
                "legacy" => ef.legacy = true,
                "wasm" => ef.wasm = true,
                other => panic!("unknown expect-fail backend {other:?} in directive: {t}"),
            }
        }
    }
    ef
}

#[test]
fn conformance_corpus() {
    let dir = corpus_dir();
    let mut files: Vec<PathBuf> = std::fs::read_dir(&dir)
        .unwrap_or_else(|e| panic!("read {dir:?}: {e}"))
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| p.extension().is_some_and(|x| x == "oo"))
        .collect();
    files.sort();
    assert!(!files.is_empty(), "empty conformance corpus at {dir:?}");

    let mut failures: Vec<String> = Vec::new();
    for file in &files {
        let name = file.file_name().unwrap().to_string_lossy().into_owned();
        let src = std::fs::read_to_string(file).unwrap();
        let ef = parse_directives(&src);

        let oracle = run_in_scratch(file, None);
        for (backend, flag, expected_fail) in [
            ("legacy", "--legacy", ef.legacy),
            ("wasm", "--wasm", ef.wasm),
        ] {
            let got = run_in_scratch(file, Some(flag));
            let conforms = got == oracle;
            match (conforms, expected_fail) {
                (true, false) => {}
                (false, true) => {}
                (false, false) => failures.push(format!(
                    "{name} [{backend}] DIVERGED from the EIR VM:\n  vm:      ok={} stdout={:?}\n  {backend}: ok={} stdout={:?}",
                    oracle.ok, oracle.stdout, got.ok, got.stdout
                )),
                (true, true) => failures.push(format!(
                    "{name} [{backend}] now CONFORMS — retire its `; expect-fail: {backend}` annotation"
                )),
            }
        }
    }
    assert!(
        failures.is_empty(),
        "conformance failures ({}/{} programs):\n{}",
        failures.len(),
        files.len(),
        failures.join("\n")
    );
}

/// Deterministic samples that must run identically on the EIR VM and the
/// legacy interpreter. (Random/Clock/network samples are excluded; the wasm
/// side of samples is exercised by differential.rs.)
const SAMPLES: &[&str] = &[
    "hello.oo",
    "fib-simple.oo",
    "fib.oo",
    "compiled-fib.oo",
    "pipeline.oo",
    "multi-arity.oo",
    "test-suite.oo",
    "types.oo",
    "effects.oo",
    "user-effects.oo",
    "physics.oo",
    "tco-stress.oo",
    "word-count.oo",
    "word-freq.oo",
    "state.oo",
    "multishot.oo",
];

/// Samples where the legacy interpreter is KNOWN to diverge from the VM.
/// Same contract as the corpus annotations: each entry must keep diverging;
/// when one starts conforming, the suite fails so the entry gets retired.
const SAMPLE_EXPECT_FAIL_LEGACY: &[(&str, &str)] = &[
    (
        "word-count.oo",
        "map iteration order differs between backends",
    ),
    (
        "word-freq.oo",
        "map iteration order differs between backends",
    ),
    (
        "state.oo",
        "interp cannot run the CPS State handler ([[handle ...] init])",
    ),
    (
        "multishot.oo",
        "multi-shot resume totals diverge (VM 198, interp 66)",
    ),
];

#[test]
fn deterministic_samples_match_on_vm_and_legacy() {
    let samples = workspace_root().join("samples");
    let mut failures: Vec<String> = Vec::new();
    for &name in SAMPLES {
        let file = samples.join(name);
        let expected_fail = SAMPLE_EXPECT_FAIL_LEGACY.iter().any(|(n, _)| *n == name);
        let oracle = run_from_root(&file, None);
        let legacy = run_from_root(&file, Some("--legacy"));
        let conforms = legacy == oracle;
        match (conforms, expected_fail) {
            (true, false) => {}
            (false, true) => {}
            (false, false) => failures.push(format!(
                "{name} [legacy] DIVERGED from the EIR VM:\n  vm:     ok={} stdout={:?}\n  legacy: ok={} stdout={:?}",
                oracle.ok, oracle.stdout, legacy.ok, legacy.stdout
            )),
            (true, true) => failures.push(format!(
                "{name} [legacy] now CONFORMS — retire its SAMPLE_EXPECT_FAIL_LEGACY entry"
            )),
        }
    }
    assert!(
        failures.is_empty(),
        "sample conformance failures:\n{}",
        failures.join("\n")
    );
}
