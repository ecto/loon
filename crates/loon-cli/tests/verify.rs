//! Integration tests for `loon verify` — the fix oracle.
//!
//! The contract under test, end to end: record a crashing run of a buggy
//! program; `loon verify` against the unchanged program says REPRODUCED
//! (exit 10); against a program with the bug fixed it says FIXED (exit 0);
//! against a program whose behavior changed before the crash point it says
//! DIVERGED (exit 11). Old traces without a recorded outcome degrade to
//! COMPLETED/CRASHED/DIVERGED with an explicit caveat, and `--json` emits a
//! single machine-readable object.

use std::io::Write;
use std::path::PathBuf;
use std::process::Command;

/// Fresh scratch dir per call so parallel tests never share files.
fn scratch_dir() -> PathBuf {
    use std::sync::atomic::{AtomicU64, Ordering};
    static SEQ: AtomicU64 = AtomicU64::new(0);
    let n = SEQ.fetch_add(1, Ordering::Relaxed);
    let dir = std::env::temp_dir().join(format!("loon-verify-{}-{}", std::process::id(), n));
    std::fs::create_dir_all(&dir).unwrap();
    dir
}

fn write_file(dir: &std::path::Path, name: &str, content: &str) -> PathBuf {
    let path = dir.join(name);
    std::fs::File::create(&path)
        .unwrap()
        .write_all(content.as_bytes())
        .unwrap();
    path
}

struct Run {
    code: Option<i32>,
    stdout: String,
    stderr: String,
}

fn loon(args: &[&str]) -> Run {
    let out = Command::new(env!("CARGO_BIN_EXE_loon"))
        .args(args)
        .output()
        .expect("spawn loon");
    Run {
        code: out.status.code(),
        stdout: String::from_utf8_lossy(&out.stdout).into_owned(),
        stderr: String::from_utf8_lossy(&out.stderr).into_owned(),
    }
}

/// The buggy program: reads the clock (nondeterministic, so the trace has a
/// real recorded op), then unconditionally divides by zero. Crashes on every
/// recording — the crash itself is deterministic, the recorded clock value is
/// not, which is exactly the record/verify use case.
const BUGGY: &str = r#"[fn main []
  [let ms [IO.millis]]
  [println "processing batch \(ms)"]
  [let d [- ms ms]]
  [println [/ 10 d]]]
"#;

/// The fix: guard the zero divisor. Consumes the same recorded prefix
/// (one IO.millis) and completes.
const FIXED: &str = r#"[fn main []
  [let ms [IO.millis]]
  [println "processing batch \(ms)"]
  [let d [- ms ms]]
  [if [= d 0]
    [println "guard: skipping degenerate batch"]
    [println [/ 10 d]]]]
"#;

/// A differently-behaving "fix": performs a different effect op before the
/// crash point. The change altered behavior beyond the bug.
const DIVERGENT: &str = r#"[fn main []
  [let id [IO.uuid]]
  [println "processing batch \(id)"]]
"#;

/// Record a crash of the buggy program into `trace.oo`, returning the paths.
fn record_crash(dir: &std::path::Path) -> (PathBuf, PathBuf) {
    let buggy = write_file(dir, "buggy.oo", BUGGY);
    let trace = dir.join("trace.oo");
    let rec = loon(&[
        "run",
        buggy.to_str().unwrap(),
        "--record",
        trace.to_str().unwrap(),
    ]);
    assert_eq!(rec.code, Some(1), "buggy program must crash on record");
    assert!(
        rec.stderr.contains("division by zero"),
        "stderr:\n{}",
        rec.stderr
    );
    (buggy, trace)
}

/// The full agent loop: record → REPRODUCED → fix → FIXED → wander → DIVERGED.
#[test]
fn agent_loop_reproduced_fixed_diverged() {
    let dir = scratch_dir();
    let (buggy, trace) = record_crash(&dir);

    // The recorded outcome is in the trace as ground truth.
    let content = std::fs::read_to_string(&trace).unwrap();
    assert!(
        content.contains(":outcome \"crash\"")
            && content.contains(":error-class \"divide-by-zero\"")
            && content.contains(":steps 1"),
        "trace must record the outcome:\n{content}"
    );

    // 1. Unfixed program: the bug still exists.
    let repro = loon(&["verify", trace.to_str().unwrap(), buggy.to_str().unwrap()]);
    assert_eq!(repro.code, Some(10), "stderr:\n{}", repro.stderr);
    assert!(
        repro.stdout.contains("REPRODUCED"),
        "stdout:\n{}",
        repro.stdout
    );
    assert!(
        repro.stdout.contains("divide-by-zero") && repro.stdout.contains("step 1"),
        "verdict should name the class and step:\n{}",
        repro.stdout
    );

    // 2. Fixed program: crash gone under the exact recorded world.
    let fixed = write_file(&dir, "fixed.oo", FIXED);
    let ok = loon(&["verify", trace.to_str().unwrap(), fixed.to_str().unwrap()]);
    assert_eq!(ok.code, Some(0), "stderr:\n{}", ok.stderr);
    assert!(ok.stdout.contains("FIXED"), "stdout:\n{}", ok.stdout);

    // 3. Differently-behaving program: altered behavior before the crash.
    let div = write_file(&dir, "divergent.oo", DIVERGENT);
    let bad = loon(&["verify", trace.to_str().unwrap(), div.to_str().unwrap()]);
    assert_eq!(bad.code, Some(11), "stderr:\n{}", bad.stderr);
    assert!(bad.stdout.contains("DIVERGED"), "stdout:\n{}", bad.stdout);
    assert!(
        bad.stdout.contains("IO.millis") && bad.stdout.contains("IO.uuid"),
        "verdict should name expected vs requested ops:\n{}",
        bad.stdout
    );
}

/// A fix may legitimately keep going past the end of the recording (the
/// recording stopped at the crash). Consuming the whole trace without
/// reproducing the crash and then requesting more ops is FIXED, not DIVERGED.
#[test]
fn fix_that_continues_past_the_recording_is_fixed() {
    let dir = scratch_dir();
    let (_buggy, trace) = record_crash(&dir);

    let continues = write_file(
        &dir,
        "continues.oo",
        r#"[fn main []
  [let ms [IO.millis]]
  [println "processing batch \(ms)"]
  [let d [- ms ms]]
  [if [= d 0] [println "guard"] [println [/ 10 d]]]
  [println "next batch \([IO.millis])"]]
"#,
    );
    let out = loon(&[
        "verify",
        trace.to_str().unwrap(),
        continues.to_str().unwrap(),
    ]);
    assert_eq!(
        out.code,
        Some(0),
        "stdout:\n{}\nstderr:\n{}",
        out.stdout,
        out.stderr
    );
    assert!(out.stdout.contains("FIXED"), "stdout:\n{}", out.stdout);
    assert!(
        out.stdout.contains("not verified"),
        "verdict should caveat the unrecorded continuation:\n{}",
        out.stdout
    );
}

/// A program that crashes with a *different* error class is not REPRODUCED
/// and not FIXED — the change altered behavior.
#[test]
fn different_crash_is_diverged() {
    let dir = scratch_dir();
    let (_buggy, trace) = record_crash(&dir);

    let other_crash = write_file(
        &dir,
        "other.oo",
        r#"[fn main []
  [let ms [IO.millis]]
  [println "processing batch \(ms)"]
  [assert-eq 1 2]]
"#,
    );
    let out = loon(&[
        "verify",
        trace.to_str().unwrap(),
        other_crash.to_str().unwrap(),
    ]);
    assert_eq!(out.code, Some(11), "stdout:\n{}", out.stdout);
    assert!(out.stdout.contains("DIVERGED"), "stdout:\n{}", out.stdout);
    assert!(
        out.stdout.contains("assert-failed") && out.stdout.contains("divide-by-zero"),
        "verdict should name both crash classes:\n{}",
        out.stdout
    );
}

/// Old traces (no recorded outcome) still work, but verify says which
/// guarantee it cannot make and uses the degraded verdict names.
#[test]
fn old_trace_without_outcome_degrades_gracefully() {
    let dir = scratch_dir();
    let (buggy, trace) = record_crash(&dir);

    // Strip the outcome map to simulate a trace from an older version.
    let content = std::fs::read_to_string(&trace).unwrap();
    let old: String = content
        .lines()
        .filter(|l| !l.contains(":outcome"))
        .collect::<Vec<_>>()
        .join("\n");
    let old = format!("{}]", old.trim_end().trim_end_matches(']'));
    std::fs::write(&trace, old).unwrap();

    // `loon replay` still accepts it (backward compatibility of the loader).
    let replay = loon(&["replay", trace.to_str().unwrap(), buggy.to_str().unwrap()]);
    assert_eq!(replay.code, Some(1), "stderr:\n{}", replay.stderr);
    assert!(
        replay.stderr.contains("division by zero"),
        "stderr:\n{}",
        replay.stderr
    );

    // Crash without ground truth: CRASHED (exit 10), with the caveat.
    let crashed = loon(&["verify", trace.to_str().unwrap(), buggy.to_str().unwrap()]);
    assert_eq!(crashed.code, Some(10), "stdout:\n{}", crashed.stdout);
    assert!(
        crashed.stdout.contains("CRASHED") && crashed.stdout.contains("no recorded outcome"),
        "stdout:\n{}",
        crashed.stdout
    );

    // Completion without ground truth: COMPLETED (exit 0), with the caveat.
    let fixed = write_file(&dir, "fixed.oo", FIXED);
    let completed = loon(&["verify", trace.to_str().unwrap(), fixed.to_str().unwrap()]);
    assert_eq!(completed.code, Some(0), "stdout:\n{}", completed.stdout);
    assert!(
        completed.stdout.contains("COMPLETED") && completed.stdout.contains("no recorded outcome"),
        "stdout:\n{}",
        completed.stdout
    );
}

/// --json emits exactly one JSON object with the documented fields, on every
/// verdict.
#[test]
fn json_output_shape() {
    let dir = scratch_dir();
    let (buggy, trace) = record_crash(&dir);
    let fixed = write_file(&dir, "fixed.oo", FIXED);
    let div = write_file(&dir, "divergent.oo", DIVERGENT);

    for (prog, verdict, code) in [
        (&buggy, "reproduced", 10),
        (&fixed, "fixed", 0),
        (&div, "diverged", 11),
    ] {
        let out = loon(&[
            "verify",
            trace.to_str().unwrap(),
            prog.to_str().unwrap(),
            "--json",
        ]);
        assert_eq!(out.code, Some(code), "stderr:\n{}", out.stderr);
        // The JSON object is the last stdout line (replayed log writes
        // re-execute live above it).
        let line = out.stdout.lines().last().unwrap_or_default();
        let obj: serde_json::Value =
            serde_json::from_str(line).unwrap_or_else(|e| panic!("bad JSON ({e}): {line}"));
        assert_eq!(obj["verdict"], verdict, "json: {obj}");
        assert_eq!(obj["exit_code"], code, "json: {obj}");
        assert!(obj["detail"].is_string(), "json: {obj}");
        assert!(obj["trace_ops_consumed"].is_u64(), "json: {obj}");
        assert_eq!(obj["trace_ops_total"], 1, "json: {obj}");
        let rec = &obj["recorded_outcome"];
        assert_eq!(rec["status"], "crash", "json: {obj}");
        assert_eq!(rec["error_class"], "divide-by-zero", "json: {obj}");
        assert_eq!(rec["steps"], 1, "json: {obj}");
    }
}

/// Successful recordings carry an :outcome "ok" map, and verifying a program
/// against a clean tape reports FIXED when it still completes and DIVERGED
/// when it now crashes (a regression against the recording).
#[test]
fn ok_outcome_recorded_and_verified() {
    let dir = scratch_dir();
    let fine = write_file(
        &dir,
        "fine.oo",
        "[fn main []\n  [println \"t=\\([IO.millis])\"]]\n",
    );
    let trace = dir.join("trace.oo");
    let rec = loon(&[
        "run",
        fine.to_str().unwrap(),
        "--record",
        trace.to_str().unwrap(),
    ]);
    assert_eq!(rec.code, Some(0), "stderr:\n{}", rec.stderr);
    let content = std::fs::read_to_string(&trace).unwrap();
    assert!(
        content.contains(":outcome \"ok\""),
        "trace should record the ok outcome:\n{content}"
    );

    let ok = loon(&["verify", trace.to_str().unwrap(), fine.to_str().unwrap()]);
    assert_eq!(ok.code, Some(0), "stdout:\n{}", ok.stdout);
    assert!(ok.stdout.contains("FIXED"), "stdout:\n{}", ok.stdout);

    let regressed = write_file(
        &dir,
        "regressed.oo",
        "[fn main []\n  [println \"t=\\([IO.millis])\"]\n  [assert-eq 1 2]]\n",
    );
    let bad = loon(&[
        "verify",
        trace.to_str().unwrap(),
        regressed.to_str().unwrap(),
    ]);
    assert_eq!(bad.code, Some(11), "stdout:\n{}", bad.stdout);
    assert!(bad.stdout.contains("DIVERGED"), "stdout:\n{}", bad.stdout);
}

/// Bad inputs are normal errors (exit 1), never a verdict.
#[test]
fn bad_inputs_exit_one() {
    let dir = scratch_dir();
    let (buggy, trace) = record_crash(&dir);

    // Missing trace file.
    let missing = loon(&["verify", "no-such-trace.oo", buggy.to_str().unwrap()]);
    assert_eq!(missing.code, Some(1), "stderr:\n{}", missing.stderr);

    // Program that does not parse.
    let broken = write_file(&dir, "broken.oo", "[fn main [");
    let bad_prog = loon(&["verify", trace.to_str().unwrap(), broken.to_str().unwrap()]);
    assert_eq!(bad_prog.code, Some(1), "stdout:\n{}", bad_prog.stdout);
    assert!(
        bad_prog.stderr.contains("parse error"),
        "stderr:\n{}",
        bad_prog.stderr
    );

    // One positional argument is a usage error, not package verification.
    let one_arg = loon(&["verify", trace.to_str().unwrap()]);
    assert_eq!(one_arg.code, Some(1), "stderr:\n{}", one_arg.stderr);
    assert!(
        one_arg.stderr.contains("usage"),
        "stderr:\n{}",
        one_arg.stderr
    );
}
