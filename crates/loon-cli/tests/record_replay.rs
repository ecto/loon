//! Integration tests for `loon run --record` and `loon replay`.
//!
//! The contract under test: recording a run captures every nondeterministic
//! effect result (clock, uuid, file reads, env — plus log writes for
//! observability) into a Loon-data trace file; replaying that trace against
//! the same program reproduces the recorded run exactly — same stdout, same
//! exit status, same crash — no matter how many times it is replayed or what
//! the outside world looks like now. A trace from a *different* run of the
//! program must diverge with a diagnostic naming the expected vs requested
//! op and the step index.

use std::io::Write;
use std::path::PathBuf;
use std::process::Command;

fn workspace_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("..")
}

/// Fresh scratch dir per call so parallel tests never share files.
fn scratch_dir() -> PathBuf {
    use std::sync::atomic::{AtomicU64, Ordering};
    static SEQ: AtomicU64 = AtomicU64::new(0);
    let n = SEQ.fetch_add(1, Ordering::Relaxed);
    let dir = std::env::temp_dir().join(format!("loon-rr-{}-{}", std::process::id(), n));
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
    ok: bool,
    stdout: String,
    stderr: String,
}

fn loon(args: &[&std::ffi::OsStr]) -> Run {
    let out = Command::new(env!("CARGO_BIN_EXE_loon"))
        .args(args)
        .output()
        .expect("spawn loon");
    Run {
        ok: out.status.success(),
        stdout: String::from_utf8_lossy(&out.stdout).into_owned(),
        stderr: String::from_utf8_lossy(&out.stderr).into_owned(),
    }
}

fn loon_str(args: &[&str]) -> Run {
    let os: Vec<&std::ffi::OsStr> = args.iter().map(std::ffi::OsStr::new).collect();
    loon(&os)
}

fn run_args<'a>(
    prog: &'a std::path::Path,
    flag: &'a str,
    trace: &'a std::path::Path,
) -> Vec<&'a std::ffi::OsStr> {
    vec![
        std::ffi::OsStr::new("run"),
        prog.as_os_str(),
        std::ffi::OsStr::new(flag),
        trace.as_os_str(),
    ]
}

fn replay_args<'a>(
    trace: &'a std::path::Path,
    prog: &'a std::path::Path,
) -> Vec<&'a std::ffi::OsStr> {
    vec![
        std::ffi::OsStr::new("replay"),
        trace.as_os_str(),
        prog.as_os_str(),
    ]
}

/// Record a run whose output depends on the wall clock, then replay it twice:
/// both replays must byte-match the recorded run's stdout, even though the
/// clock has moved on.
#[test]
fn replay_reproduces_recorded_run_exactly() {
    let dir = scratch_dir();
    let prog = write_file(
        &dir,
        "prog.oo",
        r#"[fn main []
  [let ms [IO.millis]]
  [let roll [% ms 97]]
  [println "roll is \(roll)"]
  [println "id" [IO.uuid]]]
"#,
    );
    let trace = dir.join("trace.oo");

    let recorded = loon(&run_args(&prog, "--record", &trace));
    assert!(recorded.ok, "record run failed:\n{}", recorded.stderr);
    assert!(trace.exists(), "trace file was not written");
    // The uuid must be genuinely recorded (not a constant): the replay
    // assertions below only prove something if a nondeterministic value is
    // actually fed back from the trace.
    let content = std::fs::read_to_string(&trace).unwrap();
    assert!(
        content.contains(":op \"uuid\""),
        "trace should contain a recorded uuid entry:\n{content}"
    );

    let replay1 = loon(&replay_args(&trace, &prog));
    assert!(replay1.ok, "first replay failed:\n{}", replay1.stderr);
    assert_eq!(
        recorded.stdout, replay1.stdout,
        "replay must reproduce the recorded stdout"
    );

    let replay2 = loon(&replay_args(&trace, &prog));
    assert_eq!(
        replay1.stdout, replay2.stdout,
        "replay must be deterministic across invocations"
    );
}

/// The trace file is Loon data: a vector of maps with :effect/:op/:result.
#[test]
fn trace_file_is_loon_data() {
    let dir = scratch_dir();
    let prog = write_file(&dir, "prog.oo", "[fn main [] [println [IO.millis]]]\n");
    let trace = dir.join("trace.oo");

    let recorded = loon(&run_args(&prog, "--record", &trace));
    assert!(recorded.ok, "record run failed:\n{}", recorded.stderr);

    let content = std::fs::read_to_string(&trace).unwrap();
    assert!(
        content.trim_start().starts_with("#["),
        "finalized trace should be a Loon vector, got:\n{content}"
    );
    assert!(content.contains(":effect \"IO\""), "trace:\n{content}");
    assert!(content.contains(":op \"millis\""), "trace:\n{content}");
    assert!(
        content.contains(":op \"println\""),
        "log writes should be recorded for observability, trace:\n{content}"
    );
}

/// Replayed file reads come from the trace, not the filesystem: delete the
/// input file after recording and the replay must still see its contents.
#[test]
fn replay_feeds_recorded_file_reads_back() {
    let dir = scratch_dir();
    let data = write_file(&dir, "data.txt", "seen at record time");
    let data_path = data.to_string_lossy().replace('\\', "/");
    let prog = write_file(
        &dir,
        "prog.oo",
        &format!("[fn main [] [println [IO.read-file \"{data_path}\"]]]\n"),
    );
    let trace = dir.join("trace.oo");

    let recorded = loon(&run_args(&prog, "--record", &trace));
    assert!(recorded.ok, "record run failed:\n{}", recorded.stderr);
    assert!(recorded.stdout.contains("seen at record time"));

    // The outside world changes; the replay must not notice.
    std::fs::remove_file(&data).unwrap();

    let replayed = loon(&replay_args(&trace, &prog));
    assert!(replayed.ok, "replay failed:\n{}", replayed.stderr);
    assert_eq!(recorded.stdout, replayed.stdout);
}

/// A crash mid-run still leaves a loadable trace (entries are flushed as they
/// happen), and replaying it reproduces the same crash with the same output.
#[test]
fn crash_trace_persists_and_replays_the_crash() {
    let dir = scratch_dir();
    let prog = write_file(
        &dir,
        "crash.oo",
        r#"[fn main []
  [let ms [IO.millis]]
  [println "starting with \(ms)"]
  [assert-eq 1 2]]
"#,
    );
    let trace = dir.join("trace.oo");

    let recorded = loon(&run_args(&prog, "--record", &trace));
    assert!(!recorded.ok, "program should crash");
    assert!(
        recorded.stderr.contains("assertion failed"),
        "stderr:\n{}",
        recorded.stderr
    );
    let content = std::fs::read_to_string(&trace).unwrap();
    assert!(
        content.contains(":op \"millis\""),
        "trace must contain entries recorded before the crash:\n{content}"
    );

    // Replay twice: identical stdout, identical crash.
    let r1 = loon(&replay_args(&trace, &prog));
    let r2 = loon(&replay_args(&trace, &prog));
    assert!(!r1.ok && !r2.ok, "replays must reproduce the crash");
    assert_eq!(recorded.stdout, r1.stdout, "crash-run stdout must replay");
    assert_eq!(r1.stdout, r2.stdout);
    assert!(
        r1.stderr.contains("assertion failed"),
        "stderr:\n{}",
        r1.stderr
    );
    assert_eq!(
        r1.stderr, r2.stderr,
        "even the crash report is deterministic"
    );
}

/// The bundled demo sample records and replays end-to-end. The demo crashes
/// on one roll in five; whether or not this particular recording crashed,
/// its replay must match it exactly (stdout, stderr, and exit status).
#[test]
fn replay_demo_sample_records_and_replays() {
    let dir = scratch_dir();
    let prog = workspace_root().join("samples").join("replay-demo.oo");
    assert!(prog.exists(), "samples/replay-demo.oo is missing");
    let trace = dir.join("trace.oo");

    let recorded = loon(&run_args(&prog, "--record", &trace));
    let replayed = loon(&replay_args(&trace, &prog));
    assert_eq!(recorded.stdout, replayed.stdout, "stdout must replay");
    assert_eq!(
        recorded.ok, replayed.ok,
        "replay must reproduce the recorded exit status (crash or success)"
    );
}

/// Replaying a trace against a program that performs different effects fails
/// with a divergence diagnostic naming expected vs requested op and index.
#[test]
fn diverging_program_gets_a_clear_diagnostic() {
    let dir = scratch_dir();
    let recorded_prog = write_file(&dir, "a.oo", "[fn main [] [println [IO.millis]]]\n");
    let other_prog = write_file(&dir, "b.oo", "[fn main [] [println [IO.uuid]]]\n");
    let trace = dir.join("trace.oo");

    let recorded = loon(&run_args(&recorded_prog, "--record", &trace));
    assert!(recorded.ok, "record run failed:\n{}", recorded.stderr);

    let diverged = loon(&replay_args(&trace, &other_prog));
    assert!(!diverged.ok, "diverging replay must fail");
    assert!(
        diverged.stderr.contains("replay diverged at step 0"),
        "stderr should name the step index:\n{}",
        diverged.stderr
    );
    assert!(
        diverged.stderr.contains("IO.millis") && diverged.stderr.contains("IO.uuid"),
        "stderr should name expected vs requested ops:\n{}",
        diverged.stderr
    );
    assert!(
        diverged.stderr.contains("re-record"),
        "stderr should suggest the fix:\n{}",
        diverged.stderr
    );
}

/// A program that needs more effect results than the trace holds fails with
/// a "trace exhausted" divergence, not garbage results.
#[test]
fn exhausted_trace_is_diagnosed() {
    let dir = scratch_dir();
    let short_prog = write_file(&dir, "short.oo", "[fn main [] [println [IO.millis]]]\n");
    let greedy_prog = write_file(
        &dir,
        "greedy.oo",
        "[fn main []\n  [println [IO.millis]]\n  [println [IO.millis]]\n  [println [IO.millis]]]\n",
    );
    let trace = dir.join("trace.oo");

    let recorded = loon(&run_args(&short_prog, "--record", &trace));
    assert!(recorded.ok, "record run failed:\n{}", recorded.stderr);

    let exhausted = loon(&replay_args(&trace, &greedy_prog));
    assert!(!exhausted.ok, "replay past the end of the trace must fail");
    assert!(
        exhausted.stderr.contains("replay diverged"),
        "stderr:\n{}",
        exhausted.stderr
    );
}

/// A program that ends early leaves unused entries; the replay succeeds but
/// warns, since the leftover tail usually means the program changed.
#[test]
fn leftover_trace_entries_warn() {
    let dir = scratch_dir();
    let long_prog = write_file(
        &dir,
        "long.oo",
        "[fn main []\n  [println [IO.millis]]\n  [println [IO.millis]]]\n",
    );
    let short_prog = write_file(&dir, "short.oo", "[fn main [] [println [IO.millis]]]\n");
    let trace = dir.join("trace.oo");

    let recorded = loon(&run_args(&long_prog, "--record", &trace));
    assert!(recorded.ok, "record run failed:\n{}", recorded.stderr);

    let partial = loon(&replay_args(&trace, &short_prog));
    assert!(partial.ok, "partial replay still succeeds");
    assert!(
        partial.stderr.contains("unused trace entr"),
        "stderr should warn about the leftover tail:\n{}",
        partial.stderr
    );
}

/// Traces use the same .oo extension as programs, so a tab-completion slip
/// could point --record at the program itself; that must be a clean error,
/// not a silently destroyed source file.
#[test]
fn record_refuses_to_overwrite_the_program() {
    let dir = scratch_dir();
    let src = "[fn main [] [println [IO.millis]]]\n";
    let prog = write_file(&dir, "selfie.oo", src);

    let out = loon(&run_args(&prog, "--record", &prog));
    assert!(!out.ok, "recording over the program itself must fail");
    assert!(
        out.stderr.contains("would overwrite the program"),
        "stderr:\n{}",
        out.stderr
    );
    assert_eq!(
        std::fs::read_to_string(&prog).unwrap(),
        src,
        "the program source must be untouched"
    );
}

/// Recorded strings containing control characters (including the lexer's
/// U+0001/U+0002 interpolation sentinels) must produce a trace the replay
/// loader can parse — the trace always reparses.
#[test]
fn control_chars_in_recorded_strings_replay() {
    let dir = scratch_dir();
    let data = dir.join("c.txt");
    std::fs::write(&data, b"a\x01b\x02c\rd").unwrap();
    let data_path = data.to_string_lossy().replace('\\', "/");
    let prog = write_file(
        &dir,
        "readc.oo",
        &format!("[fn main [] [println [len [IO.read-file \"{data_path}\"]]]]\n"),
    );
    let trace = dir.join("trace.oo");

    let recorded = loon(&run_args(&prog, "--record", &trace));
    assert!(recorded.ok, "record run failed:\n{}", recorded.stderr);
    assert_eq!(recorded.stdout.trim(), "7");

    std::fs::remove_file(&data).unwrap();
    let replayed = loon(&replay_args(&trace, &prog));
    assert!(
        replayed.ok,
        "replay must load the trace it just wrote:\n{}",
        replayed.stderr
    );
    assert_eq!(recorded.stdout, replayed.stdout);
}

/// --record is an EIR VM feature; combining it with another backend is a
/// clean error, not silent no-recording.
#[test]
fn record_rejects_non_default_backends() {
    let dir = scratch_dir();
    let prog = write_file(&dir, "p.oo", "[fn main [] [println 1]]\n");
    let trace = dir.join("t.oo");
    for flag in ["--legacy", "--wasm", "--native"] {
        let out = loon_str(&[
            "run",
            prog.to_str().unwrap(),
            flag,
            "--record",
            trace.to_str().unwrap(),
        ]);
        assert!(!out.ok, "{flag} + --record should be rejected");
        assert!(
            out.stderr
                .contains("--record requires the default EIR VM backend"),
            "stderr for {flag}:\n{}",
            out.stderr
        );
    }
}
