//! Tests for the `loon test` subcommand, which runs `[test ...]` forms on the
//! EIR VM (the reference backend). These lock in that: tests execute on the VM
//! (so handler/effect semantics match `loon run`), a failing `assert-eq` is
//! reported and sets a non-zero exit, and a passing suite exits 0.

use std::io::Write;
use std::process::Command;
use std::sync::atomic::{AtomicU32, Ordering};

struct Out {
    ok: bool,
    stdout: String,
}

static COUNTER: AtomicU32 = AtomicU32::new(0);

fn run_test(source: &str) -> Out {
    // Write to a uniquely-named temp file (no external tempfile dep). Tests run
    // on threads in one process, so disambiguate by an atomic counter as well
    // as the pid to avoid collisions between concurrent test cases.
    let mut path = std::env::temp_dir();
    let n = COUNTER.fetch_add(1, Ordering::Relaxed);
    path.push(format!("loon_test_cmd_{}_{n}.oo", std::process::id()));
    {
        let mut f = std::fs::File::create(&path).expect("create temp");
        f.write_all(source.as_bytes()).expect("write temp");
    }
    let output = Command::new(env!("CARGO_BIN_EXE_loon"))
        .arg("test")
        .arg(&path)
        .env("NO_COLOR", "1") // keep stdout free of ANSI codes for substring asserts
        .output()
        .expect("run loon test");
    let _ = std::fs::remove_file(&path);
    Out {
        ok: output.status.success(),
        stdout: strip_ansi(&String::from_utf8_lossy(&output.stdout)),
    }
}

/// Strip ANSI SGR escape sequences (`ESC [ ... m`) so substring assertions
/// aren't split by color codes (e.g. `pass\x1b[39m adds`).
fn strip_ansi(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        if c == '\x1b' {
            for e in chars.by_ref() {
                if e == 'm' {
                    break;
                }
            }
        } else {
            out.push(c);
        }
    }
    out
}

#[test]
fn passing_suite_exits_zero() {
    let out = run_test("[test adds [] [assert-eq [+ 1 1] 2]]");
    assert!(out.ok, "passing suite should exit 0; stdout:\n{}", out.stdout);
    assert!(out.stdout.contains("pass adds"), "{}", out.stdout);
    assert!(out.stdout.contains("1 passed"), "{}", out.stdout);
}

#[test]
fn failing_assert_fails_and_exits_nonzero() {
    let out = run_test(
        "[test adds [] [assert-eq [+ 1 1] 2]]\n\
         [test breaks [] [assert-eq [+ 1 1] 3]]",
    );
    assert!(!out.ok, "a failing assert must exit non-zero; stdout:\n{}", out.stdout);
    assert!(out.stdout.contains("pass adds"), "{}", out.stdout);
    assert!(out.stdout.contains("FAIL breaks"), "{}", out.stdout);
    assert!(out.stdout.contains("1 passed, 1 failed"), "{}", out.stdout);
}

#[test]
fn tests_run_on_eir_vm_with_handler_semantics() {
    // A test exercising an effect handler must pass on the VM — the whole point
    // of running `loon test` on the EIR backend rather than the tree-walker.
    let out = run_test(
        "[effect Log [note [] Unit]]\n\
         [test handled [] [assert-eq [handle [do [Log.note] 7] [Log.note] [resume 0]] 7]]",
    );
    assert!(out.ok, "handler test should pass on the VM; stdout:\n{}", out.stdout);
    assert!(out.stdout.contains("pass handled"), "{}", out.stdout);
}

#[test]
fn no_tests_reports_and_exits_zero() {
    let out = run_test("[fn main [] [IO.println \"hi\"]]");
    assert!(out.ok, "{}", out.stdout);
    assert!(out.stdout.contains("No tests found"), "{}", out.stdout);
}
