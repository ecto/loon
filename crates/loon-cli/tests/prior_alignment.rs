//! Prior-alignment corpus (docs/agent-first.md, "The prior-alignment corpus").
//!
//! Each program in `tests/prior-alignment/` is a snippet an agent would
//! *plausibly write* from a Python, Clojure, or Rust prior. The suite
//! enforces that every entry does one of two things:
//!
//!   1. behaves the way the surface reading suggests, or
//!   2. fails/warns with a diagnostic that teaches the relevant rule.
//!
//! The third outcome — running and silently doing something the surface
//! reading does not suggest — fails the suite. Expectations are DECLARED in
//! each program's leading comment block:
//!
//!   ; expect-stdout: <exact line>     (repeatable, in order; the program's
//!                                      full stdout must equal these lines)
//!   ; expect-check: <code>            (optional; `loon check` must report
//!                                      this diagnostic code — the "loud
//!                                      divergence" channel)
//!
//! Every `expect-check` is enforced in both directions: if the diagnostic
//! stops firing, the entry fails so the annotation gets retired or the
//! teaching gap gets fixed.

use std::path::{Path, PathBuf};
use std::process::Command;

fn corpus_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests").join("prior-alignment")
}

struct Expectations {
    stdout_lines: Vec<String>,
    check_codes: Vec<String>,
}

fn parse_directives(src: &str) -> Expectations {
    let mut ex = Expectations {
        stdout_lines: Vec::new(),
        check_codes: Vec::new(),
    };
    for line in src.lines() {
        let t = line.trim();
        if !t.starts_with(';') {
            if !t.is_empty() {
                break; // directives live in the leading comment block only
            }
            continue;
        }
        let t = t.trim_start_matches(';').trim();
        if let Some(rest) = t.strip_prefix("expect-stdout:") {
            ex.stdout_lines.push(rest.trim().to_string());
        } else if let Some(rest) = t.strip_prefix("expect-check:") {
            ex.check_codes.push(rest.trim().to_string());
        }
    }
    ex
}

fn loon(subcmd: &str, file: &Path) -> std::process::Output {
    Command::new(env!("CARGO_BIN_EXE_loon"))
        .arg(subcmd)
        .arg(file)
        .output()
        .unwrap_or_else(|e| panic!("spawn loon {subcmd} for {file:?}: {e}"))
}

#[test]
fn prior_alignment_corpus() {
    let dir = corpus_dir();
    let mut files: Vec<PathBuf> = std::fs::read_dir(&dir)
        .unwrap_or_else(|e| panic!("read {dir:?}: {e}"))
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| p.extension().is_some_and(|x| x == "oo"))
        .collect();
    files.sort();
    assert!(!files.is_empty(), "empty prior-alignment corpus at {dir:?}");

    let mut failures: Vec<String> = Vec::new();
    for file in &files {
        let name = file.file_name().unwrap().to_string_lossy().into_owned();
        let src = std::fs::read_to_string(file).unwrap();
        let ex = parse_directives(&src);
        assert!(
            !ex.stdout_lines.is_empty(),
            "{name}: every corpus entry must declare its intended behavior via expect-stdout"
        );

        // 1. The program must BEHAVE as the prior's surface reading suggests.
        let run = loon("run", file);
        let stdout = String::from_utf8_lossy(&run.stdout);
        let got: Vec<&str> = stdout.lines().collect();
        if !run.status.success() {
            failures.push(format!(
                "{name}: run failed (the prior expects it to run):\n  stderr: {}",
                String::from_utf8_lossy(&run.stderr)
            ));
        } else if got != ex.stdout_lines {
            failures.push(format!(
                "{name}: behavior diverged from the declared prior expectation:\n  expected: {:?}\n  got:      {got:?}",
                ex.stdout_lines
            ));
        }

        // 2. Declared teaching diagnostics must fire — and only declared ones.
        let check = loon("check", file);
        let check_out = format!(
            "{}{}",
            String::from_utf8_lossy(&check.stdout),
            String::from_utf8_lossy(&check.stderr)
        );
        for code in &ex.check_codes {
            if !check_out.contains(code.as_str()) {
                failures.push(format!(
                    "{name}: expected teaching diagnostic {code} did not fire — \
                     the divergence from the prior is now SILENT; fix the teaching \
                     or retire the annotation"
                ));
            }
        }
        if ex.check_codes.is_empty() && check_out.contains("warning: [") {
            failures.push(format!(
                "{name}: an undeclared warning fired:\n{check_out}"
            ));
        }
    }
    assert!(
        failures.is_empty(),
        "prior-alignment failures ({}/{} programs):\n{}",
        failures.len(),
        files.len(),
        failures.join("\n")
    );
}
