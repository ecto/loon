//! Tests for the machine interface: `loon check --json` (JSONL diagnostics,
//! schema documented in docs/machine-interface.md) and `loon card` (the
//! compact language card for LLM system prompts).

use std::io::Write;
use std::process::Command;
use std::sync::atomic::{AtomicU32, Ordering};

static COUNTER: AtomicU32 = AtomicU32::new(0);

struct Out {
    ok: bool,
    stdout: String,
}

fn run_check_json(source: &str) -> Out {
    let mut path = std::env::temp_dir();
    let n = COUNTER.fetch_add(1, Ordering::Relaxed);
    path.push(format!("loon_mi_{}_{n}.oo", std::process::id()));
    {
        let mut f = std::fs::File::create(&path).expect("create temp");
        f.write_all(source.as_bytes()).expect("write temp");
    }
    let output = Command::new(env!("CARGO_BIN_EXE_loon"))
        .arg("check")
        .arg(&path)
        .arg("--json")
        .output()
        .expect("run loon check --json");
    let _ = std::fs::remove_file(&path);
    Out {
        ok: output.status.success(),
        stdout: String::from_utf8(output.stdout).expect("stdout is utf-8"),
    }
}

/// Parse stdout as JSONL, asserting every non-empty line is valid JSON and
/// contains no ANSI escapes. Returns (diagnostics, summary).
fn parse_jsonl(stdout: &str) -> (Vec<serde_json::Value>, serde_json::Value) {
    assert!(
        !stdout.contains('\x1b'),
        "stdout must be free of ANSI escapes: {stdout:?}"
    );
    let lines: Vec<serde_json::Value> = stdout
        .lines()
        .filter(|l| !l.trim().is_empty())
        .map(|l| {
            serde_json::from_str(l).unwrap_or_else(|e| panic!("invalid JSONL line {l:?}: {e}"))
        })
        .collect();
    assert!(!lines.is_empty(), "expected at least a summary line");
    let summary = lines.last().unwrap().clone();
    assert_eq!(summary["type"], "summary", "last line must be the summary");
    let diags = lines[..lines.len() - 1].to_vec();
    for d in &diags {
        assert_eq!(d["type"], "diagnostic");
    }
    (diags, summary)
}

#[test]
fn check_json_type_error() {
    let out = run_check_json("[+ 1 \"nope\"]\n");
    assert!(!out.ok, "type error must keep exit code 1");
    let (diags, summary) = parse_jsonl(&out.stdout);
    assert_eq!(summary["schema_version"], 1);
    assert!(summary["errors"].as_u64().unwrap() >= 1);
    assert_eq!(summary["warnings"], 0);

    let d = &diags[0];
    assert_eq!(d["schema_version"], 1);
    assert_eq!(d["severity"], "error");
    let code = d["code"].as_str().unwrap();
    assert!(
        code.starts_with("E02"),
        "expected a type-error code, got {code}"
    );
    assert!(!d["message"].as_str().unwrap().is_empty());
    assert_eq!(
        d["explain_hint"].as_str().unwrap(),
        format!("loon explain {code}")
    );

    // Span sanity: on line 1, byte offsets ordered and within the file.
    let spans = d["spans"].as_array().unwrap();
    assert!(!spans.is_empty(), "type error should carry a span");
    let s = &spans[0];
    assert_eq!(s["line"], 1);
    assert!(s["col"].as_u64().unwrap() >= 1);
    let (sb, eb) = (
        s["start_byte"].as_u64().unwrap(),
        s["end_byte"].as_u64().unwrap(),
    );
    assert!(sb <= eb && eb <= "[+ 1 \"nope\"]\n".len() as u64);
    assert!(s["file"].as_str().unwrap().ends_with(".oo"));
}

#[test]
fn check_json_warning() {
    // W0100: wildcard hides known constructors.
    let out = run_check_json(
        "[type Color Red Green Blue]\n[let c Red]\n[match c Red \"red\" _ \"other\"]\n",
    );
    let (diags, summary) = parse_jsonl(&out.stdout);
    let warning = diags
        .iter()
        .find(|d| d["severity"] == "warning")
        .expect("expected a warning diagnostic");
    assert_eq!(warning["code"], "W0100");
    assert!(summary["warnings"].as_u64().unwrap() >= 1);
}

#[test]
fn check_json_clean_file_emits_only_summary() {
    let out = run_check_json("[let x 1]\n[println x]\n");
    assert!(out.ok, "clean file must exit 0");
    let (diags, summary) = parse_jsonl(&out.stdout);
    assert!(diags.is_empty(), "clean file must emit only the summary");
    assert_eq!(summary["errors"], 0);
    assert_eq!(summary["warnings"], 0);
}

#[test]
fn check_json_parse_error() {
    let out = run_check_json("[let x\n");
    assert!(!out.ok);
    let (diags, summary) = parse_jsonl(&out.stdout);
    assert_eq!(diags.len(), 1);
    let code = diags[0]["code"].as_str().unwrap();
    assert!(
        code.starts_with("E01"),
        "expected a parse-error code, got {code}"
    );
    assert_eq!(summary["errors"], 1);
}

// ── loon card ────────────────────────────────────────────────────

fn run_card(json: bool) -> Out {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_loon"));
    cmd.arg("card");
    if json {
        cmd.arg("--json");
    }
    let output = cmd.output().expect("run loon card");
    Out {
        ok: output.status.success(),
        stdout: String::from_utf8(output.stdout).expect("stdout is utf-8"),
    }
}

#[test]
fn card_prints_truthiness_rule_and_stays_small() {
    let out = run_card(false);
    assert!(out.ok);
    // The one-sentence truthiness rule, verbatim (docs/agent-first.md).
    assert!(
        out.stdout.contains(
            "a value is truthy unless it says no (`false`) or says nothing (`()`, `None`)"
        ),
        "card must state the truthiness rule verbatim"
    );
    // Version stamped in, placeholder resolved.
    assert!(!out.stdout.contains("{{VERSION}}"));
    // Size budget: the card is for system prompts; fail before it bloats.
    assert!(
        out.stdout.len() < 10 * 1024,
        "card is {} bytes; budget is 10KB",
        out.stdout.len()
    );
}

#[test]
fn card_json_is_structured() {
    let out = run_card(true);
    assert!(out.ok);
    let v: serde_json::Value = serde_json::from_str(out.stdout.trim()).expect("card --json");
    assert_eq!(v["schema_version"], 1);
    assert!(!v["loon_version"].as_str().unwrap().is_empty());
    let sections = v["sections"].as_array().unwrap();
    assert!(sections.len() >= 5, "expected the card's major sections");
    assert!(sections
        .iter()
        .any(|s| s["title"].as_str().unwrap().contains("Semantics")));
}

#[test]
fn llms_txt_matches_card() {
    // web/public/llms.txt is the card served at loonlang.com/llms.txt.
    // Everything after the title line must match card.md exactly.
    let card = include_str!("../src/card.md");
    let llms = include_str!("../../../web/public/llms.txt");
    let body = |s: &str| s.split_once('\n').map(|(_, b)| b.to_string()).unwrap();
    assert_eq!(
        body(card),
        body(llms),
        "web/public/llms.txt has drifted from crates/loon-cli/src/card.md"
    );
}
