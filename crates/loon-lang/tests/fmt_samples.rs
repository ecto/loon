//! Integration test: round-trip every `samples/**/*.oo` through the
//! comment-preserving formatter and assert (a) all comments survive, (b)
//! formatting is idempotent, and (c) parser output is preserved (same number
//! of top-level expressions).

use loon_lang::fmt::format_program_with_comments;
use loon_lang::parser::parse_with_comments;
use std::path::{Path, PathBuf};

fn samples_dir() -> PathBuf {
    // CARGO_MANIFEST_DIR is .../crates/loon-lang
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .unwrap()
        .join("samples")
}

fn collect_oo(root: &Path, out: &mut Vec<PathBuf>) {
    let entries = match std::fs::read_dir(root) {
        Ok(e) => e,
        Err(_) => return,
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect_oo(&path, out);
        } else if path.extension().and_then(|s| s.to_str()) == Some("oo") {
            out.push(path);
        }
    }
}

fn fmt(src: &str) -> String {
    let (exprs, comments) = parse_with_comments(src).expect("sample failed to parse");
    format_program_with_comments(&exprs, &comments, src)
}

#[test]
fn samples_round_trip_preserves_comments_and_is_idempotent() {
    let dir = samples_dir();
    let mut files = Vec::new();
    collect_oo(&dir, &mut files);
    assert!(!files.is_empty(), "expected to find samples in {dir:?}");

    let mut failures: Vec<String> = Vec::new();

    for path in &files {
        let src = std::fs::read_to_string(path).expect("read sample");
        let (orig_exprs, orig_comments) = match parse_with_comments(&src) {
            Ok(out) => out,
            Err(e) => {
                failures.push(format!("{}: parse failed: {}", path.display(), e.message));
                continue;
            }
        };

        let first = fmt(&src);

        // (a) Comment count preserved.
        let (_first_exprs, first_comments) = parse_with_comments(&first)
            .unwrap_or_else(|e| panic!("{} reparsed failed: {}", path.display(), e.message));
        if first_comments.len() != orig_comments.len() {
            failures.push(format!(
                "{}: comment count changed {} -> {}",
                path.display(),
                orig_comments.len(),
                first_comments.len()
            ));
        }

        // (b) Top-level expression count preserved (semantic preservation).
        let (first_exprs2, _) = parse_with_comments(&first).unwrap();
        if first_exprs2.len() != orig_exprs.len() {
            failures.push(format!(
                "{}: top-level expr count changed {} -> {}",
                path.display(),
                orig_exprs.len(),
                first_exprs2.len()
            ));
        }

        // (c) Idempotence.
        let second = fmt(&first);
        if first != second {
            // Keep the report compact — full diffs would flood the output.
            failures.push(format!(
                "{}: not idempotent ({} bytes -> {} bytes after second format)",
                path.display(),
                first.len(),
                second.len()
            ));
        }
    }

    if !failures.is_empty() {
        panic!(
            "{} sample(s) failed round-trip:\n  - {}",
            failures.len(),
            failures.join("\n  - ")
        );
    }
}
