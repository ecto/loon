//! Machine-readable diagnostic output (`loon check --json`).
//!
//! Emits diagnostics as JSON Lines on stdout: one object per diagnostic,
//! then a final summary line. The schema is a public contract documented
//! in docs/machine-interface.md and versioned via `schema_version`.
//! Nothing else may be written to stdout in JSON mode — no ANSI, no prose.

use loon_lang::errors::LoonDiagnostic;
use serde_json::{json, Value};

/// Version of the JSONL diagnostic schema. Bump on any breaking change to
/// field names, types, or meaning; additive fields do not require a bump.
pub const SCHEMA_VERSION: u32 = 1;

/// 1-based (line, column) for a byte offset. Column counts Unicode scalar
/// values on the line, matching what editors display.
fn line_col(source: &str, byte: usize) -> (usize, usize) {
    let byte = byte.min(source.len());
    let before = &source[..byte];
    let line = before.bytes().filter(|&b| b == b'\n').count() + 1;
    let line_start = before.rfind('\n').map(|i| i + 1).unwrap_or(0);
    let col = source[line_start..byte].chars().count() + 1;
    (line, col)
}

/// Serialize one diagnostic as a JSON value (one JSONL line).
pub fn diagnostic_json(filename: &str, source: &str, diag: &LoonDiagnostic) -> Value {
    let severity = if diag.code.is_warning() {
        "warning"
    } else {
        "error"
    };
    let spans: Vec<Value> = diag
        .labels
        .iter()
        .map(|l| {
            let (line, col) = line_col(source, l.span.start);
            let (end_line, end_col) = line_col(source, l.span.end);
            json!({
                "file": filename,
                "label": l.label,
                "primary": l.is_primary,
                "start_byte": l.span.start,
                "end_byte": l.span.end,
                "line": line,
                "col": col,
                "end_line": end_line,
                "end_col": end_col,
            })
        })
        .collect();
    json!({
        "type": "diagnostic",
        "schema_version": SCHEMA_VERSION,
        "code": diag.code.as_str(),
        "severity": severity,
        "message": diag.what,
        "why": diag.why,
        "fix": diag.fix,
        "spans": spans,
        "explain_hint": format!("loon explain {}", diag.code.as_str()),
    })
}

/// The final summary line of a JSONL diagnostic stream.
pub fn summary_json(errors: usize, warnings: usize) -> Value {
    json!({
        "type": "summary",
        "schema_version": SCHEMA_VERSION,
        "errors": errors,
        "warnings": warnings,
    })
}

/// Emit a full JSONL report for a set of diagnostics and return the error
/// count (warnings do not count as errors in the summary).
pub fn emit_report(filename: &str, source: &str, diags: &[LoonDiagnostic]) -> (usize, usize) {
    let mut errors = 0;
    let mut warnings = 0;
    for d in diags {
        if d.code.is_warning() {
            warnings += 1;
        } else {
            errors += 1;
        }
        println!("{}", diagnostic_json(filename, source, d));
    }
    println!("{}", summary_json(errors, warnings));
    (errors, warnings)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn line_col_basics() {
        let src = "abc\ndef\n";
        assert_eq!(line_col(src, 0), (1, 1));
        assert_eq!(line_col(src, 2), (1, 3));
        assert_eq!(line_col(src, 4), (2, 1));
        assert_eq!(line_col(src, 6), (2, 3));
        // Clamped past EOF
        assert_eq!(line_col(src, 100), (3, 1));
    }

    #[test]
    fn summary_shape() {
        let s = summary_json(2, 1);
        assert_eq!(s["type"], "summary");
        assert_eq!(s["schema_version"], SCHEMA_VERSION);
        assert_eq!(s["errors"], 2);
        assert_eq!(s["warnings"], 1);
    }
}
