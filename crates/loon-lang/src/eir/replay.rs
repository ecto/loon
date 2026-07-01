//! Record/replay of nondeterministic effect operations.
//!
//! `loon run --record trace.oo` wraps the VM's builtin (unhandled) effect path
//! so every nondeterministic operation — file reads, clock reads, uuid, env
//! lookups, network calls — has its *result* appended to a trace file in Loon
//! data format. Log writes (`IO.println`) are recorded too, purely for
//! observability and divergence detection.
//!
//! `loon replay trace.oo prog.oo` runs the same program feeding the recorded
//! results back in order: same program + same trace = identical execution,
//! including reproducing a crash at the exact same step.
//!
//! ## On-disk format
//!
//! While recording, entries are appended one map per line and flushed after
//! every write, so the trace survives a mid-run crash or panic:
//!
//! ```text
//! {:effect "IO" :op "millis" :args #[] :result 1751338712345}
//! {:effect "IO" :op "println" :args #["hello"] :result :unit}
//! ```
//!
//! On a clean CLI exit (success *or* reported error) the file is finalized
//! into a single Loon vector `#[{…} {…}]`. The loader accepts both forms.
//!
//! Unit results are written as the keyword `:unit` (builtin effects never
//! return keywords, so the marker is unambiguous).

use std::io::Write;
use std::path::Path;

/// A value recorded in (or fed back from) a trace: the subset of Loon data
/// that builtin effect operations produce and consume.
#[derive(Debug, Clone, PartialEq)]
pub enum TraceVal {
    Unit,
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    Vec(Vec<TraceVal>),
}

impl TraceVal {
    /// Serialize to Loon literal source that reparses to the same value.
    pub fn to_loon(&self) -> String {
        match self {
            TraceVal::Unit => ":unit".to_string(),
            TraceVal::Int(n) => n.to_string(),
            TraceVal::Float(f) => format_float(*f),
            TraceVal::Bool(b) => b.to_string(),
            TraceVal::Str(s) => format!("\"{}\"", escape_str(s)),
            TraceVal::Vec(items) => {
                let inner: Vec<String> = items.iter().map(|v| v.to_loon()).collect();
                format!("#[{}]", inner.join(" "))
            }
        }
    }
}

/// Escape a string for a Loon string literal. Braces are literal characters in
/// Loon strings (interpolation is `\(expr)`), so only backslash, quote, and
/// control whitespace need escaping.
fn escape_str(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\t' => out.push_str("\\t"),
            _ => out.push(c),
        }
    }
    out
}

/// Format an f64 so it lexes as a Loon Float (which requires `digits.digits`).
fn format_float(f: f64) -> String {
    if !f.is_finite() {
        return "0.0".to_string();
    }
    let s = format!("{f}");
    if s.contains('.') {
        s
    } else if let Some(epos) = s.find(['e', 'E']) {
        format!("{}.0{}", &s[..epos], &s[epos..])
    } else {
        format!("{s}.0")
    }
}

/// One recorded effect operation.
#[derive(Debug, Clone)]
pub struct TraceEntry {
    pub effect: String,
    pub op: String,
    pub args: Vec<TraceVal>,
    pub result: TraceVal,
}

impl TraceEntry {
    /// Serialize as a single-line Loon map.
    pub fn to_loon(&self) -> String {
        let args: Vec<String> = self.args.iter().map(|a| a.to_loon()).collect();
        format!(
            "{{:effect \"{}\" :op \"{}\" :args #[{}] :result {}}}",
            escape_str(&self.effect),
            escape_str(&self.op),
            args.join(" "),
            self.result.to_loon()
        )
    }
}

/// Should this builtin (unhandled) effect operation be recorded and replayed?
///
/// Everything whose result depends on the outside world is recorded — plus
/// `IO.println` and `IO.write-file` for observability and ordering checks.
/// Pure data transforms (`parse-json`, `to-json`, `blake3`) are deterministic
/// given their arguments and are re-executed live on both paths. Effects
/// handled by an in-language `handle` never reach this path at all.
pub fn is_recorded_op(effect: &str, op: &str) -> bool {
    match effect {
        "Net" | "Env" | "Process" => true,
        "IO" => !matches!(op, "parse-json" | "to-json" | "blake3"),
        _ => false,
    }
}

/// Incremental trace writer: appends one entry per line and flushes after
/// every write, so the trace persists even if the program crashes mid-run.
pub struct TraceRecorder {
    file: std::fs::File,
    count: usize,
}

impl TraceRecorder {
    /// Create (truncate) the trace file at `path`.
    pub fn create(path: &Path) -> std::io::Result<Self> {
        let file = std::fs::File::create(path)?;
        Ok(Self { file, count: 0 })
    }

    /// Append one entry and flush it to disk.
    pub fn record(&mut self, entry: &TraceEntry) -> std::io::Result<()> {
        writeln!(self.file, "{}", entry.to_loon())?;
        self.file.flush()?;
        self.count += 1;
        Ok(())
    }

    /// Number of entries written so far.
    pub fn count(&self) -> usize {
        self.count
    }
}

/// Rewrite a line-delimited trace file into a single Loon vector `#[…]`.
/// Idempotent: a file already in vector form is left untouched. Called by the
/// CLI after the run finishes (success or reported error); if the process
/// panics before finalizing, the line-delimited form is still loadable.
pub fn finalize_trace_file(path: &Path) -> std::io::Result<()> {
    let content = std::fs::read_to_string(path)?;
    if content.trim_start().starts_with("#[") {
        return Ok(());
    }
    let mut out = String::with_capacity(content.len() + 16);
    out.push_str("#[");
    for (i, line) in content.lines().filter(|l| !l.trim().is_empty()).enumerate() {
        if i > 0 {
            out.push_str("\n  ");
        }
        out.push_str(line);
    }
    out.push_str("]\n");
    std::fs::write(path, out)
}

/// Parse a trace file's source into entries. Accepts both the finalized
/// vector form `#[{…} {…}]` and the crash-time line-delimited form (a
/// sequence of top-level maps).
pub fn parse_trace(src: &str) -> Result<Vec<TraceEntry>, String> {
    let exprs = crate::parser::parse(src).map_err(|e| format!("parse error: {}", e.message))?;
    let entry_exprs: Vec<&crate::ast::Expr> = match exprs.as_slice() {
        [single] if matches!(single.kind, crate::ast::ExprKind::Vec(_)) => {
            match &single.kind {
                crate::ast::ExprKind::Vec(items) => items.iter().collect(),
                _ => unreachable!(),
            }
        }
        many => many.iter().collect(),
    };

    let mut entries = Vec::with_capacity(entry_exprs.len());
    for (i, expr) in entry_exprs.iter().enumerate() {
        entries.push(parse_entry(expr).map_err(|e| format!("trace entry {i}: {e}"))?);
    }
    Ok(entries)
}

fn parse_entry(expr: &crate::ast::Expr) -> Result<TraceEntry, String> {
    use crate::ast::ExprKind;
    let ExprKind::Map(pairs) = &expr.kind else {
        return Err("expected a map {:effect … :op … :args … :result …}".to_string());
    };
    let mut effect = None;
    let mut op = None;
    let mut args = Vec::new();
    let mut result = TraceVal::Unit;
    for (k, v) in pairs {
        let ExprKind::Keyword(key) = &k.kind else {
            continue;
        };
        match key.as_str() {
            "effect" => match &v.kind {
                ExprKind::Str(s) => effect = Some(s.clone()),
                _ => return Err(":effect must be a string".to_string()),
            },
            "op" => match &v.kind {
                ExprKind::Str(s) => op = Some(s.clone()),
                _ => return Err(":op must be a string".to_string()),
            },
            "args" => match &v.kind {
                ExprKind::Vec(items) => {
                    args = items
                        .iter()
                        .map(expr_to_trace_val)
                        .collect::<Result<Vec<_>, _>>()?;
                }
                _ => return Err(":args must be a vector".to_string()),
            },
            "result" => result = expr_to_trace_val(v)?,
            _ => {}
        }
    }
    Ok(TraceEntry {
        effect: effect.ok_or("missing :effect")?,
        op: op.ok_or("missing :op")?,
        args,
        result,
    })
}

fn expr_to_trace_val(expr: &crate::ast::Expr) -> Result<TraceVal, String> {
    use crate::ast::ExprKind;
    match &expr.kind {
        ExprKind::Int(n) => Ok(TraceVal::Int(*n)),
        ExprKind::Float(f) => Ok(TraceVal::Float(*f)),
        ExprKind::Bool(b) => Ok(TraceVal::Bool(*b)),
        ExprKind::Str(s) => Ok(TraceVal::Str(s.clone())),
        ExprKind::Keyword(k) if k == "unit" => Ok(TraceVal::Unit),
        ExprKind::Vec(items) => Ok(TraceVal::Vec(
            items
                .iter()
                .map(expr_to_trace_val)
                .collect::<Result<Vec<_>, _>>()?,
        )),
        other => Err(format!("unsupported trace value: {other:?}")),
    }
}

/// Cursor over a loaded trace during replay.
#[derive(Debug)]
pub struct ReplayCursor {
    pub entries: Vec<TraceEntry>,
    pub idx: usize,
}

impl ReplayCursor {
    pub fn new(entries: Vec<TraceEntry>) -> Self {
        Self { entries, idx: 0 }
    }

    /// Entries not yet consumed.
    pub fn remaining(&self) -> usize {
        self.entries.len().saturating_sub(self.idx)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn entry_roundtrips_through_loon_source() {
        let entry = TraceEntry {
            effect: "IO".to_string(),
            op: "read-file".to_string(),
            args: vec![TraceVal::Str("data \"x\"\nwith {braces}".to_string())],
            result: TraceVal::Str("line1\nline2\t\\end".to_string()),
        };
        let src = entry.to_loon();
        let parsed = parse_trace(&src).expect("parse");
        assert_eq!(parsed.len(), 1);
        assert_eq!(parsed[0].effect, "IO");
        assert_eq!(parsed[0].op, "read-file");
        assert_eq!(parsed[0].args, entry.args);
        assert_eq!(parsed[0].result, entry.result);
    }

    #[test]
    fn parse_accepts_vector_and_line_delimited_forms() {
        let lines = "{:effect \"IO\" :op \"millis\" :args #[] :result 42}\n\
                     {:effect \"IO\" :op \"println\" :args #[\"hi\"] :result :unit}\n";
        let from_lines = parse_trace(lines).expect("lines form");
        assert_eq!(from_lines.len(), 2);
        assert_eq!(from_lines[0].result, TraceVal::Int(42));
        assert_eq!(from_lines[1].result, TraceVal::Unit);

        let vector = format!(
            "#[{} {}]",
            from_lines[0].to_loon(),
            from_lines[1].to_loon()
        );
        let from_vec = parse_trace(&vector).expect("vector form");
        assert_eq!(from_vec.len(), 2);
        assert_eq!(from_vec[1].effect, "IO");
        assert_eq!(from_vec[1].op, "println");
    }

    #[test]
    fn finalize_wraps_lines_into_vector() {
        let dir = std::env::temp_dir().join(format!("loon-replay-unit-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("t.oo");
        std::fs::write(&path, "{:effect \"IO\" :op \"now\" :args #[] :result 7}\n").unwrap();
        finalize_trace_file(&path).unwrap();
        let content = std::fs::read_to_string(&path).unwrap();
        assert!(content.starts_with("#["), "finalized: {content}");
        let entries = parse_trace(&content).unwrap();
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].result, TraceVal::Int(7));
        // Idempotent.
        finalize_trace_file(&path).unwrap();
        assert_eq!(std::fs::read_to_string(&path).unwrap(), content);
    }

    #[test]
    fn floats_reparse_as_floats() {
        for f in [1.5f64, 3.0, 1e300, -2.25] {
            let src = TraceVal::Float(f).to_loon();
            let parsed = parse_trace(&format!(
                "{{:effect \"X\" :op \"y\" :args #[] :result {src}}}"
            ))
            .unwrap();
            assert_eq!(parsed[0].result, TraceVal::Float(f), "source was {src}");
        }
    }

    #[test]
    fn recorded_op_predicate() {
        assert!(is_recorded_op("IO", "millis"));
        assert!(is_recorded_op("IO", "println"));
        assert!(is_recorded_op("IO", "read-file"));
        assert!(is_recorded_op("Env", "get"));
        assert!(is_recorded_op("Net", "accept"));
        assert!(!is_recorded_op("IO", "parse-json"));
        assert!(!is_recorded_op("IO", "blake3"));
        assert!(!is_recorded_op("Physics", "gravity"));
        assert!(!is_recorded_op("Const", "c"));
    }
}
