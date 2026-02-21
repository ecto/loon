pub mod codes;
pub mod tutorials;

use crate::check::ownership::OwnershipError;
use crate::parser::ParseError;
use crate::syntax::Span;
use crate::types::TypeError;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

use codes::ErrorCode;

// Keep existing functions for backwards compatibility

pub fn report_error(filename: &str, source: &str, message: &str, span: Span) {
    let mut files = SimpleFiles::new();
    let file_id = files.add(filename, source);
    let diagnostic = Diagnostic::error()
        .with_message(message)
        .with_labels(vec![Label::primary(file_id, span.start..span.end)]);
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = term::Config::default();
    let _ = term::emit(&mut writer.lock(), &config, &files, &diagnostic);
}

pub fn report_type_error(filename: &str, source: &str, error: &TypeError) {
    if let Some(span) = error.span {
        report_error(filename, source, &error.message, span);
    } else {
        eprintln!("type error: {}", error.message);
    }
}

// ── New unified diagnostic infrastructure ────────────────────────

/// A span label for multi-span diagnostics.
#[derive(Debug, Clone)]
pub struct SpanLabel {
    pub span: Span,
    pub label: String,
    pub is_primary: bool,
}

/// ASCII ownership diagram showing value lifecycle.
#[derive(Debug, Clone)]
pub struct OwnershipDiagram {
    pub lines: Vec<String>,
}

/// Unified diagnostic type for all Loon errors.
#[derive(Debug, Clone)]
pub struct LoonDiagnostic {
    pub code: ErrorCode,
    /// What went wrong (compatible with TypeError.message for tests).
    pub what: String,
    /// Why it's an error.
    pub why: String,
    /// How to fix it.
    pub fix: String,
    pub labels: Vec<SpanLabel>,
    pub ownership_diagram: Option<OwnershipDiagram>,
}

impl LoonDiagnostic {
    pub fn new(code: ErrorCode, what: impl Into<String>) -> Self {
        Self {
            code,
            what: what.into(),
            why: String::new(),
            fix: String::new(),
            labels: Vec::new(),
            ownership_diagram: None,
        }
    }

    pub fn with_why(mut self, why: impl Into<String>) -> Self {
        self.why = why.into();
        self
    }

    pub fn with_fix(mut self, fix: impl Into<String>) -> Self {
        self.fix = fix.into();
        self
    }

    pub fn with_label(mut self, span: Span, label: impl Into<String>, primary: bool) -> Self {
        self.labels.push(SpanLabel {
            span,
            label: label.into(),
            is_primary: primary,
        });
        self
    }

    pub fn with_ownership_diagram(mut self, diagram: OwnershipDiagram) -> Self {
        self.ownership_diagram = Some(diagram);
        self
    }

    /// Primary span (first primary label, if any).
    pub fn primary_span(&self) -> Option<Span> {
        self.labels
            .iter()
            .find(|l| l.is_primary)
            .map(|l| l.span)
    }

    /// Compatibility: expose `what` as `message` for test assertions that
    /// use `e.message.contains(...)`.
    pub fn message(&self) -> &str {
        &self.what
    }

    /// Compatibility: expose primary span like TypeError's `span` field.
    pub fn span(&self) -> Option<Span> {
        self.primary_span()
    }
}

impl std::fmt::Display for LoonDiagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}] {}", self.code, self.what)?;
        if !self.why.is_empty() {
            write!(f, "\n  why: {}", self.why)?;
        }
        if !self.fix.is_empty() {
            write!(f, "\n  fix: {}", self.fix)?;
        }
        Ok(())
    }
}

/// Render a LoonDiagnostic using codespan-reporting.
pub fn report_diagnostic(filename: &str, source: &str, diag: &LoonDiagnostic) {
    let mut files = SimpleFiles::new();
    let file_id = files.add(filename, source);

    let message = format!("[{}] {}", diag.code, diag.what);

    let labels: Vec<Label<usize>> = diag
        .labels
        .iter()
        .map(|l| {
            let label = if l.is_primary {
                Label::primary(file_id, l.span.start..l.span.end)
            } else {
                Label::secondary(file_id, l.span.start..l.span.end)
            };
            label.with_message(&l.label)
        })
        .collect();

    let mut notes = Vec::new();
    if !diag.why.is_empty() {
        notes.push(format!("why: {}", diag.why));
    }
    if !diag.fix.is_empty() {
        notes.push(format!("fix: {}", diag.fix));
    }
    if let Some(ref diagram) = diag.ownership_diagram {
        notes.push(String::new());
        for line in &diagram.lines {
            notes.push(line.clone());
        }
    }

    let diagnostic = if diag.code.is_warning() {
        Diagnostic::warning()
    } else {
        Diagnostic::error()
    }
    .with_message(message)
    .with_labels(labels)
    .with_notes(notes);

    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = term::Config::default();
    let _ = term::emit(&mut writer.lock(), &config, &files, &diagnostic);
}

// ── From conversions ─────────────────────────────────────────────

impl From<ParseError> for LoonDiagnostic {
    fn from(e: ParseError) -> Self {
        let code = if e.message.contains("unexpected character") {
            ErrorCode::E0100
        } else if e.message.contains("unclosed") {
            ErrorCode::E0102
        } else if e.message.contains("invalid") {
            ErrorCode::E0103
        } else {
            ErrorCode::E0101
        };

        LoonDiagnostic::new(code, &e.message)
            .with_label(e.span, &e.message, true)
    }
}

impl From<OwnershipError> for LoonDiagnostic {
    fn from(e: OwnershipError) -> Self {
        let code = if e.message.contains("moved") {
            ErrorCode::E0300
        } else if e.message.contains("immutable") {
            ErrorCode::E0301
        } else if e.message.contains("more than once") {
            ErrorCode::E0302
        } else {
            ErrorCode::E0300
        };

        LoonDiagnostic::new(code, &e.message)
            .with_why(&e.why)
            .with_fix(&e.fix)
            .with_label(e.span, &e.message, true)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn diagnostic_creation() {
        let diag = LoonDiagnostic::new(ErrorCode::E0200, "cannot unify Int with String")
            .with_why("expected Int because of + operator")
            .with_fix("change the argument to an Int")
            .with_label(Span::new(0, 5), "expected Int", true)
            .with_label(Span::new(6, 13), "found String", false);

        assert_eq!(diag.code, ErrorCode::E0200);
        assert_eq!(diag.what, "cannot unify Int with String");
        assert_eq!(diag.why, "expected Int because of + operator");
        assert_eq!(diag.fix, "change the argument to an Int");
        assert_eq!(diag.labels.len(), 2);
        assert_eq!(diag.primary_span(), Some(Span::new(0, 5)));
    }

    #[test]
    fn diagnostic_display() {
        let diag = LoonDiagnostic::new(ErrorCode::E0201, "unbound symbol 'foo'")
            .with_why("'foo' is not defined in this scope")
            .with_fix("check spelling or add a definition");
        let s = format!("{}", diag);
        assert!(s.contains("[E0201]"));
        assert!(s.contains("unbound symbol 'foo'"));
    }

    #[test]
    fn diagnostic_message_compat() {
        let diag = LoonDiagnostic::new(ErrorCode::E0200, "cannot unify Int with String");
        assert_eq!(diag.message(), "cannot unify Int with String");
        assert!(diag.message().contains("unify"));
    }

    #[test]
    fn diagnostic_span_compat() {
        let diag =
            LoonDiagnostic::new(ErrorCode::E0200, "mismatch").with_label(Span::new(3, 7), "", true);
        assert_eq!(diag.span(), Some(Span::new(3, 7)));
    }

    #[test]
    fn error_code_classification() {
        assert_eq!(ErrorCode::E0100.category(), "parse");
        assert_eq!(ErrorCode::E0200.category(), "type");
        assert_eq!(ErrorCode::E0300.category(), "ownership");
        assert_eq!(ErrorCode::E0400.category(), "effect");
        assert_eq!(ErrorCode::E0500.category(), "module");
    }

    #[test]
    fn error_code_display() {
        assert_eq!(format!("{}", ErrorCode::E0201), "E0201");
        assert_eq!(ErrorCode::E0201.as_str(), "E0201");
    }

    #[test]
    fn from_parse_error() {
        let pe = ParseError {
            message: "unexpected character: '$'".to_string(),
            span: Span::new(0, 1),
        };
        let diag: LoonDiagnostic = pe.into();
        assert_eq!(diag.code, ErrorCode::E0100);
        assert!(diag.what.contains("unexpected character"));
    }

    #[test]
    fn from_ownership_error() {
        let oe = OwnershipError {
            message: "use of moved value 'x'".to_string(),
            span: Span::new(10, 11),
            why: "'x' was moved at 5..6".to_string(),
            fix: "clone 'x' before moving".to_string(),
        };
        let diag: LoonDiagnostic = oe.into();
        assert_eq!(diag.code, ErrorCode::E0300);
        assert!(diag.what.contains("moved"));
        assert!(!diag.why.is_empty());
        assert!(!diag.fix.is_empty());
    }

    #[test]
    fn ownership_diagram_attached() {
        let diagram = OwnershipDiagram {
            lines: vec![
                "  let x = vec![1, 2, 3]   -- x is alive".to_string(),
                "  take(x)                  -- x is moved".to_string(),
                "  println(x)              -- ERROR: x is dead".to_string(),
            ],
        };
        let diag = LoonDiagnostic::new(ErrorCode::E0300, "use of moved value 'x'")
            .with_ownership_diagram(diagram);
        assert!(diag.ownership_diagram.is_some());
        assert_eq!(diag.ownership_diagram.as_ref().unwrap().lines.len(), 3);
    }

    #[test]
    fn rendering_does_not_panic() {
        let diag = LoonDiagnostic::new(ErrorCode::E0200, "test error")
            .with_label(Span::new(0, 3), "here", true);
        // Just verify it doesn't panic
        report_diagnostic("test.loon", "[+ 1 2]", &diag);
    }
}
