//! Wadler-Lindig style pretty printer for Loon source code.
//!
//! Known limitations:
//! - Comments are lost (the lexer strips them at tokens.rs:28)
//! - Numeric suffixes are lost (the parser converts e.g. `42i32` to `i64(42)`)

use crate::ast::{Expr, ExprKind};

// ---------------------------------------------------------------------------
// Document algebra
// ---------------------------------------------------------------------------

#[derive(Clone, Debug)]
enum Doc {
    Nil,
    Text(String),
    /// Soft line break: rendered as a space when flat, newline when broken.
    Line,
    /// Always a line break.
    HardLine,
    Indent(i32, Box<Doc>),
    Concat(Box<Doc>, Box<Doc>),
    /// Try to render contents flat first; break if it exceeds the width.
    Group(Box<Doc>),
}

// Convenience constructors

fn nil() -> Doc {
    Doc::Nil
}

fn text(s: impl Into<String>) -> Doc {
    Doc::Text(s.into())
}

fn line() -> Doc {
    Doc::Line
}

fn hard_line() -> Doc {
    Doc::HardLine
}

fn indent(n: i32, d: Doc) -> Doc {
    Doc::Indent(n, Box::new(d))
}

fn concat(a: Doc, b: Doc) -> Doc {
    Doc::Concat(Box::new(a), Box::new(b))
}

fn group(d: Doc) -> Doc {
    Doc::Group(Box::new(d))
}

/// Concatenate a sequence of docs with a separator in between.
fn intersperse(docs: Vec<Doc>, sep: Doc) -> Doc {
    let mut iter = docs.into_iter();
    let first = match iter.next() {
        Some(d) => d,
        None => return nil(),
    };
    iter.fold(first, |acc, d| concat(concat(acc, sep.clone()), d))
}

/// Concatenate docs with no separator.
fn concat_all(docs: Vec<Doc>) -> Doc {
    docs.into_iter().fold(nil(), concat)
}

// ---------------------------------------------------------------------------
// Rendering (fits / format)
// ---------------------------------------------------------------------------

const DEFAULT_WIDTH: usize = 80;
const INDENT_SIZE: i32 = 2;

#[derive(Clone, Copy, PartialEq)]
enum Mode {
    Flat,
    Break,
}

/// Stack entry for the renderer: (indent level, mode, document).
type DocCmd<'a> = (i32, Mode, &'a Doc);

fn render(doc: &Doc, width: usize) -> String {
    let mut out = String::new();
    let mut stack: Vec<DocCmd> = vec![(0, Mode::Break, doc)];
    let mut col: usize = 0;

    while let Some((ind, mode, d)) = stack.pop() {
        match d {
            Doc::Nil => {}
            Doc::Text(s) => {
                out.push_str(s);
                col += s.len();
            }
            Doc::Line => match mode {
                Mode::Flat => {
                    out.push(' ');
                    col += 1;
                }
                Mode::Break => {
                    out.push('\n');
                    let spaces = ind as usize;
                    for _ in 0..spaces {
                        out.push(' ');
                    }
                    col = spaces;
                }
            },
            Doc::HardLine => {
                out.push('\n');
                let spaces = ind as usize;
                for _ in 0..spaces {
                    out.push(' ');
                }
                col = spaces;
            }
            Doc::Indent(n, inner) => {
                stack.push((ind + n, mode, inner));
            }
            Doc::Concat(a, b) => {
                // Push b first so a is processed first (stack is LIFO).
                stack.push((ind, mode, b));
                stack.push((ind, mode, a));
            }
            Doc::Group(inner) => {
                if fits(width as i32 - col as i32, &[(ind, Mode::Flat, inner)]) {
                    stack.push((ind, Mode::Flat, inner));
                } else {
                    stack.push((ind, Mode::Break, inner));
                }
            }
        }
    }

    out
}

/// Check whether the document fits within `remaining` columns when rendered flat.
fn fits(mut remaining: i32, cmds: &[DocCmd]) -> bool {
    let mut stack: Vec<DocCmd> = cmds.iter().rev().cloned().collect();

    while remaining >= 0 {
        let (ind, mode, d) = match stack.pop() {
            Some(cmd) => cmd,
            None => return true,
        };
        match d {
            Doc::Nil => {}
            Doc::Text(s) => {
                remaining -= s.len() as i32;
            }
            Doc::Line => match mode {
                Mode::Flat => {
                    remaining -= 1; // space
                }
                Mode::Break => return true, // line break always fits
            },
            Doc::HardLine => return true,
            Doc::Indent(n, inner) => {
                stack.push((ind + n, mode, inner));
            }
            Doc::Concat(a, b) => {
                stack.push((ind, mode, b));
                stack.push((ind, mode, a));
            }
            Doc::Group(inner) => {
                stack.push((ind, Mode::Flat, inner));
            }
        }
    }

    false
}

// ---------------------------------------------------------------------------
// AST -> Doc conversion
// ---------------------------------------------------------------------------

/// Escape special characters in a string literal for output.
fn escape_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for ch in s.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\t' => out.push_str("\\t"),
            '\r' => out.push_str("\\r"),
            '\0' => out.push_str("\\0"),
            c => out.push(c),
        }
    }
    out
}

fn expr_to_doc(expr: &Expr) -> Doc {
    match &expr.kind {
        ExprKind::Int(n) => text(n.to_string()),
        ExprKind::Float(n) => text(format!("{n}")),
        ExprKind::Bool(b) => text(if *b { "true" } else { "false" }),
        ExprKind::Str(s) => text(format!("\"{}\"", escape_string(s))),
        ExprKind::Keyword(k) => text(format!(":{k}")),
        ExprKind::Symbol(s) => text(s.clone()),

        ExprKind::List(items) => list_to_doc(items),
        ExprKind::Vec(items) => collection_to_doc("#[", "]", items),
        ExprKind::Set(items) => collection_to_doc("#{", "}", items),
        ExprKind::Map(pairs) => map_to_doc(pairs),
        ExprKind::Tuple(items) => tuple_to_doc(items),
        ExprKind::Quote(inner) => concat(text("`"), expr_to_doc(inner)),
        ExprKind::Unquote(inner) => concat(text("~"), expr_to_doc(inner)),
        ExprKind::UnquoteSplice(inner) => concat(text("~@"), expr_to_doc(inner)),
    }
}

/// Format a List (s-expression) `[head args...]` with form-specific rules.
fn list_to_doc(items: &[Expr]) -> Doc {
    if items.is_empty() {
        return text("[]");
    }

    let head = &items[0];
    let head_name = match &head.kind {
        ExprKind::Symbol(s) => Some(s.as_str()),
        _ => None,
    };

    match head_name {
        Some("fn") => {
            // Named fn: [fn name [params] body...] vs anonymous: [fn [params] body...]
            if items.len() >= 2 && matches!(&items[1].kind, ExprKind::Symbol(_)) {
                defn_to_doc(items)
            } else {
                fn_to_doc(items)
            }
        }
        Some("let") => let_to_doc(items),
        Some("if") => if_to_doc(items),
        Some("match") => match_to_doc(items),
        Some("pipe") => pipe_to_doc(items),
        Some("type") => type_to_doc(items),
        Some("effect") => type_to_doc(items), // same layout as type
        _ => generic_list_to_doc(items),
    }
}

/// `[fn name [params...] body...]`
/// Name+params on first line, body indented 2. Blank line between top-level defns
/// is handled in `format_program`.
fn defn_to_doc(items: &[Expr]) -> Doc {
    // items[0] = defn, items[1] = name, items[2] = params, items[3..] = body
    if items.len() < 3 {
        return generic_list_to_doc(items);
    }

    let keyword = expr_to_doc(&items[0]);
    let name = expr_to_doc(&items[1]);
    let params = expr_to_doc(&items[2]);

    let header = concat_all(vec![
        text("["),
        keyword,
        text(" "),
        name,
        text(" "),
        params,
    ]);

    if items.len() == 3 {
        return concat(header, text("]"));
    }

    let body_docs: Vec<Doc> = items[3..].iter().map(expr_to_doc).collect();
    let body = intersperse(body_docs, line());

    group(concat_all(vec![
        header,
        indent(INDENT_SIZE, concat(line(), body)),
        text("]"),
    ]))
}

/// `[fn [params...] body...]`
fn fn_to_doc(items: &[Expr]) -> Doc {
    if items.len() < 2 {
        return generic_list_to_doc(items);
    }

    let keyword = expr_to_doc(&items[0]);
    let params = expr_to_doc(&items[1]);

    let header = concat_all(vec![text("["), keyword, text(" "), params]);

    if items.len() == 2 {
        return concat(header, text("]"));
    }

    let body_docs: Vec<Doc> = items[2..].iter().map(expr_to_doc).collect();
    let body = intersperse(body_docs, line());

    group(concat_all(vec![
        header,
        indent(INDENT_SIZE, concat(line(), body)),
        text("]"),
    ]))
}

/// `[let name value]` — single line if fits, break after name otherwise.
fn let_to_doc(items: &[Expr]) -> Doc {
    if items.len() < 3 {
        return generic_list_to_doc(items);
    }

    // Could be [let name val] or [let mut name val]
    let docs: Vec<Doc> = items.iter().map(expr_to_doc).collect();

    group(concat_all(vec![
        text("["),
        docs[0].clone(),
        text(" "),
        intersperse(docs[1..].to_vec(), line()),
        text("]"),
    ]))
}

/// `[if cond then else]` — inline if fits; cond on same line, then/else indented.
fn if_to_doc(items: &[Expr]) -> Doc {
    if items.len() < 3 {
        return generic_list_to_doc(items);
    }

    let keyword = expr_to_doc(&items[0]);
    let cond = expr_to_doc(&items[1]);

    let branches: Vec<Doc> = items[2..].iter().map(expr_to_doc).collect();
    let body = intersperse(branches, line());

    group(concat_all(vec![
        text("["),
        keyword,
        text(" "),
        cond,
        indent(INDENT_SIZE, concat(line(), body)),
        text("]"),
    ]))
}

/// `[match expr arms...]` — expr on first line, each arm on own indented line.
fn match_to_doc(items: &[Expr]) -> Doc {
    if items.len() < 2 {
        return generic_list_to_doc(items);
    }

    let keyword = expr_to_doc(&items[0]);
    let scrutinee = expr_to_doc(&items[1]);

    let header = concat_all(vec![text("["), keyword, text(" "), scrutinee]);

    if items.len() == 2 {
        return concat(header, text("]"));
    }

    let arm_docs: Vec<Doc> = items[2..].iter().map(expr_to_doc).collect();
    let arms = intersperse(arm_docs, line());

    concat_all(vec![
        header,
        indent(INDENT_SIZE, concat(hard_line(), arms)),
        text("]"),
    ])
}

/// `[pipe expr steps...]` — first expr on same line, each step indented on own line.
fn pipe_to_doc(items: &[Expr]) -> Doc {
    if items.len() < 2 {
        return generic_list_to_doc(items);
    }

    let keyword = expr_to_doc(&items[0]);
    let first = expr_to_doc(&items[1]);

    let header = concat_all(vec![text("["), keyword, text(" "), first]);

    if items.len() == 2 {
        return concat(header, text("]"));
    }

    let step_docs: Vec<Doc> = items[2..].iter().map(expr_to_doc).collect();
    let steps = intersperse(step_docs, line());

    group(concat_all(vec![
        header,
        indent(INDENT_SIZE, concat(line(), steps)),
        text("]"),
    ]))
}

/// `[type Name constructors...]` or `[effect Name ops...]`
/// Name on first line, constructors/ops indented.
fn type_to_doc(items: &[Expr]) -> Doc {
    if items.len() < 2 {
        return generic_list_to_doc(items);
    }

    let keyword = expr_to_doc(&items[0]);
    let name = expr_to_doc(&items[1]);

    let header = concat_all(vec![text("["), keyword, text(" "), name]);

    if items.len() == 2 {
        return concat(header, text("]"));
    }

    let body_docs: Vec<Doc> = items[2..].iter().map(expr_to_doc).collect();
    let body = intersperse(body_docs, line());

    concat_all(vec![
        header,
        indent(INDENT_SIZE, concat(hard_line(), body)),
        text("]"),
    ])
}

/// Generic list: inline if fits, head on first line + args indented.
fn generic_list_to_doc(items: &[Expr]) -> Doc {
    if items.is_empty() {
        return text("[]");
    }

    if items.len() == 1 {
        return concat_all(vec![text("["), expr_to_doc(&items[0]), text("]")]);
    }

    let head = expr_to_doc(&items[0]);
    let arg_docs: Vec<Doc> = items[1..].iter().map(expr_to_doc).collect();
    let args = intersperse(arg_docs, line());

    group(concat_all(vec![
        text("["),
        head,
        indent(INDENT_SIZE, concat(line(), args)),
        text("]"),
    ]))
}

/// Vec `#[a b c]` and Set `#{a b c}`: inline if fits, one per line otherwise.
fn collection_to_doc(open: &str, close: &str, items: &[Expr]) -> Doc {
    if items.is_empty() {
        return text(format!("{open}{close}"));
    }

    let docs: Vec<Doc> = items.iter().map(expr_to_doc).collect();
    let body = intersperse(docs, line());

    group(concat_all(vec![
        text(open),
        indent(INDENT_SIZE, body),
        text(close),
    ]))
}

/// Map `{:key val ...}`: key-value pairs aligned, one per line if > 80 chars.
fn map_to_doc(pairs: &[(Expr, Expr)]) -> Doc {
    if pairs.is_empty() {
        return text("{}");
    }

    let pair_docs: Vec<Doc> = pairs
        .iter()
        .map(|(k, v)| concat_all(vec![expr_to_doc(k), text(" "), expr_to_doc(v)]))
        .collect();
    let body = intersperse(pair_docs, line());

    group(concat_all(vec![
        text("{"),
        indent(INDENT_SIZE, body),
        text("}"),
    ]))
}

/// Tuple `(a, b)`.
fn tuple_to_doc(items: &[Expr]) -> Doc {
    if items.is_empty() {
        return text("()");
    }

    let docs: Vec<Doc> = items.iter().map(expr_to_doc).collect();
    let body = intersperse(docs, line());

    group(concat_all(vec![text("("), body, text(")")]))
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Format a parsed Loon program back to source code.
///
/// Inserts blank lines between top-level `defn` forms.
pub fn format_program(exprs: &[Expr]) -> String {
    if exprs.is_empty() {
        return String::new();
    }

    let mut parts: Vec<String> = Vec::new();

    for (i, expr) in exprs.iter().enumerate() {
        let doc = expr_to_doc(expr);
        let formatted = render(&doc, DEFAULT_WIDTH);
        parts.push(formatted);

        // Insert blank line between top-level defns
        if i + 1 < exprs.len() {
            let is_defn = is_top_level_defn(expr);
            let next_is_defn = is_top_level_defn(&exprs[i + 1]);
            if is_defn || next_is_defn {
                parts.push(String::new());
            }
        }
    }

    let mut result = parts.join("\n");
    // Ensure trailing newline
    if !result.ends_with('\n') {
        result.push('\n');
    }
    result
}

fn is_top_level_defn(expr: &Expr) -> bool {
    if let ExprKind::List(items) = &expr.kind {
        if let Some(head) = items.first() {
            if let ExprKind::Symbol(s) = &head.kind {
                return matches!(s.as_str(), "fn" | "type" | "effect");
            }
        }
    }
    false
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;

    fn fmt(src: &str) -> String {
        let exprs = parse(src).expect("parse failed");
        format_program(&exprs)
    }

    // -- Idempotency tests --------------------------------------------------

    fn assert_idempotent(src: &str) {
        let first = fmt(src);
        let second = fmt(&first);
        assert_eq!(first, second, "formatting is not idempotent for: {src}");
    }

    #[test]
    fn idempotent_simple() {
        assert_idempotent("[+ 1 2]");
    }

    #[test]
    fn idempotent_defn() {
        assert_idempotent("[fn add [x y] [+ x y]]");
    }

    #[test]
    fn idempotent_let() {
        assert_idempotent("[let x 42]");
    }

    #[test]
    fn idempotent_if() {
        assert_idempotent("[if true 1 2]");
    }

    #[test]
    fn idempotent_nested() {
        assert_idempotent("[fn foo [x] [if [> x 0] [+ x 1] [- x 1]]]");
    }

    #[test]
    fn idempotent_vec() {
        assert_idempotent("#[1 2 3]");
    }

    #[test]
    fn idempotent_map() {
        assert_idempotent("{:a 1 :b 2}");
    }

    #[test]
    fn idempotent_set() {
        assert_idempotent("#{1 2 3}");
    }

    #[test]
    fn idempotent_tuple() {
        assert_idempotent("(1 2 3)");
    }

    #[test]
    fn idempotent_pipe() {
        assert_idempotent("[pipe x [foo] [bar]]");
    }

    #[test]
    fn idempotent_match() {
        assert_idempotent("[match x 1 \"one\" 2 \"two\"]");
    }

    #[test]
    fn idempotent_type() {
        assert_idempotent("[type Option [Some val] None]");
    }

    #[test]
    fn idempotent_multiline_defn() {
        let src = "[fn complex [a b c] [let x [+ a b]] [let y [* x c]] [if [> y 0] y [- 0 y]]]";
        assert_idempotent(src);
    }

    // -- Snapshot / behavior tests ------------------------------------------

    #[test]
    fn format_defn() {
        let result = fmt("[fn add [x y] [+ x y]]");
        assert!(result.contains("[fn add [x y]"));
        assert!(result.contains("[+ x y]"));
    }

    #[test]
    fn format_fn_lambda() {
        let result = fmt("[fn [x] [+ x 1]]");
        assert!(result.contains("[fn [x]"));
        assert!(result.contains("[+ x 1]"));
    }

    #[test]
    fn format_let_simple() {
        let result = fmt("[let x 42]");
        assert!(result.contains("[let x 42]"));
    }

    #[test]
    fn format_if_simple() {
        let result = fmt("[if true 1 2]");
        assert!(result.contains("[if true"));
    }

    #[test]
    fn format_pipe() {
        let result = fmt("[pipe x [foo] [bar]]");
        assert!(result.contains("[pipe x"));
    }

    #[test]
    fn format_type_decl() {
        let result = fmt("[type Option [Some val] None]");
        assert!(result.contains("[type Option"));
    }

    #[test]
    fn format_empty_program() {
        let result = fmt("");
        assert_eq!(result, "");
    }

    #[test]
    fn format_blank_lines_between_defns() {
        let result = fmt("[fn foo [] 1] [fn bar [] 2]");
        assert!(
            result.contains("\n\n"),
            "expected blank line between defns, got: {result}"
        );
    }

    #[test]
    fn format_string_escaping() {
        let result = fmt(r#"[println "hello\nworld"]"#);
        assert!(result.contains(r#""hello\nworld""#), "got: {result}");
    }

    #[test]
    fn format_deeply_nested() {
        let result = fmt("[if [> [+ [* a b] c] 0] [println \"big\"] [println \"small\"]]");
        assert!(result.contains("[if"));
        // Should be idempotent
        let second = fmt(&result);
        assert_eq!(result, second);
    }

    #[test]
    fn format_empty_list() {
        let result = fmt("[]");
        assert_eq!(result.trim(), "[]");
    }

    #[test]
    fn format_empty_vec() {
        let result = fmt("#[]");
        assert_eq!(result.trim(), "#[]");
    }

    #[test]
    fn format_empty_map() {
        let result = fmt("{}");
        assert_eq!(result.trim(), "{}");
    }

    #[test]
    fn format_empty_set() {
        let result = fmt("#{}");
        assert_eq!(result.trim(), "#{}");
    }

    #[test]
    fn format_keywords() {
        let result = fmt(":foo");
        assert_eq!(result.trim(), ":foo");
    }

    #[test]
    fn format_match_arms() {
        let result = fmt("[match x [Some v] v None 0]");
        assert!(result.contains("[match x"));
    }

    // -- Doc algebra unit tests ---------------------------------------------

    #[test]
    fn render_nil() {
        assert_eq!(render(&nil(), 80), "");
    }

    #[test]
    fn render_text() {
        assert_eq!(render(&text("hello"), 80), "hello");
    }

    #[test]
    fn render_group_fits() {
        let d = group(concat_all(vec![text("a"), line(), text("b")]));
        assert_eq!(render(&d, 80), "a b");
    }

    #[test]
    fn render_group_breaks() {
        let d = group(concat_all(vec![text("a"), line(), text("b")]));
        assert_eq!(render(&d, 2), "a\nb");
    }

    #[test]
    fn render_indent() {
        let d = concat(text("a"), indent(2, concat(hard_line(), text("b"))));
        assert_eq!(render(&d, 80), "a\n  b");
    }
}
