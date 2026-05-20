//! Wadler-Lindig style pretty printer for Loon source code.
//!
//! Known limitations:
//! - Numeric suffixes are lost (the parser converts e.g. `42i32` to `i64(42)`)

use crate::ast::{Expr, ExprKind, NodeId};
use crate::syntax::Comment;
use std::collections::{HashMap, HashSet};

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
            // A hard break inside a group means the group cannot fit flat — it
            // must render in Break mode so the surrounding soft `line()`s also
            // break (otherwise they'd render as spaces and we'd get garbled
            // output like a comment swallowing the line).
            Doc::HardLine => return false,
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
// Comment attachment
// ---------------------------------------------------------------------------

/// Comments attached to a particular AST node, by position relative to it.
#[derive(Default, Debug)]
struct NodeComments {
    /// Own-line comment(s) that should print before the node.
    leading: Vec<Comment>,
    /// Same-line comment(s) that should print after the node on its closing line.
    trailing: Vec<Comment>,
    /// Comments inside this list/collection that come after the last child but
    /// before the closing bracket — print on their own line(s) indented.
    dangling: Vec<Comment>,
}

/// All comment / blank-line attachments for a program, keyed by `NodeId`.
#[derive(Default, Debug)]
struct Attachments {
    by_node: HashMap<NodeId, NodeComments>,
    /// Comments at the top of the file before any expression.
    program_leading: Vec<Comment>,
    /// Comments at the bottom of the file after the last expression.
    program_trailing: Vec<Comment>,
    /// Nodes whose preceding gap (within their sequence) contained a blank line
    /// in the original source.
    blank_before: HashSet<NodeId>,
}

/// Context threaded through doc builders so they can consult attachments.
struct Ctx<'a> {
    att: &'a Attachments,
    /// Reserved for future use (currently only `att` is read inside builders).
    #[allow(dead_code)]
    src: &'a str,
}

const EMPTY_NC: &NodeComments = &NodeComments {
    leading: Vec::new(),
    trailing: Vec::new(),
    dangling: Vec::new(),
};

impl<'a> Ctx<'a> {
    fn nc(&self, id: NodeId) -> &NodeComments {
        self.att.by_node.get(&id).unwrap_or(EMPTY_NC)
    }
}

fn build_attachments(exprs: &[Expr], comments: &[Comment], source: &str) -> Attachments {
    let mut att = Attachments::default();
    if comments.is_empty() && exprs.is_empty() {
        return att;
    }
    let mut sorted: Vec<Comment> = comments.to_vec();
    sorted.sort_by_key(|c| c.span.start);
    let mut idx = 0usize;
    attach_seq(
        exprs,
        source,
        None,
        0,
        source.len(),
        &sorted,
        &mut idx,
        &mut att,
        true,
    );
    att
}

#[allow(clippy::too_many_arguments)]
fn attach_seq(
    children: &[Expr],
    source: &str,
    enclosing_id: Option<NodeId>,
    seq_start: usize,
    seq_end: usize,
    comments: &[Comment],
    idx: &mut usize,
    att: &mut Attachments,
    is_program: bool,
) {
    let mut prev_end: usize = seq_start;
    let mut prev_id: Option<NodeId> = None;

    for child in children {
        // Drain comments that fall before this child.
        while *idx < comments.len() && comments[*idx].span.start < child.span.start {
            let c = comments[*idx].clone();
            *idx += 1;
            // Same-line trailing of previous sibling?
            if let Some(pid) = prev_id {
                let between = source.get(prev_end..c.span.start).unwrap_or("");
                if !between.contains('\n') {
                    att.by_node.entry(pid).or_default().trailing.push(c.clone());
                    prev_end = c.span.end;
                    continue;
                }
                // Non-trailing comment: detect a blank line in the gap before
                // it. The blank "belongs to" the upcoming child since this
                // comment will print as leading of that child.
                if count_newlines(between) >= 2 {
                    att.blank_before.insert(child.id);
                }
            }
            // Otherwise leading of the upcoming child — or program_leading at the
            // very top of file.
            if prev_id.is_none() && is_program {
                att.program_leading.push(c.clone());
            } else {
                att.by_node
                    .entry(child.id)
                    .or_default()
                    .leading
                    .push(c.clone());
            }
            prev_end = c.span.end;
        }

        // Blank-line detection in the gap before this child (between siblings only;
        // the program_leading → first expr boundary uses its own rule).
        let gap = source.get(prev_end..child.span.start).unwrap_or("");
        if (prev_id.is_some() || (is_program && !att.program_leading.is_empty()))
            && count_newlines(gap) >= 2
        {
            att.blank_before.insert(child.id);
        }

        // Recurse into the child to handle its own nested sequences.
        attach_into(child, source, comments, idx, att);

        prev_end = child.span.end;
        prev_id = Some(child.id);
    }

    // Comments after the last child but before this sequence's end.
    while *idx < comments.len() && comments[*idx].span.start < seq_end {
        let c = comments[*idx].clone();
        *idx += 1;
        // Same-line trailing of the last child?
        if let Some(pid) = prev_id {
            let between = source.get(prev_end..c.span.start).unwrap_or("");
            if !between.contains('\n') {
                att.by_node.entry(pid).or_default().trailing.push(c.clone());
                prev_end = c.span.end;
                continue;
            }
        }
        // Dangling on the enclosing list, or program_trailing at the top level.
        if let Some(eid) = enclosing_id {
            att.by_node.entry(eid).or_default().dangling.push(c.clone());
        } else {
            att.program_trailing.push(c.clone());
        }
        prev_end = c.span.end;
    }
}

fn attach_into(
    expr: &Expr,
    source: &str,
    comments: &[Comment],
    idx: &mut usize,
    att: &mut Attachments,
) {
    match &expr.kind {
        ExprKind::List(items)
        | ExprKind::Vec(items)
        | ExprKind::Set(items)
        | ExprKind::Tuple(items) => {
            attach_seq(
                items,
                source,
                Some(expr.id),
                expr.span.start,
                expr.span.end,
                comments,
                idx,
                att,
                false,
            );
        }
        ExprKind::Map(pairs) => {
            // Flatten key/value into a synthetic sequence so comments between
            // pairs (or between a key and its value) land on the right node.
            let flat: Vec<Expr> = pairs
                .iter()
                .flat_map(|(k, v)| [k.clone(), v.clone()])
                .collect();
            attach_seq(
                &flat,
                source,
                Some(expr.id),
                expr.span.start,
                expr.span.end,
                comments,
                idx,
                att,
                false,
            );
        }
        ExprKind::DotAccess(inner, _)
        | ExprKind::Quote(inner)
        | ExprKind::Unquote(inner)
        | ExprKind::UnquoteSplice(inner) => {
            attach_into(inner, source, comments, idx, att);
        }
        _ => {} // atoms have no children
    }
}

fn count_newlines(s: &str) -> usize {
    s.bytes().filter(|&b| b == b'\n').count()
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

/// Produce the doc for an expression's core (no leading or trailing comments).
/// Comments are emitted by the enclosing sequence printer (`fmt_children`) or
/// special-form printers, which know where in the doc tree to place hard breaks
/// so the surrounding indent context applies correctly.
fn expr_to_doc(expr: &Expr, ctx: &Ctx) -> Doc {
    expr_kind_to_doc(expr, ctx)
}

/// Emit the trailing comments for a node as ` ; text` repeats — without any
/// hard break (callers decide where the break goes so it can fire inside the
/// right indent context).
fn trailing_doc(expr: &Expr, ctx: &Ctx) -> Doc {
    let nc = ctx.nc(expr.id);
    if nc.trailing.is_empty() {
        return nil();
    }
    let mut parts = Vec::new();
    for c in &nc.trailing {
        parts.push(text(" "));
        parts.push(text(c.text.clone()));
    }
    concat_all(parts)
}

/// Doc to place between a special form's header (its last header item) and its
/// indented body. If the last header item has a trailing comment, emit it here
/// followed by a hard break so the body starts on a new indented line; this
/// lives inside the indent block so the hard break uses the body's indent.
/// Otherwise emit a soft `line()` so the form can fit flat when small.
fn body_break(last_header: &Expr, ctx: &Ctx) -> Doc {
    let nc = ctx.nc(last_header.id);
    if nc.trailing.is_empty() {
        return line();
    }
    let mut parts = Vec::new();
    for c in &nc.trailing {
        parts.push(text(" "));
        parts.push(text(c.text.clone()));
    }
    parts.push(hard_line());
    concat_all(parts)
}

/// Doc to place between the body of a special form and its closing bracket.
/// If the last body item has a trailing comment, emit a hard break so the
/// closing bracket lands on its own line (a comment runs to EOL, so the bracket
/// cannot share a line with it). This lives outside the indent block so the
/// closing bracket emits at column 0.
fn close_after(last_body: &Expr, ctx: &Ctx) -> Doc {
    if ctx.nc(last_body.id).trailing.is_empty() {
        nil()
    } else {
        hard_line()
    }
}

fn expr_kind_to_doc(expr: &Expr, ctx: &Ctx) -> Doc {
    match &expr.kind {
        ExprKind::Int(n) => text(n.to_string()),
        ExprKind::Float(n) => text(format!("{n}")),
        ExprKind::Bool(b) => text(if *b { "true" } else { "false" }),
        ExprKind::Str(s) => text(format!("\"{}\"", escape_string(s))),
        ExprKind::Keyword(k) => text(format!(":{k}")),
        ExprKind::Symbol(s) => text(s.clone()),

        ExprKind::List(items) => list_to_doc(expr.id, items, ctx),
        ExprKind::Vec(items) => collection_to_doc(expr.id, "#[", "]", items, ctx),
        ExprKind::Set(items) => collection_to_doc(expr.id, "#{", "}", items, ctx),
        ExprKind::Map(pairs) => map_to_doc(expr.id, pairs, ctx),
        ExprKind::Tuple(items) => tuple_to_doc(expr.id, items, ctx),
        ExprKind::DotAccess(inner, field) => concat(
            concat(expr_to_doc(inner, ctx), text(".")),
            text(field.clone()),
        ),
        ExprKind::Quote(inner) => concat(text("`"), expr_to_doc(inner, ctx)),
        ExprKind::Unquote(inner) => concat(text("~"), expr_to_doc(inner, ctx)),
        ExprKind::UnquoteSplice(inner) => concat(text("~@"), expr_to_doc(inner, ctx)),
    }
}

/// Emit a sequence of sibling expressions with `sep` as the default separator.
/// Inserts leading comments, blank lines, and forces hard breaks around any
/// child that has leading/trailing comments or a blank line before it. Handles
/// the parent node's `dangling` comments (printed on their own lines before the
/// closing bracket) when `parent` is `Some`.
fn fmt_children(parent: Option<NodeId>, items: &[Expr], sep: Doc, ctx: &Ctx) -> Doc {
    let dangling = parent
        .map(|id| ctx.nc(id).dangling.as_slice())
        .unwrap_or(&[]);

    if items.is_empty() {
        if dangling.is_empty() {
            return nil();
        }
        let mut parts: Vec<Doc> = Vec::new();
        for c in dangling {
            parts.push(hard_line());
            parts.push(text(c.text.clone()));
        }
        return concat_all(parts);
    }

    let mut parts: Vec<Doc> = Vec::new();
    for (i, item) in items.iter().enumerate() {
        let nc = ctx.nc(item.id);

        if i > 0 {
            let prev_nc = ctx.nc(items[i - 1].id);
            let prev_trailing = !prev_nc.trailing.is_empty();
            let this_leading = !nc.leading.is_empty();
            let blank = ctx.att.blank_before.contains(&item.id);

            if prev_trailing || this_leading || blank {
                parts.push(hard_line());
                if blank {
                    parts.push(hard_line());
                }
            } else {
                parts.push(sep.clone());
            }
        }

        // Leading comments before this item — each on its own line.
        for c in &nc.leading {
            parts.push(text(c.text.clone()));
            parts.push(hard_line());
        }

        parts.push(expr_to_doc(item, ctx));
        // Same-line trailing comments. No hard break here — the next
        // sibling's `prev_trailing` check handles that, or `close_after`
        // does at the parent boundary.
        parts.push(trailing_doc(item, ctx));
    }

    // Dangling comments before the close.
    for c in dangling {
        parts.push(hard_line());
        parts.push(text(c.text.clone()));
    }

    concat_all(parts)
}

/// Format a List (s-expression) `[head args...]` with form-specific rules.
fn list_to_doc(id: NodeId, items: &[Expr], ctx: &Ctx) -> Doc {
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
                defn_to_doc(id, items, ctx)
            } else {
                fn_to_doc(id, items, ctx)
            }
        }
        Some("let") => let_to_doc(id, items, ctx),
        Some("if") => if_to_doc(id, items, ctx),
        Some("match") => match_to_doc(id, items, ctx),
        Some("pipe") => pipe_to_doc(id, items, ctx),
        Some("type") => type_to_doc(id, items, ctx),
        Some("effect") => type_to_doc(id, items, ctx), // same layout as type
        _ => generic_list_to_doc(id, items, ctx),
    }
}

/// `[fn name [params...] body...]`
/// Name+params on first line, body indented 2. Blank line between top-level defns
/// is handled in `format_program`.
fn defn_to_doc(id: NodeId, items: &[Expr], ctx: &Ctx) -> Doc {
    // items[0] = defn, items[1] = name, items[2] = params, items[3..] = body
    if items.len() < 3 {
        return generic_list_to_doc(id, items, ctx);
    }

    let keyword = expr_to_doc(&items[0], ctx);
    let name = expr_to_doc(&items[1], ctx);
    let params = expr_to_doc(&items[2], ctx);

    let header = concat_all(vec![text("["), keyword, text(" "), name, text(" "), params]);

    if items.len() == 3 {
        return concat(concat(header, trailing_doc(&items[2], ctx)), text("]"));
    }

    let body = fmt_children(Some(id), &items[3..], line(), ctx);
    let last_body = items.last().unwrap();

    group(concat_all(vec![
        header,
        indent(INDENT_SIZE, concat(body_break(&items[2], ctx), body)),
        close_after(last_body, ctx),
        text("]"),
    ]))
}

/// `[fn [params...] body...]`
fn fn_to_doc(id: NodeId, items: &[Expr], ctx: &Ctx) -> Doc {
    if items.len() < 2 {
        return generic_list_to_doc(id, items, ctx);
    }

    let keyword = expr_to_doc(&items[0], ctx);
    let params = expr_to_doc(&items[1], ctx);

    let header = concat_all(vec![text("["), keyword, text(" "), params]);

    if items.len() == 2 {
        return concat(concat(header, trailing_doc(&items[1], ctx)), text("]"));
    }

    let body = fmt_children(Some(id), &items[2..], line(), ctx);
    let last_body = items.last().unwrap();

    group(concat_all(vec![
        header,
        indent(INDENT_SIZE, concat(body_break(&items[1], ctx), body)),
        close_after(last_body, ctx),
        text("]"),
    ]))
}

/// `[let name value]` — single line if fits, break after name otherwise.
fn let_to_doc(id: NodeId, items: &[Expr], ctx: &Ctx) -> Doc {
    if items.len() < 3 {
        return generic_list_to_doc(id, items, ctx);
    }

    // Could be [let name val] or [let mut name val]
    let head = expr_to_doc(&items[0], ctx);
    let body = fmt_children(Some(id), &items[1..], line(), ctx);
    let last_body = items.last().unwrap();

    group(concat_all(vec![
        text("["),
        head,
        trailing_doc(&items[0], ctx),
        text(" "),
        body,
        close_after(last_body, ctx),
        text("]"),
    ]))
}

/// `[if cond then else]` — inline if fits; cond on same line, then/else indented.
fn if_to_doc(id: NodeId, items: &[Expr], ctx: &Ctx) -> Doc {
    if items.len() < 3 {
        return generic_list_to_doc(id, items, ctx);
    }

    let keyword = expr_to_doc(&items[0], ctx);
    let cond = expr_to_doc(&items[1], ctx);

    let body = fmt_children(Some(id), &items[2..], line(), ctx);
    let last_body = items.last().unwrap();

    group(concat_all(vec![
        text("["),
        keyword,
        text(" "),
        cond,
        indent(INDENT_SIZE, concat(body_break(&items[1], ctx), body)),
        close_after(last_body, ctx),
        text("]"),
    ]))
}

/// `[match scrutinee pat body pat body ...]` — each (pat, body) on its own line,
/// never collapses onto a single line.
fn match_to_doc(id: NodeId, items: &[Expr], ctx: &Ctx) -> Doc {
    if items.len() < 2 {
        return generic_list_to_doc(id, items, ctx);
    }

    let keyword = expr_to_doc(&items[0], ctx);
    let scrutinee = expr_to_doc(&items[1], ctx);

    let header = concat_all(vec![text("["), keyword, text(" "), scrutinee]);

    if items.len() == 2 {
        return concat(concat(header, trailing_doc(&items[1], ctx)), text("]"));
    }

    // Build a doc per (pattern, body) pair so arms never collapse onto one line.
    // Comments attached to the pattern node naturally precede the pair; comments
    // on the body land trailing after it.
    let arms = &items[2..];
    let mut pair_docs: Vec<Doc> = Vec::new();
    let mut i = 0;
    while i < arms.len() {
        let pat = &arms[i];

        let nc = ctx.nc(pat.id);
        let blank = ctx.att.blank_before.contains(&pat.id);
        let mut pair: Vec<Doc> = Vec::new();
        if !pair_docs.is_empty() && blank {
            pair.push(hard_line());
        }
        for c in &nc.leading {
            pair.push(text(c.text.clone()));
            pair.push(hard_line());
        }

        if i + 1 < arms.len() {
            // pattern body
            pair.push(expr_to_doc(pat, ctx));
            pair.push(trailing_doc(pat, ctx));
            pair.push(text(" "));
            pair.push(expr_to_doc(&arms[i + 1], ctx));
            pair.push(trailing_doc(&arms[i + 1], ctx));
            i += 2;
        } else {
            // odd trailing pattern with no body
            pair.push(expr_to_doc(pat, ctx));
            pair.push(trailing_doc(pat, ctx));
            i += 1;
        }
        pair_docs.push(concat_all(pair));
    }

    let arms_doc = intersperse(pair_docs, hard_line());

    // Dangling comments inside the match list (before `]`).
    let dangling = ctx.nc(id).dangling.as_slice();
    let mut tail: Vec<Doc> = Vec::new();
    for c in dangling {
        tail.push(hard_line());
        tail.push(text(c.text.clone()));
    }

    let last_arm = items.last().unwrap();
    let between = if ctx.nc(items[1].id).trailing.is_empty() {
        hard_line()
    } else {
        body_break(&items[1], ctx)
    };

    concat_all(vec![
        header,
        indent(
            INDENT_SIZE,
            concat(between, concat_all(vec![arms_doc, concat_all(tail)])),
        ),
        close_after(last_arm, ctx),
        text("]"),
    ])
}

/// `[pipe expr steps...]` — first expr on same line, each step indented on own line.
fn pipe_to_doc(id: NodeId, items: &[Expr], ctx: &Ctx) -> Doc {
    if items.len() < 2 {
        return generic_list_to_doc(id, items, ctx);
    }

    let keyword = expr_to_doc(&items[0], ctx);
    let first = expr_to_doc(&items[1], ctx);

    let header = concat_all(vec![text("["), keyword, text(" "), first]);

    if items.len() == 2 {
        return concat(concat(header, trailing_doc(&items[1], ctx)), text("]"));
    }

    let steps = fmt_children(Some(id), &items[2..], line(), ctx);
    let last_body = items.last().unwrap();

    group(concat_all(vec![
        header,
        indent(INDENT_SIZE, concat(body_break(&items[1], ctx), steps)),
        close_after(last_body, ctx),
        text("]"),
    ]))
}

/// `[type Name constructors...]` or `[effect Name ops...]`
/// Name on first line, constructors/ops indented.
fn type_to_doc(id: NodeId, items: &[Expr], ctx: &Ctx) -> Doc {
    if items.len() < 2 {
        return generic_list_to_doc(id, items, ctx);
    }

    let keyword = expr_to_doc(&items[0], ctx);
    let name = expr_to_doc(&items[1], ctx);

    let header = concat_all(vec![text("["), keyword, text(" "), name]);

    if items.len() == 2 {
        return concat(concat(header, trailing_doc(&items[1], ctx)), text("]"));
    }

    let body = fmt_children(Some(id), &items[2..], line(), ctx);
    let last_body = items.last().unwrap();
    // `type` / `effect` always render one constructor per line (hard_line); but
    // if items[1] has a trailing comment, emit it before the hard break.
    let between = if ctx.nc(items[1].id).trailing.is_empty() {
        hard_line()
    } else {
        body_break(&items[1], ctx)
    };

    concat_all(vec![
        header,
        indent(INDENT_SIZE, concat(between, body)),
        close_after(last_body, ctx),
        text("]"),
    ])
}

/// Generic list: inline if fits, head on first line + args indented.
fn generic_list_to_doc(id: NodeId, items: &[Expr], ctx: &Ctx) -> Doc {
    if items.is_empty() {
        return text("[]");
    }

    if items.len() == 1 {
        return concat_all(vec![
            text("["),
            expr_to_doc(&items[0], ctx),
            trailing_doc(&items[0], ctx),
            close_after(&items[0], ctx),
            text("]"),
        ]);
    }

    let head = expr_to_doc(&items[0], ctx);
    let args = fmt_children(Some(id), &items[1..], line(), ctx);
    let last_body = items.last().unwrap();

    group(concat_all(vec![
        text("["),
        head,
        indent(INDENT_SIZE, concat(body_break(&items[0], ctx), args)),
        close_after(last_body, ctx),
        text("]"),
    ]))
}

/// Vec `#[a b c]` and Set `#{a b c}`: inline if fits, one per line otherwise.
fn collection_to_doc(id: NodeId, open: &str, close: &str, items: &[Expr], ctx: &Ctx) -> Doc {
    if items.is_empty() {
        return text(format!("{open}{close}"));
    }

    let body = fmt_children(Some(id), items, line(), ctx);
    let last = items.last().unwrap();

    group(concat_all(vec![
        text(open),
        indent(INDENT_SIZE, body),
        close_after(last, ctx),
        text(close),
    ]))
}

/// Map `{:key val ...}`: key-value pairs aligned, one per line if > 80 chars.
fn map_to_doc(id: NodeId, pairs: &[(Expr, Expr)], ctx: &Ctx) -> Doc {
    if pairs.is_empty() {
        return text("{}");
    }

    // For comment purposes the parser tree treats k and v as separate nodes; but
    // here we group each pair tightly (no break between k and v). Comments
    // attached to the key are leading on the pair; comments attached to the
    // value are trailing on the pair.
    let mut pair_docs: Vec<Doc> = Vec::new();
    let mut leading_per_pair: Vec<Vec<Comment>> = Vec::new();
    let mut blank_per_pair: Vec<bool> = Vec::new();
    let mut trailing_after_pair: Vec<Vec<Comment>> = Vec::new();

    for (k, v) in pairs.iter() {
        let knc = ctx.nc(k.id);
        let vnc = ctx.nc(v.id);
        leading_per_pair.push(knc.leading.clone());
        blank_per_pair.push(ctx.att.blank_before.contains(&k.id));
        // Build the pair body: k + space + v (without leading on k, since we lift
        // it to before the pair; trailing on v stays attached to v).
        let k_core = expr_kind_to_doc(k, ctx);
        let mut k_trailing: Vec<Doc> = Vec::new();
        for c in &knc.trailing {
            k_trailing.push(text(" "));
            k_trailing.push(text(c.text.clone()));
        }
        let v_doc = expr_to_doc(v, ctx);
        let pair = concat_all(vec![k_core, concat_all(k_trailing), text(" "), v_doc]);
        pair_docs.push(pair);
        trailing_after_pair.push(vnc.trailing.clone());
    }

    // Assemble with separators.
    let mut parts: Vec<Doc> = Vec::new();
    for (i, pd) in pair_docs.into_iter().enumerate() {
        if i > 0 {
            let prev_trailing = !trailing_after_pair[i - 1].is_empty();
            let this_leading = !leading_per_pair[i].is_empty();
            let blank = blank_per_pair[i];
            if prev_trailing || this_leading || blank {
                parts.push(hard_line());
                if blank {
                    parts.push(hard_line());
                }
            } else {
                parts.push(line());
            }
        }
        for c in &leading_per_pair[i] {
            parts.push(text(c.text.clone()));
            parts.push(hard_line());
        }
        parts.push(pd);
    }

    // Dangling on the map node itself.
    for c in &ctx.nc(id).dangling {
        parts.push(hard_line());
        parts.push(text(c.text.clone()));
    }

    group(concat_all(vec![
        text("{"),
        indent(INDENT_SIZE, concat_all(parts)),
        text("}"),
    ]))
}

/// Tuple `(a, b)`.
fn tuple_to_doc(id: NodeId, items: &[Expr], ctx: &Ctx) -> Doc {
    if items.is_empty() {
        return text("()");
    }

    let body = fmt_children(Some(id), items, line(), ctx);
    let last = items.last().unwrap();

    group(concat_all(vec![
        text("("),
        body,
        close_after(last, ctx),
        text(")"),
    ]))
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Format a parsed Loon program back to source code (comment-less variant kept
/// for back-compat). Inserts blank lines between top-level `defn` forms.
pub fn format_program(exprs: &[Expr]) -> String {
    format_program_with_comments(exprs, &[], "")
}

/// Format a parsed Loon program back to source code, preserving comments and
/// intentional blank lines from `source`. `comments` are the comments captured
/// by `parser::parse_with_comments`; `source` is the original buffer (used for
/// position-based attachment decisions).
pub fn format_program_with_comments(exprs: &[Expr], comments: &[Comment], source: &str) -> String {
    if exprs.is_empty() && comments.is_empty() {
        return String::new();
    }

    let att = build_attachments(exprs, comments, source);
    let ctx = Ctx {
        att: &att,
        src: source,
    };

    let mut parts: Vec<String> = Vec::new();

    // Program-leading comments at the very top of the file.
    for c in &ctx.att.program_leading {
        parts.push(c.text.clone());
    }

    for (i, expr) in exprs.iter().enumerate() {
        let nc = ctx.nc(expr.id);

        // Leading comments on this top-level expr (own-line).
        for c in &nc.leading {
            parts.push(c.text.clone());
        }

        // Render the expr itself, with its own trailing comments appended.
        let doc = expr_kind_to_doc(expr, &ctx);
        let mut rendered = render(&doc, DEFAULT_WIDTH);
        for c in &nc.trailing {
            rendered.push(' ');
            rendered.push_str(&c.text);
        }
        parts.push(rendered);

        if i + 1 < exprs.len() {
            let next = &exprs[i + 1];
            let blank = ctx.att.blank_before.contains(&next.id);
            let is_defn = is_top_level_defn(expr) || is_top_level_defn(next);

            // Blank line between top-level defns (existing behavior) or when
            // the source explicitly had one. Trailing/leading comments alone do
            // NOT force a blank line at the top level — the `\n` from joining
            // is sufficient, and inserting a blank line would distort the
            // user's original spacing on every save.
            if blank || is_defn {
                parts.push(String::new());
            }
        }
    }

    // Program-trailing comments at the bottom.
    for c in &ctx.att.program_trailing {
        parts.push(c.text.clone());
    }

    let mut result = parts.join("\n");
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
    use crate::parser::{parse, parse_with_comments};

    fn fmt(src: &str) -> String {
        let exprs = parse(src).expect("parse failed");
        format_program(&exprs)
    }

    fn fmt_with_comments(src: &str) -> String {
        let (exprs, comments) = parse_with_comments(src).expect("parse failed");
        format_program_with_comments(&exprs, &comments, src)
    }

    // -- Idempotency tests --------------------------------------------------

    fn assert_idempotent(src: &str) {
        let first = fmt(src);
        let second = fmt(&first);
        assert_eq!(first, second, "formatting is not idempotent for: {src}");
    }

    fn assert_idempotent_with_comments(src: &str) {
        let first = fmt_with_comments(src);
        let second = fmt_with_comments(&first);
        assert_eq!(
            first, second,
            "formatting (with comments) is not idempotent for: {src}\nfirst:\n{first}\nsecond:\n{second}"
        );
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

    #[test]
    fn match_arms_stay_on_own_lines() {
        // Previously this collapsed to one line; now each (pat, body) pair is
        // on its own indented line.
        let src =
            "[fn fib [n]\n  [match n\n    0 0\n    1 1\n    n [+ [fib [- n 1]] [fib [- n 2]]]]]";
        let result = fmt(src);
        // Each pair on its own line means the result has 3+ newlines inside
        // the match body.
        let match_part = result.split("[match").nth(1).unwrap_or("");
        let body = match_part.split("]]").next().unwrap_or("");
        let newlines = body.matches('\n').count();
        assert!(
            newlines >= 3,
            "expected match arms on separate lines (>=3 newlines in body), got {newlines}:\n{result}"
        );
    }

    // -- Comment preservation tests -----------------------------------------

    #[test]
    fn leading_comment_preserved() {
        let src = "; header\n[let x 42]\n";
        let out = fmt_with_comments(src);
        assert!(out.contains("; header"), "got:\n{out}");
        assert!(out.contains("[let x 42]"), "got:\n{out}");
        assert_idempotent_with_comments(src);
    }

    #[test]
    fn trailing_same_line_comment_preserved() {
        let src = "[let x 42] ; the answer\n";
        let out = fmt_with_comments(src);
        assert!(out.contains("[let x 42]"), "got:\n{out}");
        assert!(out.contains("; the answer"), "got:\n{out}");
        assert_idempotent_with_comments(src);
    }

    #[test]
    fn comment_only_file_round_trips() {
        let src = "; just a comment\n";
        let out = fmt_with_comments(src);
        assert!(out.contains("; just a comment"), "got:\n{out}");
        assert_idempotent_with_comments(src);
    }

    #[test]
    fn consecutive_comments_preserved() {
        let src = "; one\n; two\n; three\n[let x 1]\n";
        let out = fmt_with_comments(src);
        assert!(out.contains("; one"), "got:\n{out}");
        assert!(out.contains("; two"), "got:\n{out}");
        assert!(out.contains("; three"), "got:\n{out}");
        assert_idempotent_with_comments(src);
    }

    #[test]
    fn comment_inside_list_preserved() {
        let src = "[fn foo []\n  ; body comment\n  [+ 1 2]]\n";
        let out = fmt_with_comments(src);
        assert!(out.contains("; body comment"), "got:\n{out}");
        assert_idempotent_with_comments(src);
    }

    #[test]
    fn eof_comment_preserved() {
        let src = "[let x 1]\n; trailing\n";
        let out = fmt_with_comments(src);
        assert!(out.contains("; trailing"), "got:\n{out}");
        assert_idempotent_with_comments(src);
    }

    #[test]
    fn user_blank_line_preserved() {
        let src = "[let x 1]\n\n[let y 2]\n";
        let out = fmt_with_comments(src);
        assert!(
            out.contains("\n\n"),
            "expected a blank line between forms, got:\n{out}"
        );
        assert_idempotent_with_comments(src);
    }

    #[test]
    fn multiple_blank_lines_collapse_to_one() {
        let src = "[let x 1]\n\n\n\n[let y 2]\n";
        let out = fmt_with_comments(src);
        // Idempotence is what matters — repeated blank lines must collapse to
        // a stable single blank-line form.
        let second = fmt_with_comments(&out);
        assert_eq!(
            out, second,
            "expected stable collapse, got:\nfirst:\n{out}\nsecond:\n{second}"
        );
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
