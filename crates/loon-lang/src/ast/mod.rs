pub mod typed;

use crate::syntax::Span;
use std::fmt;
use std::sync::atomic::{AtomicU32, Ordering};

static NEXT_NODE_ID: AtomicU32 = AtomicU32::new(0);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeId(pub u32);

impl NodeId {
    pub fn next() -> Self {
        Self(NEXT_NODE_ID.fetch_add(1, Ordering::Relaxed))
    }
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
    pub id: NodeId,
}

impl Expr {
    pub fn new(kind: ExprKind, span: Span) -> Self {
        Self {
            kind,
            span,
            id: NodeId::next(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    Keyword(String),
    Symbol(String),
    /// S-expression: [head args...]
    List(Vec<Expr>),
    /// Persistent vector: #[a b c]
    Vec(Vec<Expr>),
    /// Set: #{a b c}
    Set(Vec<Expr>),
    /// Map: {:key val ...}
    Map(Vec<(Expr, Expr)>),
    /// Tuple: (a, b)
    Tuple(Vec<Expr>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            ExprKind::Int(n) => write!(f, "{n}"),
            ExprKind::Float(n) => write!(f, "{n}"),
            ExprKind::Bool(b) => write!(f, "{b}"),
            ExprKind::Str(s) => write!(f, "\"{s}\""),
            ExprKind::Keyword(k) => write!(f, ":{k}"),
            ExprKind::Symbol(s) => write!(f, "{s}"),
            ExprKind::List(items) => {
                write!(f, "[")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{item}")?;
                }
                write!(f, "]")
            }
            ExprKind::Vec(items) => {
                write!(f, "#[")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{item}")?;
                }
                write!(f, "]")
            }
            ExprKind::Set(items) => {
                write!(f, "#{{")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{item}")?;
                }
                write!(f, "}}")
            }
            ExprKind::Map(pairs) => {
                write!(f, "{{")?;
                for (i, (k, v)) in pairs.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{k} {v}")?;
                }
                write!(f, "}}")
            }
            ExprKind::Tuple(items) => {
                write!(f, "(")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{item}")?;
                }
                write!(f, ")")
            }
        }
    }
}

/// Walk the AST to find the deepest node whose span contains the given byte offset.
pub fn node_at_offset(exprs: &[Expr], offset: usize) -> Option<&Expr> {
    fn walk(expr: &Expr, offset: usize) -> Option<&Expr> {
        if offset < expr.span.start || offset >= expr.span.end {
            return None;
        }
        // Try to find a deeper match in children
        match &expr.kind {
            ExprKind::List(items) | ExprKind::Vec(items) | ExprKind::Set(items) | ExprKind::Tuple(items) => {
                for item in items {
                    if let Some(deeper) = walk(item, offset) {
                        return Some(deeper);
                    }
                }
            }
            ExprKind::Map(pairs) => {
                for (k, v) in pairs {
                    if let Some(deeper) = walk(k, offset) {
                        return Some(deeper);
                    }
                    if let Some(deeper) = walk(v, offset) {
                        return Some(deeper);
                    }
                }
            }
            _ => {}
        }
        // No deeper child matched, this node is the deepest
        Some(expr)
    }

    for expr in exprs {
        if let Some(found) = walk(expr, offset) {
            return Some(found);
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;

    #[test]
    fn node_at_offset_finds_symbol() {
        // "[+ x y]" — x is at offset 3..4
        let exprs = parse("[+ x y]").unwrap();
        let node = node_at_offset(&exprs, 3);
        assert!(node.is_some());
        let node = node.unwrap();
        assert!(matches!(&node.kind, ExprKind::Symbol(s) if s == "x"));
    }

    #[test]
    fn node_at_offset_finds_int() {
        // "42" — the int literal at offset 0
        let exprs = parse("42").unwrap();
        let node = node_at_offset(&exprs, 0);
        assert!(node.is_some());
        assert!(matches!(&node.unwrap().kind, ExprKind::Int(42)));
    }

    #[test]
    fn node_at_offset_returns_none_out_of_range() {
        let exprs = parse("42").unwrap();
        let node = node_at_offset(&exprs, 100);
        assert!(node.is_none());
    }

    #[test]
    fn node_at_offset_deepest_in_nested() {
        // "[+ [* 2 3] 4]" — offset of "2" should find the int, not the list
        let exprs = parse("[+ [* 2 3] 4]").unwrap();
        // '2' is at offset 6
        let node = node_at_offset(&exprs, 6);
        assert!(node.is_some());
        assert!(matches!(&node.unwrap().kind, ExprKind::Int(2)));
    }
}

/// Pretty-print an AST with indentation.
pub fn pretty_print(expr: &Expr, indent: usize) -> String {
    let pad = "  ".repeat(indent);
    match &expr.kind {
        ExprKind::List(items) if items.len() > 2 => {
            let mut out = format!("{pad}[");
            if let Some(head) = items.first() {
                out.push_str(&format!("{head}"));
            }
            for item in &items[1..] {
                out.push('\n');
                out.push_str(&pretty_print(item, indent + 1));
            }
            out.push(']');
            out
        }
        _ => format!("{pad}{expr}"),
    }
}
