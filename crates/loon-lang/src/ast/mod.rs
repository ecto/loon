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
