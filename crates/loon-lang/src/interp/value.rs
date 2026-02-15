use std::fmt;
use std::sync::Arc;

use super::InterpError;

/// Opaque handle to a DOM node (index into JS-side node table).
pub type DomHandle = u32;

pub type BuiltinFn = Arc<dyn Fn(&str, &[Value]) -> Result<Value, InterpError> + Send + Sync>;

/// A function parameter: simple name or destructuring pattern.
#[derive(Debug, Clone)]
pub enum Param {
    Simple(String),
    VecDestructure(Vec<Param>),
    MapDestructure(Vec<String>),
    /// Rest parameter: `& name` — collects remaining args into a Vec
    Rest(String),
}

#[derive(Clone)]
pub struct LoonFn {
    pub name: Option<String>,
    /// Each clause: (params, body_exprs)
    pub clauses: Vec<(Vec<Param>, Vec<crate::ast::Expr>)>,
    /// Captured environment (for closures and recursive calls)
    pub captured_env: Option<super::env::Env>,
}

impl fmt::Debug for LoonFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "LoonFn({}, {} clauses)",
            self.name.as_deref().unwrap_or("anon"),
            self.clauses.len()
        )
    }
}

#[derive(Clone)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    Keyword(String),
    Vec(Vec<Value>),
    Set(Vec<Value>),
    Map(Vec<(Value, Value)>),
    Tuple(Vec<Value>),
    Fn(LoonFn),
    Builtin(String, BuiltinFn),
    Adt(String, Vec<Value>),
    DomNode(DomHandle),
    Unit,
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Bool(false) | Value::Unit | Value::Int(0))
    }

    pub fn is_callable(&self) -> bool {
        matches!(self, Value::Fn(_) | Value::Builtin(..))
    }

    /// Display without quotes for strings (used in println, str concat)
    pub fn display_str(&self) -> String {
        match self {
            Value::Str(s) => s.clone(),
            other => format!("{other}"),
        }
    }
}


impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{n}"),
            Value::Float(n) => write!(f, "{n}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Str(s) => write!(f, "\"{s}\""),
            Value::Keyword(k) => write!(f, ":{k}"),
            Value::Vec(items) => {
                write!(f, "#[")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{item}")?;
                }
                write!(f, "]")
            }
            Value::Set(items) => {
                write!(f, "#{{")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{item}")?;
                }
                write!(f, "}}")
            }
            Value::Map(pairs) => {
                write!(f, "{{")?;
                for (i, (k, v)) in pairs.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{k} {v}")?;
                }
                write!(f, "}}")
            }
            Value::Tuple(items) => {
                write!(f, "(")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{item}")?;
                }
                write!(f, ")")
            }
            Value::Fn(lf) => write!(
                f,
                "<fn {}>",
                lf.name.as_deref().unwrap_or("anonymous")
            ),
            Value::Builtin(name, _) => write!(f, "<builtin {name}>"),
            Value::Adt(tag, fields) if fields.is_empty() => write!(f, "{tag}"),
            Value::Adt(tag, fields) => {
                write!(f, "[{tag}")?;
                for field in fields {
                    write!(f, " {field}")?;
                }
                write!(f, "]")
            }
            Value::DomNode(h) => write!(f, "<dom-node {h}>"),
            Value::Unit => write!(f, "()"),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Keyword(a), Value::Keyword(b)) => a == b,
            (Value::Vec(a), Value::Vec(b)) => a == b,
            (Value::Set(a), Value::Set(b)) => a == b,
            (Value::Map(a), Value::Map(b)) => a == b,
            (Value::Tuple(a), Value::Tuple(b)) => a == b,
            (Value::Adt(a, af), Value::Adt(b, bf)) => a == b && af == bf,
            (Value::DomNode(a), Value::DomNode(b)) => a == b,
            (Value::Unit, Value::Unit) => true,
            _ => false,
        }
    }
}
