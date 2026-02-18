use std::fmt;
use std::rc::Rc;
use std::sync::{Arc, Condvar, Mutex};

use super::InterpError;

/// Opaque handle to a DOM node (index into JS-side node table).
pub type DomHandle = u32;

/// Channel identifier.
pub type ChannelId = u32;

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
    /// Each clause: (params, body_exprs). Body is Rc-shared so cloning a fn is cheap.
    pub clauses: Vec<(Vec<Param>, Rc<[crate::ast::Expr]>)>,
    /// Captured environment (for closures and recursive calls)
    pub captured_env: Option<super::env::Env>,
}

/// SAFETY: LoonFn contains Rc<[Expr]> which is not Send/Sync.
/// Same rationale as Env — single-threaded in WASM, deep_clone for thread spawns.
unsafe impl Send for LoonFn {}
unsafe impl Sync for LoonFn {}

impl LoonFn {
    /// Deep clone for thread safety — creates independent copies of all Rc's.
    pub fn deep_clone(&self) -> Self {
        Self {
            name: self.name.clone(),
            clauses: self.clauses.clone(), // Rc<[Expr]> bump is fine, AST is immutable
            captured_env: self.captured_env.as_ref().map(|e| e.deep_clone()),
        }
    }
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
    ChannelTx(ChannelId),
    ChannelRx(ChannelId),
    Future(Box<Value>),
    /// Async slot — result of a spawned thread, awaitable via Condvar.
    AsyncSlot(Arc<(Mutex<Option<Value>>, Condvar)>),
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
            Value::ChannelTx(id) => write!(f, "<channel-tx {id}>"),
            Value::ChannelRx(id) => write!(f, "<channel-rx {id}>"),
            Value::Future(inner) => write!(f, "<future {inner}>"),
            Value::AsyncSlot(_) => write!(f, "<async-slot>"),
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
            (Value::ChannelTx(a), Value::ChannelTx(b)) => a == b,
            (Value::ChannelRx(a), Value::ChannelRx(b)) => a == b,
            (Value::Future(a), Value::Future(b)) => a == b,
            (Value::AsyncSlot(_), Value::AsyncSlot(_)) => false,
            (Value::Unit, Value::Unit) => true,
            _ => false,
        }
    }
}
