use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use std::sync::{Arc, Condvar, Mutex};

use super::InterpError;

/// Shared reference to a serde_json::Value for lazy JSON access.
pub type JsonRef = Arc<serde_json::Value>;

/// Opaque handle to a DOM node (index into JS-side node table).
pub type DomHandle = u32;

/// Channel identifier.
pub type ChannelId = u32;

pub type BuiltinFn = Arc<dyn Fn(&str, &[Value]) -> Result<Value, InterpError> + Send + Sync>;

/// An insertion-ordered persistent map backing `Value::Map`. `keys`, `values`,
/// `iter`, and display all follow the order keys were first inserted, matching
/// the EIR VM's `OrdMap` (see `eir/vm.rs`) so both backends print/iterate maps
/// identically. Lookup stays O(1) via the inner `imbl::HashMap`; an
/// `imbl::Vector` of keys records order. Both halves are persistent, so
/// clone/share stays cheap.
///
/// Equality and hashing are ORDER-INDEPENDENT: two maps with the same keys and
/// values compare equal and hash equal regardless of insertion order, so
/// `{:a 1 :b 2} == {:b 2 :a 1}` stays true and maps remain usable as set/map
/// keys.
#[derive(Clone, Default)]
pub struct OrdMap {
    map: imbl::HashMap<Value, Value>,
    order: imbl::Vector<Value>, // keys, insertion order, no duplicates
}

impl OrdMap {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn len(&self) -> usize {
        self.order.len()
    }
    pub fn is_empty(&self) -> bool {
        self.order.is_empty()
    }
    pub fn get(&self, k: &Value) -> Option<&Value> {
        self.map.get(k)
    }
    pub fn contains_key(&self, k: &Value) -> bool {
        self.map.contains_key(k)
    }
    /// Insert in place; a brand-new key is appended to the order, an existing
    /// key keeps its position (only its value updates). Returns the previous
    /// value, if any.
    pub fn insert(&mut self, k: Value, v: Value) -> Option<Value> {
        let prev = self.map.insert(k.clone(), v);
        if prev.is_none() {
            self.order.push_back(k);
        }
        prev
    }
    /// Persistent insert: returns a new map with `k`→`v`, mirroring
    /// `imbl::HashMap::update`. New key appends; existing key keeps position.
    pub fn update(&self, k: Value, v: Value) -> Self {
        let mut out = self.clone();
        out.insert(k, v);
        out
    }
    /// Persistent remove: returns a new map without `k`, preserving the order
    /// of the remaining keys. Mirrors `imbl::HashMap::without`.
    pub fn without(&self, k: &Value) -> Self {
        if !self.map.contains_key(k) {
            return self.clone();
        }
        let mut map = self.map.clone();
        map.remove(k);
        let order = self.order.iter().filter(|x| *x != k).cloned().collect();
        OrdMap { map, order }
    }
    /// Left-biased union (values already in `self` win), preserving `self`'s
    /// order and appending `other`'s new keys in their order. Mirrors the VM's
    /// merge semantics.
    pub fn union(&self, other: Self) -> Self {
        let mut out = self.clone();
        for k in other.order.iter() {
            if !out.map.contains_key(k) {
                out.insert(k.clone(), other.map.get(k).unwrap().clone());
            }
        }
        out
    }
    pub fn keys(&self) -> impl Iterator<Item = &Value> + '_ {
        self.order.iter()
    }
    pub fn values(&self) -> impl Iterator<Item = &Value> + '_ {
        self.order.iter().map(move |k| self.map.get(k).unwrap())
    }
    pub fn iter(&self) -> impl Iterator<Item = (&Value, &Value)> + '_ {
        self.order
            .iter()
            .map(move |k| (k, self.map.get(k).unwrap()))
    }
}

impl fmt::Debug for OrdMap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}

impl PartialEq for OrdMap {
    /// Order-independent structural equality: same keys mapping to same values.
    fn eq(&self, other: &Self) -> bool {
        self.map == other.map
    }
}
impl Eq for OrdMap {}

impl Hash for OrdMap {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Commutative XOR — order-independent, matches the old imbl::HashMap.
        let mut h: u64 = 0;
        for (k, v) in self.map.iter() {
            let mut sub = std::hash::DefaultHasher::new();
            k.hash(&mut sub);
            v.hash(&mut sub);
            h ^= sub.finish();
        }
        h.hash(state);
    }
}

impl FromIterator<(Value, Value)> for OrdMap {
    fn from_iter<I: IntoIterator<Item = (Value, Value)>>(iter: I) -> Self {
        let mut m = OrdMap::new();
        for (k, v) in iter {
            m.insert(k, v);
        }
        m
    }
}

impl IntoIterator for OrdMap {
    type Item = (Value, Value);
    type IntoIter = std::vec::IntoIter<(Value, Value)>;
    fn into_iter(self) -> Self::IntoIter {
        self.order
            .iter()
            .map(|k| (k.clone(), self.map.get(k).unwrap().clone()))
            .collect::<Vec<_>>()
            .into_iter()
    }
}

impl<'a> IntoIterator for &'a OrdMap {
    type Item = (&'a Value, &'a Value);
    type IntoIter = std::vec::IntoIter<(&'a Value, &'a Value)>;
    fn into_iter(self) -> Self::IntoIter {
        self.order
            .iter()
            .map(|k| (k, self.map.get(k).unwrap()))
            .collect::<Vec<_>>()
            .into_iter()
    }
}

/// A function parameter: simple name or destructuring pattern.
#[derive(Debug, Clone)]
pub enum Param {
    Simple(String),
    VecDestructure(Vec<Param>),
    MapDestructure(Vec<(String, Option<crate::ast::Expr>)>),
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
    Str(Rc<str>),
    Keyword(Rc<str>),
    Vec(imbl::Vector<Value>),
    Set(imbl::HashSet<Value>),
    Map(OrdMap),
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
    /// Lazy JSON value — wraps serde_json::Value, converts on access.
    Json(JsonRef),
    Unit,
}

/// SAFETY: Value contains Rc<str> and (via LoonFn) Rc<[Expr]>, neither of which
/// is Send/Sync. This is safe because the interpreter is single-threaded in WASM,
/// and thread spawns use deep_clone to create independent copies.
unsafe impl Send for Value {}
unsafe impl Sync for Value {}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            Value::Int(n) => n.hash(state),
            Value::Float(f) => f.to_bits().hash(state),
            Value::Bool(b) => b.hash(state),
            Value::Str(s) => s.hash(state),
            Value::Keyword(k) => k.hash(state),
            Value::Vec(v) => {
                for item in v.iter() {
                    item.hash(state);
                }
            }
            Value::Set(s) => {
                // Commutative XOR for order-independent hashing
                let mut h: u64 = 0;
                for item in s.iter() {
                    let mut sub = std::hash::DefaultHasher::new();
                    item.hash(&mut sub);
                    h ^= sub.finish();
                }
                h.hash(state);
            }
            Value::Map(m) => {
                // Commutative XOR for order-independent hashing
                let mut h: u64 = 0;
                for (k, v) in m.iter() {
                    let mut sub = std::hash::DefaultHasher::new();
                    k.hash(&mut sub);
                    v.hash(&mut sub);
                    h ^= sub.finish();
                }
                h.hash(state);
            }
            Value::Tuple(items) => {
                for item in items {
                    item.hash(state);
                }
            }
            Value::Adt(tag, fields) => {
                tag.hash(state);
                for field in fields {
                    field.hash(state);
                }
            }
            Value::DomNode(h) => h.hash(state),
            Value::ChannelTx(id) => id.hash(state),
            Value::ChannelRx(id) => id.hash(state),
            Value::Future(inner) => inner.hash(state),
            // Non-hashable types use sentinel
            Value::Fn(_) | Value::Builtin(..) => 0u8.hash(state),
            Value::AsyncSlot(_) => 0u8.hash(state),
            Value::Json(j) => {
                // Hash the JSON string representation
                j.to_string().hash(state);
            }
            Value::Unit => {}
        }
    }
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        // Canonical truthiness (matches the EIR VM, the semantic reference):
        // the falsy set is exactly {false, (), None} — a value is truthy
        // unless it says no (false) or says nothing ((), None). Everything
        // else is truthy — including integer 0, 0.0, "" (empty string),
        // empty collections, and Some(x) for ANY x (even Some(false)).
        match self {
            Value::Bool(false) | Value::Unit => false,
            Value::Adt(tag, fields) => !(tag == "None" && fields.is_empty()),
            _ => true,
        }
    }

    pub fn is_callable(&self) -> bool {
        matches!(self, Value::Fn(_) | Value::Builtin(..))
    }

    /// Display without quotes for strings (used in println, str concat)
    pub fn display_str(&self) -> String {
        match self {
            Value::Str(s) => s.to_string(),
            Value::Json(j) => match j.as_ref() {
                serde_json::Value::String(s) => s.clone(),
                other => other.to_string(),
            },
            other => format!("{other}"),
        }
    }

    /// Convert a JSON primitive to a native Value. Objects/arrays stay as Json.
    pub fn from_json(j: &serde_json::Value) -> Value {
        match j {
            serde_json::Value::Null => Value::Unit,
            serde_json::Value::Bool(b) => Value::Bool(*b),
            serde_json::Value::Number(n) => {
                if let Some(i) = n.as_i64() {
                    Value::Int(i)
                } else {
                    Value::Float(n.as_f64().unwrap_or(0.0))
                }
            }
            serde_json::Value::String(s) => Value::Str(s.as_str().into()),
            serde_json::Value::Array(_) | serde_json::Value::Object(_) => {
                Value::Json(Arc::new(j.clone()))
            }
        }
    }

    /// Convert a JSON value to a native Value, wrapping compounds as Json.
    pub fn from_json_arc(j: JsonRef) -> Value {
        match j.as_ref() {
            serde_json::Value::Null => Value::Unit,
            serde_json::Value::Bool(b) => Value::Bool(*b),
            serde_json::Value::Number(n) => {
                if let Some(i) = n.as_i64() {
                    Value::Int(i)
                } else {
                    Value::Float(n.as_f64().unwrap_or(0.0))
                }
            }
            serde_json::Value::String(s) => Value::Str(s.as_str().into()),
            _ => Value::Json(j),
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
            Value::Fn(lf) => write!(f, "<fn {}>", lf.name.as_deref().unwrap_or("anonymous")),
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
            Value::Json(j) => match j.as_ref() {
                serde_json::Value::String(s) => write!(f, "\"{s}\""),
                serde_json::Value::Null => write!(f, "()"),
                other => write!(
                    f,
                    "<json {}>",
                    &other.to_string()[..64.min(other.to_string().len())]
                ),
            },
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
            // Json comparisons — convert to native for comparison
            (Value::Json(j), other) | (other, Value::Json(j)) => {
                let native = Value::from_json(j);
                if matches!(native, Value::Json(_)) {
                    // Both compound Json
                    if let Value::Json(j2) = other {
                        j == j2
                    } else {
                        false
                    }
                } else {
                    native == *other
                }
            }
            (Value::Unit, Value::Unit) => true,
            _ => false,
        }
    }
}
