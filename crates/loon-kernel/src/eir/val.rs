//! Runtime values.
//!
//! Plain `Rc`-backed enum rather than the host VM's NaN-boxed `Value64`.
//! The unikernel's bottleneck is not value representation yet, and an
//! obvious layout is worth more here than a fast one.

use alloc::rc::Rc;
use alloc::string::String;
use alloc::vec::Vec;

use super::FuncId;

#[derive(Clone)]
pub enum Val {
    Unit,
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(Rc<String>),
    Keyword(Rc<String>),
    Vec(Rc<Vec<Val>>),
    Tup(Rc<Vec<Val>>),
    Set(Rc<Vec<Val>>),
    /// Insertion-ordered association list. Small-map territory; a real
    /// hash map only pays off past sizes the kernel does not yet see.
    Map(Rc<Vec<(Val, Val)>>),
    Adt(u16, Rc<Vec<Val>>),
    Closure(FuncId, Rc<Vec<Val>>),
    /// A captured continuation — the value `resume` is bound to.
    Cont(Rc<super::vm::Continuation>),
}

impl Val {
    /// Loon truthiness: only `false` and unit are falsey. Matches the
    /// canonical ruling the host VM implements — 0 and "" are truthy.
    pub fn truthy(&self) -> bool {
        !matches!(self, Val::Bool(false) | Val::Unit)
    }

    pub fn type_name(&self) -> &'static str {
        match self {
            Val::Unit => "unit",
            Val::Int(_) => "int",
            Val::Float(_) => "float",
            Val::Bool(_) => "bool",
            Val::Str(_) => "string",
            Val::Keyword(_) => "keyword",
            Val::Vec(_) => "vector",
            Val::Tup(_) => "tuple",
            Val::Set(_) => "set",
            Val::Map(_) => "map",
            Val::Adt(..) => "adt",
            Val::Closure(..) => "function",
            Val::Cont(_) => "continuation",
        }
    }
}

impl PartialEq for Val {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Val::Unit, Val::Unit) => true,
            (Val::Int(a), Val::Int(b)) => a == b,
            (Val::Float(a), Val::Float(b)) => a == b,
            (Val::Int(a), Val::Float(b)) | (Val::Float(b), Val::Int(a)) => (*a as f64) == *b,
            (Val::Bool(a), Val::Bool(b)) => a == b,
            (Val::Str(a), Val::Str(b)) | (Val::Keyword(a), Val::Keyword(b)) => a == b,
            (Val::Vec(a), Val::Vec(b))
            | (Val::Tup(a), Val::Tup(b))
            | (Val::Set(a), Val::Set(b)) => a == b,
            (Val::Map(a), Val::Map(b)) => {
                a.len() == b.len()
                    && a.iter()
                        .all(|(k, v)| b.iter().any(|(k2, v2)| k == k2 && v == v2))
            }
            (Val::Adt(t1, a), Val::Adt(t2, b)) => t1 == t2 && a == b,
            _ => false,
        }
    }
}

/// Display, in the same shape the host VM prints.
pub fn show(v: &Val) -> String {
    let mut s = String::new();
    write_val(&mut s, v);
    s
}

fn write_val(out: &mut String, v: &Val) {
    use core::fmt::Write;
    match v {
        Val::Unit => out.push_str("()"),
        Val::Int(n) => {
            let _ = write!(out, "{n}");
        }
        Val::Float(f) => {
            let _ = write!(out, "{f}");
        }
        Val::Bool(b) => out.push_str(if *b { "true" } else { "false" }),
        Val::Str(s) => out.push_str(s),
        Val::Keyword(s) => {
            out.push(':');
            out.push_str(s);
        }
        Val::Vec(xs) | Val::Set(xs) => {
            out.push_str(if matches!(v, Val::Set(_)) { "#{" } else { "#[" });
            for (i, x) in xs.iter().enumerate() {
                if i > 0 {
                    out.push(' ');
                }
                write_val(out, x);
            }
            out.push(if matches!(v, Val::Set(_)) { '}' } else { ']' });
        }
        Val::Tup(xs) => {
            out.push('(');
            for (i, x) in xs.iter().enumerate() {
                if i > 0 {
                    out.push(' ');
                }
                write_val(out, x);
            }
            out.push(')');
        }
        Val::Map(kvs) => {
            out.push('{');
            for (i, (k, val)) in kvs.iter().enumerate() {
                if i > 0 {
                    out.push(' ');
                }
                write_val(out, k);
                out.push(' ');
                write_val(out, val);
            }
            out.push('}');
        }
        Val::Adt(tag, fields) => {
            let _ = write!(out, "<adt {tag}");
            for f in fields.iter() {
                out.push(' ');
                write_val(out, f);
            }
            out.push('>');
        }
        Val::Closure(f, _) => {
            let _ = write!(out, "<fn {}>", f.0);
        }
        Val::Cont(_) => out.push_str("<continuation>"),
    }
}
