use super::value::{ChannelId, OrdMap, Value};
use super::{call_fn, err, get_global_env, Env, InterpError};
use std::cell::{Cell, RefCell};
use std::collections::{HashMap, VecDeque};
use std::sync::{Arc, Mutex, OnceLock};

type IResult = Result<Value, InterpError>;

/// Bit flag to distinguish shared (cross-thread) channels from thread-local ones.
const SHARED_CHANNEL_BIT: u32 = 0x8000_0000;

thread_local! {
    static CHANNELS: RefCell<HashMap<ChannelId, VecDeque<Value>>> = RefCell::new(HashMap::new());
    static NEXT_CHAN: Cell<ChannelId> = const { Cell::new(0) };
    static PRINT_BUF: RefCell<Option<Vec<String>>> = const { RefCell::new(None) };
}

// --- Shared (cross-thread) channels ---

type SharedChannelMap = Mutex<HashMap<ChannelId, Arc<Mutex<VecDeque<Value>>>>>;

fn shared_channels() -> &'static SharedChannelMap {
    static INSTANCE: OnceLock<SharedChannelMap> = OnceLock::new();
    INSTANCE.get_or_init(|| Mutex::new(HashMap::new()))
}

fn shared_next_id() -> &'static std::sync::atomic::AtomicU32 {
    static INSTANCE: OnceLock<std::sync::atomic::AtomicU32> = OnceLock::new();
    INSTANCE.get_or_init(|| std::sync::atomic::AtomicU32::new(0))
}

/// Create a shared channel and return (tx_id, rx_id) with the high bit set.
pub fn create_shared_channel() -> (ChannelId, ChannelId) {
    let id = shared_next_id().fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    let id = id | SHARED_CHANNEL_BIT;
    shared_channels()
        .lock()
        .unwrap()
        .insert(id, Arc::new(Mutex::new(VecDeque::new())));
    (id, id)
}

/// Send a value on a shared channel.
pub fn shared_send(id: ChannelId, val: Value) -> IResult {
    let map = shared_channels().lock().unwrap();
    if let Some(buf) = map.get(&id) {
        buf.lock().unwrap().push_back(val);
        Ok(Value::Unit)
    } else {
        Err(err(format!("shared channel {id} does not exist")))
    }
}

/// Blocking recv on a shared channel — spins with 1ms sleep.
pub fn shared_recv(id: ChannelId) -> IResult {
    loop {
        {
            let map = shared_channels().lock().unwrap();
            if let Some(buf) = map.get(&id) {
                if let Some(val) = buf.lock().unwrap().pop_front() {
                    return Ok(val);
                }
            } else {
                return Err(err(format!("shared channel {id} does not exist")));
            }
        }
        std::thread::sleep(std::time::Duration::from_millis(1));
    }
}

/// Non-blocking recv on a shared channel.
pub fn shared_try_recv(id: ChannelId) -> IResult {
    let map = shared_channels().lock().unwrap();
    if let Some(buf) = map.get(&id) {
        if let Some(val) = buf.lock().unwrap().pop_front() {
            Ok(Value::Adt("Some".to_string(), vec![val]))
        } else {
            Ok(Value::Adt("None".to_string(), vec![]))
        }
    } else {
        Err(err(format!("shared channel {id} does not exist")))
    }
}

fn is_shared(id: ChannelId) -> bool {
    id & SHARED_CHANNEL_BIT != 0
}

/// Run `f` while capturing all println/print output into a buffer.
/// Returns the result of `f` and the captured output joined by newlines.
pub fn capture_output<F: FnOnce() -> R, R>(f: F) -> (R, String) {
    PRINT_BUF.with(|buf| {
        *buf.borrow_mut() = Some(Vec::new());
    });
    let result = f();
    let output = PRINT_BUF.with(|buf| buf.borrow_mut().take().unwrap_or_default().join("\n"));
    (result, output)
}

/// How many arguments a builtin accepts: an exact count (`2`), a minimum
/// (`2..`), or an inclusive range (`1..=2`).
pub(crate) trait Arity {
    fn bounds(&self) -> (usize, Option<usize>);
}

impl Arity for usize {
    fn bounds(&self) -> (usize, Option<usize>) {
        (*self, Some(*self))
    }
}

impl Arity for std::ops::RangeFrom<usize> {
    fn bounds(&self) -> (usize, Option<usize>) {
        (self.start, None)
    }
}

impl Arity for std::ops::RangeInclusive<usize> {
    fn bounds(&self) -> (usize, Option<usize>) {
        (*self.start(), Some(*self.end()))
    }
}

fn plural(n: usize) -> &'static str {
    if n == 1 {
        "argument"
    } else {
        "arguments"
    }
}

/// `Err` when `name` got fewer arguments than its arity's minimum.
///
/// Only the minimum is enforced. Extra arguments are ignored, as they always
/// have been and as the EIR VM does: a pipe step is thread-last partial
/// application, so `[pipe 5 [- 10 3]]` calls `[- 10 3 5]` and means `7`
/// (pinned by `backend_parity::pipe-operator-partials`). The maximum only
/// shapes the error message.
pub(crate) fn check_arity(name: &str, arity: &impl Arity, got: usize) -> Result<(), InterpError> {
    let (min, max) = arity.bounds();
    if got >= min {
        return Ok(());
    }
    let wanted = match max {
        Some(max) if max == min => format!("{min} {}", plural(min)),
        Some(max) => format!("{min} to {max} arguments"),
        None => format!("at least {min} {}", plural(min)),
    };
    Err(err(format!("{name} requires {wanted}, got {got}")))
}

/// Wrap a builtin body so a call with too few arguments is an `InterpError`,
/// never an out-of-bounds panic on `args`. A panic here would
/// abort any host that evaluates documents in-process (e.g. vcad.app, which
/// recompiles the open loon document on every edit), so every builtin —
/// including the partial applications they return — goes through this.
pub(crate) fn guarded(
    name: &str,
    arity: impl Arity + Send + Sync + 'static,
    f: impl Fn(&str, &[Value]) -> IResult + Send + Sync + 'static,
) -> super::value::BuiltinFn {
    let name = name.to_string();
    Arc::new(move |called: &str, args: &[Value]| {
        check_arity(&name, &arity, args.len())?;
        f(called, args)
    })
}

/// A partially applied builtin taking exactly one more argument.
fn partial(name: &str, f: impl Fn(&str, &[Value]) -> IResult + Send + Sync + 'static) -> Value {
    Value::Builtin(name.to_string(), guarded(name, 1, f))
}

/// Most elements (or string bytes) one builtin call may build. Hosts evaluate
/// documents in-process, and an allocation failure aborts the whole process,
/// so oversized requests are errors instead.
const MAX_BUILT_LEN: usize = 1 << 24;

pub fn register_builtins(env: &mut Env) {
    // Every builtin declares its arity; `guarded` checks the minimum before
    // the body runs, so a body may index `args` up to that minimum (and must
    // use `args.get` beyond it).
    macro_rules! builtin {
        ($env:expr, $name:expr, $arity:expr, $f:expr) => {
            $env.set(
                $name.to_string(),
                Value::Builtin($name.to_string(), guarded($name, $arity, $f)),
            );
        };
    }

    builtin!(env, "signature", 1, |_, args: &[Value]| {
        if args.len() != 1 {
            return Err(err("signature requires exactly 1 argument (a function)"));
        }
        fn param_str(p: &super::value::Param) -> String {
            match p {
                super::value::Param::Simple(n) => n.clone(),
                super::value::Param::Rest(n) => format!("& {n}"),
                super::value::Param::VecDestructure(ps) => {
                    let inner: Vec<String> = ps.iter().map(param_str).collect();
                    format!("#[{}]", inner.join(" "))
                }
                super::value::Param::MapDestructure(entries) => {
                    let keys: Vec<&str> = entries.iter().map(|(k, _)| k.as_str()).collect();
                    format!("{{{}}}", keys.join(" "))
                }
            }
        }
        match &args[0] {
            Value::Fn(lf) => {
                let name = lf.name.as_deref().unwrap_or("fn");
                let clauses: Vec<String> = lf
                    .clauses
                    .iter()
                    .map(|(params, _)| {
                        let ps: Vec<String> = params.iter().map(param_str).collect();
                        if ps.is_empty() {
                            format!("[{name}]")
                        } else {
                            format!("[{name} {}]", ps.join(" "))
                        }
                    })
                    .collect();
                Ok(Value::Str(clauses.join(" | ").into()))
            }
            Value::Builtin(name, _) => Ok(Value::Str(format!("[{name} ...] (builtin)").into())),
            other => Err(err(format!("signature requires a function, got {other}"))),
        }
    });

    builtin!(env, "+", 2.., |_, args: &[Value]| {
        if args.len() < 2 {
            return Err(err("+ requires at least 2 arguments"));
        }
        let mut acc = args[0].clone();
        for arg in &args[1..] {
            acc = match (&acc, arg) {
                (Value::Int(a), Value::Int(b)) => Value::Int(a.wrapping_add(*b)),
                (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
                (Value::Float(a), Value::Int(b)) => Value::Float(a + *b as f64),
                (Value::Int(a), Value::Float(b)) => Value::Float(*a as f64 + b),
                _ => return Err(err(format!("+ requires numbers, got {} and {}", acc, arg))),
            };
        }
        Ok(acc)
    });

    builtin!(env, "-", 2, |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a.wrapping_sub(*b))),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a - *b as f64)),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float(*a as f64 - b)),
            _ => Err(err("- requires numbers")),
        }
    });

    builtin!(env, "*", 2.., |_, args: &[Value]| {
        if args.len() < 2 {
            return Err(err("* requires at least 2 arguments"));
        }
        let mut acc = args[0].clone();
        for arg in &args[1..] {
            acc = match (&acc, arg) {
                (Value::Int(a), Value::Int(b)) => Value::Int(a.wrapping_mul(*b)),
                (Value::Float(a), Value::Float(b)) => Value::Float(a * b),
                (Value::Float(a), Value::Int(b)) => Value::Float(a * *b as f64),
                (Value::Int(a), Value::Float(b)) => Value::Float(*a as f64 * b),
                _ => return Err(err(format!("* requires numbers, got {} and {}", acc, arg))),
            };
        }
        Ok(acc)
    });

    builtin!(env, "/", 2, |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Int(a), Value::Int(b)) => {
                if *b == 0 {
                    return Err(err("division by zero"));
                }
                Ok(Value::Int(a.wrapping_div(*b)))
            }
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a / b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a / *b as f64)),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float(*a as f64 / b)),
            _ => Err(err("/ requires numbers")),
        }
    });

    builtin!(env, "%", 2, |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Int(a), Value::Int(b)) => {
                if *b == 0 {
                    return Err(err("modulo by zero"));
                }
                Ok(Value::Int(a.wrapping_rem(*b)))
            }
            _ => Err(err("% requires integers")),
        }
    });

    builtin!(env, ">", 2, |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a > b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a > b)),
            _ => Err(err(format!(
                "> requires numbers, got {} and {}",
                args[0], args[1]
            ))),
        }
    });

    builtin!(env, "<", 2, |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a < b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a < b)),
            _ => Err(err(format!(
                "< requires numbers, got {} and {}",
                args[0], args[1]
            ))),
        }
    });

    builtin!(env, "=", 2, |_, args: &[Value]| {
        Ok(Value::Bool(args[0] == args[1]))
    });

    // != mirrors the EIR VM's BinOp::Ne (the VM had it; the interp didn't).
    builtin!(env, "!=", 2, |_, args: &[Value]| {
        Ok(Value::Bool(args[0] != args[1]))
    });

    builtin!(env, ">=", 2, |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a >= b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a >= b)),
            _ => Err(err(format!(
                ">= requires numbers, got {} and {}",
                args[0], args[1]
            ))),
        }
    });

    builtin!(env, "<=", 2, |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a <= b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a <= b)),
            _ => Err(err(format!(
                "<= requires numbers, got {} and {}",
                args[0], args[1]
            ))),
        }
    });

    builtin!(env, "not", 1, |_, args: &[Value]| {
        Ok(Value::Bool(!args[0].is_truthy()))
    });

    builtin!(env, "or", 0.., |_, args: &[Value]| {
        for arg in args {
            if arg.is_truthy() {
                return Ok(arg.clone());
            }
        }
        Ok(args.last().cloned().unwrap_or(Value::Bool(false)))
    });

    builtin!(env, "and", 0.., |_, args: &[Value]| {
        for arg in args {
            if !arg.is_truthy() {
                return Ok(arg.clone());
            }
        }
        Ok(args.last().cloned().unwrap_or(Value::Bool(true)))
    });

    // --- String builtins ---

    builtin!(env, "str", 0.., |_, args: &[Value]| {
        let s: String = args.iter().map(|v| v.display_str()).collect();
        Ok(Value::Str(s.into()))
    });

    builtin!(env, "println", 0.., |_, args: &[Value]| {
        if let Some(msg) = super::compile_sandbox_denial("IO", "println") {
            return Err(err(msg));
        }
        let parts: Vec<String> = args.iter().map(|v| v.display_str()).collect();
        let line = parts.join(" ");
        let captured = PRINT_BUF.with(|buf| {
            let mut guard = buf.borrow_mut();
            if let Some(ref mut vec) = *guard {
                vec.push(line.clone());
                true
            } else {
                false
            }
        });
        if !captured {
            println!("{}", line);
        }
        Ok(Value::Unit)
    });

    builtin!(env, "print", 0.., |_, args: &[Value]| {
        if let Some(msg) = super::compile_sandbox_denial("IO", "println") {
            return Err(err(msg));
        }
        let parts: Vec<String> = args.iter().map(|v| v.display_str()).collect();
        let text = parts.join(" ");
        let captured = PRINT_BUF.with(|buf| {
            let mut guard = buf.borrow_mut();
            if let Some(ref mut vec) = *guard {
                // Append to last line or create new entry
                if let Some(last) = vec.last_mut() {
                    last.push_str(&text);
                } else {
                    vec.push(text.clone());
                }
                true
            } else {
                false
            }
        });
        if !captured {
            print!("{}", text);
        }
        Ok(Value::Unit)
    });

    builtin!(env, "split", 2, |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Str(s), Value::Str(delims)) => {
                let words: imbl::Vector<Value> = s
                    .split(|c: char| delims.contains(c))
                    .map(|w| Value::Str(w.into()))
                    .collect();
                Ok(Value::Vec(words))
            }
            _ => Err(err("split requires a string and delimiters")),
        }
    });

    builtin!(env, "join", 2, |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Str(sep), Value::Vec(v)) | (Value::Vec(v), Value::Str(sep)) => {
                let parts: Vec<String> = v.iter().map(|x| x.display_str()).collect();
                Ok(Value::Str(parts.join(sep).into()))
            }
            _ => Err(err("join requires a separator and vector")),
        }
    });

    builtin!(env, "trim", 1, |_, args: &[Value]| {
        match &args[0] {
            Value::Str(s) => Ok(Value::Str(s.trim().into())),
            _ => Err(err("trim requires a string")),
        }
    });

    builtin!(env, "starts-with?", 2, |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Str(s), Value::Str(prefix)) => Ok(Value::Bool(s.starts_with(&**prefix))),
            _ => Err(err("starts-with? requires two strings")),
        }
    });

    builtin!(env, "ends-with?", 2, |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Str(s), Value::Str(suffix)) => Ok(Value::Bool(s.ends_with(&**suffix))),
            _ => Err(err("ends-with? requires two strings")),
        }
    });

    builtin!(env, "replace", 3, |_, args: &[Value]| {
        match (&args[0], &args[1], &args[2]) {
            (Value::Str(s), Value::Str(from), Value::Str(to)) => {
                Ok(Value::Str(s.replace(&**from, &**to).into()))
            }
            _ => Err(err("replace requires three strings")),
        }
    });

    builtin!(env, "uppercase", 1, |_, args: &[Value]| {
        match &args[0] {
            Value::Str(s) => Ok(Value::Str(s.to_uppercase().into())),
            _ => Err(err("uppercase requires a string")),
        }
    });

    builtin!(env, "lowercase", 1, |_, args: &[Value]| {
        match &args[0] {
            Value::Str(s) => Ok(Value::Str(s.to_lowercase().into())),
            _ => Err(err("lowercase requires a string")),
        }
    });

    // --- Collection builtins ---

    builtin!(env, "len", 1, |_, args: &[Value]| {
        match &args[0] {
            Value::Vec(v) => Ok(Value::Int(v.len() as i64)),
            Value::Str(s) => Ok(Value::Int(if s.is_ascii() {
                s.len()
            } else {
                s.chars().count()
            } as i64)),
            Value::Map(m) => Ok(Value::Int(m.len() as i64)),
            Value::Set(s) => Ok(Value::Int(s.len() as i64)),
            Value::Json(j) => match j.as_ref() {
                serde_json::Value::Array(a) => Ok(Value::Int(a.len() as i64)),
                serde_json::Value::Object(o) => Ok(Value::Int(o.len() as i64)),
                serde_json::Value::String(s) => Ok(Value::Int(s.len() as i64)),
                _ => Ok(Value::Int(0)),
            },
            _ => Err(err("len requires a collection")),
        }
    });

    builtin!(env, "nth", 2..=3, |_, args: &[Value]| {
        if let Value::Int(i) = &args[1] {
            let idx = *i as usize;
            match &args[0] {
                Value::Vec(v) => {
                    if idx < v.len() {
                        Ok(v[idx].clone())
                    } else if args.len() > 2 {
                        Ok(args[2].clone())
                    } else {
                        Err(err(format!("index {i} out of bounds (len {})", v.len())))
                    }
                }
                Value::Tuple(v) => {
                    if idx < v.len() {
                        Ok(v[idx].clone())
                    } else if args.len() > 2 {
                        Ok(args[2].clone())
                    } else {
                        Err(err(format!("index {i} out of bounds (len {})", v.len())))
                    }
                }
                _ => Err(err(format!(
                    "nth requires a vector/tuple and index, got {} and {}",
                    args[0], args[1]
                ))),
            }
        } else {
            Err(err(format!(
                "nth requires a vector/tuple and index, got {} and {}",
                args[0], args[1]
            )))
        }
    });

    builtin!(env, "map", 1..=2, |_, args: &[Value]| {
        fn map_vec(func: &Value, v: &imbl::Vector<Value>) -> IResult {
            let mut result = imbl::Vector::new();
            for item in v {
                result.push_back(apply_value(func, std::slice::from_ref(item))?);
            }
            Ok(Value::Vec(result))
        }
        fn map_json(func: &Value, arr: &[serde_json::Value]) -> IResult {
            let mut result = imbl::Vector::new();
            for item in arr {
                let v = Value::from_json(item);
                result.push_back(apply_value(func, std::slice::from_ref(&v))?);
            }
            Ok(Value::Vec(result))
        }
        match (&args[0], args.get(1)) {
            (Value::Vec(v), Some(func)) => map_vec(func, v),
            (func, Some(Value::Vec(v))) if func.is_callable() => map_vec(func, v),
            (func, Some(Value::Json(j))) if func.is_callable() => {
                if let serde_json::Value::Array(arr) = j.as_ref() {
                    map_json(func, arr)
                } else {
                    Err(err("map requires a vector or json array"))
                }
            }
            (func, None) if func.is_callable() => {
                let func_clone = func.clone();
                Ok(partial(
                    "map-partial",
                    move |_, inner_args: &[Value]| match &inner_args[0] {
                        Value::Vec(v) => map_vec(&func_clone, v),
                        Value::Json(j) => {
                            if let serde_json::Value::Array(arr) = j.as_ref() {
                                map_json(&func_clone, arr)
                            } else {
                                Err(err("map requires a vector"))
                            }
                        }
                        _ => Err(err("map requires a vector")),
                    },
                ))
            }
            _ => Err(err("map requires a function and vector")),
        }
    });

    builtin!(env, "filter", 1..=2, |_, args: &[Value]| {
        fn filter_vec(func: &Value, v: &imbl::Vector<Value>) -> IResult {
            let mut result = imbl::Vector::new();
            for item in v {
                if apply_value(func, std::slice::from_ref(item))?.is_truthy() {
                    result.push_back(item.clone());
                }
            }
            Ok(Value::Vec(result))
        }
        fn filter_json(func: &Value, arr: &[serde_json::Value]) -> IResult {
            let mut result = imbl::Vector::new();
            for item in arr {
                let v = Value::from_json(item);
                if apply_value(func, std::slice::from_ref(&v))?.is_truthy() {
                    result.push_back(v);
                }
            }
            Ok(Value::Vec(result))
        }
        match (&args[0], args.get(1)) {
            (Value::Vec(v), Some(func)) => filter_vec(func, v),
            (func, Some(Value::Vec(v))) if func.is_callable() => filter_vec(func, v),
            (func, Some(Value::Json(j))) if func.is_callable() => {
                if let serde_json::Value::Array(arr) = j.as_ref() {
                    filter_json(func, arr)
                } else {
                    Err(err("filter requires a vector or json array"))
                }
            }
            (func, None) if func.is_callable() => {
                let func_clone = func.clone();
                Ok(partial(
                    "filter-partial",
                    move |_, inner_args: &[Value]| match &inner_args[0] {
                        Value::Vec(v) => filter_vec(&func_clone, v),
                        Value::Json(j) => {
                            if let serde_json::Value::Array(arr) = j.as_ref() {
                                filter_json(&func_clone, arr)
                            } else {
                                Err(err("filter requires a vector"))
                            }
                        }
                        _ => Err(err("filter requires a vector")),
                    },
                ))
            }
            _ => Err(err("filter requires a function and vector")),
        }
    });

    builtin!(env, "fold", 2..=3, |_, args: &[Value]| {
        fn do_fold(v: &imbl::Vector<Value>, init: &Value, func: &Value) -> IResult {
            let mut acc = init.clone();
            for item in v {
                acc = apply_value(func, &[acc, item.clone()])?;
            }
            Ok(acc)
        }

        match args {
            [Value::Vec(v), init, func] => do_fold(v, init, func),
            [init, func, Value::Vec(v)] if func.is_callable() => do_fold(v, init, func),
            [init, func] => {
                let init_clone = init.clone();
                let func_clone = func.clone();
                Ok(partial("fold-partial", move |_, inner_args: &[Value]| {
                    if let Value::Vec(v) = &inner_args[0] {
                        do_fold(v, &init_clone, &func_clone)
                    } else {
                        Err(err("fold requires a vector"))
                    }
                }))
            }
            _ => Err(err("fold requires init, function, and vector")),
        }
    });

    builtin!(env, "conj", 1.., |_, args: &[Value]| {
        match &args[0] {
            Value::Vec(v) => {
                let mut new = v.clone();
                for a in &args[1..] {
                    new.push_back(a.clone());
                }
                Ok(Value::Vec(new))
            }
            Value::Set(s) => {
                let mut new = s.clone();
                for a in &args[1..] {
                    new = new.update(a.clone());
                }
                Ok(Value::Set(new))
            }
            _ => Err(err("conj requires a collection")),
        }
    });

    builtin!(env, "get", 2..=3, |_, args: &[Value]| {
        let default = || {
            if args.len() > 2 {
                args[2].clone()
            } else {
                Value::Unit
            }
        };
        match (&args[0], &args[1]) {
            (Value::Map(m), key) => {
                // Try exact match first
                if let Some(v) = m.get(key) {
                    return Ok(v.clone());
                }
                // Fuzzy match: keyword ↔ string interop
                let alt_key = match key {
                    Value::Keyword(k) => Some(Value::Str(k.clone())),
                    Value::Str(s) => Some(Value::Keyword(s.clone())),
                    _ => None,
                };
                if let Some(ref alt) = alt_key {
                    if let Some(v) = m.get(alt) {
                        return Ok(v.clone());
                    }
                }
                Ok(default())
            }
            (Value::Vec(v), Value::Int(i)) => {
                let idx = *i as usize;
                Ok(v.get(idx).cloned().unwrap_or_else(default))
            }
            (Value::Json(j), key) => {
                let result = match (j.as_ref(), key) {
                    (serde_json::Value::Object(obj), Value::Str(k)) => {
                        obj.get(&**k).map(Value::from_json)
                    }
                    (serde_json::Value::Object(obj), Value::Keyword(k)) => {
                        obj.get(&**k).map(Value::from_json)
                    }
                    (serde_json::Value::Array(arr), Value::Int(i)) => {
                        arr.get(*i as usize).map(Value::from_json)
                    }
                    _ => None,
                };
                Ok(result.unwrap_or_else(default))
            }
            _ => Err(err("get requires a map/vector/json and key")),
        }
    });

    builtin!(env, "assoc", 3, |_, args: &[Value]| {
        if let Value::Map(m) = &args[0] {
            let key = &args[1];
            let val = &args[2];
            Ok(Value::Map(m.update(key.clone(), val.clone())))
        } else {
            Err(err("assoc requires a map"))
        }
    });

    builtin!(env, "update", 3, |_, args: &[Value]| {
        if let Value::Map(m) = &args[0] {
            let key = &args[1];
            let func = &args[2];
            let current = m.get(key).cloned().unwrap_or(Value::Unit);
            let updated = apply_value(func, &[current])?;
            Ok(Value::Map(m.update(key.clone(), updated)))
        } else {
            Err(err("update requires a map"))
        }
    });

    builtin!(env, "range", 2, |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Int(start), Value::Int(end)) => {
                // A mid-edit `[range 0 10000000000]` must not exhaust memory
                // (an allocation failure aborts the host process).
                let len = (*end as i128 - *start as i128).max(0);
                if len > MAX_BUILT_LEN as i128 {
                    return Err(err(format!(
                        "range {start}..{end} is too large ({len} elements, limit {MAX_BUILT_LEN})"
                    )));
                }
                let v: imbl::Vector<Value> = (*start..*end).map(Value::Int).collect();
                Ok(Value::Vec(v))
            }
            _ => Err(err("range requires two integers")),
        }
    });

    builtin!(env, "contains?", 2, |_, args: &[Value]| {
        match &args[0] {
            Value::Set(s) => Ok(Value::Bool(s.contains(&args[1]))),
            Value::Map(m) => Ok(Value::Bool(m.contains_key(&args[1]))),
            Value::Vec(v) => Ok(Value::Bool(v.iter().any(|i| i == &args[1]))),
            Value::Str(s) => match &args[1] {
                Value::Str(needle) => Ok(Value::Bool(s.contains(&**needle))),
                _ => Err(err("contains? on string requires a string needle")),
            },
            Value::Json(j) => match (j.as_ref(), &args[1]) {
                (serde_json::Value::Array(a), Value::Str(s)) => {
                    Ok(Value::Bool(a.iter().any(|v| v.as_str() == Some(&**s))))
                }
                (serde_json::Value::Array(a), Value::Int(n)) => {
                    Ok(Value::Bool(a.iter().any(|v| v.as_i64() == Some(*n))))
                }
                (serde_json::Value::Object(o), Value::Str(s)) => {
                    Ok(Value::Bool(o.contains_key(&**s)))
                }
                _ => Err(err("contains? on JSON requires array/object")),
            },
            _ => Err(err("contains? requires a collection or string")),
        }
    });

    builtin!(env, "empty?", 1, |_, args: &[Value]| {
        match &args[0] {
            Value::Vec(v) => Ok(Value::Bool(v.is_empty())),
            Value::Str(s) => Ok(Value::Bool(s.is_empty())),
            Value::Map(m) => Ok(Value::Bool(m.is_empty())),
            Value::Set(s) => Ok(Value::Bool(s.is_empty())),
            Value::Json(j) => match j.as_ref() {
                serde_json::Value::Array(a) => Ok(Value::Bool(a.is_empty())),
                serde_json::Value::Object(o) => Ok(Value::Bool(o.is_empty())),
                serde_json::Value::Null => Ok(Value::Bool(true)),
                _ => Ok(Value::Bool(false)),
            },
            _ => Err(err("empty? requires a collection")),
        }
    });

    builtin!(env, "sort-by", 2..=3, |_, args: &[Value]| {
        fn do_sort(func: &Value, desc: bool, v: &imbl::Vector<Value>) -> IResult {
            // Compute each key once, up front: a key fn that errors now
            // surfaces its error, and an impure one can't make the comparator
            // inconsistent (std's sort may panic on a non-total order).
            let mut keyed = Vec::with_capacity(v.len());
            for item in v {
                keyed.push((apply_value(func, std::slice::from_ref(item))?, item.clone()));
            }
            keyed.sort_by(|(ka, _), (kb, _)| {
                let ord = value_cmp(ka, kb);
                if desc {
                    ord.reverse()
                } else {
                    ord
                }
            });
            Ok(Value::Vec(
                keyed.into_iter().map(|(_, item)| item).collect(),
            ))
        }

        match args {
            [func, order, Value::Vec(v)] if func.is_callable() => {
                let desc = matches!(order, Value::Keyword(k) if &**k == "desc");
                do_sort(func, desc, v)
            }
            [func, Value::Vec(v)] if func.is_callable() => do_sort(func, false, v),
            [func, order] => {
                let func_clone = func.clone();
                let desc = matches!(order, Value::Keyword(k) if &**k == "desc");
                Ok(partial(
                    "sort-by-partial",
                    move |_, inner_args: &[Value]| {
                        if let Value::Vec(v) = &inner_args[0] {
                            do_sort(&func_clone, desc, v)
                        } else {
                            Err(err("sort-by requires a vector"))
                        }
                    },
                ))
            }
            _ => Err(err("sort-by requires a function and optional order")),
        }
    });

    builtin!(env, "take", 1..=2, |_, args: &[Value]| {
        match (&args[0], args.get(1)) {
            (Value::Int(n), Some(Value::Vec(v))) => {
                Ok(Value::Vec(v.iter().take(*n as usize).cloned().collect()))
            }
            (Value::Int(n), None) => {
                let n = *n;
                Ok(partial("take-partial", move |_, inner_args: &[Value]| {
                    if let Value::Vec(v) = &inner_args[0] {
                        Ok(Value::Vec(v.iter().take(n as usize).cloned().collect()))
                    } else {
                        Err(err("take requires a vector"))
                    }
                }))
            }
            _ => Err(err("take requires a count and vector")),
        }
    });

    builtin!(env, "drop", 1..=2, |_, args: &[Value]| {
        match (&args[0], args.get(1)) {
            (Value::Int(n), Some(Value::Vec(v))) => {
                Ok(Value::Vec(v.iter().skip(*n as usize).cloned().collect()))
            }
            (Value::Int(n), None) => {
                let n = *n;
                Ok(partial("drop-partial", move |_, inner_args: &[Value]| {
                    if let Value::Vec(v) = &inner_args[0] {
                        Ok(Value::Vec(v.iter().skip(n as usize).cloned().collect()))
                    } else {
                        Err(err("drop requires a vector"))
                    }
                }))
            }
            _ => Err(err("drop requires a count and vector")),
        }
    });

    builtin!(env, "each", 1..=2, |_, args: &[Value]| {
        fn each_vec(func: &Value, v: &imbl::Vector<Value>) -> IResult {
            for item in v {
                apply_value(func, std::slice::from_ref(item))?;
            }
            Ok(Value::Unit)
        }
        fn each_json(func: &Value, arr: &[serde_json::Value]) -> IResult {
            for item in arr {
                let v = Value::from_json(item);
                apply_value(func, std::slice::from_ref(&v))?;
            }
            Ok(Value::Unit)
        }
        match (&args[0], args.get(1)) {
            (Value::Vec(v), Some(func)) => each_vec(func, v),
            (func, Some(Value::Vec(v))) if func.is_callable() => each_vec(func, v),
            (func, Some(Value::Json(j))) if func.is_callable() => {
                if let serde_json::Value::Array(arr) = j.as_ref() {
                    each_json(func, arr)
                } else {
                    Err(err("each requires a vector or json array"))
                }
            }
            (func, None) if func.is_callable() => {
                let func_clone = func.clone();
                Ok(partial(
                    "each-partial",
                    move |_, inner_args: &[Value]| match &inner_args[0] {
                        Value::Vec(v) => each_vec(&func_clone, v),
                        Value::Json(j) => {
                            if let serde_json::Value::Array(arr) = j.as_ref() {
                                each_json(&func_clone, arr)
                            } else {
                                Err(err("each requires a vector"))
                            }
                        }
                        _ => Err(err("each requires a vector")),
                    },
                ))
            }
            _ => Err(err("each requires a function and vector")),
        }
    });

    builtin!(env, "entries", 1, |_, args: &[Value]| {
        match &args[0] {
            Value::Map(pairs) => {
                let v: imbl::Vector<Value> = pairs
                    .iter()
                    .map(|(k, v)| Value::Tuple(vec![k.clone(), v.clone()]))
                    .collect();
                Ok(Value::Vec(v))
            }
            _ => Err(err("entries requires a map")),
        }
    });

    builtin!(env, "collect", 1, |_, args: &[Value]| {
        Ok(args[0].clone())
    });

    builtin!(env, "push!", 1.., |_, args: &[Value]| {
        if let Value::Vec(v) = &args[0] {
            let mut new = v.clone();
            for a in &args[1..] {
                new.push_back(a.clone());
            }
            Ok(Value::Vec(new))
        } else if let Value::Str(s) = &args[0] {
            let mut new = s.to_string();
            for a in &args[1..] {
                new.push_str(&a.display_str());
            }
            Ok(Value::Str(new.into()))
        } else {
            Err(err("push! requires a mutable collection"))
        }
    });

    builtin!(env, "assert-eq", 2, |_, args: &[Value]| {
        if args[0] == args[1] {
            Ok(Value::Unit)
        } else {
            Err(err(format!(
                "assert-eq failed:\n  expected: {}\n  actual:   {}",
                args[1], args[0]
            )))
        }
    });

    builtin!(env, "HashMap.new", 0, |_, _args: &[Value]| {
        Ok(Value::Map(OrdMap::new()))
    });

    // --- New v0.2 builtins ---

    builtin!(env, "zip", 2, |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Vec(a), Value::Vec(b)) => {
                let pairs: imbl::Vector<Value> = a
                    .iter()
                    .zip(b.iter())
                    .map(|(x, y)| Value::Tuple(vec![x.clone(), y.clone()]))
                    .collect();
                Ok(Value::Vec(pairs))
            }
            _ => Err(err("zip requires two vectors")),
        }
    });

    builtin!(env, "flatten", 1, |_, args: &[Value]| {
        match &args[0] {
            Value::Vec(v) => {
                let mut result = imbl::Vector::new();
                for item in v {
                    if let Value::Vec(inner) = item {
                        result.append(inner.clone());
                    } else {
                        result.push_back(item.clone());
                    }
                }
                Ok(Value::Vec(result))
            }
            _ => Err(err("flatten requires a vector")),
        }
    });

    builtin!(env, "chunk", 2, |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Int(n), Value::Vec(_)) if *n <= 0 => {
                // A zero-size chunk never shrinks `remaining`: an endless loop.
                Err(err(format!("chunk size must be positive, got {n}")))
            }
            (Value::Int(n), Value::Vec(v)) => {
                let n = *n as usize;
                let mut chunks = imbl::Vector::new();
                let mut remaining = v.clone();
                while !remaining.is_empty() {
                    let end = n.min(remaining.len());
                    let chunk = remaining.slice(..end);
                    chunks.push_back(Value::Vec(chunk));
                }
                Ok(Value::Vec(chunks))
            }
            _ => Err(err("chunk requires a size and vector")),
        }
    });

    builtin!(env, "reverse", 1, |_, args: &[Value]| {
        match &args[0] {
            Value::Vec(v) => {
                let rev: imbl::Vector<Value> = v.iter().rev().cloned().collect();
                Ok(Value::Vec(rev))
            }
            Value::Str(s) => Ok(Value::Str(s.chars().rev().collect::<String>().into())),
            _ => Err(err("reverse requires a vector or string")),
        }
    });

    builtin!(env, "find", 2, |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (func, Value::Vec(v)) if func.is_callable() => {
                for item in v {
                    let val = apply_value(func, std::slice::from_ref(item))?;
                    if val.is_truthy() {
                        return Ok(Value::Adt("Some".to_string(), vec![item.clone()]));
                    }
                }
                Ok(Value::Adt("None".to_string(), vec![]))
            }
            _ => Err(err("find requires a function and vector")),
        }
    });

    builtin!(env, "any?", 2, |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (func, Value::Vec(v)) if func.is_callable() => {
                for item in v {
                    let val = apply_value(func, std::slice::from_ref(item))?;
                    if val.is_truthy() {
                        return Ok(Value::Bool(true));
                    }
                }
                Ok(Value::Bool(false))
            }
            _ => Err(err("any? requires a function and vector")),
        }
    });

    builtin!(env, "all?", 2, |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (func, Value::Vec(v)) if func.is_callable() => {
                for item in v {
                    let val = apply_value(func, std::slice::from_ref(item))?;
                    if !val.is_truthy() {
                        return Ok(Value::Bool(false));
                    }
                }
                Ok(Value::Bool(true))
            }
            _ => Err(err("all? requires a function and vector")),
        }
    });

    // --- Type predicates ---

    builtin!(env, "name", 1, |_, args: &[Value]| {
        match &args[0] {
            Value::Keyword(k) => Ok(Value::Str(k.clone())),
            Value::Str(s) => Ok(Value::Str(s.clone())),
            _ => Err(err("name requires a keyword or string")),
        }
    });

    builtin!(env, "keyword", 1, |_, args: &[Value]| {
        match &args[0] {
            Value::Str(s) => Ok(Value::Keyword(s.clone())),
            Value::Keyword(k) => Ok(Value::Keyword(k.clone())),
            _ => Err(err("keyword requires a string or keyword")),
        }
    });

    builtin!(env, "keywordize-keys", 1, |_, args: &[Value]| {
        if let Value::Map(pairs) = &args[0] {
            Ok(Value::Map(
                pairs
                    .iter()
                    .map(|(k, v)| {
                        let new_k = match k {
                            Value::Str(s) => Value::Keyword(s.clone()),
                            other => other.clone(),
                        };
                        (new_k, v.clone())
                    })
                    .collect(),
            ))
        } else {
            Err(err("keywordize-keys requires a map"))
        }
    });

    builtin!(env, "map?", 1, |_, args: &[Value]| {
        Ok(Value::Bool(matches!(&args[0], Value::Map(_))))
    });

    builtin!(env, "vec?", 1, |_, args: &[Value]| {
        Ok(Value::Bool(matches!(&args[0], Value::Vec(_))))
    });

    // --- Cons (prepend to vec) ---

    builtin!(env, "cons", 2, |_, args: &[Value]| {
        match &args[1] {
            Value::Vec(v) => {
                let mut new = v.clone();
                new.push_front(args[0].clone());
                Ok(Value::Vec(new))
            }
            _ => Err(err("cons: second arg must be a vec")),
        }
    });

    // --- Map builtins ---

    builtin!(env, "keys", 1, |_, args: &[Value]| {
        match &args[0] {
            Value::Map(pairs) => {
                let ks: imbl::Vector<Value> = pairs.iter().map(|(k, _)| k.clone()).collect();
                Ok(Value::Vec(ks))
            }
            _ => Err(err("keys requires a map")),
        }
    });

    builtin!(env, "values", 1, |_, args: &[Value]| {
        match &args[0] {
            Value::Map(pairs) => {
                let vs: imbl::Vector<Value> = pairs.iter().map(|(_, v)| v.clone()).collect();
                Ok(Value::Vec(vs))
            }
            _ => Err(err("values requires a map")),
        }
    });

    builtin!(env, "merge", 2, |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            // Left-biased union, matching the EIR VM (the semantic reference):
            // keys already in `a` keep their position AND value; keys only in
            // `b` append in `b`'s insertion order. So `[merge {:a 1 :b 2}
            // {:b 9 :c 3}]` is `{:a 1 :b 2 :c 3}` — `:b` stays 2, `:c` appends.
            (Value::Map(a), Value::Map(b)) => Ok(Value::Map(a.union(b.clone()))),
            _ => Err(err("merge requires two maps")),
        }
    });

    builtin!(env, "remove", 2, |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Map(m), key) => Ok(Value::Map(m.without(key))),
            _ => Err(err("remove requires a map and key")),
        }
    });

    // --- Number parsing ---

    builtin!(env, "int", 1, |_, args: &[Value]| {
        match &args[0] {
            Value::Str(s) => s
                .trim()
                .parse::<i64>()
                .map(Value::Int)
                .map_err(|e| err(format!("int: cannot parse '{}': {}", s, e))),
            Value::Int(n) => Ok(Value::Int(*n)),
            Value::Float(f) => Ok(Value::Int(*f as i64)),
            _ => Err(err(format!("int: cannot convert {}", args[0]))),
        }
    });

    builtin!(env, "float", 1, |_, args: &[Value]| {
        match &args[0] {
            Value::Str(s) => s
                .trim()
                .parse::<f64>()
                .map(Value::Float)
                .map_err(|e| err(format!("float: cannot parse '{}': {}", s, e))),
            Value::Float(f) => Ok(Value::Float(*f)),
            Value::Int(n) => Ok(Value::Float(*n as f64)),
            _ => Err(err(format!("float: cannot convert {}", args[0]))),
        }
    });

    // --- String ops ---

    builtin!(env, "char-at", 2, |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Str(s), Value::Int(i)) => {
                let idx = *i as usize;
                // Fast path: ASCII strings have byte index == char index
                if s.is_ascii() {
                    let bytes = s.as_bytes();
                    if idx < bytes.len() {
                        Ok(Value::Str(String::from(bytes[idx] as char).into()))
                    } else {
                        Err(err(format!(
                            "char-at: index {} out of bounds (len {})",
                            i,
                            bytes.len()
                        )))
                    }
                } else {
                    s.chars()
                        .nth(idx)
                        .map(|c| Value::Str(c.to_string().into()))
                        .ok_or_else(|| {
                            err(format!(
                                "char-at: index {} out of bounds (len {})",
                                i,
                                s.chars().count()
                            ))
                        })
                }
            }
            _ => Err(err("char-at requires a string and index")),
        }
    });

    builtin!(env, "substring", 3, |_, args: &[Value]| {
        match (&args[0], &args[1], &args[2]) {
            (Value::Str(s), Value::Int(start), Value::Int(end)) => {
                let start = *start as usize;
                let end = *end as usize;
                if s.is_ascii() {
                    if start > s.len() || end > s.len() || start > end {
                        return Err(err(format!(
                            "substring: invalid range {}..{} for len {}",
                            start,
                            end,
                            s.len()
                        )));
                    }
                    Ok(Value::Str(s[start..end].into()))
                } else {
                    let chars: Vec<char> = s.chars().collect();
                    if start > chars.len() || end > chars.len() || start > end {
                        return Err(err(format!(
                            "substring: invalid range {}..{} for len {}",
                            start,
                            end,
                            chars.len()
                        )));
                    }
                    Ok(Value::Str(
                        chars[start..end].iter().collect::<String>().into(),
                    ))
                }
            }
            _ => Err(err("substring requires a string, start, and end")),
        }
    });

    builtin!(env, "index-of", 2, |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Str(haystack), Value::Str(needle)) => match haystack.find(&**needle) {
                Some(pos) => Ok(Value::Int(pos as i64)),
                None => Ok(Value::Int(-1)),
            },
            // Vectors: index of the first equal element, -1 if absent (#25)
            (Value::Vec(v), needle) => Ok(Value::Int(
                v.iter()
                    .position(|x| x == needle)
                    .map(|p| p as i64)
                    .unwrap_or(-1),
            )),
            _ => Err(err("index-of requires a string or vector haystack")),
        }
    });

    // --- Collection ops ---

    builtin!(env, "group-by", 1..=2, |_, args: &[Value]| {
        fn do_group_by(func: &Value, v: &imbl::Vector<Value>) -> IResult {
            let mut groups: Vec<(Value, imbl::Vector<Value>)> = Vec::new();
            for item in v {
                let key = apply_value(func, std::slice::from_ref(item))?;
                if let Some(group) = groups.iter_mut().find(|(k, _)| k == &key) {
                    group.1.push_back(item.clone());
                } else {
                    groups.push((key, imbl::vector![item.clone()]));
                }
            }
            let map: OrdMap = groups
                .into_iter()
                .map(|(k, v)| (k, Value::Vec(v)))
                .collect();
            Ok(Value::Map(map))
        }
        match (&args[0], args.get(1)) {
            (func, Some(Value::Vec(v))) if func.is_callable() => do_group_by(func, v),
            (func, None) if func.is_callable() => {
                let func_clone = func.clone();
                Ok(partial(
                    "group-by-partial",
                    move |_, inner_args: &[Value]| {
                        if let Value::Vec(v) = &inner_args[0] {
                            do_group_by(&func_clone, v)
                        } else {
                            Err(err("group-by requires a vector"))
                        }
                    },
                ))
            }
            _ => Err(err("group-by requires a function and vector")),
        }
    });

    builtin!(env, "flat-map", 1..=2, |_, args: &[Value]| {
        fn do_flat_map(func: &Value, v: &imbl::Vector<Value>) -> IResult {
            let mut result = imbl::Vector::new();
            for item in v {
                let val = apply_value(func, std::slice::from_ref(item))?;
                if let Value::Vec(inner) = val {
                    result.append(inner);
                } else {
                    result.push_back(val);
                }
            }
            Ok(Value::Vec(result))
        }
        match (&args[0], args.get(1)) {
            (func, Some(Value::Vec(v))) if func.is_callable() => do_flat_map(func, v),
            (func, None) if func.is_callable() => {
                let func_clone = func.clone();
                Ok(partial(
                    "flat-map-partial",
                    move |_, inner_args: &[Value]| {
                        if let Value::Vec(v) = &inner_args[0] {
                            do_flat_map(&func_clone, v)
                        } else {
                            Err(err("flat-map requires a vector"))
                        }
                    },
                ))
            }
            _ => Err(err("flat-map requires a function and vector")),
        }
    });

    builtin!(env, "sort", 1, |_, args: &[Value]| {
        match &args[0] {
            Value::Vec(v) => {
                let mut sorted: Vec<Value> = v.iter().cloned().collect();
                sorted.sort_by(value_cmp);
                Ok(Value::Vec(sorted.into_iter().collect()))
            }
            _ => Err(err("sort requires a vector")),
        }
    });

    builtin!(env, "min", 1, |_, args: &[Value]| {
        match &args[0] {
            Value::Vec(v) if !v.is_empty() => {
                let mut result = &v[0];
                for item in v.iter().skip(1) {
                    if value_cmp(item, result) == std::cmp::Ordering::Less {
                        result = item;
                    }
                }
                Ok(result.clone())
            }
            Value::Vec(_) => Err(err("min: empty vector")),
            _ => Err(err("min requires a vector")),
        }
    });

    builtin!(env, "max", 1, |_, args: &[Value]| {
        match &args[0] {
            Value::Vec(v) if !v.is_empty() => {
                let mut result = &v[0];
                for item in v.iter().skip(1) {
                    if value_cmp(item, result) == std::cmp::Ordering::Greater {
                        result = item;
                    }
                }
                Ok(result.clone())
            }
            Value::Vec(_) => Err(err("max: empty vector")),
            _ => Err(err("max requires a vector")),
        }
    });

    builtin!(env, "sum", 1, |_, args: &[Value]| {
        match &args[0] {
            Value::Vec(v) => {
                if v.is_empty() {
                    return Ok(Value::Int(0));
                }
                let mut acc = v[0].clone();
                for item in v.iter().skip(1) {
                    acc = match (&acc, item) {
                        (Value::Int(a), Value::Int(b)) => Value::Int(a.wrapping_add(*b)),
                        (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
                        (Value::Float(a), Value::Int(b)) => Value::Float(a + *b as f64),
                        (Value::Int(a), Value::Float(b)) => Value::Float(*a as f64 + b),
                        _ => return Err(err("sum: non-numeric element")),
                    };
                }
                Ok(acc)
            }
            _ => Err(err("sum requires a vector")),
        }
    });

    builtin!(env, "sqrt", 1, |_, args: &[Value]| {
        match &args[0] {
            Value::Float(f) => Ok(Value::Float(f.sqrt())),
            Value::Int(n) => Ok(Value::Float((*n as f64).sqrt())),
            _ => Err(err("sqrt requires a number")),
        }
    });

    builtin!(env, "pow", 2, |_, args: &[Value]| {
        if args.len() < 2 {
            return Err(err("pow requires base and exponent"));
        }
        let base = match &args[0] {
            Value::Float(f) => *f,
            Value::Int(n) => *n as f64,
            _ => return Err(err("pow requires numeric base")),
        };
        let exp = match &args[1] {
            Value::Float(f) => *f,
            Value::Int(n) => *n as f64,
            _ => return Err(err("pow requires numeric exponent")),
        };
        Ok(Value::Float(base.powf(exp)))
    });

    // Core math (issue #19).
    macro_rules! math_to_float {
        ($name:expr, $f:expr) => {
            builtin!(env, $name, 1, |_, args: &[Value]| {
                let x = match &args[0] {
                    Value::Float(f) => *f,
                    Value::Int(n) => *n as f64,
                    _ => return Err(err(concat!($name, " requires a number"))),
                };
                let g: fn(f64) -> f64 = $f;
                Ok(Value::Float(g(x)))
            });
        };
    }
    math_to_float!("sin", f64::sin);
    math_to_float!("cos", f64::cos);
    math_to_float!("tan", f64::tan);
    math_to_float!("asin", f64::asin);
    math_to_float!("acos", f64::acos);
    math_to_float!("atan", f64::atan);
    math_to_float!("log", f64::ln);
    math_to_float!("log10", f64::log10);
    math_to_float!("exp", f64::exp);

    macro_rules! math_to_int {
        ($name:expr, $f:expr) => {
            builtin!(env, $name, 1, |_, args: &[Value]| {
                match &args[0] {
                    Value::Int(n) => Ok(Value::Int(*n)),
                    Value::Float(x) => {
                        let g: fn(f64) -> f64 = $f;
                        Ok(Value::Int(g(*x) as i64))
                    }
                    _ => Err(err(concat!($name, " requires a number"))),
                }
            });
        };
    }
    math_to_int!("floor", f64::floor);
    math_to_int!("ceil", f64::ceil);
    math_to_int!("round", f64::round);

    builtin!(env, "atan2", 2, |_, args: &[Value]| {
        let num = |v: &Value| match v {
            Value::Float(f) => Ok(*f),
            Value::Int(n) => Ok(*n as f64),
            _ => Err(err("atan2 requires numbers")),
        };
        Ok(Value::Float(num(&args[0])?.atan2(num(&args[1])?)))
    });

    env.set("pi".to_string(), Value::Float(std::f64::consts::PI));
    env.set("e".to_string(), Value::Float(std::f64::consts::E));

    // String → number parsing (issue #18): Option-returning, unlike int/float.
    builtin!(env, "parse-int", 1, |_, args: &[Value]| {
        match &args[0] {
            Value::Str(s) => Ok(match s.trim().parse::<i64>() {
                Ok(n) => Value::Adt("Some".to_string(), vec![Value::Int(n)]),
                Err(_) => Value::Adt("None".to_string(), vec![]),
            }),
            _ => Err(err("parse-int requires a string")),
        }
    });
    builtin!(env, "parse-float", 1, |_, args: &[Value]| {
        match &args[0] {
            Value::Str(s) => Ok(match s.trim().parse::<f64>() {
                Ok(n) => Value::Adt("Some".to_string(), vec![Value::Float(n)]),
                Err(_) => Value::Adt("None".to_string(), vec![]),
            }),
            _ => Err(err("parse-float requires a string")),
        }
    });

    // String helpers (issue #23).
    builtin!(env, "capitalize", 1, |_, args: &[Value]| {
        match &args[0] {
            Value::Str(s) => {
                let mut chars = s.chars();
                let out = match chars.next() {
                    Some(c) => c.to_uppercase().collect::<String>() + chars.as_str(),
                    None => String::new(),
                };
                Ok(Value::Str(out.into()))
            }
            _ => Err(err("capitalize requires a string")),
        }
    });
    builtin!(env, "repeat", 2, |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Str(s), Value::Int(n)) => {
                let n = (*n).max(0) as usize;
                if s.len().saturating_mul(n) > MAX_BUILT_LEN {
                    return Err(err(format!(
                        "repeat: result too large ({} x {n} bytes, limit {MAX_BUILT_LEN})",
                        s.len()
                    )));
                }
                Ok(Value::Str(s.repeat(n).into()))
            }
            _ => Err(err("repeat requires a string and a count")),
        }
    });
    fn pad(s: &str, width: i64, padding: &str, left: bool) -> Result<String, InterpError> {
        let len = s.chars().count();
        let want = width.max(0) as usize;
        if len >= want || padding.is_empty() {
            return Ok(s.to_string());
        }
        if want > MAX_BUILT_LEN {
            return Err(err(format!(
                "pad: width {want} too large (limit {MAX_BUILT_LEN})"
            )));
        }
        let fill: String = padding.chars().cycle().take(want - len).collect();
        Ok(if left {
            fill + s
        } else {
            s.to_string() + &fill
        })
    }
    builtin!(env, "pad-left", 3, |_, args: &[Value]| {
        match (&args[0], &args[1], &args[2]) {
            (Value::Str(s), Value::Int(w), Value::Str(p)) => {
                Ok(Value::Str(pad(s, *w, p, true)?.into()))
            }
            _ => Err(err("pad-left requires a string, width, and pad string")),
        }
    });
    builtin!(env, "pad-right", 3, |_, args: &[Value]| {
        match (&args[0], &args[1], &args[2]) {
            (Value::Str(s), Value::Int(w), Value::Str(p)) => {
                Ok(Value::Str(pad(s, *w, p, false)?.into()))
            }
            _ => Err(err("pad-right requires a string, width, and pad string")),
        }
    });

    // slice: [slice coll start end) on vectors and strings.
    builtin!(env, "slice", 3, |_, args: &[Value]| {
        let (start, end) = match (&args[1], &args[2]) {
            (Value::Int(s), Value::Int(e)) => (*s, *e),
            _ => return Err(err("slice requires integer start and end")),
        };
        match &args[0] {
            Value::Vec(v) => {
                let len = v.len() as i64;
                let s = start.clamp(0, len) as usize;
                let e = end.clamp(start.clamp(0, len), len) as usize;
                Ok(Value::Vec(v.clone().slice(s..e)))
            }
            Value::Str(st) => {
                let chars: Vec<char> = st.chars().collect();
                let len = chars.len() as i64;
                let s = start.clamp(0, len) as usize;
                let e = end.clamp(start.clamp(0, len), len) as usize;
                Ok(Value::Str(chars[s..e].iter().collect::<String>().into()))
            }
            _ => Err(err("slice requires a vector or string")),
        }
    });

    // concat: concatenate two vectors or strings.
    builtin!(env, "concat", 2, |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Vec(a), Value::Vec(b)) => {
                let mut out = a.clone();
                out.append(b.clone());
                Ok(Value::Vec(out))
            }
            (Value::Str(a), Value::Str(b)) => Ok(Value::Str(format!("{a}{b}").into())),
            _ => Err(err("concat requires two vectors or two strings")),
        }
    });

    builtin!(env, "abs", 1, |_, args: &[Value]| {
        match &args[0] {
            Value::Int(n) => Ok(Value::Int(n.wrapping_abs())),
            Value::Float(f) => Ok(Value::Float(f.abs())),
            _ => Err(err("abs requires a number")),
        }
    });

    builtin!(env, "first", 1, |_, args: &[Value]| {
        match &args[0] {
            Value::Vec(v) => Ok(v.front().cloned().unwrap_or(Value::Unit)),
            _ => Err(err("first requires a vector")),
        }
    });

    builtin!(env, "last", 1, |_, args: &[Value]| {
        match &args[0] {
            Value::Vec(v) => Ok(v.back().cloned().unwrap_or(Value::Unit)),
            _ => Err(err("last requires a vector")),
        }
    });

    builtin!(env, "some?", 1, |_, args: &[Value]| {
        match &args[0] {
            Value::Unit => Ok(Value::Bool(false)),
            Value::Adt(tag, _) if tag == "None" => Ok(Value::Bool(false)),
            _ => Ok(Value::Bool(true)),
        }
    });

    builtin!(env, "none?", 1, |_, args: &[Value]| {
        // Complement of some?: true for the "says nothing" values None and ().
        match &args[0] {
            Value::Unit => Ok(Value::Bool(true)),
            Value::Adt(tag, _) if tag == "None" => Ok(Value::Bool(true)),
            _ => Ok(Value::Bool(false)),
        }
    });

    builtin!(env, "nil?", 1, |_, args: &[Value]| {
        match &args[0] {
            Value::Unit => Ok(Value::Bool(true)),
            Value::Adt(tag, _) if tag == "None" => Ok(Value::Bool(true)),
            _ => Ok(Value::Bool(false)),
        }
    });

    builtin!(env, "type-of", 1, |_, args: &[Value]| {
        let t = match &args[0] {
            Value::Int(_) => "Int",
            Value::Float(_) => "Float",
            Value::Bool(_) => "Bool",
            Value::Str(_) => "String",
            Value::Keyword(_) => "Keyword",
            Value::Vec(_) => "Vec",
            Value::Set(_) => "Set",
            Value::Map(_) => "Map",
            Value::Tuple(_) => "Tuple",
            Value::Fn(_) | Value::Builtin(_, _) => "Fn",
            Value::Adt(tag, _) => tag.as_str(),
            Value::Unit => "Unit",
            _ => "Unknown",
        };
        Ok(Value::Str(t.into()))
    });

    // --- Conversion ---

    builtin!(env, "into-map", 1, |_, args: &[Value]| {
        match &args[0] {
            Value::Vec(v) => {
                let mut m = OrdMap::new();
                for item in v {
                    match item {
                        Value::Tuple(kv) if kv.len() == 2 => {
                            m = m.update(kv[0].clone(), kv[1].clone());
                        }
                        _ => return Err(err("into-map: each element must be a 2-tuple")),
                    }
                }
                Ok(Value::Map(m))
            }
            _ => Err(err("into-map requires a vector of pairs")),
        }
    });

    // --- Channel builtins ---

    builtin!(env, "channel", 0, |_, _args: &[Value]| {
        let id = NEXT_CHAN.with(|c| {
            let id = c.get();
            c.set(id + 1);
            id
        });
        CHANNELS.with(|ch| {
            ch.borrow_mut().insert(id, VecDeque::new());
        });
        Ok(Value::Tuple(vec![
            Value::ChannelTx(id),
            Value::ChannelRx(id),
        ]))
    });

    builtin!(env, "send", 1..=2, |_, args: &[Value]| {
        if let Value::ChannelTx(id) = &args[0] {
            let id = *id;
            let val = args.get(1).cloned().unwrap_or(Value::Unit);
            if is_shared(id) {
                shared_send(id, val)
            } else {
                CHANNELS.with(|ch| {
                    let mut channels = ch.borrow_mut();
                    if let Some(buf) = channels.get_mut(&id) {
                        buf.push_back(val);
                        Ok(Value::Unit)
                    } else {
                        Err(err(format!("channel {id} does not exist")))
                    }
                })
            }
        } else {
            Err(err("send requires a channel tx"))
        }
    });

    builtin!(env, "recv", 1, |_, args: &[Value]| {
        if let Value::ChannelRx(id) = &args[0] {
            let id = *id;
            if is_shared(id) {
                shared_recv(id)
            } else {
                CHANNELS.with(|ch| {
                    let mut channels = ch.borrow_mut();
                    if let Some(buf) = channels.get_mut(&id) {
                        if let Some(val) = buf.pop_front() {
                            Ok(val)
                        } else {
                            Err(err("recv on empty channel"))
                        }
                    } else {
                        Err(err(format!("channel {id} does not exist")))
                    }
                })
            }
        } else {
            Err(err("recv requires a channel rx"))
        }
    });

    builtin!(env, "try-recv", 1, |_, args: &[Value]| {
        if let Value::ChannelRx(id) = &args[0] {
            let id = *id;
            if is_shared(id) {
                shared_try_recv(id)
            } else {
                CHANNELS.with(|ch| {
                    let mut channels = ch.borrow_mut();
                    if let Some(buf) = channels.get_mut(&id) {
                        if let Some(val) = buf.pop_front() {
                            Ok(Value::Adt("Some".to_string(), vec![val]))
                        } else {
                            Ok(Value::Adt("None".to_string(), vec![]))
                        }
                    } else {
                        Err(err(format!("channel {id} does not exist")))
                    }
                })
            }
        } else {
            Err(err("try-recv requires a channel rx"))
        }
    });

    // ── Physics builtins ─────────────────────────────────────────────

    // unit: [unit value :keyword] → applies scale and returns Float
    builtin!(env, "unit", 2, |_, args: &[Value]| {
        if args.len() != 2 {
            return Err(err("unit requires exactly 2 arguments: value and :unit"));
        }
        let val = match &args[0] {
            Value::Float(f) => *f,
            Value::Int(n) => *n as f64,
            _ => return Err(err("unit: first argument must be a number")),
        };
        let unit_name = match &args[1] {
            Value::Keyword(k) => &**k,
            _ => return Err(err("unit: second argument must be a keyword")),
        };
        let scale = match unit_name {
            // Base SI (scale = 1)
            "m" | "s" | "kg" | "A" | "K" | "N" | "J" | "W" | "Pa" | "Hz" | "C" | "V" | "ohm"
            | "m2" | "m3" => 1.0,
            // Prefixed length
            "km" => 1e3,
            "cm" => 1e-2,
            "mm" => 1e-3,
            // Prefixed time
            "ms" => 1e-3,
            "us" => 1e-6,
            "ns" => 1e-9,
            // Prefixed mass
            "g" => 1e-3,
            "mg" => 1e-6,
            // Prefixed force
            "kN" => 1e3,
            // Prefixed pressure
            "kPa" => 1e3,
            "MPa" => 1e6,
            "GPa" => 1e9,
            // Prefixed power
            "kW" => 1e3,
            // Prefixed current
            "mA" => 1e-3,
            _ => return Err(err(format!("unknown unit: {unit_name}"))),
        };
        Ok(Value::Float(val * scale))
    });

    // magnitude: extracts numeric value from a Dim (identity at runtime — types are erased)
    builtin!(env, "magnitude", 1, |_, args: &[Value]| {
        if args.len() != 1 {
            return Err(err("magnitude requires exactly 1 argument"));
        }
        match &args[0] {
            Value::Float(f) => Ok(Value::Float(*f)),
            Value::Int(n) => Ok(Value::Float(*n as f64)),
            _ => Err(err("magnitude: argument must be a number")),
        }
    });

    // scalar: explicit entry into Dim world (identity at runtime)
    builtin!(env, "scalar", 1, |_, args: &[Value]| {
        if args.len() != 1 {
            return Err(err("scalar requires exactly 1 argument"));
        }
        match &args[0] {
            Value::Float(f) => Ok(Value::Float(*f)),
            Value::Int(n) => Ok(Value::Float(*n as f64)),
            _ => Err(err("scalar: argument must be a number")),
        }
    });

    // Physics constants (namespaced)
    env.set("Const.c".to_string(), Value::Float(299_792_458.0));
    env.set("Const.G".to_string(), Value::Float(6.674_30e-11));
    env.set("Const.h".to_string(), Value::Float(6.626_070_15e-34));
    env.set("Const.k-B".to_string(), Value::Float(1.380_649e-23));
    env.set(
        "Const.e-charge".to_string(),
        Value::Float(1.602_176_634e-19),
    );

    // Aliases (kept in lockstep with the builtin registry).
    for (alias, canonical) in [("reduce", "fold"), ("vals", "values")] {
        if let Some(v) = env.get(canonical) {
            env.set(alias.to_string(), v);
        }
    }
}

pub fn apply_value(func: &Value, args: &[Value]) -> IResult {
    match func {
        Value::Fn(lf) => {
            let mut env = if let Some(e) = get_global_env() {
                e
            } else {
                let mut e = Env::new();
                register_builtins(&mut e);
                e
            };
            call_fn(lf, args, &mut env, crate::syntax::Span::ZERO)
        }
        Value::Builtin(name, f) => f(name, args),
        _ => Err(err(super::not_callable_msg(func))),
    }
}

/// Ordering used by `sort`, `sort-by`, `min` and `max`.
///
/// It must be a total order: since Rust 1.81 `sort_by` may panic when the
/// comparator isn't one, and a panic aborts hosts that evaluate in-process.
/// Values of different kinds order by kind; ints and floats compare
/// numerically (an int before an equal float), floats by `total_cmp` (so NaN
/// has a place); kinds with no natural order compare equal among themselves.
pub fn value_cmp(a: &Value, b: &Value) -> std::cmp::Ordering {
    use std::cmp::Ordering;
    fn rank(v: &Value) -> u8 {
        match v {
            Value::Unit => 0,
            Value::Bool(_) => 1,
            Value::Int(_) | Value::Float(_) => 2,
            Value::Str(_) => 3,
            Value::Keyword(_) => 4,
            _ => 5,
        }
    }
    match (a, b) {
        (Value::Int(a), Value::Int(b)) => a.cmp(b),
        (Value::Float(a), Value::Float(b)) => a.total_cmp(b),
        // int -> f64 is monotone, so this plus the int-first tie-break stays
        // transitive even where the conversion rounds.
        (Value::Int(a), Value::Float(b)) => (*a as f64).total_cmp(b).then(Ordering::Less),
        (Value::Float(a), Value::Int(b)) => a.total_cmp(&(*b as f64)).then(Ordering::Greater),
        (Value::Bool(a), Value::Bool(b)) => a.cmp(b),
        (Value::Str(a), Value::Str(b)) => a.cmp(b),
        (Value::Keyword(a), Value::Keyword(b)) => a.cmp(b),
        _ => rank(a).cmp(&rank(b)),
    }
}
