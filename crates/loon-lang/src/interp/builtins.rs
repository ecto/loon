use super::value::{ChannelId, Value};
use super::{call_fn, err, get_global_env, Env, InterpError};
use std::cell::{Cell, RefCell};
use std::collections::{HashMap, VecDeque};
use std::sync::Arc;

type IResult = Result<Value, InterpError>;

thread_local! {
    static CHANNELS: RefCell<HashMap<ChannelId, VecDeque<Value>>> = RefCell::new(HashMap::new());
    static NEXT_CHAN: Cell<ChannelId> = const { Cell::new(0) };
}

pub fn register_builtins(env: &mut Env) {
    macro_rules! builtin {
        ($env:expr, $name:expr, $f:expr) => {
            $env.set(
                $name.to_string(),
                Value::Builtin($name.to_string(), Arc::new($f)),
            );
        };
    }

    builtin!(env, "+", |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a + *b as f64)),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float(*a as f64 + b)),
            _ => Err(err(format!("+ requires numbers, got {} and {}", args[0], args[1]))),
        }
    });

    builtin!(env, "-", |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a - *b as f64)),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float(*a as f64 - b)),
            _ => Err(err("- requires numbers")),
        }
    });

    builtin!(env, "*", |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a * *b as f64)),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float(*a as f64 * b)),
            _ => Err(err("* requires numbers")),
        }
    });

    builtin!(env, "/", |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Int(a), Value::Int(b)) => {
                if *b == 0 { return Err(err("division by zero")); }
                Ok(Value::Int(a / b))
            }
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a / b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a / *b as f64)),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float(*a as f64 / b)),
            _ => Err(err("/ requires numbers")),
        }
    });

    builtin!(env, "%", |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Int(a), Value::Int(b)) => {
                if *b == 0 { return Err(err("modulo by zero")); }
                Ok(Value::Int(a % b))
            }
            _ => Err(err("% requires integers")),
        }
    });

    builtin!(env, ">", |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a > b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a > b)),
            _ => Err(err("> requires numbers")),
        }
    });

    builtin!(env, "<", |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a < b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a < b)),
            _ => Err(err("< requires numbers")),
        }
    });

    builtin!(env, "=", |_, args: &[Value]| {
        Ok(Value::Bool(args[0] == args[1]))
    });

    builtin!(env, ">=", |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a >= b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a >= b)),
            _ => Err(err(">= requires numbers")),
        }
    });

    builtin!(env, "<=", |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a <= b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a <= b)),
            _ => Err(err("<= requires numbers")),
        }
    });

    builtin!(env, "not", |_, args: &[Value]| {
        Ok(Value::Bool(!args[0].is_truthy()))
    });

    builtin!(env, "or", |_, args: &[Value]| {
        for arg in args {
            if arg.is_truthy() {
                return Ok(arg.clone());
            }
        }
        Ok(args.last().cloned().unwrap_or(Value::Bool(false)))
    });

    builtin!(env, "and", |_, args: &[Value]| {
        for arg in args {
            if !arg.is_truthy() {
                return Ok(arg.clone());
            }
        }
        Ok(args.last().cloned().unwrap_or(Value::Bool(true)))
    });

    // --- String builtins ---

    builtin!(env, "str", |_, args: &[Value]| {
        let s: String = args.iter().map(|v| v.display_str()).collect();
        Ok(Value::Str(s))
    });

    builtin!(env, "println", |_, args: &[Value]| {
        let parts: Vec<String> = args.iter().map(|v| v.display_str()).collect();
        println!("{}", parts.join(" "));
        Ok(Value::Unit)
    });

    builtin!(env, "print", |_, args: &[Value]| {
        let parts: Vec<String> = args.iter().map(|v| v.display_str()).collect();
        print!("{}", parts.join(" "));
        Ok(Value::Unit)
    });

    builtin!(env, "split", |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Str(s), Value::Str(delims)) => {
                let words: Vec<Value> = s
                    .split(|c: char| delims.contains(c))
                    .map(|w| Value::Str(w.to_string()))
                    .collect();
                Ok(Value::Vec(words))
            }
            _ => Err(err("split requires a string and delimiters")),
        }
    });

    builtin!(env, "join", |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Str(sep), Value::Vec(v)) | (Value::Vec(v), Value::Str(sep)) => {
                let parts: Vec<String> = v.iter().map(|x| x.display_str()).collect();
                Ok(Value::Str(parts.join(sep)))
            }
            _ => Err(err("join requires a separator and vector")),
        }
    });

    builtin!(env, "trim", |_, args: &[Value]| {
        match &args[0] {
            Value::Str(s) => Ok(Value::Str(s.trim().to_string())),
            _ => Err(err("trim requires a string")),
        }
    });

    builtin!(env, "starts-with?", |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Str(s), Value::Str(prefix)) => Ok(Value::Bool(s.starts_with(prefix.as_str()))),
            _ => Err(err("starts-with? requires two strings")),
        }
    });

    builtin!(env, "ends-with?", |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Str(s), Value::Str(suffix)) => Ok(Value::Bool(s.ends_with(suffix.as_str()))),
            _ => Err(err("ends-with? requires two strings")),
        }
    });

    builtin!(env, "replace", |_, args: &[Value]| {
        match (&args[0], &args[1], &args[2]) {
            (Value::Str(s), Value::Str(from), Value::Str(to)) => {
                Ok(Value::Str(s.replace(from.as_str(), to)))
            }
            _ => Err(err("replace requires three strings")),
        }
    });

    builtin!(env, "uppercase", |_, args: &[Value]| {
        match &args[0] {
            Value::Str(s) => Ok(Value::Str(s.to_uppercase())),
            _ => Err(err("uppercase requires a string")),
        }
    });

    builtin!(env, "lowercase", |_, args: &[Value]| {
        match &args[0] {
            Value::Str(s) => Ok(Value::Str(s.to_lowercase())),
            _ => Err(err("lowercase requires a string")),
        }
    });

    // --- Collection builtins ---

    builtin!(env, "len", |_, args: &[Value]| {
        match &args[0] {
            Value::Vec(v) => Ok(Value::Int(v.len() as i64)),
            Value::Str(s) => Ok(Value::Int(s.len() as i64)),
            Value::Map(m) => Ok(Value::Int(m.len() as i64)),
            Value::Set(s) => Ok(Value::Int(s.len() as i64)),
            _ => Err(err("len requires a collection")),
        }
    });

    builtin!(env, "nth", |_, args: &[Value]| {
        let items = match &args[0] {
            Value::Vec(v) => v,
            Value::Tuple(v) => v,
            _ => return Err(err(format!(
                "nth requires a vector/tuple and index, got {} and {}",
                args[0], args[1]
            ))),
        };
        if let Value::Int(i) = &args[1] {
            let idx = *i as usize;
            if idx < items.len() {
                Ok(items[idx].clone())
            } else if args.len() > 2 {
                Ok(args[2].clone())
            } else {
                Err(err(format!("index {i} out of bounds (len {})", items.len())))
            }
        } else {
            Err(err(format!(
                "nth requires a vector/tuple and index, got {} and {}",
                args[0], args[1]
            )))
        }
    });

    builtin!(env, "map", |_, args: &[Value]| {
        match (&args[0], args.get(1)) {
            (Value::Vec(v), Some(func)) => {
                let mut result = Vec::new();
                for item in v {
                    let val = apply_value(func, std::slice::from_ref(item))?;
                    result.push(val);
                }
                Ok(Value::Vec(result))
            }
            (func, Some(Value::Vec(v))) if func.is_callable() => {
                let mut result = Vec::new();
                for item in v {
                    let val = apply_value(func, std::slice::from_ref(item))?;
                    result.push(val);
                }
                Ok(Value::Vec(result))
            }
            (func, None) if func.is_callable() => {
                let func_clone = func.clone();
                Ok(Value::Builtin(
                    "map-partial".to_string(),
                    Arc::new(move |_, inner_args: &[Value]| {
                        if let Value::Vec(v) = &inner_args[0] {
                            let mut result = Vec::new();
                            for item in v {
                                let val = apply_value(&func_clone, std::slice::from_ref(item))?;
                                result.push(val);
                            }
                            Ok(Value::Vec(result))
                        } else {
                            Err(err("map requires a vector"))
                        }
                    }),
                ))
            }
            _ => Err(err("map requires a function and vector")),
        }
    });

    builtin!(env, "filter", |_, args: &[Value]| {
        match (&args[0], args.get(1)) {
            (Value::Vec(v), Some(func)) => {
                let mut result = Vec::new();
                for item in v {
                    let val = apply_value(func, std::slice::from_ref(item))?;
                    if val.is_truthy() {
                        result.push(item.clone());
                    }
                }
                Ok(Value::Vec(result))
            }
            (func, Some(Value::Vec(v))) if func.is_callable() => {
                let mut result = Vec::new();
                for item in v {
                    let val = apply_value(func, std::slice::from_ref(item))?;
                    if val.is_truthy() {
                        result.push(item.clone());
                    }
                }
                Ok(Value::Vec(result))
            }
            (func, None) if func.is_callable() => {
                let func_clone = func.clone();
                Ok(Value::Builtin(
                    "filter-partial".to_string(),
                    Arc::new(move |_, inner_args: &[Value]| {
                        if let Value::Vec(v) = &inner_args[0] {
                            let mut result = Vec::new();
                            for item in v {
                                let val = apply_value(&func_clone, std::slice::from_ref(item))?;
                                if val.is_truthy() {
                                    result.push(item.clone());
                                }
                            }
                            Ok(Value::Vec(result))
                        } else {
                            Err(err("filter requires a vector"))
                        }
                    }),
                ))
            }
            _ => Err(err("filter requires a function and vector")),
        }
    });

    builtin!(env, "fold", |_, args: &[Value]| {
        fn do_fold(v: &[Value], init: &Value, func: &Value) -> IResult {
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
                Ok(Value::Builtin(
                    "fold-partial".to_string(),
                    Arc::new(move |_, inner_args: &[Value]| {
                        if let Value::Vec(v) = &inner_args[0] {
                            do_fold(v, &init_clone, &func_clone)
                        } else {
                            Err(err("fold requires a vector"))
                        }
                    }),
                ))
            }
            _ => Err(err("fold requires init, function, and vector")),
        }
    });

    builtin!(env, "conj", |_, args: &[Value]| {
        match &args[0] {
            Value::Vec(v) => {
                let mut new = v.clone();
                for a in &args[1..] {
                    new.push(a.clone());
                }
                Ok(Value::Vec(new))
            }
            Value::Set(s) => {
                let mut new = s.clone();
                for a in &args[1..] {
                    if !new.contains(a) {
                        new.push(a.clone());
                    }
                }
                Ok(Value::Set(new))
            }
            _ => Err(err("conj requires a collection")),
        }
    });

    builtin!(env, "get", |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Map(pairs), key) => {
                for (k, v) in pairs {
                    if k == key {
                        return Ok(v.clone());
                    }
                }
                if args.len() > 2 {
                    Ok(args[2].clone())
                } else {
                    Ok(Value::Unit)
                }
            }
            (Value::Vec(v), Value::Int(i)) => {
                let idx = *i as usize;
                Ok(v.get(idx).cloned().unwrap_or(Value::Unit))
            }
            _ => Err(err("get requires a map/vector and key")),
        }
    });

    builtin!(env, "assoc", |_, args: &[Value]| {
        if let Value::Map(pairs) = &args[0] {
            let mut new = pairs.clone();
            let key = &args[1];
            let val = &args[2];
            if let Some(pair) = new.iter_mut().find(|(k, _)| k == key) {
                pair.1 = val.clone();
            } else {
                new.push((key.clone(), val.clone()));
            }
            Ok(Value::Map(new))
        } else {
            Err(err("assoc requires a map"))
        }
    });

    builtin!(env, "update", |_, args: &[Value]| {
        if let Value::Map(pairs) = &args[0] {
            let key = &args[1];
            let func = &args[2];
            let mut new = pairs.clone();
            let current = new
                .iter()
                .find(|(k, _)| k == key)
                .map(|(_, v)| v.clone())
                .unwrap_or(Value::Unit);
            let updated = apply_value(func, &[current])?;
            if let Some(pair) = new.iter_mut().find(|(k, _)| k == key) {
                pair.1 = updated;
            } else {
                new.push((key.clone(), updated));
            }
            Ok(Value::Map(new))
        } else {
            Err(err("update requires a map"))
        }
    });

    builtin!(env, "range", |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Int(start), Value::Int(end)) => {
                let v: Vec<Value> = (*start..*end).map(Value::Int).collect();
                Ok(Value::Vec(v))
            }
            _ => Err(err("range requires two integers")),
        }
    });

    builtin!(env, "contains?", |_, args: &[Value]| {
        match &args[0] {
            Value::Set(s) => Ok(Value::Bool(s.contains(&args[1]))),
            Value::Map(m) => Ok(Value::Bool(m.iter().any(|(k, _)| k == &args[1]))),
            Value::Vec(v) => Ok(Value::Bool(v.contains(&args[1]))),
            _ => Err(err("contains? requires a collection")),
        }
    });

    builtin!(env, "empty?", |_, args: &[Value]| {
        match &args[0] {
            Value::Vec(v) => Ok(Value::Bool(v.is_empty())),
            Value::Str(s) => Ok(Value::Bool(s.is_empty())),
            Value::Map(m) => Ok(Value::Bool(m.is_empty())),
            Value::Set(s) => Ok(Value::Bool(s.is_empty())),
            _ => Err(err("empty? requires a collection")),
        }
    });

    builtin!(env, "sort-by", |_, args: &[Value]| {
        fn do_sort(func: &Value, desc: bool, v: &[Value]) -> IResult {
            let mut sorted = v.to_vec();
            sorted.sort_by(|a, b| {
                let ka = apply_value(func, std::slice::from_ref(a)).unwrap_or(Value::Int(0));
                let kb = apply_value(func, std::slice::from_ref(b)).unwrap_or(Value::Int(0));
                let ord = value_cmp(&ka, &kb);
                if desc { ord.reverse() } else { ord }
            });
            Ok(Value::Vec(sorted))
        }

        match args {
            [func, order, Value::Vec(v)] if func.is_callable() => {
                let desc = matches!(order, Value::Keyword(k) if k == "desc");
                do_sort(func, desc, v)
            }
            [func, order] => {
                let func_clone = func.clone();
                let desc = matches!(order, Value::Keyword(k) if k == "desc");
                Ok(Value::Builtin(
                    "sort-by-partial".to_string(),
                    Arc::new(move |_, inner_args: &[Value]| {
                        if let Value::Vec(v) = &inner_args[0] {
                            do_sort(&func_clone, desc, v)
                        } else {
                            Err(err("sort-by requires a vector"))
                        }
                    }),
                ))
            }
            _ => Err(err("sort-by requires a function and optional order")),
        }
    });

    builtin!(env, "take", |_, args: &[Value]| {
        match (&args[0], args.get(1)) {
            (Value::Int(n), Some(Value::Vec(v))) => {
                Ok(Value::Vec(v.iter().take(*n as usize).cloned().collect()))
            }
            (Value::Int(n), None) => {
                let n = *n;
                Ok(Value::Builtin(
                    "take-partial".to_string(),
                    Arc::new(move |_, inner_args: &[Value]| {
                        if let Value::Vec(v) = &inner_args[0] {
                            Ok(Value::Vec(v.iter().take(n as usize).cloned().collect()))
                        } else {
                            Err(err("take requires a vector"))
                        }
                    }),
                ))
            }
            _ => Err(err("take requires a count and vector")),
        }
    });

    builtin!(env, "drop", |_, args: &[Value]| {
        match (&args[0], args.get(1)) {
            (Value::Int(n), Some(Value::Vec(v))) => {
                Ok(Value::Vec(v.iter().skip(*n as usize).cloned().collect()))
            }
            (Value::Int(n), None) => {
                let n = *n;
                Ok(Value::Builtin(
                    "drop-partial".to_string(),
                    Arc::new(move |_, inner_args: &[Value]| {
                        if let Value::Vec(v) = &inner_args[0] {
                            Ok(Value::Vec(v.iter().skip(n as usize).cloned().collect()))
                        } else {
                            Err(err("drop requires a vector"))
                        }
                    }),
                ))
            }
            _ => Err(err("drop requires a count and vector")),
        }
    });

    builtin!(env, "each", |_, args: &[Value]| {
        match (&args[0], args.get(1)) {
            (Value::Vec(v), Some(func)) => {
                for item in v {
                    apply_value(func, std::slice::from_ref(item))?;
                }
                Ok(Value::Unit)
            }
            (func, Some(Value::Vec(v))) if func.is_callable() => {
                for item in v {
                    apply_value(func, std::slice::from_ref(item))?;
                }
                Ok(Value::Unit)
            }
            (func, None) if func.is_callable() => {
                let func_clone = func.clone();
                Ok(Value::Builtin(
                    "each-partial".to_string(),
                    Arc::new(move |_, inner_args: &[Value]| {
                        if let Value::Vec(v) = &inner_args[0] {
                            for item in v {
                                apply_value(&func_clone, std::slice::from_ref(item))?;
                            }
                            Ok(Value::Unit)
                        } else {
                            Err(err("each requires a vector"))
                        }
                    }),
                ))
            }
            _ => Err(err("each requires a function and vector")),
        }
    });

    builtin!(env, "entries", |_, args: &[Value]| {
        match &args[0] {
            Value::Map(pairs) => {
                let v: Vec<Value> = pairs
                    .iter()
                    .map(|(k, v)| Value::Tuple(vec![k.clone(), v.clone()]))
                    .collect();
                Ok(Value::Vec(v))
            }
            _ => Err(err("entries requires a map")),
        }
    });

    builtin!(env, "collect", |_, args: &[Value]| {
        Ok(args[0].clone())
    });

    builtin!(env, "push!", |_, args: &[Value]| {
        if let Value::Vec(v) = &args[0] {
            let mut new = v.clone();
            for a in &args[1..] {
                new.push(a.clone());
            }
            Ok(Value::Vec(new))
        } else if let Value::Str(s) = &args[0] {
            let mut new = s.clone();
            for a in &args[1..] {
                new.push_str(&a.display_str());
            }
            Ok(Value::Str(new))
        } else {
            Err(err("push! requires a mutable collection"))
        }
    });

    builtin!(env, "assert-eq", |_, args: &[Value]| {
        if args[0] == args[1] {
            Ok(Value::Unit)
        } else {
            Err(err(format!(
                "assert-eq failed:\n  expected: {}\n  actual:   {}",
                args[1], args[0]
            )))
        }
    });

    builtin!(env, "HashMap.new", |_, _args: &[Value]| {
        Ok(Value::Map(vec![]))
    });

    // --- New v0.2 builtins ---

    builtin!(env, "zip", |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Vec(a), Value::Vec(b)) => {
                let pairs: Vec<Value> = a.iter().zip(b.iter())
                    .map(|(x, y)| Value::Tuple(vec![x.clone(), y.clone()]))
                    .collect();
                Ok(Value::Vec(pairs))
            }
            _ => Err(err("zip requires two vectors")),
        }
    });

    builtin!(env, "flatten", |_, args: &[Value]| {
        match &args[0] {
            Value::Vec(v) => {
                let mut result = Vec::new();
                for item in v {
                    if let Value::Vec(inner) = item {
                        result.extend(inner.iter().cloned());
                    } else {
                        result.push(item.clone());
                    }
                }
                Ok(Value::Vec(result))
            }
            _ => Err(err("flatten requires a vector")),
        }
    });

    builtin!(env, "chunk", |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Int(n), Value::Vec(v)) => {
                let chunks: Vec<Value> = v.chunks(*n as usize)
                    .map(|c| Value::Vec(c.to_vec()))
                    .collect();
                Ok(Value::Vec(chunks))
            }
            _ => Err(err("chunk requires a size and vector")),
        }
    });

    builtin!(env, "reverse", |_, args: &[Value]| {
        match &args[0] {
            Value::Vec(v) => {
                let mut rev = v.clone();
                rev.reverse();
                Ok(Value::Vec(rev))
            }
            Value::Str(s) => Ok(Value::Str(s.chars().rev().collect())),
            _ => Err(err("reverse requires a vector or string")),
        }
    });

    builtin!(env, "find", |_, args: &[Value]| {
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

    builtin!(env, "any?", |_, args: &[Value]| {
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

    builtin!(env, "all?", |_, args: &[Value]| {
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

    builtin!(env, "name", |_, args: &[Value]| {
        match &args[0] {
            Value::Keyword(k) => Ok(Value::Str(k.clone())),
            Value::Str(s) => Ok(Value::Str(s.clone())),
            _ => Err(err("name requires a keyword or string")),
        }
    });

    builtin!(env, "map?", |_, args: &[Value]| {
        Ok(Value::Bool(matches!(&args[0], Value::Map(_))))
    });

    // --- Cons (prepend to vec) ---

    builtin!(env, "cons", |_, args: &[Value]| {
        match &args[1] {
            Value::Vec(v) => {
                let mut new = vec![args[0].clone()];
                new.extend(v.iter().cloned());
                Ok(Value::Vec(new))
            }
            _ => Err(err("cons: second arg must be a vec")),
        }
    });

    // --- Map builtins ---

    builtin!(env, "keys", |_, args: &[Value]| {
        match &args[0] {
            Value::Map(pairs) => {
                let ks: Vec<Value> = pairs.iter().map(|(k, _)| k.clone()).collect();
                Ok(Value::Vec(ks))
            }
            _ => Err(err("keys requires a map")),
        }
    });

    builtin!(env, "values", |_, args: &[Value]| {
        match &args[0] {
            Value::Map(pairs) => {
                let vs: Vec<Value> = pairs.iter().map(|(_, v)| v.clone()).collect();
                Ok(Value::Vec(vs))
            }
            _ => Err(err("values requires a map")),
        }
    });

    builtin!(env, "merge", |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Map(a), Value::Map(b)) => {
                let mut merged = a.clone();
                for (k, v) in b {
                    if let Some(pair) = merged.iter_mut().find(|(mk, _)| mk == k) {
                        pair.1 = v.clone();
                    } else {
                        merged.push((k.clone(), v.clone()));
                    }
                }
                Ok(Value::Map(merged))
            }
            _ => Err(err("merge requires two maps")),
        }
    });

    builtin!(env, "remove", |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Map(pairs), key) => {
                let filtered: Vec<(Value, Value)> = pairs.iter()
                    .filter(|(k, _)| k != key)
                    .cloned()
                    .collect();
                Ok(Value::Map(filtered))
            }
            _ => Err(err("remove requires a map and key")),
        }
    });

    // --- Number parsing ---

    builtin!(env, "int", |_, args: &[Value]| {
        match &args[0] {
            Value::Str(s) => s.trim().parse::<i64>()
                .map(Value::Int)
                .map_err(|e| err(format!("int: cannot parse '{}': {}", s, e))),
            Value::Int(n) => Ok(Value::Int(*n)),
            Value::Float(f) => Ok(Value::Int(*f as i64)),
            _ => Err(err(format!("int: cannot convert {}", args[0]))),
        }
    });

    builtin!(env, "float", |_, args: &[Value]| {
        match &args[0] {
            Value::Str(s) => s.trim().parse::<f64>()
                .map(Value::Float)
                .map_err(|e| err(format!("float: cannot parse '{}': {}", s, e))),
            Value::Float(f) => Ok(Value::Float(*f)),
            Value::Int(n) => Ok(Value::Float(*n as f64)),
            _ => Err(err(format!("float: cannot convert {}", args[0]))),
        }
    });

    // --- String ops ---

    builtin!(env, "char-at", |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Str(s), Value::Int(i)) => {
                let idx = *i as usize;
                s.chars().nth(idx)
                    .map(|c| Value::Str(c.to_string()))
                    .ok_or_else(|| err(format!("char-at: index {} out of bounds (len {})", i, s.chars().count())))
            }
            _ => Err(err("char-at requires a string and index")),
        }
    });

    builtin!(env, "substring", |_, args: &[Value]| {
        match (&args[0], &args[1], &args[2]) {
            (Value::Str(s), Value::Int(start), Value::Int(end)) => {
                let start = *start as usize;
                let end = *end as usize;
                let chars: Vec<char> = s.chars().collect();
                if start > chars.len() || end > chars.len() || start > end {
                    return Err(err(format!("substring: invalid range {}..{} for len {}", start, end, chars.len())));
                }
                Ok(Value::Str(chars[start..end].iter().collect()))
            }
            _ => Err(err("substring requires a string, start, and end")),
        }
    });

    builtin!(env, "contains-str?", |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Str(haystack), Value::Str(needle)) => {
                Ok(Value::Bool(haystack.contains(needle.as_str())))
            }
            _ => Err(err("contains-str? requires two strings")),
        }
    });

    builtin!(env, "index-of", |_, args: &[Value]| {
        match (&args[0], &args[1]) {
            (Value::Str(haystack), Value::Str(needle)) => {
                match haystack.find(needle.as_str()) {
                    Some(pos) => Ok(Value::Int(pos as i64)),
                    None => Ok(Value::Int(-1)),
                }
            }
            _ => Err(err("index-of requires two strings")),
        }
    });

    // --- Collection ops ---

    builtin!(env, "group-by", |_, args: &[Value]| {
        match (&args[0], args.get(1)) {
            (func, Some(Value::Vec(v))) if func.is_callable() => {
                let mut groups: Vec<(Value, Vec<Value>)> = Vec::new();
                for item in v {
                    let key = apply_value(func, std::slice::from_ref(item))?;
                    if let Some(group) = groups.iter_mut().find(|(k, _)| k == &key) {
                        group.1.push(item.clone());
                    } else {
                        groups.push((key, vec![item.clone()]));
                    }
                }
                let map: Vec<(Value, Value)> = groups
                    .into_iter()
                    .map(|(k, v)| (k, Value::Vec(v)))
                    .collect();
                Ok(Value::Map(map))
            }
            (func, None) if func.is_callable() => {
                let func_clone = func.clone();
                Ok(Value::Builtin(
                    "group-by-partial".to_string(),
                    Arc::new(move |_, inner_args: &[Value]| {
                        if let Value::Vec(v) = &inner_args[0] {
                            let mut groups: Vec<(Value, Vec<Value>)> = Vec::new();
                            for item in v {
                                let key = apply_value(&func_clone, std::slice::from_ref(item))?;
                                if let Some(group) = groups.iter_mut().find(|(k, _)| k == &key) {
                                    group.1.push(item.clone());
                                } else {
                                    groups.push((key, vec![item.clone()]));
                                }
                            }
                            let map: Vec<(Value, Value)> = groups
                                .into_iter()
                                .map(|(k, v)| (k, Value::Vec(v)))
                                .collect();
                            Ok(Value::Map(map))
                        } else {
                            Err(err("group-by requires a vector"))
                        }
                    }),
                ))
            }
            _ => Err(err("group-by requires a function and vector")),
        }
    });

    builtin!(env, "flat-map", |_, args: &[Value]| {
        match (&args[0], args.get(1)) {
            (func, Some(Value::Vec(v))) if func.is_callable() => {
                let mut result = Vec::new();
                for item in v {
                    let val = apply_value(func, std::slice::from_ref(item))?;
                    if let Value::Vec(inner) = val {
                        result.extend(inner);
                    } else {
                        result.push(val);
                    }
                }
                Ok(Value::Vec(result))
            }
            (func, None) if func.is_callable() => {
                let func_clone = func.clone();
                Ok(Value::Builtin(
                    "flat-map-partial".to_string(),
                    Arc::new(move |_, inner_args: &[Value]| {
                        if let Value::Vec(v) = &inner_args[0] {
                            let mut result = Vec::new();
                            for item in v {
                                let val = apply_value(&func_clone, std::slice::from_ref(item))?;
                                if let Value::Vec(inner) = val {
                                    result.extend(inner);
                                } else {
                                    result.push(val);
                                }
                            }
                            Ok(Value::Vec(result))
                        } else {
                            Err(err("flat-map requires a vector"))
                        }
                    }),
                ))
            }
            _ => Err(err("flat-map requires a function and vector")),
        }
    });

    builtin!(env, "sort", |_, args: &[Value]| {
        match &args[0] {
            Value::Vec(v) => {
                let mut sorted = v.clone();
                sorted.sort_by(value_cmp);
                Ok(Value::Vec(sorted))
            }
            _ => Err(err("sort requires a vector")),
        }
    });

    builtin!(env, "min", |_, args: &[Value]| {
        match &args[0] {
            Value::Vec(v) if !v.is_empty() => {
                let mut result = &v[0];
                for item in &v[1..] {
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

    builtin!(env, "max", |_, args: &[Value]| {
        match &args[0] {
            Value::Vec(v) if !v.is_empty() => {
                let mut result = &v[0];
                for item in &v[1..] {
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

    builtin!(env, "sum", |_, args: &[Value]| {
        match &args[0] {
            Value::Vec(v) => {
                if v.is_empty() {
                    return Ok(Value::Int(0));
                }
                let mut acc = v[0].clone();
                for item in &v[1..] {
                    acc = match (&acc, item) {
                        (Value::Int(a), Value::Int(b)) => Value::Int(a + b),
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

    // --- Conversion ---

    builtin!(env, "to-string", |_, args: &[Value]| {
        Ok(Value::Str(args[0].display_str()))
    });

    builtin!(env, "into-map", |_, args: &[Value]| {
        match &args[0] {
            Value::Vec(v) => {
                let mut pairs = Vec::new();
                for item in v {
                    match item {
                        Value::Tuple(kv) if kv.len() == 2 => {
                            pairs.push((kv[0].clone(), kv[1].clone()));
                        }
                        _ => return Err(err("into-map: each element must be a 2-tuple")),
                    }
                }
                Ok(Value::Map(pairs))
            }
            _ => Err(err("into-map requires a vector of pairs")),
        }
    });

    // --- Channel builtins ---

    builtin!(env, "channel", |_, _args: &[Value]| {
        let id = NEXT_CHAN.with(|c| {
            let id = c.get();
            c.set(id + 1);
            id
        });
        CHANNELS.with(|ch| {
            ch.borrow_mut().insert(id, VecDeque::new());
        });
        Ok(Value::Tuple(vec![Value::ChannelTx(id), Value::ChannelRx(id)]))
    });

    builtin!(env, "send", |_, args: &[Value]| {
        if let Value::ChannelTx(id) = &args[0] {
            let id = *id;
            let val = args.get(1).cloned().unwrap_or(Value::Unit);
            CHANNELS.with(|ch| {
                let mut channels = ch.borrow_mut();
                if let Some(buf) = channels.get_mut(&id) {
                    buf.push_back(val);
                    Ok(Value::Unit)
                } else {
                    Err(err(format!("channel {id} does not exist")))
                }
            })
        } else {
            Err(err("send requires a channel tx"))
        }
    });

    builtin!(env, "recv", |_, args: &[Value]| {
        if let Value::ChannelRx(id) = &args[0] {
            let id = *id;
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
        } else {
            Err(err("recv requires a channel rx"))
        }
    });
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
            call_fn(lf, args, &mut env)
        }
        Value::Builtin(name, f) => f(name, args),
        _ => Err(err(format!("not callable: {func}"))),
    }
}

pub fn value_cmp(a: &Value, b: &Value) -> std::cmp::Ordering {
    match (a, b) {
        (Value::Int(a), Value::Int(b)) => a.cmp(b),
        (Value::Float(a), Value::Float(b)) => a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal),
        (Value::Str(a), Value::Str(b)) => a.cmp(b),
        _ => std::cmp::Ordering::Equal,
    }
}
