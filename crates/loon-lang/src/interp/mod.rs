mod env;
mod value;

pub use env::Env;
pub use value::Value;

use crate::ast::{Expr, ExprKind};
use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::Arc;

thread_local! {
    static GLOBAL_ENV: RefCell<Option<Env>> = const { RefCell::new(None) };
}

#[derive(Debug, Clone)]
pub struct InterpError {
    pub message: String,
    /// If set, this is a performed effect that needs handling
    pub performed_effect: Option<PerformedEffect>,
}

#[derive(Debug, Clone)]
pub struct PerformedEffect {
    pub effect: String,
    pub operation: String,
    pub args: Vec<Value>,
}

impl std::fmt::Display for InterpError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "runtime error: {}", self.message)
    }
}

impl std::error::Error for InterpError {}

type IResult = Result<Value, InterpError>;

fn err(msg: impl Into<String>) -> InterpError {
    InterpError {
        message: msg.into(),
        performed_effect: None,
    }
}

fn perform_effect(effect: &str, op: &str, args: Vec<Value>) -> InterpError {
    InterpError {
        message: format!("unhandled effect: {effect}.{op}"),
        performed_effect: Some(PerformedEffect {
            effect: effect.to_string(),
            operation: op.to_string(),
            args,
        }),
    }
}

/// Public wrappers for REPL use
pub fn register_builtins_pub(env: &mut Env) {
    register_builtins(env);
}

pub fn sync_global_env_pub(env: &Env) {
    sync_global_env(env);
}

pub fn eval_program(exprs: &[Expr]) -> IResult {
    let mut env = Env::new();
    register_builtins(&mut env);
    let mut last = Value::Unit;
    for expr in exprs {
        last = eval(expr, &mut env)?;
        // Keep global env in sync for apply_value
        sync_global_env(&env);
    }
    // If there's a main function, call it
    if let Some(Value::Fn(main_fn)) = env.get("main") {
        return call_fn(&main_fn, &[], &mut env);
    }
    Ok(last)
}

fn sync_global_env(env: &Env) {
    GLOBAL_ENV.with(|g| {
        *g.borrow_mut() = Some(env.clone());
    });
}

fn get_global_env() -> Option<Env> {
    GLOBAL_ENV.with(|g| g.borrow().clone())
}

pub fn eval(expr: &Expr, env: &mut Env) -> IResult {
    match &expr.kind {
        ExprKind::Int(n) => Ok(Value::Int(*n)),
        ExprKind::Float(n) => Ok(Value::Float(*n)),
        ExprKind::Bool(b) => Ok(Value::Bool(*b)),
        ExprKind::Str(s) => Ok(Value::Str(s.clone())),
        ExprKind::Keyword(k) => Ok(Value::Keyword(k.clone())),
        ExprKind::Symbol(s) => env.get(s).ok_or_else(|| err(format!("unbound symbol '{s}'"))),

        ExprKind::Vec(items) => {
            let vals: Result<Vec<_>, _> = items.iter().map(|e| eval(e, env)).collect();
            Ok(Value::Vec(vals?))
        }
        ExprKind::Set(items) => {
            let vals: Result<Vec<_>, _> = items.iter().map(|e| eval(e, env)).collect();
            Ok(Value::Set(vals?))
        }
        ExprKind::Map(pairs) => {
            let mut map = Vec::new();
            for (k, v) in pairs {
                map.push((eval(k, env)?, eval(v, env)?));
            }
            Ok(Value::Map(map))
        }
        ExprKind::Tuple(items) => {
            let vals: Result<Vec<_>, _> = items.iter().map(|e| eval(e, env)).collect();
            Ok(Value::Tuple(vals?))
        }

        ExprKind::List(items) if items.is_empty() => Ok(Value::Unit),

        ExprKind::List(items) => {
            let head = &items[0];
            // Check for special forms
            if let ExprKind::Symbol(s) = &head.kind {
                match s.as_str() {
                    "defn" => return eval_defn(&items[1..], env),
                    "let" => return eval_let(&items[1..], env),
                    "fn" => return eval_lambda(&items[1..], env),
                    "if" => return eval_if(&items[1..], env),
                    "do" => return eval_do(&items[1..], env),
                    "match" => return eval_match(&items[1..], env),
                    "|>" => return eval_pipe(&items[1..], env),
                    "mut" => return eval_mut(&items[1..], env),
                    "type" => return eval_type_def(&items[1..], env),
                    "test" => return eval_test_def(&items[1..], env),
                    "effect" => return Ok(Value::Unit), // effect declarations are compile-time
                    "handle" => return eval_handle(&items[1..], env),
                    "pub" => {
                        // [pub defn ...] — just treat as the inner form
                        if items.len() > 1 {
                            let inner = Expr::new(
                                ExprKind::List(items[1..].to_vec()),
                                expr.span,
                            );
                            return eval(&inner, env);
                        }
                        return Ok(Value::Unit);
                    }
                    _ => {}
                }
            }

            // Check for effect operations (Effect.op pattern)
            if let ExprKind::Symbol(s) = &head.kind {
                if let Some((effect, op)) = s.split_once('.') {
                    if effect.starts_with(char::is_uppercase) {
                        let args: Result<Vec<_>, _> =
                            items[1..].iter().map(|e| eval(e, env)).collect();
                        let args = args?;
                        return Err(perform_effect(effect, op, args));
                    }
                }
            }

            // Function call
            let func = eval(head, env)?;
            let args: Result<Vec<_>, _> = items[1..].iter().map(|e| eval(e, env)).collect();
            let args = args?;

            match func {
                Value::Fn(lf) => call_fn(&lf, &args, env),
                Value::Builtin(name, f) => f(&name, &args),
                _ => Err(err(format!("not callable: {func}"))),
            }
        }
    }
}

fn eval_defn(args: &[Expr], env: &mut Env) -> IResult {
    if args.len() < 2 {
        return Err(err("defn requires a name and body"));
    }
    let name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(err("defn name must be a symbol")),
    };

    // Check for multi-arity: [defn name (params body) (params body) ...]
    if matches!(args[1].kind, ExprKind::Tuple(_)) {
        let mut clauses = Vec::new();
        for clause_expr in &args[1..] {
            if let ExprKind::Tuple(clause_items) = &clause_expr.kind {
                if clause_items.len() < 2 {
                    return Err(err("multi-arity clause needs params and body"));
                }
                let params = extract_params(&clause_items[0])?;
                let body = clause_items[1..].to_vec();
                clauses.push((params, body));
            } else {
                return Err(err("expected multi-arity clause (params body)"));
            }
        }
        let lf = value::LoonFn {
            name: Some(name.clone()),
            clauses,
            captured_env: None,
        };
        env.set_global(name, Value::Fn(lf));
        return Ok(Value::Unit);
    }

    // Single-arity: [defn name [params] body...]
    let params = extract_params(&args[1])?;
    // Skip effect annotation: / {effects}
    let mut body_start = 2;
    if body_start < args.len() {
        if let ExprKind::Symbol(s) = &args[body_start].kind {
            if s == "/" {
                body_start += 2; // skip / and the effect set
            }
        }
    }
    let body = args[body_start..].to_vec();

    let lf = value::LoonFn {
        name: Some(name.clone()),
        clauses: vec![(params, body)],
        captured_env: None, // Will be resolved at call time from caller's env
    };
    env.set_global(name, Value::Fn(lf));
    Ok(Value::Unit)
}

fn eval_let(args: &[Expr], env: &mut Env) -> IResult {
    if args.len() < 2 {
        return Err(err("let requires a binding and value"));
    }

    // Handle [let mut name val]
    let (binding, val_expr) = if matches!(&args[0].kind, ExprKind::Symbol(s) if s == "mut") {
        if args.len() < 3 {
            return Err(err("let mut requires name and value"));
        }
        (&args[1], &args[2])
    } else {
        (&args[0], &args[1])
    };

    let val = eval(val_expr, env)?;

    // Pattern destructuring
    match &binding.kind {
        ExprKind::Symbol(name) => {
            if name == "_" {
                // Wildcard — discard
            } else {
                env.set(name.clone(), val.clone());
            }
        }
        // [let [x y] expr] — vec destructuring
        ExprKind::List(patterns) => {
            if let Value::Vec(ref items) | Value::Tuple(ref items) = val {
                for (i, pat) in patterns.iter().enumerate() {
                    if let ExprKind::Symbol(name) = &pat.kind {
                        if name != "_" {
                            let v = items.get(i).cloned().unwrap_or(Value::Unit);
                            env.set(name.clone(), v);
                        }
                    }
                }
            } else {
                return Err(err("destructuring requires a vector or tuple"));
            }
        }
        // [let {name age} expr] — map destructuring
        ExprKind::Map(pairs) => {
            if let Value::Map(ref map_pairs) = val {
                for (k, _) in pairs {
                    if let ExprKind::Symbol(name) = &k.kind {
                        let key = Value::Keyword(name.clone());
                        let v = map_pairs
                            .iter()
                            .find(|(mk, _)| *mk == key)
                            .map(|(_, mv)| mv.clone())
                            .unwrap_or(Value::Unit);
                        env.set(name.clone(), v);
                    }
                }
            } else {
                return Err(err("map destructuring requires a map"));
            }
        }
        _ => return Err(err("invalid let binding pattern")),
    }

    Ok(val)
}

fn eval_lambda(args: &[Expr], env: &mut Env) -> IResult {
    if args.is_empty() {
        return Err(err("fn requires params"));
    }
    let params = extract_params(&args[0])?;
    let body = args[1..].to_vec();
    Ok(Value::Fn(value::LoonFn {
        name: None,
        clauses: vec![(params, body)],
        captured_env: Some(env.clone()),
    }))
}

fn eval_if(args: &[Expr], env: &mut Env) -> IResult {
    if args.len() < 2 {
        return Err(err("if requires condition and then-branch"));
    }
    let cond = eval(&args[0], env)?;
    if cond.is_truthy() {
        eval(&args[1], env)
    } else if args.len() > 2 {
        eval(&args[2], env)
    } else {
        Ok(Value::Unit)
    }
}

fn eval_do(args: &[Expr], env: &mut Env) -> IResult {
    let mut last = Value::Unit;
    for expr in args {
        last = eval(expr, env)?;
    }
    Ok(last)
}

fn eval_pipe(args: &[Expr], env: &mut Env) -> IResult {
    if args.is_empty() {
        return Err(err("|> requires at least one argument"));
    }
    let mut val = eval(&args[0], env)?;
    for step in &args[1..] {
        // Each step is [fn arg1 arg2...] — insert val as first arg
        match &step.kind {
            ExprKind::List(items) if !items.is_empty() => {
                let func = eval(&items[0], env)?;
                let mut call_args = vec![val];
                for a in &items[1..] {
                    call_args.push(eval(a, env)?);
                }
                val = match func {
                    Value::Fn(lf) => call_fn(&lf, &call_args, env)?,
                    Value::Builtin(name, f) => f(&name, &call_args)?,
                    _ => return Err(err(format!("not callable in pipe: {func}"))),
                };
            }
            ExprKind::Symbol(_) => {
                let func = eval(step, env)?;
                val = match func {
                    Value::Fn(lf) => call_fn(&lf, &[val], env)?,
                    Value::Builtin(name, f) => f(&name, &[val])?,
                    _ => return Err(err(format!("not callable in pipe: {func}"))),
                };
            }
            _ => return Err(err("pipe step must be a list or symbol")),
        }
    }
    Ok(val)
}

fn eval_match(args: &[Expr], env: &mut Env) -> IResult {
    if args.is_empty() {
        return Err(err("match requires a value"));
    }
    let scrutinee = eval(&args[0], env)?;
    let arms = &args[1..];

    let mut i = 0;
    while i < arms.len() {
        let pattern = &arms[i];
        // Check for => separator
        if i + 1 < arms.len() {
            if let ExprKind::Symbol(s) = &arms[i + 1].kind {
                if s == "=>" {
                    if i + 2 >= arms.len() {
                        return Err(err("match arm missing body after =>"));
                    }
                    // Check for guard: pattern [when guard] => body
                    // Simple pattern => body
                    let mut bindings = HashMap::new();
                    if pattern_matches(pattern, &scrutinee, &mut bindings, env)? {
                        env.push_scope();
                        for (k, v) in bindings {
                            env.set(k, v);
                        }
                        let result = eval(&arms[i + 2], env);
                        env.pop_scope();
                        return result;
                    }
                    i += 3;
                    continue;
                }
            }
        }

        // Check for guard pattern: pattern [when guard] => body
        if i + 2 < arms.len() {
            if let ExprKind::List(guard_form) = &arms[i + 1].kind {
                if !guard_form.is_empty() {
                    if let ExprKind::Symbol(s) = &guard_form[0].kind {
                        if s == "when" && i + 3 < arms.len() {
                            if let ExprKind::Symbol(arrow) = &arms[i + 2].kind {
                                if arrow == "=>" && i + 3 < arms.len() {
                                    let mut bindings = HashMap::new();
                                    if pattern_matches(pattern, &scrutinee, &mut bindings, env)? {
                                        env.push_scope();
                                        for (k, v) in &bindings {
                                            env.set(k.clone(), v.clone());
                                        }
                                        // Evaluate guard
                                        let guard_val = eval(&guard_form[1], env)?;
                                        if guard_val.is_truthy() {
                                            let result = eval(&arms[i + 3], env);
                                            env.pop_scope();
                                            return result;
                                        }
                                        env.pop_scope();
                                    }
                                    i += 4;
                                    continue;
                                }
                            }
                        }
                    }
                }
            }
        }

        i += 1;
    }

    Err(err(format!("no match arm matched value: {scrutinee}")))
}

fn pattern_matches(
    pattern: &Expr,
    value: &Value,
    bindings: &mut HashMap<String, Value>,
    env: &mut Env,
) -> Result<bool, InterpError> {
    match &pattern.kind {
        // Wildcard
        ExprKind::Symbol(s) if s == "_" => Ok(true),
        // Literal int
        ExprKind::Int(n) => Ok(value == &Value::Int(*n)),
        // Literal float
        ExprKind::Float(n) => Ok(value == &Value::Float(*n)),
        // Literal bool
        ExprKind::Bool(b) => Ok(value == &Value::Bool(*b)),
        // Literal string
        ExprKind::Str(s) => Ok(value == &Value::Str(s.clone())),
        // Keyword
        ExprKind::Keyword(k) => Ok(value == &Value::Keyword(k.clone())),
        // Variable binding or constructor name
        ExprKind::Symbol(s) => {
            // Check if it's an ADT constructor with no fields
            if let Some(Value::Fn(lf)) = env.get(s) {
                if let Some(ref name) = lf.name {
                    if name.starts_with(char::is_uppercase) && lf.clauses[0].0.is_empty() {
                        // Nullary constructor — match by tag
                        if let Value::Adt(tag, _) = value {
                            return Ok(tag == s);
                        }
                        return Ok(false);
                    }
                }
            }
            // Otherwise it's a variable binding
            bindings.insert(s.clone(), value.clone());
            Ok(true)
        }
        // Constructor pattern: [Some x] or [Circle r]
        ExprKind::List(items) if !items.is_empty() => {
            if let ExprKind::Symbol(ctor) = &items[0].kind {
                if let Value::Adt(tag, fields) = value {
                    if tag == ctor && fields.len() == items.len() - 1 {
                        for (pat, val) in items[1..].iter().zip(fields.iter()) {
                            if !pattern_matches(pat, val, bindings, env)? {
                                return Ok(false);
                            }
                        }
                        return Ok(true);
                    }
                    return Ok(false);
                }
            }
            Ok(false)
        }
        _ => Ok(false),
    }
}

fn eval_mut(args: &[Expr], env: &mut Env) -> IResult {
    // [mut name] — mark as mutable (for now, just a no-op that returns the value)
    if args.is_empty() {
        return Err(err("mut requires an argument"));
    }
    eval(&args[0], env)
}

fn eval_type_def(args: &[Expr], env: &mut Env) -> IResult {
    // [type Name T? Variant1 Variant2 ...]
    if args.is_empty() {
        return Err(err("type requires a name"));
    }
    let _type_name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(err("type name must be a symbol")),
    };

    // Skip type params, register constructors
    for arg in &args[1..] {
        match &arg.kind {
            // [CtorName field1 field2 ...] — constructor with fields
            ExprKind::List(items) if !items.is_empty() => {
                if let ExprKind::Symbol(ctor_name) = &items[0].kind {
                    let arity = items.len() - 1;
                    let ctor_name_clone = ctor_name.clone();
                    let lf = value::LoonFn {
                        name: Some(ctor_name.clone()),
                        clauses: vec![(
                            (0..arity).map(|i| format!("__f{i}")).collect(),
                            vec![],
                        )],
                        captured_env: None,
                    };
                    // Register as a constructor function
                    env.set(
                        ctor_name.clone(),
                        Value::Builtin(
                            ctor_name.clone(),
                            Arc::new(move |_name, args| {
                                Ok(Value::Adt(ctor_name_clone.clone(), args.to_vec()))
                            }),
                        ),
                    );
                    // Also keep the fn form for pattern matching metadata
                    let _ = lf;
                }
            }
            // CtorName — nullary constructor
            ExprKind::Symbol(ctor_name) => {
                if ctor_name.starts_with(char::is_uppercase) {
                    let cn = ctor_name.clone();
                    env.set(
                        ctor_name.clone(),
                        Value::Adt(cn, vec![]),
                    );
                }
                // else it's a type parameter, skip
            }
            _ => {}
        }
    }

    Ok(Value::Unit)
}

/// Evaluate [handle body handler1 handler2 ...]
/// Format: [handle [expr] [Effect.op params] => handler_body ...]
fn eval_handle(args: &[Expr], env: &mut Env) -> IResult {
    if args.is_empty() {
        return Err(err("handle requires a body expression"));
    }

    // First arg is the body expression to evaluate
    let body = &args[0];

    // Collect handlers: [Effect.op params...] => body
    let mut handlers: Vec<(String, String, Vec<String>, &Expr)> = Vec::new();
    let handler_args = &args[1..];
    let mut i = 0;
    while i < handler_args.len() {
        // Pattern: [Effect.op param1 param2...] => body
        if let ExprKind::List(pattern) = &handler_args[i].kind {
            if !pattern.is_empty() {
                if let ExprKind::Symbol(qualified) = &pattern[0].kind {
                    if let Some((effect, op)) = qualified.split_once('.') {
                        // Collect param names
                        let params: Vec<String> = pattern[1..]
                            .iter()
                            .filter_map(|p| {
                                if let ExprKind::Symbol(s) = &p.kind {
                                    Some(s.clone())
                                } else {
                                    None
                                }
                            })
                            .collect();

                        // Skip => and get body
                        if i + 2 < handler_args.len() {
                            if let ExprKind::Symbol(arrow) = &handler_args[i + 1].kind {
                                if arrow == "=>" {
                                    handlers.push((
                                        effect.to_string(),
                                        op.to_string(),
                                        params,
                                        &handler_args[i + 2],
                                    ));
                                    i += 3;
                                    continue;
                                }
                            }
                        }
                    }
                }
            }
        }
        i += 1;
    }

    // Try to evaluate the body
    match eval(body, env) {
        Ok(val) => Ok(val),
        Err(e) => {
            if let Some(ref performed) = e.performed_effect {
                // Look for a matching handler
                for (effect, op, params, handler_body) in &handlers {
                    if performed.effect == *effect && performed.operation == *op {
                        env.push_scope();
                        // Bind handler params to effect args
                        for (pname, pval) in params.iter().zip(performed.args.iter()) {
                            if pname != "_" {
                                env.set(pname.clone(), pval.clone());
                            }
                        }
                        // [resume val] is a builtin in the handler scope
                        // For one-shot continuations, resume just returns the value
                        env.set(
                            "resume".to_string(),
                            Value::Builtin(
                                "resume".to_string(),
                                Arc::new(|_, args: &[Value]| {
                                    Ok(args.first().cloned().unwrap_or(Value::Unit))
                                }),
                            ),
                        );
                        let result = eval(handler_body, env);
                        env.pop_scope();
                        return result;
                    }
                }
                // No handler matched — propagate
                Err(e)
            } else {
                Err(e)
            }
        }
    }
}

fn eval_test_def(args: &[Expr], env: &mut Env) -> IResult {
    // [test defn name [] body...] — register as a test function
    if args.len() >= 2 {
        if let ExprKind::Symbol(s) = &args[0].kind {
            if s == "defn" {
                return eval_defn(&args[1..], env);
            }
        }
    }
    Ok(Value::Unit)
}

fn extract_params(expr: &Expr) -> Result<Vec<String>, InterpError> {
    match &expr.kind {
        ExprKind::List(items) => {
            let mut params = Vec::new();
            for item in items {
                match &item.kind {
                    ExprKind::Symbol(s) => params.push(s.clone()),
                    _ => return Err(err("parameter must be a symbol")),
                }
            }
            Ok(params)
        }
        _ => Err(err("params must be a list")),
    }
}

fn call_fn(lf: &value::LoonFn, args: &[Value], env: &mut Env) -> IResult {
    // Use captured env if present (closures), otherwise use caller's env
    let mut use_env = if let Some(ref captured) = lf.captured_env {
        let mut e = captured.clone();
        // Merge global scope from caller so recursive calls and later defns work
        e.merge_globals(env);
        Some(e)
    } else {
        None
    };
    let env = use_env.as_mut().map_or(env, |e| e);

    // Find matching clause by arity
    for (params, body) in &lf.clauses {
        if params.len() == args.len() {
            env.push_scope();
            for (name, val) in params.iter().zip(args.iter()) {
                env.set(name.clone(), val.clone());
            }
            let mut result = Value::Unit;
            for expr in body {
                result = eval(expr, env)?;
            }
            env.pop_scope();
            return Ok(result);
        }
    }
    Err(err(format!(
        "no matching clause for {} with {} args",
        lf.name.as_deref().unwrap_or("anonymous"),
        args.len()
    )))
}

// ---- Builtins ----

fn register_builtins(env: &mut Env) {
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
        match (&args[0], &args[1]) {
            (Value::Vec(v), Value::Int(i)) => {
                let idx = *i as usize;
                if idx < v.len() {
                    Ok(v[idx].clone())
                } else if args.len() > 2 {
                    Ok(args[2].clone()) // default value
                } else {
                    Err(err(format!("index {i} out of bounds (len {})", v.len())))
                }
            }
            _ => Err(err("nth requires a vector and index")),
        }
    });

    builtin!(env, "map", |_, args: &[Value]| {
        match (&args[0], args.get(1)) {
            (Value::Vec(v), Some(func)) => {
                let mut result = Vec::new();
                for item in v {
                    let val = apply_value(func, &[item.clone()])?;
                    result.push(val);
                }
                Ok(Value::Vec(result))
            }
            // map with function first, collection second (for pipe)
            (func, Some(Value::Vec(v))) if func.is_callable() => {
                let mut result = Vec::new();
                for item in v {
                    let val = apply_value(func, &[item.clone()])?;
                    result.push(val);
                }
                Ok(Value::Vec(result))
            }
            // Partial: [map f] returns a function that maps f over a collection
            (func, None) if func.is_callable() => {
                let func_clone = func.clone();
                Ok(Value::Builtin(
                    "map-partial".to_string(),
                    Arc::new(move |_, inner_args: &[Value]| {
                        if let Value::Vec(v) = &inner_args[0] {
                            let mut result = Vec::new();
                            for item in v {
                                let val = apply_value(&func_clone, &[item.clone()])?;
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
                    let val = apply_value(func, &[item.clone()])?;
                    if val.is_truthy() {
                        result.push(item.clone());
                    }
                }
                Ok(Value::Vec(result))
            }
            (func, Some(Value::Vec(v))) if func.is_callable() => {
                let mut result = Vec::new();
                for item in v {
                    let val = apply_value(func, &[item.clone()])?;
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
                                let val = apply_value(&func_clone, &[item.clone()])?;
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
        match args {
            [Value::Vec(v), init, func] => {
                let mut acc = init.clone();
                for item in v {
                    acc = apply_value(func, &[acc, item.clone()])?;
                }
                Ok(acc)
            }
            [init, func] => {
                let init_clone = init.clone();
                let func_clone = func.clone();
                Ok(Value::Builtin(
                    "fold-partial".to_string(),
                    Arc::new(move |_, inner_args: &[Value]| {
                        if let Value::Vec(v) = &inner_args[0] {
                            let mut acc = init_clone.clone();
                            for item in v {
                                acc = apply_value(&func_clone, &[acc, item.clone()])?;
                            }
                            Ok(acc)
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
            // Replace or add
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

    builtin!(env, "sort-by", |_, args: &[Value]| {
        match args {
            [func, order] => {
                let func_clone = func.clone();
                let desc = matches!(order, Value::Keyword(k) if k == "desc");
                Ok(Value::Builtin(
                    "sort-by-partial".to_string(),
                    Arc::new(move |_, inner_args: &[Value]| {
                        if let Value::Vec(v) = &inner_args[0] {
                            let mut sorted = v.clone();
                            sorted.sort_by(|a, b| {
                                let ka = apply_value(&func_clone, &[a.clone()]).unwrap_or(Value::Int(0));
                                let kb = apply_value(&func_clone, &[b.clone()]).unwrap_or(Value::Int(0));
                                let ord = value_cmp(&ka, &kb);
                                if desc { ord.reverse() } else { ord }
                            });
                            Ok(Value::Vec(sorted))
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

    builtin!(env, "each", |_, args: &[Value]| {
        match (&args[0], args.get(1)) {
            (Value::Vec(v), Some(func)) => {
                for item in v {
                    apply_value(func, &[item.clone()])?;
                }
                Ok(Value::Unit)
            }
            (func, Some(Value::Vec(v))) if func.is_callable() => {
                for item in v {
                    apply_value(func, &[item.clone()])?;
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
                                apply_value(&func_clone, &[item.clone()])?;
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
        // Identity for vectors — they're already collected
        Ok(args[0].clone())
    });

    builtin!(env, "push!", |_, args: &[Value]| {
        // In interpreter, we can't truly mutate, so return a new vec
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
}

fn apply_value(func: &Value, args: &[Value]) -> IResult {
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

fn value_cmp(a: &Value, b: &Value) -> std::cmp::Ordering {
    match (a, b) {
        (Value::Int(a), Value::Int(b)) => a.cmp(b),
        (Value::Float(a), Value::Float(b)) => a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal),
        (Value::Str(a), Value::Str(b)) => a.cmp(b),
        _ => std::cmp::Ordering::Equal,
    }
}
