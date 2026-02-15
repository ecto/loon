pub mod builtins;
mod env;
mod value;

pub use env::Env;
pub use value::Value;

use crate::ast::{Expr, ExprKind};
use crate::module::ModuleCache;
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::Path;
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

pub(crate) fn err(msg: impl Into<String>) -> InterpError {
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
    eval_program_with_base_dir(exprs, None)
}

pub fn eval_program_with_base_dir(exprs: &[Expr], base_dir: Option<&Path>) -> IResult {
    let mut env = Env::new();
    register_builtins(&mut env);
    // Load prelude (Option, Result types)
    if let Ok(prelude_exprs) = crate::parser::parse(crate::prelude::PRELUDE) {
        for expr in &prelude_exprs {
            let _ = eval(expr, &mut env);
        }
    }
    let mut cache = ModuleCache::new();
    let default_base = std::path::PathBuf::from(".");
    let base = base_dir.unwrap_or(&default_base);

    let mut last = Value::Unit;
    for expr in exprs {
        // Intercept [use ...] at the top level
        if let ExprKind::List(items) = &expr.kind {
            if !items.is_empty() {
                if let ExprKind::Symbol(s) = &items[0].kind {
                    if s == "use" {
                        eval_use_with_cache(&items[1..], &mut env, base, &mut cache)?;
                        sync_global_env(&env);
                        continue;
                    }
                }
            }
        }
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

pub(crate) fn get_global_env() -> Option<Env> {
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
                        // [pub defn name ...] — eval the inner form and mark as pub
                        if items.len() > 1 {
                            // Extract the name being defined (for pub tracking)
                            if items.len() > 2 {
                                if let ExprKind::Symbol(ref defn_kind) = items[1].kind {
                                    if defn_kind == "defn" || defn_kind == "let" {
                                        if let ExprKind::Symbol(ref name) = items[2].kind {
                                            env.pub_names.insert(name.clone());
                                        }
                                    }
                                }
                            }
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

    // Use extract_param + bind_param for uniform destructuring
    let param = extract_param(binding)?;
    bind_param(&param, &val, env)?;

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
        match &step.kind {
            ExprKind::List(items) if !items.is_empty() => {
                let func = eval(&items[0], env)?;
                let mut explicit_args = Vec::new();
                for a in &items[1..] {
                    explicit_args.push(eval(a, env)?);
                }

                // If there are explicit args, append piped value as last arg
                // (thread-last semantics: [|> coll [f x]] → [f x coll]).
                // If no explicit args, pass piped value as sole arg.
                let call_args = if explicit_args.is_empty() {
                    vec![val]
                } else {
                    let mut args = explicit_args;
                    args.push(val);
                    args
                };

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
                            (0..arity).map(|i| value::Param::Simple(format!("__f{i}"))).collect(),
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

fn extract_params(expr: &Expr) -> Result<Vec<value::Param>, InterpError> {
    match &expr.kind {
        ExprKind::List(items) => {
            let mut params = Vec::new();
            for item in items {
                params.push(extract_param(item)?);
            }
            Ok(params)
        }
        _ => Err(err("params must be a list")),
    }
}

fn extract_param(expr: &Expr) -> Result<value::Param, InterpError> {
    match &expr.kind {
        ExprKind::Symbol(s) => Ok(value::Param::Simple(s.clone())),
        ExprKind::List(items) => {
            let mut inner = Vec::new();
            for item in items {
                inner.push(extract_param(item)?);
            }
            Ok(value::Param::VecDestructure(inner))
        }
        ExprKind::Map(pairs) => {
            let mut names = Vec::new();
            for (k, _) in pairs {
                if let ExprKind::Symbol(s) = &k.kind {
                    names.push(s.clone());
                }
            }
            Ok(value::Param::MapDestructure(names))
        }
        _ => Err(err("parameter must be a symbol or destructuring pattern")),
    }
}

fn bind_param(param: &value::Param, val: &Value, env: &mut Env) -> Result<(), InterpError> {
    match param {
        value::Param::Simple(name) => {
            if name != "_" {
                env.set(name.clone(), val.clone());
            }
            Ok(())
        }
        value::Param::VecDestructure(inner) => {
            let items = match val {
                Value::Vec(v) => v,
                Value::Tuple(v) => v,
                _ => return Err(err("destructuring requires a vector or tuple")),
            };
            for (i, p) in inner.iter().enumerate() {
                let v = items.get(i).cloned().unwrap_or(Value::Unit);
                bind_param(p, &v, env)?;
            }
            Ok(())
        }
        value::Param::MapDestructure(names) => {
            let pairs = match val {
                Value::Map(m) => m,
                _ => return Err(err("map destructuring requires a map")),
            };
            for name in names {
                let key = Value::Keyword(name.clone());
                let v = pairs
                    .iter()
                    .find(|(k, _)| *k == key)
                    .map(|(_, v)| v.clone())
                    .unwrap_or(Value::Unit);
                env.set(name.clone(), v);
            }
            Ok(())
        }
    }
}

pub(crate) fn call_fn(lf: &value::LoonFn, args: &[Value], env: &mut Env) -> IResult {
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
            for (param, val) in params.iter().zip(args.iter()) {
                bind_param(param, val, env)?;
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

/// Evaluate [use module.path] — load and import a module.
/// Supports:
///   [use http.server]              — import all as http.server.name
///   [use http.server :as http]     — import all as http.name
///   [use http.server {add sub}]    — import specific names directly
pub fn eval_use_with_cache(
    args: &[Expr],
    env: &mut Env,
    base_dir: &Path,
    cache: &mut ModuleCache,
) -> IResult {
    if args.is_empty() {
        return Err(err("use requires a module path"));
    }

    let module_path = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(err("use module path must be a symbol")),
    };

    let exports = cache
        .load_module(&module_path, base_dir)
        .map_err(err)?;

    // Determine import style
    if args.len() >= 3 {
        if let ExprKind::Keyword(k) = &args[1].kind {
            if k == "as" {
                // [use mod.path :as alias]
                let alias = match &args[2].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(err("alias must be a symbol")),
                };
                for (name, val) in &exports.values {
                    env.set_global(format!("{alias}.{name}"), val.clone());
                }
                return Ok(Value::Unit);
            }
        }
    }

    if args.len() >= 2 {
        if let ExprKind::Map(pairs) = &args[1].kind {
            // [use mod.path {name1 name2}] — selective import
            // Note: the parser sees {name1 name2} as a map with pairs,
            // but we just want the keys as imported names
            for (k, _) in pairs {
                if let ExprKind::Symbol(name) = &k.kind {
                    if let Some(val) = exports.values.get(name) {
                        env.set_global(name.clone(), val.clone());
                    } else {
                        return Err(err(format!(
                            "module '{module_path}' does not export '{name}'"
                        )));
                    }
                }
            }
            return Ok(Value::Unit);
        }

        // Check for vec-style selective import [use mod.path [name1 name2]]
        if let ExprKind::Vec(items) | ExprKind::List(items) = &args[1].kind {
            for item in items {
                if let ExprKind::Symbol(name) = &item.kind {
                    if let Some(val) = exports.values.get(name) {
                        env.set_global(name.clone(), val.clone());
                    } else {
                        return Err(err(format!(
                            "module '{module_path}' does not export '{name}'"
                        )));
                    }
                }
            }
            return Ok(Value::Unit);
        }
    }

    // Default: import all as module_path.name
    for (name, val) in &exports.values {
        env.set_global(format!("{module_path}.{name}"), val.clone());
    }
    Ok(Value::Unit)
}

fn register_builtins(env: &mut Env) {
    builtins::register_builtins(env);
}
