pub mod builtins;
pub mod dom_builtins;
mod env;
pub mod html_bridge;
pub mod net;
mod value;

pub use env::Env;
pub use value::Value;

use crate::ast::{Expr, ExprKind};
use crate::module::ModuleCache;
use crate::syntax::Span;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::path::Path;
use std::sync::Arc;

thread_local! {
    static GLOBAL_ENV: RefCell<Option<std::rc::Rc<std::cell::RefCell<HashMap<String, value::Value>>>>> = const { RefCell::new(None) };
    static CALL_STACK: RefCell<Vec<StackFrame>> = RefCell::new(Vec::new());
    static SOURCE_TEXT: RefCell<Option<String>> = const { RefCell::new(None) };
    static EFFECT_LOG_ENABLED: Cell<bool> = const { Cell::new(false) };
    static EFFECT_LOG: RefCell<Vec<EffectEntry>> = RefCell::new(Vec::new());
}

#[derive(Debug, Clone)]
pub struct StackFrame {
    pub fn_name: String,
    pub call_site: Span,
}

#[derive(Debug, Clone)]
pub struct EffectEntry {
    pub effect: String,
    pub operation: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct InterpError {
    pub message: String,
    pub span: Option<Span>,
    pub stack: Vec<StackFrame>,
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

pub fn err(msg: impl Into<String>) -> InterpError {
    InterpError {
        message: msg.into(),
        span: None,
        stack: CALL_STACK.with(|s| s.borrow().clone()),
        performed_effect: None,
    }
}

pub fn err_at(msg: impl Into<String>, span: Span) -> InterpError {
    let mut e = err(msg);
    e.span = Some(span);
    e
}

/// Try to handle an effect with a built-in handler (for IO at the top level).
fn try_builtin_handler(performed: &PerformedEffect) -> Option<IResult> {
    match (performed.effect.as_str(), performed.operation.as_str()) {
        ("IO", "println") => {
            let parts: Vec<String> = performed.args.iter().map(|v| v.display_str()).collect();
            println!("{}", parts.join(" "));
            Some(Ok(Value::Unit))
        }
        ("IO", "read-file") => {
            if let Some(Value::Str(path)) = performed.args.first() {
                match std::fs::read_to_string(path) {
                    Ok(contents) => Some(Ok(Value::Str(contents))),
                    Err(e) => Some(Err(perform_effect(
                        "Fail",
                        "fail",
                        vec![Value::Str(e.to_string())],
                    ))),
                }
            } else {
                Some(Err(err("IO.read-file requires a string path")))
            }
        }
        ("IO", "write-file") => {
            if let (Some(Value::Str(path)), Some(contents)) =
                (performed.args.first(), performed.args.get(1))
            {
                match std::fs::write(path, contents.display_str()) {
                    Ok(()) => Some(Ok(Value::Unit)),
                    Err(e) => Some(Err(perform_effect(
                        "Fail",
                        "fail",
                        vec![Value::Str(e.to_string())],
                    ))),
                }
            } else {
                Some(Err(err("IO.write-file requires a path and contents")))
            }
        }
        ("IO", "parse-json") => {
            if let Some(Value::Str(text)) = performed.args.first() {
                match serde_json::from_str::<serde_json::Value>(text) {
                    Ok(val) => Some(Ok(Value::Json(std::sync::Arc::new(val)))),
                    Err(e) => Some(Err(perform_effect(
                        "Fail",
                        "fail",
                        vec![Value::Str(e.to_string())],
                    ))),
                }
            } else {
                Some(Err(err("IO.parse-json requires a string argument")))
            }
        }
        ("Process", "args") => {
            let args: Vec<Value> = std::env::args().map(Value::Str).collect();
            Some(Ok(Value::Vec(args)))
        }
        ("Process", "env") => {
            if let Some(Value::Str(key)) = performed.args.first() {
                match std::env::var(key) {
                    Ok(val) => Some(Ok(Value::Adt("Some".to_string(), vec![Value::Str(val)]))),
                    Err(_) => Some(Ok(Value::Adt("None".to_string(), vec![]))),
                }
            } else {
                Some(Err(err("Process.env requires a string key")))
            }
        }
        ("Process", "exit") => {
            if let Some(Value::Int(code)) = performed.args.first() {
                std::process::exit(*code as i32);
            } else {
                std::process::exit(0);
            }
        }
        ("Async", "spawn") => {
            // Spawn a real thread to evaluate the thunk.
            // Returns a Future backed by a shared slot.
            if let Some(thunk) = performed.args.first() {
                let slot: std::sync::Arc<(std::sync::Mutex<Option<Value>>, std::sync::Condvar)> =
                    std::sync::Arc::new((std::sync::Mutex::new(None), std::sync::Condvar::new()));
                let slot2 = slot.clone();

                match thunk {
                    Value::Fn(lf) => {
                        let lf = lf.deep_clone();
                        let env = get_global_env().unwrap_or_default().deep_clone();
                        std::thread::Builder::new()
                            .stack_size(64 * 1024 * 1024) // 64MB stack for deep recursion
                            .spawn(move || {
                                let mut env = env;
                                sync_global_env(&env);
                                register_builtins(&mut env);
                                let result = eval_spawned_fn(&lf, &mut env);
                                let (lock, cvar) = &*slot2;
                                *lock.lock().unwrap() = Some(result);
                                cvar.notify_one();
                            })
                            .expect("failed to spawn thread");
                        Some(Ok(Value::AsyncSlot(slot)))
                    }
                    Value::Builtin(name, f) => {
                        let name = name.clone();
                        let f = f.clone();
                        std::thread::spawn(move || {
                            let result = match f(&name, &[]) {
                                Ok(val) => val,
                                Err(e) => {
                                    eprintln!("[Async.spawn] thread error: {}", e.message);
                                    Value::Unit
                                }
                            };
                            let (lock, cvar) = &*slot2;
                            *lock.lock().unwrap() = Some(result);
                            cvar.notify_one();
                        });
                        Some(Ok(Value::AsyncSlot(slot)))
                    }
                    _ => Some(Err(err("Async.spawn requires a callable thunk"))),
                }
            } else {
                Some(Err(err("Async.spawn requires a thunk argument")))
            }
        }
        ("Async", "await") => {
            match performed.args.first() {
                Some(Value::Future(inner)) => Some(Ok(*inner.clone())),
                Some(Value::AsyncSlot(slot)) => {
                    let (lock, cvar) = &**slot;
                    let mut guard = lock.lock().unwrap();
                    while guard.is_none() {
                        guard = cvar.wait(guard).unwrap();
                    }
                    Some(Ok(guard.take().unwrap()))
                }
                _ => Some(Err(err("Async.await requires a Future value"))),
            }
        }
        ("Async", "sleep") => {
            // Mock: no-op, just return Unit
            Some(Ok(Value::Unit))
        }
        ("Async", "loop") => {
            // Stateful infinite loop without stack growth.
            // [Async.loop init-state step-fn] — calls [step-fn state] repeatedly,
            // using the return value as the next state.
            if performed.args.len() < 2 {
                return Some(Err(err("Async.loop requires init-state and step-fn")));
            }
            let mut state = performed.args[0].clone();
            let step = &performed.args[1];
            loop {
                let result = match step {
                    Value::Fn(lf) => {
                        let mut env = get_global_env().unwrap_or_default();
                        call_fn(lf, &[state.clone()], &mut env, Span::ZERO)
                    }
                    Value::Builtin(name, f) => f(name, &[state.clone()]),
                    _ => break Some(Err(err("Async.loop step must be callable"))),
                };
                match result {
                    Ok(new_state) => state = new_state,
                    Err(e) => {
                        if let Some(ref performed) = e.performed_effect {
                            if let Some(inner) = try_builtin_handler(performed) {
                                match inner {
                                    Ok(new_state) => state = new_state,
                                    Err(e2) => break Some(Err(e2)),
                                }
                            } else {
                                break Some(Err(e));
                            }
                        } else {
                            break Some(Err(e));
                        }
                    }
                }
            }
        }
        ("IO", "list-dir") => {
            if let Some(Value::Str(path)) = performed.args.first() {
                match std::fs::read_dir(path) {
                    Ok(entries) => {
                        let names: Vec<Value> = entries
                            .filter_map(|e| e.ok())
                            .map(|e| Value::Str(e.file_name().to_string_lossy().into_owned()))
                            .collect();
                        Some(Ok(Value::Vec(names)))
                    }
                    // Not a directory (or doesn't exist) → return empty vec
                    Err(_) => Some(Ok(Value::Vec(vec![]))),
                }
            } else {
                Some(Err(err("IO.list-dir requires a string path")))
            }
        }
        ("IO", "mtime") => {
            if let Some(Value::Str(path)) = performed.args.first() {
                match std::fs::metadata(path).and_then(|m| m.modified()) {
                    Ok(time) => {
                        let millis = time
                            .duration_since(std::time::UNIX_EPOCH)
                            .unwrap_or_default()
                            .as_millis() as i64;
                        Some(Ok(Value::Int(millis)))
                    }
                    Err(e) => Some(Err(perform_effect(
                        "Fail",
                        "fail",
                        vec![Value::Str(e.to_string())],
                    ))),
                }
            } else {
                Some(Err(err("IO.mtime requires a string path")))
            }
        }
        ("IO", "millis") => {
            let millis = std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .as_millis() as i64;
            Some(Ok(Value::Int(millis)))
        }
        ("IO", "sleep") => {
            if let Some(Value::Int(ms)) = performed.args.first() {
                std::thread::sleep(std::time::Duration::from_millis(*ms as u64));
                Some(Ok(Value::Unit))
            } else {
                Some(Err(err("IO.sleep requires an integer (milliseconds)")))
            }
        }
        ("IO", "copy-file") => {
            if let (Some(Value::Str(src)), Some(Value::Str(dst))) =
                (performed.args.first(), performed.args.get(1))
            {
                match std::fs::copy(src, dst) {
                    Ok(_) => Some(Ok(Value::Unit)),
                    Err(e) => Some(Err(perform_effect(
                        "Fail",
                        "fail",
                        vec![Value::Str(e.to_string())],
                    ))),
                }
            } else {
                Some(Err(err("IO.copy-file requires two string paths")))
            }
        }
        ("IO", "mkdir") => {
            if let Some(Value::Str(path)) = performed.args.first() {
                match std::fs::create_dir_all(path) {
                    Ok(()) => Some(Ok(Value::Unit)),
                    Err(e) => Some(Err(perform_effect(
                        "Fail",
                        "fail",
                        vec![Value::Str(e.to_string())],
                    ))),
                }
            } else {
                Some(Err(err("IO.mkdir requires a string path")))
            }
        }
        ("IO", "read-line") => {
            let mut line = String::new();
            match std::io::stdin().read_line(&mut line) {
                Ok(_) => {
                    // Trim trailing newline
                    if line.ends_with('\n') {
                        line.pop();
                        if line.ends_with('\r') {
                            line.pop();
                        }
                    }
                    Some(Ok(Value::Str(line)))
                }
                Err(e) => Some(Err(perform_effect(
                    "Fail",
                    "fail",
                    vec![Value::Str(e.to_string())],
                ))),
            }
        }
        _ => {
            // Try Net.* effects
            if performed.effect == "Net" {
                return net::try_net_handler(performed);
            }
            None
        }
    }
}

fn perform_effect(effect: &str, op: &str, args: Vec<Value>) -> InterpError {
    InterpError {
        message: format!("unhandled effect: {effect}.{op}"),
        span: None,
        stack: CALL_STACK.with(|s| s.borrow().clone()),
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

pub fn set_source_text(source: &str) {
    SOURCE_TEXT.with(|s| *s.borrow_mut() = Some(source.to_string()));
}

pub fn enable_effect_log(enabled: bool) {
    EFFECT_LOG_ENABLED.with(|e| e.set(enabled));
}

pub fn get_effect_log() -> Vec<EffectEntry> {
    EFFECT_LOG.with(|l| l.borrow().clone())
}

pub fn clear_effect_log() {
    EFFECT_LOG.with(|l| l.borrow_mut().clear());
}

pub fn eval_program(exprs: &[Expr]) -> IResult {
    eval_program_with_base_dir(exprs, None)
}

pub fn eval_program_with_base_dir(exprs: &[Expr], base_dir: Option<&Path>) -> IResult {
    // Macro expansion phase
    let mut expander = crate::macros::MacroExpander::new();
    let exprs = expander
        .expand_program(exprs)
        .map_err(err)?;

    let mut env = Env::new();
    register_builtins(&mut env);
    // Load prelude (Option, Result types)
    if let Ok(prelude_exprs) = crate::parser::parse(crate::prelude::PRELUDE) {
        for expr in &prelude_exprs {
            let _ = eval(expr, &mut env);
        }
    }
    let default_base = std::path::PathBuf::from(".");
    let base = base_dir.unwrap_or(&default_base);
    let mut cache = match crate::pkg::Manifest::load(base) {
        Ok(Some(manifest)) => {
            let lockfile = crate::pkg::lockfile::Lockfile::load(base).ok().flatten();
            ModuleCache::with_manifest_and_lockfile(manifest, lockfile, base.to_path_buf())
        }
        _ => ModuleCache::new(),
    };
    // Initial sync so spawned threads/callbacks can access the global env
    sync_global_env(&env);

    let mut last = Value::Unit;
    for expr in &exprs {
        // Intercept [use ...] at the top level
        if let ExprKind::List(items) = &expr.kind {
            if !items.is_empty() {
                if let ExprKind::Symbol(s) = &items[0].kind {
                    if s == "use" {
                        eval_use_with_cache(&items[1..], &mut env, base, &mut cache)?;
                        continue;
                    }
                }
            }
        }
        match eval(expr, &mut env) {
            Ok(val) => last = val,
            Err(e) => {
                if let Some(ref performed) = e.performed_effect {
                    if let Some(result) = try_builtin_handler(performed) {
                        last = result?;
                    } else {
                        return Err(e);
                    }
                } else {
                    return Err(e);
                }
            }
        }
    }
    // Sync global env once before calling main (needed for apply_value in callbacks)
    sync_global_env(&env);
    // If there's a main function, call it
    if let Some(Value::Fn(main_fn)) = env.get("main") {
        match call_fn(&main_fn, &[], &mut env, Span::ZERO) {
            Ok(val) => return Ok(val),
            Err(e) => {
                if let Some(ref performed) = e.performed_effect {
                    if let Some(result) = try_builtin_handler(performed) {
                        return result;
                    }
                }
                return Err(e);
            }
        }
    }
    Ok(last)
}

fn sync_global_env(env: &Env) {
    GLOBAL_ENV.with(|g| {
        *g.borrow_mut() = Some(env.global_rc().clone());
    });
}

pub(crate) fn get_global_env() -> Option<Env> {
    GLOBAL_ENV.with(|g| {
        g.borrow().as_ref().map(|global_rc| {
            Env::from_global_rc(global_rc.clone())
        })
    })
}

pub fn eval(expr: &Expr, env: &mut Env) -> IResult {
    match &expr.kind {
        ExprKind::Int(n) => Ok(Value::Int(*n)),
        ExprKind::Float(n) => Ok(Value::Float(*n)),
        ExprKind::Bool(b) => Ok(Value::Bool(*b)),
        ExprKind::Str(s) => Ok(Value::Str(s.clone())),
        ExprKind::Keyword(k) => Ok(Value::Keyword(k.clone())),
        ExprKind::Symbol(s) => {
            if let Some(v) = env.get(s) {
                return Ok(v);
            }
            // Try field access: var.field
            if let Some((var, field)) = s.split_once('.') {
                if let Some(val) = env.get(var) {
                    if let Value::Adt(tag, fields) = &val {
                        let found = ADT_FIELDS.with(|f| {
                            let map = f.borrow();
                            if let Some(names) = map.get(tag.as_str()) {
                                if let Some(idx) = names.iter().position(|n| n == field) {
                                    return fields.get(idx).cloned();
                                }
                            }
                            None
                        });
                        if let Some(field_val) = found {
                            return Ok(field_val);
                        }
                    }
                }
            }
            Err(err_at(format!("unbound symbol '{s}'"), expr.span))
        }

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

        // Quasiquote nodes should have been expanded by the macro expander
        ExprKind::Quote(_) | ExprKind::Unquote(_) | ExprKind::UnquoteSplice(_) => {
            Err(err("unexpected quasiquote outside of macro definition"))
        }

        ExprKind::List(items) if items.is_empty() => Ok(Value::Unit),

        ExprKind::List(items) => {
            let head = &items[0];
            // Check for special forms
            if let ExprKind::Symbol(s) = &head.kind {
                match s.as_str() {
                    "fn" => return eval_fn(&items[1..], env),
                    "let" => return eval_let(&items[1..], env),
                    "if" => return eval_if(&items[1..], env),
                    "do" => return eval_do(&items[1..], env),
                    "match" => return eval_match(&items[1..], env),
                    "pipe" => return eval_pipe(&items[1..], env),
                    "mut" => return eval_mut(&items[1..], env),
                    "type" => return eval_type_def(&items[1..], env),
                    "test" => return eval_test_def(&items[1..], env),
                    "effect" => return Ok(Value::Unit), // effect declarations are compile-time
                    "trait" => return Ok(Value::Unit), // trait declarations are compile-time
                    "sig" => return Ok(Value::Unit),   // sig assertions are compile-time
                    "derive" => {
                        // [derive Copy [type Name ...]] — evaluate the inner type form
                        if items.len() >= 3 {
                            return eval(&items[2], env);
                        }
                        return Ok(Value::Unit);
                    }
                    "catch-errors" => {
                        // [catch-errors "source code"] — parse+check, return errors as data
                        if items.len() >= 2 {
                            let src_val = eval(&items[1], env)?;
                            let src = match &src_val {
                                Value::Str(s) => s.clone(),
                                _ => return Err(err("catch-errors requires a string argument")),
                            };
                            return Ok(eval_catch_errors(&src));
                        }
                        return Ok(Value::Vec(vec![]));
                    }
                    "impl" => return eval_impl_def(&items[1..], env),
                    "macro" | "macro+" => return Ok(Value::Unit), // macro defs are compile-time
                    "macroexpand" => return Ok(Value::Unit), // no-op at runtime
                    "inspect" => {
                        if items.len() >= 2 {
                            let expr_to_inspect = &items[1];
                            let val = eval(expr_to_inspect, env)?;
                            let source_text = SOURCE_TEXT.with(|s| {
                                s.borrow().as_ref().and_then(|src| {
                                    let sp = expr_to_inspect.span;
                                    if sp.end <= src.len() {
                                        Some(src[sp.start..sp.end].to_string())
                                    } else {
                                        None
                                    }
                                })
                            });
                            let label = source_text.unwrap_or_else(|| format!("{val}"));
                            println!("[inspect] {} = {}", label, val);
                            return Ok(val);
                        }
                        return Ok(Value::Unit);
                    }
                    "handle" => return eval_handle(&items[1..], env),
                    "try" => return eval_try(&items[1..], env),
                    "pub" => {
                        // [pub fn name ...] — eval the inner form and mark as pub
                        if items.len() > 1 {
                            // Extract the name being defined (for pub tracking)
                            if items.len() > 2 {
                                if let ExprKind::Symbol(ref defn_kind) = items[1].kind {
                                    if defn_kind == "fn" || defn_kind == "let" {
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
                        // Check for override (from resumable handle)
                        if let Some(override_val) = check_effect_override(effect, op) {
                            // Still evaluate args for side effects, but return the override
                            for e in &items[1..] {
                                let _ = eval(e, env);
                            }
                            return Ok(override_val);
                        }
                        let args: Result<Vec<_>, _> =
                            items[1..].iter().map(|e| eval(e, env)).collect();
                        let args = args?;
                        // Log effect if enabled
                        if EFFECT_LOG_ENABLED.with(|e| e.get()) {
                            EFFECT_LOG.with(|l| l.borrow_mut().push(EffectEntry {
                                effect: effect.to_string(),
                                operation: op.to_string(),
                                span: expr.span,
                            }));
                        }
                        // If not inside a handle block, try built-in handler directly
                        if !INSIDE_HANDLE.with(|h| *h.borrow()) {
                            let performed = PerformedEffect {
                                effect: effect.to_string(),
                                operation: op.to_string(),
                                args: args.clone(),
                            };
                            if let Some(result) = try_builtin_handler(&performed) {
                                return result;
                            }
                        }
                        return Err(perform_effect(effect, op, args));
                    }
                }
            }

            // Function call
            let func = eval(head, env)?;
            let args: Result<Vec<_>, _> = items[1..].iter().map(|e| eval(e, env)).collect();
            let args = args?;

            match func {
                Value::Fn(lf) => call_fn(&lf, &args, env, expr.span),
                Value::Builtin(name, f) => {
                    CALL_STACK.with(|s| s.borrow_mut().push(StackFrame {
                        fn_name: format!("<builtin: {name}>"),
                        call_site: expr.span,
                    }));
                    let result = f(&name, &args);
                    CALL_STACK.with(|s| s.borrow_mut().pop());
                    result
                }
                _ => Err(err_at(format!("not callable: {func}"), head.span)),
            }
        }
    }
}

fn eval_fn(args: &[Expr], env: &mut Env) -> IResult {
    if args.is_empty() {
        return Err(err("fn requires params or a name"));
    }

    // If first arg is a symbol (not a vec/tuple), treat as named function
    if let ExprKind::Symbol(name) = &args[0].kind {
        // Named function: [fn name [params] body...] or [fn name (clause) (clause) ...]
        let name = name.clone();
        if args.len() < 2 {
            return Err(err("fn requires a name and body"));
        }

        // Check for multi-arity: [fn name (params body) (params body) ...]
        if matches!(args[1].kind, ExprKind::Tuple(_)) {
            let mut clauses = Vec::new();
            for clause_expr in &args[1..] {
                if let ExprKind::Tuple(clause_items) = &clause_expr.kind {
                    if clause_items.len() < 2 {
                        return Err(err("multi-arity clause needs params and body"));
                    }
                    let params = extract_params(&clause_items[0])?;
                    let body: std::rc::Rc<[Expr]> = clause_items[1..].to_vec().into();
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

        // Single-arity: [fn name [params] body...]
        let params = extract_params(&args[1])?;
        // Skip effect annotation: #{effects}
        let mut body_start = 2;
        if body_start < args.len() {
            if matches!(&args[body_start].kind, ExprKind::Set(_)) {
                body_start += 1; // skip effect set
            }
        }
        let body: std::rc::Rc<[Expr]> = args[body_start..].to_vec().into();

        let lf = value::LoonFn {
            name: Some(name.clone()),
            clauses: vec![(params, body)],
            captured_env: None,
        };
        env.set_global(name, Value::Fn(lf));
        return Ok(Value::Unit);
    }

    // Anonymous lambda: [fn [params] body...]
    let params = extract_params(&args[0])?;
    let body: std::rc::Rc<[Expr]> = args[1..].to_vec().into();
    Ok(Value::Fn(value::LoonFn {
        name: None,
        clauses: vec![(params, body)],
        captured_env: Some(env.clone()),
    }))
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
        return Err(err("pipe requires at least one argument"));
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
                // (thread-last semantics: [pipe coll [f x]] → [f x coll]).
                // If no explicit args, pass piped value as sole arg.
                let call_args = if explicit_args.is_empty() {
                    vec![val]
                } else {
                    let mut args = explicit_args;
                    args.push(val);
                    args
                };

                val = match func {
                    Value::Fn(lf) => call_fn(&lf, &call_args, env, step.span)?,
                    Value::Builtin(name, f) => f(&name, &call_args)?,
                    _ => return Err(err(format!("not callable in pipe: {func}"))),
                };
            }
            ExprKind::Symbol(_) => {
                let func = eval(step, env)?;
                val = match func {
                    Value::Fn(lf) => call_fn(&lf, &[val], env, step.span)?,
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

        // Guard: pattern [when guard] body → i += 3
        if i + 2 < arms.len() {
            if let ExprKind::List(guard_form) = &arms[i + 1].kind {
                if !guard_form.is_empty() {
                    if let ExprKind::Symbol(s) = &guard_form[0].kind {
                        if s == "when" {
                            let mut bindings = HashMap::new();
                            if pattern_matches(pattern, &scrutinee, &mut bindings, env)? {
                                env.push_scope();
                                for (k, v) in &bindings {
                                    env.set(k.clone(), v.clone());
                                }
                                let guard_val = eval(&guard_form[1], env)?;
                                if guard_val.is_truthy() {
                                    let result = eval(&arms[i + 2], env);
                                    env.pop_scope();
                                    return result;
                                }
                                env.pop_scope();
                            }
                            i += 3;
                            continue;
                        }
                    }
                }
            }
        }

        // Simple: pattern body → i += 2
        if i + 1 < arms.len() {
            let mut bindings = HashMap::new();
            if pattern_matches(pattern, &scrutinee, &mut bindings, env)? {
                env.push_scope();
                for (k, v) in bindings {
                    env.set(k, v);
                }
                let result = eval(&arms[i + 1], env);
                env.pop_scope();
                return result;
            }
            i += 2;
            continue;
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
            // Check if it's a nullary ADT constructor
            if let Some(val) = env.get(s) {
                match val {
                    // Nullary constructor stored directly as Adt value
                    Value::Adt(ref tag, ref fields) if fields.is_empty() && tag == s => {
                        if let Value::Adt(val_tag, _) = value {
                            return Ok(val_tag == s);
                        }
                        return Ok(false);
                    }
                    // Nullary constructor stored as Fn
                    Value::Fn(ref lf) => {
                        if let Some(ref name) = lf.name {
                            if name.starts_with(char::is_uppercase) && lf.clauses[0].0.is_empty() {
                                if let Value::Adt(tag, _) = value {
                                    return Ok(tag == s);
                                }
                                return Ok(false);
                            }
                        }
                    }
                    _ => {}
                }
            }
            // Otherwise it's a variable binding
            bindings.insert(s.clone(), value.clone());
            Ok(true)
        }
        // Constructor pattern: [Some x] or [Circle r]
        // Expression guard: [> x 0] (lowercase head → evaluate as expression)
        ExprKind::List(items) if !items.is_empty() => {
            if let ExprKind::Symbol(head) = &items[0].kind {
                if head.starts_with(char::is_uppercase) {
                    // Constructor pattern
                    if let Value::Adt(tag, fields) = value {
                        if tag == head && fields.len() == items.len() - 1 {
                            for (pat, val) in items[1..].iter().zip(fields.iter()) {
                                if !pattern_matches(pat, val, bindings, env)? {
                                    return Ok(false);
                                }
                            }
                            return Ok(true);
                        }
                        return Ok(false);
                    }
                } else {
                    // Expression guard: evaluate and check truthiness
                    let result = eval(&Expr::new(ExprKind::List(items.clone()), pattern.span), env)?;
                    return Ok(result.is_truthy());
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
    let type_name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(err("type name must be a symbol")),
    };

    // Collect methods: method_name → Vec<MethodImpl>
    let mut methods: HashMap<String, Vec<MethodImpl>> = HashMap::new();

    // Skip type params, register constructors
    for arg in &args[1..] {
        match &arg.kind {
            // [CtorName field1 field2 ... [fn method [params] body] ...]
            ExprKind::List(items) if !items.is_empty() => {
                if let ExprKind::Symbol(ctor_name) = &items[0].kind {
                    // Separate fields from method definitions
                    let mut field_count = 0;
                    let mut field_names: Vec<String> = Vec::new();
                    let mut variant_methods: Vec<(&str, Vec<String>, Vec<Expr>)> = Vec::new();
                    let mut has_named_fields = false;

                    let mut idx = 1;
                    while idx < items.len() {
                        // Check for [fn method [params] body...]
                        if let ExprKind::List(inner) = &items[idx].kind {
                            if inner.len() >= 3 {
                                if let ExprKind::Symbol(kw) = &inner[0].kind {
                                    if kw == "fn" {
                                        if let ExprKind::Symbol(mname) = &inner[1].kind {
                                            let params = if let ExprKind::List(ps) = &inner[2].kind {
                                                ps.iter().filter_map(|p| {
                                                    if let ExprKind::Symbol(s) = &p.kind { Some(s.clone()) } else { None }
                                                }).collect()
                                            } else {
                                                vec![]
                                            };
                                            variant_methods.push((
                                                mname.as_str(),
                                                params,
                                                inner[3..].to_vec(),
                                            ));
                                        }
                                        idx += 1;
                                        continue;
                                    }
                                }
                            }
                        }
                        // Check for named field: :keyword Type
                        if let ExprKind::Keyword(fname) = &items[idx].kind {
                            has_named_fields = true;
                            field_names.push(fname.clone());
                            field_count += 1;
                            idx += 2; // skip keyword + type
                            continue;
                        }
                        // Otherwise it's a positional field type
                        field_count += 1;
                        idx += 1;
                    }

                    // Register named fields if present
                    if has_named_fields {
                        ADT_FIELDS.with(|f| {
                            f.borrow_mut().insert(ctor_name.clone(), field_names.clone());
                        });
                    }

                    // Register constructor
                    let ctor_name_clone = ctor_name.clone();
                    let field_count_copy = field_count;
                    let has_named = has_named_fields;
                    let field_names_for_ctor = field_names.clone();
                    env.set(
                        ctor_name.clone(),
                        Value::Builtin(
                            ctor_name.clone(),
                            Arc::new(move |_name, args| {
                                // If single arg is a Map and we have named fields, reorder
                                if has_named && args.len() == 1 {
                                    if let Value::Map(pairs) = &args[0] {
                                        let mut ordered = Vec::with_capacity(field_names_for_ctor.len());
                                        for fname in &field_names_for_ctor {
                                            let key = Value::Keyword(fname.clone());
                                            let val = pairs.iter()
                                                .find(|(k, _)| *k == key)
                                                .map(|(_, v)| v.clone())
                                                .unwrap_or(Value::Unit);
                                            ordered.push(val);
                                        }
                                        return Ok(Value::Adt(ctor_name_clone.clone(), ordered));
                                    }
                                }
                                if args.len() != field_count_copy {
                                    return Err(InterpError {
                                        message: format!(
                                            "{} expects {} args, got {}",
                                            ctor_name_clone, field_count_copy, args.len()
                                        ),
                                        span: None, stack: vec![], performed_effect: None,
                                    });
                                }
                                Ok(Value::Adt(ctor_name_clone.clone(), args.to_vec()))
                            }),
                        ),
                    );

                    // Collect methods for dispatch
                    for (mname, params, body) in variant_methods {
                        methods.entry(mname.to_string()).or_default().push(MethodImpl {
                            ctor_name: ctor_name.clone(),
                            params,
                            body,
                        });
                    }
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

    // Store methods in thread-local and register dispatch functions
    if !methods.is_empty() {
        TYPE_METHODS.with(|tm| {
            let mut map = tm.borrow_mut();
            let type_entry = map.entry(type_name.clone()).or_default();
            for (method_name, impls) in &methods {
                type_entry.entry(method_name.clone()).or_default().extend(impls.clone());
            }
        });

        // Register dispatch builtins for each method
        for (method_name, _) in &methods {
            let mname = method_name.clone();
            env.set(
                method_name.clone(),
                Value::Builtin(
                    method_name.clone(),
                    Arc::new(move |_name, args| {
                        if args.is_empty() {
                            return Err(InterpError {
                                message: format!("{} requires at least one argument", _name),
                                span: None, stack: vec![], performed_effect: None,
                            });
                        }
                        let val = &args[0];
                        if let Value::Adt(tag, fields) = val {
                            // Look up method impl for this constructor
                            let method_impl = TYPE_METHODS.with(|tm| {
                                let map = tm.borrow();
                                for (_type_name, methods) in map.iter() {
                                    if let Some(impls) = methods.get(&mname) {
                                        for imp in impls {
                                            if imp.ctor_name == *tag {
                                                return Some(imp.clone());
                                            }
                                        }
                                    }
                                }
                                None
                            });

                            if let Some(imp) = method_impl {
                                let mut env = get_global_env().unwrap_or_default();
                                env.push_scope();
                                // Bind params to fields
                                for (param, field_val) in imp.params.iter().zip(fields.iter()) {
                                    env.set(param.clone(), field_val.clone());
                                }
                                let mut result = Value::Unit;
                                for expr in &imp.body {
                                    result = eval(expr, &mut env)?;
                                }
                                env.pop_scope();
                                return Ok(result);
                            }
                        }
                        Err(InterpError {
                            message: format!("no method {} for value {}", _name, val),
                            span: None, stack: vec![], performed_effect: None,
                        })
                    }),
                ),
            );
        }
    }

    Ok(Value::Unit)
}

/// Evaluate [impl TraitName TypeName [fn method [self ...] body] ...]
fn eval_impl_def(args: &[Expr], env: &mut Env) -> IResult {
    if args.len() < 2 {
        return Err(err("impl requires trait name and type name"));
    }
    let _trait_name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(err("impl trait name must be a symbol")),
    };
    let type_name = match &args[1].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(err("impl type name must be a symbol")),
    };

    for arg in &args[2..] {
        if let ExprKind::List(items) = &arg.kind {
            if items.len() >= 3 {
                if let ExprKind::Symbol(ref kw) = items[0].kind {
                    if kw == "fn" {
                        if let ExprKind::Symbol(ref method_name) = items[1].kind {
                            let params = extract_params(&items[2])?;
                            let body: std::rc::Rc<[Expr]> = items[3..].to_vec().into();
                            let lf = value::LoonFn {
                                name: Some(format!("{type_name}.{method_name}")),
                                clauses: vec![(params, body)],
                                captured_env: None,
                            };
                            // Register as TypeName.method_name
                            env.set_global(
                                format!("{type_name}.{method_name}"),
                                Value::Fn(lf),
                            );
                        }
                    }
                }
            }
        }
    }

    Ok(Value::Unit)
}

/// Evaluate [handle body handler1 handler2 ...]
/// Format: [handle [expr] [Effect.op params] => handler_body ...]
///
/// Supports resumable handlers: when an effect is caught and [resume val] is called,
/// the resume value is fed back as the return of the effect call, and the body is
/// re-evaluated with the effect temporarily overridden to return that value.
/// Multiple sequential effects are handled by accumulating overrides.
fn eval_handle(args: &[Expr], env: &mut Env) -> IResult {
    if args.is_empty() {
        return Err(err("handle requires a body expression"));
    }

    let body = &args[0];
    let handlers = collect_handlers(&args[1..]);

    // Mark that we're inside a handle block so effects propagate as errors
    // instead of being eagerly handled by builtin handlers
    let was_inside = INSIDE_HANDLE.with(|h| {
        let old = *h.borrow();
        *h.borrow_mut() = true;
        old
    });
    let restore = || INSIDE_HANDLE.with(|h| *h.borrow_mut() = was_inside);

    // We accumulate effect overrides: when an effect is handled with resume,
    // we record the override and re-evaluate the body from scratch.
    // Each override is (effect, op, call_index, resume_value) where call_index
    // counts which invocation of that effect.op to override.
    let mut overrides: Vec<(String, String, usize, Value)> = Vec::new();
    let max_iterations = 100; // safety limit

    for _ in 0..max_iterations {
        // Install overrides as temporary effect interceptors
        let result = eval_with_effect_overrides(body, env, &overrides);

        match result {
            Ok(val) => {
                restore();
                return Ok(val);
            }
            Err(e) => {
                if let Some(ref performed) = e.performed_effect {
                    // Run handler with INSIDE_HANDLE restored so it can use builtin effects
                    restore();
                    let handler_result = run_handler(performed, &handlers, env)?;
                    INSIDE_HANDLE.with(|h| *h.borrow_mut() = true);

                    if let Some(resume_val) = handler_result {
                        // Count how many times this effect.op has been overridden already
                        let count = overrides.iter()
                            .filter(|(eff, op, _, _)| eff == &performed.effect && op == &performed.operation)
                            .count();
                        overrides.push((
                            performed.effect.clone(),
                            performed.operation.clone(),
                            count,
                            resume_val,
                        ));
                        // Re-evaluate body with the new override
                        continue;
                    } else {
                        restore();
                        return Err(e);
                    }
                } else {
                    restore();
                    return Err(e);
                }
            }
        }
    }

    restore();
    Err(err("handle: too many sequential effects (possible infinite loop)"))
}

/// Evaluate an expression, but intercept effect operations that have overrides.
/// When the Nth invocation of Effect.op is encountered and there's an override for it,
/// return the override value instead of performing the effect.
fn eval_with_effect_overrides(
    expr: &Expr,
    env: &mut Env,
    overrides: &[(String, String, usize, Value)],
) -> IResult {
    if overrides.is_empty() {
        return eval(expr, env);
    }

    // We need to track how many times each effect.op has been invoked during this
    // evaluation so we can match the correct override. We use thread-local counters.
    EFFECT_COUNTERS.with(|c| c.borrow_mut().clear());

    eval_with_overrides_inner(expr, env, overrides)
}

thread_local! {
    static EFFECT_COUNTERS: RefCell<HashMap<(String, String), usize>> = RefCell::new(HashMap::new());
    static EFFECT_OVERRIDES: RefCell<Vec<(String, String, usize, Value)>> = const { RefCell::new(Vec::new()) };
    static INSIDE_HANDLE: RefCell<bool> = const { RefCell::new(false) };
    /// Type methods: type_name → method_name → Vec<(ctor_name, param_names, body_exprs)>
    static TYPE_METHODS: RefCell<HashMap<String, HashMap<String, Vec<MethodImpl>>>> = RefCell::new(HashMap::new());
    /// ADT field names: ctor_name → Vec<field_name> (for named fields / record types)
    static ADT_FIELDS: RefCell<HashMap<String, Vec<String>>> = RefCell::new(HashMap::new());
}

#[derive(Clone)]
struct MethodImpl {
    ctor_name: String,
    params: Vec<String>,
    body: Vec<Expr>,
}

fn eval_with_overrides_inner(
    expr: &Expr,
    env: &mut Env,
    overrides: &[(String, String, usize, Value)],
) -> IResult {
    // Set up the overrides in thread-local storage so perform_effect_or_override can use them
    EFFECT_OVERRIDES.with(|o| *o.borrow_mut() = overrides.to_vec());
    eval(expr, env)
}

/// Check if an effect operation has an override. If so, return the override value
/// and increment the counter. Otherwise return None.
fn check_effect_override(effect: &str, op: &str) -> Option<Value> {
    let key = (effect.to_string(), op.to_string());

    let count = EFFECT_COUNTERS.with(|c| {
        let mut counters = c.borrow_mut();
        let count = counters.entry(key.clone()).or_insert(0);
        let current = *count;
        *count += 1;
        current
    });

    EFFECT_OVERRIDES.with(|o| {
        let overrides = o.borrow();
        for (eff, operation, idx, val) in overrides.iter() {
            if eff == effect && operation == op && *idx == count {
                return Some(val.clone());
            }
        }
        None
    })
}

struct Handler<'a> {
    effect: String,
    op: String,
    params: Vec<String>,
    body: &'a Expr,
}

fn collect_handlers<'a>(handler_args: &'a [Expr]) -> Vec<Handler<'a>> {
    let mut handlers = Vec::new();
    let mut i = 0;
    while i < handler_args.len() {
        if let ExprKind::List(pattern) = &handler_args[i].kind {
            if !pattern.is_empty() {
                if let ExprKind::Symbol(qualified) = &pattern[0].kind {
                    if let Some((effect, op)) = qualified.split_once('.') {
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

                        if i + 1 < handler_args.len() {
                            handlers.push(Handler {
                                effect: effect.to_string(),
                                op: op.to_string(),
                                params,
                                body: &handler_args[i + 1],
                            });
                            i += 2;
                            continue;
                        }
                    }
                }
            }
        }
        i += 1;
    }
    handlers
}

/// Try to match a performed effect against handlers.
/// Returns Ok(Some(resume_val)) if handled (resume was called),
/// Ok(None) if no handler matched,
/// Err if the handler errored.
fn run_handler(
    performed: &PerformedEffect,
    handlers: &[Handler<'_>],
    env: &mut Env,
) -> Result<Option<Value>, InterpError> {
    for handler in handlers {
        if performed.effect == handler.effect && performed.operation == handler.op {
            env.push_scope();
            for (pname, pval) in handler.params.iter().zip(performed.args.iter()) {
                if pname != "_" {
                    env.set(pname.clone(), pval.clone());
                }
            }
            // `resume` returns the value it's called with — this becomes
            // the return value of the handled effect call site.
            env.set(
                "resume".to_string(),
                Value::Builtin(
                    "resume".to_string(),
                    Arc::new(|_, args: &[Value]| {
                        Ok(args.first().cloned().unwrap_or(Value::Unit))
                    }),
                ),
            );
            let result = eval(handler.body, env);
            env.pop_scope();
            return result.map(Some);
        }
    }
    Ok(None)
}

/// Evaluate [try body on-fail]
/// Desugars to: [handle body [Fail.fail msg] => [on-fail msg]]
fn eval_try(args: &[Expr], env: &mut Env) -> IResult {
    if args.len() < 2 {
        return Err(err("try requires a body and failure handler"));
    }
    let body = &args[0];
    let on_fail = eval(&args[1], env)?;

    match eval(body, env) {
        Ok(val) => Ok(val),
        Err(e) => {
            if let Some(ref performed) = e.performed_effect {
                if performed.effect == "Fail" && performed.operation == "fail" {
                    let msg = performed.args.first().cloned().unwrap_or(Value::Unit);
                    match &on_fail {
                        Value::Fn(lf) => call_fn(lf, &[msg], env, Span::ZERO),
                        Value::Builtin(name, f) => f(name, &[msg]),
                        _ => Err(err("try: on-fail must be callable")),
                    }
                } else {
                    Err(e)
                }
            } else {
                Err(e)
            }
        }
    }
}

fn eval_test_def(args: &[Expr], env: &mut Env) -> IResult {
    // [test name [params] body...] — register as a named test function
    // Also supports [test fn name ...] for backward compat
    if args.len() >= 2 {
        if let ExprKind::Symbol(s) = &args[0].kind {
            if s == "fn" {
                // [test fn name ...] — strip fn and delegate
                return eval_fn(&args[1..], env);
            }
            // [test name [params] body...] — treat as named fn
            return eval_fn(args, env);
        }
    }
    Ok(Value::Unit)
}

fn extract_params(expr: &Expr) -> Result<Vec<value::Param>, InterpError> {
    match &expr.kind {
        ExprKind::List(items) => {
            let mut params = Vec::new();
            let mut i = 0;
            while i < items.len() {
                if let ExprKind::Symbol(s) = &items[i].kind {
                    if s == "&" {
                        // Rest parameter: & name
                        if i + 1 < items.len() {
                            if let ExprKind::Symbol(rest_name) = &items[i + 1].kind {
                                params.push(value::Param::Rest(rest_name.clone()));
                                i += 2;
                                continue;
                            }
                        }
                        return Err(err("& must be followed by a parameter name"));
                    }
                }
                params.push(extract_param(&items[i])?);
                i += 1;
            }
            Ok(params)
        }
        _ => Err(err_at("params must be a list", expr.span)),
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
            let mut entries = Vec::new();
            for (k, v) in pairs {
                if let ExprKind::Symbol(s) = &k.kind {
                    // If value is a symbol with the same name as the key, treat as no default
                    let default_expr = if matches!(&v.kind, ExprKind::Symbol(vs) if vs == s) {
                        None
                    } else {
                        Some(v.clone())
                    };
                    entries.push((s.clone(), default_expr));
                }
            }
            Ok(value::Param::MapDestructure(entries))
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
        value::Param::MapDestructure(entries) => {
            let pairs = match val {
                Value::Map(m) => m,
                _ => return Err(err("map destructuring requires a map")),
            };
            for (name, default_expr) in entries {
                let key = Value::Keyword(name.clone());
                let found = pairs
                    .iter()
                    .find(|(k, _)| *k == key)
                    .map(|(_, v)| v.clone());
                let bound = match found {
                    Some(val) => val,
                    None => match default_expr {
                        Some(expr) => eval(expr, env)?,
                        None => Value::Unit,
                    },
                };
                env.set(name.clone(), bound);
            }
            Ok(())
        }
        value::Param::Rest(name) => {
            // Rest param in bind_param context — bind as-is
            env.set(name.clone(), val.clone());
            Ok(())
        }
    }
}

pub(crate) fn call_fn(lf: &value::LoonFn, args: &[Value], env: &mut Env, call_span: Span) -> IResult {
    let fn_name = lf.name.as_deref().unwrap_or("anonymous").to_string();
    CALL_STACK.with(|s| s.borrow_mut().push(StackFrame {
        fn_name: fn_name.clone(),
        call_site: call_span,
    }));
    let result = call_fn_inner(lf, args, env);
    CALL_STACK.with(|s| s.borrow_mut().pop());
    result
}

fn call_fn_inner(lf: &value::LoonFn, args: &[Value], env: &mut Env) -> IResult {
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

    // Find matching clause by arity (with rest param support)
    for (params, body) in &lf.clauses {
        let has_rest = params.last().is_some_and(|p| matches!(p, value::Param::Rest(_)));
        let required = if has_rest { params.len() - 1 } else { params.len() };
        let matches = if has_rest {
            args.len() >= required
        } else {
            args.len() == required
        };

        if matches {
            env.push_scope();
            // Bind regular params
            for (param, val) in params[..required].iter().zip(args[..required].iter()) {
                bind_param(param, val, env)?;
            }
            // Bind rest param if present
            if has_rest {
                if let Some(value::Param::Rest(name)) = params.last() {
                    let rest_vals: Vec<Value> = args[required..].to_vec();
                    env.set(name.clone(), Value::Vec(rest_vals));
                }
            }
            let mut result = Value::Unit;
            for expr in body.iter() {
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

fn eval_catch_errors(source: &str) -> Value {
    let exprs = match crate::parser::parse(source) {
        Ok(exprs) => exprs,
        Err(e) => {
            let error_map = vec![
                (Value::Keyword("code".to_string()), Value::Str("E0000".to_string())),
                (Value::Keyword("what".to_string()), Value::Str(e.message)),
                (Value::Keyword("why".to_string()), Value::Str("parse error".to_string())),
                (Value::Keyword("fix".to_string()), Value::Str("check syntax".to_string())),
                (Value::Keyword("spans".to_string()), Value::Vec(vec![])),
            ];
            return Value::Vec(vec![Value::Map(error_map)]);
        }
    };

    let mut checker = crate::check::Checker::new();
    let type_errors = checker.check_program(&exprs);

    let mut ownership =
        crate::check::ownership::OwnershipChecker::with_type_info(
            &checker.type_of,
            &checker.subst,
        )
        .with_derived_copy_types(&checker.derived_copy_types);
    let ownership_errors = ownership.check_program(&checker.expanded_program);

    let all_errors: Vec<_> = type_errors.into_iter().chain(ownership_errors).collect();

    let error_maps: Vec<Value> = all_errors
        .iter()
        .map(|diag| {
            let spans: Vec<Value> = diag.labels.iter().map(|l| {
                Value::Map(vec![
                    (Value::Keyword("start".to_string()), Value::Int(l.span.start as i64)),
                    (Value::Keyword("end".to_string()), Value::Int(l.span.end as i64)),
                    (Value::Keyword("label".to_string()), Value::Str(l.label.clone())),
                ])
            }).collect();
            Value::Map(vec![
                (Value::Keyword("code".to_string()), Value::Str(format!("{}", diag.code))),
                (Value::Keyword("what".to_string()), Value::Str(diag.what.clone())),
                (Value::Keyword("why".to_string()), Value::Str(diag.why.clone())),
                (Value::Keyword("fix".to_string()), Value::Str(diag.fix.clone())),
                (Value::Keyword("spans".to_string()), Value::Vec(spans)),
            ])
        })
        .collect();

    Value::Vec(error_maps)
}

fn register_builtins(env: &mut Env) {
    builtins::register_builtins(env);
    if dom_builtins::has_dom_bridge() {
        dom_builtins::register_dom_builtins(env);
    }
}

/// Evaluate a Loon function in a spawned thread, handling effects with try_builtin_handler.
/// Uses eval_program_with_base_dir-style effect handling for the full function body.
fn eval_spawned_fn(lf: &value::LoonFn, env: &mut Env) -> Value {
    match call_fn(lf, &[], env, Span::ZERO) {
        Ok(val) => val,
        Err(e) => {
            if let Some(ref performed) = e.performed_effect {
                if let Some(result) = try_builtin_handler(performed) {
                    match result {
                        Ok(val) => val,
                        Err(e2) => {
                            eprintln!("[Async.spawn] effect error: {}", e2.message);
                            Value::Unit
                        }
                    }
                } else {
                    eprintln!("[Async.spawn] unhandled effect: {}.{}", performed.effect, performed.operation);
                    Value::Unit
                }
            } else {
                eprintln!("[Async.spawn] thread error: {}", e.message);
                Value::Unit
            }
        }
    }
}
