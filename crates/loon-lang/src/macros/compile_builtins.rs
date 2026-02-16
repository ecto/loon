//! Compile-time builtins available during macro expansion.
//!
//! These are gated by declared compile-time effects:
//! - IO: file system access at compile time
//! - Net: HTTP access at compile time
//! - Env: environment variable access
//! - Print: compile-time diagnostics

use crate::interp::{self, Value};
use std::collections::HashSet;
use super::CompileEffect;

/// Register compile-time builtins into an interpreter environment.
/// Only registers builtins for effects that are declared.
pub fn register_compile_builtins(
    env: &mut interp::Env,
    declared_effects: &HashSet<CompileEffect>,
) {
    if declared_effects.contains(&CompileEffect::IO) {
        register_io_builtins(env);
    }
    if declared_effects.contains(&CompileEffect::Env) {
        register_env_builtins(env);
    }
    if declared_effects.contains(&CompileEffect::Print) {
        register_print_builtins(env);
    }
    // Net builtins would go here when implemented
}

fn register_io_builtins(env: &mut interp::Env) {
    use std::sync::Arc;

    env.set(
        "IO.read-file".to_string(),
        Value::Builtin(
            "IO.read-file".to_string(),
            Arc::new(|_name, args| {
                if let Some(Value::Str(path)) = args.first() {
                    match std::fs::read_to_string(path) {
                        Ok(contents) => Ok(Value::Str(contents)),
                        Err(e) => Err(interp::err(format!("IO.read-file: {e}"))),
                    }
                } else {
                    Err(interp::err("IO.read-file requires a string path"))
                }
            }),
        ),
    );

    env.set(
        "IO.file-exists?".to_string(),
        Value::Builtin(
            "IO.file-exists?".to_string(),
            Arc::new(|_name, args| {
                if let Some(Value::Str(path)) = args.first() {
                    Ok(Value::Bool(std::path::Path::new(path).exists()))
                } else {
                    Err(interp::err("IO.file-exists? requires a string path"))
                }
            }),
        ),
    );

    env.set(
        "IO.write-file".to_string(),
        Value::Builtin(
            "IO.write-file".to_string(),
            Arc::new(|_name, args| {
                if let (Some(Value::Str(path)), Some(contents)) =
                    (args.first(), args.get(1))
                {
                    match std::fs::write(path, contents.display_str()) {
                        Ok(()) => Ok(Value::Unit),
                        Err(e) => Err(interp::err(format!("IO.write-file: {e}"))),
                    }
                } else {
                    Err(interp::err("IO.write-file requires a path and contents"))
                }
            }),
        ),
    );
}

fn register_env_builtins(env: &mut interp::Env) {
    use std::sync::Arc;

    env.set(
        "Env.get".to_string(),
        Value::Builtin(
            "Env.get".to_string(),
            Arc::new(|_name, args| {
                if let Some(Value::Str(key)) = args.first() {
                    match std::env::var(key) {
                        Ok(val) => Ok(Value::Adt("Some".to_string(), vec![Value::Str(val)])),
                        Err(_) => Ok(Value::Adt("None".to_string(), vec![])),
                    }
                } else {
                    Err(interp::err("Env.get requires a string key"))
                }
            }),
        ),
    );
}

fn register_print_builtins(env: &mut interp::Env) {
    use std::sync::Arc;

    env.set(
        "compile/warn".to_string(),
        Value::Builtin(
            "compile/warn".to_string(),
            Arc::new(|_name, args| {
                if let Some(Value::Str(msg)) = args.first() {
                    eprintln!("warning: {msg}");
                    Ok(Value::Unit)
                } else {
                    Err(interp::err("compile/warn requires a string message"))
                }
            }),
        ),
    );

    env.set(
        "compile/error".to_string(),
        Value::Builtin(
            "compile/error".to_string(),
            Arc::new(|_name, args| {
                if let Some(Value::Str(msg)) = args.first() {
                    Err(interp::err(format!("compile-time error: {msg}")))
                } else {
                    Err(interp::err("compile/error requires a string message"))
                }
            }),
        ),
    );
}
