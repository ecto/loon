use wasm_bindgen::prelude::*;

use loon_lang::interp::dom_builtins;
use loon_lang::interp::Value;
use std::cell::RefCell;
use std::sync::Arc;

// Store the JS bridge function in thread-local storage (WASM is single-threaded).
// The dom_builtins bridge calls back into this.
thread_local! {
    static JS_BRIDGE: RefCell<Option<js_sys::Function>> = const { RefCell::new(None) };
}

fn call_js_bridge(op: &str, args: &[Value]) -> Result<Value, loon_lang::interp::InterpError> {
    JS_BRIDGE.with(|b| {
        let guard = b.borrow();
        let bridge = guard.as_ref().ok_or_else(|| loon_lang::interp::InterpError {
            message: "JS bridge not initialized".to_string(),
            performed_effect: None,
        })?;

        let js_op = JsValue::from_str(op);
        let js_args = js_sys::Array::new();
        for arg in args {
            js_args.push(&value_to_js(arg));
        }

        let result = bridge
            .call2(&JsValue::NULL, &js_op, &js_args)
            .map_err(|e| loon_lang::interp::InterpError {
                message: format!("DOM bridge error: {:?}", e),
                performed_effect: None,
            })?;

        Ok(js_to_value(&result))
    })
}

/// Evaluate a Loon program and return the result as a string.
#[wasm_bindgen]
pub fn eval_program(source: &str) -> Result<String, String> {
    let exprs = loon_lang::parser::parse(source).map_err(|e| format!("{e}"))?;
    let result = loon_lang::interp::eval_program(&exprs).map_err(|e| e.message)?;
    Ok(format!("{result}"))
}

/// Initialize the DOM bridge from JS. The bridge is a JS function that receives
/// (operation: string, args: any[]) and returns a result.
/// Must be called before eval_ui.
#[wasm_bindgen]
pub fn init_dom_bridge(bridge: &js_sys::Function) {
    // Store JS function in thread-local
    JS_BRIDGE.with(|b| {
        *b.borrow_mut() = Some(bridge.clone());
    });

    // Set up the Loon-side bridge that delegates to our thread-local JS function
    let dom_fn: dom_builtins::DomBridgeFn = Arc::new(|op: &str, args: &[Value]| {
        call_js_bridge(op, args)
    });

    dom_builtins::set_dom_bridge(dom_fn);
}

/// Evaluate a Loon UI program. The DOM bridge must be initialized first.
#[wasm_bindgen]
pub fn eval_ui(source: &str) -> Result<(), String> {
    let exprs = loon_lang::parser::parse(source).map_err(|e| format!("{e}"))?;
    loon_lang::interp::eval_program(&exprs).map_err(|e| e.message)?;
    Ok(())
}

/// Evaluate Loon source and capture all println output.
#[wasm_bindgen]
pub fn eval_with_output(source: &str) -> Result<String, String> {
    let exprs = loon_lang::parser::parse(source).map_err(|e| format!("{e}"))?;
    let result = loon_lang::interp::eval_program(&exprs).map_err(|e| e.message)?;
    Ok(format!("{result}"))
}

/// Type-check Loon source and return diagnostics.
#[wasm_bindgen]
pub fn check_program(source: &str) -> Result<String, String> {
    let exprs = loon_lang::parser::parse(source).map_err(|e| format!("{e}"))?;
    let mut checker = loon_lang::check::Checker::new();
    let errors = checker.check_program(&exprs);
    if errors.is_empty() {
        Ok("OK".to_string())
    } else {
        let msgs: Vec<String> = errors.iter().map(|e| format!("{e}")).collect();
        Err(msgs.join("\n"))
    }
}

fn value_to_js(val: &Value) -> JsValue {
    match val {
        Value::Int(n) => JsValue::from_f64(*n as f64),
        Value::Float(n) => JsValue::from_f64(*n),
        Value::Bool(b) => JsValue::from_bool(*b),
        Value::Str(s) => JsValue::from_str(s),
        Value::Keyword(k) => JsValue::from_str(&format!(":{k}")),
        Value::DomNode(h) => JsValue::from_f64(*h as f64),
        Value::Unit => JsValue::NULL,
        Value::Vec(items) => {
            let arr = js_sys::Array::new();
            for item in items {
                arr.push(&value_to_js(item));
            }
            arr.into()
        }
        Value::Map(pairs) => {
            let obj = js_sys::Object::new();
            for (k, v) in pairs {
                let key = match k {
                    Value::Str(s) => s.clone(),
                    Value::Keyword(k) => k.clone(),
                    other => format!("{other}"),
                };
                js_sys::Reflect::set(&obj, &JsValue::from_str(&key), &value_to_js(v)).ok();
            }
            obj.into()
        }
        _ => JsValue::NULL,
    }
}

fn js_to_value(js: &JsValue) -> Value {
    if js.is_null() || js.is_undefined() {
        Value::Unit
    } else if let Some(b) = js.as_bool() {
        Value::Bool(b)
    } else if let Some(n) = js.as_f64() {
        if n == (n as u32) as f64 && n >= 0.0 {
            Value::DomNode(n as u32)
        } else if n == (n as i64) as f64 {
            Value::Int(n as i64)
        } else {
            Value::Float(n)
        }
    } else if let Some(s) = js.as_string() {
        Value::Str(s)
    } else {
        Value::Unit
    }
}
