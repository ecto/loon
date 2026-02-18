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
            span: None,
            stack: vec![],
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
                span: None,
                stack: vec![],
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
    loon_lang::interp::set_source_text(source);
    let exprs = loon_lang::parser::parse(source).map_err(|e| format!("{e}"))?;
    loon_lang::interp::eval_program(&exprs).map_err(|e| e.message)?;
    Ok(())
}

/// Evaluate Loon source and capture all println output.
#[wasm_bindgen]
pub fn eval_with_output(source: &str) -> Result<String, String> {
    let perf = web_sys::window().and_then(|w: web_sys::Window| w.performance());
    let now = || perf.as_ref().map_or_else(js_sys::Date::now, |p: &web_sys::Performance| p.now());

    let t0 = now();
    let exprs = loon_lang::parser::parse(source).map_err(|e| format!("{e}"))?;
    let t1 = now();

    let mut checker = loon_lang::check::Checker::new();
    let _errors = checker.check_program(&exprs);
    let t2 = now();

    let (result, output) = loon_lang::interp::builtins::capture_output(|| {
        loon_lang::interp::eval_program(&exprs)
    });
    let t3 = now();

    let result = result.map_err(|e| e.message)?;
    let formatted = format!("{result}");
    let body = if output.is_empty() {
        formatted
    } else if formatted == "()" {
        output
    } else {
        format!("{output}\n{formatted}")
    };

    let fmt = |ms: f64| -> String {
        if ms < 1.0 {
            format!("{:.0}\u{00B5}s", ms * 1000.0)
        } else {
            format!("{ms:.1}ms")
        }
    };
    let parse_ms = t1 - t0;
    let check_ms = t2 - t1;
    let eval_ms = t3 - t2;
    Ok(format!("{body}\n\u{2014}\nparse: {} | check: {} | eval: {}", fmt(parse_ms), fmt(check_ms), fmt(eval_ms)))
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

/// Reset the Loon runtime state (callbacks, etc.) for hot reload.
#[wasm_bindgen]
pub fn reset_runtime() {
    dom_builtins::reset_callbacks();
}

/// Evaluate a Loon UI program, returning structured JSON instead of throwing.
/// Success: {"ok":true}
/// Error:   {"ok":false,"error":{"message":"...","span":[start,end],"stack":[{"fn":"name","span":[start,end]},...]}}
#[wasm_bindgen]
pub fn eval_ui_checked(source: &str) -> String {
    let perf = web_sys::window().and_then(|w: web_sys::Window| w.performance());
    let now = || perf.as_ref().map_or_else(js_sys::Date::now, |p: &web_sys::Performance| p.now());

    loon_lang::interp::set_source_text(source);

    let t0 = now();
    let exprs = match loon_lang::parser::parse(source) {
        Ok(e) => e,
        Err(e) => return format!(
            r#"{{"ok":false,"error":{{"message":"{}","span":null,"stack":[]}}}}"#,
            escape_json(&format!("{e}"))
        ),
    };
    let t1 = now();

    let t2 = now();
    let result = loon_lang::interp::eval_program(&exprs);
    let t3 = now();

    console_warn(&format!("[loon-perf] parse: {:.0}ms, eval: {:.0}ms, total: {:.0}ms", t1-t0, t3-t2, t3-t0));

    match result {
        Ok(_) => r#"{"ok":true}"#.to_string(),
        Err(e) => {
            let span_json = match e.span {
                Some(sp) => format!("[{},{}]", sp.start, sp.end),
                None => "null".to_string(),
            };
            let stack_json: Vec<String> = e.stack.iter().map(|f| {
                format!(
                    r#"{{"fn":"{}","span":[{},{}]}}"#,
                    escape_json(&f.fn_name),
                    f.call_site.start,
                    f.call_site.end,
                )
            }).collect();
            format!(
                r#"{{"ok":false,"error":{{"message":"{}","span":{},"stack":[{}]}}}}"#,
                escape_json(&e.message),
                span_json,
                stack_json.join(","),
            )
        }
    }
}

/// Enable or disable effect logging.
#[wasm_bindgen]
pub fn enable_effect_log(enabled: bool) {
    loon_lang::interp::enable_effect_log(enabled);
}

/// Get the effect log as a JSON array.
#[wasm_bindgen]
pub fn get_effect_log() -> String {
    let log = loon_lang::interp::get_effect_log();
    let entries: Vec<String> = log.iter().map(|e| {
        format!(
            r#"{{"effect":"{}","operation":"{}","span":[{},{}]}}"#,
            escape_json(&e.effect),
            escape_json(&e.operation),
            e.span.start,
            e.span.end,
        )
    }).collect();
    format!("[{}]", entries.join(","))
}

/// Clear the effect log.
#[wasm_bindgen]
pub fn clear_effect_log() {
    loon_lang::interp::clear_effect_log();
}

fn escape_json(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if (c as u32) < 0x20 => {
                out.push_str(&format!("\\u{:04x}", c as u32));
            }
            c => out.push(c),
        }
    }
    out
}

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console, js_name = warn)]
    fn console_warn(s: &str);
}

/// Invoke a stored Loon callback by ID (called from JS event handlers).
#[wasm_bindgen]
pub fn invoke_callback(id: u32) {
    match dom_builtins::invoke_callback(id) {
        Ok(_) => {}
        Err(e) => {
            console_warn(&format!("Loon callback {id} error: {}", e.message));
        }
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
