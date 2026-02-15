use super::builtins::apply_value;
use super::value::Value;
use super::{err, Env, InterpError};
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::sync::Arc;

type IResult = Result<Value, InterpError>;

/// Callback type for DOM operations. The bridge receives:
/// - operation name (e.g. "createElement", "setAttribute")
/// - arguments as a list of Values
/// Returns a Value (often DomNode handle or Unit).
///
/// This must be Send+Sync since it's stored in a global, but in WASM
/// there's only one thread so this is safe.
pub type DomBridgeFn = Arc<dyn Fn(&str, &[Value]) -> IResult + Send + Sync>;

thread_local! {
    static DOM_BRIDGE: RefCell<Option<DomBridgeFn>> = const { RefCell::new(None) };
    static CALLBACKS: RefCell<HashMap<u32, Value>> = RefCell::new(HashMap::new());
    static NEXT_CB: Cell<u32> = const { Cell::new(1) };
}

pub fn set_dom_bridge(bridge: DomBridgeFn) {
    DOM_BRIDGE.with(|b| {
        *b.borrow_mut() = Some(bridge);
    });
}

pub fn has_dom_bridge() -> bool {
    DOM_BRIDGE.with(|b| b.borrow().is_some())
}

fn call_bridge(op: &str, args: &[Value]) -> IResult {
    DOM_BRIDGE.with(|b| {
        let guard = b.borrow();
        match guard.as_ref() {
            Some(bridge) => bridge(op, args),
            None => Err(err(format!(
                "DOM bridge not initialized (called {op})"
            ))),
        }
    })
}

pub fn store_callback(val: Value) -> u32 {
    NEXT_CB.with(|n| {
        let id = n.get();
        n.set(id + 1);
        CALLBACKS.with(|cb| {
            cb.borrow_mut().insert(id, val);
        });
        id
    })
}

pub fn invoke_callback(id: u32) -> IResult {
    let func = CALLBACKS.with(|cb| cb.borrow().get(&id).cloned());
    match func {
        Some(f) => apply_value(&f, &[]),
        None => Err(err(format!("callback {id} not found"))),
    }
}

macro_rules! builtin {
    ($env:expr, $name:expr, $f:expr) => {
        $env.set(
            $name.to_string(),
            Value::Builtin($name.to_string(), Arc::new($f)),
        );
    };
}

pub fn register_dom_builtins(env: &mut Env) {
    // DOM creation
    builtin!(env, "dom/create-element", |_, args: &[Value]| {
        call_bridge("createElement", args)
    });

    builtin!(env, "dom/create-text", |_, args: &[Value]| {
        call_bridge("createText", args)
    });

    builtin!(env, "dom/set-attribute", |_, args: &[Value]| {
        call_bridge("setAttribute", args)
    });

    builtin!(env, "dom/set-style", |_, args: &[Value]| {
        call_bridge("setStyle", args)
    });

    builtin!(env, "dom/append-child", |_, args: &[Value]| {
        call_bridge("appendChild", args)
    });

    builtin!(env, "dom/remove-child", |_, args: &[Value]| {
        call_bridge("removeChild", args)
    });

    builtin!(env, "dom/replace-child", |_, args: &[Value]| {
        call_bridge("replaceChild", args)
    });

    builtin!(env, "dom/set-text", |_, args: &[Value]| {
        call_bridge("setText", args)
    });

    builtin!(env, "dom/query-selector", |_, args: &[Value]| {
        call_bridge("querySelector", args)
    });

    builtin!(env, "dom/set-inner-html", |_, args: &[Value]| {
        call_bridge("setInnerHTML", args)
    });

    // Events — intercept callable args to store in callback registry
    builtin!(env, "dom/add-listener", |_, args: &[Value]| {
        if args.len() >= 3 && args[2].is_callable() {
            let cb_id = store_callback(args[2].clone());
            let mut new_args = args.to_vec();
            new_args[2] = Value::Int(cb_id as i64);
            call_bridge("addListener", &new_args)
        } else {
            call_bridge("addListener", args)
        }
    });

    builtin!(env, "dom/remove-listener", |_, args: &[Value]| {
        call_bridge("removeListener", args)
    });

    // Form elements
    builtin!(env, "dom/get-value", |_, args: &[Value]| {
        call_bridge("getValue", args)
    });

    builtin!(env, "dom/set-value", |_, args: &[Value]| {
        call_bridge("setValue", args)
    });

    builtin!(env, "dom/eval-loon", |_, args: &[Value]| {
        call_bridge("evalLoon", args)
    });

    // Browser
    builtin!(env, "dom/set-title", |_, args: &[Value]| {
        call_bridge("setTitle", args)
    });

    builtin!(env, "dom/push-state", |_, args: &[Value]| {
        call_bridge("pushState", args)
    });

    builtin!(env, "dom/location", |_, args: &[Value]| {
        call_bridge("location", args)
    });

    builtin!(env, "dom/request-animation-frame", |_, args: &[Value]| {
        call_bridge("requestAnimationFrame", args)
    });

    builtin!(env, "dom/set-timeout", |_, args: &[Value]| {
        call_bridge("setTimeout", args)
    });
}
