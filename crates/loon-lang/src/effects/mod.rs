use std::collections::HashMap;

/// An effect declaration (e.g., [effect IO [fn read-file ...]])
#[derive(Debug, Clone)]
pub struct EffectDecl {
    pub name: String,
    pub operations: Vec<EffectOp>,
}

/// An operation within an effect
#[derive(Debug, Clone)]
pub struct EffectOp {
    pub name: String,
    pub params: Vec<(String, Option<String>)>, // (name, type_name)
    pub return_type: Option<String>,
    /// The final parameter absorbs any number of extra arguments.
    ///
    /// `Place.run` needs this: a kernel's arguments are its own, and the
    /// placement operation carries however many of them there are.
    pub variadic: bool,
}

/// Registry of declared effects
#[derive(Debug, Clone, Default)]
pub struct EffectRegistry {
    effects: HashMap<String, EffectDecl>,
    /// Maps "Effect.op" → (effect_name, op_name)
    op_lookup: HashMap<String, (String, String)>,
}

/// Helper to build a param list from names (no types)
fn params(names: &[&str]) -> Vec<(String, Option<String>)> {
    names.iter().map(|n| (n.to_string(), None)).collect()
}

/// Helper to build a no-param, no-return EffectOp
fn op(name: &str, param_names: &[&str]) -> EffectOp {
    EffectOp {
        name: name.to_string(),
        params: params(param_names),
        return_type: None,
        variadic: false,
    }
}

/// An op whose last parameter absorbs any number of trailing arguments.
fn var_op(name: &str, param_names: &[&str]) -> EffectOp {
    EffectOp {
        name: name.to_string(),
        params: params(param_names),
        return_type: None,
        variadic: true,
    }
}

/// Helper to build a typed EffectOp with typed params and return type
fn typed_op(name: &str, param_names: &[&str], ret: &str) -> EffectOp {
    EffectOp {
        name: name.to_string(),
        params: params(param_names),
        return_type: Some(ret.to_string()),
        variadic: false,
    }
}

impl EffectRegistry {
    pub fn new() -> Self {
        let mut reg = Self::default();
        // Built-in effects
        reg.register(EffectDecl {
            name: "IO".to_string(),
            operations: vec![
                op("println", &["msg"]),
                op("read-line", &[]),
                op("read-file", &["path"]),
                op("write-file", &["path", "content"]),
                op("parse-json", &["text"]),
                op("to-json", &["value"]),
                op("list-dir", &["path"]),
                op("mkdir", &["path"]),
                op("file-exists?", &["path"]),
                op("delete-file", &["path"]),
                op("copy-file", &["src", "dst"]),
                op("mtime", &["path"]),
                op("now", &[]),
                op("millis", &[]),
                op("sleep", &["ms"]),
                op("uuid", &[]),
                op("blake3", &["text"]),
            ],
        });
        reg.register(EffectDecl {
            name: "Fail".to_string(),
            operations: vec![op("fail", &["msg"])],
        });
        reg.register(EffectDecl {
            name: "Process".to_string(),
            operations: vec![
                op("args", &[]),
                op("env", &["key"]),
                op("exit", &["code"]),
                op("exec", &["command"]),
            ],
        });
        reg.register(EffectDecl {
            name: "Async".to_string(),
            operations: vec![
                op("spawn", &["thunk"]),
                op("await", &["future"]),
                op("sleep", &["ms"]),
                op("loop", &["init", "step"]),
            ],
        });
        reg.register(EffectDecl {
            name: "Net".to_string(),
            operations: vec![
                op("get", &["url"]),
                op("post", &["url", "options"]),
                // Blocking TCP/HTTP server on the EIR VM (see eir/net.rs):
                // listen a port, accept one request (-> [method path body]),
                // send the response to it.
                typed_op("listen", &["port"], "Bool"),
                op("accept", &["port"]),
                typed_op("send", &["status", "body"], "Bool"),
                op("http-serve", &["port"]),
                op("respond", &["id", "response"]),
                op("serve-file", &["id", "path", "content-type"]),
                op("sse-open", &["id"]),
                op("sse-send", &["id", "event", "data"]),
                op("sse-broadcast", &["event", "data"]),
            ],
        });
        // Rand effect — all randomness flows through an effect (never a pure
        // builtin) so record/replay stays deterministic and tests can handle
        // it with canned values.
        reg.register(EffectDecl {
            name: "Rand".to_string(),
            operations: vec![
                typed_op("rand", &[], "Float"),
                typed_op("rand-int", &["lo", "hi"], "Int"),
                op("seed", &["n"]),
            ],
        });
        // Place effect — where a kernel runs is a decision a handler makes,
        // not a property of the program. Unhandled, it runs serially on the
        // CPU, so a program that never mentions placement still works.
        reg.register(EffectDecl {
            name: "Place".to_string(),
            operations: vec![
                // [Place.run kernel n args...] — run `kernel` once per index
                // in 0..n. Returns unit; kernels write through buffers.
                op("run", &["kernel", "n", "args"]),
                // [Place.read buf] — the only way to get buffer contents back
                // to the host. Being an operation is the point: a residency
                // handler learns where every synchronization point is without
                // the programmer marking any of them.
                op("read", &["buf"]),
                // [Place.pin buf] / [Place.unpin buf] — hints that a buffer
                // should stay where it is between launches.
                op("pin", &["buf"]),
                op("unpin", &["buf"]),
                // [Place.stats] — transfer and launch counters so far.
                op("stats", &[]),
            ],
        });
        // Host effect — the seam an asynchronous embedder needs.
        //
        // `Host.park` takes a continuation and keeps it. The handler that
        // performs it returns without resuming, so the computation unwinds and
        // the host decides when the rest of it runs. That is the whole
        // mechanism behind answering an operation that cannot be answered yet,
        // like reading back a GPU buffer in a browser.
        reg.register(EffectDecl {
            name: "Host".to_string(),
            operations: vec![op("park", &["continuation", "request"])],
        });
        reg.register(EffectDecl {
            name: "Embed".to_string(),
            operations: vec![op("encode", &["text"])],
        });
        // Physics effect — material properties and physical constants (swappable per environment)
        reg.register(EffectDecl {
            name: "Physics".to_string(),
            operations: vec![
                typed_op("gravity", &[], "Acceleration"),
                typed_op("yield-strength", &[], "Pressure"),
                typed_op("elastic-modulus", &[], "Pressure"),
                typed_op("density", &[], "Density"),
                typed_op("temperature", &[], "Temperature"),
                typed_op("thermal-conductivity", &[], "ThermalConductivity"),
            ],
        });
        // Sim effect — simulation operations (swappable between analytical/numerical/phyz)
        reg.register(EffectDecl {
            name: "Sim".to_string(),
            operations: vec![
                typed_op("stress", &["geometry", "material", "load"], "Pressure"),
                typed_op("deflection", &["geometry", "material", "load"], "Length"),
                typed_op("natural-freq", &["geometry", "material"], "Frequency"),
                typed_op(
                    "thermal-field",
                    &["geometry", "material", "sources"],
                    "Temperature",
                ),
            ],
        });
        reg
    }

    pub fn register(&mut self, decl: EffectDecl) {
        for op in &decl.operations {
            let qualified = format!("{}.{}", decl.name, op.name);
            self.op_lookup
                .insert(qualified, (decl.name.clone(), op.name.clone()));
        }
        self.effects.insert(decl.name.clone(), decl);
    }

    pub fn lookup_op(&self, qualified_name: &str) -> Option<(&str, &str)> {
        self.op_lookup
            .get(qualified_name)
            .map(|(e, o)| (e.as_str(), o.as_str()))
    }

    pub fn get_effect(&self, name: &str) -> Option<&EffectDecl> {
        self.effects.get(name)
    }

    /// Look up a specific operation by effect name and op name.
    pub fn get_op(&self, effect: &str, op: &str) -> Option<&EffectOp> {
        self.effects
            .get(effect)
            .and_then(|decl| decl.operations.iter().find(|o| o.name == op))
    }

    /// Check if an effect is registered.
    pub fn has_effect(&self, name: &str) -> bool {
        self.effects.contains_key(name)
    }
}

/// SplitMix64 step — the shared PRNG behind the `Rand` builtin effect.
/// Both backends use this exact generator so a seeded program produces
/// identical values under the interpreter and the EIR VM.
pub fn splitmix_next(state: &mut u64) -> u64 {
    *state = state.wrapping_add(0x9E37_79B9_7F4A_7C15);
    let mut z = *state;
    z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
    z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
    z ^ (z >> 31)
}

/// Uniform f64 in [0, 1) from a SplitMix64 state.
pub fn splitmix_f64(state: &mut u64) -> f64 {
    (splitmix_next(state) >> 11) as f64 / (1u64 << 53) as f64
}

/// Uniform i64 in [lo, hi) from a SplitMix64 state (lo when the range is
/// empty, matching a "no throw" builtin-effect contract).
pub fn splitmix_range(state: &mut u64, lo: i64, hi: i64) -> i64 {
    if hi <= lo {
        return lo;
    }
    let span = (hi - lo) as u64;
    lo + (splitmix_next(state) % span) as i64
}

/// A time-derived seed for unseeded `Rand` use.
pub fn entropy_seed() -> u64 {
    use std::time::{SystemTime, UNIX_EPOCH};
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos() as u64)
        .unwrap_or(0x5EED)
        | 1
}

/// Represents a performed effect that needs handling
#[derive(Debug, Clone)]
pub struct PerformEffect {
    pub effect: String,
    pub operation: String,
    pub args: Vec<crate::interp::Value>,
}
