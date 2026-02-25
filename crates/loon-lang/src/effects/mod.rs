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
    }
}

/// Helper to build a typed EffectOp with typed params and return type
fn typed_op(name: &str, param_names: &[&str], ret: &str) -> EffectOp {
    EffectOp {
        name: name.to_string(),
        params: params(param_names),
        return_type: Some(ret.to_string()),
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
                op("http-serve", &["port"]),
                op("respond", &["id", "response"]),
                op("serve-file", &["id", "path", "content-type"]),
                op("sse-open", &["id"]),
                op("sse-send", &["id", "event", "data"]),
                op("sse-broadcast", &["event", "data"]),
            ],
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
                typed_op("thermal-field", &["geometry", "material", "sources"], "Temperature"),
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

/// Represents a performed effect that needs handling
#[derive(Debug, Clone)]
pub struct PerformEffect {
    pub effect: String,
    pub operation: String,
    pub args: Vec<crate::interp::Value>,
}
