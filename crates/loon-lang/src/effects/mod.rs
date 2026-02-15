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
    pub params: Vec<String>,
}

/// Registry of declared effects
#[derive(Debug, Clone, Default)]
pub struct EffectRegistry {
    effects: HashMap<String, EffectDecl>,
    /// Maps "Effect.op" → (effect_name, op_name)
    op_lookup: HashMap<String, (String, String)>,
}

impl EffectRegistry {
    pub fn new() -> Self {
        let mut reg = Self::default();
        // Built-in effects
        reg.register(EffectDecl {
            name: "IO".to_string(),
            operations: vec![
                EffectOp {
                    name: "println".to_string(),
                    params: vec!["msg".to_string()],
                },
                EffectOp {
                    name: "read-line".to_string(),
                    params: vec![],
                },
                EffectOp {
                    name: "read-file".to_string(),
                    params: vec!["path".to_string()],
                },
                EffectOp {
                    name: "write-file".to_string(),
                    params: vec!["path".to_string(), "content".to_string()],
                },
            ],
        });
        reg.register(EffectDecl {
            name: "Fail".to_string(),
            operations: vec![EffectOp {
                name: "fail".to_string(),
                params: vec!["msg".to_string()],
            }],
        });
        reg.register(EffectDecl {
            name: "Process".to_string(),
            operations: vec![
                EffectOp {
                    name: "args".to_string(),
                    params: vec![],
                },
                EffectOp {
                    name: "env".to_string(),
                    params: vec!["key".to_string()],
                },
                EffectOp {
                    name: "exit".to_string(),
                    params: vec!["code".to_string()],
                },
            ],
        });
        reg.register(EffectDecl {
            name: "Async".to_string(),
            operations: vec![EffectOp {
                name: "await".to_string(),
                params: vec!["future".to_string()],
            }],
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
}

/// Represents a performed effect that needs handling
#[derive(Debug, Clone)]
pub struct PerformEffect {
    pub effect: String,
    pub operation: String,
    pub args: Vec<crate::interp::Value>,
}
