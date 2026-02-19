use std::collections::{HashMap, HashSet};

/// Effect grants for a dependency — the set of effects it's allowed to use.
#[derive(Debug, Clone)]
pub struct EffectGrants {
    /// Map from dep source to granted effects. Missing = pure (no effects).
    pub grants: HashMap<String, HashSet<String>>,
}

impl EffectGrants {
    pub fn new() -> Self {
        Self {
            grants: HashMap::new(),
        }
    }

    /// Get granted effects for a dependency. Returns empty set if not found (pure).
    pub fn get(&self, source: &str) -> HashSet<String> {
        self.grants.get(source).cloned().unwrap_or_default()
    }

    /// Check if a dep is allowed to use a specific effect.
    pub fn is_allowed(&self, source: &str, effect: &str) -> bool {
        match self.grants.get(source) {
            Some(allowed) => allowed.contains(effect),
            None => false, // default-deny
        }
    }
}

impl Default for EffectGrants {
    fn default() -> Self {
        Self::new()
    }
}

/// Build an EffectGrants map from a manifest's deps.
pub fn grants_from_manifest(
    deps: &HashMap<String, super::manifest::Dependency>,
) -> EffectGrants {
    let mut grants = EffectGrants::new();
    for (source, dep) in deps {
        if !dep.grant.is_empty() {
            let set: HashSet<String> = dep.grant.iter().cloned().collect();
            grants.grants.insert(source.clone(), set);
        }
    }
    grants
}
