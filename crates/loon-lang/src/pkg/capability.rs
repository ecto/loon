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

/// A capability violation: a transitive dep needs an effect not granted by its parent.
#[derive(Debug, Clone)]
pub struct CapabilityViolation {
    /// The dependency that needs the effect.
    pub source: String,
    /// The effect it needs.
    pub effect_needed: String,
    /// The parent that should grant it.
    pub parent: String,
    /// Human-readable explanation.
    pub reason: String,
}

/// Check transitive dependencies for ungrated capability requirements.
///
/// Walks the lockfile dep graph. For each transitive dep that has its own
/// pkg.loon in cache declaring `:grant` needs, checks that its parent
/// grants those effects. Returns violations.
pub fn check_transitive_grants(
    lockfile: &super::lockfile::Lockfile,
    root_manifest: &super::manifest::Manifest,
) -> Vec<CapabilityViolation> {
    let mut violations = Vec::new();

    // Build a map of source -> granted effects from the root manifest
    let root_grants = grants_from_manifest(&root_manifest.deps);

    // For each locked package, check if it has transitive deps that need effects
    for pkg in &lockfile.packages {
        // What effects does the root grant to this package?
        let granted_to_pkg = root_grants.get(&pkg.source);

        // Check each transitive dep of this package
        for dep_source in &pkg.deps {
            // Try to load the transitive dep's manifest from cache to see what it needs
            if let Some(dep_locked) = lockfile.get(dep_source) {
                if let Some(cached_dir) = super::fetch::cached_path(&dep_locked.hash) {
                    // Try to parse the dep's pkg.loon
                    let manifest_path = cached_dir.join("pkg.loon");
                    if manifest_path.exists() {
                        if let Ok(source) = std::fs::read_to_string(&manifest_path) {
                            if let Ok(dep_manifest) =
                                super::manifest::Manifest::parse(&source, &cached_dir)
                            {
                                // Check what grants the dep's own deps expect
                                for (sub_dep_name, sub_dep) in &dep_manifest.deps {
                                    for effect in &sub_dep.grant {
                                        // The parent (pkg) must have been granted this effect
                                        // for it to propagate to the sub-dep
                                        if !granted_to_pkg.contains(effect) {
                                            violations.push(CapabilityViolation {
                                                source: sub_dep_name.clone(),
                                                effect_needed: effect.clone(),
                                                parent: pkg.source.clone(),
                                                reason: format!(
                                                    "'{}' grants '{}' to '{}', but '{}' itself is not \
                                                     granted '{}' by the root manifest",
                                                    pkg.source, effect, sub_dep_name,
                                                    pkg.source, effect
                                                ),
                                            });
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    violations
}
