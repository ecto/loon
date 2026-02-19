use std::collections::HashMap;
use std::path::{Path, PathBuf};

use super::manifest::{Dependency, Manifest};
use super::version::{Version, VersionConstraint};

/// Resolved dependency graph after MVS.
#[derive(Debug, Clone)]
pub struct ResolvedGraph {
    pub packages: HashMap<String, ResolvedPackage>,
}

/// A single resolved package in the graph.
#[derive(Debug, Clone)]
pub struct ResolvedPackage {
    pub source: String,
    pub version: Version,
    pub deps: Vec<String>,
    /// Path to cached package directory (if fetched).
    pub cached_path: Option<PathBuf>,
    /// BLAKE3 hash.
    pub hash: Option<String>,
}

/// Minimum Version Selection (MVS) resolver.
///
/// Given a root manifest, recursively resolve all transitive dependencies,
/// selecting the minimum version that satisfies all constraints.
pub struct Resolver {
    /// All version constraints collected for each source.
    constraints: HashMap<String, Vec<VersionConstraint>>,
    /// Resolved packages so far.
    resolved: HashMap<String, ResolvedPackage>,
    /// Deps being resolved (cycle detection).
    in_progress: Vec<String>,
}

impl Resolver {
    pub fn new() -> Self {
        Self {
            constraints: HashMap::new(),
            resolved: HashMap::new(),
            in_progress: Vec::new(),
        }
    }

    /// Resolve all dependencies starting from the root manifest.
    /// `project_root` is where pkg.loon lives (for resolving path deps).
    pub fn resolve(
        &mut self,
        manifest: &Manifest,
        project_root: &Path,
    ) -> Result<ResolvedGraph, String> {
        // Collect and resolve direct deps
        for (source, dep) in &manifest.deps {
            self.resolve_dep(source, dep, project_root)?;
        }

        Ok(ResolvedGraph {
            packages: self.resolved.clone(),
        })
    }

    fn resolve_dep(
        &mut self,
        source: &str,
        dep: &Dependency,
        _project_root: &Path,
    ) -> Result<(), String> {
        // Cycle detection
        if self.in_progress.contains(&source.to_string()) {
            return Err(format!("circular dependency detected: {source}"));
        }

        // Collect constraint
        if let Some(ref vc) = dep.version {
            self.constraints
                .entry(source.to_string())
                .or_default()
                .push(vc.clone());
        }

        // Already resolved? Check if existing version still satisfies all constraints
        if let Some(existing) = self.resolved.get(source) {
            let all_satisfied = self
                .constraints
                .get(source)
                .map(|cs| cs.iter().all(|c| c.matches(&existing.version)))
                .unwrap_or(true);
            if all_satisfied {
                return Ok(());
            }
            // Need to bump — pick a new minimum that satisfies all constraints
            let new_version = self.pick_version(source)?;
            self.resolved.get_mut(source).unwrap().version = new_version;
            return Ok(());
        }

        // Path deps: resolve locally, look for their pkg.loon for transitive deps
        if dep.is_path_dep() {
            let dep_dir = dep.path.as_ref().unwrap();
            let version = dep
                .version
                .as_ref()
                .map(|v| v.minimum())
                .unwrap_or_else(|| Version::new(0, 0, 0));

            self.resolved.insert(
                source.to_string(),
                ResolvedPackage {
                    source: source.to_string(),
                    version,
                    deps: vec![],
                    cached_path: Some(dep_dir.clone()),
                    hash: None,
                },
            );

            // Check for transitive deps in the path dep's pkg.loon
            self.resolve_transitive(source, dep_dir)?;
            return Ok(());
        }

        // Remote deps: fetch, hash, cache, then resolve transitive deps
        if super::fetch::is_domain_qualified(source) {
            self.in_progress.push(source.to_string());

            #[cfg(feature = "pkg-fetch")]
            {
                let (cached_path, hash) = super::fetch::fetch_and_cache(source, dep)?;
                let version = dep
                    .version
                    .as_ref()
                    .map(|v| v.minimum())
                    .unwrap_or_else(|| Version::new(0, 0, 0));

                self.resolved.insert(
                    source.to_string(),
                    ResolvedPackage {
                        source: source.to_string(),
                        version,
                        deps: vec![],
                        cached_path: Some(cached_path.clone()),
                        hash: Some(hash),
                    },
                );

                // Parse the fetched package's pkg.loon for transitive deps
                let (_, subpath) = super::fetch::parse_source_name(source);
                let dep_root = if let Some(sub) = subpath {
                    cached_path.join(sub)
                } else {
                    cached_path
                };
                self.resolve_transitive(source, &dep_root)?;
            }

            #[cfg(not(feature = "pkg-fetch"))]
            {
                let version = dep
                    .version
                    .as_ref()
                    .map(|v| v.minimum())
                    .unwrap_or_else(|| Version::new(0, 0, 0));

                self.resolved.insert(
                    source.to_string(),
                    ResolvedPackage {
                        source: source.to_string(),
                        version,
                        deps: vec![],
                        cached_path: None,
                        hash: None,
                    },
                );
            }

            self.in_progress.retain(|s| s != source);
            return Ok(());
        }

        // Non-domain, non-path dep — just record it
        let version = dep
            .version
            .as_ref()
            .map(|v| v.minimum())
            .unwrap_or_else(|| Version::new(0, 0, 0));

        self.resolved.insert(
            source.to_string(),
            ResolvedPackage {
                source: source.to_string(),
                version,
                deps: vec![],
                cached_path: None,
                hash: None,
            },
        );

        Ok(())
    }

    /// Parse a dependency's pkg.loon and recursively resolve its deps.
    fn resolve_transitive(&mut self, parent: &str, dep_dir: &Path) -> Result<(), String> {
        let manifest = match Manifest::load(dep_dir) {
            Ok(Some(m)) => m,
            Ok(None) => return Ok(()), // no pkg.loon = no transitive deps
            Err(_) => return Ok(()),   // can't parse = skip
        };

        let mut child_deps = Vec::new();
        for (child_source, child_dep) in &manifest.deps {
            child_deps.push(child_source.clone());
            self.resolve_dep(child_source, child_dep, dep_dir)?;
        }

        // Record this package's deps in the resolved graph
        if let Some(pkg) = self.resolved.get_mut(parent) {
            pkg.deps = child_deps;
        }

        Ok(())
    }

    /// Pick the minimum version that satisfies all constraints for a source.
    fn pick_version(&self, source: &str) -> Result<Version, String> {
        let constraints = self.constraints.get(source);
        match constraints {
            None => Ok(Version::new(0, 0, 0)),
            Some(cs) => {
                // MVS: start from the highest minimum and verify all constraints
                let mut candidate = Version::new(0, 0, 0);
                for c in cs {
                    let min = c.minimum();
                    if min > candidate {
                        candidate = min;
                    }
                }

                // Verify the candidate satisfies all constraints
                for c in cs {
                    if !c.matches(&candidate) {
                        return Err(format!(
                            "version conflict for '{source}': {} is required but conflicts with {c}",
                            candidate
                        ));
                    }
                }

                Ok(candidate)
            }
        }
    }
}

impl Default for Resolver {
    fn default() -> Self {
        Self::new()
    }
}

/// Convenience function: resolve a manifest's full dependency graph.
pub fn resolve(manifest: &Manifest, project_root: &Path) -> Result<ResolvedGraph, String> {
    let mut resolver = Resolver::new();
    resolver.resolve(manifest, project_root)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_dep(version: &str) -> Dependency {
        Dependency {
            source: "test".to_string(),
            version: Some(VersionConstraint::parse(version).unwrap()),
            grant: vec![],
            path: None,
            git: None,
            rev: None,
            url: None,
            hash: None,
        }
    }

    #[test]
    fn mvs_picks_minimum() {
        let mut resolver = Resolver::new();
        resolver
            .constraints
            .insert("pkg".to_string(), vec![
                VersionConstraint::parse("^1.0").unwrap(),
                VersionConstraint::parse("^1.2").unwrap(),
            ]);
        let v = resolver.pick_version("pkg").unwrap();
        assert_eq!(v, Version::new(1, 2, 0));
    }

    #[test]
    fn mvs_detects_conflict() {
        let mut resolver = Resolver::new();
        resolver
            .constraints
            .insert("pkg".to_string(), vec![
                VersionConstraint::parse("^1.0").unwrap(),
                VersionConstraint::parse("^2.0").unwrap(),
            ]);
        assert!(resolver.pick_version("pkg").is_err());
    }

    #[test]
    fn resolve_empty_manifest() {
        let manifest = Manifest {
            name: "test".to_string(),
            version: "0.1.0".to_string(),
            deps: HashMap::new(),
            indices: HashMap::new(),
        };
        let graph = resolve(&manifest, Path::new(".")).unwrap();
        assert!(graph.packages.is_empty());
    }

    #[test]
    fn resolve_path_dep() {
        // Create a temp dir with a path dep that has its own pkg.loon
        let tmp = std::env::temp_dir().join("loon-resolve-test");
        let dep_dir = tmp.join("my-lib");
        let dep_src = dep_dir.join("src");
        let _ = std::fs::remove_dir_all(&tmp);
        std::fs::create_dir_all(&dep_src).unwrap();
        std::fs::write(
            dep_dir.join("pkg.loon"),
            r#"{:name "my-lib" :version "1.0.0" :deps {}}"#,
        )
        .unwrap();
        std::fs::write(dep_src.join("lib.loon"), "[pub fn hello [] 42]").unwrap();

        let mut deps = HashMap::new();
        deps.insert(
            "my-lib".to_string(),
            Dependency {
                source: "my-lib".to_string(),
                version: Some(VersionConstraint::parse("^1.0").unwrap()),
                grant: vec![],
                path: Some(dep_dir.clone()),
                git: None,
                rev: None,
                url: None,
                hash: None,
            },
        );

        let manifest = Manifest {
            name: "test-app".to_string(),
            version: "0.1.0".to_string(),
            deps,
            indices: HashMap::new(),
        };

        let graph = resolve(&manifest, &tmp).unwrap();
        assert!(graph.packages.contains_key("my-lib"));
        assert_eq!(graph.packages["my-lib"].version, Version::new(1, 0, 0));

        let _ = std::fs::remove_dir_all(&tmp);
    }
}
