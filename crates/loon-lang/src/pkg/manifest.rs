use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::ast::ExprKind;
use crate::parser;

use super::version::VersionConstraint;

/// A parsed pkg.oo manifest.
#[derive(Debug, Clone)]
pub struct Manifest {
    pub name: String,
    pub version: String,
    pub deps: HashMap<String, Dependency>,
    pub indices: HashMap<String, String>,
}

/// A dependency entry from pkg.oo.
#[derive(Debug, Clone)]
pub struct Dependency {
    /// The source identifier (e.g. "github.com/cam/json")
    pub source: String,
    /// Version constraint (e.g. "^1.2")
    pub version: Option<VersionConstraint>,
    /// Granted effects (empty = pure/default-deny)
    pub grant: Vec<String>,
    /// Local path dependency
    pub path: Option<PathBuf>,
    /// Git URL
    pub git: Option<String>,
    /// Git revision
    pub rev: Option<String>,
    /// Direct URL
    pub url: Option<String>,
    /// BLAKE3 hash
    pub hash: Option<String>,
}

impl Dependency {
    pub fn is_path_dep(&self) -> bool {
        self.path.is_some()
    }
}

impl Manifest {
    /// Load a pkg.oo (or pkg.loon) from a directory, returning None if not found.
    pub fn load(dir: &Path) -> Result<Option<Self>, String> {
        let manifest_path = dir.join("pkg.oo");
        let manifest_path = if manifest_path.exists() { manifest_path } else { dir.join("pkg.loon") };
        if !manifest_path.exists() {
            return Ok(None);
        }
        let source = std::fs::read_to_string(&manifest_path)
            .map_err(|e| format!("cannot read {}: {e}", manifest_path.display()))?;
        Self::parse(&source, dir).map(Some)
    }

    /// Parse manifest source text.
    pub fn parse(source: &str, base_dir: &Path) -> Result<Self, String> {
        let exprs = parser::parse(source).map_err(|e| format!("pkg.oo parse error: {}", e.message))?;

        if exprs.len() != 1 {
            return Err("pkg.oo must contain exactly one map expression".into());
        }

        let pairs = match &exprs[0].kind {
            ExprKind::Map(pairs) => pairs,
            _ => return Err("pkg.oo must be a map {...}".into()),
        };

        let mut name = String::new();
        let mut version = String::new();
        let mut deps = HashMap::new();
        let mut indices = HashMap::new();

        for (key, val) in pairs {
            let key_str = match &key.kind {
                ExprKind::Keyword(k) => k.as_str(),
                _ => continue,
            };

            match key_str {
                "name" => {
                    if let ExprKind::Str(s) = &val.kind {
                        name = s.clone();
                    }
                }
                "version" => {
                    if let ExprKind::Str(s) = &val.kind {
                        version = s.clone();
                    }
                }
                "deps" => {
                    if let ExprKind::Map(dep_pairs) = &val.kind {
                        for (dk, dv) in dep_pairs {
                            let dep_name = match &dk.kind {
                                ExprKind::Str(s) => s.clone(),
                                _ => continue,
                            };
                            let dep = parse_dep(&dep_name, dv, base_dir)?;
                            deps.insert(dep_name, dep);
                        }
                    }
                }
                "indices" => {
                    if let ExprKind::Map(idx_pairs) = &val.kind {
                        for (ik, iv) in idx_pairs {
                            if let (ExprKind::Str(name), ExprKind::Str(url)) =
                                (&ik.kind, &iv.kind)
                            {
                                indices.insert(name.clone(), url.clone());
                            }
                        }
                    }
                }
                _ => {} // ignore unknown keys
            }
        }

        if name.is_empty() {
            return Err("pkg.oo must have a :name field".into());
        }

        Ok(Manifest {
            name,
            version,
            deps,
            indices,
        })
    }
}

fn parse_dep(
    source: &str,
    val: &crate::ast::Expr,
    base_dir: &Path,
) -> Result<Dependency, String> {
    match &val.kind {
        // String shorthand: "^1.2" — pure dep, version constraint only
        ExprKind::Str(s) => {
            let version = VersionConstraint::parse(s)?;
            Ok(Dependency {
                source: source.to_string(),
                version: Some(version),
                grant: vec![],
                path: None,
                git: None,
                rev: None,
                url: None,
                hash: None,
            })
        }
        // Map form: {:version "^1.0" :grant #["Net" "IO"] :path "../lib" ...}
        ExprKind::Map(pairs) => {
            let mut dep = Dependency {
                source: source.to_string(),
                version: None,
                grant: vec![],
                path: None,
                git: None,
                rev: None,
                url: None,
                hash: None,
            };

            for (k, v) in pairs {
                let key = match &k.kind {
                    ExprKind::Keyword(s) => s.as_str(),
                    _ => continue,
                };
                match key {
                    "version" => {
                        if let ExprKind::Str(s) = &v.kind {
                            dep.version = Some(VersionConstraint::parse(s)?);
                        }
                    }
                    "grant" => {
                        if let ExprKind::Vec(items) = &v.kind {
                            for item in items {
                                if let ExprKind::Str(s) = &item.kind {
                                    dep.grant.push(s.clone());
                                }
                            }
                        }
                    }
                    "path" => {
                        if let ExprKind::Str(s) = &v.kind {
                            let p = PathBuf::from(s);
                            let resolved = if p.is_absolute() {
                                p
                            } else {
                                base_dir.join(p)
                            };
                            dep.path = Some(resolved);
                        }
                    }
                    "git" => {
                        if let ExprKind::Str(s) = &v.kind {
                            dep.git = Some(s.clone());
                        }
                    }
                    "rev" => {
                        if let ExprKind::Str(s) = &v.kind {
                            dep.rev = Some(s.clone());
                        }
                    }
                    "url" => {
                        if let ExprKind::Str(s) = &v.kind {
                            dep.url = Some(s.clone());
                        }
                    }
                    "hash" => {
                        if let ExprKind::Str(s) = &v.kind {
                            dep.hash = Some(s.clone());
                        }
                    }
                    _ => {} // ignore unknown keys
                }
            }

            Ok(dep)
        }
        _ => Err(format!(
            "dependency '{source}' must be a version string or map"
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_minimal_manifest() {
        let source = r#"{
  :name "my-app"
  :version "0.1.0"
}"#;
        let m = Manifest::parse(source, Path::new(".")).unwrap();
        assert_eq!(m.name, "my-app");
        assert_eq!(m.version, "0.1.0");
        assert!(m.deps.is_empty());
    }

    #[test]
    fn parse_deps_string_shorthand() {
        let source = r#"{
  :name "my-app"
  :version "0.1.0"
  :deps {
    "github.com/cam/json" "^1.2"
  }
}"#;
        let m = Manifest::parse(source, Path::new(".")).unwrap();
        assert_eq!(m.deps.len(), 1);
        let dep = m.deps.get("github.com/cam/json").unwrap();
        assert!(dep.version.is_some());
        assert!(dep.grant.is_empty());
    }

    #[test]
    fn parse_deps_map_with_grant() {
        let source = r#"{
  :name "my-app"
  :version "0.1.0"
  :deps {
    "github.com/cam/std#http" {:version "^1.0" :grant #["Net" "IO"]}
  }
}"#;
        let m = Manifest::parse(source, Path::new(".")).unwrap();
        let dep = m.deps.get("github.com/cam/std#http").unwrap();
        assert_eq!(dep.grant, vec!["Net", "IO"]);
    }

    #[test]
    fn parse_deps_path() {
        let source = r#"{
  :name "my-app"
  :version "0.1.0"
  :deps {
    "my-lib" {:path "../my-lib"}
  }
}"#;
        let m = Manifest::parse(source, Path::new("/project")).unwrap();
        let dep = m.deps.get("my-lib").unwrap();
        assert!(dep.is_path_dep());
        assert_eq!(dep.path.as_ref().unwrap(), &PathBuf::from("/project/../my-lib"));
    }

    #[test]
    fn parse_indices() {
        let source = r#"{
  :name "my-app"
  :version "0.1.0"
  :indices {
    "company" "https://git.internal.co/loon-packages/index.loon"
  }
}"#;
        let m = Manifest::parse(source, Path::new(".")).unwrap();
        assert_eq!(
            m.indices.get("company").unwrap(),
            "https://git.internal.co/loon-packages/index.loon"
        );
    }

    #[test]
    fn missing_name_errors() {
        let source = r#"{:version "0.1.0"}"#;
        assert!(Manifest::parse(source, Path::new(".")).is_err());
    }
}
