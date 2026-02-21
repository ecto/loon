use std::path::Path;

use crate::ast::ExprKind;
use crate::parser;

use super::version::Version;

/// A resolved, locked package entry.
#[derive(Debug, Clone)]
pub struct LockedPackage {
    pub source: String,
    pub version: Version,
    pub url: String,
    pub subpath: Option<String>,
    pub hash: String,
    pub deps: Vec<String>,
}

/// The lock.oo file — deterministic, content-addressed dependency snapshot.
#[derive(Debug, Clone)]
pub struct Lockfile {
    pub version: u32,
    pub packages: Vec<LockedPackage>,
}

impl Lockfile {
    pub fn new() -> Self {
        Self {
            version: 1,
            packages: vec![],
        }
    }

    /// Load lock.oo (or lock.loon) from a directory, returning None if not found.
    pub fn load(dir: &Path) -> Result<Option<Self>, String> {
        let lock_path = dir.join("lock.oo");
        let lock_path = if lock_path.exists() { lock_path } else { dir.join("lock.loon") };
        if !lock_path.exists() {
            return Ok(None);
        }
        let source = std::fs::read_to_string(&lock_path)
            .map_err(|e| format!("cannot read {}: {e}", lock_path.display()))?;
        Self::parse(&source).map(Some)
    }

    /// Parse a lock.loon source string.
    fn parse(source: &str) -> Result<Self, String> {
        let exprs = parser::parse(source)
            .map_err(|e| format!("lock.oo parse error: {}", e.message))?;

        if exprs.len() != 1 {
            return Err("lock.oo must contain exactly one map expression".into());
        }

        let pairs = match &exprs[0].kind {
            ExprKind::Map(pairs) => pairs,
            _ => return Err("lock.oo must be a map {...}".into()),
        };

        let mut version = 1u32;
        let mut packages = Vec::new();

        for (key, val) in pairs {
            let key_str = match &key.kind {
                ExprKind::Keyword(k) => k.as_str(),
                _ => continue,
            };

            match key_str {
                "version" => {
                    if let ExprKind::Int(n) = &val.kind {
                        version = *n as u32;
                    }
                }
                "packages" => {
                    if let ExprKind::Vec(items) = &val.kind {
                        for item in items {
                            if let ExprKind::Map(pkg_pairs) = &item.kind {
                                packages.push(parse_locked_package(pkg_pairs)?);
                            }
                        }
                    }
                }
                _ => {}
            }
        }

        Ok(Lockfile { version, packages })
    }

    /// Write lock.oo to a directory.
    pub fn write(&self, dir: &Path) -> Result<(), String> {
        let lock_path = dir.join("lock.oo");
        let content = self.serialize();
        std::fs::write(&lock_path, content)
            .map_err(|e| format!("writing {}: {e}", lock_path.display()))
    }

    /// Serialize to Loon data format.
    fn serialize(&self) -> String {
        let mut out = String::new();
        out.push_str("{\n");
        out.push_str(&format!("  :version {}\n", self.version));
        out.push_str("  :packages #[\n");
        for pkg in &self.packages {
            out.push_str("    {\n");
            out.push_str(&format!("      :source \"{}\"\n", pkg.source));
            out.push_str(&format!("      :version \"{}\"\n", pkg.version));
            out.push_str(&format!("      :url \"{}\"\n", pkg.url));
            if let Some(ref sub) = pkg.subpath {
                out.push_str(&format!("      :subpath \"{sub}\"\n"));
            }
            out.push_str(&format!("      :hash \"{}\"\n", pkg.hash));
            if !pkg.deps.is_empty() {
                let dep_strs: Vec<String> = pkg.deps.iter().map(|d| format!("\"{d}\"")).collect();
                out.push_str(&format!("      :deps #[{}]\n", dep_strs.join(" ")));
            } else {
                out.push_str("      :deps #[]\n");
            }
            out.push_str("    }\n");
        }
        out.push_str("  ]\n");
        out.push_str("}\n");
        out
    }

    /// Look up a package by source name.
    pub fn get(&self, source: &str) -> Option<&LockedPackage> {
        self.packages.iter().find(|p| p.source == source)
    }

    /// Add or update a locked package entry.
    pub fn upsert(&mut self, pkg: LockedPackage) {
        if let Some(existing) = self.packages.iter_mut().find(|p| p.source == pkg.source) {
            *existing = pkg;
        } else {
            self.packages.push(pkg);
        }
    }
}

fn parse_locked_package(
    pairs: &[(crate::ast::Expr, crate::ast::Expr)],
) -> Result<LockedPackage, String> {
    let mut source = String::new();
    let mut version = Version::new(0, 0, 0);
    let mut url = String::new();
    let mut subpath = None;
    let mut hash = String::new();
    let mut deps = Vec::new();

    for (k, v) in pairs {
        let key = match &k.kind {
            ExprKind::Keyword(s) => s.as_str(),
            _ => continue,
        };
        match key {
            "source" => {
                if let ExprKind::Str(s) = &v.kind {
                    source = s.clone();
                }
            }
            "version" => {
                if let ExprKind::Str(s) = &v.kind {
                    version = Version::parse(s)?;
                }
            }
            "url" => {
                if let ExprKind::Str(s) = &v.kind {
                    url = s.clone();
                }
            }
            "subpath" => {
                if let ExprKind::Str(s) = &v.kind {
                    subpath = Some(s.clone());
                }
            }
            "hash" => {
                if let ExprKind::Str(s) = &v.kind {
                    hash = s.clone();
                }
            }
            "deps" => {
                if let ExprKind::Vec(items) = &v.kind {
                    for item in items {
                        if let ExprKind::Str(s) = &item.kind {
                            deps.push(s.clone());
                        }
                    }
                }
            }
            _ => {}
        }
    }

    if source.is_empty() {
        return Err("locked package missing :source".into());
    }

    Ok(LockedPackage {
        source,
        version,
        url,
        subpath,
        hash,
        deps,
    })
}

impl Default for Lockfile {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn roundtrip_lockfile() {
        let mut lf = Lockfile::new();
        lf.upsert(LockedPackage {
            source: "github.com/cam/json".to_string(),
            version: Version::new(1, 0, 0),
            url: "https://github.com/cam/json.git".to_string(),
            subpath: None,
            hash: "abc123".to_string(),
            deps: vec![],
        });

        let serialized = lf.serialize();
        let parsed = Lockfile::parse(&serialized).unwrap();
        assert_eq!(parsed.version, 1);
        assert_eq!(parsed.packages.len(), 1);
        assert_eq!(parsed.packages[0].source, "github.com/cam/json");
        assert_eq!(parsed.packages[0].hash, "abc123");
    }

    #[test]
    fn upsert_replaces_existing() {
        let mut lf = Lockfile::new();
        lf.upsert(LockedPackage {
            source: "github.com/cam/json".to_string(),
            version: Version::new(1, 0, 0),
            url: "".to_string(),
            subpath: None,
            hash: "old".to_string(),
            deps: vec![],
        });
        lf.upsert(LockedPackage {
            source: "github.com/cam/json".to_string(),
            version: Version::new(1, 1, 0),
            url: "".to_string(),
            subpath: None,
            hash: "new".to_string(),
            deps: vec![],
        });
        assert_eq!(lf.packages.len(), 1);
        assert_eq!(lf.packages[0].hash, "new");
    }

    #[test]
    fn parse_with_subpath() {
        let source = r#"{
  :version 1
  :packages #[
    {
      :source "github.com/cam/std#http"
      :version "1.0.0"
      :url "https://github.com/cam/std.git"
      :subpath "http"
      :hash "deadbeef"
      :deps #["github.com/cam/json"]
    }
  ]
}"#;
        let lf = Lockfile::parse(source).unwrap();
        assert_eq!(lf.packages.len(), 1);
        assert_eq!(lf.packages[0].subpath, Some("http".to_string()));
        assert_eq!(lf.packages[0].deps, vec!["github.com/cam/json"]);
    }
}
