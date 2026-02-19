use crate::ast::ExprKind;

/// A package entry in an index.
#[derive(Debug, Clone)]
pub struct IndexEntry {
    pub source: String,
    pub description: String,
    pub license: Option<String>,
    pub keywords: Vec<String>,
    pub versions: Vec<IndexVersion>,
}

#[derive(Debug, Clone)]
pub struct IndexVersion {
    pub version: String,
    pub hash: String,
    pub published: Option<String>,
    pub yanked: bool,
}

/// A package index (built-in or custom).
#[derive(Debug, Clone)]
pub struct PackageIndex {
    pub name: String,
    pub entries: Vec<IndexEntry>,
}

impl PackageIndex {
    /// Search entries by query (matches source, description, keywords).
    pub fn search(&self, query: &str) -> Vec<&IndexEntry> {
        let q = query.to_lowercase();
        self.entries
            .iter()
            .filter(|e| {
                e.source.to_lowercase().contains(&q)
                    || e.description.to_lowercase().contains(&q)
                    || e.keywords.iter().any(|k| k.to_lowercase().contains(&q))
            })
            .collect()
    }

    /// Merge another index into this one (deduplicating by source).
    pub fn merge(&mut self, other: PackageIndex) {
        for entry in other.entries {
            if !self.entries.iter().any(|e| e.source == entry.source) {
                self.entries.push(entry);
            }
        }
    }

    /// Get a specific entry by source name.
    pub fn get(&self, source: &str) -> Option<&IndexEntry> {
        self.entries.iter().find(|e| e.source == source)
    }
}

/// Load the built-in index (seed packages compiled into the binary).
pub fn builtin_index() -> PackageIndex {
    PackageIndex {
        name: "builtin".to_string(),
        entries: vec![
            IndexEntry {
                source: "github.com/loon-lang/std".to_string(),
                description: "Loon standard library — collections, string utils, math".to_string(),
                license: Some("MIT".to_string()),
                keywords: vec!["std".into(), "stdlib".into(), "core".into()],
                versions: vec![IndexVersion {
                    version: "0.1.0".to_string(),
                    hash: String::new(),
                    published: None,
                    yanked: false,
                }],
            },
            IndexEntry {
                source: "github.com/loon-lang/json".to_string(),
                description: "JSON parser and serializer for Loon".to_string(),
                license: Some("MIT".to_string()),
                keywords: vec!["json".into(), "parser".into(), "serialization".into()],
                versions: vec![IndexVersion {
                    version: "0.1.0".to_string(),
                    hash: String::new(),
                    published: None,
                    yanked: false,
                }],
            },
            IndexEntry {
                source: "github.com/loon-lang/http".to_string(),
                description: "HTTP client and server for Loon".to_string(),
                license: Some("MIT".to_string()),
                keywords: vec!["http".into(), "server".into(), "client".into(), "web".into()],
                versions: vec![IndexVersion {
                    version: "0.1.0".to_string(),
                    hash: String::new(),
                    published: None,
                    yanked: false,
                }],
            },
            IndexEntry {
                source: "github.com/loon-lang/test".to_string(),
                description: "Testing utilities — assertions, property-based testing".to_string(),
                license: Some("MIT".to_string()),
                keywords: vec!["test".into(), "assert".into(), "property".into()],
                versions: vec![IndexVersion {
                    version: "0.1.0".to_string(),
                    hash: String::new(),
                    published: None,
                    yanked: false,
                }],
            },
            IndexEntry {
                source: "github.com/loon-lang/csv".to_string(),
                description: "CSV parser and writer".to_string(),
                license: Some("MIT".to_string()),
                keywords: vec!["csv".into(), "data".into(), "parser".into()],
                versions: vec![IndexVersion {
                    version: "0.1.0".to_string(),
                    hash: String::new(),
                    published: None,
                    yanked: false,
                }],
            },
            IndexEntry {
                source: "github.com/loon-lang/cli".to_string(),
                description: "Command-line argument parsing and colored output".to_string(),
                license: Some("MIT".to_string()),
                keywords: vec!["cli".into(), "args".into(), "terminal".into()],
                versions: vec![IndexVersion {
                    version: "0.1.0".to_string(),
                    hash: String::new(),
                    published: None,
                    yanked: false,
                }],
            },
        ],
    }
}

/// Parse a Loon-format index from source text.
///
/// Index format:
/// ```loon
/// {
///   :name "my-index"
///   :packages #[
///     {
///       :source "github.com/user/repo"
///       :description "A great package"
///       :license "MIT"
///       :keywords #["web" "http"]
///       :versions #[
///         {:version "1.0.0" :hash "abc123"}
///         {:version "1.1.0" :hash "def456"}
///       ]
///     }
///   ]
/// }
/// ```
pub fn parse_index(source: &str) -> Result<PackageIndex, String> {
    let exprs = crate::parser::parse(source)
        .map_err(|e| format!("index parse error: {}", e.message))?;

    if exprs.len() != 1 {
        return Err("index must contain exactly one map expression".into());
    }

    let pairs = match &exprs[0].kind {
        ExprKind::Map(pairs) => pairs,
        _ => return Err("index must be a map {...}".into()),
    };

    let mut name = String::from("custom");
    let mut entries = Vec::new();

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
            "packages" => {
                if let ExprKind::Vec(items) = &val.kind {
                    for item in items {
                        if let ExprKind::Map(pkg_pairs) = &item.kind {
                            entries.push(parse_index_entry(pkg_pairs)?);
                        }
                    }
                }
            }
            _ => {}
        }
    }

    Ok(PackageIndex { name, entries })
}

fn parse_index_entry(
    pairs: &[(crate::ast::Expr, crate::ast::Expr)],
) -> Result<IndexEntry, String> {
    let mut source = String::new();
    let mut description = String::new();
    let mut license = None;
    let mut keywords = Vec::new();
    let mut versions = Vec::new();

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
            "description" => {
                if let ExprKind::Str(s) = &v.kind {
                    description = s.clone();
                }
            }
            "license" => {
                if let ExprKind::Str(s) = &v.kind {
                    license = Some(s.clone());
                }
            }
            "keywords" => {
                if let ExprKind::Vec(items) = &v.kind {
                    for item in items {
                        if let ExprKind::Str(s) = &item.kind {
                            keywords.push(s.clone());
                        }
                    }
                }
            }
            "versions" => {
                if let ExprKind::Vec(items) = &v.kind {
                    for item in items {
                        if let ExprKind::Map(ver_pairs) = &item.kind {
                            versions.push(parse_index_version(ver_pairs)?);
                        }
                    }
                }
            }
            _ => {}
        }
    }

    if source.is_empty() {
        return Err("index entry missing :source".into());
    }

    Ok(IndexEntry {
        source,
        description,
        license,
        keywords,
        versions,
    })
}

fn parse_index_version(
    pairs: &[(crate::ast::Expr, crate::ast::Expr)],
) -> Result<IndexVersion, String> {
    let mut version = String::new();
    let mut hash = String::new();
    let mut published = None;
    let mut yanked = false;

    for (k, v) in pairs {
        let key = match &k.kind {
            ExprKind::Keyword(s) => s.as_str(),
            _ => continue,
        };
        match key {
            "version" => {
                if let ExprKind::Str(s) = &v.kind {
                    version = s.clone();
                }
            }
            "hash" => {
                if let ExprKind::Str(s) = &v.kind {
                    hash = s.clone();
                }
            }
            "published" => {
                if let ExprKind::Str(s) = &v.kind {
                    published = Some(s.clone());
                }
            }
            "yanked" => {
                if let ExprKind::Bool(b) = &v.kind {
                    yanked = *b;
                }
            }
            _ => {}
        }
    }

    Ok(IndexVersion {
        version,
        hash,
        published,
        yanked,
    })
}

/// Fetch a remote index from a URL and parse it.
#[cfg(feature = "pkg-fetch")]
pub fn load_remote_index(name: &str, url: &str) -> Result<PackageIndex, String> {
    use std::io::Read;

    let response = ureq::get(url)
        .call()
        .map_err(|e| format!("fetching index '{name}' from {url}: {e}"))?;

    let mut body = String::new();
    response
        .into_reader()
        .read_to_string(&mut body)
        .map_err(|e| format!("reading index response: {e}"))?;

    parse_index(&body)
}

#[cfg(not(feature = "pkg-fetch"))]
pub fn load_remote_index(name: &str, _url: &str) -> Result<PackageIndex, String> {
    Err(format!(
        "fetching index '{name}' requires the 'pkg-fetch' feature"
    ))
}

/// Build a combined index from the builtin + any custom indices from the manifest.
pub fn combined_index(
    custom_indices: &std::collections::HashMap<String, String>,
) -> PackageIndex {
    let mut index = builtin_index();
    for (name, url) in custom_indices {
        match load_remote_index(name, url) {
            Ok(custom) => index.merge(custom),
            Err(e) => {
                eprintln!("warning: could not load index '{name}': {e}");
            }
        }
    }
    index
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn builtin_index_has_entries() {
        let idx = builtin_index();
        assert!(!idx.entries.is_empty());
        assert!(idx.get("github.com/loon-lang/std").is_some());
    }

    #[test]
    fn search_by_keyword() {
        let idx = builtin_index();
        let results = idx.search("json");
        assert!(!results.is_empty());
        assert!(results.iter().any(|e| e.source.contains("json")));
    }

    #[test]
    fn search_by_description() {
        let idx = builtin_index();
        let results = idx.search("parser");
        assert!(!results.is_empty());
    }

    #[test]
    fn search_no_results() {
        let idx = builtin_index();
        let results = idx.search("zzzzz_nonexistent");
        assert!(results.is_empty());
    }

    #[test]
    fn parse_index_roundtrip() {
        let source = r#"{
  :name "test-index"
  :packages #[
    {
      :source "github.com/test/pkg"
      :description "A test package"
      :license "MIT"
      :keywords #["test" "example"]
      :versions #[
        {:version "1.0.0" :hash "abc123"}
        {:version "1.1.0" :hash "def456" :yanked true}
      ]
    }
  ]
}"#;
        let idx = parse_index(source).unwrap();
        assert_eq!(idx.name, "test-index");
        assert_eq!(idx.entries.len(), 1);
        assert_eq!(idx.entries[0].source, "github.com/test/pkg");
        assert_eq!(idx.entries[0].versions.len(), 2);
        assert!(!idx.entries[0].versions[0].yanked);
        assert!(idx.entries[0].versions[1].yanked);
    }

    #[test]
    fn merge_indices() {
        let mut a = builtin_index();
        let initial_len = a.entries.len();
        let b = PackageIndex {
            name: "extra".to_string(),
            entries: vec![IndexEntry {
                source: "github.com/extra/pkg".to_string(),
                description: "Extra package".to_string(),
                license: None,
                keywords: vec![],
                versions: vec![],
            }],
        };
        a.merge(b);
        assert_eq!(a.entries.len(), initial_len + 1);

        // Merging duplicate should not add again
        let c = PackageIndex {
            name: "dup".to_string(),
            entries: vec![IndexEntry {
                source: "github.com/extra/pkg".to_string(),
                description: "Duplicate".to_string(),
                license: None,
                keywords: vec![],
                versions: vec![],
            }],
        };
        a.merge(c);
        assert_eq!(a.entries.len(), initial_len + 1);
    }
}
