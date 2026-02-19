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
}

/// Load the built-in index (Phase 3: compiled into binary).
pub fn builtin_index() -> PackageIndex {
    PackageIndex {
        name: "builtin".to_string(),
        entries: vec![],
    }
}

/// Load a custom index from a URL (Phase 3).
pub fn load_custom_index(_name: &str, _url: &str) -> Result<PackageIndex, String> {
    Err("custom indices not yet implemented (Phase 3)".into())
}
