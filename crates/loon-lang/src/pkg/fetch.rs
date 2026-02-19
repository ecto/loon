use std::path::{Path, PathBuf};

/// Get the global cache directory (~/.loon/cache/blake3/).
pub fn cache_dir() -> PathBuf {
    let home = dirs_next::home_dir().unwrap_or_else(|| PathBuf::from("."));
    home.join(".loon").join("cache").join("blake3")
}

/// Check if a package is already cached by hash.
pub fn cached_path(hash: &str) -> Option<PathBuf> {
    let path = cache_dir().join(hash);
    if path.exists() {
        Some(path)
    } else {
        None
    }
}

/// Fetch a package from a git URL (Phase 2).
pub fn fetch_git(_url: &str, _rev: Option<&str>) -> Result<PathBuf, String> {
    Err("git fetching not yet implemented (Phase 2)".into())
}

/// Fetch a package from a direct URL (Phase 2).
pub fn fetch_url(_url: &str, _expected_hash: Option<&str>) -> Result<PathBuf, String> {
    Err("URL fetching not yet implemented (Phase 2)".into())
}

/// Normalize a source tree and compute its BLAKE3 hash (Phase 2).
pub fn normalize_and_hash(_dir: &Path) -> Result<String, String> {
    Err("hashing not yet implemented (Phase 2)".into())
}
