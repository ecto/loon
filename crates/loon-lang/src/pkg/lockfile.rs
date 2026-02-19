use std::path::Path;

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

/// The lock.loon file — deterministic, content-addressed dependency snapshot.
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

    /// Load lock.loon from a directory, returning None if not found.
    pub fn load(_dir: &Path) -> Result<Option<Self>, String> {
        let lock_path = _dir.join("lock.loon");
        if !lock_path.exists() {
            return Ok(None);
        }
        // TODO: Phase 2 — parse lock.loon
        Ok(None)
    }

    /// Write lock.loon to a directory.
    pub fn write(&self, _dir: &Path) -> Result<(), String> {
        // TODO: Phase 2 — serialize lock.loon
        Ok(())
    }

    /// Look up a package by source name.
    pub fn get(&self, source: &str) -> Option<&LockedPackage> {
        self.packages.iter().find(|p| p.source == source)
    }
}

impl Default for Lockfile {
    fn default() -> Self {
        Self::new()
    }
}
