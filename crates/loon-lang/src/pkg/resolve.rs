use std::collections::HashMap;

use super::manifest::Manifest;
use super::version::Version;

/// Resolved dependency graph after MVS.
#[derive(Debug, Clone)]
pub struct ResolvedGraph {
    pub packages: HashMap<String, ResolvedPackage>,
}

#[derive(Debug, Clone)]
pub struct ResolvedPackage {
    pub source: String,
    pub version: Version,
    pub deps: Vec<String>,
}

/// Minimum Version Selection (Phase 3).
///
/// Given a root manifest and a way to fetch dep manifests,
/// compute the minimum satisfying version for each dep.
pub fn resolve(_manifest: &Manifest) -> Result<ResolvedGraph, String> {
    // Phase 3: implement MVS algorithm
    Ok(ResolvedGraph {
        packages: HashMap::new(),
    })
}
