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

/// Check if the first segment of a slash-separated path looks like a domain name.
/// e.g. `github.com/cam/json` → true, `http.server` → false (no `/`).
pub fn is_domain_qualified(path: &str) -> bool {
    if let Some(slash_pos) = path.find('/') {
        let first_segment = &path[..slash_pos];
        first_segment.contains('.')
    } else {
        false
    }
}

/// Split `github.com/cam/std#http` into base (`github.com/cam/std`) and optional
/// subpath (`http`).
pub fn parse_source_name(source: &str) -> (&str, Option<&str>) {
    if let Some(hash_pos) = source.find('#') {
        (&source[..hash_pos], Some(&source[hash_pos + 1..]))
    } else {
        (source, None)
    }
}

/// Derive a git clone URL from a domain-qualified source.
/// `github.com/cam/json` → `https://github.com/cam/json.git`
pub fn git_url_from_source(source: &str) -> String {
    let (base, _) = parse_source_name(source);
    format!("https://{base}.git")
}

/// Derive an archive download URL for a given version.
/// Supports GitHub, GitLab, and Codeberg archive patterns.
pub fn archive_url_from_source(source: &str, version: &str) -> Option<String> {
    let (base, _) = parse_source_name(source);
    let parts: Vec<&str> = base.splitn(3, '/').collect();
    if parts.len() < 3 {
        return None;
    }
    let (host, user, repo) = (parts[0], parts[1], parts[2]);
    match host {
        "github.com" => Some(format!(
            "https://github.com/{user}/{repo}/archive/refs/tags/v{version}.tar.gz"
        )),
        "gitlab.com" => Some(format!(
            "https://gitlab.com/{user}/{repo}/-/archive/v{version}/{repo}-v{version}.tar.gz"
        )),
        "codeberg.org" => Some(format!(
            "https://codeberg.org/{user}/{repo}/archive/v{version}.tar.gz"
        )),
        _ => None,
    }
}

/// Find the entry point file in a dependency directory.
/// Looks for `src/lib.loon`, then `src/main.loon`.
pub fn find_entry_point(dep_dir: &Path) -> Option<PathBuf> {
    let candidates = [
        dep_dir.join("src").join("lib.loon"),
        dep_dir.join("src").join("main.loon"),
    ];
    for c in &candidates {
        if c.exists() {
            return Some(c.clone());
        }
    }
    None
}

// ---------- feature-gated implementations ----------

#[cfg(feature = "pkg-fetch")]
mod inner {
    use super::*;
    use std::io::Read as _;

    /// Walk all files in `dir` (sorted by relative path, skipping `.git/` and `target/`),
    /// and compute a BLAKE3 hash over (relative_path + file_contents) for each file.
    pub fn normalize_and_hash(dir: &Path) -> Result<String, String> {
        let mut paths = Vec::new();
        collect_files(dir, dir, &mut paths)?;
        paths.sort();

        let mut hasher = blake3::Hasher::new();
        for rel in &paths {
            hasher.update(rel.as_bytes());
            let full = dir.join(rel);
            let contents = std::fs::read(&full)
                .map_err(|e| format!("reading {}: {e}", full.display()))?;
            hasher.update(&contents);
        }
        Ok(hasher.finalize().to_hex().to_string())
    }

    fn collect_files(base: &Path, current: &Path, out: &mut Vec<String>) -> Result<(), String> {
        let entries = std::fs::read_dir(current)
            .map_err(|e| format!("reading dir {}: {e}", current.display()))?;
        for entry in entries {
            let entry = entry.map_err(|e| format!("dir entry: {e}"))?;
            let path = entry.path();
            let name = entry.file_name().to_string_lossy().to_string();
            if name == ".git" || name == "target" {
                continue;
            }
            if path.is_dir() {
                collect_files(base, &path, out)?;
            } else {
                let rel = path
                    .strip_prefix(base)
                    .map_err(|e| format!("strip prefix: {e}"))?
                    .to_string_lossy()
                    .replace('\\', "/");
                out.push(rel);
            }
        }
        Ok(())
    }

    /// Clone a git repo (shallow) into a temp directory, remove `.git/`, return path.
    pub fn fetch_git(url: &str, rev: Option<&str>) -> Result<PathBuf, String> {
        let tmp = std::env::temp_dir().join(format!("loon-git-{}", std::process::id()));
        if tmp.exists() {
            let _ = std::fs::remove_dir_all(&tmp);
        }

        let mut cmd = std::process::Command::new("git");
        cmd.arg("clone").arg("--depth").arg("1");
        if let Some(r) = rev {
            cmd.arg("--branch").arg(r);
        }
        cmd.arg(url).arg(&tmp);

        let output = cmd
            .output()
            .map_err(|e| format!("failed to run git: {e}"))?;
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(format!("git clone failed: {stderr}"));
        }

        // Remove .git directory
        let git_dir = tmp.join(".git");
        if git_dir.exists() {
            std::fs::remove_dir_all(&git_dir)
                .map_err(|e| format!("removing .git: {e}"))?;
        }

        Ok(tmp)
    }

    /// Download a `.tar.gz` from `url`, extract it, flatten GitHub's single-subdir wrapper,
    /// and optionally verify the BLAKE3 hash.
    pub fn fetch_url(url: &str, expected_hash: Option<&str>) -> Result<PathBuf, String> {
        let response = ureq::get(url)
            .call()
            .map_err(|e| format!("HTTP GET {url}: {e}"))?;

        let mut body = Vec::new();
        response
            .into_reader()
            .read_to_end(&mut body)
            .map_err(|e| format!("reading response: {e}"))?;

        let tmp = std::env::temp_dir().join(format!("loon-url-{}", std::process::id()));
        if tmp.exists() {
            let _ = std::fs::remove_dir_all(&tmp);
        }
        std::fs::create_dir_all(&tmp)
            .map_err(|e| format!("creating temp dir: {e}"))?;

        // Extract tar.gz
        let decoder = flate2::read::GzDecoder::new(body.as_slice());
        let mut archive = tar::Archive::new(decoder);
        archive
            .unpack(&tmp)
            .map_err(|e| format!("extracting archive: {e}"))?;

        // Flatten single-subdir wrapper (GitHub archives have repo-version/ prefix)
        let result_dir = flatten_single_subdir(&tmp)?;

        // Verify hash if expected
        if let Some(expected) = expected_hash {
            let actual = normalize_and_hash(&result_dir)?;
            if actual != expected {
                return Err(format!(
                    "hash mismatch: expected {expected}, got {actual}"
                ));
            }
        }

        Ok(result_dir)
    }

    /// If a directory contains exactly one subdirectory and no files,
    /// return the subdirectory path. Otherwise return the dir itself.
    fn flatten_single_subdir(dir: &Path) -> Result<PathBuf, String> {
        let entries: Vec<_> = std::fs::read_dir(dir)
            .map_err(|e| format!("reading {}: {e}", dir.display()))?
            .filter_map(|e| e.ok())
            .collect();

        if entries.len() == 1 && entries[0].path().is_dir() {
            Ok(entries[0].path())
        } else {
            Ok(dir.to_path_buf())
        }
    }

    /// Copy `source_dir` into the cache at `~/.loon/cache/blake3/<hash>/`.
    pub fn store_in_cache(source_dir: &Path, hash: &str) -> Result<PathBuf, String> {
        let dest = super::cache_dir().join(hash);
        if dest.exists() {
            return Ok(dest);
        }
        std::fs::create_dir_all(&dest)
            .map_err(|e| format!("creating cache dir: {e}"))?;
        copy_dir_recursive(source_dir, &dest)?;
        Ok(dest)
    }

    fn copy_dir_recursive(src: &Path, dst: &Path) -> Result<(), String> {
        for entry in std::fs::read_dir(src).map_err(|e| format!("reading {}: {e}", src.display()))? {
            let entry = entry.map_err(|e| format!("dir entry: {e}"))?;
            let src_path = entry.path();
            let dst_path = dst.join(entry.file_name());
            if src_path.is_dir() {
                std::fs::create_dir_all(&dst_path)
                    .map_err(|e| format!("creating {}: {e}", dst_path.display()))?;
                copy_dir_recursive(&src_path, &dst_path)?;
            } else {
                std::fs::copy(&src_path, &dst_path)
                    .map_err(|e| format!("copying {}: {e}", src_path.display()))?;
            }
        }
        Ok(())
    }

    /// Create a `.tar.gz` archive of a directory, excluding `.git/`, `target/`, and `lock.loon`.
    pub fn create_tarball(dir: &Path, output_path: &Path) -> Result<u64, String> {
        let file = std::fs::File::create(output_path)
            .map_err(|e| format!("creating {}: {e}", output_path.display()))?;
        let encoder = flate2::write::GzEncoder::new(file, flate2::Compression::default());
        let mut archive = tar::Builder::new(encoder);

        let mut paths = Vec::new();
        collect_publishable_files(dir, dir, &mut paths)?;
        paths.sort();

        for rel in &paths {
            let full = dir.join(rel);
            archive
                .append_path_with_name(&full, rel)
                .map_err(|e| format!("adding {}: {e}", rel))?;
        }

        let encoder = archive
            .into_inner()
            .map_err(|e| format!("finalizing archive: {e}"))?;
        encoder
            .finish()
            .map_err(|e| format!("compressing archive: {e}"))?;

        let metadata = std::fs::metadata(output_path)
            .map_err(|e| format!("reading archive size: {e}"))?;
        Ok(metadata.len())
    }

    fn collect_publishable_files(
        base: &Path,
        current: &Path,
        out: &mut Vec<String>,
    ) -> Result<(), String> {
        let entries = std::fs::read_dir(current)
            .map_err(|e| format!("reading dir {}: {e}", current.display()))?;
        for entry in entries {
            let entry = entry.map_err(|e| format!("dir entry: {e}"))?;
            let path = entry.path();
            let name = entry.file_name().to_string_lossy().to_string();
            if name == ".git" || name == "target" || name == "lock.loon" {
                continue;
            }
            if path.is_dir() {
                collect_publishable_files(base, &path, out)?;
            } else {
                let rel = path
                    .strip_prefix(base)
                    .map_err(|e| format!("strip prefix: {e}"))?
                    .to_string_lossy()
                    .replace('\\', "/");
                out.push(rel);
            }
        }
        Ok(())
    }

    /// High-level orchestrator: check cache → fetch → hash → store → return (path, hash).
    pub fn fetch_and_cache(
        source: &str,
        dep: &crate::pkg::manifest::Dependency,
    ) -> Result<(PathBuf, String), String> {
        // If we already have a hash and it's cached, return immediately
        if let Some(ref hash) = dep.hash {
            if let Some(path) = super::cached_path(hash) {
                return Ok((path, hash.clone()));
            }
        }

        // Determine fetch method
        let fetched_dir = if let Some(ref git_url) = dep.git {
            fetch_git(git_url, dep.rev.as_deref())?
        } else if let Some(ref url) = dep.url {
            fetch_url(url, dep.hash.as_deref())?
        } else if super::is_domain_qualified(source) {
            // Try archive URL first (faster), fall back to git
            let version_str = dep
                .version
                .as_ref()
                .map(|v| v.minimum().to_string())
                .unwrap_or_else(|| "main".to_string());

            if let Some(archive_url) = super::archive_url_from_source(source, &version_str) {
                match fetch_url(&archive_url, dep.hash.as_deref()) {
                    Ok(dir) => dir,
                    Err(_) => {
                        // Fall back to git clone
                        let git_url = super::git_url_from_source(source);
                        fetch_git(&git_url, dep.rev.as_deref())?
                    }
                }
            } else {
                let git_url = super::git_url_from_source(source);
                fetch_git(&git_url, dep.rev.as_deref())?
            }
        } else {
            return Err(format!("don't know how to fetch '{source}'"));
        };

        // Hash the fetched content
        let hash = normalize_and_hash(&fetched_dir)?;

        // Verify against expected hash if provided
        if let Some(ref expected) = dep.hash {
            if &hash != expected {
                return Err(format!(
                    "hash mismatch for '{source}': expected {expected}, got {hash}"
                ));
            }
        }

        // Store in cache
        let cached = store_in_cache(&fetched_dir, &hash)?;

        // Clean up temp dir
        let _ = std::fs::remove_dir_all(&fetched_dir);

        Ok((cached, hash))
    }
}

#[cfg(feature = "pkg-fetch")]
pub use inner::{
    create_tarball, fetch_and_cache, fetch_git, fetch_url, normalize_and_hash, store_in_cache,
};

#[cfg(not(feature = "pkg-fetch"))]
pub fn fetch_git(_url: &str, _rev: Option<&str>) -> Result<PathBuf, String> {
    Err("git fetching requires the 'pkg-fetch' feature (use loon-cli)".into())
}

#[cfg(not(feature = "pkg-fetch"))]
pub fn fetch_url(_url: &str, _expected_hash: Option<&str>) -> Result<PathBuf, String> {
    Err("URL fetching requires the 'pkg-fetch' feature (use loon-cli)".into())
}

#[cfg(not(feature = "pkg-fetch"))]
pub fn normalize_and_hash(_dir: &Path) -> Result<String, String> {
    Err("hashing requires the 'pkg-fetch' feature (use loon-cli)".into())
}

#[cfg(not(feature = "pkg-fetch"))]
pub fn store_in_cache(_source_dir: &Path, _hash: &str) -> Result<PathBuf, String> {
    Err("caching requires the 'pkg-fetch' feature (use loon-cli)".into())
}

#[cfg(not(feature = "pkg-fetch"))]
pub fn fetch_and_cache(
    _source: &str,
    _dep: &crate::pkg::manifest::Dependency,
) -> Result<(PathBuf, String), String> {
    Err("fetching requires the 'pkg-fetch' feature (use loon-cli)".into())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn domain_qualified_detection() {
        assert!(is_domain_qualified("github.com/cam/json"));
        assert!(is_domain_qualified("gitlab.com/user/repo"));
        assert!(!is_domain_qualified("http.server"));
        assert!(!is_domain_qualified("my-lib"));
        assert!(!is_domain_qualified("std"));
    }

    #[test]
    fn parse_source_with_subpath() {
        let (base, sub) = parse_source_name("github.com/cam/std#http");
        assert_eq!(base, "github.com/cam/std");
        assert_eq!(sub, Some("http"));

        let (base, sub) = parse_source_name("github.com/cam/json");
        assert_eq!(base, "github.com/cam/json");
        assert_eq!(sub, None);
    }

    #[test]
    fn git_url_derivation() {
        assert_eq!(
            git_url_from_source("github.com/cam/json"),
            "https://github.com/cam/json.git"
        );
        assert_eq!(
            git_url_from_source("github.com/cam/std#http"),
            "https://github.com/cam/std.git"
        );
    }

    #[test]
    fn archive_url_derivation() {
        assert_eq!(
            archive_url_from_source("github.com/cam/json", "1.0.0"),
            Some("https://github.com/cam/json/archive/refs/tags/v1.0.0.tar.gz".to_string())
        );
        assert_eq!(
            archive_url_from_source("gitlab.com/user/repo", "2.1"),
            Some("https://gitlab.com/user/repo/-/archive/v2.1/repo-v2.1.tar.gz".to_string())
        );
        assert_eq!(archive_url_from_source("unknown.host/a", "1.0"), None);
    }

    #[cfg(feature = "pkg-fetch")]
    #[test]
    fn hash_deterministic() {
        let tmp = std::env::temp_dir().join("loon-hash-test");
        let _ = std::fs::remove_dir_all(&tmp);
        std::fs::create_dir_all(tmp.join("sub")).unwrap();
        std::fs::write(tmp.join("a.loon"), "hello").unwrap();
        std::fs::write(tmp.join("sub").join("b.loon"), "world").unwrap();

        let h1 = normalize_and_hash(&tmp).unwrap();
        let h2 = normalize_and_hash(&tmp).unwrap();
        assert_eq!(h1, h2);
        assert!(!h1.is_empty());

        let _ = std::fs::remove_dir_all(&tmp);
    }
}
