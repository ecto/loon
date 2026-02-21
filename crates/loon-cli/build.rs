fn main() {
    // Set GIT_VERSION from `git describe --tags` for --version output.
    // Falls back to Cargo.toml version if not in a git repo or no tags exist.
    println!("cargo:rerun-if-changed=../../.git/HEAD");
    println!("cargo:rerun-if-changed=../../.git/refs/tags");

    let output = std::process::Command::new("git")
        .args(["describe", "--tags", "--always", "--dirty"])
        .output();

    let version = match output {
        Ok(o) if o.status.success() => {
            let v = String::from_utf8_lossy(&o.stdout).trim().to_string();
            // Strip leading 'v' if present
            v.strip_prefix('v').unwrap_or(&v).to_string()
        }
        _ => env!("CARGO_PKG_VERSION").to_string(),
    };

    println!("cargo:rustc-env=GIT_VERSION={version}");
}
