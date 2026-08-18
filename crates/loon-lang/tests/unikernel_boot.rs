//! Boot the unikernel under QEMU and check it agrees with the host.
//!
//! This is the phase-3 exit criterion in test form: the same Loon program,
//! compiled once, must produce identical output whether the effects land on
//! a host syscall or on a UART. Skipped (not failed) when the bare-metal
//! toolchain is absent, since most contributors will not have it.

use std::path::PathBuf;
use std::process::Command;

fn workspace_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(|p| p.parent())
        .expect("workspace root")
        .to_path_buf()
}

fn have(cmd: &str, args: &[&str]) -> bool {
    Command::new(cmd)
        .args(args)
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status()
        .map(|s| s.success())
        .unwrap_or(false)
}

#[test]
fn unikernel_boots_and_matches_the_host() {
    let root = workspace_root();
    let kernel_dir = root.join("crates/loon-kernel");

    if !have("qemu-system-riscv64", &["-version"]) {
        eprintln!("skipping: qemu-system-riscv64 not installed");
        return;
    }
    let targets = Command::new("rustup").args(["target", "list", "--installed"]).output();
    let has_target = targets
        .map(|o| String::from_utf8_lossy(&o.stdout).contains("riscv64gc-unknown-none-elf"))
        .unwrap_or(false);
    if !has_target {
        eprintln!("skipping: rustup target riscv64gc-unknown-none-elf not installed");
        return;
    }

    let out = Command::new("make")
        .arg("check")
        .current_dir(&kernel_dir)
        .output()
        .expect("running `make check` in crates/loon-kernel");

    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        out.status.success() && stdout.contains("identical output"),
        "unikernel boot diverged from the host.\n--- stdout ---\n{stdout}\n--- stderr ---\n{stderr}"
    );
}
