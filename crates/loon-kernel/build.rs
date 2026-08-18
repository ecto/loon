//! Compile `boot/init.oo` into a boot image and hand it to the kernel.
//!
//! The unikernel has no frontend, so this is where a Loon program stops
//! being source. Compiling through the workspace CLI (rather than linking
//! loon-lang directly) keeps the host toolchain entirely out of the
//! bare-metal build graph.

use std::path::{Path, PathBuf};
use std::process::Command;

fn main() {
    let manifest = PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());
    let workspace = manifest.join("../../Cargo.toml");
    println!("cargo:rerun-if-changed=build.rs");

    for (name, var) in [
        ("init", "LOON_BOOT_IMAGE"),
        ("bench", "LOON_BENCH_IMAGE"),
        ("loop", "LOON_LOOP_IMAGE"),
        ("mandel", "LOON_MANDEL_IMAGE"),
        ("gui", "LOON_GUI_IMAGE"),
    ] {
        let src = manifest.join(format!("boot/{name}.oo"));
        let out = PathBuf::from(std::env::var("OUT_DIR").unwrap()).join(format!("{name}.img"));
        println!("cargo:rerun-if-changed={}", src.display());
        image(&workspace, &manifest, &src, &out);
        println!("cargo:rustc-env={var}={}", out.display());
    }
}

fn image(workspace: &Path, manifest: &Path, src: &Path, out: &Path) {
    let status = Command::new(std::env::var("CARGO").unwrap_or_else(|_| "cargo".into()))
        // Run from the workspace root: this crate's .cargo/config.toml pins
        // a bare-metal target, and the nested build must not inherit it.
        .current_dir(manifest.join("../.."))
        .args(["run", "-q", "--manifest-path"])
        .arg(workspace)
        .args(["-p", "loon-cli", "--", "image"])
        .arg(src)
        .arg("-o")
        .arg(out)
        // Cargo's env leaks the bare-metal target into the nested build and
        // makes it try to compile the compiler for riscv; clear it.
        .env_remove("CARGO_ENCODED_RUSTFLAGS")
        .env_remove("RUSTFLAGS")
        .env_remove("CARGO_BUILD_TARGET")
        .status()
        .expect("failed to run loon-cli to build the boot image");

    if !status.success() {
        panic!("building the boot image from {} failed", src.display());
    }
}
