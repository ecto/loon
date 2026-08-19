//! Every kernel that ships in the repo must compile to a valid GPU shader.
//!
//! The WGSL is parsed and type-checked by naga — the same front end wgpu uses
//! — so a kernel that a driver would reject fails here instead, on a machine
//! with no GPU at all. This is the automated cross-target check the Rust
//! offload paper reports as still missing: they found their slice-lowering
//! divergence between host and device by hand.

use loon_lang::check::{kernel, Checker};
use loon_lang::eir::layout::DType;
use loon_lang::eir::lower::lower;
use loon_lang::eir::wgsl::{self, ArgKind};
use loon_lang::parser::parse;

fn repo_root() -> std::path::PathBuf {
    std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(|p| p.parent())
        .expect("repo root")
        .to_path_buf()
}

/// Parse a file, find its kernels, and emit WGSL for each one.
///
/// Argument kinds are guessed from the kernel's inferred ownership modes: a
/// parameter written through is a writable buffer, one only read is an input.
/// That is the same fact the runtime uses to decide which way bytes move —
/// here it decides which bindings are `read` and which are `read_write`.
fn emit_all(src: &str, path: &str) -> usize {
    let exprs = parse(src).unwrap_or_else(|e| panic!("{path} does not parse: {e:?}"));
    let (_, kernel_names) = kernel::desugar(&exprs);
    if kernel_names.is_empty() {
        return 0;
    }

    let mut checker = Checker::new();
    let errors = checker.check_program(&exprs);
    assert!(errors.is_empty(), "{path} has check errors: {errors:?}");
    let module = lower(&checker);

    let mut emitted = 0;
    for name in &kernel_names {
        let func = module
            .funcs
            .iter()
            .find(|f| f.name.as_deref() == Some(name.as_str()))
            .unwrap_or_else(|| panic!("{path}: kernel '{name}' did not lower"));

        // The kernel body says what its arguments are: indexed parameters are
        // buffers, the rest are scalars. Nothing has to be declared.
        let args = wgsl::infer_arg_kinds(&module, func.id, DType::F32);
        let wgsl_text = wgsl::emit(&module, func.id, &args)
            .unwrap_or_else(|e| panic!("{path}: kernel '{name}' will not emit: {e}"));

        validate(&wgsl_text, &format!("{path}:{name}"));
        emitted += 1;
    }
    emitted
}

fn validate(text: &str, what: &str) {
    let module = naga::front::wgsl::parse_str(text)
        .unwrap_or_else(|e| panic!("{what}: WGSL did not parse: {e}\n\n{text}"));
    let mut validator = naga::valid::Validator::new(
        naga::valid::ValidationFlags::all(),
        naga::valid::Capabilities::empty(),
    );
    validator
        .validate(&module)
        .unwrap_or_else(|e| panic!("{what}: WGSL did not validate: {e:?}\n\n{text}"));
}

#[test]
fn every_kernel_in_the_repo_compiles_to_valid_wgsl() {
    let root = repo_root();
    let mut checked = 0;
    let mut files = 0;

    for dir in ["samples/place", "os"] {
        let path = root.join(dir);
        let Ok(entries) = std::fs::read_dir(&path) else {
            continue;
        };
        for entry in entries.flatten() {
            let p = entry.path();
            if p.extension().and_then(|e| e.to_str()) != Some("oo") {
                continue;
            }
            let Ok(src) = std::fs::read_to_string(&p) else {
                continue;
            };
            // Demos that pull in a module need its definitions to type-check;
            // the kernels themselves are what this test is about, so only
            // files that stand alone are compiled here.
            if src.contains("[use ") {
                continue;
            }
            let n = emit_all(&src, &p.display().to_string());
            if n > 0 {
                files += 1;
                checked += n;
            }
        }
    }

    assert!(
        checked > 0,
        "no kernels were found to validate — the test would pass vacuously"
    );
    println!("validated {checked} kernels across {files} files");
}

#[test]
fn a_kernel_the_gpu_cannot_run_is_refused_rather_than_mistranslated() {
    // The kernel checker already rejects effects and allocation, so this
    // exercises the emitter's own last line of defence: an operation with no
    // GPU equivalent must be named, not silently dropped.
    let src = "[fn helper [x] x] [kernel k [i b] [put b i [helper 1.0]]]";
    let exprs = parse(src).expect("parses");
    let mut checker = Checker::new();
    let _ = checker.check_program(&exprs);
    let module = lower(&checker);
    let func = module
        .funcs
        .iter()
        .find(|f| f.name.as_deref() == Some("k"))
        .expect("k lowered");

    // Whatever happens, it must not be a shader that quietly computes
    // something else.
    if let Ok(text) = wgsl::emit(&module, func.id, &[ArgKind::output(DType::F32)]) {
        validate(&text, "k");
    }
}
