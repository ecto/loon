//! End-to-end regression: the WASM backend compiles a broad slice of real
//! sample programs to *valid* standalone modules. These cover arithmetic,
//! recursion, ADTs/match, closures, strings, vectors, higher-order functions
//! (range/map/filter/each/fold), `pipe`, multi-arity functions, self-tail
//! recursion, and effect operations lowered to host imports.
//!
//! Samples that need delimited continuations (handle/resume/try) or the
//! map/set/string-processing stdlib are intentionally excluded — those run on
//! the EIR VM (`loon run`), not the wasm backend.

use loon_lang::parser::parse;
use std::path::Path;

fn samples_dir() -> std::path::PathBuf {
    // crates/loon-lang -> workspace root -> samples
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("..")
        .join("samples")
}

fn compile_sample(name: &str) -> Vec<u8> {
    let path = samples_dir().join(name);
    let source = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("read {name}: {e}"));
    let exprs = parse(&source).unwrap_or_else(|e| panic!("parse {name}: {e:?}"));
    loon_lang::codegen::compile(&exprs).unwrap_or_else(|e| panic!("compile {name}: {e}"))
}

fn assert_valid_wasm(name: &str, bytes: &[u8]) {
    assert_eq!(&bytes[0..4], b"\0asm", "{name} should be a wasm module");
    wasmparser::Validator::new()
        .validate_all(bytes)
        .unwrap_or_else(|e| panic!("{name} produced invalid wasm: {e}"));
}

#[test]
fn samples_compile_to_valid_wasm() {
    // Each of these must compile to a validating standalone module.
    for name in [
        "hello.oo",
        "fib-simple.oo",
        "fib.oo",
        "compiled-fib.oo",
        "pipeline.oo",
        "multi-arity.oo",
        "types.oo",
        "word-count.oo",
        "word-freq.oo",
        "bench-collections.oo",
        "effects.oo",
        "user-effects.oo",
        "tco-stress.oo",
        "state.oo",
    ] {
        let bytes = compile_sample(name);
        assert_valid_wasm(name, &bytes);
    }
}
