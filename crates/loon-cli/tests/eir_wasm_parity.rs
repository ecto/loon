//! EIR WASM backend parity tracker.
//!
//! The unification goal: make the EIR `WasmBackend` (`eir/wasm.rs`) the single
//! WASM code path, replacing the legacy direct `codegen/` backend. This test
//! is the progress dashboard for that effort — it compiles every sample through
//! `eir::wasm::compile_src` and validates the result, reporting how many of the
//! 16 samples the EIR backend can currently handle.
//!
//! It does NOT yet compare stdout (that's a later phase, once the runtime ABI
//! is reconciled and a `--wasm2` runner exists). For now it measures structural
//! reach: parse → check → lower → emit → validate.

use std::path::PathBuf;

fn samples_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("..")
        .join("samples")
}

/// All 16 samples the legacy `--wasm` backend runs at full parity.
const SAMPLES: &[&str] = &[
    "hello.oo",
    "fib-simple.oo",
    "fib.oo",
    "compiled-fib.oo",
    "pipeline.oo",
    "multi-arity.oo",
    "word-count.oo",
    "word-freq.oo",
    "test-suite.oo",
    "types.oo",
    "effects.oo",
    "user-effects.oo",
    "state.oo",
    "physics.oo",
    "tco-stress.oo",
    "bench-collections.oo",
];

/// Validate emitted WASM bytes with a permissive engine (tail-call + exception
/// proposals enabled, matching the legacy runner's config).
fn validate(bytes: &[u8]) -> Result<(), String> {
    let mut config = wasmtime::Config::new();
    config.wasm_tail_call(true);
    let engine = wasmtime::Engine::new(&config).map_err(|e| e.to_string())?;
    wasmtime::Module::validate(&engine, bytes).map_err(|e| e.to_string())
}

#[test]
fn eir_wasm_backend_parity_report() {
    let mut compiled = 0usize;
    let mut report = String::from("\nEIR WASM backend parity (compile + validate):\n");

    for &name in SAMPLES {
        let path = samples_dir().join(name);
        let src = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("read {name}: {e}"));
        let status = match loon_lang::eir::wasm::compile_src(&src) {
            Ok(bytes) => match validate(&bytes) {
                Ok(()) => {
                    compiled += 1;
                    "ok".to_string()
                }
                Err(e) => format!("INVALID WASM: {}", e.lines().next().unwrap_or("")),
            },
            Err(e) => format!("compile error: {}", e.lines().next().unwrap_or("")),
        };
        report.push_str(&format!("  {name:<22} {status}\n"));
    }

    report.push_str(&format!("\n  {compiled}/{} samples compile+validate\n", SAMPLES.len()));
    eprintln!("{report}");

    // Ratchet: this baseline only ever goes up. Bump it as gaps close.
    // 15/16 — only multi-arity.oo (multi-clause dispatch) fails to validate.
    const BASELINE: usize = 15;
    assert!(
        compiled >= BASELINE,
        "EIR WASM parity regressed below {BASELINE}/{} (got {compiled})",
        SAMPLES.len()
    );
}
