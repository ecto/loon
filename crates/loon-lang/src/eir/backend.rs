//! Backend trait — the contract between the Evidence IR and code generators.
//!
//! Each backend is a pure function: `Module → Output`.
//! No shared codegen infrastructure — the targets are too different.

use super::Module;

/// Compilation error from a backend.
#[derive(Debug)]
pub struct Error {
    pub message: String,
    pub phase: &'static str,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.phase, self.message)
    }
}

impl std::error::Error for Error {}

/// A pluggable code generator.
///
/// Implementations:
/// - `VmBackend` → register VM bytecode (interpreted, for dev/REPL)
/// - `WasmBackend` → WASM binary (for browser/edge)
/// - `NativeBackend` → machine code via Cranelift (for production)
pub trait Backend {
    /// The output artifact.
    type Output;

    /// Compile an Evidence IR module to target-specific output.
    fn compile(&mut self, module: &Module) -> Result<Self::Output, Error>;

    /// Backend name, for diagnostics.
    fn name(&self) -> &'static str;
}
