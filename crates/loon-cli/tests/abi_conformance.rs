//! Every backend must encode a value the same way, bit for bit.
//!
//! Loon has three code generators — the register VM, the Cranelift JIT, and
//! the WASM emitter — and until recently each carried its own copy of the
//! NaN-boxing constants under a comment asking the next person to keep them in
//! sync. That is the same setup that produced a real bug in a recent Rust
//! GPU-offload compiler, where a slice lowered as `(ptr, len)` on two targets
//! and `[i64; 2]` on a third; the authors found it by hand and wrote that
//! automated cross-target validation was still missing.
//!
//! This is that validation. `eir::layout` now holds the encoding once, and the
//! tests below compile the same source on every available backend and compare
//! the raw 64-bit results. A divergence fails here rather than surfacing later
//! as a wrong answer on one target.

use loon_lang::eir::layout::{
    nanbox, BufferHeader, DType, BUF_HDR_SIZE, BUF_OFF_DATA, BUF_OFF_DTYPE, BUF_OFF_LEN,
    GOLDEN_IMMEDIATES,
};

/// A program whose value is the literal.
///
/// Deliberately a bare top-level expression rather than a `main` function:
/// reaching `main` requires closures, which the native backend does not
/// implement, and a program every backend refuses would make the comparison
/// below vacuous.
fn program_for(literal: &str) -> String {
    literal.to_string()
}

/// Evaluate on the register VM, returning the raw encoded word.
fn vm_bits(literal: &str) -> u64 {
    let src = program_for(literal);
    let result = loon_lang::eir::vm::eval_eir(&src)
        .unwrap_or_else(|e| panic!("VM failed on `{literal}`: {e:?}"));
    result.value.bits()
}

/// Evaluate through the Cranelift JIT, returning the raw encoded word.
///
/// The native backend does not implement every operation; a literal it cannot
/// compile yields `None` and is skipped rather than reported as a mismatch.
fn native_bits(literal: &str) -> Option<u64> {
    let src = program_for(literal);
    match loon_lang::eir::native::eval_native(&src) {
        Ok(v) => Some(v.bits()),
        Err(_) => None,
    }
}

/// Compile with the EIR WASM backend and run under wasmtime, returning the
/// raw encoded word. Returns `None` if the backend cannot compile the program.
fn wasm_bits(literal: &str) -> Option<u64> {
    use loon_lang::eir::backend::Backend;

    let src = program_for(literal);
    let exprs = loon_lang::parser::parse(&src).expect("parses");
    let mut checker = loon_lang::check::Checker::new();
    let errors = checker.check_program(&exprs);
    assert!(errors.is_empty(), "type errors on `{literal}`: {errors:?}");
    let module = loon_lang::eir::lower::lower(&checker);

    let mut backend = loon_lang::eir::wasm::WasmBackend;
    let bytes = backend.compile(&module).ok()?;

    let engine = wasmtime::Engine::default();
    let wasm_module = wasmtime::Module::new(&engine, &bytes).ok()?;
    let mut store = wasmtime::Store::new(&engine, ());
    let mut linker = wasmtime::Linker::new(&engine);
    // The emitter imports its two IO hooks unconditionally; a literal program
    // never calls them, but they must resolve for instantiation to succeed.
    linker
        .func_wrap("host", "println", |_: i64| {})
        .expect("bind println");
    linker
        .func_wrap("host", "print", |_: i64| {})
        .expect("bind print");
    let instance = linker.instantiate(&mut store, &wasm_module).ok()?;
    let start = instance
        .get_typed_func::<(), i64>(&mut store, "_start")
        .ok()?;
    start.call(&mut store, ()).ok().map(|v| v as u64)
}

#[test]
fn the_vm_encodes_immediates_exactly_as_the_layout_says() {
    for (name, literal, expected) in GOLDEN_IMMEDIATES {
        assert_eq!(
            vm_bits(literal),
            *expected,
            "`{name}` ({literal}): the VM disagrees with eir::layout"
        );
    }
}

#[test]
fn every_backend_agrees_on_every_immediate() {
    let mut checked_native = 0usize;
    let mut checked_wasm = 0usize;

    for (name, literal, expected) in GOLDEN_IMMEDIATES {
        let vm = vm_bits(literal);
        assert_eq!(vm, *expected, "`{name}`: VM vs layout");

        if let Some(bits) = native_bits(literal) {
            checked_native += 1;
            assert_eq!(
                bits, vm,
                "`{name}` ({literal}): native backend encodes {bits:#018x}, VM encodes {vm:#018x}"
            );
        }
        if let Some(bits) = wasm_bits(literal) {
            checked_wasm += 1;
            assert_eq!(
                bits, vm,
                "`{name}` ({literal}): wasm backend encodes {bits:#018x}, VM encodes {vm:#018x}"
            );
        }
    }

    // A backend silently compiling nothing would make this test vacuous.
    assert!(
        checked_native > 0,
        "the native backend compiled none of the golden immediates"
    );
    assert!(
        checked_wasm > 0,
        "the wasm backend compiled none of the golden immediates"
    );
}

#[test]
fn immediates_are_distinguishable_from_one_another() {
    // Encoding two different values identically is the failure mode that would
    // make the cross-backend comparison above pass while everything is broken.
    let mut seen: Vec<(&str, u64)> = Vec::new();
    for (name, literal, _) in GOLDEN_IMMEDIATES {
        let bits = vm_bits(literal);
        if let Some((other, _)) = seen.iter().find(|(_, b)| *b == bits) {
            panic!("`{name}` and `{other}` encode to the same word {bits:#018x}");
        }
        seen.push((name, bits));
    }
}

#[test]
fn the_buffer_header_layout_is_pinned() {
    // Field order and offsets are exactly what diverged across targets in the
    // Rust offload work. Pin them explicitly: a reordering of the struct is a
    // silent ABI break, and this is the test that refuses to let it be silent.
    assert_eq!(std::mem::size_of::<BufferHeader>(), BUF_HDR_SIZE);
    assert_eq!(BUF_OFF_DTYPE, 0);
    assert_eq!(BUF_OFF_LEN, 8);
    assert_eq!(BUF_OFF_DATA, 16);

    let header = BufferHeader::new(DType::F32, 4096, 0x1234_5678_9ABC_DEF0);
    let bytes = header.to_bytes();
    assert_eq!(bytes.len(), BUF_HDR_SIZE);
    assert_eq!(
        u32::from_le_bytes(bytes[BUF_OFF_DTYPE..BUF_OFF_DTYPE + 4].try_into().unwrap()),
        DType::F32 as u32
    );
    assert_eq!(
        u64::from_le_bytes(bytes[BUF_OFF_LEN..BUF_OFF_LEN + 8].try_into().unwrap()),
        4096
    );
    assert_eq!(
        u64::from_le_bytes(bytes[BUF_OFF_DATA..BUF_OFF_DATA + 8].try_into().unwrap()),
        0x1234_5678_9ABC_DEF0
    );
    assert_eq!(BufferHeader::from_bytes(&bytes), Some(header));
}

#[test]
fn a_reordered_header_does_not_round_trip() {
    // The mutation test: swap the two 64-bit fields, as a target with a
    // different struct layout effectively would, and confirm the bytes no
    // longer decode to the original. If this ever passes, the conformance
    // check above has stopped checking anything.
    let header = BufferHeader::new(DType::I32, 7, 99);
    let mut mutated = header.to_bytes();
    let (len_half, data_half): ([u8; 8], [u8; 8]) = (
        mutated[BUF_OFF_LEN..BUF_OFF_LEN + 8].try_into().unwrap(),
        mutated[BUF_OFF_DATA..BUF_OFF_DATA + 8].try_into().unwrap(),
    );
    mutated[BUF_OFF_LEN..BUF_OFF_LEN + 8].copy_from_slice(&data_half);
    mutated[BUF_OFF_DATA..BUF_OFF_DATA + 8].copy_from_slice(&len_half);

    let decoded = BufferHeader::from_bytes(&mutated).expect("still a well-formed header");
    assert_ne!(
        decoded, header,
        "swapping len and data went unnoticed — the layout check is vacuous"
    );
    assert_eq!(decoded.len, 99);
    assert_eq!(decoded.data, 7);
}

#[test]
fn gpu_placement_rejects_64_bit_element_types() {
    // WGSL core has no f64 and no 64-bit integers. Placement must refuse those
    // buffers outright rather than quietly narrowing a program's precision.
    assert!(DType::F32.gpu_ok() && DType::I32.gpu_ok());
    assert!(!DType::F64.gpu_ok() && !DType::I64.gpu_ok());
    for dtype in [DType::F32, DType::F64, DType::I32, DType::I64] {
        assert_eq!(DType::from_name(dtype.name()), Some(dtype));
    }
}

#[test]
fn int_payloads_survive_the_full_48_bit_range() {
    // The int tag carries a 48-bit signed payload. The extremes are where a
    // backend that sign-extends differently would part ways from the others.
    for n in [0i64, 1, -1, (1i64 << 47) - 1, -(1i64 << 47)] {
        let literal = n.to_string();
        let expected = nanbox::encode_int(n);
        assert_eq!(vm_bits(&literal), expected, "VM on {n}");
        if let Some(bits) = native_bits(&literal) {
            assert_eq!(bits, expected, "native on {n}");
        }
        if let Some(bits) = wasm_bits(&literal) {
            assert_eq!(bits, expected, "wasm on {n}");
        }
    }
}
