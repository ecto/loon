//! The one description of how a Loon value looks in memory.
//!
//! Every backend — the register VM, the WASM emitter, the Cranelift JIT, and
//! (later) the WGSL kernel emitter — agrees on the bits described here. Before
//! this module the NaN-boxing constants were copy-pasted into three files with
//! a "must match value64.rs" comment on top; a divergence between two backends
//! was a runtime parity failure at best and a silent wrong answer at worst.
//! That is exactly the bug class the Rust GPU-offload work hit when a slice
//! lowered as `(ptr, len)` on one target and `[i64; 2]` on another, and found
//! it by hand. Here it is one `const`, and `tests/abi_conformance.rs` proves
//! every backend still round-trips the same bytes.
//!
//! Two things live here:
//!   - [`nanbox`]: the 64-bit immediate encoding shared by all backends.
//!   - [`DType`] / [`BufferHeader`]: the dense-buffer layout that kernels and
//!     devices exchange, where "device" may be another thread, another
//!     backend, or a GPU queue.

/// NaN-boxing bit layout. See `value64.rs` for the prose version.
pub mod nanbox {
    /// Quiet-NaN base.
    pub const QNAN: u64 = 0x7FF8_0000_0000_0000;
    /// Sign bit.
    pub const SIGN: u64 = 0x8000_0000_0000_0000;
    /// Every tagged (non-float) value carries this prefix.
    pub const BASE: u64 = SIGN | QNAN;
    /// Tag bits 48-50.
    pub const TAG_MASK: u64 = 0x0007_0000_0000_0000;
    /// Payload bits 0-47.
    pub const PAYLOAD: u64 = 0x0000_FFFF_FFFF_FFFF;

    /// Heap pointer (string, closure, ADT, collection, buffer — type in header).
    pub const TAG_PTR: u64 = 0x0000_0000_0000_0000;
    /// Inline 48-bit signed integer.
    pub const TAG_INT: u64 = 0x0001_0000_0000_0000;
    /// Interned symbol/keyword (32-bit intern index).
    pub const TAG_SYM: u64 = 0x0006_0000_0000_0000;
    /// Immediate singleton (Unit, True, False, None).
    pub const TAG_IMM: u64 = 0x0007_0000_0000_0000;

    /// Immediate sub-tags, in the low bits.
    pub const IMM_UNIT: u64 = 0;
    pub const IMM_TRUE: u64 = 1;
    pub const IMM_FALSE: u64 = 2;
    pub const IMM_NONE: u64 = 3;

    /// Fully-assembled immediates. Backends that emit literals use these
    /// directly rather than re-deriving `BASE | TAG_IMM | n`.
    pub const VAL_UNIT: u64 = BASE | TAG_IMM | IMM_UNIT;
    pub const VAL_TRUE: u64 = BASE | TAG_IMM | IMM_TRUE;
    pub const VAL_FALSE: u64 = BASE | TAG_IMM | IMM_FALSE;
    pub const VAL_NONE: u64 = BASE | TAG_IMM | IMM_NONE;

    /// Encode a 48-bit signed integer the way every backend must.
    #[inline(always)]
    pub const fn encode_int(n: i64) -> u64 {
        BASE | TAG_INT | ((n as u64) & PAYLOAD)
    }

    /// Encode a float (identity — floats pass through as raw IEEE 754).
    #[inline(always)]
    pub fn encode_float(f: f64) -> u64 {
        f.to_bits()
    }
}

// ─── Dense buffers ─────────────────────────────────────────────────────────

/// Element type of a dense [`BufferHeader`].
///
/// Deliberately small. `F64`/`I64` exist because Loon's own numbers are 64-bit
/// and the CPU backends can honour them exactly; they are rejected for GPU
/// placement because WGSL core has neither (see [`DType::gpu_ok`]). A kernel
/// that wants to run on a GPU says so by using f32/i32 buffers — we never
/// silently demote precision behind the programmer's back.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u32)]
pub enum DType {
    F32 = 0,
    F64 = 1,
    I32 = 2,
    I64 = 3,
}

impl DType {
    /// Size of one element in bytes.
    pub const fn size(self) -> usize {
        match self {
            DType::F32 | DType::I32 => 4,
            DType::F64 | DType::I64 => 8,
        }
    }

    /// The WGSL scalar type name, for the kernel emitter.
    pub const fn wgsl(self) -> &'static str {
        match self {
            DType::F32 => "f32",
            DType::F64 => "f64", // not valid in WGSL core; guarded by `gpu_ok`
            DType::I32 => "i32",
            DType::I64 => "i64", // ditto
        }
    }

    /// Whether a buffer of this type can be placed on a GPU. WGSL core has no
    /// 64-bit scalars, so `F64`/`I64` are a hard error at placement time
    /// rather than a silent conversion.
    pub const fn gpu_ok(self) -> bool {
        matches!(self, DType::F32 | DType::I32)
    }

    /// Short name used in traces, diagnostics, and the record/replay tape.
    pub const fn name(self) -> &'static str {
        match self {
            DType::F32 => "f32",
            DType::F64 => "f64",
            DType::I32 => "i32",
            DType::I64 => "i64",
        }
    }

    /// Parse the name written by [`DType::name`]. Used when reading a tape.
    pub fn from_name(s: &str) -> Option<DType> {
        match s {
            "f32" => Some(DType::F32),
            "f64" => Some(DType::F64),
            "i32" => Some(DType::I32),
            "i64" => Some(DType::I64),
            _ => None,
        }
    }

    /// Reconstruct from the `#[repr(u32)]` discriminant stored in a header.
    pub fn from_u32(n: u32) -> Option<DType> {
        match n {
            0 => Some(DType::F32),
            1 => Some(DType::F64),
            2 => Some(DType::I32),
            3 => Some(DType::I64),
            _ => None,
        }
    }
}

/// How a dense buffer is described to any backend that is not the VM itself.
///
/// `data` is deliberately a `u64` rather than a pointer: on the native backend
/// it is a host address, in WASM it is a linear-memory offset, and on a device
/// it is an allocation handle. The *header* is identical in all three, which is
/// what lets one conformance test cover them all.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct BufferHeader {
    /// [`DType`] discriminant.
    pub dtype: u32,
    /// Padding so `len` is 8-byte aligned in every ABI.
    pub _pad: u32,
    /// Element count (not bytes).
    pub len: u64,
    /// Pointer, offset, or device handle — target-dependent.
    pub data: u64,
}

/// Size of [`BufferHeader`] in bytes. Asserted against `size_of` in tests.
pub const BUF_HDR_SIZE: usize = 24;
/// Byte offset of `dtype` within [`BufferHeader`].
pub const BUF_OFF_DTYPE: usize = 0;
/// Byte offset of `len`.
pub const BUF_OFF_LEN: usize = 8;
/// Byte offset of `data`.
pub const BUF_OFF_DATA: usize = 16;

impl BufferHeader {
    pub fn new(dtype: DType, len: u64, data: u64) -> Self {
        BufferHeader {
            dtype: dtype as u32,
            _pad: 0,
            len,
            data,
        }
    }

    pub fn dtype(&self) -> Option<DType> {
        DType::from_u32(self.dtype)
    }

    /// Total size of the element payload in bytes.
    pub fn byte_len(&self) -> Option<usize> {
        Some(self.len as usize * self.dtype()?.size())
    }

    /// Serialize to the canonical 24-byte little-endian form. Backends that
    /// cannot share Rust structs (WASM linear memory, a device staging
    /// buffer) use this.
    pub fn to_bytes(&self) -> [u8; BUF_HDR_SIZE] {
        let mut out = [0u8; BUF_HDR_SIZE];
        out[BUF_OFF_DTYPE..BUF_OFF_DTYPE + 4].copy_from_slice(&self.dtype.to_le_bytes());
        out[BUF_OFF_LEN..BUF_OFF_LEN + 8].copy_from_slice(&self.len.to_le_bytes());
        out[BUF_OFF_DATA..BUF_OFF_DATA + 8].copy_from_slice(&self.data.to_le_bytes());
        out
    }

    /// Inverse of [`BufferHeader::to_bytes`].
    pub fn from_bytes(b: &[u8]) -> Option<Self> {
        if b.len() < BUF_HDR_SIZE {
            return None;
        }
        let dtype = u32::from_le_bytes(b[BUF_OFF_DTYPE..BUF_OFF_DTYPE + 4].try_into().ok()?);
        let len = u64::from_le_bytes(b[BUF_OFF_LEN..BUF_OFF_LEN + 8].try_into().ok()?);
        let data = u64::from_le_bytes(b[BUF_OFF_DATA..BUF_OFF_DATA + 8].try_into().ok()?);
        DType::from_u32(dtype)?;
        Some(BufferHeader {
            dtype,
            _pad: 0,
            len,
            data,
        })
    }
}

// ─── Conformance fixtures ──────────────────────────────────────────────────

/// Values whose encoding every backend must agree on, bit for bit.
///
/// `tests/abi_conformance.rs` compiles each `source` on every available
/// backend and asserts the resulting 64-bit word equals `bits`. Add a case
/// here whenever a new immediate encoding appears.
pub const GOLDEN_IMMEDIATES: &[(&str, &str, u64)] = &[
    ("unit", "[]", nanbox::VAL_UNIT),
    ("true", "true", nanbox::VAL_TRUE),
    ("false", "false", nanbox::VAL_FALSE),
    ("none", "None", nanbox::VAL_NONE),
    ("int-zero", "0", nanbox::encode_int(0)),
    ("int-42", "42", nanbox::encode_int(42)),
    ("int-neg", "-7", nanbox::encode_int(-7)),
    (
        "int-max48",
        "140737488355327",
        nanbox::encode_int((1i64 << 47) - 1),
    ),
    (
        "int-min48",
        "-140737488355328",
        nanbox::encode_int(-(1i64 << 47)),
    ),
];

/// A byte pattern chosen to break sloppy marshalling: negative zero, a NaN
/// with a payload, a denormal, and the integer extremes. Any backend that
/// round-trips this unchanged is not quietly normalizing floats or truncating
/// through a narrower type.
pub fn golden_buffer_f32() -> (DType, Vec<f32>) {
    (
        DType::F32,
        vec![
            0.0,
            -0.0,
            1.0,
            -1.5,
            f32::MIN_POSITIVE / 2.0, // denormal
            f32::MAX,
            f32::MIN,
            f32::INFINITY,
            f32::NEG_INFINITY,
            f32::from_bits(0x7FC0_0001), // NaN with payload
            // Two ordinary values with a full mantissa, to catch a backend
            // that round-trips the special cases and mangles the mundane ones.
            // Deliberately not near any named constant: these are arbitrary
            // bit patterns, and writing them as a truncated pi would suggest
            // they meant something.
            1.234_567_9,
            -98_765.43,
        ],
    )
}

/// Integer counterpart to [`golden_buffer_f32`].
pub fn golden_buffer_i32() -> (DType, Vec<i32>) {
    (DType::I32, vec![0, -1, 1, i32::MIN, i32::MAX, 42, -42])
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn header_size_matches_the_published_constant() {
        assert_eq!(std::mem::size_of::<BufferHeader>(), BUF_HDR_SIZE);
        assert_eq!(std::mem::align_of::<BufferHeader>(), 8);
    }

    #[test]
    fn header_round_trips_through_bytes() {
        for dtype in [DType::F32, DType::F64, DType::I32, DType::I64] {
            let h = BufferHeader::new(dtype, 1024, 0xDEAD_BEEF);
            let back = BufferHeader::from_bytes(&h.to_bytes()).expect("valid header");
            assert_eq!(h, back, "{} header round-trip", dtype.name());
            assert_eq!(back.dtype(), Some(dtype));
            assert_eq!(back.byte_len(), Some(1024 * dtype.size()));
        }
    }

    #[test]
    fn header_field_offsets_are_stable() {
        // A reordering of the struct would change these, which is precisely
        // the cross-target divergence this module exists to prevent.
        let h = BufferHeader::new(DType::I32, 7, 9);
        let b = h.to_bytes();
        assert_eq!(u32::from_le_bytes(b[0..4].try_into().unwrap()), 2);
        assert_eq!(u64::from_le_bytes(b[8..16].try_into().unwrap()), 7);
        assert_eq!(u64::from_le_bytes(b[16..24].try_into().unwrap()), 9);
    }

    #[test]
    fn rejects_an_unknown_dtype_discriminant() {
        let mut b = BufferHeader::new(DType::F32, 1, 0).to_bytes();
        b[0] = 99;
        assert!(BufferHeader::from_bytes(&b).is_none());
    }

    #[test]
    fn dtype_names_round_trip() {
        for dtype in [DType::F32, DType::F64, DType::I32, DType::I64] {
            assert_eq!(DType::from_name(dtype.name()), Some(dtype));
            assert_eq!(DType::from_u32(dtype as u32), Some(dtype));
        }
    }

    #[test]
    fn only_32_bit_types_are_gpu_placeable() {
        assert!(DType::F32.gpu_ok());
        assert!(DType::I32.gpu_ok());
        assert!(!DType::F64.gpu_ok());
        assert!(!DType::I64.gpu_ok());
    }

    #[test]
    fn immediates_are_distinct_and_tagged() {
        use nanbox::*;
        let all = [VAL_UNIT, VAL_TRUE, VAL_FALSE, VAL_NONE];
        for (i, a) in all.iter().enumerate() {
            assert_eq!(a & BASE, BASE, "immediate {i} carries the tagged prefix");
            assert_eq!(a & TAG_MASK, TAG_IMM, "immediate {i} carries TAG_IMM");
            for b in all.iter().skip(i + 1) {
                assert_ne!(a, b, "immediates must be distinct");
            }
        }
    }

    #[test]
    fn int_encoding_covers_the_48_bit_range() {
        use nanbox::*;
        for n in [0i64, 1, -1, 42, -42, (1i64 << 47) - 1, -(1i64 << 47)] {
            let bits = encode_int(n);
            assert_eq!(bits & TAG_MASK, TAG_INT, "{n} is tagged as an int");
            // Sign-extend the 48-bit payload back to i64.
            let payload = bits & PAYLOAD;
            let back = ((payload << 16) as i64) >> 16;
            assert_eq!(back, n, "{n} round-trips through the payload");
        }
    }
}
