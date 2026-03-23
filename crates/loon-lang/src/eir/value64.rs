//! NaN-boxed 64-bit value representation.
//!
//! Every Loon value fits in 8 bytes. Floats pass through as raw IEEE 754.
//! Everything else uses the NaN payload space (quiet NaN with sign bit set).
//!
//! Layout:
//!   Float:     any f64 that doesn't match our tagged NaN pattern
//!   Tagged:    [1][11111111111][1][TTT][48-bit payload]
//!               ^sign          ^quiet ^tag
//!
//! Tags (3 bits):
//!   000 = heap pointer (string, closure, ADT, collection — type in header)
//!   001 = inline int (48-bit signed, ±140 trillion)
//!   010 = (reserved)
//!   011 = (reserved)
//!   100 = (reserved)
//!   101 = (reserved)
//!   110 = interned symbol/keyword (32-bit intern index)
//!   111 = immediate (Unit, True, False)

use std::fmt;

/// A NaN-boxed value in 8 bytes.
#[derive(Clone, Copy, PartialEq)]
#[repr(transparent)]
pub struct Val(u64);

// Bit layout constants
const QNAN: u64 = 0x7FF8_0000_0000_0000; // quiet NaN base
const SIGN: u64 = 0x8000_0000_0000_0000; // sign bit
const BASE: u64 = SIGN | QNAN; // all tagged values have this prefix
const TAG_MASK: u64 = 0x0007_0000_0000_0000; // bits 48-50
const PAYLOAD: u64 = 0x0000_FFFF_FFFF_FFFF; // low 48 bits

// Tag values (shifted into position)
const TAG_PTR: u64 = 0x0000_0000_0000_0000; // heap pointer
const TAG_INT: u64 = 0x0001_0000_0000_0000; // inline int48
const TAG_SYM: u64 = 0x0006_0000_0000_0000; // interned symbol/keyword
const TAG_IMM: u64 = 0x0007_0000_0000_0000; // immediate

// Immediate sub-tags (in low bits)
const IMM_UNIT: u64 = 0;
const IMM_TRUE: u64 = 1;
const IMM_FALSE: u64 = 2;

impl Val {
    // ── Constants ──────────────────────────────────────────────────────

    pub const UNIT: Val = Val(BASE | TAG_IMM | IMM_UNIT);
    pub const TRUE: Val = Val(BASE | TAG_IMM | IMM_TRUE);
    pub const FALSE: Val = Val(BASE | TAG_IMM | IMM_FALSE);

    // ── Constructors ──────────────────────────────────────────────────

    #[inline(always)]
    pub fn float(f: f64) -> Self {
        Val(f.to_bits())
    }

    #[inline(always)]
    pub fn int(n: i64) -> Self {
        // 48-bit signed range: -2^47 .. 2^47-1
        debug_assert!(
            n >= -(1i64 << 47) && n < (1i64 << 47),
            "int {n} overflows 48-bit inline range — box it"
        );
        Val(BASE | TAG_INT | ((n as u64) & PAYLOAD))
    }

    #[inline(always)]
    pub fn bool(b: bool) -> Self {
        if b {
            Self::TRUE
        } else {
            Self::FALSE
        }
    }

    /// Heap pointer (48-bit, type tag is in the heap header).
    #[inline(always)]
    pub fn ptr(p: usize) -> Self {
        debug_assert!(p & !PAYLOAD as usize == 0, "pointer exceeds 48 bits");
        Val(BASE | TAG_PTR | (p as u64 & PAYLOAD))
    }

    /// Interned symbol or keyword (32-bit index).
    #[inline(always)]
    pub fn sym(index: u32) -> Self {
        Val(BASE | TAG_SYM | index as u64)
    }

    // ── Predicates ────────────────────────────────────────────────────

    #[inline(always)]
    pub fn is_float(self) -> bool {
        // A value is a float if it doesn't have our tagged NaN prefix
        (self.0 & BASE) != BASE
    }

    #[inline(always)]
    pub fn is_int(self) -> bool {
        !self.is_float() && (self.0 & TAG_MASK) == TAG_INT
    }

    #[inline(always)]
    pub fn is_ptr(self) -> bool {
        !self.is_float() && (self.0 & TAG_MASK) == TAG_PTR
    }

    #[inline(always)]
    pub fn is_bool(self) -> bool {
        self.0 == Self::TRUE.0 || self.0 == Self::FALSE.0
    }

    #[inline(always)]
    pub fn is_unit(self) -> bool {
        self.0 == Self::UNIT.0
    }

    #[inline(always)]
    pub fn is_sym(self) -> bool {
        !self.is_float() && (self.0 & TAG_MASK) == TAG_SYM
    }

    #[inline(always)]
    pub fn is_truthy(self) -> bool {
        // Everything is truthy except false and unit
        self.0 != Self::FALSE.0 && self.0 != Self::UNIT.0
    }

    // ── Extractors ────────────────────────────────────────────────────

    #[inline(always)]
    pub fn as_float(self) -> f64 {
        f64::from_bits(self.0)
    }

    #[inline(always)]
    pub fn as_int(self) -> i64 {
        // Sign-extend from 48 bits
        let raw = (self.0 & PAYLOAD) as i64;
        (raw << 16) >> 16
    }

    #[inline(always)]
    pub fn as_bool(self) -> bool {
        self.0 == Self::TRUE.0
    }

    #[inline(always)]
    pub fn as_ptr(self) -> usize {
        (self.0 & PAYLOAD) as usize
    }

    #[inline(always)]
    pub fn as_sym(self) -> u32 {
        (self.0 & PAYLOAD) as u32
    }

    /// Raw bits (for hashing, comparison, serialization).
    #[inline(always)]
    pub fn bits(self) -> u64 {
        self.0
    }

    /// From raw bits.
    #[inline(always)]
    pub fn from_bits(bits: u64) -> Self {
        Val(bits)
    }

    /// Tag bits (for type dispatch).
    #[inline(always)]
    pub fn tag(self) -> u8 {
        if self.is_float() {
            0xFF // sentinel: it's a float
        } else {
            ((self.0 & TAG_MASK) >> 48) as u8
        }
    }
}

impl Eq for Val {}

impl std::hash::Hash for Val {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // NaN-boxed values: bit-identical values are equal
        self.0.hash(state);
    }
}

impl fmt::Debug for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_float() {
            write!(f, "Val::float({})", self.as_float())
        } else if self.is_int() {
            write!(f, "Val::int({})", self.as_int())
        } else if self.is_bool() {
            write!(f, "Val::bool({})", self.as_bool())
        } else if self.is_unit() {
            write!(f, "Val::UNIT")
        } else if self.is_ptr() {
            write!(f, "Val::ptr(0x{:x})", self.as_ptr())
        } else if self.is_sym() {
            write!(f, "Val::sym({})", self.as_sym())
        } else {
            write!(f, "Val(0x{:016x})", self.0)
        }
    }
}

// ─── Tests ─────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn roundtrip_float() {
        for f in [0.0, 1.0, -1.0, 3.14159, f64::INFINITY, f64::NEG_INFINITY] {
            let v = Val::float(f);
            assert!(v.is_float());
            assert_eq!(v.as_float(), f);
            assert!(!v.is_int());
            assert!(!v.is_ptr());
        }
    }

    #[test]
    fn roundtrip_int() {
        for n in [
            0i64,
            1,
            -1,
            42,
            -42,
            1_000_000,
            -1_000_000,
            (1i64 << 47) - 1,
            -(1i64 << 47),
        ] {
            let v = Val::int(n);
            assert!(v.is_int(), "expected int for {n}");
            assert_eq!(v.as_int(), n, "roundtrip failed for {n}");
            assert!(!v.is_float());
        }
    }

    #[test]
    fn roundtrip_bool() {
        assert_eq!(Val::bool(true), Val::TRUE);
        assert_eq!(Val::bool(false), Val::FALSE);
        assert!(Val::TRUE.is_bool());
        assert!(Val::FALSE.is_bool());
        assert!(Val::TRUE.as_bool());
        assert!(!Val::FALSE.as_bool());
    }

    #[test]
    fn unit() {
        assert!(Val::UNIT.is_unit());
        assert!(!Val::UNIT.is_float());
        assert!(!Val::UNIT.is_int());
        assert!(!Val::UNIT.is_bool());
    }

    #[test]
    fn truthiness() {
        assert!(Val::TRUE.is_truthy());
        assert!(!Val::FALSE.is_truthy());
        assert!(!Val::UNIT.is_truthy());
        assert!(Val::int(0).is_truthy()); // 0 is truthy in Loon
        assert!(Val::int(42).is_truthy());
        assert!(Val::float(0.0).is_truthy());
    }

    #[test]
    fn roundtrip_ptr() {
        let addr = 0x0000_1234_5678_9ABC_usize;
        let v = Val::ptr(addr);
        assert!(v.is_ptr());
        assert_eq!(v.as_ptr(), addr);
        assert!(!v.is_float());
        assert!(!v.is_int());
    }

    #[test]
    fn roundtrip_sym() {
        for idx in [0u32, 1, 42, 1000, u32::MAX] {
            let v = Val::sym(idx);
            assert!(v.is_sym());
            assert_eq!(v.as_sym(), idx);
        }
    }

    #[test]
    fn float_nan_is_not_tagged() {
        // A regular NaN should be treated as a float, not as a tagged value.
        // We need to ensure our tag prefix doesn't collide with standard NaN.
        let nan = Val::float(f64::NAN);
        // NaN is a float in our scheme (it doesn't have the SIGN bit set)
        assert!(nan.is_float());
        assert!(nan.as_float().is_nan());
    }

    #[test]
    fn distinct_types_dont_collide() {
        let vals = [
            Val::UNIT,
            Val::TRUE,
            Val::FALSE,
            Val::int(0),
            Val::int(1),
            Val::float(0.0),
            Val::float(1.0),
            Val::ptr(0),
            Val::sym(0),
        ];
        for (i, a) in vals.iter().enumerate() {
            for (j, b) in vals.iter().enumerate() {
                if i != j {
                    assert_ne!(a, b, "collision between {:?} and {:?}", a, b);
                }
            }
        }
    }

    #[test]
    fn size_is_8_bytes() {
        assert_eq!(std::mem::size_of::<Val>(), 8);
    }

    #[test]
    fn large_ints_need_boxing() {
        // 48-bit signed range: ±140,737,488,355,328
        // factorial(20) = 2.4×10¹⁸ — exceeds 48 bits, needs heap boxing
        let max_inline = (1i64 << 47) - 1;
        let min_inline = -(1i64 << 47);

        let v = Val::int(max_inline);
        assert_eq!(v.as_int(), max_inline);

        let v = Val::int(min_inline);
        assert_eq!(v.as_int(), min_inline);

        // Values beyond 48 bits can't use Val::int — they'll be boxed
        // as heap objects in Phase 2 (register VM with heap allocator)
        let fact20: i64 = 2_432_902_008_176_640_000;
        assert!(
            fact20 >= (1i64 << 47),
            "factorial(20) exceeds 48-bit inline range"
        );
    }
}
