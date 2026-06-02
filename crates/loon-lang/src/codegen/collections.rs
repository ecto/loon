//! Persistent data structures for WASM codegen (simplified v0.5).
//!
//! Implements simple heap-allocated vectors with copy-on-write semantics:
//! - `vec_new() -> ptr` — allocate a small array header (len + capacity + data_ptr)
//! - `vec_push(vec_ptr, val) -> new_vec_ptr` — append (copy-on-write for persistence)
//! - `vec_get(vec_ptr, idx) -> val` — bounds-checked read
//!
//! Vector layout in memory:
//!   offset 0: len (i64)
//!   offset 8: capacity (i64)
//!   offset 16: data_ptr (i64) — points to array of i64 values
//!
//! Uses global 0 as heap_ptr (bump allocator), same as the main codegen.

use super::{FunctionBody, WasmInstruction::*};
use wasm_encoder::*;

/// Tracks the function indices of collection runtime helpers.
#[derive(Clone, Debug)]
pub struct CollectionsRuntime {
    /// WASM function index for vec_new
    pub vec_new_idx: u32,
    /// WASM function index for vec_push
    pub vec_push_idx: u32,
    /// WASM function index for vec_get
    pub vec_get_idx: u32,
    /// WASM function index for vec_cons (prepend)
    pub vec_cons_idx: u32,
}

impl CollectionsRuntime {
    /// Generate `vec_new() -> i64`.
    /// Allocates a header (24 bytes) + initial data array (8 * 4 = 32 bytes).
    /// Returns the header ptr as i64.
    /// A vector is a deque over a shared backing buffer, so both `conj` (append)
    /// and `cons` (prepend) are amortized O(1). Buffer: [cap@0, lo@8, hi@16,
    /// slots@24…]; `lo`/`hi` are the occupied water-marks. Header: [len@0,
    /// start@8, data_ptr@16] with `data_ptr = slots_base + start*8` pointing at
    /// element 0. In-place growth happens only at the frontier (this version's
    /// edge == the buffer's water-mark); otherwise a copy is made
    /// (copy-on-branch), preserving persistence. Locals: 0=buf 1=slots 2=data
    /// 3=hdr.
    pub(super) fn gen_vec_new() -> FunctionBody {
        let mut i = Vec::new();
        i.push(GlobalGet(0)); i.push(I64ExtendI32U); i.push(LocalSet(0));
        i.push(GlobalGet(0)); i.push(I32Const(56)); i.push(I32Add); i.push(GlobalSet(0));
        i.push(LocalGet(0)); i.push(I32WrapI64); i.push(I64Const(4)); i.push(I64Store(3, 0));   // cap
        i.push(LocalGet(0)); i.push(I32WrapI64); i.push(I64Const(2)); i.push(I64Store(3, 8));   // lo
        i.push(LocalGet(0)); i.push(I32WrapI64); i.push(I64Const(2)); i.push(I64Store(3, 16));  // hi
        i.push(LocalGet(0)); i.push(I64Const(24)); i.push(I64Add); i.push(LocalSet(1));         // slots
        i.push(LocalGet(1)); i.push(I64Const(16)); i.push(I64Add); i.push(LocalSet(2));         // data = slots+2*8
        i.push(GlobalGet(0)); i.push(I64ExtendI32U); i.push(LocalSet(3));
        i.push(GlobalGet(0)); i.push(I32Const(24)); i.push(I32Add); i.push(GlobalSet(0));
        i.push(LocalGet(3)); i.push(I32WrapI64); i.push(I64Const(0)); i.push(I64Store(3, 0));   // len
        i.push(LocalGet(3)); i.push(I32WrapI64); i.push(I64Const(2)); i.push(I64Store(3, 8));   // start
        i.push(LocalGet(3)); i.push(I32WrapI64); i.push(LocalGet(2)); i.push(I64Store(3, 16));  // data
        i.push(LocalGet(3));
        FunctionBody { params: vec![], results: vec![ValType::I64], locals: vec![ValType::I64; 4], instructions: i }
    }

    /// `vec_push(vec, val)` — append (`conj`).
    pub(super) fn gen_vec_push() -> FunctionBody {
        Self::gen_vec_grow(false)
    }

    /// `vec_cons(val, vec)` — prepend (`cons`).
    pub(super) fn gen_vec_cons() -> FunctionBody {
        Self::gen_vec_grow(true)
    }

    /// Shared body for append/prepend. Params for append: 0=vec 1=val. For
    /// prepend: 0=val 1=vec (so the surface arg order `[cons val vec]` matches).
    fn gen_vec_grow(prepend: bool) -> FunctionBody {
        let (vec, val) = if prepend { (1u32, 0u32) } else { (0u32, 1u32) };
        // locals 2=len 3=start 4=data 5=slots 6=buf 7=cap 8=lo 9=hi
        //        10=newcap 11=newbuf 12=newslots 13=newstart 14=newdata 15=hdr 16=i
        let mut i = Vec::new();
        i.push(LocalGet(vec)); i.push(I32WrapI64); i.push(I64Load(3, 0)); i.push(LocalSet(2));   // len
        i.push(LocalGet(vec)); i.push(I32WrapI64); i.push(I64Load(3, 8)); i.push(LocalSet(3));   // start
        i.push(LocalGet(vec)); i.push(I32WrapI64); i.push(I64Load(3, 16)); i.push(LocalSet(4));  // data
        // slots = data - start*8
        i.push(LocalGet(4)); i.push(LocalGet(3)); i.push(I64Const(8)); i.push(I64Mul); i.push(I64Sub); i.push(LocalSet(5));
        // buf = slots - 24
        i.push(LocalGet(5)); i.push(I64Const(24)); i.push(I64Sub); i.push(LocalSet(6));
        i.push(LocalGet(6)); i.push(I32WrapI64); i.push(I64Load(3, 0)); i.push(LocalSet(7));   // cap
        i.push(LocalGet(6)); i.push(I32WrapI64); i.push(I64Load(3, 8)); i.push(LocalSet(8));   // lo
        i.push(LocalGet(6)); i.push(I32WrapI64); i.push(I64Load(3, 16)); i.push(LocalSet(9));  // hi
        // frontier condition
        if prepend {
            // start == lo && lo > 0
            i.push(LocalGet(3)); i.push(LocalGet(8)); i.push(I64Eq);
            i.push(LocalGet(8)); i.push(I64Const(0)); i.push(I64GtS);
        } else {
            // start+len == hi && hi < cap
            i.push(LocalGet(3)); i.push(LocalGet(2)); i.push(I64Add); i.push(LocalGet(9)); i.push(I64Eq);
            i.push(LocalGet(9)); i.push(LocalGet(7)); i.push(I64LtS);
        }
        i.push(I32Add); i.push(I32Const(2)); i.push(I32Eq);
        i.push(If(BlockType::Result(ValType::I64)));
        // ---- in place ----
        if prepend {
            // slots[lo-1] = val  (= data - 8); lo = lo-1; newdata = data-8; start-1
            i.push(LocalGet(4)); i.push(I64Const(8)); i.push(I64Sub); i.push(I32WrapI64); i.push(LocalGet(val)); i.push(I64Store(3, 0));
            i.push(LocalGet(6)); i.push(I64Const(8)); i.push(I64Add); i.push(I32WrapI64); i.push(LocalGet(8)); i.push(I64Const(1)); i.push(I64Sub); i.push(I64Store(3, 0));
            i.push(LocalGet(4)); i.push(I64Const(8)); i.push(I64Sub); i.push(LocalSet(14));  // newdata
            i.push(LocalGet(3)); i.push(I64Const(1)); i.push(I64Sub); i.push(LocalSet(13));  // newstart = start-1
        } else {
            // slots[hi] = val (= data + len*8); hi = hi+1; newdata = data; start
            i.push(LocalGet(4)); i.push(LocalGet(2)); i.push(I64Const(8)); i.push(I64Mul); i.push(I64Add); i.push(I32WrapI64); i.push(LocalGet(val)); i.push(I64Store(3, 0));
            i.push(LocalGet(6)); i.push(I64Const(16)); i.push(I64Add); i.push(I32WrapI64); i.push(LocalGet(9)); i.push(I64Const(1)); i.push(I64Add); i.push(I64Store(3, 0));
            i.push(LocalGet(4)); i.push(LocalSet(14)); // newdata = data
            i.push(LocalGet(3)); i.push(LocalSet(13)); // newstart = start
        }
        // hdr = alloc(24); [len+1, newstart, newdata]
        i.push(GlobalGet(0)); i.push(I64ExtendI32U); i.push(LocalSet(15));
        i.push(GlobalGet(0)); i.push(I32Const(24)); i.push(I32Add); i.push(GlobalSet(0));
        i.push(LocalGet(15)); i.push(I32WrapI64); i.push(LocalGet(2)); i.push(I64Const(1)); i.push(I64Add); i.push(I64Store(3, 0));
        i.push(LocalGet(15)); i.push(I32WrapI64); i.push(LocalGet(13)); i.push(I64Store(3, 8));
        i.push(LocalGet(15)); i.push(I32WrapI64); i.push(LocalGet(14)); i.push(I64Store(3, 16));
        i.push(LocalGet(15));
        i.push(Else);
        // ---- copy to a fresh centered buffer ----
        // newcap: over-allocate 3x only when copying because we ran out of room
        // at the frontier (genuine growth → keeps building amortized O(1)); on a
        // *branch* copy (this version isn't the frontier — e.g. repeatedly
        // cloning a fixed vector) allocate minimally, since the fork won't grow.
        if prepend {
            i.push(LocalGet(3)); i.push(LocalGet(8)); i.push(I64Eq);  // start == lo → full
        } else {
            i.push(LocalGet(3)); i.push(LocalGet(2)); i.push(I64Add); i.push(LocalGet(9)); i.push(I64Eq); // start+len == hi → full
        }
        i.push(If(BlockType::Result(ValType::I64)));
        i.push(LocalGet(2)); i.push(I64Const(1)); i.push(I64Add); i.push(I64Const(3)); i.push(I64Mul);   // 3*(len+1)
        i.push(Else);
        i.push(LocalGet(2)); i.push(I64Const(2)); i.push(I64Add);                                         // len+2
        i.push(End);
        i.push(LocalSet(10));
        i.push(LocalGet(10)); i.push(I64Const(8)); i.push(I64LtS);
        i.push(If(BlockType::Empty)); i.push(I64Const(8)); i.push(LocalSet(10)); i.push(End);
        // newbuf = alloc(24 + newcap*8)
        i.push(GlobalGet(0)); i.push(I64ExtendI32U); i.push(LocalSet(11));
        i.push(GlobalGet(0)); i.push(LocalGet(10)); i.push(I64Const(8)); i.push(I64Mul); i.push(I64Const(24)); i.push(I64Add); i.push(I32WrapI64); i.push(I32Add); i.push(GlobalSet(0));
        // newslots = newbuf + 24
        i.push(LocalGet(11)); i.push(I64Const(24)); i.push(I64Add); i.push(LocalSet(12));
        // newstart = (newcap - (len+1)) / 2
        i.push(LocalGet(10)); i.push(LocalGet(2)); i.push(I64Const(1)); i.push(I64Add); i.push(I64Sub); i.push(I64Const(2)); i.push(I64DivS); i.push(LocalSet(13));
        // newdata = newslots + newstart*8
        i.push(LocalGet(12)); i.push(LocalGet(13)); i.push(I64Const(8)); i.push(I64Mul); i.push(I64Add); i.push(LocalSet(14));
        if prepend {
            // newdata[0] = val; copy old -> newdata+8
            i.push(LocalGet(14)); i.push(I32WrapI64); i.push(LocalGet(val)); i.push(I64Store(3, 0));
            i.push(I64Const(0)); i.push(LocalSet(16));
            i.push(Block(BlockType::Empty)); i.push(Loop(BlockType::Empty));
            i.push(LocalGet(16)); i.push(LocalGet(2)); i.push(I64LtS); i.push(I32Eqz); i.push(BrIf(1));
            // dest = newdata + 8 + i*8
            i.push(LocalGet(14)); i.push(I64Const(8)); i.push(I64Add); i.push(LocalGet(16)); i.push(I64Const(8)); i.push(I64Mul); i.push(I64Add); i.push(I32WrapI64);
            i.push(LocalGet(4)); i.push(LocalGet(16)); i.push(I64Const(8)); i.push(I64Mul); i.push(I64Add); i.push(I32WrapI64); i.push(I64Load(3, 0));
            i.push(I64Store(3, 0));
            i.push(LocalGet(16)); i.push(I64Const(1)); i.push(I64Add); i.push(LocalSet(16));
            i.push(Br(0)); i.push(End); i.push(End);
        } else {
            // copy old -> newdata; newdata[len] = val
            i.push(I64Const(0)); i.push(LocalSet(16));
            i.push(Block(BlockType::Empty)); i.push(Loop(BlockType::Empty));
            i.push(LocalGet(16)); i.push(LocalGet(2)); i.push(I64LtS); i.push(I32Eqz); i.push(BrIf(1));
            i.push(LocalGet(14)); i.push(LocalGet(16)); i.push(I64Const(8)); i.push(I64Mul); i.push(I64Add); i.push(I32WrapI64);
            i.push(LocalGet(4)); i.push(LocalGet(16)); i.push(I64Const(8)); i.push(I64Mul); i.push(I64Add); i.push(I32WrapI64); i.push(I64Load(3, 0));
            i.push(I64Store(3, 0));
            i.push(LocalGet(16)); i.push(I64Const(1)); i.push(I64Add); i.push(LocalSet(16));
            i.push(Br(0)); i.push(End); i.push(End);
            i.push(LocalGet(14)); i.push(LocalGet(2)); i.push(I64Const(8)); i.push(I64Mul); i.push(I64Add); i.push(I32WrapI64); i.push(LocalGet(val)); i.push(I64Store(3, 0));
        }
        // buffer meta: cap=newcap, lo=newstart, hi=newstart+len+1
        i.push(LocalGet(11)); i.push(I32WrapI64); i.push(LocalGet(10)); i.push(I64Store(3, 0));
        i.push(LocalGet(11)); i.push(I32WrapI64); i.push(LocalGet(13)); i.push(I64Store(3, 8));
        i.push(LocalGet(11)); i.push(I32WrapI64); i.push(LocalGet(13)); i.push(LocalGet(2)); i.push(I64Add); i.push(I64Const(1)); i.push(I64Add); i.push(I64Store(3, 16));
        // hdr = alloc(24); [len+1, newstart, newdata]
        i.push(GlobalGet(0)); i.push(I64ExtendI32U); i.push(LocalSet(15));
        i.push(GlobalGet(0)); i.push(I32Const(24)); i.push(I32Add); i.push(GlobalSet(0));
        i.push(LocalGet(15)); i.push(I32WrapI64); i.push(LocalGet(2)); i.push(I64Const(1)); i.push(I64Add); i.push(I64Store(3, 0));
        i.push(LocalGet(15)); i.push(I32WrapI64); i.push(LocalGet(13)); i.push(I64Store(3, 8));
        i.push(LocalGet(15)); i.push(I32WrapI64); i.push(LocalGet(14)); i.push(I64Store(3, 16));
        i.push(LocalGet(15));
        i.push(End);
        FunctionBody { params: vec![ValType::I64, ValType::I64], results: vec![ValType::I64], locals: vec![ValType::I64; 15], instructions: i }
    }

    /// Generate `vec_get(vec_ptr: i64, idx: i64) -> i64`.
    /// Bounds-checked read. Returns 0 if out of bounds.
    pub(super) fn gen_vec_get() -> FunctionBody {
        // Params: 0 = vec_ptr, 1 = idx
        // Locals: 2 = len, 3 = data_ptr
        let mut instrs = Vec::new();

        // Load len
        instrs.push(LocalGet(0));
        instrs.push(I32WrapI64);
        instrs.push(I64Load(3, 0));
        instrs.push(LocalSet(2));

        // Bounds check: if idx >= len, return 0
        instrs.push(LocalGet(1));
        instrs.push(LocalGet(2));
        instrs.push(I64LtS);
        instrs.push(I32Eqz); // 1 if idx >= len
        instrs.push(If(BlockType::Result(ValType::I64)));
        instrs.push(I64Const(0));
        instrs.push(Else);

        // Load data_ptr
        instrs.push(LocalGet(0));
        instrs.push(I32WrapI64);
        instrs.push(I64Load(3, 16));
        instrs.push(LocalSet(3));

        // Load data[idx]
        instrs.push(LocalGet(3));
        instrs.push(LocalGet(1));
        instrs.push(I64Const(8));
        instrs.push(I64Mul);
        instrs.push(I64Add);
        instrs.push(I32WrapI64);
        instrs.push(I64Load(3, 0));

        instrs.push(End);

        FunctionBody {
            params: vec![ValType::I64, ValType::I64],
            results: vec![ValType::I64],
            locals: vec![
                ValType::I64, // 2: len
                ValType::I64, // 3: data_ptr
            ],
            instructions: instrs,
        }
    }
}
