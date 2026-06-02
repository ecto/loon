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
}

impl CollectionsRuntime {
    /// Generate `vec_new() -> i64`.
    /// Allocates a header (24 bytes) + initial data array (8 * 4 = 32 bytes).
    /// Returns the header ptr as i64.
    pub(super) fn gen_vec_new() -> FunctionBody {
        // Header [len@0, cap@8, data_ptr@16]; the data array carries a shared
        // "fill" high-water-mark at data_ptr-8 so `vec_push` appends in place on
        // the frontier (amortized O(1)) and copies only when a shared array
        // would be clobbered (copy-on-branch) — same persistence as plain
        // copy-on-write. Locals: 0=header 1=arr_base 2=data_ptr.
        let mut instrs = Vec::new();
        instrs.push(GlobalGet(0));
        instrs.push(I64ExtendI32U);
        instrs.push(LocalSet(0));
        instrs.push(GlobalGet(0));
        instrs.push(I32Const(24));
        instrs.push(I32Add);
        instrs.push(GlobalSet(0));
        instrs.push(GlobalGet(0));
        instrs.push(I64ExtendI32U);
        instrs.push(LocalSet(1));
        instrs.push(GlobalGet(0));
        instrs.push(I32Const(40));
        instrs.push(I32Add);
        instrs.push(GlobalSet(0));
        instrs.push(LocalGet(1));
        instrs.push(I32WrapI64);
        instrs.push(I64Const(0));
        instrs.push(I64Store(3, 0));
        instrs.push(LocalGet(1));
        instrs.push(I64Const(8));
        instrs.push(I64Add);
        instrs.push(LocalSet(2));
        instrs.push(LocalGet(0));
        instrs.push(I32WrapI64);
        instrs.push(I64Const(0));
        instrs.push(I64Store(3, 0));
        instrs.push(LocalGet(0));
        instrs.push(I32WrapI64);
        instrs.push(I64Const(4));
        instrs.push(I64Store(3, 8));
        instrs.push(LocalGet(0));
        instrs.push(I32WrapI64);
        instrs.push(LocalGet(2));
        instrs.push(I64Store(3, 16));
        instrs.push(LocalGet(0));
        FunctionBody {
            params: vec![],
            results: vec![ValType::I64],
            locals: vec![ValType::I64; 3],
            instructions: instrs,
        }
    }

    /// Generate `vec_push(vec_ptr, val) -> new_vec_ptr`. Amortized O(1): append
    /// in place when this version sits at the array's frontier with spare
    /// capacity; otherwise copy to a (doubled when full) new array. The shared
    /// fill marker at data_ptr-8 makes this safe for persistence (copy-on-branch).
    pub(super) fn gen_vec_push() -> FunctionBody {
        // params 0=vec 1=val; locals 2=len 3=cap 4=data 5=fill 6=newcap 7=newarr
        //                            8=newdata 9=hdr 10=i
        let mut instrs = Vec::new();
        instrs.push(LocalGet(0)); instrs.push(I32WrapI64); instrs.push(I64Load(3, 0)); instrs.push(LocalSet(2));
        instrs.push(LocalGet(0)); instrs.push(I32WrapI64); instrs.push(I64Load(3, 8)); instrs.push(LocalSet(3));
        instrs.push(LocalGet(0)); instrs.push(I32WrapI64); instrs.push(I64Load(3, 16)); instrs.push(LocalSet(4));
        instrs.push(LocalGet(4)); instrs.push(I64Const(8)); instrs.push(I64Sub); instrs.push(I32WrapI64); instrs.push(I64Load(3, 0)); instrs.push(LocalSet(5));
        instrs.push(LocalGet(2)); instrs.push(LocalGet(5)); instrs.push(I64Eq);
        instrs.push(LocalGet(2)); instrs.push(LocalGet(3)); instrs.push(I64LtS);
        instrs.push(I32Add); instrs.push(I32Const(2)); instrs.push(I32Eq);
        instrs.push(If(BlockType::Result(ValType::I64)));
        // in place
        instrs.push(LocalGet(4)); instrs.push(LocalGet(2)); instrs.push(I64Const(8)); instrs.push(I64Mul); instrs.push(I64Add); instrs.push(I32WrapI64);
        instrs.push(LocalGet(1)); instrs.push(I64Store(3, 0));
        instrs.push(LocalGet(4)); instrs.push(I64Const(8)); instrs.push(I64Sub); instrs.push(I32WrapI64);
        instrs.push(LocalGet(2)); instrs.push(I64Const(1)); instrs.push(I64Add); instrs.push(I64Store(3, 0));
        instrs.push(GlobalGet(0)); instrs.push(I64ExtendI32U); instrs.push(LocalSet(9));
        instrs.push(GlobalGet(0)); instrs.push(I32Const(24)); instrs.push(I32Add); instrs.push(GlobalSet(0));
        instrs.push(LocalGet(9)); instrs.push(I32WrapI64); instrs.push(LocalGet(2)); instrs.push(I64Const(1)); instrs.push(I64Add); instrs.push(I64Store(3, 0));
        instrs.push(LocalGet(9)); instrs.push(I32WrapI64); instrs.push(LocalGet(3)); instrs.push(I64Store(3, 8));
        instrs.push(LocalGet(9)); instrs.push(I32WrapI64); instrs.push(LocalGet(4)); instrs.push(I64Store(3, 16));
        instrs.push(LocalGet(9));
        instrs.push(Else);
        // copy
        instrs.push(LocalGet(2)); instrs.push(LocalGet(3)); instrs.push(I64LtS);
        instrs.push(If(BlockType::Result(ValType::I64)));
        instrs.push(LocalGet(3));
        instrs.push(Else);
        instrs.push(LocalGet(3)); instrs.push(I64Const(2)); instrs.push(I64Mul);
        instrs.push(End);
        instrs.push(LocalSet(6));
        instrs.push(GlobalGet(0)); instrs.push(I64ExtendI32U); instrs.push(LocalSet(7));
        instrs.push(GlobalGet(0)); instrs.push(LocalGet(6)); instrs.push(I64Const(8)); instrs.push(I64Mul); instrs.push(I64Const(8)); instrs.push(I64Add); instrs.push(I32WrapI64); instrs.push(I32Add); instrs.push(GlobalSet(0));
        instrs.push(LocalGet(7)); instrs.push(I64Const(8)); instrs.push(I64Add); instrs.push(LocalSet(8));
        instrs.push(I64Const(0)); instrs.push(LocalSet(10));
        instrs.push(Block(BlockType::Empty)); instrs.push(Loop(BlockType::Empty));
        instrs.push(LocalGet(10)); instrs.push(LocalGet(2)); instrs.push(I64LtS); instrs.push(I32Eqz); instrs.push(BrIf(1));
        instrs.push(LocalGet(8)); instrs.push(LocalGet(10)); instrs.push(I64Const(8)); instrs.push(I64Mul); instrs.push(I64Add); instrs.push(I32WrapI64);
        instrs.push(LocalGet(4)); instrs.push(LocalGet(10)); instrs.push(I64Const(8)); instrs.push(I64Mul); instrs.push(I64Add); instrs.push(I32WrapI64); instrs.push(I64Load(3, 0));
        instrs.push(I64Store(3, 0));
        instrs.push(LocalGet(10)); instrs.push(I64Const(1)); instrs.push(I64Add); instrs.push(LocalSet(10));
        instrs.push(Br(0));
        instrs.push(End); instrs.push(End);
        instrs.push(LocalGet(8)); instrs.push(LocalGet(2)); instrs.push(I64Const(8)); instrs.push(I64Mul); instrs.push(I64Add); instrs.push(I32WrapI64); instrs.push(LocalGet(1)); instrs.push(I64Store(3, 0));
        instrs.push(LocalGet(7)); instrs.push(I32WrapI64); instrs.push(LocalGet(2)); instrs.push(I64Const(1)); instrs.push(I64Add); instrs.push(I64Store(3, 0));
        instrs.push(GlobalGet(0)); instrs.push(I64ExtendI32U); instrs.push(LocalSet(9));
        instrs.push(GlobalGet(0)); instrs.push(I32Const(24)); instrs.push(I32Add); instrs.push(GlobalSet(0));
        instrs.push(LocalGet(9)); instrs.push(I32WrapI64); instrs.push(LocalGet(2)); instrs.push(I64Const(1)); instrs.push(I64Add); instrs.push(I64Store(3, 0));
        instrs.push(LocalGet(9)); instrs.push(I32WrapI64); instrs.push(LocalGet(6)); instrs.push(I64Store(3, 8));
        instrs.push(LocalGet(9)); instrs.push(I32WrapI64); instrs.push(LocalGet(8)); instrs.push(I64Store(3, 16));
        instrs.push(LocalGet(9));
        instrs.push(End);
        FunctionBody {
            params: vec![ValType::I64, ValType::I64],
            results: vec![ValType::I64],
            locals: vec![ValType::I64; 9],
            instructions: instrs,
        }
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
