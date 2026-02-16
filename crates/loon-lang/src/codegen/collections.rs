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
        // Locals: 0 = header_ptr (i64), 1 = data_ptr (i64)
        let mut instrs = Vec::new();

        // Allocate header: 24 bytes (3 * i64)
        instrs.push(GlobalGet(0));
        instrs.push(I64ExtendI32U);
        instrs.push(LocalSet(0)); // header_ptr

        instrs.push(GlobalGet(0));
        instrs.push(I32Const(24));
        instrs.push(I32Add);
        instrs.push(GlobalSet(0));

        // Allocate initial data: 32 bytes (capacity 4)
        instrs.push(GlobalGet(0));
        instrs.push(I64ExtendI32U);
        instrs.push(LocalSet(1)); // data_ptr

        instrs.push(GlobalGet(0));
        instrs.push(I32Const(32));
        instrs.push(I32Add);
        instrs.push(GlobalSet(0));

        // Store len = 0
        instrs.push(LocalGet(0));
        instrs.push(I32WrapI64);
        instrs.push(I64Const(0));
        instrs.push(I64Store(3, 0));

        // Store capacity = 4
        instrs.push(LocalGet(0));
        instrs.push(I32WrapI64);
        instrs.push(I64Const(4));
        instrs.push(I64Store(3, 8));

        // Store data_ptr
        instrs.push(LocalGet(0));
        instrs.push(I32WrapI64);
        instrs.push(LocalGet(1));
        instrs.push(I64Store(3, 16));

        // Return header_ptr
        instrs.push(LocalGet(0));

        FunctionBody {
            params: vec![],
            results: vec![ValType::I64],
            locals: vec![ValType::I64, ValType::I64],
            instructions: instrs,
        }
    }

    /// Generate `vec_push(vec_ptr: i64, val: i64) -> i64`.
    /// Copy-on-write: always allocates a new header + copies data + appends val.
    /// Returns the new vector ptr.
    pub(super) fn gen_vec_push() -> FunctionBody {
        // Params: 0 = vec_ptr, 1 = val
        // Locals: 2 = old_len, 3 = old_data_ptr, 4 = new_header, 5 = new_data_ptr
        //         6 = new_len, 7 = new_cap, 8 = i (loop counter)
        let mut instrs = Vec::new();

        // Load old_len
        instrs.push(LocalGet(0));
        instrs.push(I32WrapI64);
        instrs.push(I64Load(3, 0));
        instrs.push(LocalSet(2));

        // Load old_data_ptr
        instrs.push(LocalGet(0));
        instrs.push(I32WrapI64);
        instrs.push(I64Load(3, 16));
        instrs.push(LocalSet(3));

        // new_len = old_len + 1
        instrs.push(LocalGet(2));
        instrs.push(I64Const(1));
        instrs.push(I64Add);
        instrs.push(LocalSet(6));

        // new_cap = new_len * 2 (simple growth)
        instrs.push(LocalGet(6));
        instrs.push(I64Const(2));
        instrs.push(I64Mul);
        instrs.push(LocalSet(7));

        // Allocate new header (24 bytes)
        instrs.push(GlobalGet(0));
        instrs.push(I64ExtendI32U);
        instrs.push(LocalSet(4)); // new_header

        instrs.push(GlobalGet(0));
        instrs.push(I32Const(24));
        instrs.push(I32Add);
        instrs.push(GlobalSet(0));

        // Allocate new data (new_cap * 8 bytes)
        instrs.push(GlobalGet(0));
        instrs.push(I64ExtendI32U);
        instrs.push(LocalSet(5)); // new_data_ptr

        instrs.push(GlobalGet(0));
        instrs.push(LocalGet(7));
        instrs.push(I64Const(8));
        instrs.push(I64Mul);
        instrs.push(I32WrapI64);
        instrs.push(I32Add);
        instrs.push(GlobalSet(0));

        // Copy old data: for i in 0..old_len: new_data[i] = old_data[i]
        instrs.push(I64Const(0));
        instrs.push(LocalSet(8));

        instrs.push(Block(BlockType::Empty));
        instrs.push(Loop(BlockType::Empty));

        // if i >= old_len, break
        instrs.push(LocalGet(8));
        instrs.push(LocalGet(2));
        instrs.push(I64LtS);
        instrs.push(I64Eqz);
        instrs.push(BrIf(1));

        // new_data[i] = old_data[i]
        // dest addr
        instrs.push(LocalGet(5));
        instrs.push(LocalGet(8));
        instrs.push(I64Const(8));
        instrs.push(I64Mul);
        instrs.push(I64Add);
        instrs.push(I32WrapI64);
        // load source
        instrs.push(LocalGet(3));
        instrs.push(LocalGet(8));
        instrs.push(I64Const(8));
        instrs.push(I64Mul);
        instrs.push(I64Add);
        instrs.push(I32WrapI64);
        instrs.push(I64Load(3, 0));
        // store dest
        instrs.push(I64Store(3, 0));

        // i++
        instrs.push(LocalGet(8));
        instrs.push(I64Const(1));
        instrs.push(I64Add);
        instrs.push(LocalSet(8));
        instrs.push(Br(0));

        instrs.push(End); // end loop
        instrs.push(End); // end block

        // Store val at new_data[old_len]
        instrs.push(LocalGet(5));
        instrs.push(LocalGet(2));
        instrs.push(I64Const(8));
        instrs.push(I64Mul);
        instrs.push(I64Add);
        instrs.push(I32WrapI64);
        instrs.push(LocalGet(1));
        instrs.push(I64Store(3, 0));

        // Write new header
        instrs.push(LocalGet(4));
        instrs.push(I32WrapI64);
        instrs.push(LocalGet(6));
        instrs.push(I64Store(3, 0)); // len

        instrs.push(LocalGet(4));
        instrs.push(I32WrapI64);
        instrs.push(LocalGet(7));
        instrs.push(I64Store(3, 8)); // capacity

        instrs.push(LocalGet(4));
        instrs.push(I32WrapI64);
        instrs.push(LocalGet(5));
        instrs.push(I64Store(3, 16)); // data_ptr

        // Return new_header
        instrs.push(LocalGet(4));

        FunctionBody {
            params: vec![ValType::I64, ValType::I64],
            results: vec![ValType::I64],
            locals: vec![
                ValType::I64, // 2: old_len
                ValType::I64, // 3: old_data_ptr
                ValType::I64, // 4: new_header
                ValType::I64, // 5: new_data_ptr
                ValType::I64, // 6: new_len
                ValType::I64, // 7: new_cap
                ValType::I64, // 8: i
            ],
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
        instrs.push(I64Eqz); // 1 if idx >= len
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
