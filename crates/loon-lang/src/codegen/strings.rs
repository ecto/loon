//! String runtime for WASM codegen.
//!
//! Strings are represented as packed i64 values: `(ptr << 32) | len`.
//! The ptr points into linear memory where the UTF-8 bytes live.
//!
//! This module generates WASM helper functions that the compiler can call:
//! - `str_concat(a: i64, b: i64) -> i64`
//! - `str_len(s: i64) -> i64`
//! - `str_eq(a: i64, b: i64) -> i32`

use super::{FunctionBody, WasmInstruction, WasmInstruction::*};
use wasm_encoder::*;

/// Tracks the function indices of string runtime helpers.
#[derive(Clone, Debug)]
pub struct StringRuntime {
    /// WASM function index for str_concat
    pub str_concat_idx: u32,
    /// WASM function index for str_len
    pub str_len_idx: u32,
    /// WASM function index for str_eq
    pub str_eq_idx: u32,
}

#[allow(dead_code)]
impl StringRuntime {
    /// Pack a (ptr, len) pair into an i64: `(ptr << 32) | len`.
    /// Emits instructions that expect ptr (i64) and len (i64) on the stack,
    /// and leave a packed i64.
    pub(super) fn emit_pack(instructions: &mut Vec<WasmInstruction>) {
        // Stack: [ptr_i64, len_i64]
        // We need: (ptr << 32) | len
        // Save len, shift ptr, OR together
        // Assume caller will use locals for this — here we provide the raw ops.
        // Actually, provide a sequence that uses the stack directly:
        // swap isn't available in wasm, so caller should use locals.
        // Instead, provide instructions assuming: local_ptr and local_len are set.
        // This is a helper doc — the actual packing is done inline by the compiler.
        let _ = instructions;
    }

    /// Emit pack instructions given ptr and len already in locals.
    pub(super) fn emit_pack_from_locals(
        instructions: &mut Vec<WasmInstruction>,
        ptr_local: u32,
        len_local: u32,
    ) {
        // (ptr << 32) | len
        instructions.push(LocalGet(ptr_local));
        instructions.push(I64Const(32));
        instructions.push(I64Shl);
        instructions.push(LocalGet(len_local));
        instructions.push(I64Or);
    }

    /// Emit unpack-ptr: extract ptr from packed string.
    /// Expects packed i64 on stack, leaves ptr as i64.
    pub(super) fn emit_unpack_ptr(instructions: &mut Vec<WasmInstruction>) {
        instructions.push(I64Const(32));
        instructions.push(I64ShrU);
    }

    /// Emit unpack-len: extract len from packed string.
    /// Expects packed i64 on stack, leaves len as i64.
    pub(super) fn emit_unpack_len(instructions: &mut Vec<WasmInstruction>) {
        instructions.push(I64Const(0xFFFFFFFF));
        instructions.push(I64And);
    }

    /// Generate the `str_len` function body.
    /// Signature: `(s: i64) -> i64`
    /// Returns the lower 32 bits of the packed representation.
    pub(super) fn gen_str_len() -> FunctionBody {
        let mut instrs = Vec::new();
        // param 0 = s (packed i64)
        instrs.push(LocalGet(0));
        instrs.push(I64Const(0xFFFFFFFF));
        instrs.push(I64And);

        FunctionBody {
            params: vec![ValType::I64],
            results: vec![ValType::I64],
            locals: vec![],
            instructions: instrs,
        }
    }

    /// Generate the `str_eq` function body.
    /// Signature: `(a: i64, b: i64) -> i64`
    /// Byte-by-byte comparison. Returns 1 (true) or 0 (false) as i64.
    ///
    /// Algorithm:
    /// 1. Extract len_a and len_b. If different, return 0.
    /// 2. Extract ptr_a and ptr_b.
    /// 3. Loop over bytes, comparing each. If any differ, return 0.
    /// 4. Return 1.
    pub(super) fn gen_str_eq() -> FunctionBody {
        // We use locals:
        // 0 = a (param), 1 = b (param)
        // 2 = len_a, 3 = len_b, 4 = ptr_a, 5 = ptr_b, 6 = i (loop counter)
        let mut instrs = Vec::new();

        // Extract len_a
        instrs.push(LocalGet(0));
        instrs.push(I64Const(0xFFFFFFFF));
        instrs.push(I64And);
        instrs.push(LocalSet(2)); // len_a

        // Extract len_b
        instrs.push(LocalGet(1));
        instrs.push(I64Const(0xFFFFFFFF));
        instrs.push(I64And);
        instrs.push(LocalSet(3)); // len_b

        // If len_a != len_b, return 0
        instrs.push(LocalGet(2));
        instrs.push(LocalGet(3));
        instrs.push(I64Eq);
        instrs.push(I64Eqz);
        instrs.push(If(BlockType::Result(ValType::I64)));
        instrs.push(I64Const(0));
        instrs.push(Return);
        instrs.push(End);

        // Extract ptr_a
        instrs.push(LocalGet(0));
        instrs.push(I64Const(32));
        instrs.push(I64ShrU);
        instrs.push(LocalSet(4)); // ptr_a

        // Extract ptr_b
        instrs.push(LocalGet(1));
        instrs.push(I64Const(32));
        instrs.push(I64ShrU);
        instrs.push(LocalSet(5)); // ptr_b

        // i = 0
        instrs.push(I64Const(0));
        instrs.push(LocalSet(6)); // i

        // Loop: while i < len_a
        instrs.push(Block(BlockType::Empty)); // block (break target)
        instrs.push(Loop(BlockType::Empty));  // loop

        // if i >= len_a, break
        instrs.push(LocalGet(6));
        instrs.push(LocalGet(2));
        instrs.push(I64GtS); // i64.ge_s not available, use (i >= len) == !(i < len)
        // Actually we don't have ge_s. Use: if !(i < len_a) break
        // But we pushed GtS which is i > len. We need i >= len.
        // i >= len is equivalent to !(i < len). Or: (len - i) <= 0 which is (len - i) eqz when len==i
        // Simplest: check i == len_a
        // Redo: pop the GtS result
        // Actually, let me restructure. Remove the GtS.

        // Remove last instruction (I64GtS) — we'll redo the check
        instrs.pop(); // remove I64GtS
        instrs.pop(); // remove LocalGet(2)
        instrs.pop(); // remove LocalGet(6)

        // Check: i < len_a. If not, break.
        instrs.push(LocalGet(6));
        instrs.push(LocalGet(2));
        instrs.push(I64LtS);
        instrs.push(I64Eqz); // now: 1 if i >= len_a
        instrs.push(BrIf(1)); // break out of block if i >= len_a

        // Load byte at ptr_a + i
        instrs.push(LocalGet(4));
        instrs.push(LocalGet(6));
        instrs.push(I64Add);
        instrs.push(I32WrapI64);
        instrs.push(I32Load8U(0, 0)); // load byte

        // Load byte at ptr_b + i
        instrs.push(LocalGet(5));
        instrs.push(LocalGet(6));
        instrs.push(I64Add);
        instrs.push(I32WrapI64);
        instrs.push(I32Load8U(0, 0)); // load byte

        // Compare bytes. If not equal, return 0.
        instrs.push(I32Eq);
        instrs.push(I32Eqz);
        instrs.push(If(BlockType::Empty));
        instrs.push(I64Const(0));
        instrs.push(Return);
        instrs.push(End);

        // i++
        instrs.push(LocalGet(6));
        instrs.push(I64Const(1));
        instrs.push(I64Add);
        instrs.push(LocalSet(6));

        // Continue loop
        instrs.push(Br(0)); // branch to loop start
        instrs.push(End); // end loop
        instrs.push(End); // end block

        // All bytes matched — return 1
        instrs.push(I64Const(1));

        FunctionBody {
            params: vec![ValType::I64, ValType::I64],
            results: vec![ValType::I64],
            locals: vec![
                ValType::I64, // 2: len_a
                ValType::I64, // 3: len_b
                ValType::I64, // 4: ptr_a
                ValType::I64, // 5: ptr_b
                ValType::I64, // 6: i
            ],
            instructions: instrs,
        }
    }

    /// Generate the `str_concat` function body.
    /// Signature: `(a: i64, b: i64) -> i64`
    /// Allocates new string, copies both, returns packed i64.
    ///
    /// Uses global 0 as heap_ptr (bump allocator).
    pub(super) fn gen_str_concat() -> FunctionBody {
        // Locals:
        // 0 = a (param), 1 = b (param)
        // 2 = len_a, 3 = len_b, 4 = ptr_a, 5 = ptr_b
        // 6 = new_len, 7 = new_ptr (i64), 8 = i (loop counter)
        let mut instrs = Vec::new();

        // Extract len_a
        instrs.push(LocalGet(0));
        instrs.push(I64Const(0xFFFFFFFF));
        instrs.push(I64And);
        instrs.push(LocalSet(2));

        // Extract len_b
        instrs.push(LocalGet(1));
        instrs.push(I64Const(0xFFFFFFFF));
        instrs.push(I64And);
        instrs.push(LocalSet(3));

        // Extract ptr_a
        instrs.push(LocalGet(0));
        instrs.push(I64Const(32));
        instrs.push(I64ShrU);
        instrs.push(LocalSet(4));

        // Extract ptr_b
        instrs.push(LocalGet(1));
        instrs.push(I64Const(32));
        instrs.push(I64ShrU);
        instrs.push(LocalSet(5));

        // new_len = len_a + len_b
        instrs.push(LocalGet(2));
        instrs.push(LocalGet(3));
        instrs.push(I64Add);
        instrs.push(LocalSet(6));

        // Allocate: new_ptr = heap_ptr; heap_ptr += new_len
        instrs.push(GlobalGet(0)); // heap_ptr (i32)
        instrs.push(I64ExtendI32U);
        instrs.push(LocalSet(7)); // new_ptr as i64

        instrs.push(GlobalGet(0));
        instrs.push(LocalGet(6));
        instrs.push(I32WrapI64);
        instrs.push(I32Add);
        instrs.push(GlobalSet(0));

        // Copy bytes from a: i = 0; while i < len_a: mem[new_ptr+i] = mem[ptr_a+i]; i++
        instrs.push(I64Const(0));
        instrs.push(LocalSet(8));

        instrs.push(Block(BlockType::Empty));
        instrs.push(Loop(BlockType::Empty));

        // if i >= len_a, break
        instrs.push(LocalGet(8));
        instrs.push(LocalGet(2));
        instrs.push(I64LtS);
        instrs.push(I64Eqz);
        instrs.push(BrIf(1));

        // store: mem[new_ptr + i] = mem[ptr_a + i]
        instrs.push(LocalGet(7));
        instrs.push(LocalGet(8));
        instrs.push(I64Add);
        instrs.push(I32WrapI64);
        // load source byte
        instrs.push(LocalGet(4));
        instrs.push(LocalGet(8));
        instrs.push(I64Add);
        instrs.push(I32WrapI64);
        instrs.push(I32Load8U(0, 0));
        // store dest byte
        instrs.push(I32Store8(0, 0));

        // i++
        instrs.push(LocalGet(8));
        instrs.push(I64Const(1));
        instrs.push(I64Add);
        instrs.push(LocalSet(8));
        instrs.push(Br(0));

        instrs.push(End); // end loop
        instrs.push(End); // end block

        // Copy bytes from b: i = 0; while i < len_b: mem[new_ptr+len_a+i] = mem[ptr_b+i]; i++
        instrs.push(I64Const(0));
        instrs.push(LocalSet(8));

        instrs.push(Block(BlockType::Empty));
        instrs.push(Loop(BlockType::Empty));

        // if i >= len_b, break
        instrs.push(LocalGet(8));
        instrs.push(LocalGet(3));
        instrs.push(I64LtS);
        instrs.push(I64Eqz);
        instrs.push(BrIf(1));

        // store: mem[new_ptr + len_a + i] = mem[ptr_b + i]
        instrs.push(LocalGet(7));
        instrs.push(LocalGet(2));
        instrs.push(I64Add);
        instrs.push(LocalGet(8));
        instrs.push(I64Add);
        instrs.push(I32WrapI64);
        // load source byte
        instrs.push(LocalGet(5));
        instrs.push(LocalGet(8));
        instrs.push(I64Add);
        instrs.push(I32WrapI64);
        instrs.push(I32Load8U(0, 0));
        // store
        instrs.push(I32Store8(0, 0));

        // i++
        instrs.push(LocalGet(8));
        instrs.push(I64Const(1));
        instrs.push(I64Add);
        instrs.push(LocalSet(8));
        instrs.push(Br(0));

        instrs.push(End); // end loop
        instrs.push(End); // end block

        // Return packed: (new_ptr << 32) | new_len
        instrs.push(LocalGet(7));
        instrs.push(I64Const(32));
        instrs.push(I64Shl);
        instrs.push(LocalGet(6));
        instrs.push(I64Or);

        FunctionBody {
            params: vec![ValType::I64, ValType::I64],
            results: vec![ValType::I64],
            locals: vec![
                ValType::I64, // 2: len_a
                ValType::I64, // 3: len_b
                ValType::I64, // 4: ptr_a
                ValType::I64, // 5: ptr_b
                ValType::I64, // 6: new_len
                ValType::I64, // 7: new_ptr
                ValType::I64, // 8: i
            ],
            instructions: instrs,
        }
    }
}
