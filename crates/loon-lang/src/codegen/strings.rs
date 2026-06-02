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
    /// WASM function index for str_substring
    pub str_substring_idx: u32,
    /// WASM function index for int_to_str
    pub int_to_str_idx: u32,
    /// WASM function index for to_str (coerce int-or-string → string)
    pub to_str_idx: u32,
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
        instrs.push(I32Eqz); // i64.eq yields i32, so negate with i32.eqz
        instrs.push(If(BlockType::Empty)); // then-arm returns, nothing falls through
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
        instrs.push(Loop(BlockType::Empty)); // loop

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
        instrs.push(I32Eqz); // i64.lt_s yields i32; now: 1 if i >= len_a
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

    /// Generate `str_substring(s: i64, start: i64, end: i64) -> i64`.
    /// Returns a freshly allocated packed string with bytes `[start, end)` of
    /// `s`. No bounds checking (callers/typing are expected to keep it sane).
    pub(super) fn gen_str_substring() -> FunctionBody {
        // Params: 0 = s, 1 = start, 2 = end
        // Locals: 3 = src_ptr, 4 = new_len, 5 = new_ptr, 6 = i
        let mut instrs = Vec::new();

        // src_ptr = (s >> 32) + start
        instrs.push(LocalGet(0));
        instrs.push(I64Const(32));
        instrs.push(I64ShrU);
        instrs.push(LocalGet(1));
        instrs.push(I64Add);
        instrs.push(LocalSet(3));

        // new_len = end - start
        instrs.push(LocalGet(2));
        instrs.push(LocalGet(1));
        instrs.push(I64Sub);
        instrs.push(LocalSet(4));

        // Allocate new_len bytes: new_ptr = heap_ptr; heap_ptr += new_len
        instrs.push(GlobalGet(0));
        instrs.push(I64ExtendI32U);
        instrs.push(LocalSet(5));
        instrs.push(GlobalGet(0));
        instrs.push(LocalGet(4));
        instrs.push(I32WrapI64);
        instrs.push(I32Add);
        instrs.push(GlobalSet(0));

        // Copy: for i in 0..new_len: new_ptr[i] = src_ptr[i]
        instrs.push(I64Const(0));
        instrs.push(LocalSet(6));
        instrs.push(Block(BlockType::Empty));
        instrs.push(Loop(BlockType::Empty));
        instrs.push(LocalGet(6));
        instrs.push(LocalGet(4));
        instrs.push(I64LtS);
        instrs.push(I32Eqz);
        instrs.push(BrIf(1));
        // dest addr = new_ptr + i
        instrs.push(LocalGet(5));
        instrs.push(LocalGet(6));
        instrs.push(I64Add);
        instrs.push(I32WrapI64);
        // src byte = mem[src_ptr + i]
        instrs.push(LocalGet(3));
        instrs.push(LocalGet(6));
        instrs.push(I64Add);
        instrs.push(I32WrapI64);
        instrs.push(I32Load8U(0, 0));
        instrs.push(I32Store8(0, 0));
        // i++
        instrs.push(LocalGet(6));
        instrs.push(I64Const(1));
        instrs.push(I64Add);
        instrs.push(LocalSet(6));
        instrs.push(Br(0));
        instrs.push(End); // loop
        instrs.push(End); // block

        // return (new_ptr << 32) | new_len
        instrs.push(LocalGet(5));
        instrs.push(I64Const(32));
        instrs.push(I64Shl);
        instrs.push(LocalGet(4));
        instrs.push(I64Or);

        FunctionBody {
            params: vec![ValType::I64, ValType::I64, ValType::I64],
            results: vec![ValType::I64],
            locals: vec![
                ValType::I64, // 3: src_ptr
                ValType::I64, // 4: new_len
                ValType::I64, // 5: new_ptr
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
        instrs.push(I32Eqz);
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
        instrs.push(I32Eqz);
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

    /// Generate `int_to_str(n: i64) -> i64` (packed string of n's decimal form).
    /// Handles zero and negatives. Writes digits least-significant-first into a
    /// freshly bump-allocated buffer.
    pub(super) fn gen_int_to_str() -> FunctionBody {
        // Locals: 0=val(param) 1=neg 2=count 3=tmp 4=ptr 5=total_len 6=pos
        let mut instrs = Vec::new();
        // neg = 0; if val < 0 { neg = 1; val = -val }
        instrs.push(I64Const(0));
        instrs.push(LocalSet(1));
        instrs.push(LocalGet(0));
        instrs.push(I64Const(0));
        instrs.push(I64LtS);
        instrs.push(If(BlockType::Empty));
        instrs.push(I64Const(1));
        instrs.push(LocalSet(1));
        instrs.push(I64Const(0));
        instrs.push(LocalGet(0));
        instrs.push(I64Sub);
        instrs.push(LocalSet(0));
        instrs.push(End);
        // count = 0; tmp = val; do { count++; tmp /= 10 } while tmp != 0
        instrs.push(I64Const(0));
        instrs.push(LocalSet(2));
        instrs.push(LocalGet(0));
        instrs.push(LocalSet(3));
        instrs.push(Loop(BlockType::Empty));
        instrs.push(LocalGet(2));
        instrs.push(I64Const(1));
        instrs.push(I64Add);
        instrs.push(LocalSet(2));
        instrs.push(LocalGet(3));
        instrs.push(I64Const(10));
        instrs.push(I64DivS);
        instrs.push(LocalSet(3));
        instrs.push(LocalGet(3));
        instrs.push(I64Const(0));
        instrs.push(I64Ne);
        instrs.push(BrIf(0));
        instrs.push(End);
        // total_len = count + neg
        instrs.push(LocalGet(2));
        instrs.push(LocalGet(1));
        instrs.push(I64Add);
        instrs.push(LocalSet(5));
        // ptr = heap; heap += total_len
        instrs.push(GlobalGet(0));
        instrs.push(I64ExtendI32U);
        instrs.push(LocalSet(4));
        instrs.push(GlobalGet(0));
        instrs.push(LocalGet(5));
        instrs.push(I32WrapI64);
        instrs.push(I32Add);
        instrs.push(GlobalSet(0));
        // if neg { mem[ptr] = '-' }
        instrs.push(LocalGet(1));
        instrs.push(I64Eqz);
        instrs.push(I32Eqz);
        instrs.push(If(BlockType::Empty));
        instrs.push(LocalGet(4));
        instrs.push(I32WrapI64);
        instrs.push(I32Const(45)); // '-'
        instrs.push(I32Store8(0, 0));
        instrs.push(End);
        // pos = ptr + total_len - 1; tmp = val
        instrs.push(LocalGet(4));
        instrs.push(LocalGet(5));
        instrs.push(I64Add);
        instrs.push(I64Const(1));
        instrs.push(I64Sub);
        instrs.push(LocalSet(6));
        instrs.push(LocalGet(0));
        instrs.push(LocalSet(3));
        // do { mem[pos] = '0' + tmp%10; pos--; tmp /= 10 } while tmp != 0
        instrs.push(Loop(BlockType::Empty));
        instrs.push(LocalGet(6));
        instrs.push(I32WrapI64);
        instrs.push(I32Const(48)); // '0'
        instrs.push(LocalGet(3));
        instrs.push(I64Const(10));
        instrs.push(I64RemS);
        instrs.push(I32WrapI64);
        instrs.push(I32Add);
        instrs.push(I32Store8(0, 0));
        instrs.push(LocalGet(6));
        instrs.push(I64Const(1));
        instrs.push(I64Sub);
        instrs.push(LocalSet(6));
        instrs.push(LocalGet(3));
        instrs.push(I64Const(10));
        instrs.push(I64DivS);
        instrs.push(LocalSet(3));
        instrs.push(LocalGet(3));
        instrs.push(I64Const(0));
        instrs.push(I64Ne);
        instrs.push(BrIf(0));
        instrs.push(End);
        // return (ptr << 32) | total_len
        instrs.push(LocalGet(4));
        instrs.push(I64Const(32));
        instrs.push(I64Shl);
        instrs.push(LocalGet(5));
        instrs.push(I64Or);

        FunctionBody {
            params: vec![ValType::I64],
            results: vec![ValType::I64],
            locals: vec![ValType::I64; 6], // 1..=6
            instructions: instrs,
        }
    }

    /// Generate `to_str(v: i64) -> i64`: pass packed strings through unchanged,
    /// convert small integers to decimal. Strings pack a heap ptr (≥ 1024) into
    /// the high 32 bits, so any value ≥ 2^32 is already a string; anything
    /// smaller (including negatives) is treated as an integer.
    pub(super) fn gen_to_str(int_to_str_idx: u32) -> FunctionBody {
        // A packed string is `(ptr << 32) | len` with `ptr` a real heap address,
        // so a *valid* string always has `ptr < heap_ptr` (global 0). A large
        // integer can also be ≥ 2³², but its high half is normally well past the
        // heap pointer — use that to tell a big int from a string pointer rather
        // than reading it as one (which would fault).
        let mut instrs = Vec::new();
        instrs.push(LocalGet(0));
        instrs.push(I64Const(0x1_0000_0000));
        instrs.push(I64GeS);
        instrs.push(If(BlockType::Result(ValType::I64)));
        // ptr = value >> 32; is it a live heap address (< heap_ptr)?
        instrs.push(LocalGet(0));
        instrs.push(I64Const(32));
        instrs.push(I64ShrU);
        instrs.push(GlobalGet(0));
        instrs.push(I64ExtendI32U);
        instrs.push(I64LtS);
        instrs.push(If(BlockType::Result(ValType::I64)));
        instrs.push(LocalGet(0)); // a real string — pass through
        instrs.push(Else);
        instrs.push(LocalGet(0)); // a large integer
        instrs.push(Call(int_to_str_idx));
        instrs.push(End);
        instrs.push(Else);
        instrs.push(LocalGet(0));
        instrs.push(Call(int_to_str_idx));
        instrs.push(End);

        FunctionBody {
            params: vec![ValType::I64],
            results: vec![ValType::I64],
            locals: vec![],
            instructions: instrs,
        }
    }

    /// Generate `lowercase`/`uppercase` (`upper` selects which): copy the
    /// string, mapping ASCII letters to the chosen case. Other bytes pass
    /// through unchanged.
    pub(super) fn gen_str_case(upper: bool) -> FunctionBody {
        // param 0 = s; locals 1=src_ptr 2=len 3=new_ptr 4=i 5=byte (all i64)
        let (lo, hi, delta_op): (i64, i64, fn() -> WasmInstruction) = if upper {
            (97, 122, || I64Sub) // a..z → -32
        } else {
            (65, 90, || I64Add) // A..Z → +32
        };
        let mut i = Vec::new();
        i.push(LocalGet(0));
        i.push(I64Const(32));
        i.push(I64ShrU);
        i.push(LocalSet(1));
        i.push(LocalGet(0));
        i.push(I64Const(0xFFFFFFFF));
        i.push(I64And);
        i.push(LocalSet(2));
        // new_ptr = heap; heap += len
        i.push(GlobalGet(0));
        i.push(I64ExtendI32U);
        i.push(LocalSet(3));
        i.push(GlobalGet(0));
        i.push(LocalGet(2));
        i.push(I32WrapI64);
        i.push(I32Add);
        i.push(GlobalSet(0));
        i.push(I64Const(0));
        i.push(LocalSet(4));
        i.push(Block(BlockType::Empty));
        i.push(Loop(BlockType::Empty));
        i.push(LocalGet(4));
        i.push(LocalGet(2));
        i.push(I64LtS);
        i.push(I32Eqz);
        i.push(BrIf(1));
        // byte = mem[src_ptr + i]  (load i32, widen to i64)
        i.push(LocalGet(1));
        i.push(LocalGet(4));
        i.push(I64Add);
        i.push(I32WrapI64);
        i.push(I32Load8U(0, 0));
        i.push(I64ExtendI32U);
        i.push(LocalSet(5));
        // if lo <= byte <= hi: byte = byte ± 32
        // (i64 comparisons each yield an i32 0/1; sum == 2 means both held —
        // there is no i32 `and` in the instruction set.)
        i.push(LocalGet(5));
        i.push(I64Const(lo));
        i.push(I64GeS);
        i.push(LocalGet(5));
        i.push(I64Const(hi));
        i.push(I64LeS);
        i.push(I32Add);
        i.push(I32Const(2));
        i.push(I32Eq);
        i.push(If(BlockType::Empty));
        i.push(LocalGet(5));
        i.push(I64Const(32));
        i.push(delta_op());
        i.push(LocalSet(5));
        i.push(End);
        // mem[new_ptr + i] = byte
        i.push(LocalGet(3));
        i.push(LocalGet(4));
        i.push(I64Add);
        i.push(I32WrapI64);
        i.push(LocalGet(5));
        i.push(I32WrapI64);
        i.push(I32Store8(0, 0));
        i.push(LocalGet(4));
        i.push(I64Const(1));
        i.push(I64Add);
        i.push(LocalSet(4));
        i.push(Br(0));
        i.push(End);
        i.push(End);
        // return (new_ptr << 32) | len
        i.push(LocalGet(3));
        i.push(I64Const(32));
        i.push(I64Shl);
        i.push(LocalGet(2));
        i.push(I64Or);
        FunctionBody {
            params: vec![ValType::I64],
            results: vec![ValType::I64],
            locals: vec![ValType::I64; 5],
            instructions: i,
        }
    }

    /// Generate `split(s, sep) -> i64` — a vector of the substrings of `s`
    /// separated by `sep`. Consecutive/leading/trailing separators yield empty
    /// segments (matching the interpreter); the final segment is always pushed.
    pub(super) fn gen_split(
        str_substring_idx: u32,
        vec_new_idx: u32,
        vec_push_idx: u32,
    ) -> FunctionBody {
        // locals: 2=ptr_s 3=len_s 4=ptr_sep 5=len_sep 6=result 7=start 8=i
        //         9=j 10=matched
        let mut i = Vec::new();
        // unpack s
        i.push(LocalGet(0));
        i.push(I64Const(32));
        i.push(I64ShrU);
        i.push(LocalSet(2));
        i.push(LocalGet(0));
        i.push(I64Const(0xFFFFFFFF));
        i.push(I64And);
        i.push(LocalSet(3));
        // unpack sep
        i.push(LocalGet(1));
        i.push(I64Const(32));
        i.push(I64ShrU);
        i.push(LocalSet(4));
        i.push(LocalGet(1));
        i.push(I64Const(0xFFFFFFFF));
        i.push(I64And);
        i.push(LocalSet(5));
        // result = vec_new(); start = 0; i = 0
        i.push(Call(vec_new_idx));
        i.push(LocalSet(6));
        i.push(I64Const(0));
        i.push(LocalSet(7));
        i.push(I64Const(0));
        i.push(LocalSet(8));
        // outer scan
        i.push(Block(BlockType::Empty)); // A
        i.push(Loop(BlockType::Empty)); // B
        // if i + len_sep > len_s: break A
        i.push(LocalGet(8));
        i.push(LocalGet(5));
        i.push(I64Add);
        i.push(LocalGet(3));
        i.push(I64GtS);
        i.push(BrIf(1));
        // matched = 1; j = 0
        i.push(I64Const(1));
        i.push(LocalSet(10));
        i.push(I64Const(0));
        i.push(LocalSet(9));
        i.push(Block(BlockType::Empty)); // C
        i.push(Loop(BlockType::Empty)); // D
        // if j >= len_sep: break C
        i.push(LocalGet(9));
        i.push(LocalGet(5));
        i.push(I64LtS);
        i.push(I32Eqz);
        i.push(BrIf(1));
        // mem[ptr_s + i + j]
        i.push(LocalGet(2));
        i.push(LocalGet(8));
        i.push(I64Add);
        i.push(LocalGet(9));
        i.push(I64Add);
        i.push(I32WrapI64);
        i.push(I32Load8U(0, 0));
        // mem[ptr_sep + j]
        i.push(LocalGet(4));
        i.push(LocalGet(9));
        i.push(I64Add);
        i.push(I32WrapI64);
        i.push(I32Load8U(0, 0));
        // if bytes differ: matched = 0; break C
        i.push(I32Eq);
        i.push(I32Eqz);
        i.push(If(BlockType::Empty)); // E
        i.push(I64Const(0));
        i.push(LocalSet(10));
        i.push(Br(2)); // break C (0=If E, 1=Loop D, 2=Block C)
        i.push(End); // E
        // j++
        i.push(LocalGet(9));
        i.push(I64Const(1));
        i.push(I64Add);
        i.push(LocalSet(9));
        i.push(Br(0)); // continue D
        i.push(End); // D
        i.push(End); // C
        // if matched: push substring(s, start, i); i += len_sep; start = i
        i.push(LocalGet(10));
        i.push(I64Eqz);
        i.push(I32Eqz);
        i.push(If(BlockType::Empty)); // F
        i.push(LocalGet(6));
        i.push(LocalGet(0));
        i.push(LocalGet(7));
        i.push(LocalGet(8));
        i.push(Call(str_substring_idx));
        i.push(Call(vec_push_idx));
        i.push(LocalSet(6));
        i.push(LocalGet(8));
        i.push(LocalGet(5));
        i.push(I64Add);
        i.push(LocalSet(8));
        i.push(LocalGet(8));
        i.push(LocalSet(7));
        i.push(Else);
        // i++
        i.push(LocalGet(8));
        i.push(I64Const(1));
        i.push(I64Add);
        i.push(LocalSet(8));
        i.push(End); // F
        i.push(Br(0)); // continue B
        i.push(End); // B
        i.push(End); // A
        // push final segment substring(s, start, len_s)
        i.push(LocalGet(6));
        i.push(LocalGet(0));
        i.push(LocalGet(7));
        i.push(LocalGet(3));
        i.push(Call(str_substring_idx));
        i.push(Call(vec_push_idx));
        // (result left on stack)

        FunctionBody {
            params: vec![ValType::I64, ValType::I64],
            results: vec![ValType::I64],
            locals: vec![ValType::I64; 9], // 2..=10
            instructions: i,
        }
    }
}
