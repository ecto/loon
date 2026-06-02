//! Insertion-ordered maps for WASM codegen (simplified v0.5).
//!
//! A map is a heap structure mirroring the vector layout, but its data array
//! holds interleaved `key, value` i64 pairs:
//!
//!   offset 0:  len      (i64) — number of entries
//!   offset 8:  capacity (i64) — entry capacity
//!   offset 16: data_ptr (i64) — points to `capacity * 2` i64 slots
//!
//! Operations are copy-on-write for persistence, like vectors. Key comparison
//! is *self-describing* (see `emit_key_eq`): raw i64 equality first, then a
//! string-content compare when both keys look like string pointers (their
//! high-32 bits land in the heap range). This works for string keys even when
//! the static key type is unknown (e.g. a generic accumulator through `fold`).
//! The trailing `is_str` parameter on the helpers is now vestigial. Uses global
//! 0 as the bump allocator.

use super::{FunctionBody, WasmInstruction::*};
use wasm_encoder::*;

/// Function indices of the map runtime helpers.
#[derive(Clone, Debug)]
pub struct MapRuntime {
    pub map_new_idx: u32,
    pub map_set_idx: u32,
    pub map_get_idx: u32,
    pub map_has_idx: u32,
    pub map_keys_idx: u32,
}

impl MapRuntime {
    /// `map_new() -> i64` — empty map (capacity 4 entries).
    pub(super) fn gen_map_new() -> FunctionBody {
        // Locals: 0 = header, 1 = data
        let mut i = Vec::new();
        i.push(GlobalGet(0));
        i.push(I64ExtendI32U);
        i.push(LocalSet(0));
        i.push(GlobalGet(0));
        i.push(I32Const(24));
        i.push(I32Add);
        i.push(GlobalSet(0));
        // data: cap 4 entries -> 4*2 = 8 i64 = 64 bytes
        i.push(GlobalGet(0));
        i.push(I64ExtendI32U);
        i.push(LocalSet(1));
        i.push(GlobalGet(0));
        i.push(I32Const(64));
        i.push(I32Add);
        i.push(GlobalSet(0));
        // len = 0, cap = 4, data_ptr
        i.push(LocalGet(0));
        i.push(I32WrapI64);
        i.push(I64Const(0));
        i.push(I64Store(3, 0));
        i.push(LocalGet(0));
        i.push(I32WrapI64);
        i.push(I64Const(4));
        i.push(I64Store(3, 8));
        i.push(LocalGet(0));
        i.push(I32WrapI64);
        i.push(LocalGet(1));
        i.push(I64Store(3, 16));
        i.push(LocalGet(0));
        FunctionBody {
            params: vec![],
            results: vec![ValType::I64],
            locals: vec![ValType::I64, ValType::I64],
            instructions: i,
        }
    }

    /// Emit a *self-describing* key equality of locals `a` and `b`, leaving an
    /// i64 0/1. Fast path: raw `a == b` (covers ints, keywords, and identical
    /// string pointers). Otherwise, if both values look like string pointers —
    /// their packed high-32 bits fall in the heap range `[1024, heap_ptr)`,
    /// which ints (high bits 0) and keyword ids (high bits huge) never do —
    /// compare by content via `str_eq`. This removes the need for a compile-time
    /// key-type flag, so string keys work even through generic HOF-threaded code.
    fn emit_key_eq(i: &mut Vec<super::WasmInstruction>, a: u32, b: u32, str_eq: u32) {
        // strptr(x): (x>>32) >= 1024 && (x>>32) < heap_ptr  -> i32
        let push_strptr = |i: &mut Vec<super::WasmInstruction>, x: u32| {
            i.push(LocalGet(x));
            i.push(I64Const(32));
            i.push(I64ShrU);
            i.push(I64Const(1024));
            i.push(I64GeS);
            i.push(LocalGet(x));
            i.push(I64Const(32));
            i.push(I64ShrU);
            i.push(GlobalGet(0));
            i.push(I64ExtendI32U);
            i.push(I64LtS);
            i.push(I32And);
        };
        i.push(LocalGet(a));
        i.push(LocalGet(b));
        i.push(I64Eq);
        i.push(If(BlockType::Result(ValType::I64)));
        i.push(I64Const(1));
        i.push(Else);
        push_strptr(i, a);
        push_strptr(i, b);
        i.push(I32And);
        i.push(If(BlockType::Result(ValType::I64)));
        i.push(LocalGet(a));
        i.push(LocalGet(b));
        i.push(Call(str_eq));
        i.push(Else);
        i.push(I64Const(0));
        i.push(End);
        i.push(End);
    }

    /// `map_get(m, k, is_str) -> v` — value for `k`, or 0 (UNIT) if absent.
    pub(super) fn gen_map_get(str_eq: u32) -> FunctionBody {
        // Params: 0=m, 1=k, 2=is_str
        // Locals: 3=len, 4=data, 5=i, 6=ek
        let mut i = Vec::new();
        i.push(LocalGet(0));
        i.push(I32WrapI64);
        i.push(I64Load(3, 0));
        i.push(LocalSet(3));
        i.push(LocalGet(0));
        i.push(I32WrapI64);
        i.push(I64Load(3, 16));
        i.push(LocalSet(4));
        i.push(I64Const(0));
        i.push(LocalSet(5));
        i.push(Block(BlockType::Empty));
        i.push(Loop(BlockType::Empty));
        // if i >= len break
        i.push(LocalGet(5));
        i.push(LocalGet(3));
        i.push(I64LtS);
        i.push(I32Eqz);
        i.push(BrIf(1));
        // ek = data[i*16]
        i.push(LocalGet(4));
        i.push(LocalGet(5));
        i.push(I64Const(16));
        i.push(I64Mul);
        i.push(I64Add);
        i.push(I32WrapI64);
        i.push(I64Load(3, 0));
        i.push(LocalSet(6));
        // if key_eq(ek, k) return data[i*16+8]
        Self::emit_key_eq(&mut i, 6, 1, str_eq);
        i.push(I64Eqz);
        i.push(I32Eqz); // truthy?
        i.push(If(BlockType::Empty));
        i.push(LocalGet(4));
        i.push(LocalGet(5));
        i.push(I64Const(16));
        i.push(I64Mul);
        i.push(I64Add);
        i.push(I32WrapI64);
        i.push(I64Load(3, 8));
        i.push(Return);
        i.push(End);
        // i++
        i.push(LocalGet(5));
        i.push(I64Const(1));
        i.push(I64Add);
        i.push(LocalSet(5));
        i.push(Br(0));
        i.push(End);
        i.push(End);
        i.push(I64Const(0));
        FunctionBody {
            params: vec![ValType::I64, ValType::I64, ValType::I64],
            results: vec![ValType::I64],
            locals: vec![ValType::I64, ValType::I64, ValType::I64, ValType::I64],
            instructions: i,
        }
    }

    /// `map_has(m, k, is_str) -> i64` — 1 if `k` present, else 0.
    pub(super) fn gen_map_has(str_eq: u32) -> FunctionBody {
        // Params: 0=m, 1=k, 2=is_str ; Locals: 3=len,4=data,5=i,6=ek
        let mut i = Vec::new();
        i.push(LocalGet(0));
        i.push(I32WrapI64);
        i.push(I64Load(3, 0));
        i.push(LocalSet(3));
        i.push(LocalGet(0));
        i.push(I32WrapI64);
        i.push(I64Load(3, 16));
        i.push(LocalSet(4));
        i.push(I64Const(0));
        i.push(LocalSet(5));
        i.push(Block(BlockType::Empty));
        i.push(Loop(BlockType::Empty));
        i.push(LocalGet(5));
        i.push(LocalGet(3));
        i.push(I64LtS);
        i.push(I32Eqz);
        i.push(BrIf(1));
        i.push(LocalGet(4));
        i.push(LocalGet(5));
        i.push(I64Const(16));
        i.push(I64Mul);
        i.push(I64Add);
        i.push(I32WrapI64);
        i.push(I64Load(3, 0));
        i.push(LocalSet(6));
        Self::emit_key_eq(&mut i, 6, 1, str_eq);
        i.push(I64Eqz);
        i.push(I32Eqz);
        i.push(If(BlockType::Empty));
        i.push(I64Const(1));
        i.push(Return);
        i.push(End);
        i.push(LocalGet(5));
        i.push(I64Const(1));
        i.push(I64Add);
        i.push(LocalSet(5));
        i.push(Br(0));
        i.push(End);
        i.push(End);
        i.push(I64Const(0));
        FunctionBody {
            params: vec![ValType::I64, ValType::I64, ValType::I64],
            results: vec![ValType::I64],
            locals: vec![ValType::I64, ValType::I64, ValType::I64, ValType::I64],
            instructions: i,
        }
    }

    /// `map_set(m, k, v, is_str) -> i64` — copy-on-write assoc. Replaces the
    /// value if `k` exists (preserving order), else appends `k,v`.
    pub(super) fn gen_map_set(str_eq: u32) -> FunctionBody {
        // Params: 0=m,1=k,2=v,3=is_str
        // Locals: 4=len,5=old_data,6=found,7=new_len,8=new_hdr,9=new_data,
        //         10=i,11=ek
        let mut i = Vec::new();
        i.push(LocalGet(0));
        i.push(I32WrapI64);
        i.push(I64Load(3, 0));
        i.push(LocalSet(4)); // len
        i.push(LocalGet(0));
        i.push(I32WrapI64);
        i.push(I64Load(3, 16));
        i.push(LocalSet(5)); // old_data
        // found = 0
        i.push(I64Const(0));
        i.push(LocalSet(6));
        // new_len = len + 1 (capacity); actual len fixed up after the copy
        i.push(LocalGet(4));
        i.push(I64Const(1));
        i.push(I64Add);
        i.push(LocalSet(7));
        // alloc header (24)
        i.push(GlobalGet(0));
        i.push(I64ExtendI32U);
        i.push(LocalSet(8));
        i.push(GlobalGet(0));
        i.push(I32Const(24));
        i.push(I32Add);
        i.push(GlobalSet(0));
        // alloc data: new_len * 16 bytes
        i.push(GlobalGet(0));
        i.push(I64ExtendI32U);
        i.push(LocalSet(9));
        i.push(GlobalGet(0));
        i.push(LocalGet(7));
        i.push(I64Const(16));
        i.push(I64Mul);
        i.push(I32WrapI64);
        i.push(I32Add);
        i.push(GlobalSet(0));
        // copy entries: for i in 0..len
        i.push(I64Const(0));
        i.push(LocalSet(10));
        i.push(Block(BlockType::Empty));
        i.push(Loop(BlockType::Empty));
        i.push(LocalGet(10));
        i.push(LocalGet(4));
        i.push(I64LtS);
        i.push(I32Eqz);
        i.push(BrIf(1));
        // ek = old_data[i*16]
        i.push(LocalGet(5));
        i.push(LocalGet(10));
        i.push(I64Const(16));
        i.push(I64Mul);
        i.push(I64Add);
        i.push(I32WrapI64);
        i.push(I64Load(3, 0));
        i.push(LocalSet(11));
        // new_data[i*16] = ek
        i.push(LocalGet(9));
        i.push(LocalGet(10));
        i.push(I64Const(16));
        i.push(I64Mul);
        i.push(I64Add);
        i.push(I32WrapI64);
        i.push(LocalGet(11));
        i.push(I64Store(3, 0));
        // value: if key_eq(ek,k) { v ; found=1 } else { old value }
        Self::emit_key_eq(&mut i, 11, 1, str_eq);
        i.push(I64Eqz);
        i.push(I32Eqz);
        i.push(If(BlockType::Result(ValType::I64)));
        i.push(I64Const(1));
        i.push(LocalSet(6)); // found = 1
        i.push(LocalGet(2)); // v
        i.push(Else);
        i.push(LocalGet(5));
        i.push(LocalGet(10));
        i.push(I64Const(16));
        i.push(I64Mul);
        i.push(I64Add);
        i.push(I32WrapI64);
        i.push(I64Load(3, 8));
        i.push(End);
        // store the chosen value at new_data[i*16+8]
        i.push(LocalSet(11)); // reuse ek slot as scratch for the value
        i.push(LocalGet(9));
        i.push(LocalGet(10));
        i.push(I64Const(16));
        i.push(I64Mul);
        i.push(I64Add);
        i.push(I32WrapI64);
        i.push(LocalGet(11));
        i.push(I64Store(3, 8));
        // i++
        i.push(LocalGet(10));
        i.push(I64Const(1));
        i.push(I64Add);
        i.push(LocalSet(10));
        i.push(Br(0));
        i.push(End);
        i.push(End);
        // if !found: append k,v at slot `len`; final len = len+1; else len
        i.push(LocalGet(6));
        i.push(I64Eqz); // 1 if not found
        i.push(If(BlockType::Empty));
        // new_data[len*16] = k
        i.push(LocalGet(9));
        i.push(LocalGet(4));
        i.push(I64Const(16));
        i.push(I64Mul);
        i.push(I64Add);
        i.push(I32WrapI64);
        i.push(LocalGet(1));
        i.push(I64Store(3, 0));
        // new_data[len*16+8] = v
        i.push(LocalGet(9));
        i.push(LocalGet(4));
        i.push(I64Const(16));
        i.push(I64Mul);
        i.push(I64Add);
        i.push(I32WrapI64);
        i.push(LocalGet(2));
        i.push(I64Store(3, 8));
        i.push(End);
        // header: len (= found ? len : len+1), cap = new_len, data
        i.push(LocalGet(8));
        i.push(I32WrapI64);
        i.push(LocalGet(6));
        i.push(I64Eqz);
        i.push(I32Eqz); // 1 if found
        i.push(If(BlockType::Result(ValType::I64)));
        i.push(LocalGet(4)); // found: len unchanged
        i.push(Else);
        i.push(LocalGet(7)); // not found: len+1
        i.push(End);
        i.push(I64Store(3, 0));
        i.push(LocalGet(8));
        i.push(I32WrapI64);
        i.push(LocalGet(7));
        i.push(I64Store(3, 8)); // cap
        i.push(LocalGet(8));
        i.push(I32WrapI64);
        i.push(LocalGet(9));
        i.push(I64Store(3, 16)); // data
        i.push(LocalGet(8));
        FunctionBody {
            params: vec![ValType::I64, ValType::I64, ValType::I64, ValType::I64],
            results: vec![ValType::I64],
            locals: vec![ValType::I64; 8], // locals 4..=11
            instructions: i,
        }
    }

    /// `map_keys(m, vec_new, vec_push) -> i64` — a vector of the keys in order.
    pub(super) fn gen_map_keys(vec_new: u32, vec_push: u32) -> FunctionBody {
        // Params: 0=m ; Locals: 1=len,2=data,3=i,4=r
        let mut i = Vec::new();
        i.push(LocalGet(0));
        i.push(I32WrapI64);
        i.push(I64Load(3, 0));
        i.push(LocalSet(1));
        i.push(LocalGet(0));
        i.push(I32WrapI64);
        i.push(I64Load(3, 16));
        i.push(LocalSet(2));
        i.push(Call(vec_new));
        i.push(LocalSet(4));
        i.push(I64Const(0));
        i.push(LocalSet(3));
        i.push(Block(BlockType::Empty));
        i.push(Loop(BlockType::Empty));
        i.push(LocalGet(3));
        i.push(LocalGet(1));
        i.push(I64LtS);
        i.push(I32Eqz);
        i.push(BrIf(1));
        // r = vec_push(r, data[i*16])
        i.push(LocalGet(4));
        i.push(LocalGet(2));
        i.push(LocalGet(3));
        i.push(I64Const(16));
        i.push(I64Mul);
        i.push(I64Add);
        i.push(I32WrapI64);
        i.push(I64Load(3, 0));
        i.push(Call(vec_push));
        i.push(LocalSet(4));
        i.push(LocalGet(3));
        i.push(I64Const(1));
        i.push(I64Add);
        i.push(LocalSet(3));
        i.push(Br(0));
        i.push(End);
        i.push(End);
        i.push(LocalGet(4));
        FunctionBody {
            params: vec![ValType::I64],
            results: vec![ValType::I64],
            locals: vec![ValType::I64, ValType::I64, ValType::I64, ValType::I64],
            instructions: i,
        }
    }
}
