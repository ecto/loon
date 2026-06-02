//! Insertion-ordered maps for WASM codegen.
//!
//! A map is represented as a vector (see `collections.rs`) whose elements are
//! two-element `[key value]` vectors ("pairs"). Building on the vector runtime
//! means `len`/`count` (header offset 0) and `entries` (the vector itself)
//! come for free; only key lookup, insertion, and merge need new code.
//!
//! Key equality is structural for strings: two keys are equal if their raw i64
//! values match, or if both look like packed strings (value ≥ 2^32, since heap
//! pointers start at 1024) and compare equal byte-for-byte via `str_eq`.

use super::{FunctionBody, WasmInstruction::*};
use wasm_encoder::*;

/// Tracks the function indices of the map runtime helpers.
#[derive(Clone, Debug)]
pub struct MapsRuntime {
    pub val_eq_idx: u32,
    pub pair_idx: u32,
    pub map_get_idx: u32,
    pub map_assoc_idx: u32,
}

impl MapsRuntime {
    /// `val_eq(a, b) -> i64` (1 if equal, else 0). Raw equality, falling back to
    /// `str_eq` when both operands look like packed strings.
    pub(super) fn gen_val_eq(str_eq_idx: u32) -> FunctionBody {
        let mut i = Vec::new();
        i.push(LocalGet(0));
        i.push(LocalGet(1));
        i.push(I64Eq);
        i.push(If(BlockType::Result(ValType::I64)));
        i.push(I64Const(1));
        i.push(Else);
        // both >= 2^32 ?
        i.push(LocalGet(0));
        i.push(I64Const(0x1_0000_0000));
        i.push(I64GeS);
        i.push(If(BlockType::Result(ValType::I64)));
        i.push(LocalGet(1));
        i.push(I64Const(0x1_0000_0000));
        i.push(I64GeS);
        i.push(If(BlockType::Result(ValType::I64)));
        i.push(LocalGet(0));
        i.push(LocalGet(1));
        i.push(Call(str_eq_idx));
        i.push(Else);
        i.push(I64Const(0));
        i.push(End);
        i.push(Else);
        i.push(I64Const(0));
        i.push(End);
        i.push(End);
        FunctionBody {
            params: vec![ValType::I64, ValType::I64],
            results: vec![ValType::I64],
            locals: vec![],
            instructions: i,
        }
    }

    /// `pair(k, v) -> i64` — a fresh two-element `[k v]` vector.
    pub(super) fn gen_pair(vec_new_idx: u32, vec_push_idx: u32) -> FunctionBody {
        let mut i = Vec::new();
        i.push(Call(vec_new_idx));
        i.push(LocalGet(0));
        i.push(Call(vec_push_idx));
        i.push(LocalGet(1));
        i.push(Call(vec_push_idx));
        FunctionBody {
            params: vec![ValType::I64, ValType::I64],
            results: vec![ValType::I64],
            locals: vec![],
            instructions: i,
        }
    }

    /// `map_get(m, k) -> i64` — value for key `k`, or 0 if absent.
    pub(super) fn gen_map_get(vec_get_idx: u32, val_eq_idx: u32) -> FunctionBody {
        // locals: 2=len 3=i 4=p
        let mut i = Vec::new();
        i.push(LocalGet(0));
        i.push(I32WrapI64);
        i.push(I64Load(3, 0)); // len at header offset 0
        i.push(LocalSet(2));
        i.push(I64Const(0));
        i.push(LocalSet(3));
        i.push(Block(BlockType::Empty));
        i.push(Loop(BlockType::Empty));
        // if i >= len break
        i.push(LocalGet(3));
        i.push(LocalGet(2));
        i.push(I64LtS);
        i.push(I32Eqz);
        i.push(BrIf(1));
        // p = vec_get(m, i)
        i.push(LocalGet(0));
        i.push(LocalGet(3));
        i.push(Call(vec_get_idx));
        i.push(LocalSet(4));
        // if val_eq(vec_get(p,0), k) != 0: return vec_get(p,1)
        i.push(LocalGet(4));
        i.push(I64Const(0));
        i.push(Call(vec_get_idx));
        i.push(LocalGet(1));
        i.push(Call(val_eq_idx));
        i.push(I64Eqz);
        i.push(I32Eqz);
        i.push(If(BlockType::Empty));
        i.push(LocalGet(4));
        i.push(I64Const(1));
        i.push(Call(vec_get_idx));
        i.push(Return);
        i.push(End);
        // i++
        i.push(LocalGet(3));
        i.push(I64Const(1));
        i.push(I64Add);
        i.push(LocalSet(3));
        i.push(Br(0));
        i.push(End);
        i.push(End);
        i.push(I64Const(0));
        FunctionBody {
            params: vec![ValType::I64, ValType::I64],
            results: vec![ValType::I64],
            locals: vec![ValType::I64; 3], // 2,3,4
            instructions: i,
        }
    }

    /// `map_assoc(m, k, v) -> i64` — a new map with `k` bound to `v`, replacing
    /// any existing entry for `k` in place (preserving insertion order) or
    /// appending a new entry at the end.
    pub(super) fn gen_map_assoc(
        vec_new_idx: u32,
        vec_push_idx: u32,
        vec_get_idx: u32,
        val_eq_idx: u32,
        pair_idx: u32,
    ) -> FunctionBody {
        // locals: 3=len 4=i 5=new 6=found 7=p
        let mut i = Vec::new();
        i.push(LocalGet(0));
        i.push(I32WrapI64);
        i.push(I64Load(3, 0));
        i.push(LocalSet(3));
        i.push(Call(vec_new_idx));
        i.push(LocalSet(5));
        i.push(I64Const(0));
        i.push(LocalSet(6));
        i.push(I64Const(0));
        i.push(LocalSet(4));
        i.push(Block(BlockType::Empty));
        i.push(Loop(BlockType::Empty));
        i.push(LocalGet(4));
        i.push(LocalGet(3));
        i.push(I64LtS);
        i.push(I32Eqz);
        i.push(BrIf(1));
        // p = vec_get(m, i)
        i.push(LocalGet(0));
        i.push(LocalGet(4));
        i.push(Call(vec_get_idx));
        i.push(LocalSet(7));
        // if val_eq(key(p), k)
        i.push(LocalGet(7));
        i.push(I64Const(0));
        i.push(Call(vec_get_idx));
        i.push(LocalGet(1));
        i.push(Call(val_eq_idx));
        i.push(I64Eqz);
        i.push(I32Eqz);
        i.push(If(BlockType::Empty));
        // new = vec_push(new, pair(k, v)); found = 1
        i.push(LocalGet(5));
        i.push(LocalGet(1));
        i.push(LocalGet(2));
        i.push(Call(pair_idx));
        i.push(Call(vec_push_idx));
        i.push(LocalSet(5));
        i.push(I64Const(1));
        i.push(LocalSet(6));
        i.push(Else);
        // new = vec_push(new, p)
        i.push(LocalGet(5));
        i.push(LocalGet(7));
        i.push(Call(vec_push_idx));
        i.push(LocalSet(5));
        i.push(End);
        i.push(LocalGet(4));
        i.push(I64Const(1));
        i.push(I64Add);
        i.push(LocalSet(4));
        i.push(Br(0));
        i.push(End);
        i.push(End);
        // if !found: new = vec_push(new, pair(k, v))
        i.push(LocalGet(6));
        i.push(I64Eqz);
        i.push(If(BlockType::Empty));
        i.push(LocalGet(5));
        i.push(LocalGet(1));
        i.push(LocalGet(2));
        i.push(Call(pair_idx));
        i.push(Call(vec_push_idx));
        i.push(LocalSet(5));
        i.push(End);
        i.push(LocalGet(5));
        FunctionBody {
            params: vec![ValType::I64, ValType::I64, ValType::I64],
            results: vec![ValType::I64],
            locals: vec![ValType::I64; 5], // 3,4,5,6,7
            instructions: i,
        }
    }
}
