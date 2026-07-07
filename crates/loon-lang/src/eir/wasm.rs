//! WASM backend — compiles Evidence IR to WebAssembly binary.
//!
//! Targets the NaN-boxed `i64` value representation. Each EIR function
//! becomes a WASM function that takes and returns `i64` values. Control
//! flow uses the block/loop/br_table dispatch pattern to map EIR's
//! arbitrary block graph onto WASM's structured control flow.
//!
//! Simplifications for v1:
//! - Bump allocator (no GC)
//! - No closure captures (function references only)
//! - Collections stored as simple arrays in linear memory
//! - IO effects imported from host

use super::*;
use crate::eir::backend::{Backend, Error};
use wasm_encoder::{
    BlockType, CodeSection, ConstExpr, DataSection, ElementSection, Elements, EntityType,
    ExportKind, ExportSection, Function, FunctionSection, GlobalSection, GlobalType, ImportSection,
    Instruction, MemArg, MemorySection, MemoryType, Module as WasmModule, RefType, TableSection,
    TableType, TypeSection, ValType,
};

// ─── NaN-boxing constants (must match value64.rs) ─────────────────────────

const QNAN: u64 = 0x7FF8_0000_0000_0000;
const SIGN: u64 = 0x8000_0000_0000_0000;
const BASE: u64 = SIGN | QNAN;
#[allow(dead_code)]
const TAG_MASK: u64 = 0x0007_0000_0000_0000;
const PAYLOAD: u64 = 0x0000_FFFF_FFFF_FFFF;
const TAG_INT: u64 = 0x0001_0000_0000_0000;
const TAG_PTR: u64 = 0x0000_0000_0000_0000;
const TAG_IMM: u64 = 0x0007_0000_0000_0000;
const TAG_SYM: u64 = 0x0006_0000_0000_0000;
#[allow(dead_code)]
const IMM_UNIT: u64 = 0;
#[allow(dead_code)]
const IMM_TRUE: u64 = 1;
#[allow(dead_code)]
const IMM_FALSE: u64 = 2;

const VAL_UNIT: u64 = BASE | TAG_IMM;
const VAL_TRUE: u64 = BASE | TAG_IMM | 1;
const VAL_FALSE: u64 = BASE | TAG_IMM | 2;

// ─── Import indices ───────────────────────────────────────────────────────

/// Number of host imports (println, print).
const HOST_IMPORT_COUNT: u32 = 2;

/// Import function indices.
const IMPORT_PRINTLN: u32 = 0;
const IMPORT_PRINT: u32 = 1;

// ─── Global indices ───────────────────────────────────────────────────────

/// Bump-allocator heap pointer (global 0, type i32).
const GLOBAL_HEAP_PTR: u32 = 0;

/// Initial heap offset (after data segment; 64 KiB reserved).
const INITIAL_HEAP_OFFSET: u32 = 65536;

// ─── Backend ──────────────────────────────────────────────────────────────

pub struct WasmBackend;

impl Backend for WasmBackend {
    type Output = Vec<u8>;

    fn compile(&mut self, module: &super::Module) -> Result<Vec<u8>, Error> {
        let mut ctx = CompileCtx::new(module);
        ctx.compile_module()?;
        Ok(ctx.finish())
    }

    fn name(&self) -> &'static str {
        "wasm"
    }
}

// ─── Compilation context ──────────────────────────────────────────────────

struct CompileCtx<'a> {
    module: &'a super::Module,
    /// Compiled WASM function bodies (one per EIR func).
    functions: Vec<WasmFunc>,
    /// Function table entries for indirect calls.
    table_entries: Vec<u32>,
    /// String data: (offset_in_memory, raw_bytes).
    string_data: Vec<(u32, Vec<u8>)>,
    /// Next free offset in the data segment.
    data_offset: u32,
}

struct WasmFunc {
    /// Number of parameters (all i64).
    param_count: u32,
    /// Number of WASM locals beyond params (all i64).
    local_count: u32,
    /// Encoded WASM instructions.
    body: Vec<WasmInstr>,
}

/// Minimal instruction enum to avoid lifetime issues with wasm_encoder.
#[derive(Clone, Debug)]
#[allow(dead_code)]
enum WasmInstr {
    I64Const(i64),
    F64Const(f64),
    I32Const(i32),
    LocalGet(u32),
    LocalSet(u32),
    LocalTee(u32),
    GlobalGet(u32),
    GlobalSet(u32),
    Call(u32),
    CallIndirect(u32),
    Return,
    Drop,
    Unreachable,
    // Integer arithmetic
    I64Add,
    I64Sub,
    I64Mul,
    I64DivS,
    I64RemS,
    I64And,
    I64Or,
    I64Xor,
    I64Shl,
    I64ShrS,
    I64ShrU,
    I64Eq,
    I64Ne,
    I64LtS,
    I64GtS,
    I64LeS,
    I64GeS,
    I64Eqz,
    // Float arithmetic
    F64Add,
    F64Sub,
    F64Mul,
    F64Div,
    F64Neg,
    F64Eq,
    F64Ne,
    F64Lt,
    F64Gt,
    F64Le,
    F64Ge,
    F64Abs,
    // i32 arithmetic
    I32Add,
    I32And,
    // Conversions
    I32WrapI64,
    I64ExtendI32U,
    I64ExtendI32S,
    F64ReinterpretI64,
    I64ReinterpretF64,
    // Memory (align, offset)
    I64Store(u32, u32),
    I64Load(u32, u32),
    I32Store(u32, u32),
    I32Load(u32, u32),
    // Control flow
    Block(BlockType),
    Loop(BlockType),
    Br(u32),
    BrIf(u32),
    BrTable(Vec<u32>, u32),
    End,
    If(BlockType),
    Else,
    Select,
}

impl<'a> CompileCtx<'a> {
    fn new(module: &'a super::Module) -> Self {
        Self {
            module,
            functions: Vec::new(),
            table_entries: Vec::new(),
            string_data: Vec::new(),
            data_offset: 1024,
        }
    }

    fn compile_module(&mut self) -> Result<(), Error> {
        self.layout_strings();

        for func_idx in 0..self.module.funcs.len() {
            let wasm_func = self.compile_func(func_idx)?;
            self.functions.push(wasm_func);
        }

        // Populate function table for indirect calls.
        for i in 0..self.module.funcs.len() {
            self.table_entries.push(HOST_IMPORT_COUNT + i as u32);
        }

        Ok(())
    }

    /// Assign each string constant an offset in linear memory.
    /// Layout: [4-byte LE length][UTF-8 bytes], aligned to 4 bytes.
    fn layout_strings(&mut self) {
        for s in &self.module.strings {
            let bytes = s.as_bytes().to_vec();
            let len = bytes.len() as u32;
            let offset = self.data_offset;
            self.string_data.push((offset, bytes));
            self.data_offset += align4(4 + len);
        }
    }

    /// Compute how many registers a function uses (max Reg index + 1).
    fn reg_count(func: &Func) -> u32 {
        func.blocks
            .iter()
            .flat_map(|b| {
                b.ops
                    .iter()
                    .map(|op| op.dst().0 + 1)
                    .chain(b.params.iter().map(|r| r.0 + 1))
            })
            .max()
            .unwrap_or(0)
    }

    fn compile_func(&self, func_idx: usize) -> Result<WasmFunc, Error> {
        let func = &self.module.funcs[func_idx];
        let param_count = func.params.len() as u32;
        let total_regs = Self::reg_count(func).max(param_count);

        // Locals beyond params: extra registers + __block index + __addr scratch
        let extra_regs = total_regs.saturating_sub(param_count);
        let block_local = total_regs; // __block dispatch index (i64)
        let addr_local = total_regs + 1; // __addr scratch for heap ops (i64)
        let local_count = extra_regs + 2; // +1 __block, +1 __addr

        let num_blocks = func.blocks.len();
        let mut body = Vec::new();

        if num_blocks == 0 {
            body.push(WasmInstr::I64Const(VAL_UNIT as i64));
            body.push(WasmInstr::Return);
            return Ok(WasmFunc {
                param_count,
                local_count,
                body,
            });
        }

        if num_blocks == 1 {
            self.emit_block_ops(func, 0, addr_local, &mut body)?;
            self.emit_block_end_simple(func, 0, &mut body)?;
            return Ok(WasmFunc {
                param_count,
                local_count,
                body,
            });
        }

        // Multi-block: block/loop/br_table dispatch.
        //
        // (block $exit
        //   (loop $dispatch
        //     (block $b0 (block $b1 ... (block $bN
        //       (br_table $b0 $b1 ... $bN (i32.wrap_i64 (local.get $__block)))
        //     ) ;; end $bN — code for block N-1 here
        //     ...
        //     ) ;; end $b1 — code for block 1 here
        //     ) ;; end $b0 — code for block 0 here
        //   ) ;; end $dispatch
        // ) ;; end $exit

        body.push(WasmInstr::Block(BlockType::Empty)); // $exit
        body.push(WasmInstr::Loop(BlockType::Empty)); // $dispatch

        // Open N nested blocks
        for _ in 0..num_blocks {
            body.push(WasmInstr::Block(BlockType::Empty));
        }

        // br_table dispatch
        let labels: Vec<u32> = (0..num_blocks as u32).collect();
        body.push(WasmInstr::LocalGet(block_local));
        body.push(WasmInstr::I32WrapI64);
        body.push(WasmInstr::BrTable(labels, (num_blocks - 1) as u32));

        // Emit blocks in reverse order (innermost first)
        for block_idx in (0..num_blocks).rev() {
            body.push(WasmInstr::End); // close $bX
            self.emit_block_ops(func, block_idx, addr_local, &mut body)?;

            // Depth from current position:
            // - remaining open blocks above us: block_idx
            // - then $dispatch loop (1 more)
            // - then $exit block (1 more)
            let dispatch_depth = block_idx as u32 + 1;
            let exit_depth = block_idx as u32 + 2;
            self.emit_block_end_dispatch(
                func,
                block_idx,
                block_local,
                dispatch_depth,
                exit_depth,
                &mut body,
            )?;
        }

        body.push(WasmInstr::End); // $dispatch loop
        body.push(WasmInstr::End); // $exit block
                                   // Fallthrough: should not be reached
        body.push(WasmInstr::I64Const(VAL_UNIT as i64));

        Ok(WasmFunc {
            param_count,
            local_count,
            body,
        })
    }

    /// Emit WASM instructions for every operation in a block.
    fn emit_block_ops(
        &self,
        func: &Func,
        block_idx: usize,
        addr_local: u32,
        out: &mut Vec<WasmInstr>,
    ) -> Result<(), Error> {
        let block = &func.blocks[block_idx];
        for op in &block.ops {
            self.emit_op(func, op, addr_local, out)?;
        }
        Ok(())
    }

    /// Emit a single EIR operation.
    fn emit_op(
        &self,
        _func: &Func,
        op: &Op,
        addr_local: u32,
        out: &mut Vec<WasmInstr>,
    ) -> Result<(), Error> {
        match op {
            Op::Lit(dst, lit, _) => {
                out.push(WasmInstr::I64Const(self.lit_to_i64(lit)));
                out.push(WasmInstr::LocalSet(dst.0));
            }

            Op::Mov(dst, src, _) => {
                out.push(WasmInstr::LocalGet(src.0));
                out.push(WasmInstr::LocalSet(dst.0));
            }

            Op::Bin(dst, binop, lhs, rhs, _) => {
                self.emit_binop(*dst, *binop, *lhs, *rhs, out);
            }

            Op::Un(dst, unop, src, _) => {
                self.emit_unop(*dst, *unop, *src, out);
            }

            Op::Call(dst, fid, args, _) => {
                for arg in args {
                    out.push(WasmInstr::LocalGet(arg.0));
                }
                out.push(WasmInstr::Call(HOST_IMPORT_COUNT + fid.0));
                out.push(WasmInstr::LocalSet(dst.0));
            }

            Op::Invoke(dst, callee, args, _) => {
                for arg in args {
                    out.push(WasmInstr::LocalGet(arg.0));
                }
                // Extract table index from NaN-boxed int
                out.push(WasmInstr::LocalGet(callee.0));
                emit_unbox_int(out);
                out.push(WasmInstr::I32WrapI64);
                // Type index placeholder = arity (resolved in finish)
                out.push(WasmInstr::CallIndirect(args.len() as u32));
                out.push(WasmInstr::LocalSet(dst.0));
            }

            Op::Close(dst, fid, captures, _) => {
                if captures.is_empty() {
                    // No captures — store table index as NaN-boxed int
                    emit_box_int(fid.0 as i64, out);
                    out.push(WasmInstr::LocalSet(dst.0));
                } else {
                    // Allocate closure on heap: [func_id: i64][n_caps: i64][cap0..N: i64]
                    let size = (2 + captures.len() as u32) * 8;
                    emit_heap_alloc(size, addr_local, out);
                    // Store func_id at offset 0
                    emit_addr_as_i32(addr_local, out);
                    out.push(WasmInstr::I64Const(fid.0 as i64));
                    out.push(WasmInstr::I64Store(3, 0));
                    // Store capture count at offset 8
                    emit_addr_as_i32(addr_local, out);
                    out.push(WasmInstr::I64Const(captures.len() as i64));
                    out.push(WasmInstr::I64Store(3, 8));
                    // Store captures
                    for (i, cap) in captures.iter().enumerate() {
                        emit_addr_as_i32(addr_local, out);
                        out.push(WasmInstr::LocalGet(cap.0));
                        out.push(WasmInstr::I64Store(3, (16 + i * 8) as u32));
                    }
                    // Box as NaN-boxed pointer
                    emit_addr_as_boxed_ptr(addr_local, out);
                    out.push(WasmInstr::LocalSet(dst.0));
                }
            }

            Op::Upval(dst, _index, _) => {
                // V1 stub: closure upvalues not supported yet
                out.push(WasmInstr::I64Const(VAL_UNIT as i64));
                out.push(WasmInstr::LocalSet(dst.0));
            }

            Op::Vec(dst, elems, _) => {
                // Layout: [len: i32][elem0: i64]...
                let size = 4 + elems.len() as u32 * 8;
                emit_heap_alloc(size, addr_local, out);
                // Store length
                emit_addr_as_i32(addr_local, out);
                out.push(WasmInstr::I32Const(elems.len() as i32));
                out.push(WasmInstr::I32Store(2, 0));
                // Store elements
                for (i, elem) in elems.iter().enumerate() {
                    emit_addr_as_i32(addr_local, out);
                    out.push(WasmInstr::LocalGet(elem.0));
                    out.push(WasmInstr::I64Store(3, (4 + i * 8) as u32));
                }
                // Box as pointer
                emit_addr_as_boxed_ptr(addr_local, out);
                out.push(WasmInstr::LocalSet(dst.0));
            }

            Op::Map(dst, pairs, _) => {
                // Layout: [count: i32][k0: i64][v0: i64]...
                let size = 4 + pairs.len() as u32 * 16;
                emit_heap_alloc(size, addr_local, out);
                emit_addr_as_i32(addr_local, out);
                out.push(WasmInstr::I32Const(pairs.len() as i32));
                out.push(WasmInstr::I32Store(2, 0));
                for (i, (k, v)) in pairs.iter().enumerate() {
                    emit_addr_as_i32(addr_local, out);
                    out.push(WasmInstr::LocalGet(k.0));
                    out.push(WasmInstr::I64Store(3, (4 + i * 16) as u32));
                    emit_addr_as_i32(addr_local, out);
                    out.push(WasmInstr::LocalGet(v.0));
                    out.push(WasmInstr::I64Store(3, (4 + i * 16 + 8) as u32));
                }
                emit_addr_as_boxed_ptr(addr_local, out);
                out.push(WasmInstr::LocalSet(dst.0));
            }

            Op::Set(dst, elems, _) => {
                let size = 4 + elems.len() as u32 * 8;
                emit_heap_alloc(size, addr_local, out);
                emit_addr_as_i32(addr_local, out);
                out.push(WasmInstr::I32Const(elems.len() as i32));
                out.push(WasmInstr::I32Store(2, 0));
                for (i, elem) in elems.iter().enumerate() {
                    emit_addr_as_i32(addr_local, out);
                    out.push(WasmInstr::LocalGet(elem.0));
                    out.push(WasmInstr::I64Store(3, (4 + i * 8) as u32));
                }
                emit_addr_as_boxed_ptr(addr_local, out);
                out.push(WasmInstr::LocalSet(dst.0));
            }

            Op::Tup(dst, elems, _) => {
                let size = 4 + elems.len() as u32 * 8;
                emit_heap_alloc(size, addr_local, out);
                emit_addr_as_i32(addr_local, out);
                out.push(WasmInstr::I32Const(elems.len() as i32));
                out.push(WasmInstr::I32Store(2, 0));
                for (i, elem) in elems.iter().enumerate() {
                    emit_addr_as_i32(addr_local, out);
                    out.push(WasmInstr::LocalGet(elem.0));
                    out.push(WasmInstr::I64Store(3, (4 + i * 8) as u32));
                }
                emit_addr_as_boxed_ptr(addr_local, out);
                out.push(WasmInstr::LocalSet(dst.0));
            }

            Op::Adt(dst, tag, fields, _) => {
                // Layout: [tag: u16 | field_count: u16 packed as i32][fields...]
                let size = 4 + fields.len() as u32 * 8;
                emit_heap_alloc(size, addr_local, out);
                let header = (*tag as i32) | ((fields.len() as i32) << 16);
                emit_addr_as_i32(addr_local, out);
                out.push(WasmInstr::I32Const(header));
                out.push(WasmInstr::I32Store(2, 0));
                for (i, field) in fields.iter().enumerate() {
                    emit_addr_as_i32(addr_local, out);
                    out.push(WasmInstr::LocalGet(field.0));
                    out.push(WasmInstr::I64Store(3, (4 + i * 8) as u32));
                }
                emit_addr_as_boxed_ptr(addr_local, out);
                out.push(WasmInstr::LocalSet(dst.0));
            }

            Op::Field(dst, src, selector, _) => {
                match selector {
                    Selector::Index(idx) => {
                        // Unbox ptr, load field at offset 4 + idx*8
                        out.push(WasmInstr::LocalGet(src.0));
                        emit_unbox_ptr(out);
                        out.push(WasmInstr::I64Load(3, 4 + *idx as u32 * 8));
                        out.push(WasmInstr::LocalSet(dst.0));
                    }
                    Selector::Key(_) | Selector::Name(_) => {
                        // V1 stub
                        out.push(WasmInstr::I64Const(VAL_UNIT as i64));
                        out.push(WasmInstr::LocalSet(dst.0));
                    }
                }
            }

            Op::Tag(dst, src, _) => {
                // Extract ADT tag: unbox ptr → load header i32 → mask low 16 bits
                out.push(WasmInstr::LocalGet(src.0));
                emit_unbox_ptr(out);
                out.push(WasmInstr::I32Load(2, 0));
                out.push(WasmInstr::I32Const(0xFFFF));
                out.push(WasmInstr::I32And);
                // Store raw i32 tag extended to i64 (used by Switch for comparison)
                out.push(WasmInstr::I64ExtendI32U);
                out.push(WasmInstr::LocalSet(dst.0));
            }

            Op::Builtin(dst, builtin, args, _) => {
                self.emit_builtin(*dst, *builtin, args, out)?;
            }

            Op::Perform(dst, _, _, _, _, _) => {
                // V1 stub
                out.push(WasmInstr::I64Const(VAL_UNIT as i64));
                out.push(WasmInstr::LocalSet(dst.0));
            }

            Op::PushHandler(dst, _, _, _) => {
                out.push(WasmInstr::I64Const(VAL_UNIT as i64));
                out.push(WasmInstr::LocalSet(dst.0));
            }

            Op::PopHandler(_) => {}
        }
        Ok(())
    }

    /// Emit binary operation.
    fn emit_binop(&self, dst: Reg, op: BinOp, lhs: Reg, rhs: Reg, out: &mut Vec<WasmInstr>) {
        match op {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Rem => {
                out.push(WasmInstr::LocalGet(lhs.0));
                emit_unbox_int(out);
                out.push(WasmInstr::LocalGet(rhs.0));
                emit_unbox_int(out);
                match op {
                    BinOp::Add => out.push(WasmInstr::I64Add),
                    BinOp::Sub => out.push(WasmInstr::I64Sub),
                    BinOp::Mul => out.push(WasmInstr::I64Mul),
                    BinOp::Div => out.push(WasmInstr::I64DivS),
                    BinOp::Rem => out.push(WasmInstr::I64RemS),
                    _ => unreachable!(),
                }
                emit_rebox_int(out);
                out.push(WasmInstr::LocalSet(dst.0));
            }

            BinOp::Eq => {
                // Bitwise equality of NaN-boxed values
                out.push(WasmInstr::LocalGet(lhs.0));
                out.push(WasmInstr::LocalGet(rhs.0));
                out.push(WasmInstr::I64Eq);
                emit_bool_to_val(out);
                out.push(WasmInstr::LocalSet(dst.0));
            }
            BinOp::Ne => {
                out.push(WasmInstr::LocalGet(lhs.0));
                out.push(WasmInstr::LocalGet(rhs.0));
                out.push(WasmInstr::I64Ne);
                emit_bool_to_val(out);
                out.push(WasmInstr::LocalSet(dst.0));
            }

            BinOp::Lt | BinOp::Gt | BinOp::Le | BinOp::Ge => {
                out.push(WasmInstr::LocalGet(lhs.0));
                emit_unbox_int(out);
                out.push(WasmInstr::LocalGet(rhs.0));
                emit_unbox_int(out);
                match op {
                    BinOp::Lt => out.push(WasmInstr::I64LtS),
                    BinOp::Gt => out.push(WasmInstr::I64GtS),
                    BinOp::Le => out.push(WasmInstr::I64LeS),
                    BinOp::Ge => out.push(WasmInstr::I64GeS),
                    _ => unreachable!(),
                }
                emit_bool_to_val(out);
                out.push(WasmInstr::LocalSet(dst.0));
            }

            BinOp::And => {
                // Eager: truthy(lhs) ? rhs : lhs
                out.push(WasmInstr::LocalGet(lhs.0));
                emit_is_truthy(out);
                out.push(WasmInstr::If(BlockType::Result(ValType::I64)));
                out.push(WasmInstr::LocalGet(rhs.0));
                out.push(WasmInstr::Else);
                out.push(WasmInstr::LocalGet(lhs.0));
                out.push(WasmInstr::End);
                out.push(WasmInstr::LocalSet(dst.0));
            }
            BinOp::Or => {
                // Eager: truthy(lhs) ? lhs : rhs
                out.push(WasmInstr::LocalGet(lhs.0));
                emit_is_truthy(out);
                out.push(WasmInstr::If(BlockType::Result(ValType::I64)));
                out.push(WasmInstr::LocalGet(lhs.0));
                out.push(WasmInstr::Else);
                out.push(WasmInstr::LocalGet(rhs.0));
                out.push(WasmInstr::End);
                out.push(WasmInstr::LocalSet(dst.0));
            }

            BinOp::Concat => {
                // V1 stub: string concat — pass through lhs
                out.push(WasmInstr::LocalGet(lhs.0));
                out.push(WasmInstr::LocalSet(dst.0));
            }
        }
    }

    /// Emit unary operation.
    fn emit_unop(&self, dst: Reg, op: UnOp, src: Reg, out: &mut Vec<WasmInstr>) {
        match op {
            UnOp::Neg => {
                out.push(WasmInstr::LocalGet(src.0));
                emit_unbox_int(out);
                out.push(WasmInstr::I64Const(-1));
                out.push(WasmInstr::I64Mul);
                emit_rebox_int(out);
                out.push(WasmInstr::LocalSet(dst.0));
            }
            UnOp::Not => {
                out.push(WasmInstr::LocalGet(src.0));
                emit_is_truthy(out);
                out.push(WasmInstr::If(BlockType::Result(ValType::I64)));
                out.push(WasmInstr::I64Const(VAL_FALSE as i64));
                out.push(WasmInstr::Else);
                out.push(WasmInstr::I64Const(VAL_TRUE as i64));
                out.push(WasmInstr::End);
                out.push(WasmInstr::LocalSet(dst.0));
            }
        }
    }

    /// Emit built-in operation.
    fn emit_builtin(
        &self,
        dst: Reg,
        builtin: Built,
        args: &[Reg],
        out: &mut Vec<WasmInstr>,
    ) -> Result<(), Error> {
        match builtin {
            Built::Println => {
                if let Some(arg) = args.first() {
                    out.push(WasmInstr::LocalGet(arg.0));
                } else {
                    out.push(WasmInstr::I64Const(VAL_UNIT as i64));
                }
                out.push(WasmInstr::Call(IMPORT_PRINTLN));
                out.push(WasmInstr::I64Const(VAL_UNIT as i64));
                out.push(WasmInstr::LocalSet(dst.0));
            }
            Built::Print => {
                if let Some(arg) = args.first() {
                    out.push(WasmInstr::LocalGet(arg.0));
                } else {
                    out.push(WasmInstr::I64Const(VAL_UNIT as i64));
                }
                out.push(WasmInstr::Call(IMPORT_PRINT));
                out.push(WasmInstr::I64Const(VAL_UNIT as i64));
                out.push(WasmInstr::LocalSet(dst.0));
            }
            Built::Len => {
                if let Some(arg) = args.first() {
                    out.push(WasmInstr::LocalGet(arg.0));
                    emit_unbox_ptr(out);
                    out.push(WasmInstr::I32Load(2, 0));
                    out.push(WasmInstr::I64ExtendI32U);
                    emit_rebox_int(out);
                } else {
                    emit_box_int(0, out);
                }
                out.push(WasmInstr::LocalSet(dst.0));
            }
            Built::Not => {
                if let Some(arg) = args.first() {
                    out.push(WasmInstr::LocalGet(arg.0));
                    emit_is_truthy(out);
                    out.push(WasmInstr::If(BlockType::Result(ValType::I64)));
                    out.push(WasmInstr::I64Const(VAL_FALSE as i64));
                    out.push(WasmInstr::Else);
                    out.push(WasmInstr::I64Const(VAL_TRUE as i64));
                    out.push(WasmInstr::End);
                } else {
                    out.push(WasmInstr::I64Const(VAL_TRUE as i64));
                }
                out.push(WasmInstr::LocalSet(dst.0));
            }
            Built::First => {
                if let Some(arg) = args.first() {
                    out.push(WasmInstr::LocalGet(arg.0));
                    emit_unbox_ptr(out);
                    out.push(WasmInstr::I64Load(3, 4));
                } else {
                    out.push(WasmInstr::I64Const(VAL_UNIT as i64));
                }
                out.push(WasmInstr::LocalSet(dst.0));
            }
            Built::Str | Built::Int | Built::Float => {
                // Identity stubs
                if let Some(arg) = args.first() {
                    out.push(WasmInstr::LocalGet(arg.0));
                } else {
                    out.push(WasmInstr::I64Const(VAL_UNIT as i64));
                }
                out.push(WasmInstr::LocalSet(dst.0));
            }
            Built::Unit => {
                out.push(WasmInstr::I64Const(VAL_UNIT as i64));
                out.push(WasmInstr::LocalSet(dst.0));
            }
            // All other builtins: return unit for v1
            _ => {
                out.push(WasmInstr::I64Const(VAL_UNIT as i64));
                out.push(WasmInstr::LocalSet(dst.0));
            }
        }
        Ok(())
    }

    /// Emit block terminator for single-block functions.
    fn emit_block_end_simple(
        &self,
        func: &Func,
        block_idx: usize,
        out: &mut Vec<WasmInstr>,
    ) -> Result<(), Error> {
        let block = &func.blocks[block_idx];
        match &block.end {
            End::Ret(reg) => {
                out.push(WasmInstr::LocalGet(reg.0));
                out.push(WasmInstr::Return);
            }
            End::Trap => {
                out.push(WasmInstr::Unreachable);
            }
            _ => {
                out.push(WasmInstr::I64Const(VAL_UNIT as i64));
                out.push(WasmInstr::Return);
            }
        }
        Ok(())
    }

    /// Emit block terminator for the multi-block dispatch pattern.
    fn emit_block_end_dispatch(
        &self,
        func: &Func,
        block_idx: usize,
        block_local: u32,
        dispatch_depth: u32,
        _exit_depth: u32,
        out: &mut Vec<WasmInstr>,
    ) -> Result<(), Error> {
        let block = &func.blocks[block_idx];
        match &block.end {
            End::Ret(reg) => {
                out.push(WasmInstr::LocalGet(reg.0));
                out.push(WasmInstr::Return);
            }

            End::Jmp(target, args) => {
                // Copy block arguments into target's params
                let target_block = &func.blocks[target.0 as usize];
                for (i, arg) in args.iter().enumerate() {
                    if i < target_block.params.len() {
                        let param_reg = target_block.params[i];
                        if param_reg != *arg {
                            out.push(WasmInstr::LocalGet(arg.0));
                            out.push(WasmInstr::LocalSet(param_reg.0));
                        }
                    }
                }
                out.push(WasmInstr::I64Const(target.0 as i64));
                out.push(WasmInstr::LocalSet(block_local));
                out.push(WasmInstr::Br(dispatch_depth));
            }

            End::Br(cond, then_block, else_block) => {
                out.push(WasmInstr::LocalGet(cond.0));
                emit_is_truthy(out);
                out.push(WasmInstr::If(BlockType::Empty));
                out.push(WasmInstr::I64Const(then_block.0 as i64));
                out.push(WasmInstr::LocalSet(block_local));
                out.push(WasmInstr::Br(dispatch_depth + 1)); // +1 for if block
                out.push(WasmInstr::Else);
                out.push(WasmInstr::I64Const(else_block.0 as i64));
                out.push(WasmInstr::LocalSet(block_local));
                out.push(WasmInstr::Br(dispatch_depth + 1));
                out.push(WasmInstr::End);
            }

            End::Switch(reg, cases, default) => {
                for (tag_val, target) in cases {
                    out.push(WasmInstr::LocalGet(reg.0));
                    out.push(WasmInstr::I64Const(*tag_val as i64));
                    out.push(WasmInstr::I64Eq);
                    out.push(WasmInstr::If(BlockType::Empty));
                    out.push(WasmInstr::I64Const(target.0 as i64));
                    out.push(WasmInstr::LocalSet(block_local));
                    out.push(WasmInstr::Br(dispatch_depth + 1));
                    out.push(WasmInstr::End);
                }
                // Default
                out.push(WasmInstr::I64Const(default.0 as i64));
                out.push(WasmInstr::LocalSet(block_local));
                out.push(WasmInstr::Br(dispatch_depth));
            }

            End::Tail(fid, args) => {
                for arg in args {
                    out.push(WasmInstr::LocalGet(arg.0));
                }
                out.push(WasmInstr::Call(HOST_IMPORT_COUNT + fid.0));
                out.push(WasmInstr::Return);
            }

            End::TailInvoke(callee, args) => {
                for arg in args {
                    out.push(WasmInstr::LocalGet(arg.0));
                }
                out.push(WasmInstr::LocalGet(callee.0));
                emit_unbox_int(out);
                out.push(WasmInstr::I32WrapI64);
                out.push(WasmInstr::CallIndirect(args.len() as u32));
                out.push(WasmInstr::Return);
            }

            End::Recur(args) => {
                let entry_block = &func.blocks[0];
                for (i, arg) in args.iter().enumerate() {
                    if i < entry_block.params.len() {
                        let param_reg = entry_block.params[i];
                        if param_reg != *arg {
                            out.push(WasmInstr::LocalGet(arg.0));
                            out.push(WasmInstr::LocalSet(param_reg.0));
                        }
                    }
                }
                out.push(WasmInstr::I64Const(0));
                out.push(WasmInstr::LocalSet(block_local));
                out.push(WasmInstr::Br(dispatch_depth));
            }

            End::Trap => {
                out.push(WasmInstr::Unreachable);
            }
        }
        Ok(())
    }

    /// Build the final WASM binary.
    fn finish(self) -> Vec<u8> {
        let mut wasm = WasmModule::new();

        // ── Type section ──────────────────────────────────────────────
        let mut types = TypeSection::new();

        // Type 0: println import (i64) → ()
        types.ty().function(vec![ValType::I64], vec![]);
        // Type 1: print import (i64) → ()
        types.ty().function(vec![ValType::I64], vec![]);

        // Types for user functions
        let mut func_type_indices = Vec::new();
        for func in &self.functions {
            let ti = types.len();
            types.ty().function(
                vec![ValType::I64; func.param_count as usize],
                vec![ValType::I64],
            );
            func_type_indices.push(ti);
        }

        // Indirect call types (keyed by arity)
        let mut indirect_arities: Vec<u32> = Vec::new();
        for func in &self.functions {
            for instr in &func.body {
                if let WasmInstr::CallIndirect(arity) = instr {
                    if !indirect_arities.contains(arity) {
                        indirect_arities.push(*arity);
                    }
                }
            }
        }
        indirect_arities.sort();
        let mut indirect_type_map = std::collections::HashMap::new();
        for arity in &indirect_arities {
            let ti = types.len();
            types
                .ty()
                .function(vec![ValType::I64; *arity as usize], vec![ValType::I64]);
            indirect_type_map.insert(*arity, ti);
        }

        wasm.section(&types);

        // ── Import section ────────────────────────────────────────────
        let mut imports = ImportSection::new();
        imports.import("host", "println", EntityType::Function(0));
        imports.import("host", "print", EntityType::Function(1));
        wasm.section(&imports);

        // ── Function section ──────────────────────────────────────────
        let mut functions = FunctionSection::new();
        for &ti in &func_type_indices {
            functions.function(ti);
        }
        wasm.section(&functions);

        // ── Table section ─────────────────────────────────────────────
        if !self.table_entries.is_empty() {
            let mut table = TableSection::new();
            table.table(TableType {
                element_type: RefType::FUNCREF,
                minimum: self.table_entries.len() as u64,
                maximum: Some(self.table_entries.len() as u64),
                table64: false,
                shared: false,
            });
            wasm.section(&table);
        }

        // ── Memory section ────────────────────────────────────────────
        let mut mem = MemorySection::new();
        mem.memory(MemoryType {
            minimum: 2,
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        });
        wasm.section(&mem);

        // ── Global section ────────────────────────────────────────────
        let mut globals = GlobalSection::new();
        globals.global(
            GlobalType {
                val_type: ValType::I32,
                mutable: true,
                shared: false,
            },
            &ConstExpr::i32_const(INITIAL_HEAP_OFFSET as i32),
        );
        wasm.section(&globals);

        // ── Export section ─────────────────────────────────────────────
        let mut exports = ExportSection::new();
        exports.export("memory", ExportKind::Memory, 0);
        let entry_wasm_idx = HOST_IMPORT_COUNT + self.module.entry.0;
        exports.export("_start", ExportKind::Func, entry_wasm_idx);
        wasm.section(&exports);

        // ── Element section (function table) ──────────────────────────
        if !self.table_entries.is_empty() {
            let mut elems = ElementSection::new();
            elems.active(
                Some(0),
                &ConstExpr::i32_const(0),
                Elements::Functions(self.table_entries.clone().into()),
            );
            wasm.section(&elems);
        }

        // ── Code section ──────────────────────────────────────────────
        let mut code = CodeSection::new();
        for func in &self.functions {
            let locals: Vec<(u32, ValType)> = if func.local_count > 0 {
                vec![(func.local_count, ValType::I64)]
            } else {
                vec![]
            };
            let mut f = Function::new(locals);
            for instr in &func.body {
                emit_wasm_instr(&mut f, instr, &indirect_type_map);
            }
            f.instruction(&Instruction::End);
            code.function(&f);
        }
        wasm.section(&code);

        // ── Data section (string constants) ───────────────────────────
        if !self.string_data.is_empty() {
            let mut data = DataSection::new();
            for (offset, bytes) in &self.string_data {
                let mut segment = Vec::with_capacity(4 + bytes.len());
                segment.extend_from_slice(&(bytes.len() as u32).to_le_bytes());
                segment.extend_from_slice(bytes);
                data.active(
                    0,
                    &ConstExpr::i32_const(*offset as i32),
                    segment.into_iter(),
                );
            }
            wasm.section(&data);
        }

        wasm.finish()
    }

    /// Get the data segment offset for a string constant.
    fn string_offset(&self, sid: StringId) -> u32 {
        self.string_data[sid.0 as usize].0
    }

    /// Convert a Lit to its NaN-boxed i64 representation.
    fn lit_to_i64(&self, lit: &Lit) -> i64 {
        match lit {
            Lit::Int(n) => (BASE | TAG_INT | ((*n as u64) & PAYLOAD)) as i64,
            Lit::Float(f) => f.to_bits() as i64,
            Lit::Bool(true) => VAL_TRUE as i64,
            Lit::Bool(false) => VAL_FALSE as i64,
            Lit::Unit => VAL_UNIT as i64,
            Lit::Str(sid) => {
                let offset = self.string_offset(*sid);
                (BASE | TAG_PTR | (offset as u64 & PAYLOAD)) as i64
            }
            Lit::Keyword(sid) => (BASE | TAG_SYM | (sid.0 as u64)) as i64,
        }
    }
}

// ─── Free-standing helper functions ───────────────────────────────────────

fn align4(n: u32) -> u32 {
    (n + 3) & !3
}

/// Unbox NaN-boxed int: extract 48-bit payload, sign-extend to i64.
/// Consumes one i64, produces one i64.
fn emit_unbox_int(out: &mut Vec<WasmInstr>) {
    out.push(WasmInstr::I64Const(PAYLOAD as i64));
    out.push(WasmInstr::I64And);
    out.push(WasmInstr::I64Const(16));
    out.push(WasmInstr::I64Shl);
    out.push(WasmInstr::I64Const(16));
    out.push(WasmInstr::I64ShrS);
}

/// Rebox a raw i64 as a NaN-boxed int.
/// Consumes one i64, produces one i64.
fn emit_rebox_int(out: &mut Vec<WasmInstr>) {
    out.push(WasmInstr::I64Const(PAYLOAD as i64));
    out.push(WasmInstr::I64And);
    out.push(WasmInstr::I64Const((BASE | TAG_INT) as i64));
    out.push(WasmInstr::I64Or);
}

/// Push a NaN-boxed int constant onto the stack.
fn emit_box_int(n: i64, out: &mut Vec<WasmInstr>) {
    let boxed = (BASE | TAG_INT | ((n as u64) & PAYLOAD)) as i64;
    out.push(WasmInstr::I64Const(boxed));
}

/// Unbox NaN-boxed pointer to an i32 address.
/// Consumes one i64, produces one i32.
fn emit_unbox_ptr(out: &mut Vec<WasmInstr>) {
    out.push(WasmInstr::I64Const(PAYLOAD as i64));
    out.push(WasmInstr::I64And);
    out.push(WasmInstr::I32WrapI64);
}

/// Check if a NaN-boxed value is truthy.
/// Consumes one i64, produces one i32 (0 = falsy, 1 = truthy).
///
/// Falsy values: FALSE (BASE|TAG_IMM|2) and UNIT (BASE|TAG_IMM|0).
/// TRUE is BASE|TAG_IMM|1. All three share (val & ~3) == (BASE|TAG_IMM).
/// So: falsy iff (val & ~1) == (BASE|TAG_IMM|0). This catches UNIT (sub 0)
/// and FALSE (sub 2), but not TRUE (sub 1, which becomes 0 after &~1 → still
/// matches BASE|TAG_IMM). Wait — TRUE has bit 0 = 1, so TRUE & ~1 strips
/// bit 0 → gives BASE|TAG_IMM|0 → would be detected as falsy. That's wrong.
///
/// Correct approach: falsy = val == FALSE || val == UNIT. Since we can only
/// consume the value once, we use a mathematical test:
///   FALSE = BASE|TAG_IMM|2, UNIT = BASE|TAG_IMM|0
///   Both have bit 0 = 0. TRUE has bit 0 = 1.
///   For non-immediates, (val >> 48) & 7 != 7 (TAG_IMM = 7).
///
/// Test: (val & ~3) == (BASE|TAG_IMM) && (val & 1) == 0
/// But that's two conditions and we only have val once.
///
/// Simplest correct approach: subtract UNIT, check if result is 0 or 2.
///   val - UNIT == 0 → UNIT
///   val - UNIT == 2 → FALSE
///   Otherwise truthy.
/// (val - UNIT) & ~2 == 0 catches both cases.
fn emit_is_truthy(out: &mut Vec<WasmInstr>) {
    // Stack: [val: i64]
    out.push(WasmInstr::I64Const(VAL_UNIT as i64));
    out.push(WasmInstr::I64Sub);
    // now: 0 if UNIT, 2 if FALSE, something else if truthy
    out.push(WasmInstr::I64Const(!2i64)); // mask: clear bit 1
    out.push(WasmInstr::I64And);
    // If the result is 0 → falsy (was UNIT or FALSE). Non-zero → truthy.
    // i64.eqz returns 1 if zero (falsy), 0 if non-zero (truthy). We want the opposite.
    out.push(WasmInstr::I64Const(0));
    out.push(WasmInstr::I64Ne);
    // Stack: [is_truthy: i32] (1 = truthy, 0 = falsy)
}

/// Convert an i32 boolean (0/1) on the stack to a NaN-boxed Val (TRUE/FALSE).
fn emit_bool_to_val(out: &mut Vec<WasmInstr>) {
    out.push(WasmInstr::If(BlockType::Result(ValType::I64)));
    out.push(WasmInstr::I64Const(VAL_TRUE as i64));
    out.push(WasmInstr::Else);
    out.push(WasmInstr::I64Const(VAL_FALSE as i64));
    out.push(WasmInstr::End);
}

/// Bump-allocate `size` bytes. Stores the address as i64 in `addr_local`.
/// Leaves the stack clean (nothing pushed).
/// Use `emit_addr_as_i32` to get the i32 address for memory operations.
fn emit_heap_alloc(size: u32, addr_local: u32, out: &mut Vec<WasmInstr>) {
    let aligned = align4(size);
    // addr = heap_ptr (extended to i64 for storage in our i64 local)
    out.push(WasmInstr::GlobalGet(GLOBAL_HEAP_PTR));
    out.push(WasmInstr::I64ExtendI32U);
    out.push(WasmInstr::LocalSet(addr_local));
    // heap_ptr += aligned_size
    out.push(WasmInstr::GlobalGet(GLOBAL_HEAP_PTR));
    out.push(WasmInstr::I32Const(aligned as i32));
    out.push(WasmInstr::I32Add);
    out.push(WasmInstr::GlobalSet(GLOBAL_HEAP_PTR));
}

/// Push the heap address from `addr_local` as an i32 (for memory operations).
fn emit_addr_as_i32(addr_local: u32, out: &mut Vec<WasmInstr>) {
    out.push(WasmInstr::LocalGet(addr_local));
    out.push(WasmInstr::I32WrapI64);
}

/// Push the heap address from `addr_local` as a NaN-boxed pointer (i64).
fn emit_addr_as_boxed_ptr(addr_local: u32, out: &mut Vec<WasmInstr>) {
    out.push(WasmInstr::LocalGet(addr_local));
    out.push(WasmInstr::I64Const(PAYLOAD as i64));
    out.push(WasmInstr::I64And);
    out.push(WasmInstr::I64Const((BASE | TAG_PTR) as i64));
    out.push(WasmInstr::I64Or);
}

/// Translate our instruction enum to wasm_encoder instructions.
fn emit_wasm_instr(
    f: &mut Function,
    instr: &WasmInstr,
    indirect_type_map: &std::collections::HashMap<u32, u32>,
) {
    match instr {
        WasmInstr::I64Const(n) => {
            f.instruction(&Instruction::I64Const(*n));
        }
        WasmInstr::F64Const(n) => {
            f.instruction(&Instruction::F64Const(*n));
        }
        WasmInstr::I32Const(n) => {
            f.instruction(&Instruction::I32Const(*n));
        }
        WasmInstr::LocalGet(i) => {
            f.instruction(&Instruction::LocalGet(*i));
        }
        WasmInstr::LocalSet(i) => {
            f.instruction(&Instruction::LocalSet(*i));
        }
        WasmInstr::LocalTee(i) => {
            f.instruction(&Instruction::LocalTee(*i));
        }
        WasmInstr::GlobalGet(i) => {
            f.instruction(&Instruction::GlobalGet(*i));
        }
        WasmInstr::GlobalSet(i) => {
            f.instruction(&Instruction::GlobalSet(*i));
        }
        WasmInstr::Call(i) => {
            f.instruction(&Instruction::Call(*i));
        }
        WasmInstr::CallIndirect(arity) => {
            let type_idx = indirect_type_map.get(arity).copied().unwrap_or(0);
            f.instruction(&Instruction::CallIndirect {
                type_index: type_idx,
                table_index: 0,
            });
        }
        WasmInstr::Return => {
            f.instruction(&Instruction::Return);
        }
        WasmInstr::Drop => {
            f.instruction(&Instruction::Drop);
        }
        WasmInstr::Unreachable => {
            f.instruction(&Instruction::Unreachable);
        }
        WasmInstr::I64Add => {
            f.instruction(&Instruction::I64Add);
        }
        WasmInstr::I64Sub => {
            f.instruction(&Instruction::I64Sub);
        }
        WasmInstr::I64Mul => {
            f.instruction(&Instruction::I64Mul);
        }
        WasmInstr::I64DivS => {
            f.instruction(&Instruction::I64DivS);
        }
        WasmInstr::I64RemS => {
            f.instruction(&Instruction::I64RemS);
        }
        WasmInstr::I64And => {
            f.instruction(&Instruction::I64And);
        }
        WasmInstr::I64Or => {
            f.instruction(&Instruction::I64Or);
        }
        WasmInstr::I64Xor => {
            f.instruction(&Instruction::I64Xor);
        }
        WasmInstr::I64Shl => {
            f.instruction(&Instruction::I64Shl);
        }
        WasmInstr::I64ShrS => {
            f.instruction(&Instruction::I64ShrS);
        }
        WasmInstr::I64ShrU => {
            f.instruction(&Instruction::I64ShrU);
        }
        WasmInstr::I64Eq => {
            f.instruction(&Instruction::I64Eq);
        }
        WasmInstr::I64Ne => {
            f.instruction(&Instruction::I64Ne);
        }
        WasmInstr::I64LtS => {
            f.instruction(&Instruction::I64LtS);
        }
        WasmInstr::I64GtS => {
            f.instruction(&Instruction::I64GtS);
        }
        WasmInstr::I64LeS => {
            f.instruction(&Instruction::I64LeS);
        }
        WasmInstr::I64GeS => {
            f.instruction(&Instruction::I64GeS);
        }
        WasmInstr::I64Eqz => {
            f.instruction(&Instruction::I64Eqz);
        }
        WasmInstr::F64Add => {
            f.instruction(&Instruction::F64Add);
        }
        WasmInstr::F64Sub => {
            f.instruction(&Instruction::F64Sub);
        }
        WasmInstr::F64Mul => {
            f.instruction(&Instruction::F64Mul);
        }
        WasmInstr::F64Div => {
            f.instruction(&Instruction::F64Div);
        }
        WasmInstr::F64Neg => {
            f.instruction(&Instruction::F64Neg);
        }
        WasmInstr::F64Eq => {
            f.instruction(&Instruction::F64Eq);
        }
        WasmInstr::F64Ne => {
            f.instruction(&Instruction::F64Ne);
        }
        WasmInstr::F64Lt => {
            f.instruction(&Instruction::F64Lt);
        }
        WasmInstr::F64Gt => {
            f.instruction(&Instruction::F64Gt);
        }
        WasmInstr::F64Le => {
            f.instruction(&Instruction::F64Le);
        }
        WasmInstr::F64Ge => {
            f.instruction(&Instruction::F64Ge);
        }
        WasmInstr::F64Abs => {
            f.instruction(&Instruction::F64Abs);
        }
        WasmInstr::I32Add => {
            f.instruction(&Instruction::I32Add);
        }
        WasmInstr::I32And => {
            f.instruction(&Instruction::I32And);
        }
        WasmInstr::I32WrapI64 => {
            f.instruction(&Instruction::I32WrapI64);
        }
        WasmInstr::I64ExtendI32U => {
            f.instruction(&Instruction::I64ExtendI32U);
        }
        WasmInstr::I64ExtendI32S => {
            f.instruction(&Instruction::I64ExtendI32S);
        }
        WasmInstr::F64ReinterpretI64 => {
            f.instruction(&Instruction::F64ReinterpretI64);
        }
        WasmInstr::I64ReinterpretF64 => {
            f.instruction(&Instruction::I64ReinterpretF64);
        }
        WasmInstr::I64Store(align, offset) => {
            f.instruction(&Instruction::I64Store(MemArg {
                offset: *offset as u64,
                align: *align,
                memory_index: 0,
            }));
        }
        WasmInstr::I64Load(align, offset) => {
            f.instruction(&Instruction::I64Load(MemArg {
                offset: *offset as u64,
                align: *align,
                memory_index: 0,
            }));
        }
        WasmInstr::I32Store(align, offset) => {
            f.instruction(&Instruction::I32Store(MemArg {
                offset: *offset as u64,
                align: *align,
                memory_index: 0,
            }));
        }
        WasmInstr::I32Load(align, offset) => {
            f.instruction(&Instruction::I32Load(MemArg {
                offset: *offset as u64,
                align: *align,
                memory_index: 0,
            }));
        }
        WasmInstr::Block(bt) => {
            f.instruction(&Instruction::Block(*bt));
        }
        WasmInstr::Loop(bt) => {
            f.instruction(&Instruction::Loop(*bt));
        }
        WasmInstr::Br(depth) => {
            f.instruction(&Instruction::Br(*depth));
        }
        WasmInstr::BrIf(depth) => {
            f.instruction(&Instruction::BrIf(*depth));
        }
        WasmInstr::BrTable(labels, default) => {
            f.instruction(&Instruction::BrTable(
                std::borrow::Cow::Borrowed(labels),
                *default,
            ));
        }
        WasmInstr::End => {
            f.instruction(&Instruction::End);
        }
        WasmInstr::If(bt) => {
            f.instruction(&Instruction::If(*bt));
        }
        WasmInstr::Else => {
            f.instruction(&Instruction::Else);
        }
        WasmInstr::Select => {
            f.instruction(&Instruction::Select);
        }
    }
}

// ─── Tests ────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eir::backend::Backend;

    /// Build a minimal EIR module that returns a literal int.
    fn make_int_module(n: i64) -> super::super::Module {
        super::super::Module {
            funcs: vec![Func {
                id: FuncId(0),
                name: Some("main".to_string()),
                params: vec![],
                ret: Ty::Int,
                evidence: vec![],
                captures: vec![],
                blocks: vec![Block {
                    id: BlockId(0),
                    params: vec![],
                    ops: vec![Op::Lit(Reg(0), Lit::Int(n), Span::ZERO)],
                    end: End::Ret(Reg(0)),
                }],
                span: Span::ZERO,
                is_closure: false,
            }],
            strings: vec![],
            ctors: vec![],
            entry: FuncId(0),
        }
    }

    /// Build a module that adds two ints.
    fn make_add_module(a: i64, b: i64) -> super::super::Module {
        super::super::Module {
            funcs: vec![Func {
                id: FuncId(0),
                name: Some("main".to_string()),
                params: vec![],
                ret: Ty::Int,
                evidence: vec![],
                captures: vec![],
                blocks: vec![Block {
                    id: BlockId(0),
                    params: vec![],
                    ops: vec![
                        Op::Lit(Reg(0), Lit::Int(a), Span::ZERO),
                        Op::Lit(Reg(1), Lit::Int(b), Span::ZERO),
                        Op::Bin(Reg(2), BinOp::Add, Reg(0), Reg(1), Span::ZERO),
                    ],
                    end: End::Ret(Reg(2)),
                }],
                span: Span::ZERO,
                is_closure: false,
            }],
            strings: vec![],
            ctors: vec![],
            entry: FuncId(0),
        }
    }

    /// Build a module with a conditional branch.
    fn make_branch_module() -> super::super::Module {
        super::super::Module {
            funcs: vec![Func {
                id: FuncId(0),
                name: Some("main".to_string()),
                params: vec![],
                ret: Ty::Int,
                evidence: vec![],
                captures: vec![],
                blocks: vec![
                    Block {
                        id: BlockId(0),
                        params: vec![],
                        ops: vec![
                            Op::Lit(Reg(0), Lit::Int(5), Span::ZERO),
                            Op::Lit(Reg(1), Lit::Int(0), Span::ZERO),
                            Op::Bin(Reg(2), BinOp::Gt, Reg(0), Reg(1), Span::ZERO),
                        ],
                        end: End::Br(Reg(2), BlockId(1), BlockId(2)),
                    },
                    Block {
                        id: BlockId(1),
                        params: vec![],
                        ops: vec![Op::Lit(Reg(3), Lit::Int(1), Span::ZERO)],
                        end: End::Ret(Reg(3)),
                    },
                    Block {
                        id: BlockId(2),
                        params: vec![],
                        ops: vec![Op::Lit(Reg(3), Lit::Int(0), Span::ZERO)],
                        end: End::Ret(Reg(3)),
                    },
                ],
                span: Span::ZERO,
                is_closure: false,
            }],
            strings: vec![],
            ctors: vec![],
            entry: FuncId(0),
        }
    }

    /// Build a module with two functions: main calls double(21).
    fn make_call_module() -> super::super::Module {
        super::super::Module {
            funcs: vec![
                Func {
                    id: FuncId(0),
                    name: Some("double".to_string()),
                    params: vec![Ty::Int],
                    ret: Ty::Int,
                    evidence: vec![],
                    captures: vec![],
                    blocks: vec![Block {
                        id: BlockId(0),
                        params: vec![Reg(0)],
                        ops: vec![Op::Bin(Reg(1), BinOp::Add, Reg(0), Reg(0), Span::ZERO)],
                        end: End::Ret(Reg(1)),
                    }],
                    span: Span::ZERO,
                    is_closure: false,
                },
                Func {
                    id: FuncId(1),
                    name: Some("main".to_string()),
                    params: vec![],
                    ret: Ty::Int,
                    evidence: vec![],
                    captures: vec![],
                    blocks: vec![Block {
                        id: BlockId(0),
                        params: vec![],
                        ops: vec![
                            Op::Lit(Reg(0), Lit::Int(21), Span::ZERO),
                            Op::Call(Reg(1), FuncId(0), vec![Reg(0)], Span::ZERO),
                        ],
                        end: End::Ret(Reg(1)),
                    }],
                    span: Span::ZERO,
                    is_closure: false,
                },
            ],
            strings: vec![],
            ctors: vec![],
            entry: FuncId(1),
        }
    }

    /// Build a module with a recursive countdown using Recur.
    fn make_recur_module() -> super::super::Module {
        // countdown(n): if n <= 0 then 0 else recur(n - 1)
        super::super::Module {
            funcs: vec![
                // func 0: countdown(n)
                Func {
                    id: FuncId(0),
                    name: Some("countdown".to_string()),
                    params: vec![Ty::Int],
                    ret: Ty::Int,
                    evidence: vec![],
                    captures: vec![],
                    blocks: vec![
                        Block {
                            id: BlockId(0),
                            params: vec![Reg(0)], // n
                            ops: vec![
                                Op::Lit(Reg(1), Lit::Int(0), Span::ZERO),
                                Op::Bin(Reg(2), BinOp::Le, Reg(0), Reg(1), Span::ZERO),
                            ],
                            end: End::Br(Reg(2), BlockId(1), BlockId(2)),
                        },
                        Block {
                            id: BlockId(1),
                            params: vec![],
                            ops: vec![Op::Lit(Reg(3), Lit::Int(0), Span::ZERO)],
                            end: End::Ret(Reg(3)),
                        },
                        Block {
                            id: BlockId(2),
                            params: vec![],
                            ops: vec![
                                Op::Lit(Reg(3), Lit::Int(1), Span::ZERO),
                                Op::Bin(Reg(4), BinOp::Sub, Reg(0), Reg(3), Span::ZERO),
                            ],
                            end: End::Recur(vec![Reg(4)]),
                        },
                    ],
                    span: Span::ZERO,
                    is_closure: false,
                },
                // func 1: main() = countdown(10)
                Func {
                    id: FuncId(1),
                    name: Some("main".to_string()),
                    params: vec![],
                    ret: Ty::Int,
                    evidence: vec![],
                    captures: vec![],
                    blocks: vec![Block {
                        id: BlockId(0),
                        params: vec![],
                        ops: vec![
                            Op::Lit(Reg(0), Lit::Int(10), Span::ZERO),
                            Op::Call(Reg(1), FuncId(0), vec![Reg(0)], Span::ZERO),
                        ],
                        end: End::Ret(Reg(1)),
                    }],
                    span: Span::ZERO,
                    is_closure: false,
                },
            ],
            strings: vec![],
            ctors: vec![],
            entry: FuncId(1),
        }
    }

    #[test]
    fn compile_produces_valid_wasm() {
        let module = make_int_module(42);
        let mut backend = WasmBackend;
        let bytes = backend.compile(&module).expect("compilation failed");

        assert!(bytes.len() >= 8, "WASM binary too short");
        assert_eq!(&bytes[0..4], b"\0asm", "missing WASM magic number");
        assert_eq!(bytes[4..8], [1, 0, 0, 0], "unexpected WASM version");
    }

    #[test]
    fn compile_add_module() {
        let module = make_add_module(10, 32);
        let mut backend = WasmBackend;
        let bytes = backend.compile(&module).expect("compilation failed");
        assert_eq!(&bytes[0..4], b"\0asm");
    }

    #[test]
    fn compile_branch_module() {
        let module = make_branch_module();
        let mut backend = WasmBackend;
        let bytes = backend.compile(&module).expect("compilation failed");
        assert_eq!(&bytes[0..4], b"\0asm");
    }

    #[test]
    fn compile_call_module() {
        let module = make_call_module();
        let mut backend = WasmBackend;
        let bytes = backend.compile(&module).expect("compilation failed");
        assert_eq!(&bytes[0..4], b"\0asm");
    }

    #[test]
    fn compile_recur_module() {
        let module = make_recur_module();
        let mut backend = WasmBackend;
        let bytes = backend.compile(&module).expect("compilation failed");
        assert_eq!(&bytes[0..4], b"\0asm");
    }

    #[test]
    fn compile_empty_function() {
        let module = super::super::Module {
            funcs: vec![Func {
                id: FuncId(0),
                name: Some("main".to_string()),
                params: vec![],
                ret: Ty::Unit,
                evidence: vec![],
                captures: vec![],
                blocks: vec![],
                span: Span::ZERO,
                is_closure: false,
            }],
            strings: vec![],
            ctors: vec![],
            entry: FuncId(0),
        };
        let mut backend = WasmBackend;
        let bytes = backend.compile(&module).expect("compilation failed");
        assert_eq!(&bytes[0..4], b"\0asm");
    }

    #[test]
    fn compile_with_strings() {
        let module = super::super::Module {
            funcs: vec![Func {
                id: FuncId(0),
                name: Some("main".to_string()),
                params: vec![],
                ret: Ty::Str,
                evidence: vec![],
                captures: vec![],
                blocks: vec![Block {
                    id: BlockId(0),
                    params: vec![],
                    ops: vec![Op::Lit(Reg(0), Lit::Str(StringId(0)), Span::ZERO)],
                    end: End::Ret(Reg(0)),
                }],
                span: Span::ZERO,
                is_closure: false,
            }],
            strings: vec!["hello world".to_string()],
            ctors: vec![],
            entry: FuncId(0),
        };
        let mut backend = WasmBackend;
        let bytes = backend.compile(&module).expect("compilation failed");
        assert_eq!(&bytes[0..4], b"\0asm");
    }

    #[test]
    #[allow(clippy::approx_constant)]
    fn lit_to_i64_roundtrip() {
        let module = super::super::Module {
            funcs: vec![],
            strings: vec!["hello".to_string()],
            ctors: vec![],
            entry: FuncId(0),
        };
        let mut ctx = CompileCtx::new(&module);
        ctx.layout_strings();

        // Int
        let boxed = ctx.lit_to_i64(&Lit::Int(42));
        let val = crate::eir::value64::Val::from_bits(boxed as u64);
        assert!(val.is_int());
        assert_eq!(val.as_int(), 42);

        // Negative int
        let boxed = ctx.lit_to_i64(&Lit::Int(-7));
        let val = crate::eir::value64::Val::from_bits(boxed as u64);
        assert!(val.is_int());
        assert_eq!(val.as_int(), -7);

        // Bool
        let val = crate::eir::value64::Val::from_bits(ctx.lit_to_i64(&Lit::Bool(true)) as u64);
        assert!(val.is_bool());
        assert!(val.as_bool());

        let val = crate::eir::value64::Val::from_bits(ctx.lit_to_i64(&Lit::Bool(false)) as u64);
        assert!(val.is_bool());
        assert!(!val.as_bool());

        // Unit
        let val = crate::eir::value64::Val::from_bits(ctx.lit_to_i64(&Lit::Unit) as u64);
        assert!(val.is_unit());

        // Float
        let val = crate::eir::value64::Val::from_bits(ctx.lit_to_i64(&Lit::Float(3.14)) as u64);
        assert!(val.is_float());
        assert_eq!(val.as_float(), 3.14);
    }

    #[test]
    fn nan_boxing_constants_match_value64() {
        use crate::eir::value64::Val;
        assert_eq!(Val::UNIT.bits(), VAL_UNIT);
        assert_eq!(Val::TRUE.bits(), VAL_TRUE);
        assert_eq!(Val::FALSE.bits(), VAL_FALSE);
        assert_eq!(Val::int(42).bits(), (BASE | TAG_INT | 42));
    }

    #[test]
    fn backend_name() {
        assert_eq!(WasmBackend.name(), "wasm");
    }
}
