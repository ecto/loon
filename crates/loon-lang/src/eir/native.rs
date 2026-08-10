//! Native backend — compiles Evidence IR to machine code via Cranelift.
//!
//! Uses `cranelift_jit::JITModule` for in-memory compilation and execution.
//! Each EIR function becomes a Cranelift function. All values are i64
//! (NaN-boxed, matching `Val`). Complex operations (collections, closures,
//! string ops) call back into Rust helper functions via imported symbols.
//!
//! Current coverage:
//! - Literals (int, float, bool, unit)
//! - Arithmetic, comparison, and logic binary ops
//! - Unary ops (neg, not)
//! - Mov, branches, jumps, returns
//! - Function calls (direct), including tail calls (`return_call`)
//! - Builtin println (via extern)
//!
//! Not yet implemented (fall back to VM):
//! - Closures / upvalues / indirect calls
//! - Collection construction (Vec, Map, Set, Tuple, ADT)
//! - Field access, tag extraction
//! - Effect operations (perform, push/pop handler)
//! - String operations
//!
//! Loon functions are compiled with Cranelift's `tail` calling convention so
//! that `End::Tail` can lower to a real `return_call` (constant stack for
//! mutual tail recursion). That convention is not the platform C ABI, so the
//! entry point is reached through a small C-ABI trampoline — see
//! `ENTRY_TRAMPOLINE`.

use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::types::I64;
use cranelift_codegen::ir::{AbiParam, Function, InstBuilder, Signature, UserFuncName};
use cranelift_codegen::isa::CallConv;
use cranelift_codegen::settings::{self, Configurable};
use cranelift_codegen::Context;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module};

use super::backend::{Backend, Error};
use super::value64::Val;
use super::{BinOp, End, Lit, Op, Reg, UnOp};

use std::collections::HashMap;

// ─── NaN-boxing constants (must match value64.rs) ───────────────────────────

const QNAN: u64 = 0x7FF8_0000_0000_0000;
const SIGN: u64 = 0x8000_0000_0000_0000;
const BASE: u64 = SIGN | QNAN;
const TAG_INT: u64 = 0x0001_0000_0000_0000;
const TAG_IMM: u64 = 0x0007_0000_0000_0000;
const PAYLOAD: u64 = 0x0000_FFFF_FFFF_FFFF;

const VAL_UNIT: u64 = BASE | TAG_IMM;
const VAL_TRUE: u64 = BASE | TAG_IMM | 1;
const VAL_FALSE: u64 = BASE | TAG_IMM | 2;
const VAL_NONE: u64 = BASE | TAG_IMM | 3;

/// Symbol name of the C-ABI shim that calls the module's entry function.
const ENTRY_TRAMPOLINE: &str = "loon_entry_trampoline";

// ─── Runtime helper functions ───────────────────────────────────────────────

/// Runtime: println a NaN-boxed value. Called from compiled code.
///
/// # Safety
/// Called from JIT-compiled code with a valid NaN-boxed i64.
unsafe extern "C" fn rt_println(val: i64) -> i64 {
    let v = Val::from_bits(val as u64);
    if v.is_int() {
        println!("{}", v.as_int());
    } else if v.is_float() {
        let f = v.as_float();
        if f == f.floor() && f.is_finite() {
            println!("{f:.1}");
        } else {
            println!("{f}");
        }
    } else if v.is_bool() {
        println!("{}", v.as_bool());
    } else if v.is_unit() {
        println!("()");
    } else {
        println!("<val:0x{:016x}>", v.bits());
    }
    VAL_UNIT as i64
}

/// Runtime: print (no newline) a NaN-boxed value.
///
/// # Safety
/// Called from JIT-compiled code with a valid NaN-boxed i64.
unsafe extern "C" fn rt_print(val: i64) -> i64 {
    let v = Val::from_bits(val as u64);
    if v.is_int() {
        print!("{}", v.as_int());
    } else if v.is_float() {
        let f = v.as_float();
        if f == f.floor() && f.is_finite() {
            print!("{f:.1}");
        } else {
            print!("{f}");
        }
    } else if v.is_bool() {
        print!("{}", v.as_bool());
    } else if v.is_unit() {
        print!("()");
    } else {
        print!("<val:0x{:016x}>", v.bits());
    }
    VAL_UNIT as i64
}

/// Runtime: convert a NaN-boxed value to its string representation.
/// Returns a heap pointer (not implemented yet — returns unit).
///
/// # Safety
/// Called from JIT-compiled code.
unsafe extern "C" fn rt_str(_val: i64) -> i64 {
    // TODO: allocate string on heap and return pointer
    VAL_UNIT as i64
}

// ─── Compilation context ────────────────────────────────────────────────────

/// Map from EIR FuncId to Cranelift FuncId.
struct FuncMap {
    /// EIR func index -> Cranelift FuncId
    funcs: HashMap<u32, cranelift_module::FuncId>,
}

/// The native backend. Compiles an EIR Module to executable machine code.
pub struct NativeBackend;

impl Backend for NativeBackend {
    type Output = NativeModule;

    fn compile(&mut self, module: &super::Module) -> Result<NativeModule, Error> {
        NativeModule::compile(module)
    }

    fn name(&self) -> &'static str {
        "native"
    }
}

/// A compiled native module, ready to execute.
pub struct NativeModule {
    /// The JIT module holding compiled code (kept alive for the lifetime of compiled code).
    _jit: JITModule,
    /// Entry function pointer.
    entry_fn: *const u8,
}

// SAFETY: The JIT-compiled code and function pointers are safe to send
// across threads — they're just machine code in memory.
unsafe impl Send for NativeModule {}
unsafe impl Sync for NativeModule {}

impl NativeModule {
    /// Compile an EIR module to native code.
    fn compile(eir_module: &super::Module) -> Result<Self, Error> {
        // Set up Cranelift JIT.
        // Build ISA manually to handle platform differences.
        // On aarch64, we must NOT set is_pic (PLT not supported).
        let mut flag_builder = settings::builder();
        flag_builder.set("opt_level", "speed").map_err(|e| Error {
            message: format!("cranelift flag error: {e}"),
            phase: "native:setup",
        })?;
        flag_builder
            .set("use_colocated_libcalls", "false")
            .map_err(|e| Error {
                message: format!("cranelift flag error: {e}"),
                phase: "native:setup",
            })?;
        // Only enable PIC on x86_64 where PLT is supported.
        if cfg!(target_arch = "x86_64") {
            flag_builder.set("is_pic", "true").map_err(|e| Error {
                message: format!("cranelift flag error: {e}"),
                phase: "native:setup",
            })?;
        }
        // Required by `CallConv::Tail`, which every Loon function uses so that
        // `End::Tail` can lower to `return_call`. Cranelift's x64 tail-call
        // emitter asserts on a missing frame pointer ("frame pointers aren't
        // fundamentally required for tail calls, but the current
        // implementation relies on them being present"); aarch64 maintains one
        // unconditionally, so this only bites on x86_64.
        flag_builder
            .set("preserve_frame_pointers", "true")
            .map_err(|e| Error {
                message: format!("cranelift flag error: {e}"),
                phase: "native:setup",
            })?;
        let isa_builder = cranelift_native::builder().map_err(|msg| Error {
            message: format!("unsupported host: {msg}"),
            phase: "native:setup",
        })?;
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .map_err(|e| Error {
                message: format!("ISA error: {e}"),
                phase: "native:setup",
            })?;

        let mut builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

        // Register runtime helper symbols.
        builder.symbol("rt_println", rt_println as *const u8);
        builder.symbol("rt_print", rt_print as *const u8);
        builder.symbol("rt_str", rt_str as *const u8);

        let mut jit = JITModule::new(builder);

        // The platform C ABI, used for the runtime helpers (plain Rust
        // `extern "C"` functions) and for the entry trampoline.
        let c_call_conv = jit.isa().default_call_conv();
        // Loon functions use the `tail` convention instead: `return_call`
        // requires caller and callee to share a tail-call-capable convention.
        let call_conv = CallConv::Tail;

        // Declare runtime helper functions.
        let rt_println_sig = {
            let mut sig = Signature::new(c_call_conv);
            sig.params.push(AbiParam::new(I64));
            sig.returns.push(AbiParam::new(I64));
            sig
        };
        let rt_println_id = jit
            .declare_function("rt_println", Linkage::Import, &rt_println_sig)
            .map_err(|e| Error {
                message: format!("declare rt_println: {e}"),
                phase: "native:declare",
            })?;

        let rt_print_sig = rt_println_sig.clone();
        let rt_print_id = jit
            .declare_function("rt_print", Linkage::Import, &rt_print_sig)
            .map_err(|e| Error {
                message: format!("declare rt_print: {e}"),
                phase: "native:declare",
            })?;

        let rt_str_sig = rt_println_sig.clone();
        let rt_str_id = jit
            .declare_function("rt_str", Linkage::Import, &rt_str_sig)
            .map_err(|e| Error {
                message: format!("declare rt_str: {e}"),
                phase: "native:declare",
            })?;

        // Declare all EIR functions.
        let mut func_map = FuncMap {
            funcs: HashMap::new(),
        };

        for (i, func) in eir_module.funcs.iter().enumerate() {
            let name = func
                .name
                .as_deref()
                .map(|n| format!("loon_{n}_{i}"))
                .unwrap_or_else(|| format!("loon_anon_{i}"));

            let mut sig = Signature::new(call_conv);
            // All params are i64 (NaN-boxed Val).
            for _ in &func.params {
                sig.params.push(AbiParam::new(I64));
            }
            // Evidence params are also i64.
            for _ in &func.evidence {
                sig.params.push(AbiParam::new(I64));
            }
            // Single i64 return.
            sig.returns.push(AbiParam::new(I64));

            let func_id = jit
                .declare_function(&name, Linkage::Local, &sig)
                .map_err(|e| Error {
                    message: format!("declare {name}: {e}"),
                    phase: "native:declare",
                })?;

            func_map.funcs.insert(i as u32, func_id);
        }

        // Compile each EIR function.
        let mut fb_ctx = FunctionBuilderContext::new();

        for (eir_idx, eir_func) in eir_module.funcs.iter().enumerate() {
            let cl_func_id = func_map.funcs[&(eir_idx as u32)];

            let mut sig = Signature::new(call_conv);
            for _ in &eir_func.params {
                sig.params.push(AbiParam::new(I64));
            }
            for _ in &eir_func.evidence {
                sig.params.push(AbiParam::new(I64));
            }
            sig.returns.push(AbiParam::new(I64));

            let mut cl_func =
                Function::with_name_signature(UserFuncName::user(0, eir_idx as u32), sig);

            // Compile function body.
            compile_function(
                &mut cl_func,
                &mut fb_ctx,
                eir_func,
                eir_module,
                &func_map,
                &mut jit,
                rt_println_id,
                rt_print_id,
                rt_str_id,
            )?;

            // Define in JIT module.
            let mut ctx = Context::for_function(cl_func);
            jit.define_function(cl_func_id, &mut ctx)
                .map_err(|e| Error {
                    message: format!(
                        "define func {}: {e}",
                        eir_func.name.as_deref().unwrap_or("anon")
                    ),
                    phase: "native:codegen",
                })?;
        }

        // Entry trampoline: `execute()` calls the module through a plain C
        // function pointer, but the entry itself uses the `tail` convention,
        // which is not the C ABI. Bridge the two with a C-ABI shim that calls
        // the entry and returns its result.
        let entry_cl_id = func_map.funcs[&eir_module.entry.0];
        let trampoline_id = {
            let mut sig = Signature::new(c_call_conv);
            sig.returns.push(AbiParam::new(I64));
            let id = jit
                .declare_function(ENTRY_TRAMPOLINE, Linkage::Local, &sig)
                .map_err(|e| Error {
                    message: format!("declare {ENTRY_TRAMPOLINE}: {e}"),
                    phase: "native:declare",
                })?;

            // Namespace 1: EIR functions occupy namespace 0, indexed by
            // FuncId. The trampoline is not an EIR function, so it gets its
            // own namespace rather than an index just past the end of theirs.
            let mut cl_func = Function::with_name_signature(UserFuncName::user(1, 0), sig);
            {
                let mut builder = FunctionBuilder::new(&mut cl_func, &mut fb_ctx);
                let block = builder.create_block();
                builder.switch_to_block(block);
                builder.seal_block(block);
                let entry_ref = jit.declare_func_in_func(entry_cl_id, builder.func);
                let call = builder.ins().call(entry_ref, &[]);
                let result = builder.inst_results(call)[0];
                builder.ins().return_(&[result]);
                builder.finalize();
            }
            let mut ctx = Context::for_function(cl_func);
            jit.define_function(id, &mut ctx).map_err(|e| Error {
                message: format!("define {ENTRY_TRAMPOLINE}: {e}"),
                phase: "native:codegen",
            })?;
            id
        };

        // Finalize all definitions.
        jit.finalize_definitions().map_err(|e| Error {
            message: format!("finalize: {e}"),
            phase: "native:finalize",
        })?;

        // Get entry function pointer (the C-ABI trampoline, not the entry
        // itself — see above).
        let entry_fn = jit.get_finalized_function(trampoline_id);

        Ok(NativeModule {
            _jit: jit,
            entry_fn,
        })
    }

    /// Execute the compiled module's entry function.
    ///
    /// # Safety
    /// The compiled code must match the expected calling convention
    /// (no params, returns i64).
    pub fn execute(&self) -> Val {
        let entry: unsafe extern "C" fn() -> i64 = unsafe { std::mem::transmute(self.entry_fn) };
        let result = unsafe { entry() };
        Val::from_bits(result as u64)
    }

    /// Get the raw function pointer for the entry function.
    pub fn entry_ptr(&self) -> *const u8 {
        self.entry_fn
    }
}

impl Drop for NativeModule {
    fn drop(&mut self) {
        // JITModule handles its own cleanup via Drop.
        // We just need to ensure we don't use entry_fn after drop.
        self.entry_fn = std::ptr::null();
    }
}

// ─── Function compilation ───────────────────────────────────────────────────

#[allow(clippy::too_many_arguments)]
fn compile_function(
    cl_func: &mut Function,
    fb_ctx: &mut FunctionBuilderContext,
    eir_func: &super::Func,
    eir_module: &super::Module,
    func_map: &FuncMap,
    jit: &mut JITModule,
    rt_println_id: cranelift_module::FuncId,
    rt_print_id: cranelift_module::FuncId,
    rt_str_id: cranelift_module::FuncId,
) -> Result<(), Error> {
    let mut builder = FunctionBuilder::new(cl_func, fb_ctx);

    // Compute the max register index used in this function.
    let max_reg = eir_func
        .blocks
        .iter()
        .flat_map(|b| {
            b.ops
                .iter()
                .map(|op| op.dst().0)
                .chain(b.params.iter().map(|r| r.0))
        })
        .max()
        .unwrap_or(0);

    // Declare Cranelift variables for each EIR register.
    let mut vars: Vec<Variable> = Vec::new();
    for i in 0..=(max_reg + 16) {
        let var = Variable::from_u32(i);
        builder.declare_var(var, I64);
        vars.push(var);
    }

    // Create Cranelift blocks for each EIR block.
    let mut cl_blocks: Vec<cranelift_codegen::ir::Block> = Vec::new();
    for _ in &eir_func.blocks {
        cl_blocks.push(builder.create_block());
    }

    // Entry block: append parameters.
    let entry_block = cl_blocks[0];
    builder.append_block_params_for_function_params(entry_block);
    builder.switch_to_block(entry_block);
    builder.seal_block(entry_block);

    // Bind function parameters to variables.
    let param_count = eir_func.params.len() + eir_func.evidence.len();
    for i in 0..param_count {
        let param_val = builder.block_params(entry_block)[i];
        // EIR params are in entry block's params list.
        if i < eir_func.blocks[0].params.len() {
            let reg = eir_func.blocks[0].params[i];
            builder.def_var(vars[reg.0 as usize], param_val);
        }
    }

    // Initialize unset variables to VAL_UNIT to prevent use-before-def.
    let unit_const = builder.ins().iconst(I64, VAL_UNIT as i64);
    for var in vars
        .iter()
        .skip(param_count)
        .take(max_reg as usize + 17 - param_count)
    {
        builder.def_var(*var, unit_const);
    }

    // Compile each block.
    for (block_idx, eir_block) in eir_func.blocks.iter().enumerate() {
        if block_idx > 0 {
            builder.switch_to_block(cl_blocks[block_idx]);
            // Pass block params from predecessor.
            let block_param_count = eir_block.params.len();
            for _i in 0..block_param_count {
                builder.append_block_param(cl_blocks[block_idx], I64);
            }
            // Bind block params to EIR registers.
            for (i, reg) in eir_block.params.iter().enumerate() {
                let val = builder.block_params(cl_blocks[block_idx])[i];
                builder.def_var(vars[reg.0 as usize], val);
            }
            builder.seal_block(cl_blocks[block_idx]);
        }

        // Compile each operation.
        for op in &eir_block.ops {
            compile_op(
                &mut builder,
                &vars,
                op,
                eir_module,
                func_map,
                jit,
                rt_println_id,
                rt_print_id,
                rt_str_id,
            )?;
        }

        // Compile the terminator.
        compile_terminator(
            &mut builder,
            &vars,
            &eir_block.end,
            &cl_blocks,
            eir_func,
            func_map,
            jit,
        )?;
    }

    builder.finalize();
    Ok(())
}

// ─── Op compilation ─────────────────────────────────────────────────────────

#[allow(clippy::too_many_arguments)]
fn compile_op(
    builder: &mut FunctionBuilder,
    vars: &[Variable],
    op: &Op,
    _eir_module: &super::Module,
    func_map: &FuncMap,
    jit: &mut JITModule,
    rt_println_id: cranelift_module::FuncId,
    rt_print_id: cranelift_module::FuncId,
    rt_str_id: cranelift_module::FuncId,
) -> Result<(), Error> {
    match op {
        Op::Lit(dst, lit, _) => {
            let val = match lit {
                Lit::Int(n) => {
                    let nan_boxed = Val::int(*n).bits() as i64;
                    builder.ins().iconst(I64, nan_boxed)
                }
                Lit::Float(f) => {
                    let nan_boxed = Val::float(*f).bits() as i64;
                    builder.ins().iconst(I64, nan_boxed)
                }
                Lit::Bool(b) => {
                    let nan_boxed = if *b { VAL_TRUE } else { VAL_FALSE } as i64;
                    builder.ins().iconst(I64, nan_boxed)
                }
                Lit::Unit => builder.ins().iconst(I64, VAL_UNIT as i64),
                Lit::Str(_sid) => {
                    // TODO: string literals need heap allocation.
                    // For now, return unit as a placeholder.
                    builder.ins().iconst(I64, VAL_UNIT as i64)
                }
                Lit::Keyword(sid) => {
                    let nan_boxed = Val::sym(sid.0).bits() as i64;
                    builder.ins().iconst(I64, nan_boxed)
                }
            };
            builder.def_var(vars[dst.0 as usize], val);
        }

        Op::Mov(dst, src, _) => {
            let val = builder.use_var(vars[src.0 as usize]);
            builder.def_var(vars[dst.0 as usize], val);
        }

        Op::Bin(dst, binop, a, b, _) => {
            let av = builder.use_var(vars[a.0 as usize]);
            let bv = builder.use_var(vars[b.0 as usize]);
            let result = compile_binop(builder, *binop, av, bv);
            builder.def_var(vars[dst.0 as usize], result);
        }

        Op::Un(dst, unop, a, _) => {
            let av = builder.use_var(vars[a.0 as usize]);
            let result = compile_unop(builder, *unop, av);
            builder.def_var(vars[dst.0 as usize], result);
        }

        Op::Call(dst, func_id, args, _) => {
            let cl_func_id = func_map.funcs.get(&func_id.0).ok_or_else(|| Error {
                message: format!("unknown function {}", func_id.0),
                phase: "native:compile",
            })?;
            let func_ref = jit.declare_func_in_func(*cl_func_id, builder.func);
            let arg_vals: Vec<cranelift_codegen::ir::Value> = args
                .iter()
                .map(|r| builder.use_var(vars[r.0 as usize]))
                .collect();
            let call = builder.ins().call(func_ref, &arg_vals);
            let result = builder.inst_results(call)[0];
            builder.def_var(vars[dst.0 as usize], result);
        }

        Op::Builtin(dst, built, args, _) => {
            let result = compile_builtin(
                builder,
                vars,
                *built,
                args,
                jit,
                rt_println_id,
                rt_print_id,
                rt_str_id,
            )?;
            builder.def_var(vars[dst.0 as usize], result);
        }

        // Operations that need heap/runtime support — emit unit placeholder.
        Op::Upval(dst, _, _)
        | Op::Invoke(dst, _, _, _)
        | Op::Close(dst, _, _, _)
        | Op::Vec(dst, _, _)
        | Op::Map(dst, _, _)
        | Op::Set(dst, _, _)
        | Op::Tup(dst, _, _)
        | Op::Adt(dst, _, _, _)
        | Op::Field(dst, _, _, _)
        | Op::Tag(dst, _, _)
        | Op::Perform(dst, _, _, _, _, _)
        | Op::PushHandler(dst, _, _, _) => {
            // TODO: implement via runtime helper calls.
            let unit = builder.ins().iconst(I64, VAL_UNIT as i64);
            builder.def_var(vars[dst.0 as usize], unit);
        }

        Op::PopHandler(_) => {
            // No-op in native backend (no destination register).
        }
    }

    Ok(())
}

// ─── Binary operations ──────────────────────────────────────────────────────

/// Compile a binary operation on NaN-boxed values.
///
/// For now, we specialize only for the int+int fast path (most common in
/// loops). The generated code:
///   1. Extracts the 48-bit payload from both operands
///   2. Performs the arithmetic
///   3. Re-boxes the result with the int tag
///
/// If either operand is not an int, the result is UNIT (a future enhancement
/// would call a runtime helper for mixed-type dispatch).
fn compile_binop(
    builder: &mut FunctionBuilder,
    op: BinOp,
    a: cranelift_codegen::ir::Value,
    b: cranelift_codegen::ir::Value,
) -> cranelift_codegen::ir::Value {
    match op {
        BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Rem => {
            compile_int_arith(builder, op, a, b)
        }
        BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Gt | BinOp::Le | BinOp::Ge => {
            compile_int_cmp(builder, op, a, b)
        }
        BinOp::And => {
            // Eager AND on truthiness.
            let a_truthy = compile_is_truthy(builder, a);
            let b_truthy = compile_is_truthy(builder, b);
            let both = builder.ins().band(a_truthy, b_truthy);
            // Convert i8 boolean back to NaN-boxed bool.
            let true_val = builder.ins().iconst(I64, VAL_TRUE as i64);
            let false_val = builder.ins().iconst(I64, VAL_FALSE as i64);
            builder.ins().select(both, true_val, false_val)
        }
        BinOp::Or => {
            // Eager OR on truthiness.
            let a_truthy = compile_is_truthy(builder, a);
            let b_truthy = compile_is_truthy(builder, b);
            let either = builder.ins().bor(a_truthy, b_truthy);
            let true_val = builder.ins().iconst(I64, VAL_TRUE as i64);
            let false_val = builder.ins().iconst(I64, VAL_FALSE as i64);
            builder.ins().select(either, true_val, false_val)
        }
        BinOp::Concat => {
            // String concat needs runtime support.
            builder.ins().iconst(I64, VAL_UNIT as i64)
        }
    }
}

/// Compile integer arithmetic: extract payloads, compute, re-box.
fn compile_int_arith(
    builder: &mut FunctionBuilder,
    op: BinOp,
    a: cranelift_codegen::ir::Value,
    b: cranelift_codegen::ir::Value,
) -> cranelift_codegen::ir::Value {
    let payload_mask = builder.ins().iconst(I64, PAYLOAD as i64);
    let base_int = builder.ins().iconst(I64, (BASE | TAG_INT) as i64);

    // Extract 48-bit payloads.
    let a_payload = builder.ins().band(a, payload_mask);
    let b_payload = builder.ins().band(b, payload_mask);

    // Sign-extend from 48 bits for correct signed arithmetic.
    let a_ext = sign_extend_48(builder, a_payload);
    let b_ext = sign_extend_48(builder, b_payload);

    // Perform the operation.
    let result = match op {
        BinOp::Add => builder.ins().iadd(a_ext, b_ext),
        BinOp::Sub => builder.ins().isub(a_ext, b_ext),
        BinOp::Mul => builder.ins().imul(a_ext, b_ext),
        BinOp::Div => {
            // Division by zero check: if b is 0, return UNIT.
            let zero = builder.ins().iconst(I64, 0);
            let is_zero = builder.ins().icmp(IntCC::Equal, b_ext, zero);
            let unit = builder.ins().iconst(I64, VAL_UNIT as i64);
            let div_result = builder.ins().sdiv(a_ext, b_ext);
            // Re-box the division result.
            let div_payload = builder.ins().band(div_result, payload_mask);
            let div_boxed = builder.ins().bor(div_payload, base_int);
            return builder.ins().select(is_zero, unit, div_boxed);
        }
        BinOp::Rem => {
            let zero = builder.ins().iconst(I64, 0);
            let is_zero = builder.ins().icmp(IntCC::Equal, b_ext, zero);
            let unit = builder.ins().iconst(I64, VAL_UNIT as i64);
            let rem_result = builder.ins().srem(a_ext, b_ext);
            let rem_payload = builder.ins().band(rem_result, payload_mask);
            let rem_boxed = builder.ins().bor(rem_payload, base_int);
            return builder.ins().select(is_zero, unit, rem_boxed);
        }
        _ => unreachable!(),
    };

    // Re-box: mask to 48 bits and add tag.
    let result_payload = builder.ins().band(result, payload_mask);
    builder.ins().bor(result_payload, base_int)
}

/// Sign-extend a 48-bit value to 64-bit.
fn sign_extend_48(
    builder: &mut FunctionBuilder,
    val: cranelift_codegen::ir::Value,
) -> cranelift_codegen::ir::Value {
    let shifted_left = builder.ins().ishl_imm(val, 16);
    builder.ins().sshr_imm(shifted_left, 16)
}

/// Compile integer comparison, returning a NaN-boxed bool.
fn compile_int_cmp(
    builder: &mut FunctionBuilder,
    op: BinOp,
    a: cranelift_codegen::ir::Value,
    b: cranelift_codegen::ir::Value,
) -> cranelift_codegen::ir::Value {
    // For Eq/Ne: compare raw bits (works for all types).
    // For ordered comparisons: extract and sign-extend int payloads.
    let (cmp_a, cmp_b, cc) = match op {
        BinOp::Eq => (a, b, IntCC::Equal),
        BinOp::Ne => (a, b, IntCC::NotEqual),
        _ => {
            let payload_mask = builder.ins().iconst(I64, PAYLOAD as i64);
            let a_payload = builder.ins().band(a, payload_mask);
            let b_payload = builder.ins().band(b, payload_mask);
            let a_ext = sign_extend_48(builder, a_payload);
            let b_ext = sign_extend_48(builder, b_payload);
            let cc = match op {
                BinOp::Lt => IntCC::SignedLessThan,
                BinOp::Gt => IntCC::SignedGreaterThan,
                BinOp::Le => IntCC::SignedLessThanOrEqual,
                BinOp::Ge => IntCC::SignedGreaterThanOrEqual,
                _ => unreachable!(),
            };
            (a_ext, b_ext, cc)
        }
    };

    let cmp_result = builder.ins().icmp(cc, cmp_a, cmp_b);
    let true_val = builder.ins().iconst(I64, VAL_TRUE as i64);
    let false_val = builder.ins().iconst(I64, VAL_FALSE as i64);
    builder.ins().select(cmp_result, true_val, false_val)
}

/// Check if a NaN-boxed value is truthy (not false and not unit).
fn compile_is_truthy(
    builder: &mut FunctionBuilder,
    val: cranelift_codegen::ir::Value,
) -> cranelift_codegen::ir::Value {
    // The falsy set is exactly {false, (), None}.
    let false_const = builder.ins().iconst(I64, VAL_FALSE as i64);
    let unit_const = builder.ins().iconst(I64, VAL_UNIT as i64);
    let none_const = builder.ins().iconst(I64, VAL_NONE as i64);
    let not_false = builder.ins().icmp(IntCC::NotEqual, val, false_const);
    let not_unit = builder.ins().icmp(IntCC::NotEqual, val, unit_const);
    let not_none = builder.ins().icmp(IntCC::NotEqual, val, none_const);
    let both = builder.ins().band(not_false, not_unit);
    builder.ins().band(both, not_none)
}

// ─── Unary operations ───────────────────────────────────────────────────────

fn compile_unop(
    builder: &mut FunctionBuilder,
    op: UnOp,
    a: cranelift_codegen::ir::Value,
) -> cranelift_codegen::ir::Value {
    match op {
        UnOp::Neg => {
            // Negate integer: extract payload, negate, re-box.
            let payload_mask = builder.ins().iconst(I64, PAYLOAD as i64);
            let base_int = builder.ins().iconst(I64, (BASE | TAG_INT) as i64);
            let a_payload = builder.ins().band(a, payload_mask);
            let a_ext = sign_extend_48(builder, a_payload);
            let negated = builder.ins().ineg(a_ext);
            let result_payload = builder.ins().band(negated, payload_mask);
            builder.ins().bor(result_payload, base_int)
        }
        UnOp::Not => {
            // Logical not: truthy -> false, falsy -> true.
            let is_truthy = compile_is_truthy(builder, a);
            let true_val = builder.ins().iconst(I64, VAL_TRUE as i64);
            let false_val = builder.ins().iconst(I64, VAL_FALSE as i64);
            // NOT truthy → false; NOT falsy → true
            builder.ins().select(is_truthy, false_val, true_val)
        }
    }
}

// ─── Builtin calls ──────────────────────────────────────────────────────────

#[allow(clippy::too_many_arguments)]
fn compile_builtin(
    builder: &mut FunctionBuilder,
    vars: &[Variable],
    built: super::Built,
    args: &[Reg],
    jit: &mut JITModule,
    rt_println_id: cranelift_module::FuncId,
    rt_print_id: cranelift_module::FuncId,
    rt_str_id: cranelift_module::FuncId,
) -> Result<cranelift_codegen::ir::Value, Error> {
    match built {
        super::Built::Println => {
            let func_ref = jit.declare_func_in_func(rt_println_id, builder.func);
            let arg = if args.is_empty() {
                builder.ins().iconst(I64, VAL_UNIT as i64)
            } else {
                builder.use_var(vars[args[0].0 as usize])
            };
            let call = builder.ins().call(func_ref, &[arg]);
            Ok(builder.inst_results(call)[0])
        }
        super::Built::Print => {
            let func_ref = jit.declare_func_in_func(rt_print_id, builder.func);
            let arg = if args.is_empty() {
                builder.ins().iconst(I64, VAL_UNIT as i64)
            } else {
                builder.use_var(vars[args[0].0 as usize])
            };
            let call = builder.ins().call(func_ref, &[arg]);
            Ok(builder.inst_results(call)[0])
        }
        super::Built::Str => {
            let func_ref = jit.declare_func_in_func(rt_str_id, builder.func);
            let arg = if args.is_empty() {
                builder.ins().iconst(I64, VAL_UNIT as i64)
            } else {
                builder.use_var(vars[args[0].0 as usize])
            };
            let call = builder.ins().call(func_ref, &[arg]);
            Ok(builder.inst_results(call)[0])
        }
        super::Built::Not => {
            let arg = if args.is_empty() {
                builder.ins().iconst(I64, VAL_UNIT as i64)
            } else {
                builder.use_var(vars[args[0].0 as usize])
            };
            Ok(compile_unop(builder, UnOp::Not, arg))
        }
        super::Built::Int => {
            // Identity for int conversion (already NaN-boxed).
            if args.is_empty() {
                Ok(builder.ins().iconst(I64, VAL_UNIT as i64))
            } else {
                Ok(builder.use_var(vars[args[0].0 as usize]))
            }
        }
        super::Built::Float => {
            // Identity for float conversion (already NaN-boxed).
            if args.is_empty() {
                Ok(builder.ins().iconst(I64, VAL_UNIT as i64))
            } else {
                Ok(builder.use_var(vars[args[0].0 as usize]))
            }
        }
        // All other builtins: return UNIT (needs runtime support).
        _ => Ok(builder.ins().iconst(I64, VAL_UNIT as i64)),
    }
}

// ─── Terminator compilation ─────────────────────────────────────────────────

fn compile_terminator(
    builder: &mut FunctionBuilder,
    vars: &[Variable],
    end: &End,
    cl_blocks: &[cranelift_codegen::ir::Block],
    _eir_func: &super::Func,
    func_map: &FuncMap,
    jit: &mut JITModule,
) -> Result<(), Error> {
    match end {
        End::Ret(reg) => {
            let val = builder.use_var(vars[reg.0 as usize]);
            builder.ins().return_(&[val]);
        }

        End::Jmp(target, args) => {
            let vals: Vec<cranelift_codegen::ir::Value> = args
                .iter()
                .map(|r| builder.use_var(vars[r.0 as usize]))
                .collect();
            builder.ins().jump(cl_blocks[target.0 as usize], &vals);
        }

        End::Br(cond, then_b, else_b) => {
            let cond_val = builder.use_var(vars[cond.0 as usize]);
            // Check truthiness: not false and not unit.
            let is_truthy = compile_is_truthy(builder, cond_val);
            builder.ins().brif(
                is_truthy,
                cl_blocks[then_b.0 as usize],
                &[],
                cl_blocks[else_b.0 as usize],
                &[],
            );
        }

        End::Switch(scrutinee, cases, default) => {
            // For now, implement as a chain of if-else comparisons.
            // A proper switch would use Cranelift's br_table.
            let val = builder.use_var(vars[scrutinee.0 as usize]);

            if cases.is_empty() {
                builder.ins().jump(cl_blocks[default.0 as usize], &[]);
            } else {
                // Create intermediate blocks for the if-else chain.
                let mut next_blocks = Vec::new();
                for _ in 0..cases.len() {
                    next_blocks.push(builder.create_block());
                }

                for (i, (tag, target)) in cases.iter().enumerate() {
                    let tag_val = builder.ins().iconst(I64, *tag as i64);
                    // Extract tag from value — for simplicity compare raw value.
                    // In practice we'd extract the ADT tag.
                    let cmp = builder.ins().icmp(IntCC::Equal, val, tag_val);

                    let fallthrough = if i + 1 < cases.len() {
                        next_blocks[i + 1]
                    } else {
                        cl_blocks[default.0 as usize]
                    };

                    builder
                        .ins()
                        .brif(cmp, cl_blocks[target.0 as usize], &[], fallthrough, &[]);

                    if i + 1 < cases.len() {
                        builder.switch_to_block(next_blocks[i + 1]);
                        builder.seal_block(next_blocks[i + 1]);
                    }
                }

                // Seal all intermediate blocks.
                // The first intermediate block was already used.
                builder.seal_block(next_blocks[0]);
            }
        }

        End::Tail(func_id, args) => {
            // A real tail call: `return_call` replaces the current frame, so
            // mutual tail recursion runs in constant stack. This is why loon
            // functions are compiled with `CallConv::Tail`.
            let cl_func_id = func_map.funcs.get(&func_id.0).ok_or_else(|| Error {
                message: format!("unknown tail call target {}", func_id.0),
                phase: "native:compile",
            })?;
            let func_ref = jit.declare_func_in_func(*cl_func_id, builder.func);
            let arg_vals: Vec<cranelift_codegen::ir::Value> = args
                .iter()
                .map(|r| builder.use_var(vars[r.0 as usize]))
                .collect();
            builder.ins().return_call(func_ref, &arg_vals);
        }

        End::TailInvoke(_callee, _args) => {
            // Indirect tail calls need closures, which this backend does not
            // represent yet (`Op::Close`/`Op::Invoke` are still stubs). Fail
            // loudly rather than returning Unit — a silent wrong answer is far
            // worse than a missing feature.
            return Err(Error {
                message: "tail call to a closure is not supported by the native backend yet"
                    .to_string(),
                phase: "native:compile",
            });
        }

        End::Recur(args) => {
            // Self-recursion: jump back to block 0 with new params.
            let vals: Vec<cranelift_codegen::ir::Value> = args
                .iter()
                .map(|r| builder.use_var(vars[r.0 as usize]))
                .collect();
            builder.ins().jump(cl_blocks[0], &vals);
        }

        End::Trap => {
            builder
                .ins()
                .trap(cranelift_codegen::ir::TrapCode::unwrap_user(0));
        }
    }

    Ok(())
}

// ─── Public API ─────────────────────────────────────────────────────────────

/// Compile and run source code via the native backend.
pub fn eval_native(src: &str) -> Result<Val, String> {
    let exprs = crate::parser::parse(src).map_err(|e| e.to_string())?;
    let mut checker = crate::check::Checker::new();
    let _errors = checker.check_program(&exprs);
    let module = crate::eir::lower::lower(&checker);

    let mut backend = NativeBackend;
    let native_module = backend
        .compile(&module)
        .map_err(|e| format!("native compilation error: {e}"))?;

    Ok(native_module.execute())
}

// ─── Tests ──────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn compile_int_literal() {
        let result = eval_native("42").unwrap();
        assert!(result.is_int());
        assert_eq!(result.as_int(), 42);
    }

    #[test]
    fn compile_addition() {
        let result = eval_native("[+ 1 2]").unwrap();
        assert!(result.is_int());
        assert_eq!(result.as_int(), 3);
    }

    #[test]
    fn compile_nested_arithmetic() {
        let result = eval_native("[* [+ 2 3] [- 10 4]]").unwrap();
        assert!(result.is_int());
        assert_eq!(result.as_int(), 30);
    }

    #[test]
    fn compile_comparison() {
        let result = eval_native("[< 1 2]").unwrap();
        assert_eq!(result, Val::TRUE);

        let result = eval_native("[> 1 2]").unwrap();
        assert_eq!(result, Val::FALSE);

        let result = eval_native("[= 5 5]").unwrap();
        assert_eq!(result, Val::TRUE);
    }

    #[test]
    fn compile_bool_literal() {
        let result = eval_native("true").unwrap();
        assert_eq!(result, Val::TRUE);

        let result = eval_native("false").unwrap();
        assert_eq!(result, Val::FALSE);
    }

    #[test]
    fn compile_negation() {
        let result = eval_native("[- 0 42]").unwrap();
        assert!(result.is_int());
        assert_eq!(result.as_int(), -42);
    }

    #[test]
    fn compile_not() {
        let result = eval_native("[not true]").unwrap();
        assert_eq!(result, Val::FALSE);

        let result = eval_native("[not false]").unwrap();
        assert_eq!(result, Val::TRUE);
    }

    #[test]
    fn compile_if_expression() {
        let result = eval_native("[if true 1 2]").unwrap();
        assert!(result.is_int());
        assert_eq!(result.as_int(), 1);

        let result = eval_native("[if false 1 2]").unwrap();
        assert!(result.is_int());
        assert_eq!(result.as_int(), 2);
    }

    #[test]
    fn compile_let_binding() {
        let result = eval_native("[do [let x 10] [+ x 5]]").unwrap();
        assert!(result.is_int());
        assert_eq!(result.as_int(), 15);
    }

    #[test]
    fn compile_function_call() {
        let src = r#"
            [fn double [x] [* x 2]]
            [double 21]
        "#;
        let result = eval_native(src).unwrap();
        assert!(result.is_int());
        assert_eq!(result.as_int(), 42);
    }

    #[test]
    fn compile_recursive_function() {
        let src = r#"
            [fn fact [n]
                [if [<= n 1]
                    1
                    [* n [fact [- n 1]]]]]
            [fact 5]
        "#;
        let result = eval_native(src).unwrap();
        assert!(result.is_int());
        assert_eq!(result.as_int(), 120);
    }

    /// Mutual tail recursion from source must run in constant stack. Compiled
    /// as a plain call + return this recurses a million frames deep and
    /// overflows, so reaching the assert is the evidence that `End::Tail` is
    /// both emitted by the lowering and lowered to `return_call` here.
    #[test]
    fn tail_calls_run_in_constant_stack_from_source() {
        let src = r#"
            [fn even? [n] [if [= n 0] true [odd? [- n 1]]]]
            [fn odd? [n] [if [= n 0] false [even? [- n 1]]]]
            [even? 1000000]
        "#;
        assert_eq!(eval_native(src).unwrap(), Val::TRUE);
    }

    /// The same property stated directly against `End::Tail`, independent of
    /// what the lowering happens to produce.
    #[test]
    fn tail_calls_run_in_constant_stack() {
        use crate::eir::{Block, BlockId, Func, FuncId, Ty};
        use crate::syntax::Span;

        // `even(n) = n == 0 ? true : odd(n - 1)`, and vice versa.
        let parity = |id: u32, other: u32, base: bool| Func {
            id: FuncId(id),
            name: Some(format!("parity{id}")),
            params: vec![Ty::Int],
            ret: Ty::Bool,
            evidence: vec![],
            captures: vec![],
            blocks: vec![
                Block {
                    id: BlockId(0),
                    params: vec![Reg(0)],
                    ops: vec![
                        Op::Lit(Reg(1), Lit::Int(0), Span::ZERO),
                        Op::Bin(Reg(2), BinOp::Eq, Reg(0), Reg(1), Span::ZERO),
                    ],
                    end: End::Br(Reg(2), BlockId(1), BlockId(2)),
                },
                Block {
                    id: BlockId(1),
                    params: vec![],
                    ops: vec![Op::Lit(Reg(3), Lit::Bool(base), Span::ZERO)],
                    end: End::Ret(Reg(3)),
                },
                Block {
                    id: BlockId(2),
                    params: vec![],
                    ops: vec![
                        Op::Lit(Reg(4), Lit::Int(1), Span::ZERO),
                        Op::Bin(Reg(5), BinOp::Sub, Reg(0), Reg(4), Span::ZERO),
                    ],
                    end: End::Tail(FuncId(other), vec![Reg(5)]),
                },
            ],
            span: Span::ZERO,
            is_closure: false,
        };

        let module = crate::eir::Module {
            funcs: vec![
                parity(0, 1, true),
                parity(1, 0, false),
                Func {
                    id: FuncId(2),
                    name: Some("__main".to_string()),
                    params: vec![],
                    ret: Ty::Bool,
                    evidence: vec![],
                    captures: vec![],
                    blocks: vec![Block {
                        id: BlockId(0),
                        params: vec![],
                        ops: vec![
                            Op::Lit(Reg(0), Lit::Int(1_000_000), Span::ZERO),
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
            entry: FuncId(2),
        };

        let mut backend = NativeBackend;
        let native = backend.compile(&module).expect("compilation failed");
        assert_eq!(native.execute(), Val::TRUE);
    }

    /// A tail call to a closure has no lowering yet. It must fail loudly —
    /// it used to return Unit without performing the call at all.
    #[test]
    fn tail_invoke_is_an_error_not_a_silent_unit() {
        use crate::eir::{Block, BlockId, Func, FuncId, Ty};
        use crate::syntax::Span;

        let module = crate::eir::Module {
            funcs: vec![Func {
                id: FuncId(0),
                name: Some("__main".to_string()),
                params: vec![],
                ret: Ty::Any,
                evidence: vec![],
                captures: vec![],
                blocks: vec![Block {
                    id: BlockId(0),
                    params: vec![],
                    ops: vec![Op::Lit(Reg(0), Lit::Int(1), Span::ZERO)],
                    end: End::TailInvoke(Reg(0), vec![Reg(0)]),
                }],
                span: Span::ZERO,
                is_closure: false,
            }],
            strings: vec![],
            ctors: vec![],
            entry: FuncId(0),
        };

        match NativeBackend.compile(&module) {
            Ok(_) => panic!("tail invoke should not compile silently"),
            Err(e) => assert!(e.message.contains("closure"), "unexpected error: {e}"),
        }
    }

    #[test]
    fn compile_division() {
        let result = eval_native("[/ 10 3]").unwrap();
        assert!(result.is_int());
        assert_eq!(result.as_int(), 3);
    }

    #[test]
    fn compile_remainder() {
        let result = eval_native("[% 10 3]").unwrap();
        assert!(result.is_int());
        assert_eq!(result.as_int(), 1);
    }

    #[test]
    fn compile_unit() {
        let result = eval_native("()").unwrap();
        assert_eq!(result, Val::UNIT);
    }
}
