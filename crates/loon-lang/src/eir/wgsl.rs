//! Kernels, as WGSL compute shaders.
//!
//! This is the part of placement that is a compiler rather than a runtime. An
//! EIR kernel function becomes a `@compute` entry point that a GPU can run:
//! buffers become storage bindings, scalars become uniform fields, and the
//! kernel's work index becomes `global_invocation_id.x`.
//!
//! Two decisions shape the whole file.
//!
//! **Specialize per launch.** A kernel is emitted for a *concrete* argument
//! signature — which arguments are buffers, of which element type, and which
//! are scalars — rather than once for all possible uses. That is the same
//! monomorphization a Rust GPU compiler performs, and it is what lets Loon
//! stay a language with inferred types while still producing a shader whose
//! every binding has a definite type.
//!
//! **No relooper.** WGSL has no `goto`, and EIR is a graph of basic blocks.
//! Rather than restructure the graph, the emitter does what the WASM backend
//! already does: a `loop` around a `switch` on a block index, with each block
//! ending by assigning the next index and continuing. Structured control flow
//! falls out of the trivial case — a single-block kernel emits as straight
//! line code with no dispatch at all — and everything else stays correct
//! without a restructuring pass to get wrong.
//!
//! What cannot be emitted is refused by name. A kernel is already restricted
//! (see `check::kernel`) to the numeric subset this file covers, so reaching a
//! rejection here means the two definitions of "kernel" have drifted apart,
//! and saying which operation was unsupported is more useful than a shader
//! that silently computes something else.

use super::layout::DType;
use super::{BinOp, Block, Built, End, FuncId, Lit, Module, Op, Reg, UnOp};
use std::collections::HashMap;
use std::fmt::Write as _;

/// What a kernel argument turns out to be at a particular launch.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArgKind {
    /// A dense buffer, bound as a storage array.
    Buffer { dtype: DType, writable: bool },
    /// A single number, passed in the uniform block.
    Scalar(DType),
}

impl ArgKind {
    /// A read-only buffer of `dtype`.
    pub fn input(dtype: DType) -> ArgKind {
        ArgKind::Buffer {
            dtype,
            writable: false,
        }
    }

    /// A buffer the kernel writes through.
    pub fn output(dtype: DType) -> ArgKind {
        ArgKind::Buffer {
            dtype,
            writable: true,
        }
    }
}

/// Why a kernel could not be emitted.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Error(pub String);

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for Error {}

fn err<T>(msg: impl Into<String>) -> Result<T, Error> {
    Err(Error(msg.into()))
}

/// The WGSL type of a value inside a kernel.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Ty {
    F32,
    I32,
    Bool,
    /// Produced by `put`, which is a statement rather than an expression.
    Unit,
}

impl Ty {
    fn wgsl(self) -> &'static str {
        match self {
            Ty::F32 => "f32",
            Ty::I32 => "i32",
            Ty::Bool => "bool",
            Ty::Unit => "i32", // never read; kept declarable
        }
    }

    fn of(dtype: DType) -> Ty {
        match dtype {
            DType::F32 | DType::F64 => Ty::F32,
            DType::I32 | DType::I64 => Ty::I32,
        }
    }

    /// The type an arithmetic result takes when the operands disagree.
    ///
    /// WGSL will not mix `i32` and `f32` in one expression, so the emitter has
    /// to pick, and picking float is the choice that does not silently discard
    /// a fractional part.
    fn join(a: Ty, b: Ty) -> Ty {
        match (a, b) {
            (Ty::F32, _) | (_, Ty::F32) => Ty::F32,
            (Ty::I32, _) | (_, Ty::I32) => Ty::I32,
            _ => Ty::Bool,
        }
    }
}

/// Work out each parameter's shape from how the kernel body uses it.
///
/// A parameter that is indexed (`at`, `put`) or measured (`buf-len`) is a
/// buffer; one that is only computed with is a scalar. A buffer written
/// through is bound `read_write`, and one only read is bound `read`.
///
/// This is the same question the ownership pass answers for transfer
/// direction, asked of the lowered code: the kernel's own body says what its
/// arguments are, so a launch does not have to be told.
pub fn infer_arg_kinds(module: &Module, func: FuncId, dtype: DType) -> Vec<ArgKind> {
    let Some(f) = module.funcs.get(func.0 as usize) else {
        return Vec::new();
    };
    let arity = f.params.len().saturating_sub(1);
    let mut is_buffer = vec![false; arity];
    let mut written = vec![false; arity];

    // Parameters occupy registers 0..n, the work index first.
    let param_slot = |r: Reg| -> Option<usize> {
        if r.0 >= 1 && (r.0 as usize) <= arity {
            Some(r.0 as usize - 1)
        } else {
            None
        }
    };

    for block in &f.blocks {
        for op in &block.ops {
            if let Op::Builtin(_, built, args, _) = op {
                let target = args.first().copied().and_then(param_slot);
                match (built, target) {
                    (Built::BufAt | Built::BufLen, Some(i)) => is_buffer[i] = true,
                    (Built::BufPut, Some(i)) => {
                        is_buffer[i] = true;
                        written[i] = true;
                    }
                    _ => {}
                }
            }
        }
    }

    (0..arity)
        .map(|i| {
            if is_buffer[i] {
                ArgKind::Buffer {
                    dtype,
                    writable: written[i],
                }
            } else {
                ArgKind::Scalar(dtype)
            }
        })
        .collect()
}

/// Emit a WGSL compute shader for `func`, specialized to `args`.
///
/// `args` describes the kernel's parameters *after* the leading work index,
/// which is always supplied by the dispatch itself.
pub fn emit(module: &Module, func: FuncId, args: &[ArgKind]) -> Result<String, Error> {
    let f = module
        .funcs
        .get(func.0 as usize)
        .ok_or_else(|| Error(format!("no function {func:?}")))?;

    if f.params.len() != args.len() + 1 {
        return err(format!(
            "kernel '{}' takes {} parameters, but {} arguments were given \
             (the work index is supplied by the dispatch)",
            f.name.as_deref().unwrap_or("<anonymous>"),
            f.params.len(),
            args.len() + 1
        ));
    }

    let mut e = Emitter {
        module,
        args,
        types: HashMap::new(),
        buffers: HashMap::new(),
        scalars: HashMap::new(),
    };
    e.bind_params();
    e.check_buffer_uses(&f.blocks)?;
    e.infer(&f.blocks)?;

    let name = f.name.as_deref().unwrap_or("kernel");
    let mut out = String::new();
    e.header(&mut out, name);

    writeln!(out, "@compute @workgroup_size(64)").unwrap();
    writeln!(
        out,
        "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {{"
    )
    .unwrap();
    writeln!(out, "    let idx: i32 = i32(gid.x);").unwrap();
    writeln!(out, "    if (idx >= params.n) {{ return; }}").unwrap();

    e.declare_registers(&mut out, &f.blocks);
    writeln!(out, "    var r{}: i32 = idx;", 0).unwrap();

    if f.blocks.len() == 1 {
        // The common case: no control flow, so no dispatch machinery.
        e.emit_block(&mut out, &f.blocks, &f.blocks[0], "    ", false)?;
    } else {
        writeln!(out, "    var blk: i32 = 0;").unwrap();
        writeln!(out, "    loop {{").unwrap();
        writeln!(out, "        switch blk {{").unwrap();
        for block in &f.blocks {
            writeln!(out, "            case {}: {{", block.id.0).unwrap();
            e.emit_block(&mut out, &f.blocks, block, "                ", true)?;
            writeln!(out, "            }}").unwrap();
        }
        writeln!(out, "            default: {{ return; }}").unwrap();
        writeln!(out, "        }}").unwrap();
        writeln!(out, "    }}").unwrap();
    }

    writeln!(out, "}}").unwrap();
    Ok(out)
}

struct Emitter<'a> {
    module: &'a Module,
    args: &'a [ArgKind],
    /// Inferred type of each register.
    types: HashMap<u32, Ty>,
    /// Register → binding index, for buffer parameters.
    buffers: HashMap<u32, usize>,
    /// Register → uniform field name, for scalar parameters.
    scalars: HashMap<u32, String>,
}

impl Emitter<'_> {
    /// Parameters occupy registers 0..n in order, the index first.
    fn bind_params(&mut self) {
        self.types.insert(0, Ty::I32);
        for (i, arg) in self.args.iter().enumerate() {
            let reg = (i + 1) as u32;
            match arg {
                ArgKind::Buffer { dtype, .. } => {
                    self.buffers.insert(reg, i);
                    self.types.insert(reg, Ty::of(*dtype));
                }
                ArgKind::Scalar(dtype) => {
                    self.scalars.insert(reg, format!("params.s{i}"));
                    self.types.insert(reg, Ty::of(*dtype));
                }
            }
        }
    }

    /// Refuse a launch signature that disagrees with the kernel body.
    ///
    /// A buffer is a binding, not a value: there is no WGSL expression that
    /// denotes one. So a kernel that multiplies by an argument the launch
    /// declared to be a buffer cannot be compiled, and saying which argument
    /// is far more useful than emitting a shader that names an identifier
    /// nothing declared. This is the check that turns a mismatch between a
    /// kernel and its call site into a sentence rather than a driver error.
    fn check_buffer_uses(&self, blocks: &[Block]) -> Result<(), Error> {
        for block in blocks {
            for op in &block.ops {
                let value_reads: Vec<Reg> = match op {
                    // The first argument of these is a binding, by design.
                    Op::Builtin(_, Built::BufAt, a, _) => a.iter().skip(1).copied().collect(),
                    Op::Builtin(_, Built::BufPut, a, _) => a.iter().skip(1).copied().collect(),
                    Op::Builtin(_, Built::BufLen, _, _) => Vec::new(),
                    Op::Builtin(_, _, a, _) => a.clone(),
                    Op::Mov(_, s, _) => vec![*s],
                    Op::Bin(_, _, a, b, _) => vec![*a, *b],
                    Op::Un(_, _, a, _) => vec![*a],
                    _ => Vec::new(),
                };
                for r in value_reads {
                    if let Some(binding) = self.buffers.get(&r.0) {
                        return err(format!(
                            "argument {} was given as a buffer, but the kernel uses it as a number; a buffer can only be indexed (`at`, `put`) or measured (`buf-len`)",
                            binding + 1
                        ));
                    }
                }
            }
        }
        Ok(())
    }

    fn header(&self, out: &mut String, name: &str) {
        writeln!(out, "// kernel '{name}', specialized for this launch").unwrap();
        writeln!(out, "struct Params {{").unwrap();
        writeln!(out, "    n: i32,").unwrap();
        for (i, arg) in self.args.iter().enumerate() {
            if let ArgKind::Scalar(dtype) = arg {
                writeln!(out, "    s{}: {},", i, Ty::of(*dtype).wgsl()).unwrap();
            }
        }
        writeln!(out, "}}").unwrap();
        writeln!(out, "@group(0) @binding(0) var<uniform> params: Params;").unwrap();

        // Bindings are numbered contiguously from 1 (0 is the uniform block),
        // skipping scalar arguments, so the layout a host builds matches the
        // shader without a gap to reason about.
        let mut binding = 1;
        for (i, arg) in self.args.iter().enumerate() {
            if let ArgKind::Buffer { dtype, writable } = arg {
                let access = if *writable { "read_write" } else { "read" };
                writeln!(
                    out,
                    "@group(0) @binding({binding}) var<storage, {access}> b{}: array<{}>;",
                    i,
                    Ty::of(*dtype).wgsl()
                )
                .unwrap();
                binding += 1;
            }
        }
        writeln!(out).unwrap();
    }

    /// Walk the blocks assigning a type to every register.
    ///
    /// Repeats until stable so a register defined in a later block — a loop
    /// back-edge carrying a value — still gets a type before it is used.
    fn infer(&mut self, blocks: &[Block]) -> Result<(), Error> {
        // Iterate until nothing changes, not merely until nothing new appears.
        //
        // A loop-carried value is the case that makes the difference. Its type
        // is set by whichever predecessor is visited first, and a later one may
        // demand a wider type; stopping as soon as every register *had* a type
        // left the earlier, narrower answer in place. The result was a shader
        // that assigned an i32 register to an f32 one and failed validation —
        // which is at least a loud failure, but only because something checks.
        let limit = blocks.len().max(1) * 4 + 4;
        for _ in 0..limit {
            let snapshot = self.types.clone();
            for block in blocks {
                for op in &block.ops {
                    self.infer_op(op)?;
                }
                // Block parameters take the widest type any predecessor passes.
                if let End::Jmp(_, args) = &block.end {
                    // A jump's arguments land in the *target* block's params.
                    if let Some(target) = self.block_by_id(blocks, block) {
                        for (i, a) in args.iter().enumerate() {
                            if let (Some(t), Some(p)) = (self.ty_opt(*a), target.params.get(i)) {
                                self.widen(p.0, t);
                            }
                        }
                    }
                }
                if let End::Recur(args) = &block.end {
                    // `recur` re-enters the function's entry block.
                    if let (Some(entry), true) = (blocks.first(), true) {
                        for (i, a) in args.iter().enumerate() {
                            if let (Some(t), Some(p)) = (self.ty_opt(*a), entry.params.get(i)) {
                                self.widen(p.0, t);
                            }
                        }
                    }
                }
            }
            if self.types == snapshot {
                break;
            }
        }
        Ok(())
    }

    /// The block a terminator jumps to.
    fn block_by_id<'b>(&self, blocks: &'b [Block], from: &Block) -> Option<&'b Block> {
        let End::Jmp(target, _) = &from.end else {
            return None;
        };
        blocks.iter().find(|b| b.id == *target)
    }

    /// Record a register's type, widening rather than replacing.
    ///
    /// WGSL will not mix i32 and f32, so when predecessors disagree the wider
    /// type has to win everywhere — otherwise the declaration and the
    /// assignments describe different types.
    fn widen(&mut self, reg: u32, t: Ty) {
        let merged = match self.types.get(&reg).copied() {
            Some(existing) if existing != t => Ty::join(existing, t),
            Some(existing) => existing,
            None => t,
        };
        self.types.insert(reg, merged);
    }

    fn infer_op(&mut self, op: &Op) -> Result<(), Error> {
        match op {
            Op::Lit(d, lit, _) => {
                let t = match lit {
                    Lit::Int(_) => Ty::I32,
                    Lit::Float(_) => Ty::F32,
                    Lit::Bool(_) => Ty::Bool,
                    Lit::Unit => Ty::Unit,
                    Lit::Str(_) | Lit::Keyword(_) => {
                        return err("a kernel cannot use strings or keywords")
                    }
                };
                self.types.insert(d.0, t);
            }
            Op::Mov(d, s, _) => {
                if let Some(t) = self.ty_opt(*s) {
                    self.widen(d.0, t);
                }
            }
            Op::Bin(d, bop, a, b, _) => {
                let t = match bop {
                    BinOp::Eq
                    | BinOp::Ne
                    | BinOp::Lt
                    | BinOp::Gt
                    | BinOp::Le
                    | BinOp::Ge
                    | BinOp::And
                    | BinOp::Or => Ty::Bool,
                    BinOp::Concat => return err("a kernel cannot concatenate strings"),
                    _ => Ty::join(
                        self.ty_opt(*a).unwrap_or(Ty::F32),
                        self.ty_opt(*b).unwrap_or(Ty::F32),
                    ),
                };
                self.types.insert(d.0, t);
            }
            Op::Un(d, uop, a, _) => {
                let t = match uop {
                    UnOp::Neg => self.ty_opt(*a).unwrap_or(Ty::F32),
                    UnOp::Not => Ty::Bool,
                };
                self.types.insert(d.0, t);
            }
            Op::Builtin(d, built, args, _) => {
                let t = self.builtin_type(*built, args)?;
                self.types.insert(d.0, t);
            }
            Op::Call(d, f, _, _) => {
                // A kernel calling a kernel: its result type is whatever that
                // kernel's own body produces. Kernels return unit today, so
                // this is only reached for effectful helpers.
                let _ = f;
                self.types.insert(d.0, Ty::Unit);
            }
            other => return err(format!("{} cannot run on a GPU", describe(other))),
        }
        Ok(())
    }

    fn builtin_type(&self, built: Built, args: &[Reg]) -> Result<Ty, Error> {
        Ok(match built {
            Built::BufAt => {
                let buf = args.first().copied().unwrap_or(Reg(0));
                self.ty_opt(buf).unwrap_or(Ty::F32)
            }
            Built::BufPut => Ty::Unit,
            Built::BufLen => Ty::I32,
            Built::Int => Ty::I32,
            Built::Float => Ty::F32,
            Built::Not => Ty::Bool,
            Built::Sqrt
            | Built::Pow
            | Built::Floor
            | Built::Ceil
            | Built::Round
            | Built::Sin
            | Built::Cos
            | Built::Tan
            | Built::Asin
            | Built::Acos
            | Built::Atan
            | Built::Atan2
            | Built::Log
            | Built::Log10
            | Built::Exp => Ty::F32,
            Built::Abs | Built::Min | Built::Max => {
                let a = args
                    .first()
                    .and_then(|r| self.ty_opt(*r))
                    .unwrap_or(Ty::F32);
                let b = args.get(1).and_then(|r| self.ty_opt(*r)).unwrap_or(a);
                Ty::join(a, b)
            }
            other => return err(format!("builtin {other:?} has no GPU equivalent")),
        })
    }

    fn ty_opt(&self, r: Reg) -> Option<Ty> {
        self.types.get(&r.0).copied()
    }

    fn ty(&self, r: Reg) -> Ty {
        self.ty_opt(r).unwrap_or(Ty::F32)
    }

    /// How a register is read in an expression.
    fn read(&self, r: Reg) -> String {
        if let Some(name) = self.scalars.get(&r.0) {
            name.clone()
        } else {
            format!("r{}", r.0)
        }
    }

    fn declare_registers(&self, out: &mut String, blocks: &[Block]) {
        let mut seen: Vec<u32> = Vec::new();
        for block in blocks {
            for p in &block.params {
                seen.push(p.0);
            }
            for op in &block.ops {
                if let Some(d) = dest(op) {
                    seen.push(d.0);
                }
            }
        }
        seen.sort_unstable();
        seen.dedup();
        for r in seen {
            // Parameters already exist: buffers as bindings, scalars as
            // uniform fields, the index as r0.
            if r == 0 || self.buffers.contains_key(&r) || self.scalars.contains_key(&r) {
                continue;
            }
            let t = self.types.get(&r).copied().unwrap_or(Ty::F32);
            let init = match t {
                Ty::F32 => "0.0",
                Ty::I32 | Ty::Unit => "0",
                Ty::Bool => "false",
            };
            writeln!(out, "    var r{}: {} = {};", r, t.wgsl(), init).unwrap();
        }
    }

    fn emit_block(
        &self,
        out: &mut String,
        blocks: &[Block],
        block: &Block,
        pad: &str,
        dispatched: bool,
    ) -> Result<(), Error> {
        for op in &block.ops {
            self.emit_op(out, op, pad)?;
        }
        match &block.end {
            End::Ret(_) => {
                writeln!(out, "{pad}return;").unwrap();
            }
            End::Trap => {
                // A kernel has no way to report a trap, and inventing a value
                // would be worse than stopping.
                writeln!(out, "{pad}return;").unwrap();
            }
            End::Jmp(target, args) => {
                self.emit_branch_args(out, blocks, *target, args, pad);
                if dispatched {
                    writeln!(out, "{pad}blk = {}; continue;", target.0).unwrap();
                } else {
                    return err("a jump needs the block dispatcher");
                }
            }
            End::Br(cond, t, f) => {
                if !dispatched {
                    return err("a branch needs the block dispatcher");
                }
                writeln!(
                    out,
                    "{pad}if ({}) {{ blk = {}; }} else {{ blk = {}; }}",
                    self.read_as(*cond, Ty::Bool),
                    t.0,
                    f.0
                )
                .unwrap();
                writeln!(out, "{pad}continue;").unwrap();
            }
            End::Switch(scrutinee, arms, default) => {
                if !dispatched {
                    return err("a switch needs the block dispatcher");
                }
                writeln!(out, "{pad}switch {} {{", self.read_as(*scrutinee, Ty::I32)).unwrap();
                for (value, target) in arms {
                    writeln!(out, "{pad}    case {value}: {{ blk = {}; }}", target.0).unwrap();
                }
                writeln!(out, "{pad}    default: {{ blk = {}; }}", default.0).unwrap();
                writeln!(out, "{pad}}}").unwrap();
                writeln!(out, "{pad}continue;").unwrap();
            }
            End::Recur(args) => {
                if !dispatched {
                    return err("a loop needs the block dispatcher");
                }
                self.emit_branch_args(out, blocks, block.id, args, pad);
                writeln!(out, "{pad}blk = {}; continue;", block.id.0).unwrap();
            }
            End::Tail(..) | End::TailInvoke(..) => {
                return err("tail calls have no GPU equivalent");
            }
        }
        Ok(())
    }

    /// Copy branch arguments into the target block's parameter registers.
    fn emit_branch_args(
        &self,
        out: &mut String,
        blocks: &[Block],
        target: super::BlockId,
        args: &[Reg],
        pad: &str,
    ) {
        // Within this function's blocks only. `BlockId` is per-function, so
        // searching the whole module found whichever function happened to have
        // a block with the same number — and copied jump arguments into its
        // parameters, with its types. The shader that came out assigned an i32
        // register to an f32 one.
        let Some(block) = blocks.iter().find(|b| b.id == target) else {
            return;
        };
        for (p, a) in block.params.iter().zip(args.iter()) {
            if p.0 != a.0 {
                writeln!(out, "{pad}r{} = {};", p.0, self.read_as(*a, self.ty(*p))).unwrap();
            }
        }
    }

    /// Read a register, converting if the context needs another type.
    fn read_as(&self, r: Reg, want: Ty) -> String {
        let have = self.ty(r);
        let text = self.read(r);
        if have == want || want == Ty::Unit {
            return text;
        }
        match (have, want) {
            (Ty::I32, Ty::F32) => format!("f32({text})"),
            (Ty::F32, Ty::I32) => format!("i32({text})"),
            (Ty::Bool, Ty::I32) => format!("select(0, 1, {text})"),
            (Ty::Bool, Ty::F32) => format!("select(0.0, 1.0, {text})"),
            (Ty::I32, Ty::Bool) => format!("({text} != 0)"),
            (Ty::F32, Ty::Bool) => format!("({text} != 0.0)"),
            // A unit value reaching a numeric slot is zero, which is how the
            // CPU executor already reads it. It happens on a control-flow path
            // that produces no value — the fallthrough of a loop, say — and
            // leaving it unconverted emitted a bare i32 register where WGSL
            // wanted an f32, which is a shader that does not compile.
            (Ty::Unit, Ty::F32) => format!("f32({text})"),
            (Ty::Unit, Ty::Bool) => format!("({text} != 0)"),
            _ => text,
        }
    }

    fn emit_op(&self, out: &mut String, op: &Op, pad: &str) -> Result<(), Error> {
        match op {
            Op::Lit(d, lit, _) => {
                let v = match lit {
                    Lit::Int(n) => format!("{n}"),
                    Lit::Float(f) => format_float(*f),
                    Lit::Bool(b) => format!("{b}"),
                    Lit::Unit => "0".to_string(),
                    Lit::Str(_) | Lit::Keyword(_) => {
                        return err("a kernel cannot use strings or keywords")
                    }
                };
                writeln!(out, "{pad}r{} = {v};", d.0).unwrap();
            }
            Op::Mov(d, s, _) => {
                writeln!(out, "{pad}r{} = {};", d.0, self.read_as(*s, self.ty(*d))).unwrap();
            }
            Op::Bin(d, bop, a, b, _) => {
                let text = self.binop(*bop, *a, *b)?;
                writeln!(out, "{pad}r{} = {text};", d.0).unwrap();
            }
            Op::Un(d, uop, a, _) => {
                let text = match uop {
                    UnOp::Neg => format!("-({})", self.read_as(*a, self.ty(*d))),
                    UnOp::Not => format!("!({})", self.read_as(*a, Ty::Bool)),
                };
                writeln!(out, "{pad}r{} = {text};", d.0).unwrap();
            }
            Op::Builtin(d, built, args, _) => self.emit_builtin(out, *d, *built, args, pad)?,
            other => return err(format!("{} cannot run on a GPU", describe(other))),
        }
        Ok(())
    }

    fn binop(&self, bop: BinOp, a: Reg, b: Reg) -> Result<String, Error> {
        let joined = Ty::join(self.ty(a), self.ty(b));
        let (x, y) = (self.read_as(a, joined), self.read_as(b, joined));
        Ok(match bop {
            BinOp::Add => format!("({x} + {y})"),
            BinOp::Sub => format!("({x} - {y})"),
            BinOp::Mul => format!("({x} * {y})"),
            BinOp::Div => format!("({x} / {y})"),
            BinOp::Rem => {
                if joined == Ty::F32 {
                    // WGSL's `%` is defined for floats, but naming the intent
                    // keeps the emitted shader readable.
                    format!("({x} % {y})")
                } else {
                    format!("({x} % {y})")
                }
            }
            BinOp::Eq => format!("({x} == {y})"),
            BinOp::Ne => format!("({x} != {y})"),
            BinOp::Lt => format!("({x} < {y})"),
            BinOp::Gt => format!("({x} > {y})"),
            BinOp::Le => format!("({x} <= {y})"),
            BinOp::Ge => format!("({x} >= {y})"),
            BinOp::And => format!(
                "({} && {})",
                self.read_as(a, Ty::Bool),
                self.read_as(b, Ty::Bool)
            ),
            BinOp::Or => format!(
                "({} || {})",
                self.read_as(a, Ty::Bool),
                self.read_as(b, Ty::Bool)
            ),
            BinOp::Concat => return err("a kernel cannot concatenate strings"),
        })
    }

    fn emit_builtin(
        &self,
        out: &mut String,
        d: Reg,
        built: Built,
        args: &[Reg],
        pad: &str,
    ) -> Result<(), Error> {
        // Buffer access is the one place a register names a binding rather
        // than a value.
        match built {
            Built::BufAt => {
                let buf = args.first().copied().unwrap_or(Reg(0));
                let idx = args.get(1).copied().unwrap_or(Reg(0));
                let b = self.binding(buf)?;
                writeln!(
                    out,
                    "{pad}r{} = b{b}[u32({})];",
                    d.0,
                    self.read_as(idx, Ty::I32)
                )
                .unwrap();
                return Ok(());
            }
            Built::BufPut => {
                let buf = args.first().copied().unwrap_or(Reg(0));
                let idx = args.get(1).copied().unwrap_or(Reg(0));
                let val = args.get(2).copied().unwrap_or(Reg(0));
                let b = self.binding(buf)?;
                let elem = self.ty(buf);
                writeln!(
                    out,
                    "{pad}b{b}[u32({})] = {};",
                    self.read_as(idx, Ty::I32),
                    self.read_as(val, elem)
                )
                .unwrap();
                return Ok(());
            }
            Built::BufLen => {
                let buf = args.first().copied().unwrap_or(Reg(0));
                let b = self.binding(buf)?;
                writeln!(out, "{pad}r{} = i32(arrayLength(&b{b}));", d.0).unwrap();
                return Ok(());
            }
            _ => {}
        }

        let a = |i: usize, t: Ty| -> String {
            args.get(i)
                .map(|r| self.read_as(*r, t))
                .unwrap_or_else(|| "0.0".to_string())
        };
        let text = match built {
            Built::Sqrt => format!("sqrt({})", a(0, Ty::F32)),
            Built::Pow => format!("pow({}, {})", a(0, Ty::F32), a(1, Ty::F32)),
            Built::Floor => format!("floor({})", a(0, Ty::F32)),
            Built::Ceil => format!("ceil({})", a(0, Ty::F32)),
            Built::Round => format!("round({})", a(0, Ty::F32)),
            Built::Sin => format!("sin({})", a(0, Ty::F32)),
            Built::Cos => format!("cos({})", a(0, Ty::F32)),
            Built::Tan => format!("tan({})", a(0, Ty::F32)),
            Built::Asin => format!("asin({})", a(0, Ty::F32)),
            Built::Acos => format!("acos({})", a(0, Ty::F32)),
            Built::Atan => format!("atan({})", a(0, Ty::F32)),
            Built::Atan2 => format!("atan2({}, {})", a(0, Ty::F32), a(1, Ty::F32)),
            Built::Log => format!("log({})", a(0, Ty::F32)),
            Built::Log10 => format!("(log({}) / log(10.0))", a(0, Ty::F32)),
            Built::Exp => format!("exp({})", a(0, Ty::F32)),
            Built::Abs => {
                let t = self.ty(d);
                format!("abs({})", a(0, t))
            }
            Built::Min => {
                let t = self.ty(d);
                format!("min({}, {})", a(0, t), a(1, t))
            }
            Built::Max => {
                let t = self.ty(d);
                format!("max({}, {})", a(0, t), a(1, t))
            }
            Built::Int => format!("i32({})", a(0, Ty::F32)),
            Built::Float => format!("f32({})", a(0, Ty::I32)),
            Built::Not => format!("!({})", a(0, Ty::Bool)),
            other => return err(format!("builtin {other:?} has no GPU equivalent")),
        };
        writeln!(out, "{pad}r{} = {text};", d.0).unwrap();
        Ok(())
    }

    /// The storage binding a register refers to, or an error naming the
    /// mismatch — a kernel indexing something that is not a buffer means the
    /// launch signature and the kernel body disagree.
    fn binding(&self, r: Reg) -> Result<usize, Error> {
        self.buffers.get(&r.0).copied().ok_or_else(|| {
            Error(format!(
                "register r{} is indexed as a buffer but was not bound as one",
                r.0
            ))
        })
    }
}

/// WGSL has no integer-valued float literals: `1` is an i32, `1.0` is an f32.
fn format_float(f: f64) -> String {
    if f.is_nan() {
        // No NaN literal in WGSL; construct one that the validator accepts.
        return "(0.0 / 0.0)".to_string();
    }
    if f.is_infinite() {
        return if f > 0.0 {
            "(1.0 / 0.0)".to_string()
        } else {
            "(-1.0 / 0.0)".to_string()
        };
    }
    let s = format!("{f:?}");
    if s.contains('.') || s.contains('e') || s.contains('E') {
        s
    } else {
        format!("{s}.0")
    }
}

fn dest(op: &Op) -> Option<Reg> {
    match op {
        Op::Lit(d, ..)
        | Op::Mov(d, ..)
        | Op::Upval(d, ..)
        | Op::Bin(d, ..)
        | Op::Un(d, ..)
        | Op::Call(d, ..)
        | Op::Invoke(d, ..)
        | Op::Close(d, ..)
        | Op::Vec(d, ..)
        | Op::Map(d, ..)
        | Op::Set(d, ..)
        | Op::Tup(d, ..)
        | Op::Adt(d, ..)
        | Op::Field(d, ..)
        | Op::Tag(d, ..)
        | Op::Perform(d, ..)
        | Op::Builtin(d, ..)
        | Op::PushHandler(d, ..) => Some(*d),
        Op::PopHandler(_) => None,
    }
}

/// A phrase naming what an operation does, for a refusal message.
fn describe(op: &Op) -> &'static str {
    match op {
        Op::Upval(..) => "reading a closure upvalue",
        Op::Invoke(..) => "calling a closure",
        Op::Close(..) => "creating a closure",
        Op::Vec(..) | Op::Map(..) | Op::Set(..) | Op::Tup(..) => "building a collection",
        Op::Adt(..) => "constructing a value",
        Op::Field(..) => "field access",
        Op::Tag(..) => "reading a constructor tag",
        Op::Perform(..) => "performing an effect",
        Op::PushHandler(..) | Op::PopHandler(..) => "installing a handler",
        _ => "this operation",
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::check::Checker;
    use crate::eir::lower::lower;
    use crate::parser::parse;

    /// Lower a program and emit WGSL for the named kernel.
    fn emit_kernel(src: &str, name: &str, args: &[ArgKind]) -> Result<String, Error> {
        let exprs = parse(src).expect("parses");
        let mut checker = Checker::new();
        let errors = checker.check_program(&exprs);
        assert!(errors.is_empty(), "check errors: {errors:?}");
        let module = lower(&checker);
        let func = module
            .funcs
            .iter()
            .find(|f| f.name.as_deref() == Some(name))
            .unwrap_or_else(|| panic!("no kernel '{name}'"));
        emit(&module, func.id, args)
    }

    /// Validate WGSL the way a GPU driver would, without needing one.
    ///
    /// This is the check the offload paper says is missing for cross-target
    /// work: the shader is parsed and type-checked in CI, so a kernel that
    /// would be rejected on a device is rejected here first.
    fn validate(wgsl: &str) {
        let module = naga::front::wgsl::parse_str(wgsl)
            .unwrap_or_else(|e| panic!("WGSL did not parse: {e}\n\n{wgsl}"));
        let mut validator = naga::valid::Validator::new(
            naga::valid::ValidationFlags::all(),
            naga::valid::Capabilities::empty(),
        );
        validator
            .validate(&module)
            .unwrap_or_else(|e| panic!("WGSL did not validate: {e:?}\n\n{wgsl}"));
    }

    #[test]
    fn saxpy_emits_a_valid_shader() {
        let wgsl = emit_kernel(
            "[kernel saxpy [i a x y out] [put out i [+ [* a [at x i]] [at y i]]]]",
            "saxpy",
            &[
                ArgKind::Scalar(DType::F32),
                ArgKind::input(DType::F32),
                ArgKind::input(DType::F32),
                ArgKind::output(DType::F32),
            ],
        )
        .expect("emits");

        assert!(wgsl.contains("@compute"), "{wgsl}");
        assert!(wgsl.contains("var<storage, read> b1"), "{wgsl}");
        assert!(wgsl.contains("var<storage, read_write> b3"), "{wgsl}");
        validate(&wgsl);
    }

    #[test]
    fn a_single_block_kernel_needs_no_dispatcher() {
        // The straight-line case should read like the source did.
        let wgsl = emit_kernel(
            "[kernel double [i b] [put b i [* 2.0 [at b i]]]]",
            "double",
            &[ArgKind::output(DType::F32)],
        )
        .expect("emits");
        assert!(
            !wgsl.contains("switch blk"),
            "no control flow should mean no dispatcher:\n{wgsl}"
        );
        validate(&wgsl);
    }

    #[test]
    fn branching_kernels_use_the_block_dispatcher() {
        let wgsl = emit_kernel(
            "[kernel clamp01 [i b] \
               [let v [at b i]] \
               [put b i [if [> v 1.0] 1.0 [if [< v 0.0] 0.0 v]]]]",
            "clamp01",
            &[ArgKind::output(DType::F32)],
        )
        .expect("emits");
        assert!(wgsl.contains("switch blk"), "{wgsl}");
        validate(&wgsl);
    }

    #[test]
    fn math_builtins_map_onto_wgsl_intrinsics() {
        let wgsl = emit_kernel(
            "[kernel mathy [i b] \
               [put b i [+ [sqrt [abs [at b i]]] [* [sin [at b i]] [exp 1.0]]]]]",
            "mathy",
            &[ArgKind::output(DType::F32)],
        )
        .expect("emits");
        for f in ["sqrt(", "abs(", "sin(", "exp("] {
            assert!(wgsl.contains(f), "expected {f} in:\n{wgsl}");
        }
        validate(&wgsl);
    }

    #[test]
    fn integer_buffers_emit_integer_bindings() {
        let wgsl = emit_kernel(
            "[kernel bump [i b] [put b i [+ 1 [at b i]]]]",
            "bump",
            &[ArgKind::output(DType::I32)],
        )
        .expect("emits");
        assert!(wgsl.contains("array<i32>"), "{wgsl}");
        validate(&wgsl);
    }

    #[test]
    fn mixed_arithmetic_converts_rather_than_mixing_types() {
        // WGSL will not add an i32 to an f32. The emitter has to insert the
        // conversion, and the validator is what proves it did.
        let wgsl = emit_kernel(
            "[kernel mixed [i b] [put b i [+ 1 [at b i]]]]",
            "mixed",
            &[ArgKind::output(DType::F32)],
        )
        .expect("emits");
        validate(&wgsl);
    }

    #[test]
    fn buffer_length_is_available_to_a_kernel() {
        let wgsl = emit_kernel(
            "[kernel norm [i b] [put b i [if [> [buf-len b] 0] [at b i] 0.0]]]",
            "norm",
            &[ArgKind::output(DType::F32)],
        )
        .expect("emits");
        assert!(wgsl.contains("arrayLength"), "{wgsl}");
        validate(&wgsl);
    }

    #[test]
    fn the_work_index_bounds_check_is_always_emitted() {
        // A GPU dispatch rounds up to whole workgroups, so the last group runs
        // invocations past the end of the data. Without this check they would
        // write outside the buffer.
        let wgsl = emit_kernel(
            "[kernel k [i b] [put b i 1.0]]",
            "k",
            &[ArgKind::output(DType::F32)],
        )
        .expect("emits");
        assert!(wgsl.contains("if (idx >= params.n) { return; }"), "{wgsl}");
    }

    #[test]
    fn a_wrong_argument_count_is_refused_by_name() {
        let e = emit_kernel(
            "[kernel k [i a b] [put b i a]]",
            "k",
            &[ArgKind::output(DType::F32)],
        )
        .expect_err("should refuse");
        assert!(e.0.contains("parameters"), "{}", e.0);
    }

    #[test]
    fn scalars_arrive_through_the_uniform_block() {
        let wgsl = emit_kernel(
            "[kernel scale [i s b] [put b i [* s [at b i]]]]",
            "scale",
            &[ArgKind::Scalar(DType::F32), ArgKind::output(DType::F32)],
        )
        .expect("emits");
        assert!(wgsl.contains("s0: f32"), "{wgsl}");
        assert!(wgsl.contains("params.s0"), "{wgsl}");
        validate(&wgsl);
    }

    #[test]
    fn buffer_bindings_are_numbered_without_gaps() {
        // A scalar argument occupies no binding, so the buffers after it must
        // still number consecutively — a host builds its bind group layout
        // from these, and a hole in the sequence is a mismatch waiting to
        // happen.
        let wgsl = emit_kernel(
            "[kernel saxpy [i a x y out] [put out i [+ [* a [at x i]] [at y i]]]]",
            "saxpy",
            &[
                ArgKind::Scalar(DType::F32),
                ArgKind::input(DType::F32),
                ArgKind::input(DType::F32),
                ArgKind::output(DType::F32),
            ],
        )
        .expect("emits");
        assert!(wgsl.contains("@binding(1) var<storage, read> b1"), "{wgsl}");
        assert!(wgsl.contains("@binding(2) var<storage, read> b2"), "{wgsl}");
        assert!(
            wgsl.contains("@binding(3) var<storage, read_write> b3"),
            "{wgsl}"
        );
        validate(&wgsl);
    }

    #[test]
    fn argument_shapes_are_read_off_the_kernel_body() {
        // `a` is multiplied, so it is a scalar; `x` is indexed, so it is a
        // buffer; `out` is written through, so it is writable. Nobody said so.
        let src = "[kernel saxpy [i a x y out] [put out i [+ [* a [at x i]] [at y i]]]]";
        let exprs = parse(src).expect("parses");
        let mut checker = Checker::new();
        assert!(checker.check_program(&exprs).is_empty());
        let module = lower(&checker);
        let f = module
            .funcs
            .iter()
            .find(|f| f.name.as_deref() == Some("saxpy"))
            .expect("saxpy");
        let kinds = infer_arg_kinds(&module, f.id, DType::F32);
        assert_eq!(
            kinds,
            vec![
                ArgKind::Scalar(DType::F32),
                ArgKind::input(DType::F32),
                ArgKind::input(DType::F32),
                ArgKind::output(DType::F32),
            ]
        );
    }

    #[test]
    fn a_buffer_used_as_a_number_is_refused_by_argument() {
        // The launch says argument 1 is a buffer; the body multiplies by it.
        // There is no WGSL expression for a binding, so this cannot be
        // compiled — and the message says which argument disagrees.
        let e = emit_kernel(
            "[kernel scale [i s b] [put b i [* s [at b i]]]]",
            "scale",
            &[ArgKind::input(DType::F32), ArgKind::output(DType::F32)],
        )
        .expect_err("should refuse");
        assert!(e.0.contains("argument 1"), "{}", e.0);
        assert!(e.0.contains("as a number"), "{}", e.0);
    }

    #[test]
    fn float_literals_are_never_emitted_as_integers() {
        // `1` and `1.0` are different types in WGSL, and getting this wrong is
        // a validation error rather than a wrong answer — but only because
        // something checks.
        assert_eq!(format_float(1.0), "1.0");
        assert_eq!(format_float(-2.5), "-2.5");
        assert_eq!(format_float(0.0), "0.0");
    }
}
