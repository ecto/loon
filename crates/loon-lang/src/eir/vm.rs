//! Register VM backend — executes Evidence IR directly.
//!
//! Each function gets a register file (`Vec<Val>`). The dispatch loop walks
//! blocks linearly, interpreting each `Op`. Function calls push frames.
//! Tail calls reuse the current frame. No continuation stack — just a
//! call stack of `(FuncId, BlockId, ip, registers)`.

pub use crate::eir::value64::Val;
use crate::eir::*;
use std::collections::HashMap;
use std::rc::Rc;

// Persistent collection types — O(log n) clone via structural sharing.
type ImVec = imbl::Vector<Val>;
type ImMap = imbl::HashMap<Val, Val>;
type ImSet = imbl::HashSet<Val>;

// ─── Heap objects ──────────────────────────────────────────────────────────

/// A heap-allocated object. Stored in the VM's object table.
#[derive(Debug, Clone)]
enum Obj {
    Str(String),
    Vec(ImVec),
    Map(ImMap),
    Set(ImSet),
    Tuple(Vec<Val>),           // fixed-size, no persistence needed
    Adt(u16, Vec<Val>),        // tag + fields
    Closure(FuncId, Vec<Val>), // func + captured values
}

impl Obj {
    /// Rough estimate of the byte size of this heap object (for stats, not GC).
    fn estimated_bytes(&self) -> u64 {
        match self {
            Obj::Str(s) => (24 + s.len()) as u64,
            Obj::Vec(v) => (24 + v.len() * 8) as u64,
            Obj::Set(s) => (24 + s.len() * 8) as u64,
            Obj::Tuple(v) => (24 + v.len() * 8) as u64,
            Obj::Map(m) => (24 + m.len() * 16) as u64,
            Obj::Adt(_, fields) => (24 + 2 + fields.len() * 8) as u64,
            Obj::Closure(_, caps) => (24 + 4 + caps.len() * 8) as u64,
        }
    }
}

// ─── Call frame ────────────────────────────────────────────────────────────

/// Saved call frame — pushed when entering a function, popped on return.
struct Frame {
    func: FuncId,
    block: BlockId,
    /// Instruction pointer within the current block.
    ip: usize,
    /// Register file for the suspended frame.
    regs: Vec<Val>,
    /// Register in the caller's frame to write the return value into.
    ret_reg: u32,
    /// Closure captures for the suspended frame.
    captures: Vec<Val>,
}

// ─── VM ────────────────────────────────────────────────────────────────────

/// Dynamic effect handler entry.
struct DynHandler {
    /// String pool index for the effect name.
    effect: StringId,
    /// String pool index for the operation name.
    op: StringId,
    closure: Val,
}

/// The register VM. Executes an EIR Module.
pub struct Vm {
    module: Rc<Module>,
    heap: Vec<Obj>,
    frames: Vec<Frame>,
    regs: Vec<Val>,     // current register file
    captures: Vec<Val>, // current closure captures
    func: FuncId,
    block: BlockId,
    ip: usize,
    /// Output capture (for println).
    output: Vec<String>,
    /// String constants resolved to heap indices.
    string_cache: HashMap<StringId, usize>,
    /// Dynamic effect handler stack.
    handlers: Vec<DynHandler>,
    /// Pre-allocated identity closure for `resume`.
    resume_closure: Val,
    /// Span of the instruction currently being executed (for error reporting).
    current_span: Span,
    /// Heap statistics tracked during execution.
    heap_stats: HeapStats,
}

/// Heap allocation statistics collected during VM execution.
#[derive(Debug, Clone, Default)]
pub struct HeapStats {
    /// Total number of heap allocations performed.
    pub total_allocs: u64,
    /// Estimated total bytes allocated (rough, based on object kind).
    pub total_bytes: u64,
    /// Peak number of live objects (snapshot of heap length high-water mark).
    pub peak_objects: usize,
}

/// Result of running the VM.
#[derive(Debug)]
pub struct VmResult {
    pub value: Val,
    pub output: Vec<String>,
    /// Heap statistics from the execution.
    pub heap_stats: HeapStats,
}

impl Vm {
    pub fn new(module: Module) -> Self {
        Self {
            module: Rc::new(module),
            heap: Vec::new(),
            frames: Vec::new(),
            regs: Vec::new(),
            captures: Vec::new(),
            func: FuncId(0),
            block: BlockId(0),
            ip: 0,
            output: Vec::new(),
            string_cache: HashMap::new(),
            handlers: Vec::new(),
            resume_closure: Val::UNIT, // set in run()
            current_span: Span::ZERO,
            heap_stats: HeapStats::default(),
        }
    }

    /// Run the module's entry function.
    pub fn run(&mut self) -> Result<VmResult, VmError> {
        // Pre-create the identity function for `resume` in effect handlers.
        self.resume_closure = {
            let module = Rc::get_mut(&mut self.module).unwrap();
            let id = FuncId(module.funcs.len() as u32);
            module.funcs.push(Func {
                id,
                name: Some("resume".to_string()),
                params: vec![Ty::Any],
                ret: Ty::Any,
                evidence: Vec::new(),
                captures: Vec::new(),
                blocks: vec![Block {
                    id: BlockId(0),
                    params: vec![Reg(0)],
                    ops: Vec::new(),
                    end: End::Ret(Reg(0)),
                }],
                span: Span::ZERO,
                is_closure: false,
            });
            self.alloc(Obj::Closure(id, Vec::new()))
        };
        let entry = self.module.entry;
        self.call_func(entry, &[], 0)?;
        let val = self.execute(0)?;
        Ok(VmResult {
            value: val,
            output: std::mem::take(&mut self.output),
            heap_stats: self.heap_stats.clone(),
        })
    }

    /// Call a function and run it to completion, returning its result.
    /// Used by higher-order builtins (map, filter, each).
    #[allow(dead_code)]
    fn run_call(&mut self, func_id: FuncId, args: &[Val]) -> Result<Val, VmError> {
        self.run_call_with_captures(func_id, args, Vec::new())
    }

    /// Call a closure (function + captures) and run it to completion.
    fn run_call_with_captures(
        &mut self,
        func_id: FuncId,
        args: &[Val],
        caps: Vec<Val>,
    ) -> Result<Val, VmError> {
        let depth = self.frames.len();
        self.call_func_with_captures(func_id, args, 0, caps)?;
        self.execute(depth + 1)
    }

    // ── Heap ───────────────────────────────────────────────────────────

    fn alloc(&mut self, obj: Obj) -> Val {
        // Track heap statistics.
        self.heap_stats.total_allocs += 1;
        self.heap_stats.total_bytes += obj.estimated_bytes();
        let idx = self.heap.len();
        self.heap.push(obj);
        if self.heap.len() > self.heap_stats.peak_objects {
            self.heap_stats.peak_objects = self.heap.len();
        }
        Val::ptr(idx)
    }

    fn get_obj(&self, val: Val) -> Option<&Obj> {
        if val.is_ptr() {
            self.heap.get(val.as_ptr())
        } else {
            None
        }
    }

    fn get_str(&self, val: Val) -> Option<&str> {
        match self.get_obj(val)? {
            Obj::Str(s) => Some(s),
            _ => None,
        }
    }

    /// Structural value equality (the semantics of `=` / `!=`).
    ///
    /// Immediates and identical pointers compare by bits (the fast path); two
    /// distinct heap objects are compared by content, recursively. This matches
    /// the legacy tree-walking interpreter (whose `Value` derives a structural
    /// `PartialEq`) — without it `=` on strings is pointer identity, so e.g.
    /// `[= "ab" [str "a" "b"]]` would be false. Loon values are immutable and
    /// acyclic, so the recursion terminates.
    fn val_eq(&self, a: Val, b: Val) -> bool {
        if a == b {
            return true;
        }
        match (self.get_obj(a), self.get_obj(b)) {
            (Some(oa), Some(ob)) => self.obj_eq(oa, ob),
            _ => false,
        }
    }

    fn obj_eq(&self, a: &Obj, b: &Obj) -> bool {
        match (a, b) {
            (Obj::Str(x), Obj::Str(y)) => x == y,
            (Obj::Vec(x), Obj::Vec(y)) => {
                x.len() == y.len() && x.iter().zip(y.iter()).all(|(p, q)| self.val_eq(*p, *q))
            }
            (Obj::Tuple(x), Obj::Tuple(y)) => {
                x.len() == y.len() && x.iter().zip(y.iter()).all(|(p, q)| self.val_eq(*p, *q))
            }
            (Obj::Adt(tx, fx), Obj::Adt(ty, fy)) => {
                tx == ty
                    && fx.len() == fy.len()
                    && fx.iter().zip(fy.iter()).all(|(p, q)| self.val_eq(*p, *q))
            }
            // Sets and maps are unordered: every entry of one must have a
            // structurally equal counterpart in the other.
            (Obj::Set(x), Obj::Set(y)) => {
                x.len() == y.len() && x.iter().all(|p| y.iter().any(|q| self.val_eq(*p, *q)))
            }
            (Obj::Map(x), Obj::Map(y)) => {
                x.len() == y.len()
                    && x.iter().all(|(k, v)| {
                        y.iter()
                            .any(|(k2, v2)| self.val_eq(*k, *k2) && self.val_eq(*v, *v2))
                    })
            }
            // Closures compare by identity only (handled by the fast path).
            _ => false,
        }
    }

    fn resolve_string(&mut self, sid: StringId) -> Val {
        if let Some(&idx) = self.string_cache.get(&sid) {
            return Val::ptr(idx);
        }
        let s = self.module.strings[sid.0 as usize].clone();
        let val = self.alloc(Obj::Str(s));
        self.string_cache.insert(sid, val.as_ptr());
        val
    }

    // ── Register access ────────────────────────────────────────────────

    #[inline(always)]
    fn r(&self, reg: Reg) -> Val {
        self.regs[reg.0 as usize]
    }

    #[inline(always)]
    fn w(&mut self, reg: Reg, val: Val) {
        let idx = reg.0 as usize;
        if idx >= self.regs.len() {
            self.regs.resize(idx + 1, Val::UNIT);
        }
        self.regs[idx] = val;
    }

    fn read_regs(&self, regs: &[Reg]) -> Vec<Val> {
        regs.iter().map(|r| self.r(*r)).collect()
    }

    // ── Function call ──────────────────────────────────────────────────

    fn call_func(&mut self, func_id: FuncId, args: &[Val], ret_reg: u32) -> Result<(), VmError> {
        self.call_func_with_captures(func_id, args, ret_reg, Vec::new())
    }

    fn call_func_with_captures(
        &mut self,
        func_id: FuncId,
        args: &[Val],
        ret_reg: u32,
        new_captures: Vec<Val>,
    ) -> Result<(), VmError> {
        // Save current frame
        if !self.regs.is_empty() || !self.frames.is_empty() {
            self.frames.push(Frame {
                func: self.func,
                block: self.block,
                ip: self.ip,
                regs: std::mem::take(&mut self.regs),
                ret_reg,
                captures: std::mem::take(&mut self.captures),
            });
        }

        // Set up new frame
        let func = &self.module.funcs[func_id.0 as usize];
        let reg_count = func
            .blocks
            .iter()
            .flat_map(|b| {
                b.ops
                    .iter()
                    .map(|op| op.dst().0 + 1)
                    .chain(b.params.iter().map(|r| r.0 + 1))
            })
            .max()
            .unwrap_or(0) as usize;
        let reg_count = reg_count.max(args.len());

        self.regs = vec![Val::UNIT; reg_count + 16]; // padding for safety
        for (i, &val) in args.iter().enumerate() {
            self.regs[i] = val;
        }

        self.captures = new_captures;
        self.func = func_id;
        self.block = BlockId(0);
        self.ip = 0;
        Ok(())
    }

    fn return_val(&mut self, val: Val) -> Result<Option<Val>, VmError> {
        if let Some(frame) = self.frames.pop() {
            let ret_reg = frame.ret_reg;
            self.regs = frame.regs;
            self.captures = frame.captures;
            self.func = frame.func;
            self.block = frame.block;
            self.ip = frame.ip;
            self.regs[ret_reg as usize] = val;
            Ok(None) // keep executing
        } else {
            Ok(Some(val)) // top-level return
        }
    }

    // ── Main dispatch loop ─────────────────────────────────────────────

    fn execute(&mut self, min_depth: usize) -> Result<Val, VmError> {
        let module = Rc::clone(&self.module);

        loop {
            // Fetch current instruction or terminator by index (no stale refs)
            let func_idx = self.func.0 as usize;
            let block_idx = self.block.0 as usize;
            let ops_len = module.funcs[func_idx].blocks[block_idx].ops.len();

            if self.ip < ops_len {
                // Clone the op to avoid borrowing module across exec_op
                let op = module.funcs[func_idx].blocks[block_idx].ops[self.ip].clone();
                self.current_span = op.span();
                self.ip += 1;
                self.exec_op(&op)?;
                continue;
            }

            // Execute terminator
            let end = module.funcs[func_idx].blocks[block_idx].end.clone();
            match end {
                End::Ret(reg) => {
                    let val = self.r(reg);
                    if self.frames.is_empty() {
                        return Ok(val);
                    }
                    self.return_val(val)?;
                    // After restoring the caller's frame, check if we should
                    // return to run_call (higher-order builtin callback).
                    // Use strict < to avoid early return when a nested call
                    // within the entry function completes.
                    if self.frames.len() < min_depth {
                        return Ok(val);
                    }
                }

                End::Jmp(target, ref args) => {
                    let vals = self.read_regs(args);
                    let params: Vec<Reg> = module.funcs[func_idx].blocks[target.0 as usize]
                        .params
                        .clone();
                    for (param, val) in params.iter().zip(vals.iter()) {
                        self.w(*param, *val);
                    }
                    self.block = target;
                    self.ip = 0;
                }

                End::Br(cond, then_b, else_b) => {
                    let v = self.r(cond);
                    self.block = if v.is_truthy() { then_b } else { else_b };
                    self.ip = 0;
                }

                End::Switch(scrutinee, ref cases, default) => {
                    let v = self.r(scrutinee);
                    let tag = if let Some(Obj::Adt(t, _)) = self.get_obj(v) {
                        *t
                    } else if v.is_int() {
                        v.as_int() as u16
                    } else {
                        0
                    };
                    let target = cases
                        .iter()
                        .find(|(t, _)| *t == tag)
                        .map(|(_, b)| *b)
                        .unwrap_or(default);
                    self.block = target;
                    self.ip = 0;
                }

                End::Tail(func_id, ref args) => {
                    let vals = self.read_regs(args);
                    let f = &module.funcs[func_id.0 as usize];
                    let reg_count = f
                        .blocks
                        .iter()
                        .flat_map(|b| {
                            b.ops
                                .iter()
                                .map(|op| op.dst().0 + 1)
                                .chain(b.params.iter().map(|r| r.0 + 1))
                        })
                        .max()
                        .unwrap_or(0) as usize;
                    let needed = reg_count.max(vals.len()) + 16;
                    self.regs.resize(needed, Val::UNIT);
                    for (i, &val) in vals.iter().enumerate() {
                        self.regs[i] = val;
                    }
                    self.func = func_id;
                    self.block = BlockId(0);
                    self.ip = 0;
                }

                End::TailInvoke(callee, ref args) => {
                    let func_val = self.r(callee);
                    let vals = self.read_regs(args);
                    if let Some(Obj::Closure(fid, caps)) = self.get_obj(func_val).cloned() {
                        let f = &module.funcs[fid.0 as usize];
                        let reg_count = f
                            .blocks
                            .iter()
                            .flat_map(|b| {
                                b.ops
                                    .iter()
                                    .map(|op| op.dst().0 + 1)
                                    .chain(b.params.iter().map(|r| r.0 + 1))
                            })
                            .max()
                            .unwrap_or(0) as usize;
                        let needed = reg_count.max(vals.len()) + 16;
                        self.regs.resize(needed, Val::UNIT);
                        for (i, &val) in vals.iter().enumerate() {
                            self.regs[i] = val;
                        }
                        self.captures = caps;
                        self.func = fid;
                        self.block = BlockId(0);
                        self.ip = 0;
                    } else {
                        return Err(
                            VmError::new(VmErrorKind::NotCallable).with_span(self.current_span)
                        );
                    }
                }

                End::Recur(ref args) => {
                    let vals = self.read_regs(args);
                    let params: Vec<Reg> = module.funcs[func_idx].blocks[0].params.clone();
                    for (param, val) in params.iter().zip(vals.iter()) {
                        self.w(*param, *val);
                    }
                    self.block = BlockId(0);
                    self.ip = 0;
                }

                End::Trap => {
                    return Err(VmError::new(VmErrorKind::Trap).with_span(self.current_span));
                }
            }
        }
    }

    // ── Instruction dispatch ───────────────────────────────────────────

    fn exec_op(&mut self, op: &Op) -> Result<(), VmError> {
        match op {
            Op::Lit(dst, lit, _) => {
                let val = match lit {
                    Lit::Int(n) => {
                        if (-(1i64 << 47)..(1i64 << 47)).contains(n) {
                            Val::int(*n)
                        } else {
                            // Box large ints
                            self.alloc(Obj::Str(n.to_string()))
                            // TODO: proper boxed int
                        }
                    }
                    Lit::Float(f) => Val::float(*f),
                    Lit::Bool(b) => Val::bool(*b),
                    Lit::Str(sid) => self.resolve_string(*sid),
                    Lit::Keyword(sid) => {
                        // Keywords as interned symbols
                        Val::sym(sid.0)
                    }
                    Lit::Unit => Val::UNIT,
                };
                self.w(*dst, val);
            }

            Op::Mov(dst, src, _) => {
                let v = self.r(*src);
                self.w(*dst, v);
            }

            Op::Upval(dst, idx, _) => {
                let val = self
                    .captures
                    .get(*idx as usize)
                    .copied()
                    .unwrap_or(Val::UNIT);
                self.w(*dst, val);
            }

            Op::Bin(dst, binop, a, b, _) => {
                let av = self.r(*a);
                let bv = self.r(*b);
                let result = self.exec_binop(*binop, av, bv);
                self.w(*dst, result);
            }

            Op::Un(dst, unop, a, _) => {
                let av = self.r(*a);
                let result = match unop {
                    UnOp::Neg => {
                        if av.is_int() {
                            Val::int(-av.as_int())
                        } else if av.is_float() {
                            Val::float(-av.as_float())
                        } else {
                            Val::UNIT
                        }
                    }
                    UnOp::Not => Val::bool(!av.is_truthy()),
                };
                self.w(*dst, result);
            }

            Op::Call(dst, func_id, args, _) => {
                let vals = self.read_regs(args);
                let ret_reg = dst.0;
                self.call_func(*func_id, &vals, ret_reg)?;
            }

            Op::Invoke(dst, callee, args, span) => {
                let func_val = self.r(*callee);
                let vals = self.read_regs(args);
                if let Some(Obj::Closure(fid, caps)) = self.get_obj(func_val).cloned() {
                    let ret_reg = dst.0;
                    self.call_func_with_captures(fid, &vals, ret_reg, caps)?;
                } else {
                    return Err(VmError::new(VmErrorKind::NotCallable).with_span(*span));
                }
            }

            Op::Close(dst, func_id, captures, _) => {
                let cap_vals = self.read_regs(captures);
                let val = self.alloc(Obj::Closure(*func_id, cap_vals));
                self.w(*dst, val);
            }

            Op::Vec(dst, elems, _) => {
                let vals: ImVec = elems.iter().map(|r| self.r(*r)).collect();
                let val = self.alloc(Obj::Vec(vals));
                self.w(*dst, val);
            }

            Op::Map(dst, pairs, _) => {
                let kv: ImMap = pairs
                    .iter()
                    .map(|(k, v)| (self.r(*k), self.r(*v)))
                    .collect();
                let val = self.alloc(Obj::Map(kv));
                self.w(*dst, val);
            }

            Op::Set(dst, elems, _) => {
                let vals: ImSet = elems.iter().map(|r| self.r(*r)).collect();
                let val = self.alloc(Obj::Set(vals));
                self.w(*dst, val);
            }

            Op::Tup(dst, elems, _) => {
                let vals = self.read_regs(elems);
                let val = self.alloc(Obj::Tuple(vals));
                self.w(*dst, val);
            }

            Op::Adt(dst, tag, fields, _) => {
                let vals = self.read_regs(fields);
                let val = self.alloc(Obj::Adt(*tag, vals));
                self.w(*dst, val);
            }

            Op::Field(dst, obj, selector, _) => {
                let oval = self.r(*obj);
                let result = match (self.get_obj(oval), selector) {
                    (Some(Obj::Adt(_, fields)), Selector::Index(i)) => {
                        fields.get(*i as usize).copied().unwrap_or(Val::UNIT)
                    }
                    (Some(Obj::Tuple(fields)), Selector::Index(i)) => {
                        fields.get(*i as usize).copied().unwrap_or(Val::UNIT)
                    }
                    (Some(Obj::Map(map)), Selector::Name(sid)) => {
                        // Try symbol key first, then string key
                        let sym_key = Val::sym(sid.0);
                        if let Some(v) = map.get(&sym_key) {
                            *v
                        } else {
                            let key_str = &self.module.strings[sid.0 as usize];
                            // Fall back to scanning for string keys
                            map.iter()
                                .find(|(k, _)| self.get_str(**k) == Some(key_str))
                                .map(|(_, v)| *v)
                                .unwrap_or(Val::UNIT)
                        }
                    }
                    _ => Val::UNIT,
                };
                self.w(*dst, result);
            }

            Op::Tag(dst, obj, _) => {
                let oval = self.r(*obj);
                let tag = match self.get_obj(oval) {
                    Some(Obj::Adt(t, _)) => *t as i64,
                    _ => -1,
                };
                self.w(*dst, Val::int(tag));
            }

            Op::Perform(dst, eff_sid, op_sid, args, evidence, _span) => {
                let vals = self.read_regs(args);
                if let Some(ev_reg) = evidence {
                    // Evidence-passed: direct call to handler
                    let handler = self.r(*ev_reg);
                    if let Some(Obj::Closure(fid, caps)) = self.get_obj(handler).cloned() {
                        let resume_val = self.resume_closure;
                        let mut call_args = vec![resume_val];
                        call_args.extend_from_slice(&vals);
                        let ret_reg = dst.0;
                        self.call_func_with_captures(fid, &call_args, ret_reg, caps)?;
                    } else {
                        self.w(*dst, Val::UNIT);
                    }
                } else {
                    // Dynamic: check handler stack, then builtin.
                    // Compare by StringId (integer equality) — no string cloning.
                    let handler = self
                        .handlers
                        .iter()
                        .rev()
                        .find(|h| h.effect == *eff_sid && h.op == *op_sid)
                        .map(|h| h.closure);

                    if let Some(hval) = handler {
                        if let Some(Obj::Closure(fid, caps)) = self.get_obj(hval).cloned() {
                            let resume_val = self.resume_closure;
                            let mut call_args = vec![resume_val];
                            call_args.extend_from_slice(&vals);
                            let ret_reg = dst.0;
                            self.call_func_with_captures(fid, &call_args, ret_reg, caps)?;
                        } else {
                            self.w(*dst, Val::UNIT);
                        }
                    } else {
                        let effect = self.module.strings[eff_sid.0 as usize].clone();
                        let op_name = self.module.strings[op_sid.0 as usize].clone();
                        let result = self.builtin_effect(&effect, &op_name, &vals);
                        self.w(*dst, result);
                    }
                }
            }

            Op::Builtin(dst, built, args, _) => {
                let vals = self.read_regs(args);
                let result = self.exec_builtin(*built, &vals)?;
                self.w(*dst, result);
            }

            Op::PushHandler(handler_reg, eff_sid, op_sid, _) => {
                let closure = self.r(*handler_reg);
                self.handlers.push(DynHandler {
                    effect: *eff_sid,
                    op: *op_sid,
                    closure,
                });
            }

            Op::PopHandler(_) => {
                self.handlers.pop();
            }
        }
        Ok(())
    }

    // ── Binary operations ──────────────────────────────────────────────

    /// Create an int; if it overflows 48 bits, promote to float.
    fn safe_int(&self, n: i64) -> Val {
        if (-(1i64 << 47)..(1i64 << 47)).contains(&n) {
            Val::int(n)
        } else {
            Val::float(n as f64)
        }
    }

    fn exec_binop(&mut self, op: BinOp, a: Val, b: Val) -> Val {
        match op {
            BinOp::Add => {
                if a.is_int() && b.is_int() {
                    self.safe_int(a.as_int().wrapping_add(b.as_int()))
                } else if a.is_float() || b.is_float() {
                    let af = if a.is_float() {
                        a.as_float()
                    } else {
                        a.as_int() as f64
                    };
                    let bf = if b.is_float() {
                        b.as_float()
                    } else {
                        b.as_int() as f64
                    };
                    Val::float(af + bf)
                } else {
                    Val::UNIT
                }
            }
            BinOp::Sub => {
                if a.is_int() && b.is_int() {
                    self.safe_int(a.as_int().wrapping_sub(b.as_int()))
                } else {
                    let af = if a.is_float() {
                        a.as_float()
                    } else {
                        a.as_int() as f64
                    };
                    let bf = if b.is_float() {
                        b.as_float()
                    } else {
                        b.as_int() as f64
                    };
                    Val::float(af - bf)
                }
            }
            BinOp::Mul => {
                if a.is_int() && b.is_int() {
                    self.safe_int(a.as_int().wrapping_mul(b.as_int()))
                } else {
                    let af = if a.is_float() {
                        a.as_float()
                    } else {
                        a.as_int() as f64
                    };
                    let bf = if b.is_float() {
                        b.as_float()
                    } else {
                        b.as_int() as f64
                    };
                    Val::float(af * bf)
                }
            }
            BinOp::Div => {
                if a.is_int() && b.is_int() {
                    let bv = b.as_int();
                    if bv == 0 {
                        Val::UNIT
                    } else {
                        Val::int(a.as_int() / bv)
                    }
                } else {
                    let af = if a.is_float() {
                        a.as_float()
                    } else {
                        a.as_int() as f64
                    };
                    let bf = if b.is_float() {
                        b.as_float()
                    } else {
                        b.as_int() as f64
                    };
                    Val::float(af / bf)
                }
            }
            BinOp::Rem => {
                if a.is_int() && b.is_int() {
                    let bv = b.as_int();
                    if bv == 0 {
                        Val::UNIT
                    } else {
                        Val::int(a.as_int() % bv)
                    }
                } else {
                    Val::UNIT
                }
            }
            BinOp::Eq => Val::bool(self.val_eq(a, b)),
            BinOp::Ne => Val::bool(!self.val_eq(a, b)),
            BinOp::Lt => {
                if a.is_int() && b.is_int() {
                    Val::bool(a.as_int() < b.as_int())
                } else {
                    let af = if a.is_float() {
                        a.as_float()
                    } else {
                        a.as_int() as f64
                    };
                    let bf = if b.is_float() {
                        b.as_float()
                    } else {
                        b.as_int() as f64
                    };
                    Val::bool(af < bf)
                }
            }
            BinOp::Gt => {
                if a.is_int() && b.is_int() {
                    Val::bool(a.as_int() > b.as_int())
                } else {
                    let af = if a.is_float() {
                        a.as_float()
                    } else {
                        a.as_int() as f64
                    };
                    let bf = if b.is_float() {
                        b.as_float()
                    } else {
                        b.as_int() as f64
                    };
                    Val::bool(af > bf)
                }
            }
            BinOp::Le => {
                if a.is_int() && b.is_int() {
                    Val::bool(a.as_int() <= b.as_int())
                } else {
                    let af = if a.is_float() {
                        a.as_float()
                    } else {
                        a.as_int() as f64
                    };
                    let bf = if b.is_float() {
                        b.as_float()
                    } else {
                        b.as_int() as f64
                    };
                    Val::bool(af <= bf)
                }
            }
            BinOp::Ge => {
                if a.is_int() && b.is_int() {
                    Val::bool(a.as_int() >= b.as_int())
                } else {
                    let af = if a.is_float() {
                        a.as_float()
                    } else {
                        a.as_int() as f64
                    };
                    let bf = if b.is_float() {
                        b.as_float()
                    } else {
                        b.as_int() as f64
                    };
                    Val::bool(af >= bf)
                }
            }
            BinOp::And => {
                if a.is_truthy() {
                    b
                } else {
                    a
                }
            }
            BinOp::Or => {
                if a.is_truthy() {
                    a
                } else {
                    b
                }
            }
            BinOp::Concat => {
                let sa = self.val_to_string(a);
                let sb = self.val_to_string(b);
                self.alloc_str_owned(format!("{sa}{sb}"))
            }
        }
    }

    // ── Builtins ───────────────────────────────────────────────────────

    fn exec_builtin(&mut self, built: Built, args: &[Val]) -> Result<Val, VmError> {
        match built {
            Built::Println => {
                let s: Vec<String> = args.iter().map(|v| self.val_to_string(*v)).collect();
                let line = s.join(" ");
                println!("{line}");
                self.output.push(line);
                Ok(Val::UNIT)
            }
            Built::Print => {
                let s: Vec<String> = args.iter().map(|v| self.val_to_string(*v)).collect();
                print!("{}", s.join(" "));
                Ok(Val::UNIT)
            }
            Built::Str => {
                let s: Vec<String> = args.iter().map(|v| self.val_to_string(*v)).collect();
                Ok(self.alloc_str_owned(s.join("")))
            }
            Built::Len => {
                let v = args.first().copied().unwrap_or(Val::UNIT);
                let len = match self.get_obj(v) {
                    Some(Obj::Vec(items)) => items.len() as i64,
                    Some(Obj::Map(pairs)) => pairs.len() as i64,
                    Some(Obj::Set(items)) => items.len() as i64,
                    Some(Obj::Str(s)) => s.len() as i64,
                    Some(Obj::Tuple(items)) => items.len() as i64,
                    _ => 0,
                };
                Ok(Val::int(len))
            }
            Built::Get => {
                let coll = args.first().copied().unwrap_or(Val::UNIT);
                let key = args.get(1).copied().unwrap_or(Val::UNIT);
                let default = args.get(2).copied().unwrap_or(Val::UNIT);
                match self.get_obj(coll) {
                    Some(Obj::Map(map)) => {
                        // Direct hash lookup (O(log₃₂ n))
                        if let Some(v) = map.get(&key) {
                            return Ok(*v);
                        }
                        // Fuzzy: try string↔symbol interop
                        if let Some(ks) = self.get_str(key).map(|s| s.to_string()) {
                            for (k, v) in map.iter() {
                                if let Some(ks2) = self.get_str(*k) {
                                    if ks2 == ks {
                                        return Ok(*v);
                                    }
                                }
                            }
                        }
                        Ok(default)
                    }
                    Some(Obj::Vec(items)) => {
                        if key.is_int() {
                            Ok(items.get(key.as_int() as usize).copied().unwrap_or(default))
                        } else {
                            Ok(default)
                        }
                    }
                    _ => Ok(default),
                }
            }
            Built::Conj => {
                let coll = args.first().copied().unwrap_or(Val::UNIT);
                let val = args.get(1).copied().unwrap_or(Val::UNIT);
                match self.get_obj(coll).cloned() {
                    Some(Obj::Vec(mut items)) => {
                        items.push_back(val);  // O(log n) with structural sharing
                        Ok(self.alloc(Obj::Vec(items)))
                    }
                    Some(Obj::Set(mut items)) => {
                        items.insert(val);
                        Ok(self.alloc(Obj::Set(items)))
                    }
                    _ => Ok(Val::UNIT),
                }
            }
            Built::Assoc => {
                let coll = args.first().copied().unwrap_or(Val::UNIT);
                let key = args.get(1).copied().unwrap_or(Val::UNIT);
                let val = args.get(2).copied().unwrap_or(Val::UNIT);
                match self.get_obj(coll).cloned() {
                    Some(Obj::Map(mut map)) => {
                        map.insert(key, val);  // O(log₃₂ n) with structural sharing
                        Ok(self.alloc(Obj::Map(map)))
                    }
                    _ => Ok(Val::UNIT),
                }
            }
            Built::Range => {
                let start = args.first().copied().unwrap_or(Val::int(0));
                let end = args.get(1).copied().unwrap_or(Val::int(0));
                if start.is_int() && end.is_int() {
                    let items: ImVec = (start.as_int()..end.as_int()).map(Val::int).collect();
                    Ok(self.alloc(Obj::Vec(items)))
                } else {
                    Ok(self.alloc(Obj::Vec(ImVec::new())))
                }
            }
            Built::Map | Built::Filter | Built::Each | Built::Reduce => {
                // Higher-order builtins: call the function for each element
                let (func, coll) = if args.len() == 2 {
                    (args[0], args[1])
                } else {
                    return Ok(Val::UNIT);
                };

                match (self.get_obj(func).cloned(), self.get_obj(coll).cloned()) {
                    (Some(Obj::Closure(fid, caps)), Some(Obj::Vec(items))) => {
                        let mut results = ImVec::new();
                        for item in &items {
                            let result =
                                self.run_call_with_captures(fid, &[*item], caps.clone())?;
                            match built {
                                Built::Map => results.push_back(result),
                                Built::Filter => {
                                    if result.is_truthy() {
                                        results.push_back(*item);
                                    }
                                }
                                Built::Each => {} // side-effect only
                                _ => {}
                            }
                        }
                        Ok(self.alloc(Obj::Vec(results)))
                    }
                    _ => Ok(Val::UNIT),
                }
            }
            Built::Nth => {
                let coll = args.first().copied().unwrap_or(Val::UNIT);
                let idx = args.get(1).copied().unwrap_or(Val::int(0));
                match self.get_obj(coll) {
                    Some(Obj::Vec(items)) if idx.is_int() => Ok(items
                        .get(idx.as_int() as usize)
                        .copied()
                        .unwrap_or(Val::UNIT)),
                    _ => Ok(Val::UNIT),
                }
            }
            Built::Contains => {
                let coll = args.first().copied().unwrap_or(Val::UNIT);
                let val = args.get(1).copied().unwrap_or(Val::UNIT);
                match self.get_obj(coll) {
                    Some(Obj::Vec(items)) => Ok(Val::bool(items.contains(&val))),
                    Some(Obj::Set(items)) => Ok(Val::bool(items.contains(&val))),
                    Some(Obj::Map(map)) => Ok(Val::bool(
                        map.contains_key(&val) || {
                            let ks = self.get_str(val).map(|s| s.to_string());
                            ks.is_some() && map.iter().any(|(k, _)| {
                                let kk = self.get_str(*k).map(|s| s.to_string());
                                kk.is_some() && kk == ks
                            })
                        }
                    )),
                    Some(Obj::Str(s)) => {
                        // String contains substring
                        if let Some(needle) = self.get_str(val) {
                            Ok(Val::bool(s.contains(needle)))
                        } else {
                            Ok(Val::bool(false))
                        }
                    }
                    _ => Ok(Val::bool(false)),
                }
            }
            Built::Sort => {
                let coll = args.first().copied().unwrap_or(Val::UNIT);
                match self.get_obj(coll).cloned() {
                    Some(Obj::Vec(items)) => {
                        let mut v: Vec<Val> = items.into_iter().collect();
                        v.sort_by(|a, b| {
                            if a.is_int() && b.is_int() {
                                a.as_int().cmp(&b.as_int())
                            } else {
                                std::cmp::Ordering::Equal
                            }
                        });
                        Ok(self.alloc(Obj::Vec(v.into_iter().collect())))
                    }
                    _ => Ok(Val::UNIT),
                }
            }
            Built::Reverse => {
                let coll = args.first().copied().unwrap_or(Val::UNIT);
                match self.get_obj(coll).cloned() {
                    Some(Obj::Vec(items)) => {
                        let reversed: ImVec = items.into_iter().rev().collect();
                        Ok(self.alloc(Obj::Vec(reversed)))
                    }
                    _ => Ok(Val::UNIT),
                }
            }
            Built::Sum => {
                let coll = args.first().copied().unwrap_or(Val::UNIT);
                match self.get_obj(coll) {
                    Some(Obj::Vec(items)) => {
                        let sum: i64 = items
                            .iter()
                            .filter(|v| v.is_int())
                            .map(|v| v.as_int())
                            .sum();
                        Ok(Val::int(sum))
                    }
                    _ => Ok(Val::int(0)),
                }
            }
            Built::Min => {
                let coll = args.first().copied().unwrap_or(Val::UNIT);
                match self.get_obj(coll) {
                    Some(Obj::Vec(items)) => {
                        let min = items
                            .iter()
                            .filter(|v| v.is_int())
                            .map(|v| v.as_int())
                            .min();
                        Ok(min.map(Val::int).unwrap_or(Val::UNIT))
                    }
                    _ => Ok(Val::UNIT),
                }
            }
            Built::Max => {
                let coll = args.first().copied().unwrap_or(Val::UNIT);
                match self.get_obj(coll) {
                    Some(Obj::Vec(items)) => {
                        let max = items
                            .iter()
                            .filter(|v| v.is_int())
                            .map(|v| v.as_int())
                            .max();
                        Ok(max.map(Val::int).unwrap_or(Val::UNIT))
                    }
                    _ => Ok(Val::UNIT),
                }
            }
            Built::Cons => {
                let val = args.first().copied().unwrap_or(Val::UNIT);
                let coll = args.get(1).copied().unwrap_or(Val::UNIT);
                match self.get_obj(coll).cloned() {
                    Some(Obj::Vec(mut items)) => {
                        items.push_front(val);  // O(log n) with imbl
                        Ok(self.alloc(Obj::Vec(items)))
                    }
                    _ => Ok(Val::UNIT),
                }
            }
            Built::Merge => {
                let a = args.first().copied().unwrap_or(Val::UNIT);
                let b = args.get(1).copied().unwrap_or(Val::UNIT);
                match (self.get_obj(a).cloned(), self.get_obj(b).cloned()) {
                    (Some(Obj::Map(ma)), Some(Obj::Map(mb))) => {
                        let merged = ma.union(mb);  // O(n log n) with structural sharing
                        Ok(self.alloc(Obj::Map(merged)))
                    }
                    _ => Ok(Val::UNIT),
                }
            }
            Built::Take => {
                let n = args.first().copied().unwrap_or(Val::int(0));
                let coll = args.get(1).copied().unwrap_or(Val::UNIT);
                match self.get_obj(coll).cloned() {
                    Some(Obj::Vec(items)) if n.is_int() => {
                        let taken = items.take(n.as_int() as usize);
                        Ok(self.alloc(Obj::Vec(taken)))
                    }
                    _ => Ok(Val::UNIT),
                }
            }
            Built::Drop => {
                let n = args.first().copied().unwrap_or(Val::int(0));
                let coll = args.get(1).copied().unwrap_or(Val::UNIT);
                match self.get_obj(coll).cloned() {
                    Some(Obj::Vec(items)) if n.is_int() => {
                        let dropped = items.skip(n.as_int() as usize);
                        Ok(self.alloc(Obj::Vec(dropped)))
                    }
                    _ => Ok(Val::UNIT),
                }
            }
            Built::Split => {
                // [split text separator] — matches Loon convention
                let s = args.first().copied().unwrap_or(Val::UNIT);
                let sep = args.get(1).copied().unwrap_or(Val::UNIT);
                match (
                    self.get_str(sep).map(|s| s.to_string()),
                    self.get_str(s).map(|s| s.to_string()),
                ) {
                    (Some(sep), Some(s)) => {
                        let parts: ImVec = s
                            .split(&sep)
                            .map(|p| self.alloc(Obj::Str(p.to_string())))
                            .collect();
                        Ok(self.alloc(Obj::Vec(parts)))
                    }
                    _ => Ok(self.alloc(Obj::Vec(ImVec::new()))),
                }
            }
            Built::StartsWith => {
                let s = args.first().copied().unwrap_or(Val::UNIT);
                let prefix = args.get(1).copied().unwrap_or(Val::UNIT);
                match (
                    self.get_str(s).map(|s| s.to_string()),
                    self.get_str(prefix).map(|s| s.to_string()),
                ) {
                    (Some(s), Some(p)) => Ok(Val::bool(s.starts_with(&p))),
                    _ => Ok(Val::bool(false)),
                }
            }
            Built::EndsWith => {
                let s = args.first().copied().unwrap_or(Val::UNIT);
                let suffix = args.get(1).copied().unwrap_or(Val::UNIT);
                match (
                    self.get_str(s).map(|s| s.to_string()),
                    self.get_str(suffix).map(|s| s.to_string()),
                ) {
                    (Some(s), Some(p)) => Ok(Val::bool(s.ends_with(&p))),
                    _ => Ok(Val::bool(false)),
                }
            }
            Built::Replace => {
                let from = args.first().copied().unwrap_or(Val::UNIT);
                let to = args.get(1).copied().unwrap_or(Val::UNIT);
                let s = args.get(2).copied().unwrap_or(Val::UNIT);
                match (
                    self.get_str(from).map(|s| s.to_string()),
                    self.get_str(to).map(|s| s.to_string()),
                    self.get_str(s).map(|s| s.to_string()),
                ) {
                    (Some(f), Some(t), Some(s)) => Ok(self.alloc(Obj::Str(s.replace(&f, &t)))),
                    _ => Ok(Val::UNIT),
                }
            }
            Built::Uppercase => {
                let s = args.first().copied().unwrap_or(Val::UNIT);
                match self.get_str(s).map(|s| s.to_string()) {
                    Some(s) => Ok(self.alloc(Obj::Str(s.to_uppercase()))),
                    _ => Ok(Val::UNIT),
                }
            }
            Built::Lowercase => {
                let s = args.first().copied().unwrap_or(Val::UNIT);
                match self.get_str(s).map(|s| s.to_string()) {
                    Some(s) => Ok(self.alloc(Obj::Str(s.to_lowercase()))),
                    _ => Ok(Val::UNIT),
                }
            }
            Built::Not => {
                let v = args.first().copied().unwrap_or(Val::UNIT);
                Ok(Val::bool(!v.is_truthy()))
            }
            Built::Keys => {
                let m = args.first().copied().unwrap_or(Val::UNIT);
                match self.get_obj(m).cloned() {
                    Some(Obj::Map(map)) => {
                        let keys: ImVec = map.keys().copied().collect();
                        Ok(self.alloc(Obj::Vec(keys)))
                    }
                    _ => Ok(self.alloc(Obj::Vec(ImVec::new()))),
                }
            }
            Built::Vals => {
                let m = args.first().copied().unwrap_or(Val::UNIT);
                match self.get_obj(m).cloned() {
                    Some(Obj::Map(map)) => {
                        let vals: ImVec = map.values().copied().collect();
                        Ok(self.alloc(Obj::Vec(vals)))
                    }
                    _ => Ok(self.alloc(Obj::Vec(ImVec::new()))),
                }
            }
            Built::Join => {
                let sep = args.first().copied().unwrap_or(Val::UNIT);
                let coll = args.get(1).copied().unwrap_or(Val::UNIT);
                let sep_str = self.val_to_string(sep);
                match self.get_obj(coll).cloned() {
                    Some(Obj::Vec(items)) => {
                        let parts: Vec<String> =
                            items.iter().map(|v| self.val_to_string(*v)).collect();
                        Ok(self.alloc(Obj::Str(parts.join(&sep_str))))
                    }
                    _ => Ok(self.alloc(Obj::Str(String::new()))),
                }
            }
            Built::Trim => {
                let s = args.first().copied().unwrap_or(Val::UNIT);
                match self.get_str(s).map(|s| s.to_string()) {
                    Some(s) => Ok(self.alloc(Obj::Str(s.trim().to_string()))),
                    _ => Ok(Val::UNIT),
                }
            }
            Built::Flatten => {
                let coll = args.first().copied().unwrap_or(Val::UNIT);
                match self.get_obj(coll).cloned() {
                    Some(Obj::Vec(items)) => {
                        let mut flat = ImVec::new();
                        for item in items {
                            if let Some(Obj::Vec(inner)) = self.get_obj(item).cloned() {
                                flat.append(inner);
                            } else {
                                flat.push_back(item);
                            }
                        }
                        Ok(self.alloc(Obj::Vec(flat)))
                    }
                    _ => Ok(Val::UNIT),
                }
            }
            Built::Zip => {
                let a = args.first().copied().unwrap_or(Val::UNIT);
                let b = args.get(1).copied().unwrap_or(Val::UNIT);
                match (self.get_obj(a).cloned(), self.get_obj(b).cloned()) {
                    (Some(Obj::Vec(va)), Some(Obj::Vec(vb))) => {
                        let zipped: ImVec = va
                            .into_iter()
                            .zip(vb)
                            .map(|(x, y)| self.alloc(Obj::Tuple(vec![x, y])))
                            .collect();
                        Ok(self.alloc(Obj::Vec(zipped)))
                    }
                    _ => Ok(self.alloc(Obj::Vec(ImVec::new()))),
                }
            }
            Built::Any => {
                let func = args.first().copied().unwrap_or(Val::UNIT);
                let coll = args.get(1).copied().unwrap_or(Val::UNIT);
                match (self.get_obj(func).cloned(), self.get_obj(coll).cloned()) {
                    (Some(Obj::Closure(fid, caps)), Some(Obj::Vec(items))) => {
                        for item in &items {
                            let result =
                                self.run_call_with_captures(fid, &[*item], caps.clone())?;
                            if result.is_truthy() {
                                return Ok(Val::TRUE);
                            }
                        }
                        Ok(Val::FALSE)
                    }
                    _ => Ok(Val::FALSE),
                }
            }
            Built::All => {
                let func = args.first().copied().unwrap_or(Val::UNIT);
                let coll = args.get(1).copied().unwrap_or(Val::UNIT);
                match (self.get_obj(func).cloned(), self.get_obj(coll).cloned()) {
                    (Some(Obj::Closure(fid, caps)), Some(Obj::Vec(items))) => {
                        for item in &items {
                            let result =
                                self.run_call_with_captures(fid, &[*item], caps.clone())?;
                            if !result.is_truthy() {
                                return Ok(Val::FALSE);
                            }
                        }
                        Ok(Val::TRUE)
                    }
                    _ => Ok(Val::TRUE),
                }
            }
            Built::Int => {
                let v = args.first().copied().unwrap_or(Val::UNIT);
                if v.is_int() {
                    Ok(v)
                } else if v.is_float() {
                    Ok(self.safe_int(v.as_float() as i64))
                } else {
                    Ok(Val::UNIT)
                }
            }
            Built::Float => {
                let v = args.first().copied().unwrap_or(Val::UNIT);
                if v.is_float() {
                    Ok(v)
                } else if v.is_int() {
                    Ok(Val::float(v.as_int() as f64))
                } else {
                    Ok(Val::UNIT)
                }
            }
            Built::Empty => {
                let v = args.first().copied().unwrap_or(Val::UNIT);
                let empty = match self.get_obj(v) {
                    Some(Obj::Vec(items)) => items.is_empty(),
                    Some(Obj::Map(map)) => map.is_empty(),
                    Some(Obj::Set(set)) => set.is_empty(),
                    Some(Obj::Str(s)) => s.is_empty(),
                    _ => true,
                };
                Ok(Val::bool(empty))
            }
            Built::Or => {
                // [or a b] — return first truthy, or last
                let a = args.first().copied().unwrap_or(Val::UNIT);
                let b = args.get(1).copied().unwrap_or(Val::UNIT);
                Ok(if a.is_truthy() { a } else { b })
            }
            Built::Fold => {
                // [fold init f coll] or [fold init f coll]
                let init = args.first().copied().unwrap_or(Val::UNIT);
                let func = args.get(1).copied().unwrap_or(Val::UNIT);
                let coll = args.get(2).copied().unwrap_or(Val::UNIT);
                match (self.get_obj(func).cloned(), self.get_obj(coll).cloned()) {
                    (Some(Obj::Closure(fid, caps)), Some(Obj::Vec(items))) => {
                        let mut acc = init;
                        for item in &items {
                            acc = self.run_call_with_captures(fid, &[acc, *item], caps.clone())?;
                        }
                        Ok(acc)
                    }
                    _ => Ok(init),
                }
            }
            Built::Update => {
                // [update map key f] — apply f to map[key], store result
                let map_val = args.first().copied().unwrap_or(Val::UNIT);
                let key = args.get(1).copied().unwrap_or(Val::UNIT);
                let func = args.get(2).copied().unwrap_or(Val::UNIT);
                match (self.get_obj(map_val).cloned(), self.get_obj(func).cloned()) {
                    (Some(Obj::Map(mut map)), Some(Obj::Closure(fid, caps))) => {
                        let old_val = map.get(&key).copied().unwrap_or(Val::UNIT);
                        let new_val = self.run_call_with_captures(fid, &[old_val], caps)?;
                        map.insert(key, new_val);
                        Ok(self.alloc(Obj::Map(map)))
                    }
                    _ => Ok(map_val),
                }
            }
            Built::Entries => {
                // [entries map] → vector of [key value] tuples
                let m = args.first().copied().unwrap_or(Val::UNIT);
                match self.get_obj(m).cloned() {
                    Some(Obj::Map(map)) => {
                        let entries: ImVec = map
                            .into_iter()
                            .map(|(k, v)| self.alloc(Obj::Tuple(vec![k, v])))
                            .collect();
                        Ok(self.alloc(Obj::Vec(entries)))
                    }
                    _ => Ok(self.alloc(Obj::Vec(ImVec::new()))),
                }
            }
            Built::SortBy => {
                // [sort-by f coll] or [sort-by f :desc coll]
                let func = args.first().copied().unwrap_or(Val::UNIT);
                let coll = args.last().copied().unwrap_or(Val::UNIT);
                match (self.get_obj(func).cloned(), self.get_obj(coll).cloned()) {
                    (Some(Obj::Closure(fid, caps)), Some(Obj::Vec(items))) => {
                        // Compute sort keys
                        let mut keyed: Vec<(Val, Val)> = Vec::new();
                        for item in &items {
                            let key = self.run_call_with_captures(fid, &[*item], caps.clone())?;
                            keyed.push((key, *item));
                        }
                        // Check for :desc flag
                        let desc = args.get(1).is_some_and(|v| {
                            v.is_sym()
                                && self
                                    .module
                                    .strings
                                    .get(v.as_sym() as usize)
                                    .is_some_and(|s| s == "desc")
                        });
                        keyed.sort_by(|(a, _), (b, _)| {
                            let ord = if a.is_int() && b.is_int() {
                                a.as_int().cmp(&b.as_int())
                            } else {
                                std::cmp::Ordering::Equal
                            };
                            if desc {
                                ord.reverse()
                            } else {
                                ord
                            }
                        });
                        let sorted: ImVec = keyed.into_iter().map(|(_, v)| v).collect();
                        Ok(self.alloc(Obj::Vec(sorted)))
                    }
                    _ => Ok(coll),
                }
            }
            Built::Unit => {
                // [unit value :dimension] — at runtime, just return the value
                // (dimensions are compile-time only)
                Ok(args.first().copied().unwrap_or(Val::UNIT))
            }
            Built::Magnitude => {
                // [magnitude dimensioned-value] — extract the numeric value
                Ok(args.first().copied().unwrap_or(Val::UNIT))
            }
            Built::FlatMap => {
                // [flat-map f coll] → map then flatten
                let func = args.first().copied().unwrap_or(Val::UNIT);
                let coll = args.get(1).copied().unwrap_or(Val::UNIT);
                match (self.get_obj(func).cloned(), self.get_obj(coll).cloned()) {
                    (Some(Obj::Closure(fid, caps)), Some(Obj::Vec(items))) => {
                        let mut results = ImVec::new();
                        for item in &items {
                            let result =
                                self.run_call_with_captures(fid, &[*item], caps.clone())?;
                            if let Some(Obj::Vec(inner)) = self.get_obj(result).cloned() {
                                results.append(inner);
                            } else {
                                results.push_back(result);
                            }
                        }
                        Ok(self.alloc(Obj::Vec(results)))
                    }
                    _ => Ok(Val::UNIT),
                }
            }
            Built::GroupBy => {
                // [group-by f coll] → map of key → vec
                let func = args.first().copied().unwrap_or(Val::UNIT);
                let coll = args.get(1).copied().unwrap_or(Val::UNIT);
                match (self.get_obj(func).cloned(), self.get_obj(coll).cloned()) {
                    (Some(Obj::Closure(fid, caps)), Some(Obj::Vec(items))) => {
                        // Build groups using a temporary HashMap for O(1) lookup
                        let mut groups: HashMap<Val, Vec<Val>> = HashMap::new();
                        let mut order: Vec<Val> = Vec::new();
                        for item in &items {
                            let key = self.run_call_with_captures(fid, &[*item], caps.clone())?;
                            let entry = groups.entry(key);
                            if matches!(entry, std::collections::hash_map::Entry::Vacant(_)) {
                                order.push(key);
                            }
                            entry.or_default().push(*item);
                        }
                        // Convert to ImMap of key → ImVec
                        let mut map = ImMap::new();
                        for k in order {
                            if let Some(vals) = groups.remove(&k) {
                                let v: ImVec = vals.into_iter().collect();
                                let v = self.alloc(Obj::Vec(v));
                                map.insert(k, v);
                            }
                        }
                        Ok(self.alloc(Obj::Map(map)))
                    }
                    _ => Ok(self.alloc(Obj::Map(ImMap::new()))),
                }
            }
            Built::Collect => {
                // [collect coll] → evaluate lazy collection (identity for eager)
                Ok(args.first().copied().unwrap_or(Val::UNIT))
            }
            Built::IntoMap => {
                // [into-map pairs] → convert vec of tuples to map
                let coll = args.first().copied().unwrap_or(Val::UNIT);
                match self.get_obj(coll).cloned() {
                    Some(Obj::Vec(items)) => {
                        let mut map = ImMap::new();
                        for item in &items {
                            if let Some(Obj::Tuple(fields)) = self.get_obj(*item) {
                                if fields.len() >= 2 {
                                    map.insert(fields[0], fields[1]);
                                }
                            }
                        }
                        Ok(self.alloc(Obj::Map(map)))
                    }
                    _ => Ok(self.alloc(Obj::Map(ImMap::new()))),
                }
            }
            Built::Chunk => {
                // [chunk n coll] → split into groups of n
                let n = args.first().copied().unwrap_or(Val::int(1));
                let coll = args.get(1).copied().unwrap_or(Val::UNIT);
                match self.get_obj(coll).cloned() {
                    Some(Obj::Vec(items)) if n.is_int() && n.as_int() > 0 => {
                        let chunk_size = n.as_int() as usize;
                        let mut chunks = ImVec::new();
                        let mut i = 0;
                        let len = items.len();
                        while i < len {
                            let end = (i + chunk_size).min(len);
                            let chunk = items.clone().slice(i..end);
                            chunks.push_back(self.alloc(Obj::Vec(chunk)));
                            i = end;
                        }
                        Ok(self.alloc(Obj::Vec(chunks)))
                    }
                    _ => Ok(self.alloc(Obj::Vec(ImVec::new()))),
                }
            }
            Built::IndexOf => {
                // [index-of str substr] → first index or -1
                let s = args.first().copied().unwrap_or(Val::UNIT);
                let sub = args.get(1).copied().unwrap_or(Val::UNIT);
                match (
                    self.get_str(s).map(|s| s.to_string()),
                    self.get_str(sub).map(|s| s.to_string()),
                ) {
                    (Some(s), Some(sub)) => {
                        let idx = s.find(&sub).map(|i| i as i64).unwrap_or(-1);
                        Ok(Val::int(idx))
                    }
                    _ => Ok(Val::int(-1)),
                }
            }
            Built::CharAt => {
                // [char-at str index] → character at position
                let s = args.first().copied().unwrap_or(Val::UNIT);
                let idx = args.get(1).copied().unwrap_or(Val::int(0));
                match self.get_str(s).map(|s| s.to_string()) {
                    Some(s) if idx.is_int() => {
                        let i = idx.as_int() as usize;
                        if let Some(c) = s.chars().nth(i) {
                            Ok(self.alloc(Obj::Str(c.to_string())))
                        } else {
                            Ok(self.alloc(Obj::Str(String::new())))
                        }
                    }
                    _ => Ok(self.alloc(Obj::Str(String::new()))),
                }
            }
            Built::Substring => {
                // [substring str start end] → slice string
                let s = args.first().copied().unwrap_or(Val::UNIT);
                let start = args.get(1).copied().unwrap_or(Val::int(0));
                let end = args.get(2).copied().unwrap_or(Val::int(0));
                match self.get_str(s).map(|s| s.to_string()) {
                    Some(s) if start.is_int() && end.is_int() => {
                        let st = start.as_int().max(0) as usize;
                        let en = end.as_int().max(0) as usize;
                        let chars: Vec<char> = s.chars().collect();
                        let en = en.min(chars.len());
                        let st = st.min(en);
                        let sub: String = chars[st..en].iter().collect();
                        Ok(self.alloc(Obj::Str(sub)))
                    }
                    _ => Ok(self.alloc(Obj::Str(String::new()))),
                }
            }
            Built::Slice => {
                // [slice coll start end] → slice collection
                let coll = args.first().copied().unwrap_or(Val::UNIT);
                let start = args.get(1).copied().unwrap_or(Val::int(0));
                let end = args.get(2).copied().unwrap_or(Val::int(0));
                match self.get_obj(coll).cloned() {
                    Some(Obj::Vec(items)) if start.is_int() && end.is_int() => {
                        let st = start.as_int().max(0) as usize;
                        let en = end.as_int().max(0) as usize;
                        let en = en.min(items.len());
                        let st = st.min(en);
                        Ok(self.alloc(Obj::Vec(items.clone().slice(st..en))))
                    }
                    Some(Obj::Str(s)) if start.is_int() && end.is_int() => {
                        let st = start.as_int().max(0) as usize;
                        let en = end.as_int().max(0) as usize;
                        let chars: Vec<char> = s.chars().collect();
                        let en = en.min(chars.len());
                        let st = st.min(en);
                        let sub: String = chars[st..en].iter().collect();
                        Ok(self.alloc(Obj::Str(sub)))
                    }
                    _ => Ok(Val::UNIT),
                }
            }
            Built::Abs => {
                let v = args.first().copied().unwrap_or(Val::UNIT);
                if v.is_int() {
                    Ok(Val::int(v.as_int().abs()))
                } else if v.is_float() {
                    Ok(Val::float(v.as_float().abs()))
                } else {
                    Ok(Val::UNIT)
                }
            }
            Built::First => {
                let coll = args.first().copied().unwrap_or(Val::UNIT);
                match self.get_obj(coll) {
                    Some(Obj::Vec(items)) => Ok(items.front().copied().unwrap_or(Val::UNIT)),
                    Some(Obj::Tuple(items)) => Ok(items.first().copied().unwrap_or(Val::UNIT)),
                    _ => Ok(Val::UNIT),
                }
            }
            Built::Last => {
                let coll = args.first().copied().unwrap_or(Val::UNIT);
                match self.get_obj(coll) {
                    Some(Obj::Vec(items)) => Ok(items.back().copied().unwrap_or(Val::UNIT)),
                    Some(Obj::Tuple(items)) => Ok(items.last().copied().unwrap_or(Val::UNIT)),
                    _ => Ok(Val::UNIT),
                }
            }
            Built::Find => {
                // [find f coll] → first element where f returns truthy
                let func = args.first().copied().unwrap_or(Val::UNIT);
                let coll = args.get(1).copied().unwrap_or(Val::UNIT);
                match (self.get_obj(func).cloned(), self.get_obj(coll).cloned()) {
                    (Some(Obj::Closure(fid, caps)), Some(Obj::Vec(items))) => {
                        for item in &items {
                            let result =
                                self.run_call_with_captures(fid, &[*item], caps.clone())?;
                            if result.is_truthy() {
                                return Ok(*item);
                            }
                        }
                        Ok(Val::UNIT)
                    }
                    _ => Ok(Val::UNIT),
                }
            }
            Built::Keyword => {
                // [keyword str] → keyword from string
                let v = args.first().copied().unwrap_or(Val::UNIT);
                match self.get_str(v).map(|s| s.to_string()) {
                    Some(s) => Ok(Val::sym(
                        self.module
                            .strings
                            .iter()
                            .position(|x| *x == s)
                            .unwrap_or(0) as u32,
                    )),
                    _ => Ok(Val::UNIT),
                }
            }
            Built::KeywordizeKeys => {
                // [keywordize-keys map] → convert string keys to keywords
                let m = args.first().copied().unwrap_or(Val::UNIT);
                match self.get_obj(m).cloned() {
                    Some(Obj::Map(map)) => {
                        let mut new_map = ImMap::new();
                        for (k, v) in map {
                            if let Some(s) = self.get_str(k).map(|s| s.to_string()) {
                                let sym_id = self
                                    .module
                                    .strings
                                    .iter()
                                    .position(|x| *x == s)
                                    .unwrap_or(0)
                                    as u32;
                                new_map.insert(Val::sym(sym_id), v);
                            } else {
                                new_map.insert(k, v);
                            }
                        }
                        Ok(self.alloc(Obj::Map(new_map)))
                    }
                    _ => Ok(Val::UNIT),
                }
            }
            Built::AssertEq => {
                // [assert-eq actual expected] → Unit or panic
                let actual = args.first().copied().unwrap_or(Val::UNIT);
                let expected = args.get(1).copied().unwrap_or(Val::UNIT);
                if actual == expected {
                    Ok(Val::UNIT)
                } else {
                    let actual_s = self.val_to_string(actual);
                    let expected_s = self.val_to_string(expected);
                    Err(
                        VmError::new(VmErrorKind::AssertFailed(actual_s, expected_s))
                            .with_span(self.current_span),
                    )
                }
            }
            Built::Concat => {
                // [concat coll1 coll2] → concatenate collections
                let a = args.first().copied().unwrap_or(Val::UNIT);
                let b = args.get(1).copied().unwrap_or(Val::UNIT);
                match (self.get_obj(a).cloned(), self.get_obj(b).cloned()) {
                    (Some(Obj::Vec(mut va)), Some(Obj::Vec(vb))) => {
                        va.append(vb);
                        Ok(self.alloc(Obj::Vec(va)))
                    }
                    (Some(Obj::Str(sa)), Some(Obj::Str(sb))) => {
                        Ok(self.alloc(Obj::Str(format!("{sa}{sb}"))))
                    }
                    _ => Ok(Val::UNIT),
                }
            }
        }
    }

    // ── Effect builtins ────────────────────────────────────────────────

    fn builtin_effect(&mut self, effect: &str, op: &str, args: &[Val]) -> Val {
        match (effect, op) {
            ("IO", "println") => {
                let s: Vec<String> = args.iter().map(|v| self.val_to_string(*v)).collect();
                let line = s.join(" ");
                println!("{line}");
                self.output.push(line);
                Val::UNIT
            }
            ("IO", "read-file") => {
                if let Some(path) = args.first() {
                    let path_str = self.val_to_string(*path);
                    match std::fs::read_to_string(&path_str) {
                        Ok(contents) => self.alloc(Obj::Str(contents)),
                        Err(_) => Val::UNIT,
                    }
                } else {
                    Val::UNIT
                }
            }
            ("Const", "c") => Val::float(299_792_458.0),
            ("Physics", "yield-strength") => Val::float(250.0),
            ("Physics", "gravity") => Val::float(9.80665),
            _ => Val::UNIT,
        }
    }

    // ── Value display ──────────────────────────────────────────────────

    fn val_to_string(&self, val: Val) -> String {
        if val.is_float() {
            let f = val.as_float();
            if f == (f as i64) as f64 && f.abs() < 1e15 {
                format!("{}", f as i64)
            } else {
                format!("{f}")
            }
        } else if val.is_int() {
            format!("{}", val.as_int())
        } else if val.is_bool() {
            if val.as_bool() {
                "true".to_string()
            } else {
                "false".to_string()
            }
        } else if val.is_unit() {
            "()".to_string()
        } else if val.is_sym() {
            let idx = val.as_sym() as usize;
            if idx < self.module.strings.len() {
                format!(":{}", self.module.strings[idx])
            } else {
                format!(":<sym:{idx}>")
            }
        } else if val.is_ptr() {
            match self.get_obj(val) {
                Some(Obj::Str(s)) => s.clone(),
                Some(Obj::Vec(items)) => {
                    let inner: Vec<String> = items.iter().map(|v| self.val_to_string(*v)).collect();
                    format!("#[{}]", inner.join(" "))
                }
                Some(Obj::Map(map)) => {
                    let inner: Vec<String> = map
                        .iter()
                        .map(|(k, v)| {
                            format!("{} {}", self.val_to_string(*k), self.val_to_string(*v))
                        })
                        .collect();
                    format!("{{{}}}", inner.join(" "))
                }
                Some(Obj::Adt(tag, fields)) => {
                    let name = self
                        .module
                        .ctors
                        .iter()
                        .find(|c| c.tag == *tag)
                        .map(|c| c.name.as_str())
                        .unwrap_or("?");
                    if fields.is_empty() {
                        name.to_string()
                    } else {
                        let inner: Vec<String> =
                            fields.iter().map(|v| self.val_to_string(*v)).collect();
                        format!("[{name} {}]", inner.join(" "))
                    }
                }
                Some(Obj::Closure(fid, _caps)) => {
                    let name = self
                        .module
                        .funcs
                        .get(fid.0 as usize)
                        .and_then(|f| f.name.as_deref())
                        .unwrap_or("anon");
                    format!("<fn:{name}>")
                }
                _ => format!("<obj:{}>", val.as_ptr()),
            }
        } else {
            format!("<val:0x{:016x}>", val.bits())
        }
    }

    fn alloc_str_owned(&mut self, s: String) -> Val {
        self.alloc(Obj::Str(s))
    }
}

// ─── Error type ────────────────────────────────────────────────────────────

/// Runtime error from the EIR VM, with optional source location.
#[derive(Debug)]
pub struct VmError {
    /// What went wrong.
    pub kind: VmErrorKind,
    /// Source span of the instruction that failed (if available).
    pub span: Option<Span>,
    /// Human-readable context, e.g. function name.
    pub context: Option<String>,
}

/// The category of VM runtime error.
#[derive(Debug)]
pub enum VmErrorKind {
    /// Tried to call a value that is not a closure or function.
    NotCallable,
    /// Hit an unreachable `End::Trap` (non-exhaustive match, etc.).
    Trap,
    /// Call stack exceeded the limit.
    StackOverflow,
    /// `assert-eq` failed with mismatched values.
    AssertFailed(String, String),
}

impl VmError {
    fn new(kind: VmErrorKind) -> Self {
        Self {
            kind,
            span: None,
            context: None,
        }
    }

    fn with_span(mut self, span: Span) -> Self {
        if span != Span::ZERO {
            self.span = Some(span);
        }
        self
    }
}

impl std::fmt::Display for VmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            VmErrorKind::NotCallable => write!(f, "value is not callable"),
            VmErrorKind::Trap => write!(f, "unreachable code"),
            VmErrorKind::StackOverflow => write!(f, "stack overflow"),
            VmErrorKind::AssertFailed(actual, expected) => {
                write!(f, "assertion failed: {actual} != {expected}")
            }
        }
    }
}

// ─── Public API ────────────────────────────────────────────────────────────

/// Run a Loon program through the EIR pipeline: parse → check → lower → VM.
pub fn eval_eir(src: &str) -> Result<VmResult, VmError> {
    let exprs = crate::parser::parse(src).map_err(|e| VmError {
        kind: VmErrorKind::Trap,
        span: Some(e.span),
        context: Some(format!("parse error: {}", e.message)),
    })?;
    let mut checker = crate::check::Checker::new();
    let _errors = checker.check_program(&exprs);
    let module = crate::eir::lower::lower(&checker);
    let mut vm = Vm::new(module);
    vm.run()
}

// ─── Tests ─────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn run(src: &str) -> Val {
        eval_eir(src).expect("vm error").value
    }

    fn run_output(src: &str) -> Vec<String> {
        eval_eir(src).expect("vm error").output
    }

    #[test]
    fn vm_int_literal() {
        let v = run("42");
        assert!(v.is_int());
        assert_eq!(v.as_int(), 42);
    }

    #[test]
    fn vm_arithmetic() {
        let v = run("[+ 1 2]");
        assert!(v.is_int());
        assert_eq!(v.as_int(), 3);

        let v = run("[* 4 5]");
        assert_eq!(v.as_int(), 20);

        let v = run("[- 10 3]");
        assert_eq!(v.as_int(), 7);
    }

    #[test]
    fn vm_comparison() {
        assert!(run("[> 3 2]").as_bool());
        assert!(!run("[< 3 2]").as_bool());
        assert!(run("[= 1 1]").as_bool());
    }

    #[test]
    fn vm_structural_equality() {
        // Strings compare by content, not by heap identity: two independently
        // computed strings are equal (this used to be false — pointer equality).
        assert!(run(r#"[= "ab" [str "a" "b"]]"#).as_bool());
        assert!(run(r#"[= [str "a" "b"] [str "a" "b"]]"#).as_bool());
        assert!(!run(r#"[= "ab" "ba"]"#).as_bool());
        // Aggregates are structural and recursive.
        assert!(run("[= #[1 2 3] #[1 2 3]]").as_bool());
        assert!(!run("[= #[1 2] #[1 2 3]]").as_bool());
        assert!(run("[= #[#[1] #[2]] #[#[1] #[2]]]").as_bool());
        assert!(run("[= {:a 1 :b 2} {:b 2 :a 1}]").as_bool());
        assert!(!run("[= {:a 1} {:a 2}]").as_bool());
        assert!(run("[type T [C Int]] [= [C 1] [C 1]]").as_bool());
        assert!(!run("[type T [C Int]] [= [C 1] [C 2]]").as_bool());
        // `!=` is the negation.
        assert!(run(r#"[!= "ab" [str "a" "c"]]"#).as_bool());
    }

    #[test]
    fn vm_if() {
        let v = run("[if true 1 2]");
        assert_eq!(v.as_int(), 1);
        let v = run("[if false 1 2]");
        assert_eq!(v.as_int(), 2);
    }

    #[test]
    fn vm_let() {
        let v = run("[do [let x 42] x]");
        assert_eq!(v.as_int(), 42);
    }

    #[test]
    fn vm_function() {
        let v = run("[fn add [x y] [+ x y]] [add 3 4]");
        assert_eq!(v.as_int(), 7);
    }

    #[test]
    fn vm_println() {
        let out = run_output(r#"[println "hello"]"#);
        assert_eq!(out, vec!["hello"]);
    }

    #[test]
    fn vm_loop_recur() {
        let v = run(r#"
            [loop [i 0 sum 0]
              [if [>= i 10] sum
                [recur [+ i 1] [+ sum i]]]]
        "#);
        assert_eq!(v.as_int(), 45);
    }

    #[test]
    fn vm_adt() {
        let v = run(r#"
            [type Shape [Circle Int] Point]
            [Circle 5]
        "#);
        assert!(v.is_ptr()); // ADT is a heap object
    }

    #[test]
    fn vm_vec() {
        let v = run("[len #[1 2 3]]");
        assert_eq!(v.as_int(), 3);
    }

    #[test]
    fn vm_range() {
        let v = run("[len [range 0 5]]");
        assert_eq!(v.as_int(), 5);
    }

    #[test]
    fn vm_nested_calls() {
        let v = run("[+ [+ 1 2] [+ 3 4]]");
        assert_eq!(v.as_int(), 10);
    }

    #[test]
    fn vm_bool_logic() {
        assert!(run("[and true true]").as_bool());
        assert!(!run("[and true false]").as_bool());
        assert!(run("[or false true]").as_bool());
        assert!(!run("[not true]").as_bool());
    }

    #[test]
    fn vm_float() {
        let v = run("[+ 1.5 2.5]");
        assert!(v.is_float());
        assert_eq!(v.as_float(), 4.0);
    }

    #[test]
    fn vm_evidence_effect() {
        // Evidence-passing for effects needs closure calling to work fully.
        // For now, verify that non-handled effects use builtin handlers.
        let out = run_output(r#"[IO.println "hello from eir"]"#);
        assert_eq!(out, vec!["hello from eir"]);
    }
}
