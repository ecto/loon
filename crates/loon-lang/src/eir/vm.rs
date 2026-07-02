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
type ImSet = imbl::HashSet<Val>;
type ImMap = OrdMap;

/// An insertion-ordered persistent map. `keys`, iteration, and display follow
/// the order keys were first inserted (deterministic), rather than hash order.
/// Lookup stays O(1) via the inner `HashMap`; an `imbl::Vector` of keys records
/// order. Both halves are persistent, so clone/share is still cheap. Mirrors the
/// subset of `imbl::HashMap`'s API the VM uses, so it's a drop-in for the alias.
#[derive(Debug, Clone, Default)]
struct OrdMap {
    map: imbl::HashMap<Val, Val>,
    order: imbl::Vector<Val>, // keys, insertion order, no duplicates
}

impl OrdMap {
    fn new() -> Self {
        Self::default()
    }
    fn len(&self) -> usize {
        self.order.len()
    }
    fn is_empty(&self) -> bool {
        self.order.is_empty()
    }
    fn get(&self, k: &Val) -> Option<&Val> {
        self.map.get(k)
    }
    fn contains_key(&self, k: &Val) -> bool {
        self.map.contains_key(k)
    }
    /// Insert; a brand-new key is appended to the order, an existing key keeps
    /// its position (only its value updates). Returns the previous value, if any.
    fn insert(&mut self, k: Val, v: Val) -> Option<Val> {
        let prev = self.map.insert(k, v);
        if prev.is_none() {
            self.order.push_back(k);
        }
        prev
    }
    fn keys(&self) -> impl Iterator<Item = &Val> + '_ {
        self.order.iter()
    }
    fn values(&self) -> impl Iterator<Item = &Val> + '_ {
        self.order.iter().map(move |k| self.map.get(k).unwrap())
    }
    fn iter(&self) -> impl Iterator<Item = (&Val, &Val)> + '_ {
        self.order
            .iter()
            .map(move |k| (k, self.map.get(k).unwrap()))
    }
    /// Left-biased union (values already in `self` win), preserving `self`'s
    /// order and appending `other`'s new keys in their order.
    fn union(&self, other: Self) -> Self {
        let mut out = self.clone();
        for k in other.order.iter() {
            if !out.map.contains_key(k) {
                out.insert(*k, *other.map.get(k).unwrap());
            }
        }
        out
    }
}

impl FromIterator<(Val, Val)> for OrdMap {
    fn from_iter<I: IntoIterator<Item = (Val, Val)>>(iter: I) -> Self {
        let mut m = OrdMap::new();
        for (k, v) in iter {
            m.insert(k, v);
        }
        m
    }
}

impl IntoIterator for OrdMap {
    type Item = (Val, Val);
    type IntoIter = std::vec::IntoIter<(Val, Val)>;
    fn into_iter(self) -> Self::IntoIter {
        self.order
            .iter()
            .map(|k| (*k, *self.map.get(k).unwrap()))
            .collect::<Vec<_>>()
            .into_iter()
    }
}

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
    /// A reified multi-shot delimited continuation captured at a `perform`: the
    /// frame segment between the perform and its handler's prompt, plus the
    /// execution point to resume at. `resume_continuation` clones this segment on
    /// every invocation, so the same continuation may be resumed zero, one, or
    /// many times (multi-shot) — the functional substrate the agent framework's
    /// backtracking needs. As with the WASM/CPS backend, soundness holds for the
    /// functional case; a re-resumed segment that mutates shared heap state in
    /// place would observe that sharing.
    Continuation {
        saved: Vec<Frame>,
        func: FuncId,
        block: BlockId,
        ip: usize,
        regs: Vec<Val>,
        captures: Vec<Val>,
        perform_dst: u32,
        /// The handle's handlers, so a continuation resumed AFTER its `handle`
        /// has exited (an escaping continuation) can re-establish them.
        prompt_handlers: Vec<DynHandler>,
    },
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
            Obj::Continuation {
                saved,
                regs,
                captures,
                ..
            } => (48 + saved.len() * 64 + regs.len() * 8 + captures.len() * 8) as u64,
        }
    }
}

// ─── Call frame ────────────────────────────────────────────────────────────

/// Saved call frame — pushed when entering a function, popped on return.
#[derive(Clone, Debug)]
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

/// Generate a v4-format UUID string using only std (no optional `uuid` dep, so
/// host `IO.uuid` works in every build). 122 bits come from the wall clock and
/// a process-global counter; the version (4) and variant bits are set so the
/// shape is a valid v4 UUID. Not cryptographically random, fine for ids/keys.
fn gen_uuid_v4() -> String {
    use std::sync::atomic::{AtomicU64, Ordering};
    static COUNTER: AtomicU64 = AtomicU64::new(0);
    let nanos = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_nanos() as u64)
        .unwrap_or(0);
    let c = COUNTER.fetch_add(0x9E37_79B9_7F4A_7C15, Ordering::Relaxed);
    // Mix the two 64-bit sources (splitmix-style) into two halves.
    let mut hi = nanos
        .wrapping_mul(0xD1B5_4A32_D192_ED03)
        .rotate_left(31)
        .wrapping_add(c);
    let mut lo = c
        .wrapping_mul(0x94D0_49BB_1331_11EB)
        .rotate_left(29)
        .wrapping_add(nanos);
    // Set version (4) and variant (RFC 4122) bits.
    hi = (hi & 0xFFFF_FFFF_FFFF_0FFF) | 0x0000_0000_0000_4000;
    lo = (lo & 0x3FFF_FFFF_FFFF_FFFF) | 0x8000_0000_0000_0000;
    format!(
        "{:08x}-{:04x}-{:04x}-{:04x}-{:012x}",
        hi >> 32,
        (hi >> 16) & 0xFFFF,
        hi & 0xFFFF,
        lo >> 48,
        lo & 0xFFFF_FFFF_FFFF
    )
}

// ─── VM ────────────────────────────────────────────────────────────────────

/// Dynamic effect handler entry.
#[derive(Clone, Debug)]
struct DynHandler {
    /// String pool index for the effect name.
    effect: StringId,
    /// String pool index for the operation name.
    op: StringId,
    closure: Val,
    /// Frame-stack depth at the `handle` (the prompt). The continuation captured
    /// by a `perform` is the frames above this depth.
    prompt_depth: usize,
    /// True for handlers re-established by a non-tail resume (base=Some). These
    /// are not bracketed by lexical PushHandler/PopHandler, so they are pruned
    /// automatically when their prompt frame leaves the stack — by ANY path
    /// (normal return or a `perform` that discards frames). Lexical handlers
    /// (ephemeral=false) are managed solely by PopHandler. Pruning ephemeral
    /// handlers is what stops a completed handle from shadowing a later one.
    ephemeral: bool,
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
    /// Interns string *objects* by content, so structurally-equal strings share
    /// a single heap slot (and thus a single `Val`). This makes handle-keyed
    /// containers (maps, sets, `group-by`) treat equal strings as equal keys,
    /// matching `val_eq` / the legacy interpreter. Strings are immutable, so
    /// sharing is sound.
    str_interner: HashMap<String, usize>,
    /// Dynamic effect handler stack.
    handlers: Vec<DynHandler>,
    /// Pre-allocated identity closure for `resume`.
    resume_closure: Val,
    /// Span of the instruction currently being executed (for error reporting).
    current_span: Span,
    /// Heap statistics tracked during execution.
    heap_stats: HeapStats,
    /// Incremental effect-trace recorder (`loon run --record`).
    recorder: Option<crate::eir::replay::TraceRecorder>,
    /// Loaded trace being replayed (`loon replay`).
    replay: Option<crate::eir::replay::ReplayCursor>,
    /// Symbol/keyword names created at RUNTIME (e.g. `IO.parse-json` object
    /// keys, `[keyword s]`) that are not in the module's compile-time string
    /// table. A runtime symbol's id is `module.strings.len() + index`.
    runtime_syms: Vec<String>,
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
    /// High-water mark of the frame stack (sampled at each Perform).
    pub max_frames: usize,
    /// High-water mark of the dynamic handler stack (sampled at each Perform).
    pub max_handlers: usize,
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
            str_interner: HashMap::new(),
            handlers: Vec::new(),
            resume_closure: Val::UNIT, // set in run()
            current_span: Span::ZERO,
            heap_stats: HeapStats::default(),
            recorder: None,
            replay: None,
            runtime_syms: Vec::new(),
        }
    }

    /// Record every builtin (unhandled) nondeterministic effect result.
    pub fn set_recorder(&mut self, recorder: crate::eir::replay::TraceRecorder) {
        self.recorder = Some(recorder);
    }

    /// Feed recorded results back instead of executing builtin effects.
    pub fn set_replay(&mut self, entries: Vec<crate::eir::replay::TraceEntry>) {
        self.replay = Some(crate::eir::replay::ReplayCursor::new(entries));
    }

    /// Trace entries not yet consumed by the replay (0 when not replaying).
    pub fn replay_remaining(&self) -> usize {
        self.replay.as_ref().map(|r| r.remaining()).unwrap_or(0)
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

    /// Allocate a string, interning by content so equal strings share one
    /// heap slot (and `Val`). See `str_interner`.
    fn alloc_str(&mut self, s: String) -> Val {
        if let Some(&idx) = self.str_interner.get(s.as_str()) {
            return Val::ptr(idx);
        }
        let val = self.alloc(Obj::Str(s.clone()));
        self.str_interner.insert(s, val.as_ptr());
        val
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
        let val = self.alloc_str(s);
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

    /// Resume a multi-shot delimited continuation `k` with value `v`: re-install
    /// a *clone* of the captured frame segment and continue at the perform point
    /// with `v` plugged in. Because the segment is cloned (not consumed) on each
    /// call, `k` may be resumed any number of times.
    ///
    /// `base` distinguishes how the continuation's result is delivered:
    /// - `Some(dst)` (non-tail resume): push the current (handler) frame as a
    ///   fresh prompt returning into `dst`, and re-establish the handle's
    ///   handlers at that prompt. This makes the continuation self-contained, so
    ///   it works even when resumed after its original `handle` has exited (an
    ///   escaping continuation, e.g. the function-passing `State`).
    /// - `None` (tail resume): leave the frame already on top as the return
    ///   target (the reader's non-escaping tail-resume path).
    fn resume_continuation(&mut self, k: Val, v: Val, base: Option<u32>) -> Result<(), VmError> {
        let (saved, func, block, ip, mut regs, captures, perform_dst, prompt_handlers) =
            match self.get_obj(k) {
                Some(Obj::Continuation {
                    saved,
                    func,
                    block,
                    ip,
                    regs,
                    captures,
                    perform_dst,
                    prompt_handlers,
                }) => (
                    saved.clone(),
                    *func,
                    *block,
                    *ip,
                    regs.clone(),
                    captures.clone(),
                    *perform_dst,
                    prompt_handlers.clone(),
                ),
                _ => {
                    return Err(VmError::new(VmErrorKind::NotCallable).with_span(self.current_span));
                }
            };
        if let Some(dst) = base {
            // Push the handler frame as a fresh prompt returning into `dst`,
            // so the continuation is self-contained even when resumed after
            // its original `handle` has exited (an escaping continuation).
            self.frames.push(Frame {
                func: self.func,
                block: self.block,
                ip: self.ip,
                regs: std::mem::take(&mut self.regs),
                ret_reg: dst,
                captures: std::mem::take(&mut self.captures),
            });
        }
        // Re-establish the snapshotted handlers (the handle's own, plus any
        // inner handle's that were suspended inside the segment). Perform moved
        // them off the dynamic stack into the snapshot; without this, performs
        // inside the resumed segment would not find their handlers. Snapshot
        // depths are relative to the prompt; the saved frames are pushed
        // directly above the current top, so absolute depth = prompt + rel.
        // They are ephemeral: pruned automatically once their prompt frame
        // leaves the stack (see prune_ephemeral_handlers), or removed by their
        // own depth-matched PopHandler when a suspended handle completes
        // normally inside the resumed segment.
        let prompt = self.frames.len().saturating_sub(1);
        for h in prompt_handlers {
            self.handlers.push(DynHandler {
                prompt_depth: prompt + h.prompt_depth,
                ephemeral: true,
                ..h
            });
        }
        for f in saved {
            self.frames.push(f);
        }
        let pd = perform_dst as usize;
        if pd >= regs.len() {
            regs.resize(pd + 1, Val::UNIT);
        }
        regs[pd] = v;
        self.func = func;
        self.block = block;
        self.ip = ip;
        self.regs = regs;
        self.captures = captures;
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
            // A resumed segment may have left ephemeral handlers scoped to a
            // prompt frame that is now gone; drop them so they cannot shadow a
            // later handle for the same effect.
            self.prune_ephemeral_handlers();
            Ok(None) // keep executing
        } else {
            Ok(Some(val)) // top-level return
        }
    }

    /// Remove ephemeral (resume-re-established) handlers whose prompt frame is
    /// no longer on the stack. A handler at prompt depth P is in scope only
    /// while some frame sits above it (frames.len() > P); once we are back at or
    /// below P its delimited region has been exited. Lexical handlers are left
    /// to PopHandler.
    fn prune_ephemeral_handlers(&mut self) {
        let depth = self.frames.len();
        self.handlers
            .retain(|h| !h.ephemeral || h.prompt_depth < depth);
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
                    if matches!(self.get_obj(func_val), Some(Obj::Continuation { .. })) {
                        // Tail resume (`[resume v]` as the handler's whole body):
                        // the current (handler) frame is being tail-replaced, so
                        // the resumed body's result returns into the frame already
                        // below it (the prompt frame) — don't push a frame.
                        let v = vals.first().copied().unwrap_or(Val::UNIT);
                        self.resume_continuation(func_val, v, None)?;
                    } else if let Some(Obj::Closure(fid, caps)) = self.get_obj(func_val).cloned() {
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
                            self.alloc_str(n.to_string())
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
                let result = self.exec_binop(*binop, av, bv)?;
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
                if matches!(self.get_obj(func_val), Some(Obj::Continuation { .. })) {
                    // Resume a continuation (e.g. `[resume v]` not in tail
                    // position). resume_continuation pushes the handler frame as
                    // a fresh prompt (returning into `dst`) and re-establishes
                    // the handlers, so the continuation is self-contained.
                    let v = vals.first().copied().unwrap_or(Val::UNIT);
                    self.resume_continuation(func_val, v, Some(dst.0))?;
                } else if let Some(Obj::Closure(fid, caps)) = self.get_obj(func_val).cloned() {
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

            Op::Perform(dst, _eff_sid, op_sid, args, _evidence, _span) => {
                let vals = self.read_regs(args);
                // Perf telemetry: unbounded growth here is the O(N^2) signature
                // (each perform re-scans/copies the stacks), so track highs.
                self.heap_stats.max_frames = self.heap_stats.max_frames.max(self.frames.len());
                self.heap_stats.max_handlers =
                    self.heap_stats.max_handlers.max(self.handlers.len());
                if std::env::var_os("LOON_TRACE_PERFORM").is_some() {
                    let names: Vec<&str> = self
                        .frames
                        .iter()
                        .map(|fr| {
                            self.module.funcs[fr.func.0 as usize]
                                .name
                                .as_deref()
                                .unwrap_or("anon")
                        })
                        .collect();
                    eprintln!(
                        "perform: frames={} handlers={} stack={:?} cur={}",
                        self.frames.len(),
                        self.handlers.len(),
                        names,
                        self.module.funcs[self.func.0 as usize]
                            .name
                            .as_deref()
                            .unwrap_or("anon"),
                    );
                }
                // Find the innermost handler for this operation on the dynamic
                // stack, with its prompt depth. (Evidence is ignored: capturing
                // a continuation needs the prompt boundary, which only the
                // dynamic handler records. Every `handle` installs handlers, so
                // the dynamic lookup finds them all.)
                let found = self
                    .handlers
                    .iter()
                    .rev()
                    .find(|h| h.effect == *_eff_sid && h.op == *op_sid)
                    .map(|h| (h.closure, h.prompt_depth));

                if let Some((hval, prompt_depth)) = found {
                    // Capture the continuation: every frame above the prompt,
                    // plus the current execution point (already advanced past
                    // this perform), as a multi-shot Obj::Continuation.
                    //
                    // Every handler at or above the prompt is delimited by the
                    // captured segment: the prompt's own handlers (deep-handler
                    // semantics — the clause body runs OUTSIDE its own handle,
                    // so a re-perform forwards to the next handler out instead
                    // of recursing into itself) and any inner handle's handlers
                    // whose frames are being captured. Move them all into the
                    // continuation snapshot, with depths stored RELATIVE to the
                    // prompt so `resume_continuation` can re-establish them at
                    // whatever depth the segment is re-installed.
                    let prompt_handlers: Vec<DynHandler> = self
                        .handlers
                        .iter()
                        .filter(|h| h.prompt_depth >= prompt_depth)
                        .map(|h| DynHandler {
                            prompt_depth: h.prompt_depth - prompt_depth,
                            ..h.clone()
                        })
                        .collect();
                    self.handlers.retain(|h| h.prompt_depth < prompt_depth);
                    let saved: Vec<Frame> = self.frames.split_off(prompt_depth + 1);
                    let cont = Obj::Continuation {
                        saved,
                        func: self.func,
                        block: self.block,
                        ip: self.ip,
                        regs: std::mem::take(&mut self.regs),
                        captures: std::mem::take(&mut self.captures),
                        perform_dst: dst.0,
                        prompt_handlers,
                    };
                    let k = self.alloc(cont);
                    // Restore the prompt frame (the `handle`'s frame) as current;
                    // its saved ret_reg is the handle's result register.
                    let f0 = self.frames.pop().expect("prompt frame");
                    let handle_ret = f0.ret_reg;
                    self.func = f0.func;
                    self.block = f0.block;
                    self.ip = f0.ip;
                    self.regs = f0.regs;
                    self.captures = f0.captures;
                    // Capturing the continuation discarded every frame above the
                    // prompt; drop ephemeral handlers re-established at those
                    // (now-gone) prompts so they cannot shadow this or a later
                    // handle.
                    self.prune_ephemeral_handlers();
                    // Run the handler at the prompt with `resume` := k. If it
                    // returns without invoking k, that value becomes the handle's
                    // result (abort / 0-shot).
                    if let Some(Obj::Closure(fid, caps)) = self.get_obj(hval).cloned() {
                        let mut call_args = vec![k];
                        call_args.extend_from_slice(&vals);
                        self.call_func_with_captures(fid, &call_args, handle_ret, caps)?;
                    } else {
                        self.w(*dst, Val::UNIT);
                    }
                } else {
                    // No dynamic handler: fall through to a builtin effect
                    // (real IO, clock, net, …), threading record/replay. An op
                    // with no builtin implementation is a hard error, not a
                    // silent `()` (see builtin_effect) — this notably makes an
                    // uncaught `Fail.fail` abort with a message, including one
                    // raised inside a handler clause whose enclosing `try` was
                    // frozen into the continuation.
                    let effect = self.module.strings[_eff_sid.0 as usize].clone();
                    let op_name = self.module.strings[op_sid.0 as usize].clone();
                    let result = self.perform_builtin_effect(&effect, &op_name, &vals)?;
                    self.w(*dst, result);
                }
            }

            Op::Builtin(dst, built, args, _) => {
                let vals = self.read_regs(args);
                let result = self.exec_builtin(*built, &vals)?;
                self.w(*dst, result);
            }

            Op::PushHandler(handler_reg, eff_sid, op_sid, _) => {
                let closure = self.r(*handler_reg);
                // The prompt is the current frame (the `handle`'s frame). The
                // body runs as a thunk invoked right after this, so it and any
                // functions it calls sit at frames above this depth.
                self.handlers.push(DynHandler {
                    effect: *eff_sid,
                    op: *op_sid,
                    closure,
                    prompt_depth: self.frames.len(),
                    ephemeral: false,
                });
            }

            Op::PopHandler(_) => {
                // Remove the innermost handler installed at THIS frame depth.
                // Depth-matched rather than a blind pop: if the handle's body
                // performed, Perform already moved this handle's handlers into
                // the continuation snapshot, and a blind pop here (on the
                // abort/no-resume path) would remove some outer handle's
                // handler instead. Matching PushHandler's prompt_depth
                // (frames.len() at the handle) makes the pop a no-op then.
                let depth = self.frames.len();
                if let Some(idx) = self.handlers.iter().rposition(|h| h.prompt_depth == depth) {
                    self.handlers.remove(idx);
                }
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

    fn exec_binop(&mut self, op: BinOp, a: Val, b: Val) -> Result<Val, VmError> {
        Ok(match op {
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
                        return Err(VmError::new(VmErrorKind::DivideByZero("division"))
                            .with_span(self.current_span));
                    }
                    Val::int(a.as_int() / bv)
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
                        return Err(VmError::new(VmErrorKind::DivideByZero("modulo"))
                            .with_span(self.current_span));
                    }
                    Val::int(a.as_int() % bv)
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
        })
    }

    // ── Builtins ───────────────────────────────────────────────────────

    fn exec_builtin(&mut self, built: Built, args: &[Val]) -> Result<Val, VmError> {
        match built {
            Built::Println => {
                // Under record/replay, bare `println` is treated as the
                // `IO.println` effect so log writes land in the trace for
                // observability. On replay it re-executes live and is not
                // order-checked, so added/removed prints don't diverge.
                if self.recorder.is_some() || self.replay.is_some() {
                    return self.perform_builtin_effect("IO", "println", args);
                }
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
                        items.push_back(val); // O(log n) with structural sharing
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
                        map.insert(key, val); // O(log₃₂ n) with structural sharing
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
                // Higher-order builtins: call the function for each element.
                // Detect collection vs function by TYPE, not position, so both
                // the direct form `[map coll fn]` and the pipe/thread-last form
                // `[map fn coll]` work (mirrors the tree-walking interpreter).
                if args.len() != 2 {
                    return Ok(Val::UNIT);
                }
                let a0_is_vec = matches!(self.get_obj(args[0]), Some(Obj::Vec(_)));
                let (func, coll) = if a0_is_vec {
                    (args[1], args[0])
                } else {
                    (args[0], args[1])
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
                            ks.is_some()
                                && map.iter().any(|(k, _)| {
                                    let kk = self.get_str(*k).map(|s| s.to_string());
                                    kk.is_some() && kk == ks
                                })
                        },
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
                        items.push_front(val); // O(log n) with imbl
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
                        let merged = ma.union(mb); // O(n log n) with structural sharing
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
                        // Clamp to [0, len]: `imbl::Vector::take` panics past len.
                        let k = (n.as_int().max(0) as usize).min(items.len());
                        let taken = items.take(k);
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
                        // Clamp to [0, len]: `imbl::Vector::skip` panics past len.
                        let k = (n.as_int().max(0) as usize).min(items.len());
                        let dropped = items.skip(k);
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
                            .map(|p| self.alloc_str(p.to_string()))
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
                // Loon order is subject-first: [replace s from to].
                let s = args.first().copied().unwrap_or(Val::UNIT);
                let from = args.get(1).copied().unwrap_or(Val::UNIT);
                let to = args.get(2).copied().unwrap_or(Val::UNIT);
                match (
                    self.get_str(from).map(|s| s.to_string()),
                    self.get_str(to).map(|s| s.to_string()),
                    self.get_str(s).map(|s| s.to_string()),
                ) {
                    (Some(f), Some(t), Some(s)) => Ok(self.alloc_str(s.replace(&f, &t))),
                    _ => Ok(Val::UNIT),
                }
            }
            Built::Uppercase => {
                let s = args.first().copied().unwrap_or(Val::UNIT);
                match self.get_str(s).map(|s| s.to_string()) {
                    Some(s) => Ok(self.alloc_str(s.to_uppercase())),
                    _ => Ok(Val::UNIT),
                }
            }
            Built::Lowercase => {
                let s = args.first().copied().unwrap_or(Val::UNIT);
                match self.get_str(s).map(|s| s.to_string()) {
                    Some(s) => Ok(self.alloc_str(s.to_lowercase())),
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
                // Loon order is collection-first: [join coll sep]. Detect the
                // vector by type so the pipe/thread-last form also works.
                let a0_is_vec = matches!(
                    self.get_obj(args.first().copied().unwrap_or(Val::UNIT)),
                    Some(Obj::Vec(_))
                );
                let (coll, sep) = if a0_is_vec {
                    (args[0], args.get(1).copied().unwrap_or(Val::UNIT))
                } else {
                    (
                        args.get(1).copied().unwrap_or(Val::UNIT),
                        args.first().copied().unwrap_or(Val::UNIT),
                    )
                };
                let sep_str = self.val_to_string(sep);
                match self.get_obj(coll).cloned() {
                    Some(Obj::Vec(items)) => {
                        let parts: Vec<String> =
                            items.iter().map(|v| self.val_to_string(*v)).collect();
                        Ok(self.alloc_str(parts.join(&sep_str)))
                    }
                    _ => Ok(self.alloc_str(String::new())),
                }
            }
            Built::Trim => {
                let s = args.first().copied().unwrap_or(Val::UNIT);
                match self.get_str(s).map(|s| s.to_string()) {
                    Some(s) => Ok(self.alloc_str(s.trim().to_string())),
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
                } else if let Some(s) = self.get_str(v) {
                    // Parse a string to an integer (the builtin's declared type
                    // is Str → Int); unparseable strings yield ().
                    match s.trim().parse::<i64>() {
                        Ok(n) => Ok(self.safe_int(n)),
                        Err(_) => Ok(Val::UNIT),
                    }
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
                } else if let Some(s) = self.get_str(v) {
                    match s.trim().parse::<f64>() {
                        Ok(n) => Ok(Val::float(n)),
                        Err(_) => Ok(Val::UNIT),
                    }
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
                // Detect the collection by TYPE so both the direct form
                // `[fold coll init f]` and the pipe/thread-last form
                // `[fold init f coll]` work (mirrors the interpreter).
                if args.len() != 3 {
                    return Ok(args.first().copied().unwrap_or(Val::UNIT));
                }
                let (coll, init, func) = if matches!(self.get_obj(args[0]), Some(Obj::Vec(_))) {
                    (args[0], args[1], args[2])
                } else {
                    (args[2], args[0], args[1])
                };
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
                            Ok(self.alloc_str(c.to_string()))
                        } else {
                            Ok(self.alloc_str(String::new()))
                        }
                    }
                    _ => Ok(self.alloc_str(String::new())),
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
                        Ok(self.alloc_str(sub))
                    }
                    _ => Ok(self.alloc_str(String::new())),
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
                        Ok(self.alloc_str(sub))
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
                // [keyword str] → keyword from string (interned at runtime if
                // the name is not in the compile-time string table — it used
                // to silently collapse unknown names to symbol 0)
                let v = args.first().copied().unwrap_or(Val::UNIT);
                match self.get_str(v).map(|s| s.to_string()) {
                    Some(s) => Ok(self.intern_sym(&s)),
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
                                let key = self.intern_sym(&s);
                                new_map.insert(key, v);
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
                        Ok(self.alloc_str(format!("{sa}{sb}")))
                    }
                    _ => Ok(Val::UNIT),
                }
            }
        }
    }

    // ── Effect builtins ────────────────────────────────────────────────

    /// Execute a builtin (unhandled) effect, threading record/replay through.
    ///
    /// - Recording: run the real effect, then append `{effect op args result}`
    ///   to the trace (flushed immediately, so it survives a crash).
    /// - Replaying: consume the next trace entry and return its recorded
    ///   result instead of touching the outside world. Log writes
    ///   (`IO.println`) are the exception: they re-execute live and never
    ///   consume a trace entry, so adding or removing prints while debugging
    ///   does not invalidate the trace. A mismatched or exhausted trace is a
    ///   `ReplayDivergence` error.
    fn perform_builtin_effect(
        &mut self,
        effect: &str,
        op: &str,
        args: &[Val],
    ) -> Result<Val, VmError> {
        let recorded = crate::eir::replay::is_recorded_op(effect, op);
        if recorded && self.replay.is_some() {
            if crate::eir::replay::is_log_op(effect, op) {
                return self.builtin_effect(effect, op, args);
            }
            return self.replay_effect(effect, op, args);
        }
        let result = self.builtin_effect(effect, op, args)?;
        if recorded && self.recorder.is_some() {
            let entry = crate::eir::replay::TraceEntry {
                effect: effect.to_string(),
                op: op.to_string(),
                args: args.iter().map(|a| self.val_to_trace(*a)).collect(),
                result: self.val_to_trace(result),
            };
            // A failed write must stop recording entirely: continuing after a
            // dropped entry would finalize a gapped trace that looks valid but
            // replays wrong values (ops shifted one step earlier).
            if let Some(mut rec) = self.recorder.take() {
                match rec.record(&entry) {
                    Ok(()) => self.recorder = Some(rec),
                    Err(e) => eprintln!(
                        "warning: failed to write trace entry: {e}; recording stopped — \
                         the trace is truncated at {} entr{}",
                        rec.count(),
                        if rec.count() == 1 { "y" } else { "ies" }
                    ),
                }
            }
        }
        Ok(result)
    }

    /// Replay path: feed the next recorded result back for this operation.
    /// Log entries in the trace are skipped, never matched — log writes
    /// re-execute live in `perform_builtin_effect` and do not reach here.
    fn replay_effect(&mut self, effect: &str, op: &str, args: &[Val]) -> Result<Val, VmError> {
        let cursor = self.replay.as_mut().expect("replay cursor");
        while cursor
            .entries
            .get(cursor.idx)
            .is_some_and(|e| crate::eir::replay::is_log_op(&e.effect, &e.op))
        {
            cursor.idx += 1;
        }
        let idx = cursor.idx;
        if idx >= cursor.entries.len() {
            return Err(VmError::new(VmErrorKind::ReplayDivergence(format!(
                "trace exhausted at step {idx}: the program performed {effect}.{op} \
                 but the trace has no more recorded operations"
            )))
            .with_span(self.current_span));
        }
        let entry = cursor.entries[idx].clone();
        cursor.idx += 1;
        if entry.effect != effect || entry.op != op {
            return Err(VmError::new(VmErrorKind::ReplayDivergence(format!(
                "at step {idx}: trace recorded {}.{} but the program performed {effect}.{op}",
                entry.effect, entry.op
            )))
            .with_span(self.current_span));
        }
        // Same op but different arguments (say, a changed file path) means
        // the program no longer matches the trace: feeding the stale result
        // back would silently replay the wrong world.
        let live_args: Vec<crate::eir::replay::TraceVal> =
            args.iter().map(|a| self.val_to_trace(*a)).collect();
        if entry.args != live_args {
            let recorded = crate::eir::replay::TraceVal::Vec(entry.args.clone()).to_loon();
            let live = crate::eir::replay::TraceVal::Vec(live_args).to_loon();
            return Err(VmError::new(VmErrorKind::ReplayDivergence(format!(
                "at step {idx}: {effect}.{op} was recorded with args {recorded} \
                 but the program passed {live}"
            )))
            .with_span(self.current_span));
        }
        // Return the recorded result without touching the outside world.
        Ok(self.trace_to_val(&entry.result))
    }

    /// Convert a runtime value to a trace value for serialization. Values a
    /// builtin effect never produces (closures, ADTs, maps) fall back to
    /// their display string — good enough for `:args` observability.
    fn val_to_trace(&self, val: Val) -> crate::eir::replay::TraceVal {
        use crate::eir::replay::TraceVal;
        if val.is_unit() {
            TraceVal::Unit
        } else if val.is_int() {
            TraceVal::Int(val.as_int())
        } else if val.is_float() {
            TraceVal::Float(val.as_float())
        } else if val.is_bool() {
            TraceVal::Bool(val.as_bool())
        } else if val.is_ptr() {
            match self.get_obj(val) {
                Some(Obj::Str(s)) => TraceVal::Str(s.clone()),
                Some(Obj::Vec(items)) => {
                    TraceVal::Vec(items.iter().map(|v| self.val_to_trace(*v)).collect())
                }
                _ => TraceVal::Str(self.val_to_string(val)),
            }
        } else {
            TraceVal::Str(self.val_to_string(val))
        }
    }

    /// Materialize a recorded trace value back into a runtime value.
    fn trace_to_val(&mut self, t: &crate::eir::replay::TraceVal) -> Val {
        use crate::eir::replay::TraceVal;
        match t {
            TraceVal::Unit => Val::UNIT,
            TraceVal::Int(n) => self.safe_int(*n),
            TraceVal::Float(f) => Val::float(*f),
            TraceVal::Bool(b) => Val::bool(*b),
            TraceVal::Str(s) => self.alloc_str(s.clone()),
            TraceVal::Vec(items) => {
                let vals: ImVec = items.iter().map(|i| self.trace_to_val(i)).collect();
                self.alloc(Obj::Vec(vals))
            }
        }
    }

    /// Look up an ADT constructor tag by name (latest definition wins, the
    /// same override rule the lowerer applies to `ctor_map`). The prelude's
    /// Option/Result are always registered, so `Some`/`None` resolve here.
    fn ctor_tag(&self, name: &str) -> Option<u16> {
        self.module.ctors.iter().rev().find(|c| c.name == name).map(|c| c.tag)
    }

    /// The textual name of a symbol/keyword id: compile-time ids index the
    /// module string table, runtime ids the `runtime_syms` overflow.
    fn sym_name(&self, idx: usize) -> Option<&str> {
        let n = self.module.strings.len();
        if idx < n {
            self.module.strings.get(idx).map(|s| s.as_str())
        } else {
            self.runtime_syms.get(idx - n).map(|s| s.as_str())
        }
    }

    /// Intern a symbol/keyword NAME to a `Val::sym`, creating a runtime id if
    /// the module string table doesn't already contain it. Deduplicating by
    /// content keeps symbol equality equal to name equality, so a runtime
    /// keyword (an `IO.parse-json` object key) matches a same-named source
    /// keyword.
    fn intern_sym(&mut self, name: &str) -> Val {
        if let Some(i) = self.module.strings.iter().position(|s| s == name) {
            return Val::sym(i as u32);
        }
        let base = self.module.strings.len();
        if let Some(i) = self.runtime_syms.iter().position(|s| s == name) {
            return Val::sym((base + i) as u32);
        }
        self.runtime_syms.push(name.to_string());
        Val::sym((base + self.runtime_syms.len() - 1) as u32)
    }

    /// Convert a parsed JSON document to VM values, matching the legacy
    /// interpreter's mapping: object keys become keywords, null becomes Unit.
    fn json_to_val(&mut self, j: serde_json::Value) -> Val {
        match j {
            serde_json::Value::Null => Val::UNIT,
            serde_json::Value::Bool(b) => Val::bool(b),
            serde_json::Value::Number(n) => {
                if let Some(i) = n.as_i64() {
                    self.safe_int(i)
                } else {
                    Val::float(n.as_f64().unwrap_or(0.0))
                }
            }
            serde_json::Value::String(s) => self.alloc_str(s),
            serde_json::Value::Array(items) => {
                let vals: ImVec = items.into_iter().map(|x| self.json_to_val(x)).collect();
                self.alloc(Obj::Vec(vals))
            }
            serde_json::Value::Object(obj) => {
                let mut map = ImMap::new();
                for (k, v) in obj {
                    let key = self.intern_sym(&k);
                    let val = self.json_to_val(v);
                    map.insert(key, val);
                }
                self.alloc(Obj::Map(map))
            }
        }
    }

    /// Convert a VM value to JSON, matching the interpreter's `value_to_json`:
    /// keywords/strings become strings, unit becomes null, nullary ADTs their
    /// ctor name, ADTs with fields a `{_tag, _fields}` object.
    fn val_to_json(&self, v: Val) -> serde_json::Value {
        use serde_json::Value as J;
        if v.is_int() {
            return J::Number(v.as_int().into());
        }
        if v.is_float() {
            return serde_json::Number::from_f64(v.as_float())
                .map(J::Number)
                .unwrap_or(J::Null);
        }
        if v.is_bool() {
            return J::Bool(v.as_bool());
        }
        if v.is_unit() {
            return J::Null;
        }
        if v.is_sym() {
            return J::String(self.sym_name(v.as_sym() as usize).unwrap_or("").to_string());
        }
        match self.get_obj(v) {
            Some(Obj::Str(s)) => J::String(s.clone()),
            Some(Obj::Vec(items)) => J::Array(items.iter().map(|x| self.val_to_json(*x)).collect()),
            Some(Obj::Set(items)) => J::Array(items.iter().map(|x| self.val_to_json(*x)).collect()),
            Some(Obj::Map(map)) => {
                let mut obj = serde_json::Map::new();
                for (k, val) in map.iter() {
                    let key = if k.is_sym() {
                        self.sym_name(k.as_sym() as usize).unwrap_or("").to_string()
                    } else if let Some(Obj::Str(s)) = self.get_obj(*k) {
                        s.clone()
                    } else {
                        self.val_to_string(*k)
                    };
                    obj.insert(key, self.val_to_json(*val));
                }
                J::Object(obj)
            }
            Some(Obj::Adt(tag, fields)) => {
                let name = self
                    .module
                    .ctors
                    .iter()
                    .rev()
                    .find(|c| c.tag == *tag)
                    .map(|c| c.name.clone())
                    .unwrap_or_default();
                if fields.is_empty() {
                    J::String(name)
                } else {
                    let mut obj = serde_json::Map::new();
                    obj.insert("_tag".to_string(), J::String(name));
                    obj.insert(
                        "_fields".to_string(),
                        J::Array(fields.iter().map(|f| self.val_to_json(*f)).collect()),
                    );
                    J::Object(obj)
                }
            }
            _ => J::Null,
        }
    }

    /// Handle an effect operation that reached the top of the handler stack
    /// unhandled: the built-in "prod" implementations (real IO, clock, env,
    /// sockets). An operation with NO built-in implementation is a hard
    /// error — the same error class the interpreter raises — never a silent
    /// `()`: a silent no-op here means a program believes it wrote a file
    /// (or slept, or exited) when nothing happened.
    fn builtin_effect(&mut self, effect: &str, op: &str, args: &[Val]) -> Result<Val, VmError> {
        Ok(match (effect, op) {
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
                        Ok(contents) => self.alloc_str(contents),
                        Err(_) => Val::UNIT,
                    }
                } else {
                    Val::UNIT
                }
            }
            ("IO", "write-file") => {
                if let (Some(path), Some(contents)) = (args.first(), args.get(1)) {
                    let p = self.val_to_string(*path);
                    let c = self.val_to_string(*contents);
                    let _ = std::fs::write(&p, c);
                }
                Val::UNIT
            }
            ("IO", "file-exists?") => {
                let p = args.first().map(|v| self.val_to_string(*v)).unwrap_or_default();
                Val::bool(std::path::Path::new(&p).exists())
            }
            ("IO", "delete-file") => {
                if let Some(path) = args.first() {
                    let p = self.val_to_string(*path);
                    let _ = std::fs::remove_file(&p);
                }
                Val::UNIT
            }
            ("IO", "mkdir") => {
                if let Some(path) = args.first() {
                    let p = self.val_to_string(*path);
                    let _ = std::fs::create_dir_all(&p);
                }
                Val::UNIT
            }
            ("IO", "copy-file") => {
                if let (Some(src), Some(dst)) = (args.first(), args.get(1)) {
                    let s = self.val_to_string(*src);
                    let d = self.val_to_string(*dst);
                    let _ = std::fs::copy(&s, &d);
                }
                Val::UNIT
            }
            ("IO", "list-dir") => {
                let p = args.first().map(|v| self.val_to_string(*v)).unwrap_or_default();
                let names: Vec<String> = match std::fs::read_dir(&p) {
                    Ok(entries) => entries
                        .filter_map(|e| e.ok())
                        .map(|e| e.file_name().to_string_lossy().into_owned())
                        .collect(),
                    // Not a directory (or doesn't exist) → empty vec, as interp.
                    Err(_) => Vec::new(),
                };
                let vals: ImVec = names.into_iter().map(|n| self.alloc_str(n)).collect();
                self.alloc(Obj::Vec(vals))
            }
            ("IO", "mtime") => {
                let p = args.first().map(|v| self.val_to_string(*v)).unwrap_or_default();
                let millis = std::fs::metadata(&p)
                    .and_then(|m| m.modified())
                    .ok()
                    .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
                    .map(|d| d.as_millis() as i64)
                    .unwrap_or(0);
                self.safe_int(millis)
            }
            ("IO", "sleep") => {
                if let Some(ms) = args.first() {
                    if ms.is_int() {
                        std::thread::sleep(std::time::Duration::from_millis(
                            ms.as_int().max(0) as u64,
                        ));
                    }
                }
                Val::UNIT
            }
            ("IO", "read-line") => {
                let mut line = String::new();
                let _ = std::io::stdin().read_line(&mut line);
                if line.ends_with('\n') {
                    line.pop();
                    if line.ends_with('\r') {
                        line.pop();
                    }
                }
                self.alloc_str(line)
            }
            // Clock: real wall-clock time. `now` in whole seconds, `millis` in
            // milliseconds since the Unix epoch (both fit in a 48-bit int).
            ("IO", "now") => {
                let secs = std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .map(|d| d.as_secs() as i64)
                    .unwrap_or(0);
                self.safe_int(secs)
            }
            ("IO", "millis") => {
                let ms = std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .map(|d| d.as_millis() as i64)
                    .unwrap_or(0);
                self.safe_int(ms)
            }
            ("IO", "uuid") => self.alloc_str(gen_uuid_v4()),
            ("IO", "parse-json") => {
                let text = args.first().map(|v| self.val_to_string(*v)).unwrap_or_default();
                match serde_json::from_str::<serde_json::Value>(&text) {
                    Ok(json) => self.json_to_val(json),
                    // The interpreter performs Fail.fail here; unhandled, that
                    // surfaces as this same error class. (A user Fail handler
                    // catching IO parse errors on the VM is not supported —
                    // see known VM/interp IO error-path differences.)
                    Err(e) => {
                        return Err(VmError::new(VmErrorKind::UnhandledEffect(format!(
                            "Fail.fail ({e})"
                        )))
                        .with_span(self.current_span));
                    }
                }
            }
            ("IO", "to-json") => {
                let v = args.first().copied().unwrap_or(Val::UNIT);
                let json = self.val_to_json(v);
                match serde_json::to_string(&json) {
                    Ok(s) => self.alloc_str(s),
                    Err(_) => Val::UNIT,
                }
            }
            #[cfg(feature = "pkg-fetch")]
            ("IO", "blake3") => {
                let text = args.first().map(|v| self.val_to_string(*v)).unwrap_or_default();
                self.alloc_str(blake3::hash(text.as_bytes()).to_hex().to_string())
            }
            ("Process", "exec") => {
                let cmd_str = args.first().map(|v| self.val_to_string(*v)).unwrap_or_default();
                let input = args.get(1).map(|v| self.val_to_string(*v));
                let parts: Vec<&str> = cmd_str.split_whitespace().collect();
                if parts.is_empty() {
                    return Err(VmError::new(VmErrorKind::UnhandledEffect(
                        "Process.exec (empty command)".to_string(),
                    ))
                    .with_span(self.current_span));
                }
                let mut cmd = std::process::Command::new(parts[0]);
                cmd.args(&parts[1..]);
                if input.is_some() {
                    cmd.stdin(std::process::Stdio::piped());
                }
                cmd.stdout(std::process::Stdio::piped());
                cmd.stderr(std::process::Stdio::piped());
                let output = cmd.spawn().and_then(|mut child| {
                    if let Some(stdin_data) = input {
                        if let Some(mut stdin) = child.stdin.take() {
                            use std::io::Write;
                            let _ = stdin.write_all(stdin_data.as_bytes());
                        }
                    }
                    child.wait_with_output()
                });
                match output {
                    Ok(out) => {
                        // {:exit-code Int :stdout Str :stderr Str}, as interp.
                        let code =
                            self.safe_int(out.status.code().unwrap_or(-1) as i64);
                        let stdout =
                            self.alloc_str(String::from_utf8_lossy(&out.stdout).into_owned());
                        let stderr =
                            self.alloc_str(String::from_utf8_lossy(&out.stderr).into_owned());
                        let mut map = ImMap::new();
                        let k_code = self.intern_sym("exit-code");
                        let k_out = self.intern_sym("stdout");
                        let k_err = self.intern_sym("stderr");
                        map.insert(k_code, code);
                        map.insert(k_out, stdout);
                        map.insert(k_err, stderr);
                        self.alloc(Obj::Map(map))
                    }
                    Err(e) => {
                        return Err(VmError::new(VmErrorKind::UnhandledEffect(format!(
                            "Process.exec ({e})"
                        )))
                        .with_span(self.current_span));
                    }
                }
            }
            ("Async", "sleep") => {
                if let Some(ms) = args.first() {
                    if ms.is_int() {
                        std::thread::sleep(std::time::Duration::from_millis(
                            ms.as_int().max(0) as u64,
                        ));
                    }
                }
                Val::UNIT
            }
            // Process.env matches the interpreter: [Some value] when set,
            // None when unset (the prelude Option ctors are always
            // registered by the lowerer).
            ("Process", "env") => {
                let k = args.first().map(|v| self.val_to_string(*v)).unwrap_or_default();
                match (std::env::var(&k), self.ctor_tag("Some"), self.ctor_tag("None")) {
                    (Ok(v), Some(some_tag), _) => {
                        let s = self.alloc_str(v);
                        self.alloc(Obj::Adt(some_tag, vec![s]))
                    }
                    (Err(_), _, Some(none_tag)) => self.alloc(Obj::Adt(none_tag, Vec::new())),
                    // No Option ctors registered (shouldn't happen): raw string.
                    (Ok(v), None, _) => self.alloc_str(v),
                    (Err(_), _, None) => self.alloc_str(String::new()),
                }
            }
            ("Process", "args") => {
                let vals: ImVec = std::env::args().map(|a| self.alloc_str(a)).collect();
                self.alloc(Obj::Vec(vals))
            }
            ("Process", "exit") => {
                let code = args.first().filter(|v| v.is_int()).map(|v| v.as_int()).unwrap_or(0);
                std::process::exit(code as i32);
            }
            // NOTE: `Env.lookup`/`Env.get` used to be a VM-only convenience
            // (value-or-"" lookup). The interpreter has no such ops — it hard
            // errors — so they were dropped for cross-backend conformance;
            // `Process.env` is the Option-returning form both backends share.
            // Real TCP/HTTP sockets (see eir/net.rs). A blocking one-at-a-time
            // server: listen a port, accept a request, send the response.
            ("Net", "listen") => {
                let port = args.first().map(|v| v.as_int() as u16).unwrap_or(0);
                Val::bool(crate::eir::net::listen(port))
            }
            ("Net", "accept") => {
                let port = args.first().map(|v| v.as_int() as u16).unwrap_or(0);
                match crate::eir::net::accept(port) {
                    Some((method, path, body)) => {
                        let m = self.alloc_str(method);
                        let p = self.alloc_str(path);
                        let b = self.alloc_str(body);
                        self.alloc(Obj::Vec([m, p, b].into_iter().collect()))
                    }
                    None => self.alloc(Obj::Vec(ImVec::new())),
                }
            }
            ("Net", "send") => {
                let status = args.first().map(|v| v.as_int()).unwrap_or(200);
                let body = args
                    .get(1)
                    .map(|v| self.val_to_string(*v))
                    .unwrap_or_default();
                Val::bool(crate::eir::net::send(status, &body))
            }
            ("Const", "c") => Val::float(299_792_458.0),
            ("Physics", "yield-strength") => Val::float(250.0),
            ("Physics", "gravity") => Val::float(9.80665),
            _ => {
                return Err(VmError::new(VmErrorKind::UnhandledEffect(format!(
                    "{effect}.{op}"
                )))
                .with_span(self.current_span));
            }
        })
    }

    // ── Value display ──────────────────────────────────────────────────

    fn val_to_string(&self, val: Val) -> String {
        self.val_to_string_inner(val, false)
    }

    /// Render a value. `nested` mirrors the interpreter's display rules: a
    /// string at the top level (println/str) prints raw, but a string INSIDE
    /// a container (vec/map/ADT/...) prints quoted, e.g. `#["a" "b"]`.
    fn val_to_string_inner(&self, val: Val, nested: bool) -> String {
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
            match self.sym_name(idx) {
                Some(name) => format!(":{name}"),
                None => format!(":<sym:{idx}>"),
            }
        } else if val.is_ptr() {
            match self.get_obj(val) {
                Some(Obj::Str(s)) => {
                    if nested {
                        format!("\"{s}\"")
                    } else {
                        s.clone()
                    }
                }
                Some(Obj::Vec(items)) => {
                    let inner: Vec<String> =
                        items.iter().map(|v| self.val_to_string_inner(*v, true)).collect();
                    format!("#[{}]", inner.join(" "))
                }
                Some(Obj::Map(map)) => {
                    let inner: Vec<String> = map
                        .iter()
                        .map(|(k, v)| {
                            format!(
                                "{} {}",
                                self.val_to_string_inner(*k, true),
                                self.val_to_string_inner(*v, true)
                            )
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
                        let inner: Vec<String> = fields
                            .iter()
                            .map(|v| self.val_to_string_inner(*v, true))
                            .collect();
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
        self.alloc_str(s)
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
    /// A replayed program requested a different effect op than the trace
    /// recorded (or ran past the end of the trace).
    ReplayDivergence(String),
    /// An effect reached the top of the handler stack with no handler and no
    /// builtin implementation. Silently returning `()` here would let programs
    /// believe the effect happened. Notably covers an uncaught `Fail.fail` —
    /// an abort with no surrounding `try`/`Fail` handler, including one raised
    /// inside a handler clause (whose body runs at its `handle`'s prompt,
    /// outside the dynamic extent of any `try` in the handled body).
    UnhandledEffect(String),
    /// Integer division or modulo by a zero divisor. Silently returning `()`
    /// here would let programs believe they got a valid quotient. The `&str`
    /// names the operation ("division" or "modulo") for the diagnostic.
    DivideByZero(&'static str),
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
            VmErrorKind::ReplayDivergence(msg) => {
                write!(f, "replay diverged {msg}")
            }
            VmErrorKind::UnhandledEffect(name) => {
                // Same wording as the interpreter's unhandled-effect error, so
                // the two backends fail the same way.
                write!(
                    f,
                    "unhandled effect: {name} — add a [handle ...] block to handle this effect"
                )
            }
            VmErrorKind::DivideByZero(kind) => {
                // Wording matches the interpreter's "division by zero" /
                // "modulo by zero" so both backends fail the same way.
                write!(f, "{kind} by zero")
            }
        }
    }
}

// ─── Public API ────────────────────────────────────────────────────────────

/// Run a Loon program through the EIR pipeline: parse → check → lower → VM.
pub fn eval_eir(src: &str) -> Result<VmResult, VmError> {
    eval_eir_impl(src, crate::check::Checker::new())
}

/// Like `eval_eir`, but resolves `[use ...]` modules relative to `base_dir`.
pub fn eval_eir_with_base_dir(src: &str, base_dir: &std::path::Path) -> Result<VmResult, VmError> {
    eval_eir_impl(src, crate::check::Checker::with_base_dir(base_dir))
}

fn eval_eir_impl(src: &str, mut checker: crate::check::Checker) -> Result<VmResult, VmError> {
    let exprs = crate::parser::parse(src).map_err(|e| VmError {
        kind: VmErrorKind::Trap,
        span: Some(e.span),
        context: Some(format!("parse error: {}", e.message)),
    })?;
    let _errors = checker.check_program(&exprs);
    let module = crate::eir::lower::lower(&checker);
    let mut vm = Vm::new(module);
    vm.run()
}

/// Like `eval_eir_with_base_dir`, but records every builtin nondeterministic
/// effect result through `recorder` (see `crate::eir::replay`).
pub fn eval_eir_recorded(
    src: &str,
    base_dir: &std::path::Path,
    recorder: crate::eir::replay::TraceRecorder,
) -> Result<VmResult, VmError> {
    let mut checker = crate::check::Checker::with_base_dir(base_dir);
    let exprs = crate::parser::parse(src).map_err(|e| VmError {
        kind: VmErrorKind::Trap,
        span: Some(e.span),
        context: Some(format!("parse error: {}", e.message)),
    })?;
    let _errors = checker.check_program(&exprs);
    let module = crate::eir::lower::lower(&checker);
    let mut vm = Vm::new(module);
    vm.set_recorder(recorder);
    vm.run()
}

/// Like `eval_eir_with_base_dir`, but feeds recorded effect results back from
/// `entries` instead of executing builtin effects. On success also returns
/// the number of unconsumed trace entries (nonzero means the program ended
/// before using the whole trace — usually a sign the program changed).
pub fn eval_eir_replayed(
    src: &str,
    base_dir: &std::path::Path,
    entries: Vec<crate::eir::replay::TraceEntry>,
) -> Result<(VmResult, usize), VmError> {
    let mut checker = crate::check::Checker::with_base_dir(base_dir);
    let exprs = crate::parser::parse(src).map_err(|e| VmError {
        kind: VmErrorKind::Trap,
        span: Some(e.span),
        context: Some(format!("parse error: {}", e.message)),
    })?;
    let _errors = checker.check_program(&exprs);
    let module = crate::eir::lower::lower(&checker);
    let mut vm = Vm::new(module);
    vm.set_replay(entries);
    let result = vm.run()?;
    Ok((result, vm.replay_remaining()))
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
    fn vm_durable_resume() {
        // Durable execution as a handler: a RESUME tower replays a journal of
        // recorded effect results, then goes live once exhausted. A journal
        // truncated by a "crash" replays its prefix and continues live to the
        // SAME final state as a full run.
        let base = "[effect Llm [complete [String] String]] [effect Tool [call [String] String]] \
                    [fn agent [step h] [let a [Llm.complete [str step]]] \
                      [if [= a \"done\"] h \
                        [do [let lbl [str h \" \" a]] [let r [Tool.call a]] \
                            [agent [+ step 1] [str lbl \":\" r]]]]] \
                    [fn lm [p] [match p \"0\" \"search\" \"1\" \"summarize\" _ \"done\"]] \
                    [fn lt [t] [match t \"search\" \"hit\" \"summarize\" \"sum\" _ \"ok\"]] \
                    [fn replay [journal] [[handle [agent 0 \"s\"] \
                       [return x] [fn [j] x] \
                       [Llm.complete p] [fn [j] [let v [if [empty? j] [lm p] [nth j 0]]] [[resume v] [drop 1 j]]] \
                       [Tool.call t] [fn [j] [let v [if [empty? j] [lt t] [nth j 0]]] [[resume v] [drop 1 j]]]] journal]] ";
        let full = format!(
            "{base} [fn main [] [println [replay #[\"search\" \"hit\" \"summarize\" \"sum\" \"done\"]]]]"
        );
        let crashed = format!("{base} [fn main [] [println [replay #[\"search\" \"hit\"]]]]");
        // Both reach the same final state; the crashed journal replays its prefix
        // then continues live.
        assert_eq!(
            run_output(&full),
            vec!["s search:hit summarize:sum".to_string()]
        );
        assert_eq!(
            run_output(&crashed),
            vec!["s search:hit summarize:sum".to_string()]
        );
    }

    #[test]
    fn vm_agent_under_towers() {
        // The same agent loop runs under different towers. A deterministic test
        // tower (scripted model + mocked tools, approve-all) reproduces a
        // multi-step run offline; a multi-shot "explore" tower resumes each
        // Approval.request BOTH ways to visit every approve/deny world.
        let loop_src = "[effect Llm [complete [String] Keyword]] \
                        [effect Tool [call [Keyword] String]] \
                        [effect Approval [request [Keyword] Bool]] \
                        [fn act [p] [match p \"0\" :search _ :done]] \
                        [fn tool [t] [match t :search \"hit\" _ \"ok\"]] \
                        [fn agent [step h] \
                          [let a [Llm.complete [str step]]] \
                          [if [= a :done] h \
                            [if [Approval.request a] \
                              [agent [+ step 1] [str h \" \" a \":\" [Tool.call a]]] \
                              [agent [+ step 1] [str h \" \" a \":no\"]]]]] ";
        // Deterministic offline run, approve-all.
        let test = format!(
            "{loop_src} [fn main [] [println [handle [agent 0 \"r\"] \
               [Llm.complete p] [resume [act p]] [Tool.call t] [resume [tool t]] \
               [Approval.request a] [resume true]]]]"
        );
        assert_eq!(run_output(&test), vec!["r :search:hit".to_string()]);
        // Multi-shot: resume the approval both ways and join the two worlds.
        let explore = format!(
            "{loop_src} [fn main [] [println [handle [agent 0 \"r\"] \
               [Llm.complete p] [resume [act p]] [Tool.call t] [resume [tool t]] \
               [Approval.request a] [str [resume true] \" | \" [resume false]]]]]"
        );
        assert_eq!(
            run_output(&explore),
            vec!["r :search:hit | r :search:no".to_string()]
        );
    }

    #[test]
    fn vm_http_app_under_towers() {
        // The same app + routes run under different handler towers unchanged: a
        // deterministic test tower and a "prod-ish" tower that differs only in
        // the handler. Routing and capability effects (Auth/Db/Log) work.
        let routes = "[type Request [Req String String String]] \
                      [type Response [Resp Int String]] \
                      [effect Http [route [] Request] [respond [Response] Unit]] \
                      [effect Db [query [String] String]] [effect Auth [require [] String]] \
                      [fn home [] [Http.respond [Resp 200 \"home\"]]] \
                      [fn dash [] [let u [Auth.require]] [let d [Db.query \"q\"]] \
                        [Http.respond [Resp 200 [str u \":\" d]]]] \
                      [fn app [] [match [Http.route] [Req m p b] \
                        [match p \"/\" [home] \"/dash\" [dash] _ [Http.respond [Resp 404 p]]]]] \
                      [fn under [m p who db] [handle [app] [return _] \"\" \
                        [Http.route] [resume [Req m p \"\"]] \
                        [Auth.require] [resume who] [Db.query q] [resume db] \
                        [Http.respond r] [match r [Resp s b] [str \"[\" s \"] \" b]]]] ";
        let prog = format!(
            "{routes} [fn main [] [println [under \"GET\" \"/\" \"x\" \"y\"]] \
             [println [under \"GET\" \"/dash\" \"alice\" \"rows\"]] \
             [println [under \"GET\" \"/nope\" \"x\" \"y\"]]]"
        );
        assert_eq!(
            run_output(&prog),
            vec![
                "[200] home".to_string(),
                "[200] alice:rows".to_string(),
                "[404] /nope".to_string()
            ]
        );
    }

    #[test]
    fn vm_cooperative_scheduler() {
        // The async substrate: a cooperative scheduler as an effect handler,
        // built on interleaved multi-shot continuations. Two forked tasks that
        // each yield once interleave round-robin: A1 B1 A2 B2.
        let src = "[effect Co [fork [] Bool] [yield [] Unit]] \
                   [type Cont [MkCont [-> [Vec Cont] Unit]]] \
                   [fn run-next [q] [if [empty? q] [] [match [nth q 0] [MkCont f] [f [drop 1 q]]]]] \
                   [fn sched [entry] \
                     [[handle [entry] \
                         [return _] [fn [q] [run-next q]] \
                         [Co.yield] [fn [q] [run-next [conj q [MkCont [fn [qp] [[resume []] qp]]]]]] \
                         [Co.fork]  [fn [q] [[resume true] [conj q [MkCont [fn [qp] [[resume false] qp]]]]]]] \
                       #[]]] \
                   [fn w [t] [println [str t 1]] [Co.yield] [println [str t 2]]] \
                   [fn main [] [sched [fn [] [if [Co.fork] [w \"A\"] [w \"B\"]]]]]";
        assert_eq!(
            run_output(src),
            vec![
                "A1".to_string(),
                "B1".to_string(),
                "A2".to_string(),
                "B2".to_string()
            ]
        );
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
    fn vm_int_float_parse() {
        // int/float parse strings (their declared type is Str -> Int/Float).
        assert_eq!(run(r#"[int "42"]"#).as_int(), 42);
        assert_eq!(run(r#"[int "-7"]"#).as_int(), -7);
        assert_eq!(run(r#"[int " 13 "]"#).as_int(), 13);
        assert_eq!(run(r#"[+ [int "40"] 2]"#).as_int(), 42);
        assert!(run(r#"[int "nope"]"#).is_unit());
        assert!((run(r#"[float "3.14"]"#).as_float() - 3.14).abs() < 1e-9);
        // numeric conversions still work.
        assert_eq!(run("[int 9]").as_int(), 9);
        assert_eq!(run("[int 3.9]").as_int(), 3);
    }

    #[test]
    fn vm_ctor_tags_globally_unique() {
        // Constructors of different types must not collide: P (first ctor of B)
        // and X (first ctor of A) used to share per-type tag 0, so [X n] matched
        // a P value. Tags are now globally unique.
        let v = run("[type A [X Int] [Y]] [type B [P Int] [Q]] \
                     [match [P 5] [X n] 1 [Y] 2 [P n] n [Q] 4]");
        assert_eq!(v.as_int(), 5);
        let v = run("[type A [X Int] [Y]] [type B [P Int] [Q]] \
                     [match [Y] [X n] 1 [Y] 2 [P n] 3 [Q] 4]");
        assert_eq!(v.as_int(), 2);
    }

    #[test]
    fn vm_effect_abort_and_resume() {
        let prog = |h: &str| {
            format!(
                "[effect E [op [Int] Int]] [fn d [x] #{{E}} [+ 100 [E.op x]]] [handle [d 5] {h}]"
            )
        };
        // Abort: a non-resuming handler discards the body's continuation
        // (the `+ 100` after the perform never runs). This is what makes `try`
        // work; it used to wrongly return 1099.
        assert_eq!(run(&prog("[E.op v] 999")).as_int(), 999);
        // Tail resume: the continuation runs and the body completes ([+ 100 5]).
        assert_eq!(run(&prog("[E.op v] [resume v]")).as_int(), 105);
        // Resume NOT in tail position: the handler does work after the
        // continuation's result comes back ([+ 1000 105]).
        assert_eq!(run(&prog("[E.op v] [+ 1000 [resume v]]")).as_int(), 1105);
        // Return clause transforms the body's normal-completion value.
        assert_eq!(run("[handle 42 [return x] [+ x 100]]").as_int(), 142);
    }

    #[test]
    fn vm_effect_multi_shot() {
        // A multi-shot continuation may be resumed more than once. The captured
        // segment ([+ 100 _]) is cloned per resume, so each `[resume 5]` yields
        // 105 independently: [+ 105 105] = 210.
        assert_eq!(
            run(
                "[effect E [op [Int] Int]] [fn d [x] #{E} [+ 100 [E.op x]]] \
                 [handle [d 5] [E.op v] [+ [resume v] [resume v]]]"
            )
            .as_int(),
            210
        );
        // Three resumes compose the same way: [+ 105 [+ 105 105]] = 315.
        assert_eq!(
            run(
                "[effect E [op [Int] Int]] [fn d [x] #{E} [+ 100 [E.op x]]] \
                 [handle [d 5] [E.op v] [+ [resume v] [+ [resume v] [resume v]]]]"
            )
            .as_int(),
            315
        );
    }

    #[test]
    fn vm_multi_file_use() {
        // Multi-file `use` runs on the EIR VM (LIM-5): an imported module's pub
        // functions are callable both qualified (`mod.fn`) and via selective
        // import. Module files are resolved relative to base_dir.
        let dir = std::env::temp_dir().join(format!("loon_use_{}", std::process::id()));
        let _ = std::fs::create_dir_all(&dir);
        std::fs::write(dir.join("mymath.oo"), "[pub fn add [a b] [+ a b]]\n").unwrap();
        let r = eval_eir_with_base_dir(
            "[use mymath] [fn main [] [println [mymath.add 40 2]]]",
            &dir,
        )
        .expect("vm error");
        assert_eq!(r.output, vec!["42".to_string()]);
        let r2 =
            eval_eir_with_base_dir("[use mymath [add]] [fn main [] [println [add 1 2]]]", &dir)
                .expect("vm error");
        assert_eq!(r2.output, vec!["3".to_string()]);
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn vm_host_effects() {
        // Host effects are wired into the EIR VM (LIM-4): unhandled IO.now /
        // IO.uuid / Process.env reach real implementations. now is a positive
        // Unix timestamp; uuid is a 36-char string; env reads the environment.
        assert!(run("[IO.now]").as_int() > 1_000_000_000);
        assert!(run("[IO.millis]").as_int() > 1_000_000_000_000);
        let uuid = &run_output("[println [IO.uuid]]")[0];
        assert_eq!(uuid.len(), 36, "uuid: {uuid}");
        assert_eq!(uuid.matches('-').count(), 4);
        // Process.env matches the interpreter: [Some value] / None.
        std::env::set_var("LOON_TEST_VAR", "hello-env");
        assert_eq!(
            run_output(r#"[println [Process.env "LOON_TEST_VAR"]]"#),
            vec!["[Some \"hello-env\"]".to_string()]
        );
        assert_eq!(
            run_output(r#"[println [Process.env "LOON_DEFINITELY_UNSET_VAR"]]"#),
            vec!["None".to_string()]
        );
        // Env.lookup was a VM-only convenience the interpreter never had; it
        // is gone for cross-backend conformance (Process.env is the shared
        // form), so it now hits the unhandled-effect hard error like interp.
        let err = eval_eir(r#"[fn main [] [println [Env.lookup "LOON_TEST_VAR"]]]"#)
            .expect_err("Env.lookup should be unhandled");
        assert!(
            matches!(err.kind, VmErrorKind::UnhandledEffect(ref n) if n == "Env.lookup"),
            "got: {err}"
        );
    }

    #[test]
    fn vm_json_effects_match_interp() {
        // IO.to-json serializes maps/vecs like the interpreter…
        assert_eq!(
            run_output("[fn main [] [println [IO.to-json {:a 1}]]]"),
            vec![r#"{"a":1}"#.to_string()]
        );
        // …and IO.parse-json produces keyword-keyed maps whose keys are equal
        // to same-named source keywords (runtime symbol interning), plus
        // vec/bool/null payloads mapped as the interpreter maps them.
        assert_eq!(
            run_output(
                r#"[fn main []
                     [let m [IO.parse-json "{\"a\": 42, \"b\": [1, 2, true, null]}"]]
                     [println [get m :a]]
                     [println [get m :b]]]"#
            ),
            vec!["42".to_string(), "#[1 2 true ()]".to_string()]
        );
    }

    #[test]
    fn vm_process_exec_matches_interp_shape() {
        // {:exit-code Int :stdout Str :stderr Str}, as the interpreter.
        assert_eq!(
            run_output(
                r#"[fn main []
                     [let r [Process.exec "echo hi"]]
                     [println [get r :exit-code]]
                     [println [contains? [get r :stdout] "hi"]]]"#
            ),
            vec!["0".to_string(), "true".to_string()]
        );
    }

    #[test]
    fn vm_unknown_effect_op_is_a_hard_error() {
        // An effect op with neither a handler nor a builtin implementation
        // must ERROR, not silently evaluate to () — the silent path is how
        // "IO.write-file did nothing" class bugs hide.
        let err = eval_eir("[effect Zap [zap [] Int]] [fn main [] [println [Zap.zap]]]")
            .expect_err("unhandled effect should be an error");
        assert!(
            matches!(err.kind, VmErrorKind::UnhandledEffect(ref n) if n == "Zap.zap"),
            "got: {err}"
        );
        // ...but a handled one is fine.
        let out = run_output(
            "[effect Zap [zap [] Int]] \
             [fn main [] [println [handle [Zap.zap] [Zap.zap] [resume 7]]]]",
        );
        assert_eq!(out, vec!["7".to_string()]);
    }

    #[test]
    fn vm_int_divide_by_zero_raises() {
        // Integer / and % by zero must ERROR, not silently return () — the
        // same silent-failure class as unhandled effects. Matches the
        // interpreter's "division by zero" / "modulo by zero" wording.
        let derr = eval_eir("[fn main [] [println [/ 5 0]]]")
            .expect_err("[/ 5 0] should error, not return ()");
        assert!(
            matches!(derr.kind, VmErrorKind::DivideByZero("division")),
            "got: {derr}"
        );
        assert!(derr.to_string().contains("division by zero"), "got: {derr}");

        let merr = eval_eir("[fn main [] [println [% 5 0]]]")
            .expect_err("[% 5 0] should error, not return ()");
        assert!(
            matches!(merr.kind, VmErrorKind::DivideByZero("modulo")),
            "got: {merr}"
        );
        assert!(merr.to_string().contains("modulo by zero"), "got: {merr}");

        // Non-zero divisors still work.
        assert_eq!(run("[/ 17 5]").as_int(), 3);
        assert_eq!(run("[% 17 5]").as_int(), 2);

        // Float division by zero is IEEE infinity, NOT an error.
        assert!(run("[/ 1.0 0.0]").as_float().is_infinite());
    }

    #[test]
    fn vm_io_write_file_round_trips() {
        // IO.write-file must actually write (it silently no-oped on early VM
        // builds); read it back through IO.read-file on the same backend.
        let dir = std::env::temp_dir().join(format!("loon_vm_wf_{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("out.txt");
        let p = path.to_string_lossy().replace('\\', "/");
        let out = run_output(&format!(
            r#"[fn main [] [IO.write-file "{p}" "written-by-vm"] [println [IO.read-file "{p}"]]]"#
        ));
        assert_eq!(out, vec!["written-by-vm".to_string()]);
        assert_eq!(std::fs::read_to_string(&path).unwrap(), "written-by-vm");
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn vm_tail_resume_runs_in_constant_stack() {
        // The syscall fast path: a handler clause ending in [resume ...] seals
        // as a tail resume (End::TailInvoke), so a perform/resume loop must not
        // grow the frame stack with iteration count. Before this, each cycle
        // leaked the handler frame as a fresh prompt, the captured segment grew
        // per iteration, and effect loops were O(N^2) time / O(N) memory.
        let run_n = |n: i64| {
            let src = format!(
                "[effect Tick [next [Int] Int]] \
                 [fn work [acc n] [if [<= n 0] acc [recur [Tick.next acc] [- n 1]]]] \
                 [fn main [] [println [handle [work 0 {n}] [Tick.next a] [resume [+ a 1]]]]]"
            );
            let r = eval_eir(&src).expect("vm error");
            assert_eq!(r.output, vec![n.to_string()]);
            r.heap_stats.max_frames
        };
        let small = run_n(100);
        let large = run_n(4000);
        assert_eq!(small, large, "frame high-water mark must not scale with N");
        assert!(large < 10, "expected O(1) frames, got {large}");
    }

    #[test]
    fn vm_unhandled_fail_is_loud() {
        // An uncaught Fail must raise a loud error, not silently collapse to
        // unit. This covers the deep-handler case: a `try` installed INSIDE a
        // handled body is frozen into the continuation when a clause runs, so a
        // Fail raised by that clause finds no live handler — it must error, not
        // vanish. (Matches the tree-walking interpreter.)
        let src = "[effect E [op [Int] Int]] \
                   [fn body [] [try [E.op 1] [fn [m] [str \"caught \" m]]]] \
                   [fn main [] [handle [body] [E.op x] [Fail.fail \"denied\"]]]";
        let err = eval_eir(src).expect_err("uncaught Fail should error");
        assert!(
            matches!(err.kind, VmErrorKind::UnhandledEffect(ref n) if n == "Fail.fail"),
            "expected UnhandledEffect(Fail.fail), got {err:?}"
        );
    }

    #[test]
    fn vm_unhandled_user_effect_is_loud() {
        // A user-defined effect with no handler and no builtin meaning is an
        // error too, not silent unit.
        let src = "[effect Foo [bar [] Int]] [fn main [] [Foo.bar]]";
        let err = eval_eir(src).expect_err("unhandled user effect should error");
        assert!(matches!(err.kind, VmErrorKind::UnhandledEffect(ref n) if n == "Foo.bar"));
    }

    #[test]
    fn vm_outer_try_still_catches_clause_fail() {
        // A `try` ENCLOSING the whole handle stays live while the clause runs,
        // so it still catches a Fail the clause raises — only the frozen
        // inner-try case is uncatchable.
        let src = "[effect E [op [Int] Int]] \
                   [fn guarded [] [handle [E.op 1] [E.op x] [Fail.fail \"boom\"]]] \
                   [fn main [] [println [try [guarded] [fn [m] [str \"caught: \" m]]]]]";
        assert_eq!(run_output(src), vec!["caught: boom".to_string()]);
    }

    #[test]
    fn vm_handler_isolation_across_handles() {
        // Regression: a non-tail resume re-establishes the handle's handlers on
        // the dynamic stack (so the continuation is self-contained). Those
        // ephemeral handlers must be dropped when their prompt frame leaves —
        // otherwise a completed handle shadows a *later* one for the same
        // effect. Here `flat` (R+L, with a NON-tail resume for L) runs first,
        // then a NESTED handle (outer L, inner R) for the same program. The
        // nested R must reach the inner handler (2), not the stale flat one (1).
        let src = "[effect R [ask [] Int]] [effect L [note [] Int]] \
                   [fn prog [] [L.note] [R.ask]] \
                   [fn flat [b] [handle [b] \
                       [R.ask]  [resume 1] \
                       [L.note] [+ 0 [resume 0]]]] \
                   [fn nested [b] \
                     [handle [[fn [] [handle [b] [R.ask] [resume 2]]]] \
                        [L.note] [resume 0]]] \
                   [fn main [] [flat prog] [nested prog]]";
        assert_eq!(run(src).as_int(), 2);
        // And the symmetric multi-clause flat-then-flat case stays correct.
        let src2 = "[effect R [ask [] Int]] [effect L [note [] Int]] \
                    [fn prog [] [L.note] [R.ask]] \
                    [fn a [b] [handle [b] [R.ask] [resume 10] [L.note] [+ 0 [resume 0]]]] \
                    [fn c [b] [handle [b] [R.ask] [resume 20] [L.note] [+ 0 [resume 0]]]] \
                    [fn main [] [a prog] [c prog]]";
        assert_eq!(run(src2).as_int(), 20);
    }

    #[test]
    fn vm_effect_state_escaping() {
        // A pure `State` effect via the function-passing encoding: the handlers
        // return functions and the continuation is resumed AFTER the `handle`
        // has exited (an escaping continuation). Threads state through get/put.
        let rs = "[effect State [get [] Int] [put [Int] Unit]] \
                  [fn run-state [t init] \
                    [[handle [t] \
                        [return x]    [fn [s] x] \
                        [State.get]   [fn [s] [[resume s] s]] \
                        [State.put n] [fn [s] [[resume 0] n]]] \
                      init]] ";
        // get; +1; get; +10; get  threaded from 0 -> 11
        let counter = format!(
            "{rs} [fn counter [] [let a [State.get]] [State.put [+ a 1]] \
             [let b [State.get]] [State.put [+ b 10]] [State.get]] \
             [run-state counter 0]"
        );
        assert_eq!(run(&counter).as_int(), 11);
        // A recursive stateful loop summing 1..5.
        let loopsum = format!(
            "{rs} [fn lp [i] [if [> i 5] [State.get] \
             [do [State.put [+ [State.get] i]] [lp [+ i 1]]]]] \
             [fn c [] [lp 1]] [run-state c 0]"
        );
        assert_eq!(run(&loopsum).as_int(), 15);
    }

    #[test]
    fn vm_interpolation() {
        // \(expr) interpolates...
        assert_eq!(
            run_output(r#"[let n "world"] [println "hi \(n)"]"#),
            vec!["hi world"]
        );
        assert_eq!(run_output(r#"[println "2+2=\([+ 2 2])"]"#), vec!["2+2=4"]);
        // ...and bare braces are ordinary literal characters (no escaping).
        assert_eq!(
            run_output(r#"[println "{:a 1 :b 2}"]"#),
            vec!["{:a 1 :b 2}"]
        );
        assert_eq!(
            run_output(r##"[println "#{IO Fail}"]"##),
            vec!["#{IO Fail}"]
        );
    }

    #[test]
    fn vm_map_insertion_order() {
        // keys / display follow insertion order deterministically (not hash
        // order). assoc of an existing key keeps its position; a new key appends.
        assert_eq!(
            run_output(r#"[println [keys [assoc [assoc {:x 1} :y 2] :z 3]]]"#),
            vec!["#[:x :y :z]"]
        );
        assert_eq!(
            run_output(r#"[println [assoc [assoc {:x 1} :y 2] :x 9]]"#),
            vec!["{:x 9 :y 2}"]
        );
        assert_eq!(
            run_output(r#"[println {:a 1 :b 2 :c 3}]"#),
            vec!["{:a 1 :b 2 :c 3}"]
        );
    }

    #[test]
    fn vm_user_fn_shadows_builtin() {
        // A user [fn sum …] overrides the builtin `sum` instead of being ignored.
        assert_eq!(run("[fn sum [x] 99] [sum #[1 2 3]]").as_int(), 99);
        // Builtins still resolve when not shadowed.
        assert_eq!(run("[sum #[1 2 3]]").as_int(), 6);
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

    /// A failed trace write must stop recording entirely. Continuing would
    /// finalize a trace with a silent gap: replay would feed later recorded
    /// values one step early with no divergence diagnostic.
    #[test]
    fn recorder_disables_after_write_failure() {
        use std::io::Write as _;
        use std::sync::{Arc, Mutex};

        /// Shared buffer that rejects any write mentioning "uuid".
        struct FailOnUuid(Arc<Mutex<Vec<u8>>>);
        impl std::io::Write for FailOnUuid {
            fn write(&mut self, data: &[u8]) -> std::io::Result<usize> {
                if data.windows(4).any(|w| w == b"uuid") {
                    return Err(std::io::Error::other("disk full"));
                }
                self.0.lock().unwrap().write(data)
            }
            fn flush(&mut self) -> std::io::Result<()> {
                Ok(())
            }
        }

        let buf = Arc::new(Mutex::new(Vec::new()));
        let recorder =
            crate::eir::replay::TraceRecorder::from_writer(Box::new(FailOnUuid(buf.clone())));
        // Three recordable ops: millis succeeds, uuid's write fails, and the
        // final millis must NOT be appended after the gap.
        let result = eval_eir_recorded(
            "[fn main [] [IO.millis] [IO.uuid] [IO.millis]]",
            std::path::Path::new("."),
            recorder,
        );
        assert!(result.is_ok(), "run should survive a trace write failure");
        let written = String::from_utf8(buf.lock().unwrap().clone()).unwrap();
        assert_eq!(
            written.matches(":op \"millis\"").count(),
            1,
            "recording must stop at the failed entry, not resume with a gap:\n{written}"
        );
        assert!(!written.contains("uuid"), "trace:\n{written}");
    }
}
