//! The EIR interpreter, on bare metal.
//!
//! Structurally this is the host VM with the host removed: same frame stack,
//! same dynamic handler stack keyed by prompt depth, same continuation
//! capture on `Perform`. Deep-handler semantics are load-bearing — a clause
//! that re-performs its own effect must forward *outward*, which only works
//! if capturing moves every handler at or above the prompt into the
//! continuation. Divergence from the host here would mean a program means
//! one thing on Linux and another on hardware, which is the whole thing we
//! are trying not to build.

use alloc::rc::Rc;
use alloc::string::{String, ToString};
use alloc::vec;
use alloc::vec::Vec;

use super::val::{show, Val};
use super::*;

/// What the VM reaches for when an effect has no Loon handler left. On this
/// target that means hardware.
pub trait Host {
    fn write(&mut self, s: &str);
    /// Monotonic ticks since boot.
    fn ticks(&mut self) -> i64;
}

pub struct Frame {
    func: FuncId,
    block: BlockId,
    ip: usize,
    regs: Vec<Val>,
    captures: Rc<Vec<Val>>,
    /// Where the callee's value lands in this frame, or `DISCARD`.
    ret_reg: u32,
}

/// A frame whose callee's result is thrown away — used to park a caller
/// while Rust drives a nested run, where the value comes back directly.
const DISCARD: u32 = u32::MAX;

/// A suspended computation: the frames between a `perform` and its prompt.
pub struct Continuation {
    saved: Vec<Frame>,
    func: FuncId,
    block: BlockId,
    ip: usize,
    regs: Vec<Val>,
    captures: Rc<Vec<Val>>,
    perform_dst: u32,
    /// Handlers that lived at or above the prompt, with depths stored
    /// relative to it so they can be re-established wherever this segment
    /// is resumed.
    prompt_handlers: Vec<DynHandler>,
}

#[derive(Clone)]
struct DynHandler {
    effect: StringId,
    op: StringId,
    closure: Val,
    prompt_depth: usize,
    /// Re-established by a resume rather than by a `PushHandler`, so it has
    /// no matching `PopHandler` and must be pruned by frame depth instead.
    ephemeral: bool,
}

pub struct Vm<'m, H: Host> {
    m: &'m Module,
    host: &'m mut H,
    frames: Vec<Frame>,
    handlers: Vec<DynHandler>,
    func: FuncId,
    block: BlockId,
    ip: usize,
    regs: Vec<Val>,
    captures: Rc<Vec<Val>>,
    /// Bounds runaway programs; there is no watchdog timer to save us yet.
    fuel: u64,
}

pub type VmResult<T> = Result<T, String>;

impl<'m, H: Host> Vm<'m, H> {
    pub fn new(m: &'m Module, host: &'m mut H) -> Self {
        let entry = &m.funcs[m.entry.0 as usize];
        Vm {
            regs: vec![Val::Unit; entry.regs as usize],
            captures: Rc::new(Vec::new()),
            func: m.entry,
            block: BlockId(0),
            ip: 0,
            frames: Vec::new(),
            handlers: Vec::new(),
            m,
            host,
            fuel: u64::MAX,
        }
    }

    pub fn with_fuel(mut self, fuel: u64) -> Self {
        self.fuel = fuel;
        self
    }

    // ── Register access ────────────────────────────────────────────────

    fn r(&self, r: Reg) -> Val {
        self.regs.get(r.0 as usize).cloned().unwrap_or(Val::Unit)
    }

    fn w(&mut self, r: Reg, v: Val) {
        let i = r.0 as usize;
        if i >= self.regs.len() {
            self.regs.resize(i + 1, Val::Unit);
        }
        self.regs[i] = v;
    }

    fn read(&self, rs: &[Reg]) -> Vec<Val> {
        rs.iter().map(|r| self.r(*r)).collect()
    }

    fn func_def(&self, f: FuncId) -> VmResult<&'m Func> {
        self.m
            .funcs
            .get(f.0 as usize)
            .ok_or_else(|| alloc::format!("bad function id {}", f.0))
    }

    // ── Driving ────────────────────────────────────────────────────────

    /// Run the entry point to completion.
    pub fn run(&mut self) -> VmResult<Val> {
        let base = self.frames.len();
        self.run_until(base)
    }

    /// Execute until the frame stack drops back to `base` and the current
    /// function returns. Nested runs (a builtin calling back into Loon) use
    /// the same loop with a higher floor.
    fn run_until(&mut self, base: usize) -> VmResult<Val> {
        loop {
            if self.fuel == 0 {
                return Err("out of fuel: the program did not terminate".to_string());
            }
            self.fuel -= 1;

            // Borrow the code out of the module reference, not out of `self`:
            // `'m` outlives this loop, so ops stay borrowed while `self` is
            // mutated. Cloning each instruction instead would allocate on
            // every dispatch, which for ops carrying operand vectors is most
            // of them.
            let m = self.m;
            let f = m
                .funcs
                .get(self.func.0 as usize)
                .ok_or_else(|| alloc::format!("bad function id {}", self.func.0))?;
            let block = f
                .blocks
                .get(self.block.0 as usize)
                .ok_or_else(|| alloc::format!("bad block id {}", self.block.0))?;

            if self.ip < block.ops.len() {
                let op = &block.ops[self.ip];
                self.ip += 1;
                self.exec(op)?;
                continue;
            }

            match &block.end {
                End::Ret(r) => {
                    let v = self.r(*r);
                    if self.frames.len() == base {
                        return Ok(v);
                    }
                    self.ret(v)?;
                }
                End::Jmp(b, args) => {
                    let vals = self.read(args);
                    self.jump(*b, &vals)?;
                }
                End::Br(c, t, e) => {
                    let target = if self.r(*c).truthy() { *t } else { *e };
                    self.jump(target, &[])?;
                }
                End::Switch(scrut, arms, dflt) => {
                    let tag = match self.r(*scrut) {
                        Val::Adt(t, _) => Some(t),
                        Val::Int(n) => Some(n as u16),
                        _ => None,
                    };
                    let target = tag
                        .and_then(|t| arms.iter().find(|(a, _)| *a == t).map(|(_, b)| *b))
                        .unwrap_or(*dflt);
                    self.jump(target, &[])?;
                }
                End::Recur(args) => {
                    let vals = self.read(args);
                    self.jump(BlockId(0), &vals)?;
                }
                End::Tail(callee, args) => {
                    let vals = self.read(args);
                    self.enter(*callee, &vals, Rc::new(Vec::new()))?;
                }
                End::TailInvoke(f, args) => {
                    let callee = self.r(*f);
                    let vals = self.read(args);
                    // A tail call must not push a frame — that is the whole
                    // promise — so it cannot reuse `invoke`'s path.
                    match callee {
                        Val::Closure(fid, caps) => self.enter(fid, &vals, caps)?,
                        Val::Cont(k) => {
                            let v = vals.into_iter().next().unwrap_or(Val::Unit);
                            self.resume(&k, v)?;
                        }
                        other => {
                            return Err(alloc::format!(
                                "cannot call a {} in tail position",
                                other.type_name()
                            ))
                        }
                    }
                }
                End::Trap => {
                    return Err("reached unreachable code (non-exhaustive match?)".to_string())
                }
            }
        }
    }

    /// Jump within the current function, binding the target's block params.
    fn jump(&mut self, b: BlockId, args: &[Val]) -> VmResult<()> {
        let f = self.func_def(self.func)?;
        let target = f
            .blocks
            .get(b.0 as usize)
            .ok_or_else(|| alloc::format!("bad block id {}", b.0))?;
        let params = target.params.clone();
        for (p, v) in params.iter().zip(args.iter()) {
            self.w(*p, v.clone());
        }
        self.block = b;
        self.ip = 0;
        Ok(())
    }

    /// Replace the current frame with a call to `callee` (tail position).
    fn enter(&mut self, callee: FuncId, args: &[Val], caps: Rc<Vec<Val>>) -> VmResult<()> {
        let f = self.func_def(callee)?;
        if f.blocks.is_empty() {
            return Err("function has no blocks".to_string());
        }
        // Arguments land in registers 0..n, not in the entry block's params:
        // lowering numbers parameters first and the entry block inherits
        // them rather than being jumped to with operands.
        let n = (f.regs as usize).max(args.len());
        let mut regs = vec![Val::Unit; n];
        for (i, v) in args.iter().enumerate() {
            regs[i] = v.clone();
        }
        self.func = callee;
        self.block = BlockId(0);
        self.ip = 0;
        self.regs = regs;
        self.captures = caps;
        Ok(())
    }

    /// Push a frame and call `callee`, returning into `ret_reg`.
    fn call(
        &mut self,
        callee: FuncId,
        args: &[Val],
        ret_reg: u32,
        caps: Rc<Vec<Val>>,
    ) -> VmResult<()> {
        self.frames.push(Frame {
            func: self.func,
            block: self.block,
            ip: self.ip,
            regs: core::mem::take(&mut self.regs),
            captures: core::mem::replace(&mut self.captures, Rc::new(Vec::new())),
            ret_reg,
        });
        if self.frames.len() > 8192 {
            return Err("call stack exhausted".to_string());
        }
        self.enter(callee, args, caps)
    }

    fn ret(&mut self, v: Val) -> VmResult<()> {
        let fr = self
            .frames
            .pop()
            .ok_or_else(|| "return with no caller".to_string())?;
        self.func = fr.func;
        self.block = fr.block;
        self.ip = fr.ip;
        self.regs = fr.regs;
        self.captures = fr.captures;
        if fr.ret_reg != DISCARD {
            self.w(Reg(fr.ret_reg), v);
        }
        // A resumed segment can leave ephemeral handlers scoped to a prompt
        // frame that just left the stack; drop them before they shadow a
        // later handle for the same effect.
        self.prune_ephemeral();
        Ok(())
    }

    /// Call a Loon value from Rust (a builtin taking a function, say) and
    /// run it to completion.
    fn apply(&mut self, f: &Val, args: &[Val]) -> VmResult<Val> {
        match f {
            Val::Closure(fid, caps) => {
                let base = self.frames.len();
                // Park the caller so the nested run has somewhere to return
                // to; reg 0 is scratch, the value comes back through `run_until`.
                self.call(*fid, args, DISCARD, caps.clone())?;
                let out = self.run_until(base + 1)?;
                // `run_until` stops *before* popping, so unwind by hand.
                self.ret(out.clone())?;
                Ok(out)
            }
            Val::Cont(k) => {
                let v = args.first().cloned().unwrap_or(Val::Unit);
                let base = self.frames.len();
                self.resume(k, v)?;
                self.run_until(base)
            }
            other => Err(alloc::format!("cannot call a {}", other.type_name())),
        }
    }
}

// ── Instruction execution ──────────────────────────────────────────────

impl<'m, H: Host> Vm<'m, H> {
    fn exec(&mut self, op: &Op) -> VmResult<()> {
        match op {
            Op::Lit(d, l) => {
                let v = self.lit(l);
                self.w(*d, v);
            }
            Op::Mov(d, a) => {
                let v = self.r(*a);
                self.w(*d, v);
            }
            Op::Upval(d, i) => {
                let v = self.captures.get(*i as usize).cloned().unwrap_or(Val::Unit);
                self.w(*d, v);
            }
            Op::Bin(d, o, a, b) => {
                let (x, y) = (self.r(*a), self.r(*b));
                let v = self.binop(*o, x, y)?;
                self.w(*d, v);
            }
            Op::Un(d, o, a) => {
                let x = self.r(*a);
                let v = match o {
                    UnOp::Neg => match x {
                        Val::Int(n) => Val::Int(-n),
                        Val::Float(f) => Val::Float(-f),
                        v => return Err(alloc::format!("cannot negate a {}", v.type_name())),
                    },
                    UnOp::Not => Val::Bool(!x.truthy()),
                };
                self.w(*d, v);
            }
            Op::Call(d, f, args) => {
                let vals = self.read(args);
                self.call(*f, &vals, d.0, Rc::new(Vec::new()))?;
            }
            Op::Invoke(d, f, args) => {
                let callee = self.r(*f);
                let vals = self.read(args);
                match callee {
                    Val::Closure(fid, caps) => self.call(fid, &vals, d.0, caps)?,
                    Val::Cont(k) => {
                        let v = vals.into_iter().next().unwrap_or(Val::Unit);
                        self.resume_at(&k, v, Some(d.0))?;
                    }
                    other => {
                        return Err(alloc::format!("cannot call a {}", other.type_name()));
                    }
                }
            }
            Op::Close(d, f, caps) => {
                let vals = self.read(caps);
                self.w(*d, Val::Closure(*f, Rc::new(vals)));
            }
            Op::Vec(d, rs) => {
                let vals = self.read(rs);
                self.w(*d, Val::Vec(Rc::new(vals)));
            }
            Op::Tup(d, rs) => {
                let vals = self.read(rs);
                self.w(*d, Val::Tup(Rc::new(vals)));
            }
            Op::Set(d, rs) => {
                let mut vals: Vec<Val> = Vec::new();
                for v in self.read(rs) {
                    if !vals.contains(&v) {
                        vals.push(v);
                    }
                }
                self.w(*d, Val::Set(Rc::new(vals)));
            }
            Op::Map(d, kvs) => {
                let mut out: Vec<(Val, Val)> = Vec::with_capacity(kvs.len());
                for (k, v) in kvs {
                    let (k, v) = (self.r(*k), self.r(*v));
                    match out.iter_mut().find(|(k2, _)| *k2 == k) {
                        Some(slot) => slot.1 = v,
                        None => out.push((k, v)),
                    }
                }
                self.w(*d, Val::Map(Rc::new(out)));
            }
            Op::Adt(d, tag, rs) => {
                let vals = self.read(rs);
                self.w(*d, Val::Adt(*tag, Rc::new(vals)));
            }
            Op::Tag(d, a) => {
                let v = match self.r(*a) {
                    Val::Adt(t, _) => Val::Int(t as i64),
                    _ => Val::Int(-1),
                };
                self.w(*d, v);
            }
            Op::Field(d, a, sel) => {
                let base = self.r(*a);
                let v = self.field(&base, sel)?;
                self.w(*d, v);
            }
            Op::Builtin(d, tag, args) => {
                let vals = self.read(args);
                let v = self.builtin(*tag, &vals)?;
                self.w(*d, v);
            }
            Op::PushHandler(h, eff, o) => {
                let closure = self.r(*h);
                self.handlers.push(DynHandler {
                    effect: *eff,
                    op: *o,
                    closure,
                    // The prompt is the `handle`'s own frame; the body runs
                    // above it.
                    prompt_depth: self.frames.len(),
                    ephemeral: false,
                });
            }
            Op::PopHandler => {
                // Depth-matched, not a blind pop: if the body performed, the
                // capture already moved this handle's handlers into the
                // continuation, and popping blindly would take some outer
                // handle's instead.
                let depth = self.frames.len();
                if let Some(i) = self.handlers.iter().rposition(|h| h.prompt_depth == depth) {
                    self.handlers.remove(i);
                }
            }
            Op::Perform(d, eff, o, args) => {
                let vals = self.read(args);
                self.perform(*d, *eff, *o, vals)?;
            }
        }
        Ok(())
    }

    fn lit(&self, l: &Lit) -> Val {
        match l {
            Lit::Int(n) => Val::Int(*n),
            Lit::Float(f) => Val::Float(*f),
            Lit::Bool(b) => Val::Bool(*b),
            Lit::Str(s) => Val::Str(Rc::new(self.m.string(*s).to_string())),
            Lit::Keyword(s) => Val::Keyword(Rc::new(self.m.string(*s).to_string())),
            Lit::Unit => Val::Unit,
        }
    }

    fn field(&self, base: &Val, sel: &Selector) -> VmResult<Val> {
        Ok(match sel {
            Selector::Index(i) => match base {
                Val::Tup(xs) | Val::Vec(xs) | Val::Adt(_, xs) => {
                    xs.get(*i as usize).cloned().unwrap_or(Val::Unit)
                }
                v => return Err(alloc::format!("cannot index a {}", v.type_name())),
            },
            Selector::Key(s) | Selector::Name(s) => {
                let key = self.m.string(*s);
                match base {
                    Val::Map(kvs) => kvs
                        .iter()
                        .find(|(k, _)| match k {
                            Val::Str(t) | Val::Keyword(t) => t.as_str() == key,
                            _ => false,
                        })
                        .map(|(_, v)| v.clone())
                        .unwrap_or(Val::Unit),
                    v => {
                        return Err(alloc::format!(
                            "cannot read field '{key}' of a {}",
                            v.type_name()
                        ))
                    }
                }
            }
        })
    }

    // ── Effects ────────────────────────────────────────────────────────

    fn prune_ephemeral(&mut self) {
        let depth = self.frames.len();
        self.handlers
            .retain(|h| !h.ephemeral || h.prompt_depth < depth);
    }

    /// Perform an effect: find the innermost handler, capture everything
    /// between here and its prompt as a continuation, and run the clause at
    /// the prompt with `resume` bound to that continuation.
    fn perform(&mut self, dst: Reg, eff: StringId, o: StringId, args: Vec<Val>) -> VmResult<()> {
        let found = self
            .handlers
            .iter()
            .rev()
            .find(|h| h.effect == eff && h.op == o)
            .map(|h| (h.closure.clone(), h.prompt_depth));

        let Some((hval, prompt_depth)) = found else {
            // Nothing in Loon handles this, so it falls through to hardware.
            let v = self.hardware(eff, o, &args)?;
            self.w(dst, v);
            return Ok(());
        };

        // Deep-handler semantics: every handler at or above the prompt moves
        // into the snapshot, including the prompt's own. That is what makes a
        // clause re-performing its own effect forward *outward* instead of
        // recursing into itself.
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
        let k = Continuation {
            saved,
            func: self.func,
            block: self.block,
            ip: self.ip,
            regs: core::mem::take(&mut self.regs),
            captures: core::mem::replace(&mut self.captures, Rc::new(Vec::new())),
            perform_dst: dst.0,
            prompt_handlers,
        };
        let k = Val::Cont(Rc::new(k));

        // Restore the prompt frame as current; its ret_reg is where the whole
        // `handle` expression's value belongs.
        let f0 = self
            .frames
            .pop()
            .ok_or_else(|| "perform with no prompt frame".to_string())?;
        let handle_ret = f0.ret_reg;
        self.func = f0.func;
        self.block = f0.block;
        self.ip = f0.ip;
        self.regs = f0.regs;
        self.captures = f0.captures;
        self.prune_ephemeral();

        match hval {
            Val::Closure(fid, caps) => {
                let mut call_args = vec![k];
                call_args.extend(args);
                self.call(fid, &call_args, handle_ret, caps)
            }
            other => Err(alloc::format!(
                "handler for {}.{} is a {}, not a function",
                self.m.string(eff),
                self.m.string(o),
                other.type_name()
            )),
        }
    }

    fn resume(&mut self, k: &Continuation, v: Val) -> VmResult<()> {
        self.resume_inner(k, v, None)
    }

    fn resume_at(&mut self, k: &Continuation, v: Val, dst: Option<u32>) -> VmResult<()> {
        self.resume_inner(k, v, dst)
    }

    /// Re-install a captured segment and run on from the `perform` that
    /// produced it, with `v` as that perform's value.
    fn resume_inner(&mut self, k: &Continuation, v: Val, dst: Option<u32>) -> VmResult<()> {
        if let Some(dst) = dst {
            // Park the clause's frame as a fresh prompt, so the continuation
            // stays self-contained even when resumed after its original
            // `handle` has already exited.
            self.frames.push(Frame {
                func: self.func,
                block: self.block,
                ip: self.ip,
                regs: core::mem::take(&mut self.regs),
                captures: core::mem::replace(&mut self.captures, Rc::new(Vec::new())),
                ret_reg: dst,
            });
        }

        // Snapshot depths are relative to the prompt; the saved frames go
        // directly above the current top, so absolute = prompt + relative.
        let prompt = self.frames.len().saturating_sub(1);
        for h in &k.prompt_handlers {
            self.handlers.push(DynHandler {
                prompt_depth: prompt + h.prompt_depth,
                ephemeral: true,
                ..h.clone()
            });
        }
        for f in &k.saved {
            self.frames.push(Frame {
                func: f.func,
                block: f.block,
                ip: f.ip,
                regs: f.regs.clone(),
                captures: f.captures.clone(),
                ret_reg: f.ret_reg,
            });
        }

        let mut regs = k.regs.clone();
        let pd = k.perform_dst as usize;
        if pd >= regs.len() {
            regs.resize(pd + 1, Val::Unit);
        }
        regs[pd] = v;
        self.func = k.func;
        self.block = k.block;
        self.ip = k.ip;
        self.regs = regs;
        self.captures = k.captures.clone();
        Ok(())
    }

    /// The bottom of the handler stack: effects nothing in Loon caught.
    ///
    /// On a hosted runtime these reach the OS. Here there is no OS below to
    /// reach, so the set is exactly what the machine can do — and anything
    /// outside it is a hard error, never a silent `()`.
    fn hardware(&mut self, eff: StringId, o: StringId, args: &[Val]) -> VmResult<Val> {
        let effect = self.m.string(eff);
        let op = self.m.string(o);
        match (effect, op) {
            ("Console", "write") | ("IO", "print") => {
                let s = args.first().map(show).unwrap_or_default();
                self.host.write(&s);
                Ok(Val::Unit)
            }
            ("Console", "line") | ("IO", "println") => {
                let s = args.first().map(show).unwrap_or_default();
                self.host.write(&s);
                self.host.write("\n");
                Ok(Val::Unit)
            }
            ("Clock", "now") | ("Clock", "ticks") => Ok(Val::Int(self.host.ticks())),
            ("Fail", "fail") => Err(alloc::format!(
                "unhandled failure: {}",
                args.first().map(show).unwrap_or_default()
            )),
            _ => Err(alloc::format!(
                "unhandled effect {effect}.{op} — this machine has no handler for it"
            )),
        }
    }
}

// ── Operators and intrinsics ───────────────────────────────────────────

impl<'m, H: Host> Vm<'m, H> {
    fn binop(&mut self, o: BinOp, a: Val, b: Val) -> VmResult<Val> {
        use BinOp::*;
        // Comparison and logic first: they accept anything.
        match o {
            Eq => return Ok(Val::Bool(a == b)),
            Ne => return Ok(Val::Bool(a != b)),
            And => return Ok(if a.truthy() { b } else { a }),
            Or => return Ok(if a.truthy() { a } else { b }),
            Concat => return self.concat(a, b),
            _ => {}
        }

        // String `+` concatenates, matching the host.
        if let (Add, Val::Str(x), Val::Str(y)) = (o, &a, &b) {
            let mut s = String::with_capacity(x.len() + y.len());
            s.push_str(x);
            s.push_str(y);
            return Ok(Val::Str(Rc::new(s)));
        }

        let num = |v: &Val| -> Option<f64> {
            match v {
                Val::Int(n) => Some(*n as f64),
                Val::Float(f) => Some(*f),
                _ => None,
            }
        };
        let (Some(x), Some(y)) = (num(&a), num(&b)) else {
            return Err(alloc::format!(
                "cannot apply {o:?} to a {} and a {}",
                a.type_name(),
                b.type_name()
            ));
        };

        // Integer arithmetic stays integral; a mixed operand promotes.
        let ints = matches!((&a, &b), (Val::Int(_), Val::Int(_)));
        Ok(match o {
            Lt => Val::Bool(x < y),
            Gt => Val::Bool(x > y),
            Le => Val::Bool(x <= y),
            Ge => Val::Bool(x >= y),
            Div if y == 0.0 => return Err("division by zero".to_string()),
            Rem if y == 0.0 => return Err("remainder by zero".to_string()),
            _ if ints => {
                let (i, j) = (x as i64, y as i64);
                Val::Int(match o {
                    Add => i.wrapping_add(j),
                    Sub => i.wrapping_sub(j),
                    Mul => i.wrapping_mul(j),
                    Div => i / j,
                    Rem => i % j,
                    _ => unreachable!(),
                })
            }
            Add => Val::Float(x + y),
            Sub => Val::Float(x - y),
            Mul => Val::Float(x * y),
            Div => Val::Float(x / y),
            Rem => Val::Float(x % y),
            _ => unreachable!(),
        })
    }

    fn concat(&mut self, a: Val, b: Val) -> VmResult<Val> {
        Ok(match (&a, &b) {
            (Val::Vec(x), Val::Vec(y)) => {
                let mut v = x.as_ref().clone();
                v.extend(y.iter().cloned());
                Val::Vec(Rc::new(v))
            }
            _ => {
                let mut s = show(&a);
                s.push_str(&show(&b));
                Val::Str(Rc::new(s))
            }
        })
    }

    fn builtin(&mut self, tag: u16, args: &[Val]) -> VmResult<Val> {
        let name = self
            .m
            .builtin_name(tag)
            .ok_or_else(|| alloc::format!("boot image references unknown builtin tag {tag}"))?;
        let a0 = || args.first().cloned().unwrap_or(Val::Unit);
        let a1 = || args.get(1).cloned().unwrap_or(Val::Unit);

        let seq = |v: &Val| -> Option<Rc<Vec<Val>>> {
            match v {
                Val::Vec(xs) | Val::Tup(xs) | Val::Set(xs) => Some(xs.clone()),
                _ => None,
            }
        };

        Ok(match name {
            "Println" => {
                let mut out = String::new();
                for (i, a) in args.iter().enumerate() {
                    if i > 0 {
                        out.push(' ');
                    }
                    out.push_str(&show(a));
                }
                out.push('\n');
                self.host.write(&out);
                Val::Unit
            }
            "Print" => {
                let mut out = String::new();
                for (i, a) in args.iter().enumerate() {
                    if i > 0 {
                        out.push(' ');
                    }
                    out.push_str(&show(a));
                }
                self.host.write(&out);
                Val::Unit
            }
            "Str" => {
                let mut out = String::new();
                for a in args {
                    out.push_str(&show(a));
                }
                Val::Str(Rc::new(out))
            }
            "Len" => Val::Int(match a0() {
                Val::Str(s) => s.chars().count() as i64,
                Val::Map(kvs) => kvs.len() as i64,
                v => seq(&v).map(|x| x.len()).unwrap_or(0) as i64,
            }),
            "Empty" => Val::Bool(match a0() {
                Val::Str(s) => s.is_empty(),
                Val::Map(kvs) => kvs.is_empty(),
                Val::Unit => true,
                v => seq(&v).map(|x| x.is_empty()).unwrap_or(false),
            }),
            "Not" => Val::Bool(!a0().truthy()),
            "TypeOf" => Val::Str(Rc::new(a0().type_name().to_string())),
            "SomeP" => Val::Bool(!matches!(a0(), Val::Unit)),
            "NoneP" => Val::Bool(matches!(a0(), Val::Unit)),
            "VecP" => Val::Bool(matches!(a0(), Val::Vec(_))),
            "MapP" => Val::Bool(matches!(a0(), Val::Map(_))),
            "Range" => {
                let (lo, hi) = match (a0(), a1()) {
                    (Val::Int(a), Val::Int(b)) => (a, b),
                    (Val::Int(n), Val::Unit) => (0, n),
                    _ => return Err("range expects integers".to_string()),
                };
                Val::Vec(Rc::new((lo..hi).map(Val::Int).collect()))
            }
            "Nth" | "Get" => {
                let base = a0();
                match (&base, a1()) {
                    (Val::Map(kvs), key) => kvs
                        .iter()
                        .find(|(k, _)| *k == key)
                        .map(|(_, v)| v.clone())
                        .unwrap_or(Val::Unit),
                    (_, Val::Int(i)) => seq(&base)
                        .and_then(|xs| xs.get(i as usize).cloned())
                        .unwrap_or(Val::Unit),
                    _ => Val::Unit,
                }
            }
            "First" => seq(&a0())
                .and_then(|xs| xs.first().cloned())
                .unwrap_or(Val::Unit),
            "Last" => seq(&a0())
                .and_then(|xs| xs.last().cloned())
                .unwrap_or(Val::Unit),
            "Reverse" => {
                let mut xs = seq(&a0()).map(|x| x.as_ref().clone()).unwrap_or_default();
                xs.reverse();
                Val::Vec(Rc::new(xs))
            }
            "Conj" => {
                let mut xs = seq(&a0()).map(|x| x.as_ref().clone()).unwrap_or_default();
                xs.extend(args.iter().skip(1).cloned());
                Val::Vec(Rc::new(xs))
            }
            "Cons" => {
                let mut xs = vec![a0()];
                xs.extend(seq(&a1()).map(|x| x.as_ref().clone()).unwrap_or_default());
                Val::Vec(Rc::new(xs))
            }
            "Sum" => {
                let xs = seq(&a0()).ok_or_else(|| "sum expects a sequence".to_string())?;
                let mut acc = Val::Int(0);
                for x in xs.iter() {
                    acc = self.binop(BinOp::Add, acc, x.clone())?;
                }
                acc
            }
            "Concat" => {
                let mut acc = a0();
                for b in args.iter().skip(1) {
                    acc = self.concat(acc, b.clone())?;
                }
                acc
            }
            "Join" => {
                let xs = seq(&a0()).unwrap_or_default();
                let sep = match a1() {
                    Val::Str(s) => s.as_ref().clone(),
                    Val::Unit => String::new(),
                    v => show(&v),
                };
                let mut out = String::new();
                for (i, x) in xs.iter().enumerate() {
                    if i > 0 {
                        out.push_str(&sep);
                    }
                    out.push_str(&show(x));
                }
                Val::Str(Rc::new(out))
            }
            // Higher-order intrinsics re-enter the interpreter.
            "Map" => {
                let xs = seq(&a0()).ok_or_else(|| "map expects a sequence".to_string())?;
                let f = a1();
                let mut out = Vec::with_capacity(xs.len());
                for x in xs.iter() {
                    out.push(self.apply(&f, core::slice::from_ref(x))?);
                }
                Val::Vec(Rc::new(out))
            }
            "Filter" => {
                let xs = seq(&a0()).ok_or_else(|| "filter expects a sequence".to_string())?;
                let f = a1();
                let mut out = Vec::new();
                for x in xs.iter() {
                    if self.apply(&f, core::slice::from_ref(x))?.truthy() {
                        out.push(x.clone());
                    }
                }
                Val::Vec(Rc::new(out))
            }
            "Each" => {
                let xs = seq(&a0()).ok_or_else(|| "each expects a sequence".to_string())?;
                let f = a1();
                for x in xs.iter() {
                    self.apply(&f, core::slice::from_ref(x))?;
                }
                Val::Unit
            }
            "Fold" => {
                let xs = seq(&a0()).ok_or_else(|| "fold expects a sequence".to_string())?;
                let mut acc = a1();
                let f = args.get(2).cloned().unwrap_or(Val::Unit);
                for x in xs.iter() {
                    acc = self.apply(&f, &[acc, x.clone()])?;
                }
                acc
            }
            "AssertEq" => {
                let (a, b) = (a0(), a1());
                if a != b {
                    return Err(alloc::format!(
                        "assertion failed: {} != {}",
                        show(&a),
                        show(&b)
                    ));
                }
                Val::Unit
            }
            "MatchFail" => return Err(alloc::format!("no match arm matched {}", show(&a0()))),
            "UnboundSym" => return Err(alloc::format!("unbound symbol '{}'", show(&a0()))),
            // Everything else exists on the host but has not been ported.
            // Saying so is the point: a silently wrong answer on hardware is
            // far worse than a refusal.
            other => {
                return Err(alloc::format!(
                    "builtin '{other}' is not implemented in the unikernel runtime"
                ))
            }
        })
    }
}
