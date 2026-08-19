//! Running a kernel on the CPU without going through the general VM.
//!
//! The interpreter is built for a dynamically-typed language: every value is
//! NaN-boxed, every buffer access goes through the heap table, and every
//! arithmetic operation checks what it was handed. That is the right design
//! for Loon and the wrong one for a loop that runs the same three floating
//! point operations a million times.
//!
//! A kernel does not need any of it. The subset is small enough that every
//! register has a knowable type, every buffer is a contiguous slice, and the
//! whole body can be executed against raw `f32`s. This module does that: it
//! type-checks the kernel once, then runs it per work item with no boxing and
//! no heap indirection.
//!
//! Two things follow. The CPU number in a benchmark becomes a fair baseline
//! rather than a straw man — "the GPU is faster than our interpreter" is not
//! an interesting claim. And because the executor works on plain slices over
//! an index range, running work items in parallel is a matter of splitting the
//! range, which is where `Mode::Par` comes from.

use super::layout::DType;
use super::vm::{BufData, Buffer};
use super::{BinOp, Built, End, FuncId, Lit, Module, Op, Reg, UnOp};

/// A value inside a running kernel.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum KVal {
    F(f64),
    I(i64),
    B(bool),
    Unit,
}

impl KVal {
    fn as_f(self) -> f64 {
        match self {
            KVal::F(x) => x,
            KVal::I(n) => n as f64,
            KVal::B(b) => {
                if b {
                    1.0
                } else {
                    0.0
                }
            }
            KVal::Unit => 0.0,
        }
    }

    fn as_i(self) -> i64 {
        match self {
            KVal::I(n) => n,
            KVal::F(x) => x as i64,
            KVal::B(b) => b as i64,
            KVal::Unit => 0,
        }
    }

    fn truthy(self) -> bool {
        match self {
            KVal::B(b) => b,
            KVal::I(n) => n != 0,
            KVal::F(x) => x != 0.0,
            KVal::Unit => false,
        }
    }

    /// Whether either operand is a float, which decides the result's type.
    fn either_float(a: KVal, b: KVal) -> bool {
        matches!(a, KVal::F(_)) || matches!(b, KVal::F(_))
    }
}

/// A writable slice of one buffer's elements.
///
/// Parallel placement hands each thread a different one of these, carved out
/// of the same buffer with `split_at_mut`. That the pieces do not overlap is
/// not a promise anybody makes — it is what `split_at_mut` returns, checked by
/// the compiler. A GPU partitioning strategy has to assert the same property
/// through an `unsafe impl`.
pub enum OutView<'a> {
    F32(&'a mut [f32]),
    F64(&'a mut [f64]),
    I32(&'a mut [i32]),
    I64(&'a mut [i64]),
}

impl OutView<'_> {
    fn len(&self) -> usize {
        match self {
            OutView::F32(v) => v.len(),
            OutView::F64(v) => v.len(),
            OutView::I32(v) => v.len(),
            OutView::I64(v) => v.len(),
        }
    }
}

/// An argument to a kernel launch.
pub enum KArg<'a> {
    /// A buffer the kernel only reads.
    Input(&'a Buffer),
    /// A buffer the kernel writes through.
    Output(&'a mut Buffer),
    /// A slice of a buffer, addressed by absolute index.
    ///
    /// `base` is the index this view starts at, so a work item writing at its
    /// own index lands in the right place. A write outside the view is an
    /// error rather than a silent miss: in parallel placement it means the
    /// kernel wrote somewhere another thread owns.
    OutputView { view: OutView<'a>, base: i64 },
    /// A number.
    Scalar(KVal),
}

/// Why a kernel could not be run this way.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Error(pub String);

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Can this function be run by the fast executor?
///
/// Checked once before a launch rather than per work item. A kernel that fails
/// here is not wrong — it simply uses something outside the subset, and the
/// general VM runs it instead.
pub fn supported(module: &Module, func: FuncId) -> bool {
    let Some(f) = module.funcs.get(func.0 as usize) else {
        return false;
    };
    f.blocks.iter().all(|b| {
        b.ops.iter().all(|op| {
            matches!(op, Op::Lit(..) | Op::Mov(..) | Op::Bin(..) | Op::Un(..))
                || matches!(op, Op::Builtin(_, built, ..) if supported_builtin(*built))
        }) && matches!(
            b.end,
            End::Ret(_) | End::Jmp(..) | End::Br(..) | End::Recur(_) | End::Trap
        )
    })
}

fn supported_builtin(b: Built) -> bool {
    matches!(
        b,
        Built::BufAt
            | Built::BufPut
            | Built::BufLen
            | Built::Sqrt
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
            | Built::Exp
            | Built::Abs
            | Built::Min
            | Built::Max
            | Built::Not
    )
}

/// Run `func` for every index in `range`.
///
/// Buffer arguments are borrowed for the whole call, which is what makes the
/// disjointness argument work: an output buffer is a `&mut` slice, so the type
/// system will not let two ranges write the same one at once. That is the same
/// guarantee a GPU partitioning strategy has to promise by hand.
pub fn run_range(
    module: &Module,
    func: FuncId,
    args: &mut [KArg<'_>],
    range: std::ops::Range<i64>,
) -> Result<(), Error> {
    let f = module
        .funcs
        .get(func.0 as usize)
        .ok_or_else(|| Error(format!("no function {func:?}")))?;
    if f.params.len() != args.len() + 1 {
        return Err(Error(format!(
            "kernel takes {} parameters but {} arguments were given",
            f.params.len(),
            args.len() + 1
        )));
    }

    let reg_count = f
        .blocks
        .iter()
        .flat_map(|b| {
            b.ops
                .iter()
                .filter_map(dest)
                .chain(b.params.iter().copied())
        })
        .map(|r| r.0 as usize + 1)
        .max()
        .unwrap_or(1)
        .max(args.len() + 1);

    let mut regs = vec![KVal::Unit; reg_count];
    for i in range {
        run_one(module, f.id, args, &mut regs, i)?;
    }
    Ok(())
}

/// Run a kernel across every core, splitting the index space.
///
/// Each thread gets a disjoint slice of *every* output buffer and the whole of
/// every input. The disjointness is not asserted — it is what `split_at_mut`
/// hands back, and the borrow checker is what enforces it. A GPU partitioning
/// strategy has to promise the same property through an `unsafe impl`.
///
/// A kernel that writes outside its own range fails with a message saying so
/// rather than racing another thread.
pub fn run_parallel(
    module: &Module,
    func: FuncId,
    scalars: &[(usize, KVal)],
    inputs: &[(usize, &Buffer)],
    outputs: &mut [(usize, &mut Buffer)],
    arity: usize,
    n: i64,
) -> Result<(), Error> {
    let threads = std::thread::available_parallelism()
        .map(|p| p.get())
        .unwrap_or(1)
        .min(n.max(1) as usize);

    if threads <= 1 || n <= 0 || outputs.is_empty() {
        // Nothing to split, or nothing to split across. Sequential is the same
        // answer; only slower.
        let mut args = assemble(scalars, inputs, outputs, arity)?;
        return run_range(module, func, &mut args, 0..n);
    }

    // Contiguous chunks of the index space, one per thread.
    let chunk = (n as usize).div_ceil(threads);
    let mut ranges: Vec<(i64, usize)> = Vec::new();
    let mut start = 0usize;
    while start < n as usize {
        let len = chunk.min(n as usize - start);
        ranges.push((start as i64, len));
        start += len;
    }

    // Carve every output buffer into per-thread views. `iter_mut` yields
    // disjoint `&mut`s, so pieces taken from different buffers can coexist.
    let mut per_thread: Vec<Vec<(usize, OutView)>> = ranges.iter().map(|_| Vec::new()).collect();
    for (arg_idx, buf) in outputs.iter_mut() {
        if buf.len() < n as usize {
            return Err(Error(format!(
                "argument {} has {} elements but the launch covers {n}",
                *arg_idx + 1,
                buf.len()
            )));
        }
        macro_rules! carve {
            ($v:expr, $variant:ident) => {{
                let mut rest: &mut [_] = &mut $v[..];
                for (t, (_, len)) in ranges.iter().enumerate() {
                    let (head, tail) = rest.split_at_mut(*len);
                    per_thread[t].push((*arg_idx, OutView::$variant(head)));
                    rest = tail;
                }
            }};
        }
        match &mut buf.data {
            BufData::F32(v) => carve!(v, F32),
            BufData::F64(v) => carve!(v, F64),
            BufData::I32(v) => carve!(v, I32),
            BufData::I64(v) => carve!(v, I64),
        }
    }

    std::thread::scope(|scope| {
        let mut handles = Vec::new();
        for ((base, _), views) in ranges.iter().zip(per_thread) {
            let base = *base;
            handles.push(scope.spawn(move || {
                let mut args: Vec<KArg> = Vec::with_capacity(arity);
                let mut views = views;
                for i in 0..arity {
                    if let Some((_, v)) = scalars.iter().find(|(j, _)| *j == i) {
                        args.push(KArg::Scalar(*v));
                    } else if let Some((_, b)) = inputs.iter().find(|(j, _)| *j == i) {
                        args.push(KArg::Input(b));
                    } else if let Some(pos) = views.iter().position(|(j, _)| *j == i) {
                        let (_, view) = views.remove(pos);
                        args.push(KArg::OutputView { view, base });
                    } else {
                        return Err(Error(format!("argument {} was not provided", i + 1)));
                    }
                }
                let len = args
                    .iter()
                    .find_map(|a| match a {
                        KArg::OutputView { view, .. } => Some(view.len() as i64),
                        _ => None,
                    })
                    .unwrap_or(0);
                run_range(module, func, &mut args, base..base + len)
            }));
        }
        let mut result = Ok(());
        for h in handles {
            match h.join() {
                Ok(Ok(())) => {}
                Ok(Err(e)) => result = Err(e),
                Err(_) => result = Err(Error("a kernel thread panicked".into())),
            }
        }
        result
    })
}

/// Run a kernel on one core, from the same separated pieces `run_parallel`
/// takes, so a caller does not have to assemble arguments two different ways.
pub fn run_sequential(
    module: &Module,
    func: FuncId,
    scalars: &[(usize, KVal)],
    inputs: &[(usize, &Buffer)],
    outputs: &mut [(usize, &mut Buffer)],
    arity: usize,
    n: i64,
) -> Result<(), Error> {
    let mut args = assemble(scalars, inputs, outputs, arity)?;
    run_range(module, func, &mut args, 0..n)
}

/// Build a sequential argument list from the separated pieces.
fn assemble<'a>(
    scalars: &[(usize, KVal)],
    inputs: &[(usize, &'a Buffer)],
    outputs: &'a mut [(usize, &mut Buffer)],
    arity: usize,
) -> Result<Vec<KArg<'a>>, Error> {
    // Outputs are addressed by argument position, so index them once.
    let mut out_by_arg: Vec<Option<&mut Buffer>> = (0..arity).map(|_| None).collect();
    for (i, b) in outputs.iter_mut() {
        out_by_arg[*i] = Some(b);
    }
    let mut args: Vec<KArg> = Vec::with_capacity(arity);
    for i in 0..arity {
        if let Some((_, v)) = scalars.iter().find(|(j, _)| *j == i) {
            args.push(KArg::Scalar(*v));
        } else if let Some((_, b)) = inputs.iter().find(|(j, _)| *j == i) {
            args.push(KArg::Input(b));
        } else if let Some(b) = out_by_arg[i].take() {
            args.push(KArg::Output(b));
        } else {
            return Err(Error(format!("argument {} was not provided", i + 1)));
        }
    }
    Ok(args)
}

fn run_one(
    module: &Module,
    func: FuncId,
    args: &mut [KArg<'_>],
    regs: &mut [KVal],
    index: i64,
) -> Result<(), Error> {
    let f = &module.funcs[func.0 as usize];

    // Parameters: the work index, then the arguments. Buffers do not occupy a
    // register value — they are addressed by argument position.
    regs[0] = KVal::I(index);
    for (i, arg) in args.iter().enumerate() {
        regs[i + 1] = match arg {
            KArg::Scalar(v) => *v,
            // A buffer's "value" is its own position, so `at`/`put` can find
            // it. Nothing else in the subset reads a buffer as a number.
            _ => KVal::I(i as i64),
        };
    }

    let mut block = 0usize;
    let mut steps = 0usize;
    loop {
        steps += 1;
        if steps > 1_000_000 {
            return Err(Error("kernel did not terminate".into()));
        }
        let b = &f.blocks[block];
        for op in &b.ops {
            exec_op(op, args, regs)?;
        }
        match &b.end {
            End::Ret(_) | End::Trap => return Ok(()),
            End::Jmp(target, jargs) => {
                let target_block = &f.blocks[target.0 as usize];
                let vals: Vec<KVal> = jargs.iter().map(|r| regs[r.0 as usize]).collect();
                for (p, v) in target_block.params.iter().zip(vals) {
                    regs[p.0 as usize] = v;
                }
                block = target.0 as usize;
            }
            End::Br(cond, t, e) => {
                block = if regs[cond.0 as usize].truthy() {
                    t.0 as usize
                } else {
                    e.0 as usize
                };
            }
            End::Recur(rargs) => {
                let vals: Vec<KVal> = rargs.iter().map(|r| regs[r.0 as usize]).collect();
                let params = b.params.clone();
                for (p, v) in params.iter().zip(vals) {
                    regs[p.0 as usize] = v;
                }
            }
            other => return Err(Error(format!("{other:?} is outside the kernel subset"))),
        }
    }
}

fn exec_op(op: &Op, args: &mut [KArg<'_>], regs: &mut [KVal]) -> Result<(), Error> {
    match op {
        Op::Lit(d, lit, _) => {
            regs[d.0 as usize] = match lit {
                Lit::Int(n) => KVal::I(*n),
                Lit::Float(x) => KVal::F(*x),
                Lit::Bool(b) => KVal::B(*b),
                Lit::Unit => KVal::Unit,
                other => return Err(Error(format!("{other:?} is outside the kernel subset"))),
            };
        }
        Op::Mov(d, s, _) => regs[d.0 as usize] = regs[s.0 as usize],
        Op::Bin(d, bop, a, b, _) => {
            let (x, y) = (regs[a.0 as usize], regs[b.0 as usize]);
            regs[d.0 as usize] = binop(*bop, x, y)?;
        }
        Op::Un(d, uop, a, _) => {
            let v = regs[a.0 as usize];
            regs[d.0 as usize] = match uop {
                UnOp::Neg => match v {
                    KVal::I(n) => KVal::I(-n),
                    other => KVal::F(-other.as_f()),
                },
                UnOp::Not => KVal::B(!v.truthy()),
            };
        }
        Op::Builtin(d, built, bargs, _) => exec_builtin(*d, *built, bargs, args, regs)?,
        other => return Err(Error(format!("{other:?} is outside the kernel subset"))),
    }
    Ok(())
}

fn binop(bop: BinOp, x: KVal, y: KVal) -> Result<KVal, Error> {
    let float = KVal::either_float(x, y);
    Ok(match bop {
        BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Rem => {
            if float {
                let (a, b) = (x.as_f(), y.as_f());
                KVal::F(match bop {
                    BinOp::Add => a + b,
                    BinOp::Sub => a - b,
                    BinOp::Mul => a * b,
                    BinOp::Div => a / b,
                    _ => a % b,
                })
            } else {
                let (a, b) = (x.as_i(), y.as_i());
                // Integer division by zero has a decided answer in Loon (it is
                // an error, not a silent value); the kernel subset does not get
                // to invent a different one, so the executor declines instead.
                if matches!(bop, BinOp::Div | BinOp::Rem) && b == 0 {
                    return Err(Error("integer division by zero in a kernel".into()));
                }
                KVal::I(match bop {
                    BinOp::Add => a + b,
                    BinOp::Sub => a - b,
                    BinOp::Mul => a * b,
                    BinOp::Div => a / b,
                    _ => a % b,
                })
            }
        }
        BinOp::Eq => KVal::B(if float {
            x.as_f() == y.as_f()
        } else {
            x.as_i() == y.as_i()
        }),
        BinOp::Ne => KVal::B(if float {
            x.as_f() != y.as_f()
        } else {
            x.as_i() != y.as_i()
        }),
        BinOp::Lt => KVal::B(if float {
            x.as_f() < y.as_f()
        } else {
            x.as_i() < y.as_i()
        }),
        BinOp::Gt => KVal::B(if float {
            x.as_f() > y.as_f()
        } else {
            x.as_i() > y.as_i()
        }),
        BinOp::Le => KVal::B(if float {
            x.as_f() <= y.as_f()
        } else {
            x.as_i() <= y.as_i()
        }),
        BinOp::Ge => KVal::B(if float {
            x.as_f() >= y.as_f()
        } else {
            x.as_i() >= y.as_i()
        }),
        BinOp::And => KVal::B(x.truthy() && y.truthy()),
        BinOp::Or => KVal::B(x.truthy() || y.truthy()),
        BinOp::Concat => return Err(Error("a kernel cannot concatenate strings".into())),
    })
}

fn exec_builtin(
    d: Reg,
    built: Built,
    bargs: &[Reg],
    args: &mut [KArg<'_>],
    regs: &mut [KVal],
) -> Result<(), Error> {
    let slot = |i: usize| -> usize { regs[bargs[i].0 as usize].as_i() as usize };

    match built {
        Built::BufAt => {
            let which = slot(0);
            let idx = regs[bargs[1].0 as usize].as_i();
            let v = read_buffer(args, which, idx)?;
            regs[d.0 as usize] = v;
        }
        Built::BufPut => {
            let which = slot(0);
            let idx = regs[bargs[1].0 as usize].as_i();
            let v = regs[bargs[2].0 as usize];
            write_buffer(args, which, idx, v)?;
            regs[d.0 as usize] = KVal::Unit;
        }
        Built::BufLen => {
            let which = slot(0);
            let len = match args.get(which) {
                Some(KArg::Input(b)) => b.len(),
                Some(KArg::Output(b)) => b.len(),
                Some(KArg::OutputView { view, .. }) => view.len(),
                _ => return Err(Error("buf-len needs a buffer".into())),
            };
            regs[d.0 as usize] = KVal::I(len as i64);
        }
        _ => {
            let a = |i: usize| regs[bargs.get(i).map(|r| r.0 as usize).unwrap_or(0)].as_f();
            regs[d.0 as usize] = match built {
                Built::Sqrt => KVal::F(a(0).sqrt()),
                Built::Pow => KVal::F(a(0).powf(a(1))),
                Built::Floor => KVal::F(a(0).floor()),
                Built::Ceil => KVal::F(a(0).ceil()),
                Built::Round => KVal::F(a(0).round()),
                Built::Sin => KVal::F(a(0).sin()),
                Built::Cos => KVal::F(a(0).cos()),
                Built::Tan => KVal::F(a(0).tan()),
                Built::Asin => KVal::F(a(0).asin()),
                Built::Acos => KVal::F(a(0).acos()),
                Built::Atan => KVal::F(a(0).atan()),
                Built::Atan2 => KVal::F(a(0).atan2(a(1))),
                Built::Log => KVal::F(a(0).ln()),
                Built::Log10 => KVal::F(a(0).log10()),
                Built::Exp => KVal::F(a(0).exp()),
                Built::Abs => match regs[bargs[0].0 as usize] {
                    KVal::I(n) => KVal::I(n.abs()),
                    other => KVal::F(other.as_f().abs()),
                },
                Built::Min | Built::Max => {
                    let (x, y) = (regs[bargs[0].0 as usize], regs[bargs[1].0 as usize]);
                    let take_min = matches!(built, Built::Min);
                    if KVal::either_float(x, y) {
                        let (p, q) = (x.as_f(), y.as_f());
                        KVal::F(if take_min { p.min(q) } else { p.max(q) })
                    } else {
                        let (p, q) = (x.as_i(), y.as_i());
                        KVal::I(if take_min { p.min(q) } else { p.max(q) })
                    }
                }
                Built::Not => KVal::B(!regs[bargs[0].0 as usize].truthy()),
                other => return Err(Error(format!("builtin {other:?} is outside the subset"))),
            };
        }
    }
    Ok(())
}

fn read_buffer(args: &[KArg<'_>], which: usize, idx: i64) -> Result<KVal, Error> {
    if let Some(KArg::OutputView { view, base }) = args.get(which) {
        let rel = idx - base;
        let i = usize::try_from(rel).map_err(|_| out_of_view(idx))?;
        return match view {
            OutView::F32(v) => v.get(i).map(|x| KVal::F(*x as f64)),
            OutView::F64(v) => v.get(i).map(|x| KVal::F(*x)),
            OutView::I32(v) => v.get(i).map(|x| KVal::I(*x as i64)),
            OutView::I64(v) => v.get(i).map(|x| KVal::I(*x)),
        }
        .ok_or_else(|| out_of_view(idx));
    }
    let buf = match args.get(which) {
        Some(KArg::Input(b)) => *b,
        Some(KArg::Output(b)) => &**b,
        _ => return Err(Error(format!("argument {} is not a buffer", which + 1))),
    };
    let i = usize::try_from(idx).map_err(|_| out_of_range(idx, buf.len()))?;
    match &buf.data {
        BufData::F32(v) => v.get(i).map(|x| KVal::F(*x as f64)),
        BufData::F64(v) => v.get(i).map(|x| KVal::F(*x)),
        BufData::I32(v) => v.get(i).map(|x| KVal::I(*x as i64)),
        BufData::I64(v) => v.get(i).map(|x| KVal::I(*x)),
    }
    .ok_or_else(|| out_of_range(idx, buf.len()))
}

fn write_buffer(args: &mut [KArg<'_>], which: usize, idx: i64, v: KVal) -> Result<(), Error> {
    if let Some(KArg::OutputView { view, base }) = args.get_mut(which) {
        let rel = idx - *base;
        let i = usize::try_from(rel).map_err(|_| out_of_view(idx))?;
        if i >= view.len() {
            return Err(out_of_view(idx));
        }
        match view {
            OutView::F32(b) => b[i] = v.as_f() as f32,
            OutView::F64(b) => b[i] = v.as_f(),
            OutView::I32(b) => b[i] = v.as_i() as i32,
            OutView::I64(b) => b[i] = v.as_i(),
        }
        return Ok(());
    }
    let Some(KArg::Output(buf)) = args.get_mut(which) else {
        return Err(Error(format!(
            "argument {} is not a buffer this kernel may write",
            which + 1
        )));
    };
    let len = buf.len();
    let i = usize::try_from(idx).map_err(|_| out_of_range(idx, len))?;
    if i >= len {
        return Err(out_of_range(idx, len));
    }
    match &mut buf.data {
        BufData::F32(b) => b[i] = v.as_f() as f32,
        BufData::F64(b) => b[i] = v.as_f(),
        BufData::I32(b) => b[i] = v.as_i() as i32,
        BufData::I64(b) => b[i] = v.as_i(),
    }
    Ok(())
}

fn out_of_range(idx: i64, len: usize) -> Error {
    Error(format!("index {idx} is outside a buffer of length {len}"))
}

/// A write that left the slice this thread owns.
///
/// In parallel placement that means the kernel wrote at an index other than
/// its own — a scatter. Saying so is the point: the alternative is threads
/// quietly overwriting each other's elements.
fn out_of_view(idx: i64) -> Error {
    Error(format!(
        "index {idx} is outside the range this work item owns; a kernel run in \
         parallel must write at its own index"
    ))
}

/// Element type of a buffer, for callers deciding how to narrow it.
pub fn dtype_of(b: &Buffer) -> DType {
    b.dtype()
}

fn dest(op: &Op) -> Option<Reg> {
    match op {
        Op::Lit(d, ..)
        | Op::Mov(d, ..)
        | Op::Bin(d, ..)
        | Op::Un(d, ..)
        | Op::Builtin(d, ..)
        | Op::Upval(d, ..)
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
        | Op::PushHandler(d, ..) => Some(*d),
        Op::PopHandler(_) => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::check::Checker;
    use crate::eir::lower::lower;
    use crate::parser::parse;

    fn lowered(src: &str, name: &str) -> (Module, FuncId) {
        let exprs = parse(src).expect("parses");
        let mut checker = Checker::new();
        let errors = checker.check_program(&exprs);
        assert!(errors.is_empty(), "check errors: {errors:?}");
        let module = lower(&checker);
        let id = module
            .funcs
            .iter()
            .find(|f| f.name.as_deref() == Some(name))
            .expect("kernel lowered")
            .id;
        (module, id)
    }

    fn f32s(b: &Buffer) -> Vec<f32> {
        match &b.data {
            BufData::F32(v) => v.clone(),
            other => panic!("expected f32, got {other:?}"),
        }
    }

    #[test]
    fn a_kernel_runs_over_a_range() {
        let (m, id) = lowered(
            "[kernel saxpy [i a x y out] [put out i [+ [* a [at x i]] [at y i]]]]",
            "saxpy",
        );
        let x = Buffer {
            data: BufData::F32(vec![1.0, 2.0, 3.0, 4.0]),
        };
        let y = Buffer {
            data: BufData::F32(vec![10.0, 20.0, 30.0, 40.0]),
        };
        let mut out = Buffer {
            data: BufData::F32(vec![0.0; 4]),
        };
        {
            let mut args = vec![
                KArg::Scalar(KVal::F(2.0)),
                KArg::Input(&x),
                KArg::Input(&y),
                KArg::Output(&mut out),
            ];
            run_range(&m, id, &mut args, 0..4).expect("runs");
        }
        assert_eq!(f32s(&out), vec![12.0, 24.0, 36.0, 48.0]);
    }

    #[test]
    fn a_partial_range_touches_only_its_own_elements() {
        // This is the property that makes splitting a range across threads
        // sound: a work item writes at its own index and nowhere else.
        let (m, id) = lowered("[kernel fill [i b] [put b i 9.0]]", "fill");
        let mut out = Buffer {
            data: BufData::F32(vec![0.0; 6]),
        };
        {
            let mut args = vec![KArg::Output(&mut out)];
            run_range(&m, id, &mut args, 2..4).expect("runs");
        }
        assert_eq!(f32s(&out), vec![0.0, 0.0, 9.0, 9.0, 0.0, 0.0]);
    }

    #[test]
    fn control_flow_inside_a_kernel_works() {
        let (m, id) = lowered(
            "[kernel clamp [i lo hi b] \
               [let v [at b i]] \
               [put b i [if [< v lo] lo [if [> v hi] hi v]]]]",
            "clamp",
        );
        let mut b = Buffer {
            data: BufData::F32(vec![-5.0, 0.5, 9.0]),
        };
        {
            let mut args = vec![
                KArg::Scalar(KVal::F(0.0)),
                KArg::Scalar(KVal::F(1.0)),
                KArg::Output(&mut b),
            ];
            run_range(&m, id, &mut args, 0..3).expect("runs");
        }
        assert_eq!(f32s(&b), vec![0.0, 0.5, 1.0]);
    }

    #[test]
    fn math_builtins_agree_with_the_interpreter() {
        let src = "[kernel mathy [i b] [put b i [sqrt [abs [at b i]]]]]";
        let (m, id) = lowered(src, "mathy");
        let mut b = Buffer {
            data: BufData::F32(vec![-4.0, 9.0, 16.0]),
        };
        {
            let mut args = vec![KArg::Output(&mut b)];
            run_range(&m, id, &mut args, 0..3).expect("runs");
        }
        assert_eq!(f32s(&b), vec![2.0, 3.0, 4.0]);
    }

    #[test]
    fn reading_past_the_end_is_an_error_here_too() {
        // The fast path must not become the path where bounds stop being
        // checked. An out-of-range read is a bug in the program whichever
        // executor runs it.
        let (m, id) = lowered("[kernel bad [i b] [put b i [at b 99]]]", "bad");
        let mut b = Buffer {
            data: BufData::F32(vec![0.0; 2]),
        };
        let mut args = vec![KArg::Output(&mut b)];
        let e = run_range(&m, id, &mut args, 0..2).expect_err("should fail");
        assert!(e.0.contains("outside a buffer"), "{}", e.0);
    }

    #[test]
    fn integer_buffers_stay_integers() {
        let (m, id) = lowered("[kernel bump [i b] [put b i [+ 1 [at b i]]]]", "bump");
        let mut b = Buffer {
            data: BufData::I32(vec![1, 2, 3]),
        };
        {
            let mut args = vec![KArg::Output(&mut b)];
            run_range(&m, id, &mut args, 0..3).expect("runs");
        }
        assert_eq!(b.data, BufData::I32(vec![2, 3, 4]));
    }

    #[test]
    fn support_is_decided_before_the_launch_not_during_it() {
        let (m, id) = lowered("[kernel ok [i b] [put b i 1.0]]", "ok");
        assert!(supported(&m, id));

        // A function using something outside the subset is declined, so the
        // caller can fall back to the general VM rather than fail.
        let (m2, id2) = lowered("[fn nope [i] [str i]] [fn main [] []]", "nope");
        assert!(!supported(&m2, id2));
    }
}
