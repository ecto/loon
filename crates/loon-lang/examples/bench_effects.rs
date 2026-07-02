//! Effect-dispatch microbenchmark: what does a "syscall" (an effect operation
//! resolved by a handler that resumes once) cost, relative to a plain function
//! call, on the EIR VM?
//!
//! Dependency-free (no criterion): run in release for meaningful numbers:
//!
//!     cargo run -q --release -p loon-lang --example bench_effects
//!
//! Loops use `recur` (the VM has no implicit TCO), so the stack stays flat and
//! the numbers isolate DISPATCH cost. A handler clause ending in `[resume ...]`
//! is sealed as a tail resume (End::TailInvoke), so a perform/resume cycle
//! runs in O(1) stack: capture continuation → run clause → splice back.
//! Reports ns per iteration and the effect-vs-call overhead ratio — the
//! receipts behind the "a syscall should cost about a function call" goal in
//! docs/plans/2026-07-01-loon-os.md (§Performance).

use std::time::Instant;

const N: i64 = 200_000;

fn bench(label: &str, src: &str) -> f64 {
    // Warm once (compile + run), then time a fresh run.
    let _ = loon_lang::eir::vm::eval_eir(src).expect("warm run");
    let start = Instant::now();
    let r = loon_lang::eir::vm::eval_eir(src).expect("timed run");
    let elapsed = start.elapsed();
    let ns_per = elapsed.as_nanos() as f64 / N as f64;
    // Keep the optimizer honest: fold the result into the report.
    let last = r.output.last().cloned().unwrap_or_default();
    println!(
        "  {label:<26} {ns_per:>8.1} ns/iter   (result {last}, max_frames {})",
        r.heap_stats.max_frames
    );
    ns_per
}

fn main() {
    // Baseline: a plain function call per iteration, no effects.
    let plain = format!(
        "[fn bump [acc] [+ acc 1]] \
         [fn work [acc n] [if [<= n 0] acc [recur [bump acc] [- n 1]]]] \
         [fn main [] [println [work 0 {N}]]]"
    );

    // One effect operation per iteration, handled by a resume-once clause —
    // the shape a syscall takes: perform, handler resumes with a value.
    let eff_one = format!(
        "[effect Tick [next [Int] Int]] \
         [fn work [acc n] [if [<= n 0] acc [recur [Tick.next acc] [- n 1]]]] \
         [fn main [] [println [handle [work 0 {N}] [Tick.next a] [resume [+ a 1]]]]]"
    );

    // Same, through a second (unrelated) handler in the stack — measures
    // dispatch depth cost, the kernel <- trace <- sandbox composition shape.
    let eff_nested = format!(
        "[effect Tick [next [Int] Int]] [effect Other [noop [] Int]] \
         [fn work [acc n] [if [<= n 0] acc [recur [Tick.next acc] [- n 1]]]] \
         [fn main [] \
           [println [handle [handle [work 0 {N}] [Tick.next a] [resume [+ a 1]]] \
                            [Other.noop] [resume 0]]]]"
    );

    // Forwarding wrapper: the inner clause re-performs to the outer handler —
    // one interposition layer (trace/sandbox shape), two captures per op.
    let eff_forward = format!(
        "[effect Tick [next [Int] Int]] \
         [fn work [acc n] [if [<= n 0] acc [recur [Tick.next acc] [- n 1]]]] \
         [fn main [] \
           [println [handle [handle [work 0 {N}] [Tick.next a] [resume [Tick.next a]]] \
                            [Tick.next a] [resume [+ a 1]]]]]"
    );

    println!("effect-dispatch microbenchmark (N = {N} iterations)\n");
    let base = bench("plain function call", &plain);
    let one = bench("effect op (1 handler)", &eff_one);
    let nested = bench("effect op (2 handlers)", &eff_nested);
    let forward = bench("effect op (forwarded)", &eff_forward);
    println!();
    println!(
        "  overhead vs plain call:  {:.1}x (1 handler)  {:.1}x (2 handlers)  {:.1}x (forwarded)",
        one / base,
        nested / base,
        forward / base
    );
}
