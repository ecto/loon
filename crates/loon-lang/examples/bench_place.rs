//! What placement costs, and what a residency policy saves.
//!
//! Dependency-free (no criterion). Run in release for meaningful numbers:
//!
//!     cargo run -q --release -p loon-lang --features gpu --example bench_place
//!
//! Without the `gpu` feature the GPU rows are skipped and everything else
//! still reports.
//!
//! Two things are measured, and it is worth being precise about which is which
//! because they answer different questions.
//!
//! **Kernel time** compares running a kernel through Loon's own interpreter
//! against running it on the GPU. That is not a comparison against optimized C
//! — the CPU side here is an interpreter walking EIR one work item at a time,
//! which is the slowest reasonable baseline. The number says how much there is
//! to gain by leaving the interpreter, not how Loon's generated code compares
//! to a hand-written kernel. Anyone quoting it as the latter is quoting it
//! wrong.
//!
//! **Transfer counts** compare a chain of launches with no residency policy
//! against the same chain under a handler that pins what each launch touches.
//! This one is exact rather than statistical: it counts uploads, and the count
//! does not vary between runs. It is the number the offload literature cares
//! about, and the one a policy actually controls.

use loon_lang::eir::place::{Mode, PlaceStats};
use loon_lang::eir::vm::eval_eir_placed;
use std::time::{Duration, Instant};

/// A kernel with enough arithmetic per element to be worth moving.
const KERNEL: &str = "[kernel heat [i src dst] \
       [let v [at src i]] \
       [put dst i [+ [* 0.25 v] [* 0.75 [sqrt [abs [+ v 1.0]]]]]]]";

fn dir() -> std::path::PathBuf {
    std::env::current_dir().expect("cwd")
}

/// Run `src` under `mode`, returning wall time and the placement accounting.
fn timed(src: &str, mode: Mode) -> Option<(Duration, PlaceStats)> {
    // Warm once: the first GPU launch pays for adapter discovery and shader
    // compilation, which is a real cost but not a per-launch one.
    let _ = eval_eir_placed(src, &dir(), mode).ok()?;
    let start = Instant::now();
    let (_, stats) = eval_eir_placed(src, &dir(), mode).ok()?;
    Some((start.elapsed(), stats))
}

/// A program that runs one kernel over `n` elements, `reps` times.
fn workload(n: usize, reps: usize) -> String {
    let runs = (0..reps)
        .map(|_| format!("[Place.run heat {n} #[src dst]]"))
        .collect::<Vec<_>>()
        .join(" ");
    format!(
        "{KERNEL} \
         [fn main [] \
           [let src [buf-zeros {n}]] \
           [let mut dst [buf-zeros {n}]] \
           {runs} \
           [let _ [Place.read dst]] \
           []]"
    )
}

/// The same workload under a residency handler.
fn workload_resident(n: usize, reps: usize) -> String {
    let runs = (0..reps)
        .map(|_| format!("[Place.run heat {n} #[src dst]]"))
        .collect::<Vec<_>>()
        .join(" ");
    format!(
        "{KERNEL} \
         [fn work [] \
           [let src [buf-zeros {n}]] \
           [let mut dst [buf-zeros {n}]] \
           {runs} \
           [Place.read dst]] \
         [fn resident [thunk] \
           [handle [thunk] \
             [Place.run k m args] [do [Place.pin args] [resume [Place.run k m args]]] \
             [Place.read b]       [resume [Place.read b]]]] \
         [fn main [] [let _ [resident work]] []]"
    )
}

fn main() {
    println!("placement benchmarks");
    println!("machine: {}", std::env::consts::OS);

    // ── Kernel time: interpreter versus GPU ──
    //
    // The CPU column is Loon's interpreter running the kernel body once per
    // element. It is the honest floor, not a tuned baseline.
    println!("\nkernel time — one launch, varying size");
    println!(
        "  {:>10}  {:>12}  {:>12}  {:>8}",
        "elements", "interpreter", "gpu", "ratio"
    );
    for n in [1_024usize, 16_384, 262_144] {
        let src = workload(n, 1);
        let cpu = timed(&src, Mode::Cpu).map(|(d, _)| d);
        let gpu = timed(&src, Mode::Gpu).map(|(d, _)| d);
        let ratio = match (cpu, gpu) {
            (Some(c), Some(g)) if g.as_secs_f64() > 0.0 => {
                format!("{:.1}x", c.as_secs_f64() / g.as_secs_f64())
            }
            _ => "—".to_string(),
        };
        println!(
            "  {:>10}  {:>12}  {:>12}  {:>8}",
            n,
            cpu.map(fmt).unwrap_or_else(|| "—".into()),
            gpu.map(fmt).unwrap_or_else(|| "skipped".into()),
            ratio
        );
    }

    // ── Transfers: what a policy controls ──
    //
    // Exact counts, identical every run. This is the gap the offload
    // literature reports and the one a handler closes here.
    println!("\ntransfers — a chain of launches over one buffer");
    println!(
        "  {:>8}  {:>16}  {:>16}  {:>10}",
        "launches", "no policy", "place/resident", "saved"
    );
    for reps in [1usize, 4, 16, 64] {
        let naive = timed(&workload(4_096, reps), Mode::Device).map(|(_, s)| s);
        let managed = timed(&workload_resident(4_096, reps), Mode::Device).map(|(_, s)| s);
        match (naive, managed) {
            (Some(a), Some(b)) => {
                let saved = a.bytes_in.saturating_sub(b.bytes_in);
                println!(
                    "  {:>8}  {:>16}  {:>16}  {:>10}",
                    reps,
                    format!("{} uploads", a.uploads),
                    format!("{} uploads", b.uploads),
                    loon_lang::eir::place::human_bytes(saved)
                );
            }
            _ => println!("  {reps:>8}  (failed)"),
        }
    }

    // ── What the policy is worth in time, not just in bytes ──
    //
    // The transfer counts above are exact but abstract. This is the same
    // comparison in wall clock on whatever device is present: a chain of
    // launches with no residency policy, where every launch uploads its
    // arguments and copies its results back, against the identical program
    // under a handler that keeps them in place.
    //
    // This is the shape of the gap a recent Rust offload paper measures at up
    // to 400x between its convenient and explicit interfaces. There it is
    // closed by annotations at every call site plus an LLVM pass; here by the
    // handler in os/place.oo.
    // Only the real device is timed. The modelled one moves no actual bytes,
    // so its wall clock would be measuring the interpreter and calling it a
    // transfer cost.
    for (label, mode) in [("gpu", Mode::Gpu)] {
        println!("\nchain of launches on the {label} — 4096 elements");
        println!(
            "  {:>8}  {:>12}  {:>16}  {:>8}",
            "launches", "no policy", "place/resident", "speedup"
        );
        let mut any = false;
        for reps in [8usize, 32, 128] {
            let naive = timed(&workload(4_096, reps), mode).map(|(d, _)| d);
            let managed = timed(&workload_resident(4_096, reps), mode).map(|(d, _)| d);
            match (naive, managed) {
                (Some(a), Some(b)) => {
                    any = true;
                    let speedup = if b.as_secs_f64() > 0.0 {
                        format!("{:.1}x", a.as_secs_f64() / b.as_secs_f64())
                    } else {
                        "—".into()
                    };
                    println!(
                        "  {:>8}  {:>12}  {:>16}  {:>8}",
                        reps,
                        fmt(a),
                        fmt(b),
                        speedup
                    );
                }
                _ => {}
            }
        }
        if !any {
            println!("  skipped — this mode is not available in this build");
        }
    }

    // ── Launch overhead ──
    //
    // Placement is an effect, so every launch is an effect dispatch. This is
    // the cost of that decision, measured rather than asserted: a kernel over
    // a single element, where the work itself is negligible. On the CPU that
    // is dispatch and nothing else.
    println!("\nlaunch overhead — one work item, so the cost is dispatch");
    let one = workload(1, 200);
    if let Some((d, stats)) = timed(&one, Mode::Cpu) {
        let per = d.as_nanos() as f64 / stats.launches.max(1) as f64;
        println!("  {per:>10.0} ns per launch (cpu)");
    }
    if let Some((d, stats)) = timed(&workload_resident(1, 200), Mode::Gpu) {
        let per = d.as_nanos() as f64 / stats.launches.max(1) as f64;
        println!("  {per:>10.0} ns per launch (gpu, buffers resident)");
        println!(
            "  a GPU launch is a submission to another processor; the effect\n               dispatch around it is not what you are paying for."
        );
    }
}

fn fmt(d: Duration) -> String {
    let ms = d.as_secs_f64() * 1000.0;
    if ms >= 1.0 {
        format!("{ms:.1} ms")
    } else {
        format!("{:.0} µs", d.as_micros())
    }
}
