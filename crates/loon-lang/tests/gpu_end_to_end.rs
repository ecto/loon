//! A Loon kernel, on a real GPU, agreeing with the CPU.
//!
//! Everything in the chain gets exercised: the `[kernel ...]` form, ownership
//! modes deciding which buffers are inputs, the WGSL emitter, and wgpu putting
//! the shader on whatever hardware this machine has. The result is compared
//! against the same kernel run through the interpreter, because a GPU that
//! computes something *different* quickly is not an optimization.
//!
//! Skipped, loudly, when there is no GPU. A machine without one is not a
//! failing machine, and a test that quietly passed in that case would be
//! worse than no test.

#![cfg(feature = "gpu")]

use loon_lang::check::Checker;
use loon_lang::eir::gpu::{self, Gpu, GpuArg};
use loon_lang::eir::layout::DType;
use loon_lang::eir::lower::lower;
use loon_lang::eir::vm::{eval_eir, BufData, Buffer};
use loon_lang::eir::wgsl;
use loon_lang::parser::parse;

fn gpu_or_skip() -> Option<Gpu> {
    match Gpu::open() {
        Ok(g) => {
            println!("device: {}", g.name());
            Some(g)
        }
        Err(e) => {
            println!("SKIPPED — no GPU on this machine: {e}");
            None
        }
    }
}

/// Compile the named kernel in `src` to WGSL and run it on the GPU.
fn run_on_gpu(
    gpu_dev: &Gpu,
    src: &str,
    kernel: &str,
    n: u32,
    scalars: &[f32],
    buffers: &[Buffer],
) -> Vec<Buffer> {
    let exprs = parse(src).expect("parses");
    let mut checker = Checker::new();
    let errors = checker.check_program(&exprs);
    assert!(errors.is_empty(), "check errors: {errors:?}");
    let module = lower(&checker);
    let func = module
        .funcs
        .iter()
        .find(|f| f.name.as_deref() == Some(kernel))
        .expect("kernel lowered");

    let kinds = wgsl::infer_arg_kinds(&module, func.id, DType::F32);
    let shader = wgsl::emit(&module, func.id, &kinds).expect("emits WGSL");

    // Narrow every buffer to what the device can hold, in argument order.
    // (WGSL core has no 64-bit scalar, so an f64 buffer is computed in f32.)
    let owned: Vec<Buffer> = buffers.iter().map(gpu::narrow).collect();

    // Then describe each argument: scalars from `scalars`, buffers from
    // `owned`, in the order the kernel declared them.
    let mut scalar_iter = scalars.iter();
    let mut owned_iter = owned.iter();
    let args: Vec<GpuArg> = kinds
        .iter()
        .map(|kind| match kind {
            wgsl::ArgKind::Scalar(_) => {
                GpuArg::Scalar(*scalar_iter.next().expect("a scalar argument"))
            }
            wgsl::ArgKind::Buffer { writable, .. } => GpuArg::Buffer {
                data: owned_iter.next().expect("a buffer argument"),
                writable: *writable,
            },
        })
        .collect();

    let results = gpu_dev.run(&shader, "main", n, &args).expect("dispatch");

    let mut out = owned.clone();
    for (slot, bytes) in results {
        // `slot` is an argument index; find which buffer that was.
        let buffer_index = kinds
            .iter()
            .take(slot)
            .filter(|k| matches!(k, wgsl::ArgKind::Buffer { .. }))
            .count();
        let target = &mut out[buffer_index];
        let n = target.len();
        let mut vals: Vec<f32> = bytes
            .chunks_exact(4)
            .map(|c| f32::from_le_bytes(c.try_into().unwrap()))
            .collect();
        vals.truncate(n);
        target.data = BufData::F32(vals);
    }
    out
}

/// The same kernel through the interpreter, as the reference answer.
fn run_on_cpu(src: &str, main: &str) -> Vec<String> {
    eval_eir(&format!("{src} {main}")).expect("cpu run").output
}

const SAXPY: &str = "[kernel saxpy [i a x y out] \
                       [put out i [+ [* a [at x i]] [at y i]]]]";

#[test]
fn saxpy_agrees_between_the_gpu_and_the_cpu() {
    let Some(dev) = gpu_or_skip() else { return };

    let n = 256usize;
    let x: Vec<f32> = (0..n).map(|i| i as f32).collect();
    let y: Vec<f32> = (0..n).map(|i| (i * 2) as f32).collect();
    let out = vec![0.0f32; n];

    let results = run_on_gpu(
        &dev,
        SAXPY,
        "saxpy",
        n as u32,
        &[3.0],
        &[
            Buffer {
                data: BufData::F32(x.clone()),
            },
            Buffer {
                data: BufData::F32(y.clone()),
            },
            Buffer {
                data: BufData::F32(out),
            },
        ],
    );

    let BufData::F32(got) = &results[2].data else {
        panic!("expected an f32 buffer");
    };
    let expected: Vec<f32> = (0..n).map(|i| 3.0 * x[i] + y[i]).collect();
    assert_eq!(got.len(), n);
    for (i, (g, e)) in got.iter().zip(expected.iter()).enumerate() {
        assert!(
            (g - e).abs() < 1e-4,
            "element {i}: GPU gave {g}, expected {e}"
        );
    }
}

#[test]
fn the_gpu_result_matches_what_the_interpreter_computes() {
    // Not "close to a formula I wrote in Rust" — the same Loon program, run
    // both ways. This is the comparison that would catch the emitter and the
    // interpreter disagreeing about what a kernel means.
    let Some(dev) = gpu_or_skip() else { return };

    let cpu = run_on_cpu(
        SAXPY,
        "[fn main [] \
           [let x [buf #[0 1 2 3 4 5 6 7]]] \
           [let y [buf #[0 2 4 6 8 10 12 14]]] \
           [let mut out [buf-zeros 8]] \
           [Place.run saxpy 8 #[3.0 x y out]] \
           [IO.println [Place.read out]]]",
    );

    let results = run_on_gpu(
        &dev,
        SAXPY,
        "saxpy",
        8,
        &[3.0],
        &[
            Buffer {
                data: BufData::F32((0..8).map(|i| i as f32).collect()),
            },
            Buffer {
                data: BufData::F32((0..8).map(|i| (i * 2) as f32).collect()),
            },
            Buffer {
                data: BufData::F32(vec![0.0; 8]),
            },
        ],
    );
    let BufData::F32(got) = &results[2].data else {
        panic!("expected an f32 buffer");
    };
    let rendered = format!(
        "#[{}]",
        got.iter()
            .map(|v| {
                if v.fract() == 0.0 {
                    format!("{}", *v as i64)
                } else {
                    format!("{v}")
                }
            })
            .collect::<Vec<_>>()
            .join(" ")
    );
    assert_eq!(cpu, vec![rendered]);
}

#[test]
fn a_branching_kernel_runs_on_the_gpu() {
    // Control flow goes through the block dispatcher in the emitted shader,
    // so this exercises a different path than straight-line arithmetic.
    let Some(dev) = gpu_or_skip() else { return };

    let src = "[kernel clamp [i lo hi b] \
                 [let v [at b i]] \
                 [put b i [if [< v lo] lo [if [> v hi] hi v]]]]";
    let input: Vec<f32> = vec![-5.0, 0.25, 0.5, 9.0];
    let results = run_on_gpu(
        &dev,
        src,
        "clamp",
        4,
        &[0.0, 1.0],
        &[Buffer {
            data: BufData::F32(input),
        }],
    );
    let BufData::F32(got) = &results[0].data else {
        panic!("expected an f32 buffer");
    };
    assert_eq!(got, &vec![0.0, 0.25, 0.5, 1.0]);
}

#[test]
fn a_large_launch_covers_every_element() {
    // Enough work items to span many workgroups, so a mistake in the dispatch
    // arithmetic shows up as untouched elements at the end.
    let Some(dev) = gpu_or_skip() else { return };

    let n = 10_000usize;
    let results = run_on_gpu(
        &dev,
        "[kernel fill [i b] [put b i 7.0]]",
        "fill",
        n as u32,
        &[],
        &[Buffer {
            data: BufData::F32(vec![0.0; n]),
        }],
    );
    let BufData::F32(got) = &results[0].data else {
        panic!("expected an f32 buffer");
    };
    assert_eq!(got.len(), n);
    assert!(
        got.iter().all(|v| *v == 7.0),
        "every element should have been written; {} were not",
        got.iter().filter(|v| **v != 7.0).count()
    );
}
