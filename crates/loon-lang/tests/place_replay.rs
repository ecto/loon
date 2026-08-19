//! A placed program can be recorded once and replayed without the device.
//!
//! This is the half of the story an offload compiler does not tell. A kernel
//! that ran on a GPU is, from the program's point of view, a launch that
//! returned nothing and a read that produced some numbers. Recording those and
//! feeding them back reproduces the run exactly — on a machine with no GPU, in
//! CI, or after the hardware it was written for stopped existing.

use loon_lang::eir::place::Mode;
use loon_lang::eir::replay::{parse_trace, TraceRecorder};
use loon_lang::eir::vm::{eval_eir_placed, eval_eir_recorded, eval_eir_replayed};

const PROGRAM: &str = "[kernel scale [i s b] [put b i [* s [at b i]]]] \
     [fn main [] \
       [let mut b [buf #[1 2 3 4]]] \
       [Place.run scale 4 #[3.0 b]] \
       [Place.run scale 4 #[2.0 b]] \
       [IO.println [Place.read b]]]";

fn temp_trace(name: &str) -> std::path::PathBuf {
    let mut p = std::env::temp_dir();
    p.push(format!(
        "loon-place-replay-{name}-{}.oo",
        std::process::id()
    ));
    let _ = std::fs::remove_file(&p);
    p
}

fn record(src: &str, path: &std::path::Path) -> Vec<String> {
    let dir = std::env::current_dir().expect("cwd");
    let recorder = TraceRecorder::create(path).expect("create trace");
    let result = eval_eir_recorded(src, &dir, recorder).expect("records");
    loon_lang::eir::replay::finalize_trace_file(path).expect("finalize");
    result.output
}

fn replay(src: &str, path: &std::path::Path) -> Vec<String> {
    let dir = std::env::current_dir().expect("cwd");
    let text = std::fs::read_to_string(path).expect("read trace");
    let entries = parse_trace(&text).expect("parse trace");
    eval_eir_replayed(src, &dir, entries)
        .expect("replays")
        .0
        .output
}

#[test]
fn a_placed_run_replays_to_the_same_output() {
    let path = temp_trace("basic");
    let recorded = record(PROGRAM, &path);
    assert_eq!(recorded, vec!["#[6 12 18 24]"]);

    let replayed = replay(PROGRAM, &path);
    assert_eq!(
        recorded, replayed,
        "a replayed run must observe what the recorded run observed"
    );
    let _ = std::fs::remove_file(&path);
}

#[test]
fn the_trace_records_launches_and_reads() {
    let path = temp_trace("entries");
    record(PROGRAM, &path);
    let text = std::fs::read_to_string(&path).expect("read trace");

    let runs = text.matches(":op \"run\"").count();
    let reads = text.matches(":op \"read\"").count();
    assert_eq!(runs, 2, "both launches should be in the trace:\n{text}");
    assert_eq!(reads, 1, "the one read should be in the trace:\n{text}");
    let _ = std::fs::remove_file(&path);
}

#[test]
fn statistics_are_never_replayed() {
    // `Place.stats` reports on the run currently happening. A replayed run
    // really did move no bytes, so feeding back the original counts would be
    // a recording that lies about the execution it is part of.
    let src = "[kernel k [i b] [put b i 1.0]] \
         [fn main [] \
           [let mut b [buf-zeros 4]] \
           [Place.run k 4 #[b]] \
           [let _ [Place.read b]] \
           [IO.println [get [Place.stats] :launches]]]";
    let path = temp_trace("stats");

    let recorded = record(src, &path);
    assert_eq!(recorded, vec!["1"], "the recorded run launched once");

    let text = std::fs::read_to_string(&path).expect("read trace");
    assert!(
        !text.contains(":op \"stats\""),
        "stats should not be recorded:\n{text}"
    );

    // The replayed run performed no launch of its own, and says so.
    let replayed = replay(src, &path);
    assert_eq!(replayed, vec!["0"]);
    let _ = std::fs::remove_file(&path);
}

#[test]
fn a_recording_made_on_one_device_replays_anywhere() {
    // The recording is of what the *program* observed, not of how the device
    // behaved, so where it was made does not constrain where it is replayed.
    let path = temp_trace("device");
    let dir = std::env::current_dir().expect("cwd");

    let recorder = TraceRecorder::create(&path).expect("create trace");
    // Record against the simulated discrete device, which really does move
    // bytes and evict buffers.
    let recorded = {
        let _ = eval_eir_placed(PROGRAM, &dir, Mode::Device);
        eval_eir_recorded(PROGRAM, &dir, recorder)
            .expect("records")
            .output
    };
    loon_lang::eir::replay::finalize_trace_file(&path).expect("finalize");

    let replayed = replay(PROGRAM, &path);
    assert_eq!(recorded, replayed);
    let _ = std::fs::remove_file(&path);
}

#[test]
fn a_buffer_prints_as_its_shape_not_its_address() {
    // Buffers are handles to bulk data that may live on another device.
    // Printing a heap slot would put an allocation number in output that is
    // supposed to be reproducible — and would make traces differ run to run.
    let out = loon_lang::eir::vm::eval_eir(
        "[fn main [] [IO.println [buf #[1 2 3]]] [IO.println [buf-i32 #[1]]]]",
    )
    .expect("runs")
    .output;
    assert_eq!(out, vec!["#buf<f32 x 3>", "#buf<i32 x 1>"]);
}
