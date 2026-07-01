//! Loon OS (os/) integration suite — runs the phase-1/2 OS library and its
//! demos end-to-end on the EIR VM (the reference backend for handler
//! semantics; `loon test`'s tree-walker lacks correct forwarding).
//!
//! These lock in the OS design's load-bearing claims:
//! - handler interposition (kernel <- trace <- sandbox composition)
//! - record/replay determinism (a tape replays bit-identically, kernel-free)
//! - the pure cooperative scheduler (spawn/yield/send/recv, deadlock detection)
//! - sealed deterministic simulation (same seed -> identical world)

use loon_lang::eir::vm::eval_eir_with_base_dir;
use std::path::{Path, PathBuf};

fn os_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("..").join("..").join("os")
}

/// Run a Loon source string with `[use ...]` resolved against os/.
fn run(src: &str) -> Vec<String> {
    eval_eir_with_base_dir(src, &os_dir())
        .unwrap_or_else(|e| panic!("vm error: {e}"))
        .output
}

/// Run a demo file from os/ and return its printed lines.
fn run_demo(name: &str) -> Vec<String> {
    let path = os_dir().join(name);
    let src = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("read {name}: {e}"));
    run(&src)
}

#[test]
fn sandbox_demo_composes_kernel_trace_sandbox_env() {
    let out = run_demo("demo-sandbox.oo").join("\n");
    // trace sees the POST-rewrite path (sandbox sits inside the trace)
    assert!(out.contains("[trace] Fs.write-file /tmp/loon-motd"), "{out}");
    assert!(out.contains("[trace] Fs.read-file /tmp/loon-motd"), "{out}");
    // real IO round-tripped through the jail, env fully virtualized
    assert!(out.contains("motd: 'welcome to loon os' (HOME=/home/jailbird)"), "{out}");
}

#[test]
fn tape_demo_replays_bit_identically() {
    let out = run_demo("demo-tape.oo").join("\n");
    assert!(out.contains("REPLAY IDENTICAL"), "{out}");
}

#[test]
fn procs_demo_interleaves_and_quiesces() {
    let out = run_demo("demo-procs.oo");
    let joined = out.join("\n");
    // all three processes ran to completion
    assert!(joined.contains("ping done"), "{joined}");
    assert!(joined.contains("pong done"), "{joined}");
    assert!(joined.contains("worker done"), "{joined}");
    assert!(joined.contains("world quiescent"), "{joined}");
    // yield actually interleaves: a worker tick lands between ping/pong turns
    let tick2 = out.iter().position(|l| l.contains("worker tick 2")).unwrap();
    let pong2 = out.iter().position(|l| l.contains("pong got {:n 3")).unwrap();
    assert!(tick2 < pong2, "worker should interleave with ping-pong: {joined}");
}

#[test]
fn sim_demo_is_deterministic_per_seed() {
    let out = run_demo("demo-sim.oo").join("\n");
    assert!(out.contains("WORLDS IDENTICAL"), "{out}");
    assert!(out.contains("seed 7: different world"), "{out}");
}

#[test]
fn scheduler_detects_deadlock() {
    let out = run(r#"
        [use sys]
        [use sched]
        [fn stuck [] [Proc.recv]]
        [fn main []
          [let st [run-procs [fn [] [do [Proc.spawn stuck] [Proc.recv]]]]]
          [IO.println [if [deadlocked? st] "DEADLOCK" "quiet"]]]
    "#)
    .join("\n");
    assert!(out.contains("DEADLOCK"), "{out}");
}

#[test]
fn read_only_sandbox_denies_writes() {
    let out = run(r#"
        [use sys]
        [use kernel]
        [use sandbox]
        [fn main []
          [let r [kernel [fn []
            [try [read-only [fn [] [Fs.write-file "/tmp/loon-denied" "x"]]]
                 [fn [msg] [str "caught: " msg]]]]]]
          [IO.println r]]
    "#)
    .join("\n");
    assert!(out.contains("caught: read-only: denied write to /tmp/loon-denied"), "{out}");
}

#[test]
fn replay_needs_no_kernel_and_no_real_world() {
    // Record against a sealed simulation, then replay the tape with NOTHING
    // underneath: proof that a tape fully captures a program's world.
    let out = run(r#"
        [use sys]
        [use sim]
        [use tape]
        [fn prog []
          [str [Fs.read-file "/cfg"] "-" [Clock.millis] "-" [Rand.int 100]]]
        [fn main []
          [let rec [get [simulate [fn [] [record prog]] 42 {:fs {"/cfg" "v1"}}] :result]]
          [let ghost [replay prog [get rec :tape]]]
          [IO.println [if [= [get rec :result] ghost] "GHOST MATCHES" "diverged"]]]
    "#)
    .join("\n");
    assert!(out.contains("GHOST MATCHES"), "{out}");
}
