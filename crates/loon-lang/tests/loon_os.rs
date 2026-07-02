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
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("..")
        .join("os")
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
    assert!(
        out.contains("[trace] Fs.write-file /tmp/loon-motd"),
        "{out}"
    );
    assert!(out.contains("[trace] Fs.read-file /tmp/loon-motd"), "{out}");
    // real IO round-tripped through the jail, env fully virtualized
    assert!(
        out.contains("motd: 'welcome to loon os' (HOME=/home/jailbird)"),
        "{out}"
    );
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
    let tick2 = out
        .iter()
        .position(|l| l.contains("worker tick 2"))
        .unwrap();
    let pong2 = out
        .iter()
        .position(|l| l.contains("pong got {:n 3"))
        .unwrap();
    assert!(
        tick2 < pong2,
        "worker should interleave with ping-pong: {joined}"
    );
}

#[test]
fn sim_demo_is_deterministic_per_seed() {
    let out = run_demo("demo-sim.oo").join("\n");
    assert!(out.contains("WORLDS IDENTICAL"), "{out}");
    assert!(out.contains("seed 7: different world"), "{out}");
}

#[test]
fn kv_demo_survives_lossy_network_deterministically() {
    // The phase-2 exit demo: a KV server + 2 clients over a 25%-drop/10%-dup
    // network whose dice come from the sim's seeded PRNG. All puts must land
    // (retries recover the drops), the same seed must reproduce the identical
    // world, and a different seed must converge to the same database.
    let out = run_demo("demo-kv.oo").join("\n");
    assert!(
        out.contains("final db:      {alpha 1 gamma 3 beta 2 delta 4}"),
        "{out}"
    );
    assert!(out.contains("STORM REPRODUCED EXACTLY"), "{out}");
    assert!(out.contains("SAME converged database"), "{out}");
}

#[test]
fn try_recv_is_nonblocking() {
    // try-recv returns :none instead of parking — no deadlock when the
    // mailbox is empty, {:msg m} when something is waiting.
    let out = run(r#"
        [use sys]
        [use sched]
        [fn main []
          [let st [run-procs [fn []
            [let empty [Proc.try-recv]]
            [Proc.send [Proc.self] "hi"]
            [let full [Proc.try-recv]]
            [str empty "/" [get full :msg]]]]]
          [println [get [first [get st :done]] :value]]]
    "#)
    .join("\n");
    assert!(out.contains(":none/hi"), "{out}");
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
    assert!(
        out.contains("caught: read-only: denied write to /tmp/loon-denied"),
        "{out}"
    );
}

#[test]
fn agent_demo_contains_untrusted_code() {
    let out = run_demo("demo-agent.oo").join("\n");
    // honest work succeeded
    assert!(
        out.contains("answer file: '42 (compute the answer)'"),
        "{out}"
    );
    // both hostile ops neutralized: sentinels in the agent's own result...
    assert!(out.contains(":stole EACCES: /etc/credentials"), "{out}");
    assert!(out.contains(":exfil-result :denied"), "{out}");
    // ...and the exfil file was never written to the (virtual) filesystem
    assert!(out.contains("exfil file:  '<never written>'"), "{out}");
    // the flight recorder captured every attempt, including the denied ones
    assert!(
        out.contains("{:op :read :path /etc/credentials :got EACCES"),
        "{out}"
    );
    assert!(out.contains("{:op :write :path /evil/exfil"), "{out}");
    // whole run is seed-deterministic
    assert!(out.contains("AGENT RUN REPRODUCED EXACTLY"), "{out}");
}

#[test]
fn gated_denial_is_a_sentinel_not_an_abort() {
    // A denied op resumes the agent with a value, so the agent keeps running
    // and its later work still completes (no cross-handler abort).
    let out = run(r#"
        [use sys]
        [use kernel]
        [use agent]
        [fn prog []
          [let a [Fs.read-file "/allowed"]]
          [let b [Fs.read-file "/secret"]]
          [str "a=" a " b=" b " alive"]]
        [fn main []
          [IO.println
            [kernel [fn []
              [gated prog [fn [req] [= [get req :path] "/allowed"]]]]]]]
    "#)
    .join("\n");
    assert!(out.contains("b=EACCES: /secret"), "{out}");
    assert!(
        out.contains("alive"),
        "denied op must not abort the agent: {out}"
    );
}

#[test]
fn sandbox_denies_dotdot_traversal() {
    // A jailed path containing `..` would resolve above the jail root once the
    // real filesystem collapses it; the sandbox rejects it instead.
    let out = run(r#"
        [use sys]
        [use sim]
        [use sandbox]
        [fn escape [] [Fs.read-file "/../etc/passwd"]]
        [fn main []
          [let r [simulate
                   [fn [] [try [sandboxed escape "/jail"] [fn [m] [str "BLOCKED: " m]]]]
                   1 {}]]
          [println [get r :result]]]
    "#)
    .join("\n");
    assert!(
        out.contains("BLOCKED: sandbox: path escapes jail: /../etc/passwd"),
        "{out}"
    );
}

#[test]
fn audited_records_clock_sleep() {
    // The flight recorder must log Clock.sleep, not let it slip through
    // unrecorded while the (virtual) clock still advances.
    let out = run(r#"
        [use sys]
        [use sim]
        [use agent]
        [fn sleepy [] [do [Clock.sleep 500] [Rand.int 10]]]
        [fn main []
          [let w [simulate [fn [] [audited sleepy]] 1 {}]]
          [println [str "time=" [get w :time] " audit=" [get [get w :result] :audit]]]]
    "#)
    .join("\n");
    assert!(
        out.contains("time=500"),
        "sleep must advance the clock: {out}"
    );
    assert!(
        out.contains("{:op :sleep :ms 500}"),
        "sleep must be audited: {out}"
    );
}

#[test]
fn replay_detects_desync_loudly() {
    // A replay that reads more than the tape recorded has diverged; it must
    // fail loudly, not silently resume with Unit.
    let out = run(r#"
        [use sys]
        [use tape]
        [fn two-reads [] [str [Fs.read-file "/a"] "-" [Fs.read-file "/b"]]]
        [fn main []
          [println [try [replay two-reads #["only-one"]]
                        [fn [m] [str "CAUGHT: " m]]]]]
    "#)
    .join("\n");
    assert!(
        out.contains("CAUGHT: replay desync: tape exhausted"),
        "{out}"
    );
}

#[test]
fn chaos_demo_reproduces_faults_and_supervisor_recovers() {
    let out = run_demo("demo-chaos.oo").join("\n");
    // seed 3 injects faults; the supervisor absorbs them within its budget
    assert!(out.contains("[supervisor] child failed: chaos:"), "{out}");
    assert!(out.contains("outcome: ok (v1)"), "{out}");
    assert!(out.contains("result file: 'processed:v1'"), "{out}");
    // the whole storm — faults, restarts, final world — is seed-deterministic
    assert!(out.contains("CHAOS REPRODUCED EXACTLY"), "{out}");
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
