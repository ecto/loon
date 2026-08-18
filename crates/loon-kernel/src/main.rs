//! Loon as a unikernel.
//!
//! There is no userspace here and no syscall boundary: the Loon program *is*
//! the kernel, and what would be a syscall elsewhere is an effect performed
//! into a handler that happens to touch hardware.

#![no_std]
#![no_main]

extern crate alloc;

use core::arch::global_asm;

#[global_allocator]
static HEAP: heap::Heap = heap::Heap::new();

#[macro_use]
mod uart;
mod eir;
mod heap;
mod mmio;
mod sbi;

// Set up a stack and clear .bss before anything Rust-shaped runs. `a0` holds
// the hart id and `a1` the device tree pointer; we keep them for `kmain`.
global_asm!(
    r#"
    .section .text.entry
    .globl _start
_start:
    la      sp, __stack_top

    la      t0, __bss_start
    la      t1, __bss_end
1:  bgeu    t0, t1, 2f
    sd      zero, 0(t0)
    addi    t0, t0, 8
    j       1b

2:  tail    kmain
"#
);

extern "C" {
    static __heap_start: u8;
    static __heap_end: u8;
}

/// `rdtime` at kernel entry, for the boot-to-init measurement.
static BOOT: core::sync::atomic::AtomicU64 = core::sync::atomic::AtomicU64::new(0);

#[no_mangle]
pub extern "C" fn kmain(hart: usize, dtb: usize) -> ! {
    BOOT.store(now(), core::sync::atomic::Ordering::Relaxed);
    let (start, end) = unsafe {
        (
            &__heap_start as *const u8 as usize,
            &__heap_end as *const u8 as usize,
        )
    };
    unsafe { HEAP.init(start, end - start) };

    println!();
    println!("loon unikernel — hart {hart}, dtb {dtb:#x}");
    println!(
        "heap {:#x}..{:#x} ({} KiB)",
        start,
        end,
        (end - start) / 1024
    );

    let image = include_bytes!(env!("LOON_BOOT_IMAGE"));
    println!("init image {} bytes", image.len());
    println!();

    for (name, img) in [
        ("loop ", include_bytes!(env!("LOON_LOOP_IMAGE")).as_slice()),
        ("bench", include_bytes!(env!("LOON_BENCH_IMAGE")).as_slice()),
    ] {
        if let Err(e) = run_bench_named(name, img) {
            println!("{name} failed: {e}");
        }
    }
    println!();

    let t0 = now();
    match run_init(image) {
        Ok(()) => {
            println!();
            println!(
                "init exited cleanly in {} us ({} us since entry)",
                micros_since(t0),
                micros_since(BOOT.load(core::sync::atomic::Ordering::Relaxed)),
            );
            sbi::shutdown(false)
        }
        Err(e) => {
            println!();
            println!("init failed: {e}");
            sbi::shutdown(true)
        }
    }
}

/// The machine, as the VM sees it. Effects that no Loon handler caught
/// arrive here, which is the only place in the system that touches hardware.
struct Machine;

impl eir::vm::Host for Machine {
    fn write(&mut self, s: &str) {
        print!("{s}");
    }

    fn ticks(&mut self) -> i64 {
        let t: u64;
        unsafe { core::arch::asm!("rdtime {}", out(reg) t) };
        t as i64
    }
}

/// QEMU's `virt` machine ticks the RISC-V `time` CSR at 10 MHz.
const TIMEBASE_HZ: u64 = 10_000_000;

fn now() -> u64 {
    let t: u64;
    unsafe { core::arch::asm!("rdtime {}", out(reg) t) };
    t
}

/// Microseconds between two `rdtime` reads.
fn micros_since(start: u64) -> u64 {
    now().saturating_sub(start) * 1_000_000 / TIMEBASE_HZ
}

fn run_init(image: &[u8]) -> Result<(), alloc::string::String> {
    let module = eir::decode::decode(image)?;
    let mut machine = Machine;
    let mut vm = eir::vm::Vm::new(&module, &mut machine).with_fuel(500_000_000);
    vm.run()?;
    Ok(())
}

/// Time the interpreter on an IO-free workload and report ns per dispatched
/// op. Steps come from the VM's own loop counter, so this measures the
/// interpreter rather than the console.
fn run_bench_named(name: &str, image: &[u8]) -> Result<(), alloc::string::String> {
    let module = eir::decode::decode(image)?;
    let mut machine = Machine;
    let mut vm = eir::vm::Vm::new(&module, &mut machine).with_fuel(2_000_000_000);

    let t = now();
    let a0 = HEAP.allocs();
    vm.run()?;
    let us = micros_since(t);
    let allocs = HEAP.allocs() - a0;
    let steps = vm.steps();

    println!(
        "{name}: {steps} ops in {us} us = {} ns/op, {allocs} allocs = {} per 100 ops",
        (us * 1000).checked_div(steps).unwrap_or(0),
        (allocs * 100).checked_div(steps).unwrap_or(0),
    );
    Ok(())
}

#[panic_handler]
fn panic(info: &core::panic::PanicInfo) -> ! {
    println!("\nkernel panic: {info}");
    sbi::shutdown(true)
}
