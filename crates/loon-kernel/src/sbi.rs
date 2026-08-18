//! The thin slice of the RISC-V SBI we need from OpenSBI.

use core::arch::asm;

fn ecall(eid: usize, fid: usize, a0: usize, a1: usize) -> isize {
    let err: isize;
    unsafe {
        asm!(
            "ecall",
            inlateout("a0") a0 => err,
            in("a1") a1,
            in("a6") fid,
            in("a7") eid,
            options(nostack),
        );
    }
    err
}

/// Power off the machine. Returns only if the firmware refuses.
pub fn shutdown(failure: bool) -> ! {
    const SRST: usize = 0x5352_5354;
    let reason = if failure { 1 } else { 0 };
    ecall(SRST, 0, 0, reason); // system_reset(SHUTDOWN, reason)
    ecall(0x08, 0, 0, 0); // legacy shutdown, for older firmware
    loop {
        core::hint::spin_loop();
    }
}
