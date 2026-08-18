//! Raw memory-mapped IO.
//!
//! Everything a driver does to hardware bottoms out here. Keeping it in one
//! module is what lets a driver be simulated instead of executed: the same
//! driver code over a different `Mmio` is a test, not a boot.

/// Volatile read of a device register.
///
/// # Safety
/// `addr` must be a valid MMIO register for the current machine.
pub unsafe fn read8(addr: usize) -> u8 {
    core::ptr::read_volatile(addr as *const u8)
}

/// Volatile write of a device register.
///
/// # Safety
/// `addr` must be a valid MMIO register for the current machine.
pub unsafe fn write8(addr: usize, val: u8) {
    core::ptr::write_volatile(addr as *mut u8, val)
}
