//! QEMU fw_cfg — the firmware configuration channel.
//!
//! A tiny key/value store the emulator exposes to the guest; on `virt` it
//! sits at 0x1010_0000. We use exactly one thing from it: the `etc/ramfb`
//! file, whose contents tell QEMU where our framebuffer lives. Everything
//! goes through the DMA interface, which is byte-order-defined (big-endian)
//! and does not care about the register's access width.

use core::sync::atomic::{fence, Ordering};

use crate::mmio;

const BASE: usize = 0x1010_0000;
const SELECTOR: usize = BASE + 0x08;
const DMA: usize = BASE + 0x10;

const KEY_FILE_DIR: u16 = 0x0019;

const CTL_ERROR: u32 = 1 << 0;
const CTL_READ: u32 = 1 << 1;
const CTL_SELECT: u32 = 1 << 3;
const CTL_WRITE: u32 = 1 << 4;

/// One DMA descriptor, in memory, all fields big-endian.
#[repr(C)]
struct DmaAccess {
    control: u32,
    length: u32,
    address: u64,
}

/// One entry of the file directory: `struct FWCfgFile`.
#[repr(C)]
struct File {
    size: u32,
    select: u16,
    _reserved: u16,
    name: [u8; 56],
}

pub struct FwCfg;

impl FwCfg {
    /// Issue one DMA transfer and wait for it. `control` carries the op bits
    /// (and, if selecting, the key in the upper half).
    unsafe fn dma(&self, control: u32, buf: *mut u8, len: usize) -> Result<(), ()> {
        let desc = DmaAccess {
            control: control.to_be(),
            length: (len as u32).to_be(),
            address: (buf as u64).to_be(),
        };
        // The device reads the descriptor and the buffer straight from RAM.
        fence(Ordering::SeqCst);
        mmio::write64(DMA, (&desc as *const DmaAccess as u64).to_be());
        // Completion is signalled by the device clearing `control`.
        loop {
            fence(Ordering::SeqCst);
            let c = u32::from_be(core::ptr::read_volatile(&desc.control));
            if c == 0 {
                return Ok(());
            }
            if c & CTL_ERROR != 0 {
                return Err(());
            }
            core::hint::spin_loop();
        }
    }

    unsafe fn read(&self, key: u16, buf: &mut [u8]) -> Result<(), ()> {
        self.dma(
            ((key as u32) << 16) | CTL_SELECT | CTL_READ,
            buf.as_mut_ptr(),
            buf.len(),
        )
    }

    /// Continue reading the currently selected item.
    unsafe fn read_more(&self, buf: &mut [u8]) -> Result<(), ()> {
        self.dma(CTL_READ, buf.as_mut_ptr(), buf.len())
    }

    /// Find a named file and return its selector key.
    pub fn find(&self, name: &str) -> Option<u16> {
        unsafe {
            let mut count = [0u8; 4];
            self.read(KEY_FILE_DIR, &mut count).ok()?;
            let count = u32::from_be_bytes(count);
            for _ in 0..count {
                let mut raw = [0u8; core::mem::size_of::<File>()];
                self.read_more(&mut raw).ok()?;
                let f: File = core::ptr::read_unaligned(raw.as_ptr() as *const File);
                let n = f.name.iter().position(|&b| b == 0).unwrap_or(f.name.len());
                if &f.name[..n] == name.as_bytes() {
                    return Some(u16::from_be(f.select));
                }
            }
            None
        }
    }

    /// Overwrite a file's contents (only meaningful for the few writable
    /// ones, like `etc/ramfb`).
    pub fn write(&self, key: u16, data: &[u8]) -> Result<(), ()> {
        unsafe {
            // Selecting via the register first is belt and braces: some
            // firmware paths do it and it costs nothing.
            mmio::write16(SELECTOR, key.to_be());
            self.dma(
                ((key as u32) << 16) | CTL_SELECT | CTL_WRITE,
                data.as_ptr() as *mut u8,
                data.len(),
            )
        }
    }
}
