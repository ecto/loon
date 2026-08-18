//! ramfb — a linear framebuffer in guest RAM that QEMU scans out.
//!
//! The simplest display a VM can have: we own a `width * height` array of
//! XRGB pixels, tell QEMU where it is once through fw_cfg, and from then on
//! drawing is writing memory. No command queue, no interrupts, no GPU. Boot
//! with `-device ramfb` and a real `-display` to see it.

use alloc::vec;
use alloc::vec::Vec;

use crate::fwcfg::FwCfg;

/// DRM_FORMAT_XRGB8888 — 'XR24' as a little-endian fourcc.
const FOURCC_XRGB8888: u32 = 0x3432_5258;

/// The config record QEMU expects in `etc/ramfb`, all fields big-endian.
#[repr(C, packed)]
struct Cfg {
    addr: u64,
    fourcc: u32,
    flags: u32,
    width: u32,
    height: u32,
    stride: u32,
}

pub struct Ramfb {
    pub width: u32,
    pub height: u32,
    pixels: Vec<u32>,
}

impl Ramfb {
    /// Allocate a framebuffer and point QEMU at it. `None` if the machine
    /// has no `etc/ramfb` — i.e. was booted without `-device ramfb`.
    pub fn init(width: u32, height: u32) -> Option<Ramfb> {
        let key = FwCfg.find("etc/ramfb")?;
        let pixels = vec![0u32; (width * height) as usize];
        let cfg = Cfg {
            addr: (pixels.as_ptr() as u64).to_be(),
            fourcc: FOURCC_XRGB8888.to_be(),
            flags: 0,
            width: width.to_be(),
            height: height.to_be(),
            stride: (width * 4).to_be(),
        };
        let bytes = unsafe {
            core::slice::from_raw_parts(
                &cfg as *const Cfg as *const u8,
                core::mem::size_of::<Cfg>(),
            )
        };
        FwCfg.write(key, bytes).ok()?;
        Some(Ramfb {
            width,
            height,
            pixels,
        })
    }

    pub fn clear(&mut self, color: u32) {
        self.pixels.fill(color);
    }

    /// Fill a rectangle, clipped to the screen. Coordinates are signed so a
    /// shape can hang off any edge without the caller doing arithmetic.
    pub fn fill_rect(&mut self, x: i64, y: i64, w: i64, h: i64, color: u32) {
        let (sw, sh) = (self.width as i64, self.height as i64);
        let x0 = x.max(0);
        let y0 = y.max(0);
        let x1 = (x + w).min(sw);
        let y1 = (y + h).min(sh);
        if x0 >= x1 || y0 >= y1 {
            return;
        }
        for row in y0..y1 {
            let start = (row * sw + x0) as usize;
            let end = (row * sw + x1) as usize;
            self.pixels[start..end].fill(color);
        }
    }

    /// Nothing to flush — QEMU reads the buffer on its own refresh timer.
    /// Kept as the seam where a double buffer or a dirty-rect hint would go.
    pub fn present(&mut self) {
        core::sync::atomic::fence(core::sync::atomic::Ordering::SeqCst);
    }
}
