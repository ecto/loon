//! NS16550a UART — the console driver.
//!
//! QEMU's `virt` machine puts one at 0x1000_0000. OpenSBI has already
//! initialised it by the time we get control, so transmit needs no setup
//! beyond waiting for the holding register to drain.

use crate::mmio;

const BASE: usize = 0x1000_0000;
const THR: usize = BASE; // transmit holding register
const RBR: usize = BASE; // receive buffer register
const LSR: usize = BASE + 5; // line status register

const LSR_RX_READY: u8 = 1 << 0;
const LSR_TX_IDLE: u8 = 1 << 5;

pub struct Uart;

// The receive half is unused until init wants a console to read from; it is
// kept because a driver that can only talk is not a console driver.
#[allow(dead_code)]

impl Uart {
    pub fn putc(&self, c: u8) {
        unsafe {
            while mmio::read8(LSR) & LSR_TX_IDLE == 0 {
                core::hint::spin_loop();
            }
            mmio::write8(THR, c);
        }
    }

    pub fn getc(&self) -> Option<u8> {
        unsafe {
            if mmio::read8(LSR) & LSR_RX_READY == 0 {
                None
            } else {
                Some(mmio::read8(RBR))
            }
        }
    }
}

impl core::fmt::Write for Uart {
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        for b in s.bytes() {
            // The console is line-oriented; QEMU's terminal wants CRLF.
            if b == b'\n' {
                self.putc(b'\r');
            }
            self.putc(b);
        }
        Ok(())
    }
}

#[macro_export]
macro_rules! print {
    ($($arg:tt)*) => {{
        use core::fmt::Write;
        let _ = write!($crate::uart::Uart, $($arg)*);
    }};
}

#[macro_export]
macro_rules! println {
    () => { $crate::print!("\n") };
    ($($arg:tt)*) => {{
        use core::fmt::Write;
        let _ = writeln!($crate::uart::Uart, $($arg)*);
    }};
}
