//! What a placement backend has to be able to do.
//!
//! There are two of these and they could hardly be less alike. On a desktop,
//! `eir::gpu::Gpu` drives wgpu directly and blocks on the queue. In a browser,
//! `loon-wasm` proxies every call to JavaScript, because WebGPU is only
//! reachable through promises and the VM is synchronous — the wasm side blocks
//! on `Atomics.wait` while another thread does the asynchronous part.
//!
//! Both answer the same six questions, so the VM does not know which one it
//! has. That is the same move placement makes at the language level, one layer
//! down: the thing that varies is behind an interface, and the code that uses
//! it does not change when the answer does.

use super::vm::Buffer;

/// Why a device operation failed, in a sentence a person can act on.
#[derive(Debug, Clone)]
pub struct DeviceError(pub String);

impl std::fmt::Display for DeviceError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for DeviceError {}

/// A device that kernels can be dispatched to.
///
/// Buffers are identified by the host's heap slot, so residency is something
/// the caller and the device agree about by name rather than by handle.
pub trait Device {
    /// A human-readable name, for `--place-stats` and error messages.
    fn name(&self) -> String;

    /// Put `buf` on the device if it is not there already. Returns whether an
    /// upload actually happened, so the caller's accounting reflects the
    /// hardware rather than a prediction of it.
    fn ensure_resident(&self, id: usize, buf: &Buffer) -> Result<bool, DeviceError>;

    /// Whether `id` currently has device storage.
    fn is_resident(&self, id: usize) -> bool;

    /// Run `shader` over `n` work items against buffers that are already
    /// resident. Nothing is uploaded and nothing is read back: when data moves
    /// is the caller's decision, which is the whole point.
    fn dispatch(
        &self,
        shader: &str,
        entry: &str,
        n: u32,
        scalars: &[f32],
        buffers: &[usize],
    ) -> Result<(), DeviceError>;

    /// Copy a resident buffer's contents back to the host.
    fn download(&self, id: usize, byte_len: usize) -> Result<Vec<u8>, DeviceError>;

    /// Release device storage for `id`.
    fn evict(&self, id: usize);
}

// ─── The installed device ──────────────────────────────────────────────────

thread_local! {
    /// A device supplied by the host rather than opened by the VM.
    ///
    /// This is how a browser gets a GPU. `loon-wasm` installs a bridge that
    /// proxies every call to JavaScript; the VM finds it here and never learns
    /// that the work is happening on the other side of a worker boundary.
    static INSTALLED: std::cell::RefCell<Option<std::rc::Rc<dyn Device>>> =
        const { std::cell::RefCell::new(None) };
}

/// Install a device for this thread. Replaces any previous one.
pub fn install(device: std::rc::Rc<dyn Device>) {
    INSTALLED.with(|d| *d.borrow_mut() = Some(device));
}

/// Forget the installed device, if any.
pub fn uninstall() {
    INSTALLED.with(|d| *d.borrow_mut() = None);
}

/// The device installed by the host, if there is one.
pub fn installed() -> Option<std::rc::Rc<dyn Device>> {
    INSTALLED.with(|d| d.borrow().clone())
}

/// Convert a buffer to the 32-bit form a device can hold.
///
/// WGSL core has no 64-bit scalar. A launch with such a buffer is refused
/// before reaching here, so this is only the identity in practice — but it is
/// the one place that would change if a device ever gained wider types.
pub fn narrow(buf: &Buffer) -> Buffer {
    use super::vm::BufData;
    match &buf.data {
        BufData::F64(v) => Buffer {
            data: BufData::F32(v.iter().map(|x| *x as f32).collect()),
        },
        BufData::I64(v) => Buffer {
            data: BufData::I32(v.iter().map(|x| *x as i32).collect()),
        },
        _ => buf.clone(),
    }
}
