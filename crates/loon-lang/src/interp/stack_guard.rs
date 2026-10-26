//! Native stack-overflow guard for the tree-walking interpreter.
//!
//! Overflowing the native stack doesn't unwind: the process aborts. Hosts
//! evaluate loon in-process (vcad.app recompiles the open document on every
//! edit), so a runaway recursion like `[fn f [x] [+ 1 [f x]]]` would take the
//! whole app down. `eval` asks [`exhausted`] before every node, and running
//! into the red zone at the bottom of the stack becomes an error.
//!
//! The stack's bounds come from pthreads (Apple targets and Linux glibc), so
//! the guard adapts to whatever thread the host runs us on (a 512 KiB
//! dispatch thread or an 8 MiB main thread). Elsewhere (wasm, where overflow
//! is a catchable trap, and other targets) it is a no-op.

use std::cell::Cell;

thread_local! {
    /// (lowest usable address, red zone) for this thread; `None` until
    /// computed, `Some(None)` when the bounds are unknown.
    static LIMIT: Cell<Option<Option<(usize, usize)>>> = const { Cell::new(None) };
}

/// Approximate current stack pointer.
#[inline(always)]
fn stack_pointer() -> usize {
    let marker = 0u8;
    std::hint::black_box(&marker) as *const u8 as usize
}

/// Stack kept free: an eighth of it, 32..=256 KiB. `eval` checks on every
/// node, so this only has to cover one eval-to-eval hop plus a builtin's own
/// frames (an unoptimized eval frame is ~15 KiB).
fn red_zone(size: usize) -> usize {
    (size / 8).clamp(32 * 1024, 256 * 1024)
}

#[cfg(target_vendor = "apple")]
fn stack_bounds() -> Option<(usize, usize)> {
    use std::ffi::c_void;
    extern "C" {
        fn pthread_self() -> *mut c_void;
        fn pthread_get_stackaddr_np(thread: *mut c_void) -> *mut c_void;
        fn pthread_get_stacksize_np(thread: *mut c_void) -> usize;
    }
    // SAFETY: plain queries on the calling thread's own pthread handle.
    let (top, size) = unsafe {
        let t = pthread_self();
        (
            pthread_get_stackaddr_np(t) as usize,
            pthread_get_stacksize_np(t),
        )
    };
    (top > size && size > 0).then(|| (top - size, size))
}

#[cfg(all(target_os = "linux", target_env = "gnu"))]
fn stack_bounds() -> Option<(usize, usize)> {
    use std::ffi::c_void;
    extern "C" {
        fn pthread_self() -> usize;
        fn pthread_getattr_np(thread: usize, attr: *mut c_void) -> i32;
        fn pthread_attr_getstack(
            attr: *const c_void,
            addr: *mut *mut c_void,
            size: *mut usize,
        ) -> i32;
        fn pthread_attr_destroy(attr: *mut c_void) -> i32;
    }
    // pthread_attr_t is 56 bytes on x86_64 and 64 on aarch64 glibc.
    let mut attr = [0u64; 16];
    let attr_ptr = attr.as_mut_ptr() as *mut c_void;
    let mut addr: *mut c_void = std::ptr::null_mut();
    let mut size = 0usize;
    // SAFETY: `attr` is large and aligned enough for pthread_attr_t, and is
    // destroyed only after a successful getattr.
    unsafe {
        if pthread_getattr_np(pthread_self(), attr_ptr) != 0 {
            return None;
        }
        let ok = pthread_attr_getstack(attr_ptr, &mut addr, &mut size) == 0;
        pthread_attr_destroy(attr_ptr);
        if !ok {
            return None;
        }
    }
    (size > 0).then(|| (addr as usize, size))
}

#[cfg(not(any(target_vendor = "apple", all(target_os = "linux", target_env = "gnu"))))]
fn stack_bounds() -> Option<(usize, usize)> {
    None
}

fn limit() -> Option<(usize, usize)> {
    LIMIT.with(|l| {
        if let Some(known) = l.get() {
            return known;
        }
        let computed = stack_bounds().map(|(low, size)| (low, red_zone(size)));
        l.set(Some(computed));
        computed
    })
}

/// Stack bytes left before the red zone, or `None` if unknown here.
pub(crate) fn remaining() -> Option<usize> {
    let (low, red) = limit()?;
    Some(stack_pointer().saturating_sub(low).saturating_sub(red))
}

/// `true` when another loon call could overflow the native stack.
pub(crate) fn exhausted() -> bool {
    remaining() == Some(0)
}
