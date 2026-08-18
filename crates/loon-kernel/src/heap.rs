//! The kernel heap: a first-fit free-list allocator over the RAM left above
//! the image.
//!
//! Single-hart and non-reentrant, which is why the lock is a bare `Cell`
//! guard rather than a real spinlock — there is nothing to race with yet.
//! Preemption lands before SMP does, and that is when this needs revisiting.

use core::alloc::{GlobalAlloc, Layout};
use core::cell::UnsafeCell;
use core::ptr;

/// A free region. Stored in the first bytes of the region itself.
#[repr(C)]
struct Block {
    size: usize,
    next: *mut Block,
}

const MIN_BLOCK: usize = core::mem::size_of::<Block>();

pub struct Heap {
    free: UnsafeCell<*mut Block>,
    /// Allocation count. Wall-clock under emulation is noisy; this is not,
    /// so it is what optimisation work should be judged against.
    allocs: core::sync::atomic::AtomicU64,
}

// Single hart, interrupts off: no concurrent access exists.
unsafe impl Sync for Heap {}

impl Heap {
    pub const fn new() -> Self {
        Heap {
            free: UnsafeCell::new(ptr::null_mut()),
            allocs: core::sync::atomic::AtomicU64::new(0),
        }
    }

    pub fn allocs(&self) -> u64 {
        self.allocs.load(core::sync::atomic::Ordering::Relaxed)
    }

    /// # Safety
    /// `start..start+size` must be untouched, writable RAM that outlives all
    /// allocations, and this must be called exactly once.
    pub unsafe fn init(&self, start: usize, size: usize) {
        let block = start as *mut Block;
        (*block).size = size;
        (*block).next = ptr::null_mut();
        *self.free.get() = block;
    }

    /// Splice a region back into the address-ordered free list, coalescing
    /// with whichever neighbours it now touches.
    unsafe fn insert(&self, region: *mut Block) {
        let mut prev: *mut Block = ptr::null_mut();
        let mut cur = *self.free.get();
        while !cur.is_null() && (cur as usize) < (region as usize) {
            prev = cur;
            cur = (*cur).next;
        }

        (*region).next = cur;
        if prev.is_null() {
            *self.free.get() = region;
        } else {
            (*prev).next = region;
        }

        // Coalesce forward, then backward.
        if !cur.is_null() && (region as usize) + (*region).size == cur as usize {
            (*region).size += (*cur).size;
            (*region).next = (*cur).next;
        }
        if !prev.is_null() && (prev as usize) + (*prev).size == region as usize {
            (*prev).size += (*region).size;
            (*prev).next = (*region).next;
        }
    }
}

unsafe impl GlobalAlloc for Heap {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        self.allocs
            .fetch_add(1, core::sync::atomic::Ordering::Relaxed);
        let align = layout.align().max(core::mem::align_of::<Block>());
        let size = align_up(layout.size().max(MIN_BLOCK), core::mem::align_of::<Block>());

        let mut prev: *mut Block = ptr::null_mut();
        let mut cur = *self.free.get();
        while !cur.is_null() {
            let base = cur as usize;
            let start = align_up(base, align);
            let end = start + size;

            if end <= base + (*cur).size {
                let head = start - base;
                let tail = base + (*cur).size - end;

                // Unlink, then give back whatever is left at either end —
                // but only if the remainder can hold a link of its own.
                let next = (*cur).next;
                if prev.is_null() {
                    *self.free.get() = next;
                } else {
                    (*prev).next = next;
                }
                if tail >= MIN_BLOCK {
                    let t = end as *mut Block;
                    (*t).size = tail;
                    self.insert(t);
                }
                if head >= MIN_BLOCK {
                    (*cur).size = head;
                    self.insert(cur);
                }
                return start as *mut u8;
            }
            prev = cur;
            cur = (*cur).next;
        }
        ptr::null_mut()
    }

    unsafe fn dealloc(&self, p: *mut u8, layout: Layout) {
        let size = align_up(layout.size().max(MIN_BLOCK), core::mem::align_of::<Block>());
        let block = p as *mut Block;
        (*block).size = size;
        self.insert(block);
    }
}

fn align_up(n: usize, align: usize) -> usize {
    (n + align - 1) & !(align - 1)
}
