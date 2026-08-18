//! Placement: where a kernel runs, and what it cost to put it there.
//!
//! Loon expresses placement as an effect. A program performs `Place.run` and a
//! handler decides what that means — run it here, run it across threads, ship
//! it to a device, record it, or pretend. Nothing in the program changes when
//! the answer changes.
//!
//! This module holds the parts that are the same whoever answers: the record
//! of what happened ([`PlaceEvent`]) and the running totals ([`PlaceStats`]).
//! Transfer accounting is not an afterthought here. The interesting question
//! about an offloaded program is almost never "was the kernel fast" — it is
//! "how many times did these bytes cross the boundary, and did they need to".
//! Making that observable is what lets a residency policy be written as a
//! handler and then *checked*, rather than assumed.

use super::layout::DType;

/// What a placement backend did.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EventKind {
    /// Bytes moved toward the device.
    Upload,
    /// Bytes moved back to the host.
    Download,
    /// A kernel ran.
    Launch,
    /// An argument was already resident, so no transfer happened. Counting
    /// the transfers that *didn't* occur is what makes a residency policy
    /// legible: the win shows up as a number, not as a wall-clock guess.
    ResidentHit,
    /// Device memory was reserved.
    Alloc,
    /// Device memory was released.
    Free,
}

impl EventKind {
    pub fn name(self) -> &'static str {
        match self {
            EventKind::Upload => "upload",
            EventKind::Download => "download",
            EventKind::Launch => "launch",
            EventKind::ResidentHit => "resident-hit",
            EventKind::Alloc => "alloc",
            EventKind::Free => "free",
        }
    }
}

/// One thing that happened during placement.
#[derive(Debug, Clone)]
pub struct PlaceEvent {
    pub kind: EventKind,
    /// Kernel name, for a launch.
    pub kernel: Option<String>,
    /// Which argument this concerns, for a transfer.
    pub arg: Option<u16>,
    /// Element type of the buffer involved.
    pub dtype: Option<DType>,
    /// Bytes moved.
    pub bytes: u64,
    /// Work items, for a launch.
    pub items: u64,
    /// Which backend answered.
    pub device: &'static str,
}

/// Running totals across a program.
#[derive(Debug, Clone, Default)]
pub struct PlaceStats {
    pub launches: u64,
    pub work_items: u64,
    pub uploads: u64,
    pub downloads: u64,
    pub bytes_in: u64,
    pub bytes_out: u64,
    pub resident_hits: u64,
    pub events: Vec<PlaceEvent>,
}

impl PlaceStats {
    pub fn record(&mut self, event: PlaceEvent) {
        match event.kind {
            EventKind::Upload => {
                self.uploads += 1;
                self.bytes_in += event.bytes;
            }
            EventKind::Download => {
                self.downloads += 1;
                self.bytes_out += event.bytes;
            }
            EventKind::Launch => {
                self.launches += 1;
                self.work_items += event.items;
            }
            EventKind::ResidentHit => self.resident_hits += 1,
            EventKind::Alloc | EventKind::Free => {}
        }
        self.events.push(event);
    }

    /// A one-line summary in the shape offload papers report: how many
    /// transfers, how many bytes, how many launches.
    pub fn summary(&self) -> String {
        format!(
            "{} launches over {} work items; {} uploads ({}), {} downloads ({}), {} resident hits",
            self.launches,
            self.work_items,
            self.uploads,
            human_bytes(self.bytes_in),
            self.downloads,
            human_bytes(self.bytes_out),
            self.resident_hits,
        )
    }

    /// A multi-line table for `--place-stats`.
    pub fn table(&self) -> String {
        let mut out = String::new();
        out.push_str("placement\n");
        out.push_str(&format!("  launches       {}\n", self.launches));
        out.push_str(&format!("  work items     {}\n", self.work_items));
        out.push_str(&format!(
            "  uploads        {} ({})\n",
            self.uploads,
            human_bytes(self.bytes_in)
        ));
        out.push_str(&format!(
            "  downloads      {} ({})\n",
            self.downloads,
            human_bytes(self.bytes_out)
        ));
        out.push_str(&format!("  resident hits  {}\n", self.resident_hits));
        out
    }
}

// ─── A device with its own memory ──────────────────────────────────────────

/// Where kernels run.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Mode {
    /// Right here. There is one memory, so nothing is ever transferred.
    #[default]
    Cpu,
    /// A discrete device with separate memory.
    ///
    /// The arithmetic still happens on the host — this is not a GPU — but the
    /// *bookkeeping* is real: a buffer must be uploaded before a kernel can
    /// use it, and results must be downloaded before the host can read them.
    /// That makes the cost of a placement policy measurable, and measurable is
    /// the whole argument. A policy whose benefit you cannot count is a story.
    Device,
}

impl Mode {
    pub fn parse(s: &str) -> Option<Mode> {
        match s {
            "cpu" | "serial" | "here" => Some(Mode::Cpu),
            "device" | "sim" => Some(Mode::Device),
            _ => None,
        }
    }

    pub fn name(self) -> &'static str {
        match self {
            Mode::Cpu => "cpu",
            Mode::Device => "device",
        }
    }
}

/// Device-side memory: which buffers are resident, and which hold results the
/// host has not seen yet.
///
/// Buffers are identified by their heap slot. Nothing is copied — the point is
/// to account for the copies a real device would need, not to simulate its
/// arithmetic.
#[derive(Debug, Clone, Default)]
pub struct Device {
    resident: std::collections::HashSet<usize>,
    dirty: std::collections::HashSet<usize>,
    /// Buffers a handler asked to keep resident across launches.
    pinned: std::collections::HashSet<usize>,
}

impl Device {
    pub fn is_resident(&self, id: usize) -> bool {
        self.resident.contains(&id)
    }

    pub fn is_dirty(&self, id: usize) -> bool {
        self.dirty.contains(&id)
    }

    pub fn is_pinned(&self, id: usize) -> bool {
        self.pinned.contains(&id)
    }

    pub fn mark_resident(&mut self, id: usize) {
        self.resident.insert(id);
    }

    pub fn mark_dirty(&mut self, id: usize) {
        self.dirty.insert(id);
    }

    pub fn clear_dirty(&mut self, id: usize) {
        self.dirty.remove(&id);
    }

    /// Ask that a buffer survive eviction.
    ///
    /// Deliberately does *not* make it resident: pinning says "keep this once
    /// it is here", not "it is here already". The first launch that uses it
    /// still pays for the upload. Otherwise a residency policy would look free
    /// in the accounting by declaring itself so, which is exactly the kind of
    /// measurement that flatters a design instead of testing it.
    pub fn pin(&mut self, id: usize) {
        self.pinned.insert(id);
    }

    pub fn unpin(&mut self, id: usize) {
        self.pinned.remove(&id);
    }

    /// Drop every unpinned buffer.
    ///
    /// This is what makes a transfer-per-launch policy the *default* rather
    /// than a strawman: without someone deciding otherwise, the device does
    /// not assume a buffer will be wanted again. Deciding otherwise is exactly
    /// what a residency handler does, and pinning is how it says so.
    pub fn evict_unpinned(&mut self) -> Vec<usize> {
        let evicted: Vec<usize> = self
            .resident
            .iter()
            .copied()
            .filter(|id| !self.pinned.contains(id))
            .collect();
        for id in &evicted {
            self.resident.remove(id);
        }
        evicted
    }

    pub fn resident_count(&self) -> usize {
        self.resident.len()
    }
}

/// Bytes in a unit a person can read at a glance.
pub fn human_bytes(n: u64) -> String {
    const KB: u64 = 1024;
    const MB: u64 = KB * 1024;
    const GB: u64 = MB * 1024;
    if n >= GB {
        format!("{:.1} GB", n as f64 / GB as f64)
    } else if n >= MB {
        format!("{:.1} MB", n as f64 / MB as f64)
    } else if n >= KB {
        format!("{:.1} KB", n as f64 / KB as f64)
    } else {
        format!("{n} B")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pinning_does_not_make_a_buffer_resident_for_free() {
        // The first use still has to upload it; pinning only says it should
        // stay afterwards.
        let mut d = Device::default();
        d.pin(3);
        assert!(!d.is_resident(3), "pinning is not a transfer");
    }

    #[test]
    fn an_unpinned_buffer_does_not_survive_a_launch() {
        let mut d = Device::default();
        d.mark_resident(1);
        d.mark_resident(2);
        d.pin(2);
        let evicted = d.evict_unpinned();
        assert_eq!(evicted, vec![1]);
        assert!(!d.is_resident(1), "unpinned buffers are dropped");
        assert!(d.is_resident(2), "pinned buffers stay");
    }

    #[test]
    fn unpinning_lets_a_buffer_be_evicted_again() {
        let mut d = Device::default();
        d.mark_resident(7);
        d.pin(7);
        assert!(d.evict_unpinned().is_empty());
        d.unpin(7);
        assert_eq!(d.evict_unpinned(), vec![7]);
    }

    #[test]
    fn mode_names_round_trip() {
        for m in [Mode::Cpu, Mode::Device] {
            assert_eq!(Mode::parse(m.name()), Some(m));
        }
        assert_eq!(Mode::parse("sim"), Some(Mode::Device));
        assert_eq!(Mode::parse("nonsense"), None);
    }

    fn ev(kind: EventKind, bytes: u64, items: u64) -> PlaceEvent {
        PlaceEvent {
            kind,
            kernel: None,
            arg: None,
            dtype: None,
            bytes,
            items,
            device: "test",
        }
    }

    #[test]
    fn totals_follow_the_events() {
        let mut s = PlaceStats::default();
        s.record(ev(EventKind::Upload, 100, 0));
        s.record(ev(EventKind::Upload, 40, 0));
        s.record(ev(EventKind::Launch, 0, 1024));
        s.record(ev(EventKind::Download, 60, 0));
        s.record(ev(EventKind::ResidentHit, 0, 0));

        assert_eq!(s.uploads, 2);
        assert_eq!(s.bytes_in, 140);
        assert_eq!(s.downloads, 1);
        assert_eq!(s.bytes_out, 60);
        assert_eq!(s.launches, 1);
        assert_eq!(s.work_items, 1024);
        assert_eq!(s.resident_hits, 1);
        assert_eq!(s.events.len(), 5);
    }

    #[test]
    fn byte_counts_read_like_prose() {
        assert_eq!(human_bytes(512), "512 B");
        assert_eq!(human_bytes(2048), "2.0 KB");
        assert_eq!(human_bytes(3 * 1024 * 1024), "3.0 MB");
        assert_eq!(human_bytes(2 * 1024 * 1024 * 1024), "2.0 GB");
    }

    #[test]
    fn a_quiet_run_reports_zeroes_rather_than_nothing() {
        let s = PlaceStats::default();
        assert!(s.summary().contains("0 launches"));
        assert!(s.table().contains("resident hits  0"));
    }
}
