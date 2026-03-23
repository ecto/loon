//! Trace recorder — collects hot-loop traces from the register VM.
//!
//! The recorder instruments the VM's dispatch loop to detect hot back-edges
//! (loops). When a back-edge counter exceeds a threshold, recording starts.
//! A trace captures the linear sequence of operations executed in a single
//! loop iteration, along with observed value types at each step.
//!
//! This module collects traces only — it does not compile them. A future
//! enhancement would feed recorded traces into the Cranelift native backend
//! for profile-guided JIT compilation.

use std::collections::HashMap;

use super::{BinOp, BlockId, Built, FuncId, Lit, Op, Reg, UnOp};
use crate::eir::value64::Val;

// ─── Observed type tags ─────────────────────────────────────────────────────

/// Observed runtime type of a value, for specialization.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ObservedType {
    Int,
    Float,
    Bool,
    Ptr,
    Sym,
    Unit,
    Unknown,
}

impl ObservedType {
    /// Classify a NaN-boxed Val into an observed type.
    pub fn from_val(v: Val) -> Self {
        if v.is_int() {
            ObservedType::Int
        } else if v.is_float() {
            ObservedType::Float
        } else if v.is_bool() {
            ObservedType::Bool
        } else if v.is_unit() {
            ObservedType::Unit
        } else if v.is_ptr() {
            ObservedType::Ptr
        } else if v.is_sym() {
            ObservedType::Sym
        } else {
            ObservedType::Unknown
        }
    }
}

// ─── Trace entries ──────────────────────────────────────────────────────────

/// A single recorded step in a trace.
#[derive(Debug, Clone)]
pub struct TraceEntry {
    /// Which function we were in.
    pub func_id: u32,
    /// Which block within the function.
    pub block_id: u32,
    /// Instruction index within the block.
    pub op_index: usize,
    /// The kind of operation recorded.
    pub kind: TraceOpKind,
    /// Observed types of input operands.
    pub input_types: Vec<ObservedType>,
    /// Observed type of the output (if any).
    pub output_type: Option<ObservedType>,
}

/// Classifies the trace entry for potential specialization.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TraceOpKind {
    Lit,
    Mov,
    Upval,
    BinOp(BinOp),
    UnOp(UnOp),
    Call(u32), // func_id
    Invoke,
    Close(u32), // func_id
    Builtin(Built),
    CollectionOp, // Vec, Map, Set, Tup, Adt
    Field,
    Tag,
    Perform,
    /// Block terminator (jump, branch, return).
    Terminator(TraceTerminator),
}

/// Terminator classification for the trace.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TraceTerminator {
    Ret,
    Jmp(u32),     // target block
    BrTrue(u32),  // taken branch target
    BrFalse(u32), // taken branch target
    Switch(u16),  // matched tag
    Tail(u32),    // func_id
    TailInvoke,
    Recur,
}

// ─── Completed trace ────────────────────────────────────────────────────────

/// A complete recorded trace — one loop iteration from header to back-edge.
#[derive(Debug, Clone)]
pub struct Trace {
    /// The loop header: (func_id, block_id) where the trace starts.
    pub header: (u32, u32),
    /// The linear sequence of recorded operations.
    pub entries: Vec<TraceEntry>,
    /// How many times the header was hit before recording (hotness).
    pub hit_count: u32,
}

impl Trace {
    /// Number of operations in this trace.
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Whether the trace is empty.
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Check if all observed types in the trace are monomorphic (no Unknown).
    pub fn is_monomorphic(&self) -> bool {
        self.entries.iter().all(|e| {
            !e.input_types.contains(&ObservedType::Unknown)
                && e.output_type != Some(ObservedType::Unknown)
        })
    }
}

// ─── Trace cache ────────────────────────────────────────────────────────────

/// Cache of completed traces, keyed by loop header.
#[derive(Debug, Default)]
pub struct TraceCache {
    traces: HashMap<(u32, u32), Vec<Trace>>,
}

impl TraceCache {
    pub fn new() -> Self {
        Self {
            traces: HashMap::new(),
        }
    }

    /// Store a completed trace.
    pub fn insert(&mut self, trace: Trace) {
        self.traces.entry(trace.header).or_default().push(trace);
    }

    /// Get traces for a given loop header.
    pub fn get(&self, header: &(u32, u32)) -> Option<&[Trace]> {
        self.traces.get(header).map(|v| v.as_slice())
    }

    /// Total number of traces recorded.
    pub fn len(&self) -> usize {
        self.traces.values().map(|v| v.len()).sum()
    }

    /// Whether the cache is empty.
    pub fn is_empty(&self) -> bool {
        self.traces.is_empty()
    }

    /// Iterate over all (header, traces) pairs.
    pub fn iter(&self) -> impl Iterator<Item = (&(u32, u32), &[Trace])> {
        self.traces.iter().map(|(k, v)| (k, v.as_slice()))
    }
}

// ─── Trace recorder ────────────────────────────────────────────────────────

/// Default back-edge count before recording starts.
const DEFAULT_THRESHOLD: u32 = 50;

/// Maximum trace length before aborting recording.
const MAX_TRACE_LENGTH: usize = 4096;

/// The trace recorder. Attaches to the VM's dispatch loop to detect hot loops
/// and record their execution traces.
pub struct TraceRecorder {
    /// Whether we are currently recording a trace.
    recording: bool,
    /// The trace being recorded (empty when not recording).
    trace: Vec<TraceEntry>,
    /// Per-block back-edge counters: (func_id, block_id) -> hit count.
    counters: HashMap<(u32, u32), u32>,
    /// Hit count threshold to start recording.
    threshold: u32,
    /// The loop header we started recording at.
    recording_header: Option<(u32, u32)>,
    /// Completed traces.
    pub cache: TraceCache,
}

impl TraceRecorder {
    /// Create a new trace recorder with the default threshold.
    pub fn new() -> Self {
        Self {
            recording: false,
            trace: Vec::new(),
            counters: HashMap::new(),
            threshold: DEFAULT_THRESHOLD,
            recording_header: None,
            cache: TraceCache::new(),
        }
    }

    /// Create a trace recorder with a custom threshold.
    pub fn with_threshold(threshold: u32) -> Self {
        Self {
            threshold,
            ..Self::new()
        }
    }

    /// Whether we are currently recording.
    pub fn is_recording(&self) -> bool {
        self.recording
    }

    /// Get the current back-edge count for a block.
    pub fn counter(&self, func_id: u32, block_id: u32) -> u32 {
        self.counters
            .get(&(func_id, block_id))
            .copied()
            .unwrap_or(0)
    }

    /// Notify the recorder of a block transition. Returns `true` if recording
    /// just started (the VM should begin supplying trace data).
    ///
    /// A "back-edge" is detected when we jump to a block with a lower or equal
    /// ID within the same function (i.e., a loop).
    pub fn notify_block_entry(&mut self, func_id: u32, block_id: u32, from_block_id: u32) -> bool {
        // Detect back-edge: jumping to a block with lower/equal ID.
        let is_back_edge = block_id <= from_block_id;

        if !is_back_edge {
            return false;
        }

        // Increment counter for the target (loop header).
        let key = (func_id, block_id);
        let count = self.counters.entry(key).or_insert(0);
        *count += 1;
        let count_val = *count;

        if self.recording {
            // We're already recording. If we just hit the same header we
            // started at, the trace is complete — one full loop iteration.
            if self.recording_header == Some(key) {
                self.finish_trace(count_val);
                return false;
            }
            // Otherwise we're in a nested loop or different loop — keep recording.
            return false;
        }

        // Not recording yet. Check if we hit the threshold.
        if count_val >= self.threshold {
            // Already have a trace for this header? Skip.
            if self.cache.get(&key).is_some() {
                return false;
            }
            self.recording = true;
            self.recording_header = Some(key);
            self.trace.clear();
            return true;
        }

        false
    }

    /// Record an operation.
    pub fn record_op(
        &mut self,
        func_id: u32,
        block_id: u32,
        op_index: usize,
        op: &Op,
        regs: &dyn Fn(Reg) -> Val,
    ) {
        if !self.recording {
            return;
        }

        // Abort if trace is too long (likely not a simple loop).
        if self.trace.len() >= MAX_TRACE_LENGTH {
            self.abort_trace();
            return;
        }

        let (kind, inputs, output_reg) = classify_op(op);
        let input_types: Vec<ObservedType> = inputs
            .iter()
            .map(|r| ObservedType::from_val(regs(*r)))
            .collect();
        let output_type = output_reg.map(|r| ObservedType::from_val(regs(r)));

        self.trace.push(TraceEntry {
            func_id,
            block_id,
            op_index,
            kind,
            input_types,
            output_type,
        });
    }

    /// Record a terminator (for branch profiling).
    pub fn record_terminator(&mut self, func_id: u32, block_id: u32, terminator: TraceTerminator) {
        if !self.recording {
            return;
        }

        if self.trace.len() >= MAX_TRACE_LENGTH {
            self.abort_trace();
            return;
        }

        self.trace.push(TraceEntry {
            func_id,
            block_id,
            op_index: usize::MAX, // sentinel for terminator
            kind: TraceOpKind::Terminator(terminator),
            input_types: Vec::new(),
            output_type: None,
        });
    }

    /// Finalize the current trace and store it.
    fn finish_trace(&mut self, hit_count: u32) {
        if let Some(header) = self.recording_header.take() {
            let trace = Trace {
                header,
                entries: std::mem::take(&mut self.trace),
                hit_count,
            };
            self.cache.insert(trace);
        }
        self.recording = false;
    }

    /// Abort recording (trace too long or function return during recording).
    pub fn abort_trace(&mut self) {
        self.recording = false;
        self.recording_header = None;
        self.trace.clear();
    }

    /// Reset all counters and clear the cache.
    pub fn reset(&mut self) {
        self.recording = false;
        self.trace.clear();
        self.counters.clear();
        self.recording_header = None;
        self.cache = TraceCache::new();
    }
}

impl Default for TraceRecorder {
    fn default() -> Self {
        Self::new()
    }
}

// ─── Op classification ──────────────────────────────────────────────────────

/// Classify an Op into a TraceOpKind and extract input/output registers.
fn classify_op(op: &Op) -> (TraceOpKind, Vec<Reg>, Option<Reg>) {
    match op {
        Op::Lit(dst, _lit, _) => (TraceOpKind::Lit, vec![], Some(*dst)),
        Op::Mov(dst, src, _) => (TraceOpKind::Mov, vec![*src], Some(*dst)),
        Op::Upval(dst, _, _) => (TraceOpKind::Upval, vec![], Some(*dst)),
        Op::Bin(dst, binop, a, b, _) => (TraceOpKind::BinOp(*binop), vec![*a, *b], Some(*dst)),
        Op::Un(dst, unop, a, _) => (TraceOpKind::UnOp(*unop), vec![*a], Some(*dst)),
        Op::Call(dst, fid, args, _) => (TraceOpKind::Call(fid.0), args.clone(), Some(*dst)),
        Op::Invoke(dst, callee, args, _) => {
            let mut inputs = vec![*callee];
            inputs.extend(args);
            (TraceOpKind::Invoke, inputs, Some(*dst))
        }
        Op::Close(dst, fid, caps, _) => (TraceOpKind::Close(fid.0), caps.clone(), Some(*dst)),
        Op::Vec(dst, elems, _) => (TraceOpKind::CollectionOp, elems.clone(), Some(*dst)),
        Op::Map(dst, pairs, _) => {
            let inputs: Vec<Reg> = pairs.iter().flat_map(|(k, v)| [*k, *v]).collect();
            (TraceOpKind::CollectionOp, inputs, Some(*dst))
        }
        Op::Set(dst, elems, _) => (TraceOpKind::CollectionOp, elems.clone(), Some(*dst)),
        Op::Tup(dst, elems, _) => (TraceOpKind::CollectionOp, elems.clone(), Some(*dst)),
        Op::Adt(dst, _, fields, _) => (TraceOpKind::CollectionOp, fields.clone(), Some(*dst)),
        Op::Field(dst, src, _, _) => (TraceOpKind::Field, vec![*src], Some(*dst)),
        Op::Tag(dst, src, _) => (TraceOpKind::Tag, vec![*src], Some(*dst)),
        Op::Perform(dst, _, _, args, ev, _) => {
            let mut inputs = args.clone();
            if let Some(e) = ev {
                inputs.push(*e);
            }
            (TraceOpKind::Perform, inputs, Some(*dst))
        }
        Op::Builtin(dst, built, args, _) => {
            (TraceOpKind::Builtin(*built), args.clone(), Some(*dst))
        }
        Op::PushHandler(_, _, _, _) => (TraceOpKind::Perform, vec![], None),
        Op::PopHandler(_) => (TraceOpKind::Perform, vec![], None),
    }
}

// ─── Tests ──────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn counter_increments_on_back_edge() {
        let mut rec = TraceRecorder::with_threshold(100);
        // Forward edge: block 0 -> block 1 (no counter bump)
        rec.notify_block_entry(0, 1, 0);
        assert_eq!(rec.counter(0, 1), 0);

        // Back edge: block 1 -> block 0 (counter bumps)
        rec.notify_block_entry(0, 0, 1);
        assert_eq!(rec.counter(0, 0), 1);

        // Another back edge
        rec.notify_block_entry(0, 0, 1);
        assert_eq!(rec.counter(0, 0), 2);
    }

    #[test]
    fn recording_starts_at_threshold() {
        let mut rec = TraceRecorder::with_threshold(3);

        // Hit the loop header 3 times (threshold).
        assert!(!rec.notify_block_entry(0, 0, 1)); // count=1
        assert!(!rec.notify_block_entry(0, 0, 1)); // count=2
        assert!(rec.notify_block_entry(0, 0, 1)); // count=3, starts recording
        assert!(rec.is_recording());
    }

    #[test]
    fn trace_completes_on_loop_back_edge() {
        let mut rec = TraceRecorder::with_threshold(2);

        // Heat up the loop.
        rec.notify_block_entry(0, 0, 1); // count=1
        rec.notify_block_entry(0, 0, 1); // count=2, starts recording
        assert!(rec.is_recording());

        // Record some ops in the loop body.
        let lit_op = Op::Lit(Reg(0), Lit::Int(42), crate::syntax::Span::ZERO);
        rec.record_op(0, 0, 0, &lit_op, &|_| Val::int(42));
        assert_eq!(rec.trace.len(), 1);

        // Record a terminator.
        rec.record_terminator(0, 0, TraceTerminator::Jmp(1));

        // Record an op in block 1.
        let add_op = Op::Bin(
            Reg(2),
            BinOp::Add,
            Reg(0),
            Reg(1),
            crate::syntax::Span::ZERO,
        );
        rec.record_op(0, 1, 0, &add_op, &|r| {
            if r.0 == 0 {
                Val::int(42)
            } else {
                Val::int(1)
            }
        });

        // Back-edge: block 1 -> block 0 (completes the trace).
        rec.notify_block_entry(0, 0, 1);
        assert!(!rec.is_recording());

        // Trace should be in the cache.
        assert_eq!(rec.cache.len(), 1);
        let traces = rec.cache.get(&(0, 0)).unwrap();
        assert_eq!(traces.len(), 1);
        assert_eq!(traces[0].header, (0, 0));
        assert_eq!(traces[0].entries.len(), 3);
    }

    #[test]
    fn observed_type_classification() {
        assert_eq!(ObservedType::from_val(Val::int(42)), ObservedType::Int);
        assert_eq!(
            ObservedType::from_val(Val::float(3.14)),
            ObservedType::Float
        );
        assert_eq!(ObservedType::from_val(Val::bool(true)), ObservedType::Bool);
        assert_eq!(ObservedType::from_val(Val::UNIT), ObservedType::Unit);
        assert_eq!(ObservedType::from_val(Val::sym(0)), ObservedType::Sym);
        assert_eq!(ObservedType::from_val(Val::ptr(0)), ObservedType::Ptr);
    }

    #[test]
    fn trace_monomorphism_check() {
        let mono_trace = Trace {
            header: (0, 0),
            entries: vec![TraceEntry {
                func_id: 0,
                block_id: 0,
                op_index: 0,
                kind: TraceOpKind::BinOp(BinOp::Add),
                input_types: vec![ObservedType::Int, ObservedType::Int],
                output_type: Some(ObservedType::Int),
            }],
            hit_count: 50,
        };
        assert!(mono_trace.is_monomorphic());

        let poly_trace = Trace {
            header: (0, 0),
            entries: vec![TraceEntry {
                func_id: 0,
                block_id: 0,
                op_index: 0,
                kind: TraceOpKind::BinOp(BinOp::Add),
                input_types: vec![ObservedType::Unknown, ObservedType::Int],
                output_type: Some(ObservedType::Int),
            }],
            hit_count: 50,
        };
        assert!(!poly_trace.is_monomorphic());
    }

    #[test]
    fn abort_on_max_length() {
        let mut rec = TraceRecorder::with_threshold(1);
        rec.notify_block_entry(0, 0, 1); // starts recording
        assert!(rec.is_recording());

        // Fill up to max length.
        let lit_op = Op::Lit(Reg(0), Lit::Int(1), crate::syntax::Span::ZERO);
        for i in 0..MAX_TRACE_LENGTH {
            rec.record_op(0, 0, i, &lit_op, &|_| Val::int(1));
        }
        // Next record should abort.
        rec.record_op(0, 0, MAX_TRACE_LENGTH, &lit_op, &|_| Val::int(1));
        assert!(!rec.is_recording());
        assert!(rec.cache.is_empty());
    }

    #[test]
    fn no_duplicate_traces() {
        let mut rec = TraceRecorder::with_threshold(2);

        // First trace.
        rec.notify_block_entry(0, 0, 1); // count=1
        rec.notify_block_entry(0, 0, 1); // count=2, starts recording
        rec.notify_block_entry(0, 0, 1); // completes trace
        assert_eq!(rec.cache.len(), 1);

        // Same header should not start another recording.
        let started = rec.notify_block_entry(0, 0, 1);
        assert!(!started);
        assert_eq!(rec.cache.len(), 1);
    }

    #[test]
    fn reset_clears_everything() {
        let mut rec = TraceRecorder::with_threshold(2);
        rec.notify_block_entry(0, 0, 1);
        rec.notify_block_entry(0, 0, 1);
        rec.notify_block_entry(0, 0, 1); // complete trace
        assert_eq!(rec.cache.len(), 1);

        rec.reset();
        assert_eq!(rec.counter(0, 0), 0);
        assert!(rec.cache.is_empty());
        assert!(!rec.is_recording());
    }

    #[test]
    fn trace_cache_iteration() {
        let mut cache = TraceCache::new();
        cache.insert(Trace {
            header: (0, 0),
            entries: vec![],
            hit_count: 50,
        });
        cache.insert(Trace {
            header: (1, 0),
            entries: vec![],
            hit_count: 100,
        });

        assert_eq!(cache.len(), 2);
        let headers: Vec<(u32, u32)> = cache.iter().map(|(h, _)| *h).collect();
        assert!(headers.contains(&(0, 0)));
        assert!(headers.contains(&(1, 0)));
    }
}
