//! Evidence IR — the pivot between Loon's frontend and pluggable backends.
//!
//! Flat, SSA-like, block-based. Effects compiled to evidence-passing.
//! Every backend (Register VM, WASM, Cranelift) lowers from this IR.

pub mod backend;
pub mod lower;
#[cfg(feature = "native")]
pub mod native;
pub mod trace;
pub mod value64;
pub mod vm;
pub mod wasm;

use crate::syntax::Span;

// ─── Indices ───────────────────────────────────────────────────────────────

/// Register: a value slot within a function. Defined exactly once (SSA).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Reg(pub u32);

/// Index into the module's function table.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FuncId(pub u32);

/// Index into the module's string constant pool.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StringId(pub u32);

/// Index into a function's block list.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(pub u32);

// ─── Module ────────────────────────────────────────────────────────────────

/// A complete compilation unit.
#[derive(Debug)]
pub struct Module {
    pub funcs: Vec<Func>,
    pub strings: Vec<String>,
    pub ctors: Vec<Ctor>,
    pub entry: FuncId,
}

/// ADT constructor definition.
#[derive(Debug, Clone)]
pub struct Ctor {
    pub name: String,
    pub tag: u16,
    pub arity: u16,
}

// ─── Function ──────────────────────────────────────────────────────────────

#[derive(Debug)]
pub struct Func {
    pub id: FuncId,
    pub name: Option<String>,
    pub params: Vec<Ty>,
    pub ret: Ty,
    /// Implicit handler function-pointer parameters (evidence-passing).
    pub evidence: Vec<Evidence>,
    /// Captured values from enclosing scope (for closures).
    pub captures: Vec<Capture>,
    /// Basic blocks. Block 0 is the entry.
    pub blocks: Vec<Block>,
    pub span: Span,
    pub is_closure: bool,
}

/// Evidence parameter — a handler function pointer threaded through calls.
#[derive(Debug, Clone)]
pub struct Evidence {
    pub effect: StringId,
    pub op: StringId,
    /// Index in the function's combined parameter list.
    pub slot: u16,
}

/// A captured value from an enclosing scope.
#[derive(Debug, Clone)]
pub struct Capture {
    pub name: String,
    pub ty: Ty,
    pub mode: CaptureMode,
}

/// How a value is captured by a closure.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CaptureMode {
    /// Value is copied (cheap, for primitives and shared references).
    Copy,
    /// Value is moved (ownership transferred into the closure).
    Move,
}

/// Static type tag — used by backends for specialization.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Ty {
    Int,
    Float,
    Bool,
    Str,
    Keyword,
    Fn,
    Vec,
    Map,
    Set,
    Tuple,
    Adt,
    Unit,
    /// Not statically known — must check at runtime.
    Any,
}

// ─── Block ─────────────────────────────────────────────────────────────────

/// A basic block: a straight-line sequence of `Op`s ending with a terminator.
#[derive(Debug)]
pub struct Block {
    pub id: BlockId,
    /// Phi-like parameters — values passed in by predecessor branches.
    pub params: Vec<Reg>,
    /// Instructions executed in order.
    pub ops: Vec<Op>,
    /// How this block transfers control.
    pub end: End,
}

// ─── Instructions ──────────────────────────────────────────────────────────

/// A single operation. Each produces a value in `dst` (the first Reg).
#[derive(Debug, Clone)]
pub enum Op {
    /// Load a literal constant.
    Lit(Reg, Lit, Span),
    /// Copy a register.
    Mov(Reg, Reg, Span),
    /// Load a closure upvalue by index.
    Upval(Reg, u16, Span),
    /// Binary operation.
    Bin(Reg, BinOp, Reg, Reg, Span),
    /// Unary operation.
    Un(Reg, UnOp, Reg, Span),
    /// Call a known function by FuncId.
    Call(Reg, FuncId, Vec<Reg>, Span),
    /// Indirect call (closure or function pointer).
    Invoke(Reg, Reg, Vec<Reg>, Span),
    /// Create a closure: function + captured values.
    Close(Reg, FuncId, Vec<Reg>, Span),
    /// Construct a vector.
    Vec(Reg, Vec<Reg>, Span),
    /// Construct a map (alternating key, value).
    Map(Reg, Vec<(Reg, Reg)>, Span),
    /// Construct a set.
    Set(Reg, Vec<Reg>, Span),
    /// Construct a tuple.
    Tup(Reg, Vec<Reg>, Span),
    /// Construct an ADT value: tag + field values.
    Adt(Reg, u16, Vec<Reg>, Span),
    /// Access a field on a value.
    Field(Reg, Reg, Selector, Span),
    /// Extract ADT tag (for match dispatch).
    Tag(Reg, Reg, Span),
    /// Perform an effect operation.
    /// If `evidence` is Some, this compiles to a direct call (zero-cost).
    /// If None, the backend does a dynamic handler lookup.
    Perform(Reg, StringId, StringId, Vec<Reg>, Option<Reg>, Span),
    /// Call a built-in runtime operation.
    Builtin(Reg, Built, Vec<Reg>, Span),
    /// Push a dynamic effect handler (handler_closure, effect_sid, op_sid).
    PushHandler(Reg, StringId, StringId, Span),
    /// Pop a dynamic effect handler.
    PopHandler(Span),
}

impl Op {
    /// The destination register (first Reg in each variant).
    pub fn dst(&self) -> Reg {
        match self {
            Op::Lit(r, ..)
            | Op::Mov(r, ..)
            | Op::Upval(r, ..)
            | Op::Bin(r, ..)
            | Op::Un(r, ..)
            | Op::Call(r, ..)
            | Op::Invoke(r, ..)
            | Op::Close(r, ..)
            | Op::Vec(r, ..)
            | Op::Map(r, ..)
            | Op::Set(r, ..)
            | Op::Tup(r, ..)
            | Op::Adt(r, ..)
            | Op::Field(r, ..)
            | Op::Tag(r, ..)
            | Op::Perform(r, ..)
            | Op::Builtin(r, ..)
            | Op::PushHandler(r, ..) => *r,
            Op::PopHandler(_) => Reg(0), // no destination
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Op::Lit(_, _, s)
            | Op::Mov(_, _, s)
            | Op::Upval(_, _, s)
            | Op::Bin(_, _, _, _, s)
            | Op::Un(_, _, _, s)
            | Op::Call(_, _, _, s)
            | Op::Invoke(_, _, _, s)
            | Op::Close(_, _, _, s)
            | Op::Vec(_, _, s)
            | Op::Map(_, _, s)
            | Op::Set(_, _, s)
            | Op::Tup(_, _, s)
            | Op::Adt(_, _, _, s)
            | Op::Field(_, _, _, s)
            | Op::Tag(_, _, s)
            | Op::Perform(_, _, _, _, _, s)
            | Op::Builtin(_, _, _, s)
            | Op::PushHandler(_, _, _, s)
            | Op::PopHandler(s) => *s,
        }
    }
}

/// How a block ends.
#[derive(Debug, Clone)]
pub enum End {
    /// Return from function.
    Ret(Reg),
    /// Unconditional jump.
    Jmp(BlockId, Vec<Reg>),
    /// Conditional branch.
    Br(Reg, BlockId, BlockId),
    /// Multi-way branch (match on ADT tag).
    Switch(Reg, Vec<(u16, BlockId)>, BlockId),
    /// Tail call to known function.
    Tail(FuncId, Vec<Reg>),
    /// Tail call to closure/function pointer.
    TailInvoke(Reg, Vec<Reg>),
    /// Self-recursion (jump to block 0 with new params).
    Recur(Vec<Reg>),
    /// Unreachable (after exhaustive match).
    Trap,
}

// ─── Primitives ────────────────────────────────────────────────────────────

/// A literal constant embedded in the IR.
#[derive(Debug, Clone)]
pub enum Lit {
    Int(i64),
    Float(f64),
    Bool(bool),
    /// String literal, stored in the module's string pool.
    Str(StringId),
    /// Keyword literal (e.g. `:foo`), stored in the string pool.
    Keyword(StringId),
    Unit,
}

/// Binary operator tag.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    And,
    Or,
    Concat,
}

/// Unary operator tag.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOp {
    Neg,
    Not,
}

/// Field selector for record/tuple/map access.
#[derive(Debug, Clone)]
pub enum Selector {
    /// Tuple or ADT field by position.
    Index(u16),
    /// Map key (string constant).
    Key(StringId),
    /// Named record field.
    Name(StringId),
}

/// Built-in runtime operation tag (intrinsics implemented in the VM).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Built {
    Println,
    Print,
    Str,
    Len,
    Get,
    Conj,
    Cons,
    Assoc,
    Merge,
    Range,
    Map,
    Filter,
    Reduce,
    Each,
    FlatMap,
    Keys,
    Vals,
    Nth,
    Take,
    Drop,
    Slice,
    Contains,
    Join,
    Trim,
    Split,
    Sort,
    Reverse,
    Flatten,
    Zip,
    Chunk,
    Any,
    All,
    Sum,
    Min,
    Max,
    Int,
    Float,
    IntoMap,
    GroupBy,
    Collect,
    StartsWith,
    EndsWith,
    Replace,
    Uppercase,
    Lowercase,
    IndexOf,
    CharAt,
    Substring,
    Not,
    Empty,
    Fold,
    Update,
    Entries,
    SortBy,
    Unit,
    Magnitude,
    Or,
    Abs,
    First,
    Last,
    Find,
    Keyword,
    KeywordizeKeys,
    AssertEq,
    Concat,
}

impl Built {
    /// Resolve a source-level name to its builtin tag.
    ///
    /// This is the **single source of truth** for which names denote runtime
    /// builtins. Every frontend consults it: EIR lowering (→ VM / native /
    /// wasm backends) and the legacy direct WASM `codegen/`. Adding a builtin
    /// means adding one variant and one arm here — nowhere else decides what
    /// counts as a builtin.
    pub fn from_name(name: &str) -> Option<Built> {
        Some(match name {
            "println" => Built::Println,
            "print" => Built::Print,
            "str" => Built::Str,
            "len" => Built::Len,
            "get" => Built::Get,
            "conj" => Built::Conj,
            "cons" => Built::Cons,
            "assoc" => Built::Assoc,
            "merge" => Built::Merge,
            "range" => Built::Range,
            "map" => Built::Map,
            "filter" => Built::Filter,
            "reduce" => Built::Reduce,
            "each" => Built::Each,
            "flat-map" => Built::FlatMap,
            "keys" => Built::Keys,
            "vals" => Built::Vals,
            "nth" => Built::Nth,
            "take" => Built::Take,
            "drop" => Built::Drop,
            "slice" => Built::Slice,
            "contains?" => Built::Contains,
            "join" => Built::Join,
            "trim" => Built::Trim,
            "split" => Built::Split,
            "sort" => Built::Sort,
            "reverse" => Built::Reverse,
            "flatten" => Built::Flatten,
            "zip" => Built::Zip,
            "chunk" => Built::Chunk,
            "any?" => Built::Any,
            "all?" => Built::All,
            "sum" => Built::Sum,
            "min" => Built::Min,
            "max" => Built::Max,
            "int" => Built::Int,
            "float" => Built::Float,
            "into-map" => Built::IntoMap,
            "group-by" => Built::GroupBy,
            "collect" => Built::Collect,
            "starts-with?" => Built::StartsWith,
            "ends-with?" => Built::EndsWith,
            "replace" => Built::Replace,
            "uppercase" => Built::Uppercase,
            "lowercase" => Built::Lowercase,
            "index-of" => Built::IndexOf,
            "char-at" => Built::CharAt,
            "substring" => Built::Substring,
            "not" => Built::Not,
            "empty?" => Built::Empty,
            "fold" => Built::Fold,
            "update" => Built::Update,
            "entries" => Built::Entries,
            "sort-by" => Built::SortBy,
            "unit" => Built::Unit,
            "magnitude" => Built::Magnitude,
            "or" => Built::Or,
            "abs" => Built::Abs,
            "first" => Built::First,
            "last" => Built::Last,
            "find" => Built::Find,
            "keyword" => Built::Keyword,
            "keywordize-keys" => Built::KeywordizeKeys,
            "assert-eq" => Built::AssertEq,
            "concat" => Built::Concat,
            _ => return None,
        })
    }

    /// The canonical source-level name for this builtin — the inverse of
    /// [`Built::from_name`]. Exhaustive by construction: adding a variant to
    /// `Built` is a compile error until it is named here.
    pub fn name(self) -> &'static str {
        match self {
            Built::Println => "println",
            Built::Print => "print",
            Built::Str => "str",
            Built::Len => "len",
            Built::Get => "get",
            Built::Conj => "conj",
            Built::Cons => "cons",
            Built::Assoc => "assoc",
            Built::Merge => "merge",
            Built::Range => "range",
            Built::Map => "map",
            Built::Filter => "filter",
            Built::Reduce => "reduce",
            Built::Each => "each",
            Built::FlatMap => "flat-map",
            Built::Keys => "keys",
            Built::Vals => "vals",
            Built::Nth => "nth",
            Built::Take => "take",
            Built::Drop => "drop",
            Built::Slice => "slice",
            Built::Contains => "contains?",
            Built::Join => "join",
            Built::Trim => "trim",
            Built::Split => "split",
            Built::Sort => "sort",
            Built::Reverse => "reverse",
            Built::Flatten => "flatten",
            Built::Zip => "zip",
            Built::Chunk => "chunk",
            Built::Any => "any?",
            Built::All => "all?",
            Built::Sum => "sum",
            Built::Min => "min",
            Built::Max => "max",
            Built::Int => "int",
            Built::Float => "float",
            Built::IntoMap => "into-map",
            Built::GroupBy => "group-by",
            Built::Collect => "collect",
            Built::StartsWith => "starts-with?",
            Built::EndsWith => "ends-with?",
            Built::Replace => "replace",
            Built::Uppercase => "uppercase",
            Built::Lowercase => "lowercase",
            Built::IndexOf => "index-of",
            Built::CharAt => "char-at",
            Built::Substring => "substring",
            Built::Not => "not",
            Built::Empty => "empty?",
            Built::Fold => "fold",
            Built::Update => "update",
            Built::Entries => "entries",
            Built::SortBy => "sort-by",
            Built::Unit => "unit",
            Built::Magnitude => "magnitude",
            Built::Or => "or",
            Built::Abs => "abs",
            Built::First => "first",
            Built::Last => "last",
            Built::Find => "find",
            Built::Keyword => "keyword",
            Built::KeywordizeKeys => "keywordize-keys",
            Built::AssertEq => "assert-eq",
            Built::Concat => "concat",
        }
    }

    /// Every builtin, for exhaustive iteration in tests and tooling.
    pub const ALL: &'static [Built] = &[
        Built::Println, Built::Print, Built::Str, Built::Len, Built::Get,
        Built::Conj, Built::Cons, Built::Assoc, Built::Merge, Built::Range,
        Built::Map, Built::Filter, Built::Reduce, Built::Each, Built::FlatMap,
        Built::Keys, Built::Vals, Built::Nth, Built::Take, Built::Drop,
        Built::Slice, Built::Contains, Built::Join, Built::Trim, Built::Split,
        Built::Sort, Built::Reverse, Built::Flatten, Built::Zip, Built::Chunk,
        Built::Any, Built::All, Built::Sum, Built::Min, Built::Max,
        Built::Int, Built::Float, Built::IntoMap, Built::GroupBy, Built::Collect,
        Built::StartsWith, Built::EndsWith, Built::Replace, Built::Uppercase,
        Built::Lowercase, Built::IndexOf, Built::CharAt, Built::Substring,
        Built::Not, Built::Empty, Built::Fold, Built::Update, Built::Entries,
        Built::SortBy, Built::Unit, Built::Magnitude, Built::Or, Built::Abs,
        Built::First, Built::Last, Built::Find, Built::Keyword,
        Built::KeywordizeKeys, Built::AssertEq, Built::Concat,
    ];
}

#[cfg(test)]
mod builtin_name_tests {
    use super::Built;

    /// The single source of truth must be internally consistent: every builtin
    /// round-trips name → tag → name, and `ALL` covers the whole enum.
    #[test]
    fn builtin_names_round_trip() {
        assert_eq!(Built::ALL.len(), 65, "ALL must list every Built variant");
        for &b in Built::ALL {
            assert_eq!(
                Built::from_name(b.name()),
                Some(b),
                "round-trip failed for {:?} (name {:?})",
                b,
                b.name()
            );
        }
    }
}
