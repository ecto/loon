//! Evidence IR — the pivot between Loon's frontend and pluggable backends.
//!
//! Flat, SSA-like, block-based. Effects compiled to evidence-passing.
//! Every backend (Register VM, WASM, Cranelift) lowers from this IR.

pub mod backend;
pub mod lower;
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CaptureMode {
    Copy,
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

#[derive(Debug)]
pub struct Block {
    pub id: BlockId,
    /// Phi-like parameters — values passed in by predecessor branches.
    pub params: Vec<Reg>,
    pub ops: Vec<Op>,
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

#[derive(Debug, Clone)]
pub enum Lit {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(StringId),
    Keyword(StringId),
    Unit,
}

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOp {
    Neg,
    Not,
}

#[derive(Debug, Clone)]
pub enum Selector {
    /// Tuple or ADT field by position.
    Index(u16),
    /// Map key (string constant).
    Key(StringId),
    /// Named record field.
    Name(StringId),
}

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
