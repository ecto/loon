//! EIR, as the unikernel sees it: already lowered, already checked.
//!
//! The host compiler owns everything upstream of this point. What arrives
//! here is a boot image — a flat instruction graph with a string pool — and
//! the only job left is to run it.

pub mod decode;
pub mod val;
pub mod vm;

use alloc::string::String;
use alloc::vec::Vec;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Reg(pub u32);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FuncId(pub u32);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StringId(pub u32);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(pub u32);

/// Decoded in full even where the interpreter does not consult every field
/// yet — a partial decode that silently skips bytes is how image formats
/// drift apart.
#[allow(dead_code)]
pub struct Module {
    pub funcs: Vec<Func>,
    pub strings: Vec<String>,
    pub ctors: Vec<Ctor>,
    /// Tag → variant name for every builtin this image references. Dispatch
    /// goes through the name so a reordered host enum cannot silently
    /// remap an intrinsic.
    pub builtins: Vec<(u16, String)>,
    pub entry: FuncId,
}

impl Module {
    pub fn builtin_name(&self, tag: u16) -> Option<&str> {
        self.builtins
            .iter()
            .find(|(t, _)| *t == tag)
            .map(|(_, n)| n.as_str())
    }

    pub fn string(&self, id: StringId) -> &str {
        self.strings
            .get(id.0 as usize)
            .map(|s| s.as_str())
            .unwrap_or("<bad string id>")
    }
}

#[allow(dead_code)]
pub struct Ctor {
    pub name: String,
    pub tag: u16,
    pub arity: u16,
}

#[allow(dead_code)]
pub struct Func {
    pub name: Option<String>,
    pub params: u32,
    pub captures: u32,
    pub evidence: u32,
    /// Frame size: one past the highest register the body mentions.
    pub regs: u32,
    pub blocks: Vec<Block>,
}

pub struct Block {
    pub params: Vec<Reg>,
    pub ops: Vec<Op>,
    pub end: End,
}

#[derive(Debug, Clone)]
pub enum Op {
    Lit(Reg, Lit),
    Mov(Reg, Reg),
    Upval(Reg, u16),
    Bin(Reg, BinOp, Reg, Reg),
    Un(Reg, UnOp, Reg),
    Call(Reg, FuncId, Vec<Reg>),
    Invoke(Reg, Reg, Vec<Reg>),
    Close(Reg, FuncId, Vec<Reg>),
    Vec(Reg, Vec<Reg>),
    Map(Reg, Vec<(Reg, Reg)>),
    Set(Reg, Vec<Reg>),
    Tup(Reg, Vec<Reg>),
    Adt(Reg, u16, Vec<Reg>),
    Field(Reg, Reg, Selector),
    Tag(Reg, Reg),
    Perform(Reg, StringId, StringId, Vec<Reg>),
    Builtin(Reg, u16, Vec<Reg>),
    PushHandler(Reg, StringId, StringId),
    PopHandler,
}

#[derive(Debug, Clone)]
pub enum End {
    Ret(Reg),
    Jmp(BlockId, Vec<Reg>),
    Br(Reg, BlockId, BlockId),
    Switch(Reg, Vec<(u16, BlockId)>, BlockId),
    Tail(FuncId, Vec<Reg>),
    TailInvoke(Reg, Vec<Reg>),
    Recur(Vec<Reg>),
    Trap,
}

#[derive(Debug, Clone)]
pub enum Lit {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(StringId),
    Keyword(StringId),
    Unit,
}

#[derive(Debug, Clone)]
pub enum Selector {
    Index(u16),
    Key(StringId),
    Name(StringId),
}

/// Tags must match `loon_lang::eir::BinOp` declaration order.
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

/// Tags must match `loon_lang::eir::UnOp` declaration order.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOp {
    Neg,
    Not,
}
