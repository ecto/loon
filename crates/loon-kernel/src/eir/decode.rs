//! Boot image → `Module`.
//!
//! Mirrors `loon_lang::eir::image::encode` byte for byte. A malformed image
//! is a hard error: this runs before anything else, and a VM built on a
//! half-decoded module fails much later and much more confusingly.

use super::*;
use alloc::string::{String, ToString};
use alloc::vec::Vec;

pub const MAGIC: &[u8; 8] = b"LOONIMG\0";
pub const VERSION: u32 = 1;

pub struct Dec<'a> {
    buf: &'a [u8],
    pos: usize,
}

type R<T> = Result<T, String>;

impl<'a> Dec<'a> {
    fn take(&mut self, n: usize) -> R<&'a [u8]> {
        if self.pos + n > self.buf.len() {
            return Err("boot image truncated".to_string());
        }
        let s = &self.buf[self.pos..self.pos + n];
        self.pos += n;
        Ok(s)
    }
    fn u8(&mut self) -> R<u8> {
        Ok(self.take(1)?[0])
    }
    fn u16(&mut self) -> R<u16> {
        Ok(u16::from_le_bytes(self.take(2)?.try_into().unwrap()))
    }
    fn u32(&mut self) -> R<u32> {
        Ok(u32::from_le_bytes(self.take(4)?.try_into().unwrap()))
    }
    fn i64(&mut self) -> R<i64> {
        Ok(i64::from_le_bytes(self.take(8)?.try_into().unwrap()))
    }
    fn f64(&mut self) -> R<f64> {
        Ok(f64::from_le_bytes(self.take(8)?.try_into().unwrap()))
    }
    fn str(&mut self) -> R<String> {
        let n = self.u32()? as usize;
        let b = self.take(n)?;
        core::str::from_utf8(b)
            .map(|s| s.to_string())
            .map_err(|_| "boot image has a non-utf8 string".to_string())
    }
    fn reg(&mut self) -> R<Reg> {
        Ok(Reg(self.u32()?))
    }
    fn regs(&mut self) -> R<Vec<Reg>> {
        let n = self.u32()? as usize;
        let mut v = Vec::with_capacity(n);
        for _ in 0..n {
            v.push(self.reg()?);
        }
        Ok(v)
    }
}

pub fn decode(buf: &[u8]) -> R<Module> {
    let mut d = Dec { buf, pos: 0 };
    if d.take(8)? != MAGIC {
        return Err("not a loon boot image".to_string());
    }
    let v = d.u32()?;
    if v != VERSION {
        return Err(alloc::format!(
            "boot image version {v}, this kernel speaks {VERSION}"
        ));
    }

    let n = d.u32()? as usize;
    let mut strings = Vec::with_capacity(n);
    for _ in 0..n {
        strings.push(d.str()?);
    }

    let n = d.u32()? as usize;
    let mut ctors = Vec::with_capacity(n);
    for _ in 0..n {
        ctors.push(Ctor {
            name: d.str()?,
            tag: d.u16()?,
            arity: d.u16()?,
        });
    }

    let n = d.u32()? as usize;
    let mut builtins = Vec::with_capacity(n);
    for _ in 0..n {
        let tag = d.u16()?;
        builtins.push((tag, d.str()?));
    }

    let entry = FuncId(d.u32()?);

    let n = d.u32()? as usize;
    let mut funcs = Vec::with_capacity(n);
    for _ in 0..n {
        funcs.push(func(&mut d)?);
    }

    Ok(Module {
        funcs,
        strings,
        ctors,
        builtins,
        entry,
    })
}

fn func(d: &mut Dec) -> R<Func> {
    let name = if d.u8()? == 1 { Some(d.str()?) } else { None };
    let params = d.u32()?;
    let captures = d.u32()?;
    let evidence = d.u32()?;
    let regs = d.u32()?;

    let n = d.u32()? as usize;
    let mut blocks = Vec::with_capacity(n);
    for _ in 0..n {
        let params = d.regs()?;
        let n_ops = d.u32()? as usize;
        let mut ops = Vec::with_capacity(n_ops);
        for _ in 0..n_ops {
            ops.push(op(d)?);
        }
        blocks.push(Block {
            params,
            ops,
            end: end(d)?,
        });
    }

    Ok(Func {
        name,
        params,
        captures,
        evidence,
        regs,
        blocks,
    })
}

fn op(d: &mut Dec) -> R<Op> {
    Ok(match d.u8()? {
        0 => Op::Lit(d.reg()?, lit(d)?),
        1 => Op::Mov(d.reg()?, d.reg()?),
        2 => Op::Upval(d.reg()?, d.u16()?),
        3 => {
            let dst = d.reg()?;
            let o = binop(d.u8()?)?;
            Op::Bin(dst, o, d.reg()?, d.reg()?)
        }
        4 => {
            let dst = d.reg()?;
            let o = match d.u8()? {
                0 => UnOp::Neg,
                1 => UnOp::Not,
                t => return Err(alloc::format!("unknown unop tag {t}")),
            };
            Op::Un(dst, o, d.reg()?)
        }
        5 => Op::Call(d.reg()?, FuncId(d.u32()?), d.regs()?),
        6 => Op::Invoke(d.reg()?, d.reg()?, d.regs()?),
        7 => Op::Close(d.reg()?, FuncId(d.u32()?), d.regs()?),
        8 => Op::Vec(d.reg()?, d.regs()?),
        9 => {
            let dst = d.reg()?;
            let n = d.u32()? as usize;
            let mut kvs = Vec::with_capacity(n);
            for _ in 0..n {
                kvs.push((d.reg()?, d.reg()?));
            }
            Op::Map(dst, kvs)
        }
        10 => Op::Set(d.reg()?, d.regs()?),
        11 => Op::Tup(d.reg()?, d.regs()?),
        12 => Op::Adt(d.reg()?, d.u16()?, d.regs()?),
        13 => {
            let dst = d.reg()?;
            let src = d.reg()?;
            let sel = match d.u8()? {
                0 => Selector::Index(d.u16()?),
                1 => Selector::Key(StringId(d.u32()?)),
                2 => Selector::Name(StringId(d.u32()?)),
                t => return Err(alloc::format!("unknown selector tag {t}")),
            };
            Op::Field(dst, src, sel)
        }
        14 => Op::Tag(d.reg()?, d.reg()?),
        15 => {
            let dst = d.reg()?;
            let eff = StringId(d.u32()?);
            let o = StringId(d.u32()?);
            let args = d.regs()?;
            // Evidence is decoded and dropped: like the host VM, dispatch is
            // dynamic, because capturing a continuation needs the prompt
            // boundary that only the handler stack records.
            if d.u8()? == 1 {
                let _ = d.u32()?;
            }
            Op::Perform(dst, eff, o, args)
        }
        16 => Op::Builtin(d.reg()?, d.u16()?, d.regs()?),
        17 => Op::PushHandler(d.reg()?, StringId(d.u32()?), StringId(d.u32()?)),
        18 => Op::PopHandler,
        t => return Err(alloc::format!("unknown op tag {t}")),
    })
}

fn binop(t: u8) -> R<BinOp> {
    Ok(match t {
        0 => BinOp::Add,
        1 => BinOp::Sub,
        2 => BinOp::Mul,
        3 => BinOp::Div,
        4 => BinOp::Rem,
        5 => BinOp::Eq,
        6 => BinOp::Ne,
        7 => BinOp::Lt,
        8 => BinOp::Gt,
        9 => BinOp::Le,
        10 => BinOp::Ge,
        11 => BinOp::And,
        12 => BinOp::Or,
        13 => BinOp::Concat,
        t => return Err(alloc::format!("unknown binop tag {t}")),
    })
}

fn lit(d: &mut Dec) -> R<Lit> {
    Ok(match d.u8()? {
        0 => Lit::Int(d.i64()?),
        1 => Lit::Float(d.f64()?),
        2 => Lit::Bool(d.u8()? != 0),
        3 => Lit::Str(StringId(d.u32()?)),
        4 => Lit::Keyword(StringId(d.u32()?)),
        5 => Lit::Unit,
        t => return Err(alloc::format!("unknown literal tag {t}")),
    })
}

fn end(d: &mut Dec) -> R<End> {
    Ok(match d.u8()? {
        0 => End::Ret(d.reg()?),
        1 => End::Jmp(BlockId(d.u32()?), d.regs()?),
        2 => End::Br(d.reg()?, BlockId(d.u32()?), BlockId(d.u32()?)),
        3 => {
            let scrut = d.reg()?;
            let n = d.u32()? as usize;
            let mut arms = Vec::with_capacity(n);
            for _ in 0..n {
                arms.push((d.u16()?, BlockId(d.u32()?)));
            }
            End::Switch(scrut, arms, BlockId(d.u32()?))
        }
        4 => End::Tail(FuncId(d.u32()?), d.regs()?),
        5 => End::TailInvoke(d.reg()?, d.regs()?),
        6 => End::Recur(d.regs()?),
        7 => End::Trap,
        t => return Err(alloc::format!("unknown terminator tag {t}")),
    })
}
