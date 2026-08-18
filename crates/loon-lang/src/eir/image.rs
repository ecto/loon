//! The boot image: EIR serialized for a runtime that cannot compile.
//!
//! The unikernel has no parser, no checker and no lowering — it is handed a
//! finished `Module` and interprets it. This module is the wire format
//! between the two, and it is deliberately dumb: little-endian, length-
//! prefixed, no compression, no relocation. Spans and types are dropped
//! because nothing on the far side reads them.

use super::*;

pub const MAGIC: &[u8; 8] = b"LOONIMG\0";
pub const VERSION: u32 = 1;

#[derive(Default)]
struct Enc(Vec<u8>);

impl Enc {
    fn u8(&mut self, v: u8) {
        self.0.push(v);
    }
    fn u16(&mut self, v: u16) {
        self.0.extend_from_slice(&v.to_le_bytes());
    }
    fn u32(&mut self, v: u32) {
        self.0.extend_from_slice(&v.to_le_bytes());
    }
    fn i64(&mut self, v: i64) {
        self.0.extend_from_slice(&v.to_le_bytes());
    }
    fn f64(&mut self, v: f64) {
        self.0.extend_from_slice(&v.to_le_bytes());
    }
    fn str(&mut self, s: &str) {
        self.u32(s.len() as u32);
        self.0.extend_from_slice(s.as_bytes());
    }
    fn regs(&mut self, rs: &[Reg]) {
        self.u32(rs.len() as u32);
        for r in rs {
            self.u32(r.0);
        }
    }
}

/// Serialize a module into a boot image.
pub fn encode(m: &Module) -> Vec<u8> {
    let mut e = Enc::default();
    e.0.extend_from_slice(MAGIC);
    e.u32(VERSION);

    e.u32(m.strings.len() as u32);
    for s in &m.strings {
        e.str(s);
    }

    e.u32(m.ctors.len() as u32);
    for c in &m.ctors {
        e.str(&c.name);
        e.u16(c.tag);
        e.u16(c.arity);
    }

    // Builtins are referenced by numeric tag, but a tag is just an enum
    // discriminant — reorder `Built` and every previously-built image would
    // silently call the wrong intrinsic. Ship the names alongside so the
    // runtime dispatches on something stable and can refuse what it lacks.
    let mut used: Vec<Built> = Vec::new();
    for f in &m.funcs {
        for b in &f.blocks {
            for o in &b.ops {
                if let Op::Builtin(_, built, ..) = o {
                    if !used.contains(built) {
                        used.push(*built);
                    }
                }
            }
        }
    }
    e.u32(used.len() as u32);
    for b in &used {
        e.u16(*b as u16);
        e.str(&alloc_name(*b));
    }

    e.u32(m.entry.0);

    e.u32(m.funcs.len() as u32);
    for f in &m.funcs {
        func(&mut e, f);
    }
    e.0
}

/// The variant name, which is what the runtime dispatches on.
fn alloc_name(b: Built) -> String {
    format!("{b:?}")
}

fn func(e: &mut Enc, f: &Func) {
    match &f.name {
        Some(n) => {
            e.u8(1);
            e.str(n);
        }
        None => e.u8(0),
    }
    e.u32(f.params.len() as u32);
    e.u32(f.captures.len() as u32);
    // Evidence params are appended after the declared params; the runtime
    // only needs to know how many slots to reserve for them.
    e.u32(f.evidence.len() as u32);
    e.u32(max_reg(f) + 1);

    e.u32(f.blocks.len() as u32);
    for b in &f.blocks {
        e.regs(&b.params);
        e.u32(b.ops.len() as u32);
        for o in &b.ops {
            op(e, o);
        }
        end(e, &b.end);
    }
}

/// Highest register mentioned anywhere in the function — the frame size.
fn max_reg(f: &Func) -> u32 {
    let mut hi = 0u32;
    let mut bump = |r: Reg| hi = hi.max(r.0);
    for b in &f.blocks {
        for r in &b.params {
            bump(*r);
        }
        for o in &b.ops {
            bump(o.dst());
            for r in op_srcs(o) {
                bump(r);
            }
        }
        for r in end_srcs(&b.end) {
            bump(r);
        }
    }
    hi
}

fn op_srcs(o: &Op) -> Vec<Reg> {
    match o {
        Op::Lit(..) | Op::Upval(..) | Op::PopHandler(_) => vec![],
        Op::Mov(_, a, _) | Op::Un(_, _, a, _) | Op::Field(_, a, _, _) | Op::Tag(_, a, _) => vec![*a],
        Op::Bin(_, _, a, b, _) => vec![*a, *b],
        Op::Call(_, _, rs, _)
        | Op::Close(_, _, rs, _)
        | Op::Vec(_, rs, _)
        | Op::Set(_, rs, _)
        | Op::Tup(_, rs, _)
        | Op::Adt(_, _, rs, _)
        | Op::Builtin(_, _, rs, _) => rs.clone(),
        Op::Invoke(_, f, rs, _) => {
            let mut v = vec![*f];
            v.extend(rs.iter().copied());
            v
        }
        Op::Map(_, kvs, _) => kvs.iter().flat_map(|(k, v)| [*k, *v]).collect(),
        Op::Perform(_, _, _, rs, ev, _) => {
            let mut v = rs.clone();
            v.extend(ev.iter().copied());
            v
        }
        Op::PushHandler(r, ..) => vec![*r],
    }
}

fn end_srcs(e: &End) -> Vec<Reg> {
    match e {
        End::Ret(r) | End::Br(r, _, _) | End::Switch(r, _, _) => vec![*r],
        End::Jmp(_, rs) | End::Tail(_, rs) | End::Recur(rs) => rs.clone(),
        End::TailInvoke(f, rs) => {
            let mut v = vec![*f];
            v.extend(rs.iter().copied());
            v
        }
        End::Trap => vec![],
    }
}

fn op(e: &mut Enc, o: &Op) {
    match o {
        Op::Lit(d, l, _) => {
            e.u8(0);
            e.u32(d.0);
            lit(e, l);
        }
        Op::Mov(d, a, _) => {
            e.u8(1);
            e.u32(d.0);
            e.u32(a.0);
        }
        Op::Upval(d, i, _) => {
            e.u8(2);
            e.u32(d.0);
            e.u16(*i);
        }
        Op::Bin(d, o2, a, b, _) => {
            e.u8(3);
            e.u32(d.0);
            e.u8(*o2 as u8);
            e.u32(a.0);
            e.u32(b.0);
        }
        Op::Un(d, o2, a, _) => {
            e.u8(4);
            e.u32(d.0);
            e.u8(*o2 as u8);
            e.u32(a.0);
        }
        Op::Call(d, f, rs, _) => {
            e.u8(5);
            e.u32(d.0);
            e.u32(f.0);
            e.regs(rs);
        }
        Op::Invoke(d, f, rs, _) => {
            e.u8(6);
            e.u32(d.0);
            e.u32(f.0);
            e.regs(rs);
        }
        Op::Close(d, f, rs, _) => {
            e.u8(7);
            e.u32(d.0);
            e.u32(f.0);
            e.regs(rs);
        }
        Op::Vec(d, rs, _) => {
            e.u8(8);
            e.u32(d.0);
            e.regs(rs);
        }
        Op::Map(d, kvs, _) => {
            e.u8(9);
            e.u32(d.0);
            e.u32(kvs.len() as u32);
            for (k, v) in kvs {
                e.u32(k.0);
                e.u32(v.0);
            }
        }
        Op::Set(d, rs, _) => {
            e.u8(10);
            e.u32(d.0);
            e.regs(rs);
        }
        Op::Tup(d, rs, _) => {
            e.u8(11);
            e.u32(d.0);
            e.regs(rs);
        }
        Op::Adt(d, tag, rs, _) => {
            e.u8(12);
            e.u32(d.0);
            e.u16(*tag);
            e.regs(rs);
        }
        Op::Field(d, a, sel, _) => {
            e.u8(13);
            e.u32(d.0);
            e.u32(a.0);
            match sel {
                Selector::Index(i) => {
                    e.u8(0);
                    e.u16(*i);
                }
                Selector::Key(s) => {
                    e.u8(1);
                    e.u32(s.0);
                }
                Selector::Name(s) => {
                    e.u8(2);
                    e.u32(s.0);
                }
            }
        }
        Op::Tag(d, a, _) => {
            e.u8(14);
            e.u32(d.0);
            e.u32(a.0);
        }
        Op::Perform(d, eff, o2, rs, ev, _) => {
            e.u8(15);
            e.u32(d.0);
            e.u32(eff.0);
            e.u32(o2.0);
            e.regs(rs);
            match ev {
                Some(r) => {
                    e.u8(1);
                    e.u32(r.0);
                }
                None => e.u8(0),
            }
        }
        Op::Builtin(d, b, rs, _) => {
            e.u8(16);
            e.u32(d.0);
            e.u16(*b as u16);
            e.regs(rs);
        }
        Op::PushHandler(r, eff, o2, _) => {
            e.u8(17);
            e.u32(r.0);
            e.u32(eff.0);
            e.u32(o2.0);
        }
        Op::PopHandler(_) => e.u8(18),
    }
}

fn lit(e: &mut Enc, l: &Lit) {
    match l {
        Lit::Int(v) => {
            e.u8(0);
            e.i64(*v);
        }
        Lit::Float(v) => {
            e.u8(1);
            e.f64(*v);
        }
        Lit::Bool(v) => {
            e.u8(2);
            e.u8(*v as u8);
        }
        Lit::Str(s) => {
            e.u8(3);
            e.u32(s.0);
        }
        Lit::Keyword(s) => {
            e.u8(4);
            e.u32(s.0);
        }
        Lit::Unit => e.u8(5),
    }
}

fn end(e: &mut Enc, t: &End) {
    match t {
        End::Ret(r) => {
            e.u8(0);
            e.u32(r.0);
        }
        End::Jmp(b, rs) => {
            e.u8(1);
            e.u32(b.0);
            e.regs(rs);
        }
        End::Br(r, a, b) => {
            e.u8(2);
            e.u32(r.0);
            e.u32(a.0);
            e.u32(b.0);
        }
        End::Switch(r, arms, dflt) => {
            e.u8(3);
            e.u32(r.0);
            e.u32(arms.len() as u32);
            for (tag, b) in arms {
                e.u16(*tag);
                e.u32(b.0);
            }
            e.u32(dflt.0);
        }
        End::Tail(f, rs) => {
            e.u8(4);
            e.u32(f.0);
            e.regs(rs);
        }
        End::TailInvoke(f, rs) => {
            e.u8(5);
            e.u32(f.0);
            e.regs(rs);
        }
        End::Recur(rs) => {
            e.u8(6);
            e.regs(rs);
        }
        End::Trap => e.u8(7),
    }
}
