//! Backend for x86_64. The main goal is for it to work on my computer.

use std::{collections::HashMap, io::Write};

use crate::{ctx::{Ctx, StrId}, ir::{BinaryOp, Label, UnaryOp}};

pub mod lowering;
pub use lowering::lower_function;
use poni_arena::ArenaKey;

pub struct Program {
    pub functions: Vec<Function>,
}

pub struct Function {
    pub name: StrId,
    pub stack_size: i32,
    pub instructions: Vec<Instr>
}

pub enum Instr {
    Ret,
    // Note: I'm not sure the cleanest way to represent these.
    // It seems unnecessary to add YET ANOTHER enum for the same '~', '-' etc.
    Unary { op: UnaryOp, operand: Operand },
    // x86 sure is not three address code.
    Binary { op: BinaryOp, dst: Operand, src: Operand },
    Cdq,
    Cmp { lhs: Operand, rhs: Operand },
    Jmp(Label),
    JmpCC(CondCode, Label),
    SetCC(CondCode, Operand),
    Idiv { rhs: Operand },
    Mov { src: Operand, dst: Operand },

    // Shifts by ecx into the dst.
    Shift { op: BinaryOp, dst: Operand },

    Label(Label),
}

pub enum CondCode {
    E,
    NE,
    L,
    LE,
    G,
    GE
}

impl CondCode {
    pub fn as_asm(&self) -> &'static [u8] {
        match self {
            CondCode::E  => b"e",
            CondCode::NE => b"ne",
            CondCode::L  => b"l",
            CondCode::LE => b"le",
            CondCode::G  => b"g",
            CondCode::GE => b"ge",
        }
    }
}

#[derive(Clone, Copy)]
pub enum Operand {
    Reg(Register, u8),
    Imm(StrId),

    /// Psuedoregister. Should not actually be generated in real code.
    Psuedo(StrId),

    /// Stack slot.
    Stack(i32),
}

impl Operand {
    pub fn with_size(&self, size: u8) -> Operand {
        match self {
            Operand::Reg(r, _) => Operand::Reg(*r, size),
            _ => *self
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Register {
    Eax,
    Ecx,
    Edx,
    R10d,
    R11d,
}

impl From<Register> for Operand {
    fn from(value: Register) -> Self {
        // For now, default to 4 bytes (?)
        Operand::Reg(value, 4)
    }
}

impl Register {
    pub fn as_asm(&self, size: u8) -> &'static [u8] {
        match (self, size) {
            (Register::Eax , 4) => b"%eax",
            (Register::Ecx , 4) => b"%ecx",
            (Register::Edx , 4) => b"%edx",
            (Register::R10d, 4) => b"%r10d",
            (Register::R11d, 4) => b"%r11d",

            (Register::Eax , 1) => b"%al",
            (Register::Ecx , 1) => b"%cl",
            (Register::Edx , 1) => b"%dl",
            (Register::R10d, 1) => b"%r10b",
            (Register::R11d, 1) => b"%r11b",

            _ => todo!()
        }
    }
}

macro_rules! cwrite {
	($into:expr, $($arg:tt)*) => {
		write!($into, $($arg)*)?
	}
}

macro_rules! cwriteln {
	($into:expr, $($arg:tt)*) => {
		writeln!($into, $($arg)*)?
	}
}

impl Program {
    pub fn write_as_text(&self, ctx: &Ctx, output: &mut Box<dyn Write>) -> std::io::Result<()> {
        for fun in &self.functions {
            fun.write_as_text(ctx, output)?;
        }

        // Write the .note.GNU-stack section ourselves.
        cwriteln!(output, "\t.section .note.GNU-stack,\"\",@progbits\n");

        Ok(())
    }
}

impl Function {
    pub fn write_as_text(&self, ctx: &Ctx, output: &mut Box<dyn Write>) -> std::io::Result<()> {
        cwriteln!(output, ".globl {}", ctx.get(self.name));
        cwriteln!(output, "{}:", ctx.get(self.name));

        // Manually generate stack allocation & prelude. This saves us from
        // needing to add any additional instructions to the instructions vector.
        cwriteln!(output, "\tpushq\t%rbp");
        cwriteln!(output, "\tmovq\t%rsp, %rbp");
        cwriteln!(output, "\tsubq\t${}, %rsp", self.stack_size);

        for instr in &self.instructions {
            instr.write_as_text(ctx, output)?;
        }

        Ok(())
    }

    /// Passes over all the instructions in this Function, and applies the
    /// given 'fun' to every Operand.
    /// 
    /// Used to implement psuedoregister replacement AND instruction fixup.
    pub fn operand_pass<F: FnMut(&mut Operand)>(&mut self, mut fun: F) {
        for instr in &mut self.instructions {
            match instr {
                Instr::Ret => {},
                Instr::Unary { operand, .. } => {
                    fun(operand);
                },
                Instr::Binary { dst, src, .. } => {
                    fun(dst);
                    fun(src);
                }
                Instr::Mov { src, dst } => {
                    fun(src);
                    fun(dst);
                },
                Instr::Cdq => {},
                Instr::Idiv { rhs: dst } => {
                    fun(dst);
                }
                Instr::Shift { dst, .. } => {
                    fun(dst)
                }
                Instr::Jmp(_) => {}
                Instr::Label(_) => {}
                Instr::JmpCC(..) => {}
                Instr::SetCC(_, op) => { fun(op); }
                Instr::Cmp { lhs, rhs } => {
                    fun(lhs);
                    fun(rhs);
                }
            }
        }
    }
}

impl Instr {
    fn two_ops(ctx: &Ctx, output: &mut Box<dyn Write>, str: &'static [u8], a: &Operand, b: &Operand) -> std::io::Result<()> {
        output.write_all(str)?;

        a.write_as_text(ctx, output)?;
        output.write_all(b", ")?;
        b.write_as_text(ctx, output)?;
        output.write_all(b"\n")?;

        Ok(())
    }

    fn one_op(ctx: &Ctx, output: &mut Box<dyn Write>, str: &'static [u8], a: &Operand) -> std::io::Result<()> {
        output.write_all(str)?;

        a.write_as_text(ctx, output)?;
        output.write_all(b"\n")?;

        Ok(())
    }

    pub fn write_as_text(&self, ctx: &Ctx, output: &mut Box<dyn Write>) -> std::io::Result<()> {
        match self {
            Instr::Ret => {
                // TODO: This will depend on the stack frame. Annoying...
                cwriteln!(output, "\tmovq\t%rbp, %rsp");
                cwriteln!(output, "\tpopq\t%rbp");
                output.write_all(b"\tret\n")?;
            }
            Instr::Mov { src, dst } => { Self::two_ops(ctx, output, b"\tmovl\t", src, dst)?; }
            Instr::Unary { op, operand } => {
                let opstr = match op {
                    UnaryOp::Complement => b"\tnotl\t",
                    UnaryOp::Negate => b"\tnegl\t",
                    UnaryOp::Not => panic!(),
                };
                Self::one_op(ctx, output, opstr, operand)?;
            }
            Instr::Binary { op, dst, src } => {
                let opstr: &[u8] = match op {
                    BinaryOp::Add => b"\taddl\t",
                    BinaryOp::Subtract => b"\tsubl\t",
                    BinaryOp::Multiply => b"\timull\t",
                    BinaryOp::And => b"\tandl\t",
                    BinaryOp::Or => b"\torl\t",
                    BinaryOp::Xor => b"\txorl\t",
                    BinaryOp::Lshift | BinaryOp::Rshift => panic!(),
                    _ => panic!("Bad binary operator"),
                };
                Self::two_ops(ctx, output, opstr, src, dst)?;
            }
            Instr::Cdq => {
                output.write_all(b"\tcdq\n")?;
            }
            Instr::Idiv { rhs: dst } => {
                Self::one_op(ctx, output, b"\tidivl\t", dst)?;
            }
            Instr::Shift { op, dst } => {
                let opstr: &[u8] = match op {
                    BinaryOp::Lshift => b"\tshll\t",
                    // For now, because we are using essentially 'int', we
                    // should be using an arithmetic left shift. This will have
                    // to change for unsigned values.
                    BinaryOp::Rshift => b"\tsarl\t",
                    _ => panic!()
                };

                output.write_all(opstr)?;
                // For now, we are always shifting by cl.
                output.write_all(b"%cl, ")?;
                dst.write_as_text(ctx, output)?;
                output.write_all(b"\n")?;
            }
            Instr::Jmp(label) => {
                output.write_all(b"\tjmp\t")?;
                output.write_all(ctx.get(*label).as_bytes())?;
                output.write_all(b"\n")?;
            }
            Instr::JmpCC(cc, label) => {
                output.write_all(b"\tj")?;
                output.write_all(cc.as_asm())?;
                output.write_all(b"\t")?;
                output.write_all(ctx.get(*label).as_bytes())?;
                output.write_all(b"\n")?;
            }
            Instr::SetCC(cc, dst) => {
                output.write_all(b"\tset")?;
                output.write_all(cc.as_asm())?;
                output.write_all(b"\t")?;
                dst.write_as_text(ctx, output)?;
                output.write_all(b"\n")?;
            }
            Instr::Cmp { lhs, rhs } => {
                Self::two_ops(ctx, output, b"\tcmpl\t", rhs, lhs);
            }
            Instr::Label(label) => {
                output.write_all(ctx.get(*label).as_bytes())?;
                output.write_all(b":\n");
            }
        }

        Ok(())
    }
}

impl Operand {
    pub fn write_as_text(&self, ctx: &Ctx, output: &mut Box<dyn Write>) -> std::io::Result<()> {
        match self {
            Operand::Reg(register, size) => { output.write_all(register.as_asm(*size))?; }
            Operand::Imm(str_id) => {
                output.write_all(b"$")?;
                output.write_all(ctx.get(*str_id).as_bytes())?;
            }
            Operand::Psuedo(str_id) => {
                // This is not actually valid, but it is good to be able to
                // see what is going on in case something bad happens.
                //
                // We could have some sort of flag to ICE in this case.
                cwrite!(output, "<psuedoreg {}>", str_id.to_index());
            }
            Operand::Stack(offset) => {
                cwrite!(output, "{}(%rbp)", offset)
            }
        }

        Ok(())
    }
}

pub fn replace_psuedoregister_pass(fun: &mut Function) {
    let mut offset = 0;

    // This map maps variables/psuedoregisters to their stack offset.
    // However, I feel like maybe we want to do this in a different way.
    //
    // In particular, if each Function had its *own* map of variables, it
    // could be a flat array indexed from 0. Then, doing this kind of pass
    // could use that same array, which would probably be much faster than
    // a HashMap.
    let mut map: HashMap<StrId, i32> = HashMap::new();

    fun.operand_pass(|op: &mut Operand| {
        match op {
            Operand::Psuedo(str_id) => {
                let offset = map.entry(*str_id)
                    .or_insert_with(|| {
                        // We have to subtract *then* return the offset. E.g.
                        // allocating one int on the stack, it needs an
                        // offset of -4, not 0.
                        offset -= 4;
                        offset
                    });
                *op = Operand::Stack(*offset);
            },
            _ => {}
        }
    });

    fun.stack_size = -offset;
}

pub fn fixup_pass(fun: &mut Function) {
    // I'm not actually sure how to do this in an efficient way. The book
    // suggests inserting new instructions, which is awkward in a Vec.
    //
    // For now, we construct an entirely new Vec. :shrug:.
    let mut new_instrs = Vec::with_capacity(fun.instructions.len());
    let old = std::mem::take(&mut fun.instructions);

    for instr in old {
        match instr {
            // Fixup memory-memory mov with a new instruction
            Instr::Mov { src: s1 @ Operand::Stack(_), dst: s2 @ Operand::Stack(_) } => {
                new_instrs.push(Instr::Mov { src: s1, dst: Register::R10d.into() });
                new_instrs.push(Instr::Mov { src: Register::R10d.into(), dst: s2 });
            },
            // Whenever the multiply operation has a stack as its dest, fix it
            // up to use r11d instead.
            Instr::Binary { op: op @ BinaryOp::Multiply, src, dst: s1 @ Operand::Stack(_) } => {
                new_instrs.push(Instr::Mov { src: s1, dst: Register::R11d.into() });
                new_instrs.push(Instr::Binary { op, dst: Register::R11d.into(), src: src });
                new_instrs.push(Instr::Mov { src: Register::R11d.into(), dst: s1 });
            }
            Instr::Binary { op, src: s1 @ Operand::Stack(_), dst: s2 @ Operand::Stack(_) } => {
                new_instrs.push(Instr::Mov { src: s1, dst: Register::R10d.into() });
                new_instrs.push(Instr::Binary { op, src: Register::R10d.into(), dst: s2 });
            }

            // Fixup idiv <imm> so that the value is instead in a register.
            Instr::Idiv { rhs: dst @ Operand::Imm(_) } => {
                new_instrs.push(Instr::Mov { src: dst, dst: Register::R10d.into() });
                new_instrs.push(Instr::Idiv { rhs: Register::R10d.into() });
            }
            Instr::Cmp { mut lhs, mut rhs } => {
                // There are two fixes here.
                //
                // First, if both operands are a stack operand, we replace
                // the RHS with r10d.
                //
                // Second, if the lhs is a constant, we replace it with r11d.
                if matches!(lhs, Operand::Stack(_)) && matches!(rhs, Operand::Stack(_)) {
                    new_instrs.push(Instr::Mov { src: rhs, dst: Register::R10d.into() });
                    rhs = Register::R10d.into();
                }

                if matches!(lhs, Operand::Imm(_)) {
                    new_instrs.push(Instr::Mov { src: lhs, dst: Register::R11d.into() });
                    lhs = Register::R11d.into();
                }

                new_instrs.push(Instr::Cmp { lhs, rhs });
            }
            instr @ _ => {
                new_instrs.push(instr);
            }
        }
    }

    fun.instructions = new_instrs;
}