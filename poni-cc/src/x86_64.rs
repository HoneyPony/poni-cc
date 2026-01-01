//! Backend for x86_64. The main goal is for it to work on my computer.

use std::{collections::HashMap, io::Write};

use crate::{ctx::{Ctx, StrId}, ir::UnaryOp};

pub mod lowering;
pub use lowering::lower_function;
use poni_arena::ArenaKey;

pub struct Program {
    pub functions: Vec<Function>,
}

pub struct Function {
    pub name: StrId,
    pub instructions: Vec<Instr>
}

pub enum Instr {
    Ret,
    // Note: I'm not sure the cleanest way to represent these.
    // It seems unnecessary to add YET ANOTHER enum for the same '~', '-' etc.
    Unary { op: UnaryOp, operand: Operand },
    Mov { src: Operand, dst: Operand },
}

pub enum Operand {
    Reg(Register),
    Imm(StrId),

    /// Psuedoregister. Should not actually be generated in real code.
    Psuedo(StrId),

    /// Stack slot.
    Stack(i32),
}

pub enum Register {
    Eax,
}

impl From<Register> for Operand {
    fn from(value: Register) -> Self {
        Operand::Reg(value)
    }
}

impl Register {
    pub fn as_asm(&self) -> &'static [u8] {
        match self {
            Register::Eax => b"%eax",
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
                Instr::Mov { src, dst } => {
                    fun(src);
                    fun(dst);
                },
            }
        }
    }
}

impl Instr {
    fn two_ops(ctx: &Ctx, output: &mut Box<dyn Write>, str: &'static [u8], a: &Operand, b: &Operand) -> std::io::Result<()> {
        output.write_all(str)?;

        a.write_as_text(ctx, output)?;
        output.write_all(b",\t")?;
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
            Instr::Ret => { output.write_all(b"\tret\n")?; }
            Instr::Mov { src, dst } => { Self::two_ops(ctx, output, b"\tmov ", src, dst)?; }
            Instr::Unary { op, operand } => {
                let opstr = match op {
                    UnaryOp::Complement => b"\tnot ",
                    UnaryOp::Negate => b"\tneg ",
                };
                Self::one_op(ctx, output, opstr, operand)?;
            }
        }

        Ok(())
    }
}

impl Operand {
    pub fn write_as_text(&self, ctx: &Ctx, output: &mut Box<dyn Write>) -> std::io::Result<()> {
        match self {
            Operand::Reg(register) => { output.write_all(register.as_asm())?; }
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
                        let x = offset;
                        offset -= 4;
                        x
                    });
                *op = Operand::Stack(*offset);
            },
            _ => {}
        }
    });
}