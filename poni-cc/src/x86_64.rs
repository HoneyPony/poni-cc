//! Backend for x86_64. The main goal is for it to work on my computer.

use std::io::Write;

use crate::ctx::{Ctx, StrId};

pub struct Program {
    pub functions: Vec<Function>,
}

pub struct Function {
    pub name: StrId,
    pub instructions: Vec<Instr>
}

pub enum Instr {
    Ret,
    Mov { src: Operand, dst: Operand }
}

pub enum Operand {
    Reg(Register),
    Imm(StrId),
}

pub enum Register {
    Eax,
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

    pub fn write_as_text(&self, ctx: &Ctx, output: &mut Box<dyn Write>) -> std::io::Result<()> {
        match self {
            Instr::Ret => { output.write_all(b"\tret\n")?; }
            Instr::Mov { src, dst } => Self::two_ops(ctx, output, b"\tmov ", src, dst)?
        }

        Ok(())
    }
}

impl Operand {
    pub fn write_as_text(&self, ctx: &Ctx, output: &mut Box<dyn Write>) -> std::io::Result<()> {
        match self {
            Operand::Reg(register) => output.write_all(register.as_asm()),
            Operand::Imm(str_id) => {
                output.write_all(b"$")?;
                output.write_all(ctx.get(*str_id).as_bytes())
            },
        }
    }
}
