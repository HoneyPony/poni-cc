use std::io::Write;

use crate::{ctx::Ctx, x86_64::*};

impl Program {
    pub fn write_as_binary<W: Write>(&self, ctx: &Ctx, output: &mut W) -> std::io::Result<()> {
        for fun in &self.functions {
            fun.write_as_binary(ctx, output)?;
        }

        Ok(())
    }
}

impl Function {
    pub fn write_as_binary<W: Write>(&self, ctx: &Ctx, output: &mut W) -> std::io::Result<()> {
        //cwriteln!(output, ".globl {}", ctx.get(self.name));
        //cwriteln!(output, "{}:", ctx.get(self.name));

        // TODO:  Emit these as binary.
        //
        // What we really should do is make both write_as_text and write_as_binary
        // simply be common hooks called from some runner that emits each instruction,
        // then there would be no duplication.
        //cwriteln!(output, "\tpushq\t%rbp");
        //cwriteln!(output, "\tmovq\t%rsp, %rbp");
        //cwriteln!(output, "\tsubq\t${}, %rsp", self.stack_size);

        for instr in &self.instructions {
            instr.write_as_binary(ctx, output)?;
        }

        Ok(())
    }
}

impl Instr {
    pub fn write_as_binary<W: Write>(&self, _ctx: &Ctx, output: &mut W) -> std::io::Result<()> {
        // As an initial test, just write 8 bytes.
        output.write_all(b"ABCDEFGH")?;

        Ok(())
    }
}