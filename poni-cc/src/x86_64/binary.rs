use std::io::Write;

use crate::{ctx::Ctx, x86_64::*};

/// Intermediate struct for binary code. Will probably eventually need to be
/// ELF related or something?
/// 
/// We need to keep track of our offset in the binary so that we can compute
/// offsets, eventually.
struct Binary {
    // Text section
    text: Vec<u8>,
    // Data section
    data: Vec<u8>,
}

impl Binary {
    pub fn new() -> Self {
        Binary {
            text: Vec::new(),
            data: Vec::new(),
        }
    }
}

impl Program {
    pub fn write_as_binary<W: Write>(&self, ctx: &Ctx, output: &mut W) -> std::io::Result<()> {
        let mut binary = Binary::new();

        for fun in &self.functions {
            fun.write_as_binary(ctx, &mut binary)?;
        }

        Ok(())
    }
}

impl Function {
    fn write_as_binary(&self, ctx: &Ctx, binary: &mut Binary) -> std::io::Result<()> {
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
            instr.write_as_binary(ctx, binary)?;
        }

        Ok(())
    }
}

struct Opcodes {
    mem_dst_reg_src: u8,
    reg_dst_mem_src: u8,
    mem_dst_imm32_src: u8,
}

/// Helper function for writing the following combinations in x86:
/// - reg <- reg/mem
/// - reg/mem <- reg
/// - reg/mem <- imm
fn write_general_opcode(opcode: Opcodes, src: &Operand, dst: &Operand, binary: &mut Binary) {
    match (src, dst) {
        // Immediate form
        (Operand::Imm(_), Operand::Stack(_) | Operand::Reg(..)) => {
            todo!()
        }
        // stack <= reg
        (Operand::Reg(..), Operand::Stack(_)) => {
            todo!()
        }
        // reg <= stack
        (Operand::Stack(_), Operand::Reg(..)) => {
            todo!()
        }
        // reg <= reg
        (Operand::Reg(..), Operand::Reg(..)) => {

        },

        // Nothing else should be possible.
        _ => unreachable!()
    }
}

impl Instr {
    fn write_as_binary(&self, _ctx: &Ctx, binary: &mut Binary) -> std::io::Result<()> {
        match self {
            Instr::Ret => binary.data.push(0xc3),
            Instr::Unary { op, operand } => todo!(),
            Instr::Binary { op, dst, src } => match src {
                Operand::Imm(_) => todo!(),
                _ => write_general_opcode(Opcodes {
                    mem_dst_reg_src: 0x01,
                    reg_dst_mem_src: 0x03,
                    mem_dst_imm32_src: 0x81
                }, src, dst, binary),
            },
            Instr::Cdq => todo!(),
            Instr::Cmp { lhs, rhs } => todo!(),
            Instr::Jmp(str_id) => todo!(),
            Instr::JmpCC(cond_code, str_id) => todo!(),
            Instr::SetCC(cond_code, operand) => todo!(),
            Instr::Idiv { rhs } => todo!(),
            Instr::Mov { src, dst } => todo!(),
            Instr::Shift { op, dst } => todo!(),
            Instr::Label(str_id) => todo!(),
        }

        Ok(())
    }
}