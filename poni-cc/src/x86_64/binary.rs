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
    mem_dst_imm32_src: (u8, u8),
}

struct SpecialBytes {
    rex: u8,
    modrm: u8,
    sib: Option<(u8, u32)>,
    operand: Option<u32>,
}

const REX_EMPTY: u8 = 0b0100_0000;

fn get_special_bytes_imm(mem_reg_op: &Operand, reg_field: u8, imm: i32) -> SpecialBytes {
    let mut mod_ = 0b00u8;
    let mut rm   = 0b000u8;

    let mut sib = None;

    // TODO: We will also need to use rex to index byte registers sometimes.
    let mut rex = REX_EMPTY;
    match mem_reg_op {
        Operand::Reg(r, _) => {
            mod_ = 0b11;
            rm = r.low_3();
            rex |= r.high_bit();
        },
        Operand::Stack(offset) => {
            // Always use SIB with disp32?
            mod_ = 0b10;
            rm = 0b100;

            let sib_byte = 
                  0b00  << 6 // scale doesn't matter, we don't index
                | 0b100 << 3 // index is 0b100 = esp = don't have an index.
                | 0b101;     // always use RBP as the base.
            
            let sib_offset = *offset as u32;

            sib = Some((sib_byte, sib_offset))
        },
        _ => unreachable!()
    }

    SpecialBytes {
        rex,
        modrm: mod_ << 6 | reg_field << 3 | rm,
        sib,
        operand: Some(imm as u32)
    }
}

fn get_special_bytes(mem_reg_op: &Operand, reg_op: &Operand) -> SpecialBytes {
    let mut mod_ = 0b00u8;
    let mut reg  = 0b000u8;
    let mut rm   = 0b000u8;

    let mut sib = None;

    // TODO: We will also need to use rex to index byte registers sometimes.
    let mut rex = REX_EMPTY;
    match mem_reg_op {
        Operand::Reg(r, _) => {
            mod_ = 0b11;
            rm = r.low_3();
            rex |= r.high_bit();
        },
        Operand::Stack(offset) => {
            // Always use SIB with disp32?
            mod_ = 0b10;
            rm = 0b100;

            let sib_byte = 
                  0b00  << 6 // scale doesn't matter, we don't index
                | 0b100 << 3 // index is 0b100 = esp = don't have an index.
                | 0b101;     // always use RBP as the base.
            
            let sib_offset = *offset as u32;

            sib = Some((sib_byte, sib_offset))
        },
        _ => unreachable!()
    }

    // Fill out the reg field and the R field.
    match reg_op {
        Operand::Reg(r, _) => {
            reg = r.low_3();
            rex |= (r.high_bit() << 2);
        }
        _ => unreachable!()
    }

    SpecialBytes { rex, modrm: mod_ << 6 | reg << 3 | rm, sib, operand: None }
}

fn do_write_imm(opcode: u8, reg_field: u8, mem_reg_op: &Operand, imm_op: i32, binary: &mut Binary) {
    let bytes = get_special_bytes_imm(mem_reg_op, reg_field, imm_op);

    if bytes.rex != REX_EMPTY {
        binary.text.push(bytes.rex);
    }
    binary.text.push(opcode);
    binary.text.push(bytes.modrm);
    
    if let Some(sib) = bytes.sib {
        binary.text.push(sib.0);
        binary.text.extend_from_slice(&sib.1.to_le_bytes());
    }

    if let Some(op) = bytes.operand {
        binary.text.extend_from_slice(&op.to_le_bytes());
    }
}

fn do_write(opcode: u8, mem_reg_op: &Operand, reg_op: &Operand, binary: &mut Binary) {
    let bytes = get_special_bytes(mem_reg_op, reg_op);

    if bytes.rex != REX_EMPTY {
        binary.text.push(bytes.rex);
    }
    binary.text.push(opcode);
    binary.text.push(bytes.modrm);
    
    if let Some(sib) = bytes.sib {
        binary.text.push(sib.0);
        binary.text.extend_from_slice(&sib.1.to_le_bytes());
    }
}

/// Helper function for writing the following combinations in x86:
/// - reg <- reg/mem
/// - reg/mem <- reg
/// - reg/mem <- imm
fn write_general_opcode(ctx: &Ctx, opcode: Opcodes, src: &Operand, dst: &Operand, binary: &mut Binary) {
    match (src, dst) {
        // Immediate form
        (Operand::Imm(val), Operand::Stack(_) | Operand::Reg(..)) => {
            let imm_val = 0;
            do_write_imm(opcode.mem_dst_imm32_src.0, opcode.mem_dst_imm32_src.1, dst, imm_val, binary)
        }
        // stack <= reg
        (Operand::Reg(..), Operand::Stack(_)) => {
            do_write(opcode.mem_dst_reg_src, dst, src, binary);
        }
        // reg <= stack
        (Operand::Stack(_), Operand::Reg(..)) => {
            do_write(opcode.reg_dst_mem_src, src, dst, binary);
        }
        // reg <= reg
        (Operand::Reg(..), Operand::Reg(..)) => {
            // IDK which thing is the best one here.
            do_write(opcode.reg_dst_mem_src, src, dst, binary);
        },

        // Nothing else should be possible.
        _ => unreachable!()
    }
}

impl Instr {
    fn write_as_binary(&self, ctx: &Ctx, binary: &mut Binary) -> std::io::Result<()> {
        match self {
            Instr::Ret => binary.data.push(0xc3),
            Instr::Unary { op, operand } => todo!(),
            Instr::Binary { op, dst, src } => match src {
                Operand::Imm(_) => todo!(),
                _ => write_general_opcode(ctx, Opcodes {
                    mem_dst_reg_src: 0x01,
                    reg_dst_mem_src: 0x03,
                    mem_dst_imm32_src: (0x81, 0b000)
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