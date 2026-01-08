use std::io::Write;

use crate::{ctx::Ctx, elf::{ElfWriter, Symbol}, x86_64::*};

/// Intermediate struct for binary code. Will probably eventually need to be
/// ELF related or something?
/// 
/// We need to keep track of our offset in the binary so that we can compute
/// offsets, eventually.
struct Binary {
    // Text section
    text: Vec<u8>,
    // Data section
    #[expect(unused)]
    data: Vec<u8>,

    /// Offsets of labels from the text section
    label_offsets: FxHashMap<StrId, usize>,

    /// List of offsets to patch with a specific symbol
    fixups: Vec<(StrId, usize)>,
}

impl Binary {
    pub fn new() -> Self {
        Binary {
            text: Vec::new(),
            data: Vec::new(),

            // TODO: Probably instead of using a HashMap, we want to just store
            // this stuff inline in the Ctx.
            label_offsets: FxHashMap::default(),
            fixups: Vec::new(),
        }
    }
    
    fn fixup(&mut self) {
        for fixup in &self.fixups {
            if let Some(known) = self.label_offsets.get(&fixup.0) {
                // If we found the fixup, we generate a rel32 operand. This looks
                // like:
                //     target - address_of_instruction_after_me
                //
                // In theory, all of the addresses of the instruction after me
                // should just be the fixup.1 + 4, because we always generate
                // 4 bytes after the fixup.1.

                // Convert everything to i64. That way, we can easily tell
                // if the address fits in i32, without any shenanigans.
                let address_of_instruction_after_me = fixup.1 as i64 + 4;
                let target = *known as i64;

                let displacement = target - address_of_instruction_after_me;
                let value = i32::try_from(displacement)
                    // It's an error if we can't fit the displacement.
                    .expect("can't fit displacement");

                // Copy the value into the text
                let dest = &mut self.text[fixup.1..fixup.1 + 4];
                dest.copy_from_slice(&value.to_le_bytes());
            }
        }
    }
}

impl Program {
    pub fn write_as_binary<W: Write>(&self, ctx: &Ctx, output: &mut W) -> std::io::Result<()> {
        let mut binary = Binary::new();

        for fun in &self.functions {
            fun.write_as_binary(ctx, &mut binary)?;
        }

        binary.fixup();

        let mut elf = ElfWriter::new(output);
        let symbols = [
            Symbol {
                name: b"main",
                offset: 0,
            }
        ];
        elf.write(&binary.text, &symbols)?;

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

        // It's not very principled, but for now, we can hardcode the function
        // prelude.
        binary.text.write_all(&[0x55])?; //pushq %rbp 
        binary.text.write_all(&[0x48, 0x89, 0xe5])?; // movq %rsp, %rbp

        // subq $imm32 sign extended, %rsp (more or less)
        //
        // Note that the 0x83 encoding of subq with an 8 bit (?) immediate
        // is much more efficient in some cases. TODO: Consider writing good
        // code.
        do_write_imm(/* hack rex to have W bit */0b1000,
            Some((&[0x83], RegField::Constant(5))), (&[0x81], RegField::Constant(5)),
            &Operand::Reg(Register::Esp, 8), self.stack_size,
            binary);

        for instr in &self.instructions {
            instr.write_as_binary(ctx, binary)?;
        }

        Ok(())
    }
}

struct Opcodes {
    mem_dst_reg_src: &'static [u8],
    reg_dst_mem_src: &'static [u8],
    mem_dst_sext_imm8_src: Option<(&'static [u8], RegField)>,
    mem_dst_imm32_src: (&'static[u8], RegField),
}

struct SpecialBytes {
    rex: u8,
    modrm: u8,
    disp: [u8; 4],
    disp_len: u8,
}

const REX_EMPTY: u8 = 0b0100_0000;

enum RegField {
    Constant(u8),
    DupOperand,
}

fn get_special_bytes_imm(mem_reg_op: &Operand, reg_field: RegField) -> SpecialBytes {
    let mod_;
    // Note that we DO check both RegField::Constant and RegField::DupOperand
    // for initializing this eventually.
    let mut reg= 0b000;
    let rm;

    if let RegField::Constant(c) = reg_field {
        reg = c;
    }

    let mut disp: [u8; 4] = [0; 4];
    let mut disp_len = 0;

    //let mut sib = None;

    // TODO: We will also need to use rex to index byte registers sometimes.
    let mut rex = REX_EMPTY;
    match mem_reg_op {
        Operand::Reg(r, _) => {
            mod_ = 0b11;
            rm = r.low_3();
            rex |= r.high_bit();

            if matches!(reg_field, RegField::DupOperand) {
                reg = r.low_3();
                rex |= r.high_bit() << 2;
            }
        },
        Operand::Stack(offset) => {
            // Always use SIB with disp32?
            // mod_ = 0b10;
            // rm = 0b100;

            // let sib_byte = 
            //       0b00  << 6 // scale doesn't matter, we don't index
            //     | 0b100 << 3 // index is 0b100 = esp = don't have an index.
            //     | 0b101;     // always use RBP as the base.
            
            // let sib_offset = *offset as u32;

            // sib = Some((sib_byte, sib_offset))

            // For now, we use either EBP + disp8 or EBP + disp32, depending on
            // the size of the offset.
            //
            // We could also considering using a SIB when it seems relevant.
            //
            // (Note that we can't use EBP + disp0 as that has special meaning.)
            rm = 0b101; // use ebp
            if *offset <= 127 && *offset >= -128 {
                mod_ = 0b01; // ebp + disp8
                disp[0] = *offset as u8;
                disp_len = 1;
            }
            else {
                mod_ = 0b10; // ebp + disp32
                disp.copy_from_slice(&offset.to_le_bytes());
                disp_len = 4;
            }

            if matches!(reg_field, RegField::DupOperand) {
                panic!("bad assembly: can't have a memory operand here.");
            }
        },
        _ => unreachable!()
    }

    SpecialBytes {
        rex,
        modrm: mod_ << 6 | reg << 3 | rm,
        disp,
        disp_len,
    }
}

fn get_special_bytes(mem_reg_op: &Operand, reg_op: &Operand) -> SpecialBytes {
    let mod_;
    let reg;
    let rm;

    let mut disp: [u8; 4] = [0; 4];
    let mut disp_len = 0;

    //let mut sib = None;

    // TODO: We will also need to use rex to index byte registers sometimes.
    let mut rex = REX_EMPTY;
    match mem_reg_op {
        Operand::Reg(r, _) => {
            mod_ = 0b11;
            rm = r.low_3();
            rex |= r.high_bit();
        },
        Operand::Stack(offset) => {
            // // Always use SIB with disp32?
            // mod_ = 0b10;
            // rm = 0b100;

            // let sib_byte = 
            //       0b00  << 6 // scale doesn't matter, we don't index
            //     | 0b100 << 3 // index is 0b100 = esp = don't have an index.
            //     | 0b101;     // always use RBP as the base.
            
            // let sib_offset = *offset as u32;

            // sib = Some((sib_byte, sib_offset))

            // Same as above.
            rm = 0b101; // use ebp
            if *offset <= 127 && *offset >= -128 {
                mod_ = 0b01; // ebp + disp8
                disp[0] = *offset as u8;
                disp_len = 1;
            }
            else {
                mod_ = 0b10; // ebp + disp32
                disp.copy_from_slice(&offset.to_le_bytes());
                disp_len = 4;
            }
        },
        _ => unreachable!()
    }

    // Fill out the reg field and the R field.
    match reg_op {
        Operand::Reg(r, _) => {
            reg = r.low_3();
            rex |= r.high_bit() << 2;
        }
        _ => unreachable!()
    }

    SpecialBytes { rex, modrm: mod_ << 6 | reg << 3 | rm, disp, disp_len }
}

fn do_write_imm(/* hack parameter for sub $imm, %rsp */ rex_oreq: u8, imm8: Option<(&[u8], RegField)>, imm32: (&[u8], RegField), mem_reg_op: &Operand, imm_op: i32, binary: &mut Binary) {
    let (opcode, operand, mut bytes) = 
        if imm_op >= -128 && imm_op <= 127 && let Some(imm8) = imm8 {
            let byte = imm_op as u8;
            (imm8.0, &byte.to_le_bytes()[..], get_special_bytes_imm(mem_reg_op, imm8.1))
        }
        else {
            (imm32.0, &imm_op.to_le_bytes()[..], get_special_bytes_imm(mem_reg_op, imm32.1))
        };
    bytes.rex |= rex_oreq;

    if bytes.rex != REX_EMPTY {
        binary.text.push(bytes.rex);
    }
    binary.text.extend_from_slice(opcode);
    binary.text.push(bytes.modrm);

    binary.text.extend_from_slice(&bytes.disp[0..bytes.disp_len as usize]);

    // TODO:  Get rid of SpecialBytes::operand field, which is useless
    binary.text.extend_from_slice(operand);
    // if let Some(op) = bytes.operand {
    //     binary.text.extend_from_slice(&op.to_le_bytes());
    // }
}

fn do_write(opcode: &[u8], mem_reg_op: &Operand, reg_op: &Operand, binary: &mut Binary) {
    let bytes = get_special_bytes(mem_reg_op, reg_op);

    if bytes.rex != REX_EMPTY {
        binary.text.push(bytes.rex);
    }
    binary.text.extend_from_slice(opcode);
    binary.text.push(bytes.modrm);
    
    binary.text.extend_from_slice(&bytes.disp[0..bytes.disp_len as usize]);
}

fn write_general_unary(opcode: (&[u8], u8), op: &Operand, binary: &mut Binary) {
    // TODO: We really need to clean up this repeated logic.
    let mod_;
    // For instructions with no /N field, I guess just use 0.
    let reg  = opcode.1;
    let rm;
    let mut rex = REX_EMPTY;

    let mut disp = [0; 4];
    let mut disp_len = 0;

    match op {
        Operand::Reg(r, _) => {
            mod_ = 0b11;
            rm = r.low_3();
            rex |= r.high_bit();
        },
        Operand::Stack(offset) => {
            // Same as above.
            rm = 0b101; // use ebp
            if *offset <= 127 && *offset >= -128 {
                mod_ = 0b01; // ebp + disp8
                disp[0] = *offset as u8;
                disp_len = 1;
            }
            else {
                mod_ = 0b10; // ebp + disp32
                disp.copy_from_slice(&offset.to_le_bytes());
                disp_len = 4;
            }
        },
        _ => unreachable!()
    }

    if rex != REX_EMPTY {
        binary.text.push(rex);
    }
    binary.text.extend_from_slice(opcode.0);
    binary.text.push(mod_ << 6 | reg << 3 | rm);

    binary.text.extend_from_slice(&disp[0..disp_len]);

}

/// Helper function for writing the following combinations in x86:
/// - reg <- reg/mem
/// - reg/mem <- reg
/// - reg/mem <- imm
fn write_general_opcode(ctx: &Ctx, opcode: Opcodes, src: &Operand, dst: &Operand, binary: &mut Binary) {
    match (src, dst) {
        // Immediate form
        (Operand::Imm(val), Operand::Stack(_) | Operand::Reg(..)) => {
            let mut buf = [0; 8];
            let imm_str = ctx.get(val,  &mut buf);

            // TODO: More principled integer value checking.
            let imm_val = i32::from_str_radix(imm_str, 10)
                .expect("bad integer constant");

            do_write_imm(0, opcode.mem_dst_sext_imm8_src, opcode.mem_dst_imm32_src, dst, imm_val, binary)
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
            //
            // Prefer reg_dst_mem_src because it should work for imul.
            do_write(opcode.reg_dst_mem_src, src, dst, binary);
        },

        // Nothing else should be possible.
        _ => panic!("ice: unknown instruction format src {} dst {}",
            src.fmt_name(), dst.fmt_name())
    }
}

fn binop_table(op: BinaryOp) -> Opcodes {
    match op {
        BinaryOp::Add => Opcodes {
            mem_dst_reg_src: &[0x01],
            reg_dst_mem_src: &[0x03],
            mem_dst_sext_imm8_src: Some((&[0x83], RegField::Constant(0))),
            mem_dst_imm32_src: (&[0x81], RegField::Constant(0)),
        },
        BinaryOp::Subtract => Opcodes {
            mem_dst_reg_src: &[0x29],
            reg_dst_mem_src: &[0x2B],
            mem_dst_sext_imm8_src: Some((&[0x83], RegField::Constant(5))),
            mem_dst_imm32_src: (&[0x81], RegField::Constant(5)),
        },
        BinaryOp::Multiply => Opcodes {
            mem_dst_reg_src: &[], // Not possible (?)
            reg_dst_mem_src: &[0x0F, 0xAF],
            // Same as below.
            mem_dst_sext_imm8_src: Some((&[0x6B], RegField::DupOperand)),
            // In theory this is the 0x69 encoding, that dups its operand so
            // that everything is fine.
            mem_dst_imm32_src: (&[0x69], RegField::DupOperand),
        },
        BinaryOp::Divide => panic!("divide should have lowered to idiv"),
        BinaryOp::Remainder => panic!("remainder should have lowered to idiv"),
        BinaryOp::And => Opcodes {
            mem_dst_reg_src: &[0x21],
            reg_dst_mem_src: &[0x23],
            mem_dst_sext_imm8_src: Some((&[0x83], RegField::Constant(4))),
            mem_dst_imm32_src: (&[0x81], RegField::Constant(4)),
        },
        BinaryOp::Or => Opcodes {
            mem_dst_reg_src: &[0x09],
            reg_dst_mem_src: &[0x0B],
            mem_dst_sext_imm8_src: Some((&[0x83], RegField::Constant(1))),
            mem_dst_imm32_src: (&[0x81], RegField::Constant(1)),
        },
        BinaryOp::Xor => Opcodes {
            mem_dst_reg_src: &[0x31],
            reg_dst_mem_src: &[0x33],
            mem_dst_sext_imm8_src: Some((&[0x83], RegField::Constant(6))),
            mem_dst_imm32_src: (&[0x81], RegField::Constant(6)),
        },
        BinaryOp::Less | BinaryOp::Greater | BinaryOp::LessEqual | BinaryOp::GreaterEqual | BinaryOp::Equal | BinaryOp::NotEqual => 
            panic!("comparison should have lowered to Cmp"),

        BinaryOp::Lshift | BinaryOp::Rshift =>
            panic!("shift should have lowered to Shift"),
    }
}

impl Instr {
    fn write_as_binary(&self, ctx: &Ctx, binary: &mut Binary) -> std::io::Result<()> {
        match self {
            Instr::Ret => {
                // It's not very principled, but for now just hackily add on:
                // mov %rbp, %rsp
                binary.text.write_all(&[0x48, 0x89, 0xec])?;
                // pop %rbp
                binary.text.write_all(&[0x5d])?;

                // The actual ret instruction:
                binary.text.push(0xc3);
            }
            Instr::Unary { op, operand } => {
                let opcode: (&[u8], _) = match op {
                    UnaryOp::Complement => (&[0xF7], 2), // notl
                    UnaryOp::Negate     => (&[0xF7], 3), // negl
                    UnaryOp::Not => panic!("! should have lowered to Cmp"),
                };
                write_general_unary(opcode, operand, binary);
            }
            Instr::Binary { op, dst, src } => {
                write_general_opcode(ctx, binop_table(*op), src, dst, binary);
            },
            Instr::Cdq => binary.text.push(0x99),
            Instr::Cmp { lhs, rhs } => {
                let op = Opcodes {
                    mem_dst_reg_src: &[0x39],
                    reg_dst_mem_src: &[0x3B],
                    mem_dst_sext_imm8_src: Some((&[0x83], RegField::Constant(7))),
                    mem_dst_imm32_src: (&[0x81], RegField::Constant(7)),
                };
                write_general_opcode(ctx, op, rhs, lhs, binary);
            },
            Instr::Jmp(str_id) => {
                binary.text.push(0xE9); // JMP rel32

                let fixup_addr = binary.text.len();

                // Don't push a label yet (?)
                binary.text.push(0);
                binary.text.push(0);
                binary.text.push(0);
                binary.text.push(0);

                // Add us to the fixup list
                binary.fixups.push((*str_id, fixup_addr));
            },
            Instr::JmpCC(cond_code, str_id) => {
                // We want rel32...?
                binary.text.push(0x0F);
                binary.text.push(cond_code.near_jmp_opcode());

                let fixup_addr = binary.text.len();

                // Don't push a label yet (?)
                binary.text.push(0);
                binary.text.push(0);
                binary.text.push(0);
                binary.text.push(0);

                // Add us to the fixup list
                binary.fixups.push((*str_id, fixup_addr));
            },
            Instr::SetCC(cond_code, operand) => {
                let op: (&[u8], _) = match cond_code {
                    // These don't have a /N, so I guess just use 0??
                    CondCode::E =>  (&[0x0F, 0x94], 0),
                    CondCode::NE => (&[0x0F, 0x95], 0),
                    CondCode::L =>  (&[0x0F, 0x9C], 0),
                    CondCode::LE => (&[0x0F, 0x9E], 0),
                    CondCode::G =>  (&[0x0F, 0x9F], 0),
                    CondCode::GE => (&[0x0F, 0x9D], 0),
                };
                write_general_unary(op, operand, binary);
            },
            Instr::Idiv { rhs } => {
                write_general_unary((&[0xF7], 7), rhs, binary);
            },
            Instr::Mov { src, dst } => {
                let op = Opcodes {
                    mem_dst_reg_src: &[0x89],
                    reg_dst_mem_src: &[0x8B],
                    mem_dst_sext_imm8_src: None,
                    mem_dst_imm32_src: (&[0xC7], RegField::Constant(0))
                };
                write_general_opcode(ctx, op, src, dst, binary);
            },
            Instr::Shift { op, dst } => {
                // Right now this is effectively a unary op, because we get
                // one destination operand and everything else is CL.
                //
                // If we ever want to also support the imm8 encoding, we will
                // need slightly different handling.
                let opcode: (&[u8], _) = match op {
                    BinaryOp::Lshift => (&[0xD3], 4), // shll
                    BinaryOp::Rshift => (&[0xD3], 7), // sarl
                    _ => panic!("not a shift")
                };
                write_general_unary(opcode, dst, binary);
            },
            Instr::Label(str_id) => {
                binary.label_offsets.insert(*str_id, binary.text.len());
            },
        }

        Ok(())
    }
}