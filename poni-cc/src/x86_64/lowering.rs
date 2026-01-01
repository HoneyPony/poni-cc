//! Lowering from crate::ir into the x86_64 assembly.

use crate::{ir, x86_64::{self, Instr, Operand, Register}};

fn lower_val(val: &ir::Val) -> Operand {
    match val {
        ir::Val::Constant(str_id) => Operand::Imm(*str_id),
        ir::Val::Var(str_id) => Operand::Psuedo(*str_id),
    }
}

fn lower_var(var: &ir::Var) -> Operand {
    Operand::Psuedo(*var)
}

fn lower(instr: &ir::Instr, out: &mut Vec<x86_64::Instr>) {
    match instr {
        ir::Instr::Return(val) => {
            out.push(Instr::Mov { dst: Register::Eax.into(), src: lower_val(val) });
            out.push(Instr::Ret);
        }
        ir::Instr::Unary { op, dst } => {
            out.push(Instr::Unary { op: *op, operand: lower_var(dst) })
        }
        ir::Instr::Copy { src, dst } => {
            out.push(Instr::Mov { src: lower_val(src), dst: lower_var(dst) })
        }
        ir::Instr::Binary { op, dst, src1, src2 } => {
            let dst = lower_var(dst);
            // Copy src1 into dst, then do dst op= src2.
            out.push(Instr::Mov { src: lower_val(src1), dst });
            out.push(Instr::Binary { op: *op, dst, src: lower_val(src2) })
        }
    }
}

pub fn lower_function(function: &ir::Function) -> x86_64::Function {
    let mut instrs = Vec::new();

    for instr in &function.body {
        lower(instr, &mut instrs);
    }

    x86_64::Function { name: function.name, stack_size: 0, instructions: instrs }
}