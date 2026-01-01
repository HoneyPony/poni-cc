//! Lowering from crate::ir into the x86_64 assembly.

use crate::{ir, x86_64::{self, Instr, Operand, Register}};

fn lower_val(val: &ir::Val) -> Operand {
    match val {
        ir::Val::Constant(str_id) => Operand::Imm(*str_id),
        ir::Val::Var(str_id) => todo!("we need psuedoregisters"),
    }
}

fn lower(instr: &ir::Instr, out: &mut Vec<x86_64::Instr>) {
    match instr {
        ir::Instr::Return(val) => {
            out.push(Instr::Mov { dst: Register::Eax.into(), src: lower_val(val) });
            out.push(Instr::Ret);
        },
        ir::Instr::Unary { op, src, dst } => {
            // TODO: This is very wasteful.
            // Instead,the Unary should just have a single Val, and we just
            // directly generate the Unary op for that Val. That would be much
            // better.
            out.push(Instr::Mov { src: lower_val(src), dst: lower_val(dst) });
            out.push(Instr::Unary { op: *op, operand: lower_val(dst) })
        },
    }
}

pub fn lower_function(function: &ir::Function) -> x86_64::Function {
    let mut instrs = Vec::new();

    for instr in &function.body {
        lower(instr, &mut instrs);
    }

    x86_64::Function { name: function.name, instructions: instrs }
}