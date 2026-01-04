//! Lowering from crate::ir into the x86_64 assembly.

use crate::{ctx::Ctx, ir::{self, BinaryOp, UnaryOp}, x86_64::{self, CondCode, Instr, Operand, Register}};

fn lower_val(val: &ir::Val) -> Operand {
    match val {
        ir::Val::Constant(str_id) => Operand::Imm(*str_id),
        ir::Val::RValue(str_id) => Operand::Psuedo(*str_id),
        ir::Val::LValue(str_id) => Operand::Psuedo(*str_id),
        ir::Val::Tmp(str_id) => Operand::Psuedo(*str_id),
    }
}

fn lower_var(var: &ir::Var) -> Operand {
    Operand::Psuedo(*var)
}

// Checks if the given BinaryOp is relational, and if so, fetches the associated
// CondCode.
fn relational(op: BinaryOp) -> Option<CondCode> {
    match op {
        BinaryOp::Equal        => Some(CondCode::E),
        BinaryOp::NotEqual     => Some(CondCode::NE),
        BinaryOp::Less         => Some(CondCode::L),
        BinaryOp::LessEqual    => Some(CondCode::LE),
        BinaryOp::Greater      => Some(CondCode::G),
        BinaryOp::GreaterEqual => Some(CondCode::GE),
        _ => None,
    }
}

fn mov(out: &mut Vec<x86_64::Instr>, src: Operand, dst: Operand) {
    if src != dst {
        out.push(Instr::Mov { src, dst });
    }
}

fn lower(ctx: &mut Ctx, instr: &ir::Instr, out: &mut Vec<x86_64::Instr>) {
    match instr {
        ir::Instr::Return(val) => {
            out.push(Instr::Mov { dst: Register::Eax.into(), src: lower_val(val) });
            out.push(Instr::Ret);
        }
        ir::Instr::Unary { op, src, dst } => {
            if *op == UnaryOp::Not {
                // Special handling.
                let dst = lower_var(dst);
                out.push(Instr::Cmp { lhs: lower_val(src), rhs: Operand::Imm(ctx.zero()) });
                out.push(Instr::Mov { dst, src: Operand::Imm(ctx.zero()) });
                out.push(Instr::SetCC(CondCode::E, dst));
                return;
            }

            // We would like to get rid of the Mov where possible, as it's
            // pretty useless...
            mov(out, lower_val(src), lower_var(dst));
            out.push(Instr::Unary { op: *op, operand: lower_var(dst) })
        }
        ir::Instr::Copy { src, dst } => {
            out.push(Instr::Mov { src: lower_val(src), dst: lower_var(dst) })
        }
        ir::Instr::Binary { op, dst, lhs, rhs } => {
            if *op == BinaryOp::Divide || *op == BinaryOp::Remainder {
                // Move value into Eax & cdq to prepare for idivl instruction.
                out.push(Instr::Mov { src: lower_val(lhs), dst: Register::Eax.into() });
                out.push(Instr::Cdq);
                // We divide by src2. Constant values will be fixed-up in Fixup
                // pass. (TODO: Maybe we could architect that differently...?)
                out.push(Instr::Idiv { rhs: lower_val(rhs) });
                // Get the value out of Eax or Edx depending on if it was / or %.
                let src = match op {
                    BinaryOp::Divide => Register::Eax,
                    BinaryOp::Remainder => Register::Edx,
                    _ => unreachable!()
                };
                mov(out, src.into(), lower_var(dst));
                // Done.
                return;
            }

            // For shifts, we need the shift amount in the cl register.
            // (or as an 8 bit immediate, which we SHOULD do...)
            if *op == BinaryOp::Lshift || *op == BinaryOp::Rshift {
                out.push(Instr::Mov { src: lower_val(rhs), dst: Register::Ecx.into() });
                // Copy the lhs into dst, then do dst shift= ecx.
                let dst = lower_var(dst);
                mov(out, lower_val(lhs), dst);
                out.push(Instr::Shift { op: *op, dst });
                return;
            }

            // For relational ops, we generate cmp & setcc instructions.
            if let Some(cc) = relational(*op) {
                let dst = lower_var(dst);
                out.push(Instr::Cmp { lhs: lower_val(lhs), rhs: lower_val(rhs) });

                // We generate a Mov of 0 so that the full register is zeroed out.
                out.push(Instr::Mov { src: Operand::Imm(ctx.zero()), dst });
                // Need a 1 byte register. This is a bit awkward (?)
                out.push(Instr::SetCC(cc, dst.with_size(1)));

                return;
            }

            let dst = lower_var(dst);
            // Copy src1 into dst, then do dst op= src2.
            mov(out, lower_val(lhs), dst);
            out.push(Instr::Binary { op: *op, dst, src: lower_val(rhs) })
        },
        ir::Instr::Jump(label) => {
            out.push(Instr::Jmp(*label));
        }
        ir::Instr::JumpIfZero { condition, target } => {
            out.push(Instr::Cmp { lhs: Operand::Imm(ctx.zero()), rhs: lower_val(condition) });
            out.push(Instr::JmpCC(CondCode::E, *target));
        }
        ir::Instr::JumpIfNotZero { condition, target } => {
            out.push(Instr::Cmp { lhs: Operand::Imm(ctx.zero()), rhs: lower_val(condition) });
            out.push(Instr::JmpCC(CondCode::NE, *target));
        }
        ir::Instr::Label(label) => {
            out.push(Instr::Label(*label));
        }
    }
}

pub fn lower_function(ctx: &mut Ctx, function: &ir::Function) -> x86_64::Function {
    let mut instrs = Vec::new();

    for instr in &function.body {
        lower(ctx, instr, &mut instrs);
    }

    x86_64::Function { name: function.name, stack_size: 0, instructions: instrs }
}