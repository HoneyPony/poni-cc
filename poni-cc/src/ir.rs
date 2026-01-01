use crate::ctx::StrId;

pub struct Function {
    pub name: StrId,
    pub body: Vec<Instr>,
}

pub enum Instr {
    Return(Val),
    Unary {
        op: UnaryOp,
        src: Val,
        dst: Val,
    }
}

pub enum Val {
    // TODO: Probably integers? IDK
    Constant(StrId),
    Var(StrId),
}

#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Complement = b'~',
    Negate     = b'-',
}
