use crate::{ctx::StrId, lexer::TokenType};

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

#[derive(Clone, Copy)]
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

impl UnaryOp {
    pub fn from(typ: TokenType) -> Self {
        match typ {
            TokenType::Tilde => UnaryOp::Complement,
            TokenType::Minus => UnaryOp::Negate,
            // TODO: ice! macro maybe? So that we can distinguish certain
            // panics from the possible panic-hook idea?
            _ => panic!("ICE: Bad UnaryOp conversion")
        }
    }
}