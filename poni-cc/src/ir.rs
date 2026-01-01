use crate::{ctx::StrId, lexer::TokenType};

pub struct Function {
    pub name: StrId,
    pub body: Vec<Instr>,
}

pub enum Instr {
    Return(Val),
    Unary {
        op: UnaryOp,
        // One reason to have separate src/dst here is for architectures like
        // aarch64, where we could e.g. re-use the non-negated value later.
        // Hmm...
        dst: Var,
    },
    Binary {
        op: BinaryOp,
        dst: Var,
        src1: Val,
        src2: Val,
    },
    Copy {
        src: Val,
        dst: Var,
    },
}

pub type Var = StrId;

#[derive(Clone, Copy)]
pub enum Val {
    // TODO: Probably integers? IDK
    Constant(StrId),
    Var(Var),
}

impl From<Var> for Val {
    fn from(value: Var) -> Self {
        Val::Var(value)
    }
}

#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Complement = b'~',
    Negate     = b'-',
}

#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add       = b'+',
    Subtract  = b'-',
    Multiply  = b'*',
    Divide    = b'/',
    Remainder = b'%',
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

impl BinaryOp {
    pub fn from(typ: TokenType) -> Self {
        match typ {
            TokenType::Plus    => BinaryOp::Add      ,
            TokenType::Minus   => BinaryOp::Subtract ,
            TokenType::Star    => BinaryOp::Multiply ,
            TokenType::Slash   => BinaryOp::Divide   ,
            TokenType::Percent => BinaryOp::Remainder,
            // TODO: ice! macro maybe? So that we can distinguish certain
            // panics from the possible panic-hook idea?
            _ => panic!("ICE: Bad BinaryOp conversion")
        }
    }
}