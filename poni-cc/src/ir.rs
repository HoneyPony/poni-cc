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
        //
        // The other reason, apparently, is because we don't want to accidentally
        // overwrite an existing variable. Although this might only be a problem
        // with how we implemented !. Either way, back to the old model.
        src: Val,
        dst: Var,
    },
    Binary {
        op: BinaryOp,
        dst: Var,
        lhs: Val,
        rhs: Val,
    },
    Copy {
        src: Val,
        dst: Var,
    },
    Jump(Label),
    JumpIfZero {
        condition: Val,
        target: Label,
    },
    JumpIfNotZero {
        condition: Val,
        target: Label,
    },
    Label(Label),
}

pub type Var = StrId;
pub type Label = StrId;

#[derive(Clone, Copy)]
pub enum Val {
    // TODO: Probably integers? IDK
    Constant(StrId),
    RValue(Var),

    /// This may change in the future.
    /// 
    /// For now, LValues are *merely* Vars, but with the ability to assign
    /// to them (and maybe to take their address)?
    /// 
    /// Likely we will need a different representation for arrays, maybe
    /// just with an offset field or something.
    LValue(Var),
}

impl Val {
    /// Converts the given Val to an equivalent Val that is an RValue instead
    /// of an LValue.
    pub fn to_rvalue(&self) -> Val {
        match self {
            Val::LValue(v) => Val::RValue(*v),
            _ => *self
        }
    }
}

impl From<Var> for Val {
    fn from(value: Var) -> Self {
        Val::RValue(value)
    }
}

#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Complement = b'~',
    Negate     = b'-',
    Not        = b'!',
}

#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add       = b'+',
    Subtract  = b'-',
    Multiply  = b'*',
    Divide    = b'/',
    Remainder = b'%',
    And       = b'&',
    Or        = b'|',
    Xor       = b'^',
    Less      = b'<',
    Greater   = b'>',
    LessEqual,
    GreaterEqual,
    Equal,
    NotEqual,
    Lshift,
    Rshift,
}

impl UnaryOp {
    pub fn from(typ: TokenType) -> Self {
        match typ {
            TokenType::Tilde => UnaryOp::Complement,
            TokenType::Minus => UnaryOp::Negate,
            TokenType::Bang  => UnaryOp::Not,
            // TODO: ice! macro maybe? So that we can distinguish certain
            // panics from the possible panic-hook idea?
            _ => panic!("ICE: Bad UnaryOp conversion")
        }
    }
}

impl BinaryOp {
    pub fn from(typ: TokenType) -> Self {
        // TODO: we could consider splitting this into two functions:
        //     from_op
        // and
        //     from_assign_op
        // That way we don't waste time considering cases it can't be. Although,
        // if the compiler inlines this function, it will probably be fine either
        // way.
        match typ {
            // Arithmetic operations
            TokenType::Plus    | TokenType::PlusEqual    => BinaryOp::Add      ,
            TokenType::Minus   | TokenType::MinusEqual   => BinaryOp::Subtract ,
            TokenType::Star    | TokenType::StarEqual    => BinaryOp::Multiply ,
            TokenType::Slash   | TokenType::SlashEqual   => BinaryOp::Divide   ,
            TokenType::Percent | TokenType::PercentEqual => BinaryOp::Remainder,

            TokenType::PlusPlus   => BinaryOp::Add,
            TokenType::MinusMinus => BinaryOp::Subtract,

            // Bitwise operations
            TokenType::Ampersand      | TokenType::AmpersandEqual      => BinaryOp::And,
            TokenType::Pipe           | TokenType::PipeEqual           => BinaryOp::Or,
            TokenType::Caret          | TokenType::CaretEqual          => BinaryOp::Xor,
            TokenType::LessLess       | TokenType::LessLessEqual       => BinaryOp::Lshift,
            TokenType::GreaterGreater | TokenType::GreaterGreaterEqual => BinaryOp::Rshift,

            // Comparison operators
            TokenType::Less         => BinaryOp::Less,
            TokenType::LessEqual    => BinaryOp::LessEqual,
            TokenType::Greater      => BinaryOp::Greater,
            TokenType::GreaterEqual => BinaryOp::GreaterEqual,
            TokenType::EqualEqual   => BinaryOp::Equal,
            TokenType::BangEqual    => BinaryOp::NotEqual,

            // TODO: ice! macro maybe? So that we can distinguish certaain
            // panics from the possible panic-hook idea?
            _ => panic!("ICE: Bad BinaryOp conversion")
        }
    }
}