//! The parser for poni-cc.
//! 
//! The main goal is to primarily operate through syntax-directed translation;
//! in particular, a single-pass compiler *should* be reasonably fast, which
//! is one of the main goals of this particular project.
//! 
//! This does mean we combine all three of:
//! - Parsing
//! - Semantic analysis (typechecking)
//! - Code generation
//! 
//! Which is a little ugly. But on the other hand, it is also sometimes quite
//! pleasant.
//! 
//! Of course, we would *also* like to provide an API for generating compiled
//! code from other applications. I'm not sure exactly how that should fit in;
//! perhaps the Parser calls into something else..?

use std::io::Read;

use crate::{ctx::Ctx, lexer::{Lexer, Token, TokenType}, x86_64};

pub struct Parser {
    next_token: Token,
    lexer: Lexer,
}

impl Parser {
    pub fn new(input: Box<dyn Read>, ctx: &mut Ctx) -> Self {
        let mut result = Parser {
            next_token: Token { typ: TokenType::Eof, str: None },
            lexer: Lexer::new(input, ctx)
        };

        // Prime the parser so it has a legit token.
        result.advance(ctx);
        result
    }

    fn advance(&mut self, ctx: &mut Ctx) -> Token {
        let result = self.next_token;
        self.next_token = self.lexer.next(ctx).unwrap();

        result
    }

    fn expect(&mut self, ctx: &mut Ctx, typ: TokenType) -> Token {
        if self.next_token.typ != typ {
            panic!("expected {} got {}", typ, self.next_token.typ);
        }
        self.advance(ctx)
    }

    pub fn program(&mut self, ctx: &mut Ctx) -> x86_64::Program {
        let mut p = x86_64::Program {
            functions: Vec::new(),
        };

        p.functions.push(self.function(ctx));
        
        self.expect(ctx, TokenType::Eof);

        // TODO: We'll probably want to codegen functions as we go, not
        // push them all to a thing first.
        p
    }

    pub fn function(&mut self, ctx: &mut Ctx) -> x86_64::Function {
        let mut instructions: Vec<x86_64::Instr> = Vec::new();

        self.expect(ctx, TokenType::Int);
        let ident = self.expect(ctx, TokenType::Identifier);
        self.expect(ctx, TokenType::LParen);
        self.expect(ctx, TokenType::Void);
        self.expect(ctx, TokenType::RParen);
        self.expect(ctx, TokenType::LBrace);

        let inner = self.statement(ctx, &mut instructions);

        self.expect(ctx, TokenType::RBrace);

        // TODO: We probably want to make the ident.str just a property of
        // e.g. TokenType::Identifier, so that we don't have to call .unwrap()
        // here.
        x86_64::Function { name: ident.str.unwrap(), instructions: vec![] }
    }

    pub fn statement(&mut self, ctx: &mut Ctx, into: &mut Vec<x86_64::Instr>) {
        self.expect(ctx, TokenType::Return);
        let return_val = self.expression(ctx, into);
        self.expect(ctx, TokenType::Semicolon);

        // Generate the code for return
        into.push(x86_64::Instr::Mov { dst: x86_64::Operand::Reg(x86_64::Register::Eax), src: return_val });
        into.push(x86_64::Instr::Ret);
    }

    pub fn expression(&mut self, ctx: &mut Ctx, into: &mut Vec<x86_64::Instr>) -> x86_64::Operand {
        let value = self.expect(ctx, TokenType::Constant);
        // Same thing as above
        //
        // Also, TODO: Validate that this StrId is a valid integer constant,
        // and/or convert it if necessary.
        x86_64::Operand::Imm(value.str.unwrap())
    }
}