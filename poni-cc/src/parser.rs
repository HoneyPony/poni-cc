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

use crate::{ctx::Ctx, lexer::{Lexer, Token, TokenType}};

pub struct Parser {
    next_token: Token,
    lexer: Lexer,
}

impl Parser {
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

    pub fn program(&mut self, ctx: &mut Ctx) {
        self.function(ctx);
    }

    pub fn function(&mut self, ctx: &mut Ctx) {
        self.expect(ctx, TokenType::Int);
        self.expect(ctx, TokenType::Identifier);
        self.expect(ctx, TokenType::LParen);
        self.expect(ctx, TokenType::Void);
        self.expect(ctx, TokenType::RParen);
        self.expect(ctx, TokenType::LBrace);

        let inner = self.statement(ctx);

        self.expect(ctx, TokenType::RBrace);
    }

    pub fn statement(&mut self, ctx: &mut Ctx) {
        self.expect(ctx, TokenType::Return);
        let return_val = self.expression(ctx);
        self.expect(ctx, TokenType::Semicolon);

    }

    pub fn expression(&mut self, ctx: &mut Ctx) {
        let value = self.expect(ctx, TokenType::Constant);

    }
}