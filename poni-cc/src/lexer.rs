//! The lexer.
//! 
//! This is a hand-implemented lexer. The main idea is to match tokens into
//! strings, which are then matched against keywords. Then, operators and other
//! special characters are handled specially.

use std::io::Read;

use crate::ctx::{Ctx, StrId};

pub struct Lexer {
    next_byte: u8,
    at_eof: bool,

    input: Box<dyn Read>,

    next_buf: Vec<u8>,

    /// A Map from StrId's to TokenType's for our keywords. Use a Vec instead
    /// of a HashMap as there really aren't very many.
    keyword_map: Vec<(StrId, TokenType)>,
}

#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    Identifier,
    Constant,
    
    Int, Void,

    Return,
    LParen = b'(', RParen = b')',

    LBrace = b'{', RBrace = b'}',

    Semicolon = b';',

    Eof,
}

impl TokenType {
    pub fn to_str_const(&self ) -> &'static str {
        match self {
            TokenType::Identifier => "identifier",
            TokenType::Constant => "numerical constant",
            TokenType::Int => "'int'",
            TokenType::Void => "'void'",
            TokenType::Return => "'return'",
            TokenType::LParen => "'('",
            TokenType::RParen => "')'",
            TokenType::LBrace => "'{'",
            TokenType::RBrace => "'}'",
            TokenType::Semicolon => "';'",
            TokenType::Eof => "<eof>",
        }
    }
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.to_str_const())
    }
}

#[derive(Clone, Copy)]
pub struct Token {
    pub typ: TokenType,
    pub str: Option<StrId>,
}

impl Lexer {
    pub fn new(input: Box<dyn Read>, ctx: &mut Ctx) -> Self {
        Lexer {
            next_byte: b' ',
            at_eof: false,

            input,

            next_buf: Vec::new(),
            keyword_map: vec![
                // TODO: Optimization: When we're looking at an identifier,
                // if it is greater than the last keyword StrId, then we
                // know it can't be any keyword and can skip the rest of the
                // search.
                (ctx.put_str(b"int"), TokenType::Int),
                (ctx.put_str(b"void"), TokenType::Void),
                (ctx.put_str(b"return"), TokenType::Return),
            ]
        }
    }

    fn advance(&mut self) -> std::io::Result<u8> {
        let result = self.next_byte;
        self.next_buf.push(result);

        let mut buf = [0];
        match self.input.read(&mut buf)? {
            0 => self.at_eof = true,
            _ => {}
        }
        self.next_byte = buf[0];

        Ok(result)
    }

    #[inline(always)]
    fn token(&mut self, typ: TokenType) -> Token {
        Token {
            typ,
            str: None
        }
    }

    fn token_from_buf(&mut self, ctx: &mut Ctx, typ: TokenType) -> Token {
        let str = ctx.put_and_clear_str(&mut self.next_buf);
        Token {
            typ,
            str: Some(str)
        }
    }

    fn number(&mut self, ctx: &mut Ctx) -> std::io::Result<Token> {
        while self.next_byte.is_ascii_digit() {
            self.advance()?;
        }

        // For now, we are only doing integers.
        Ok(self.token_from_buf(ctx, TokenType::Constant))
    }

    fn identifier(&mut self, ctx: &mut Ctx) -> std::io::Result<Token> {
        while self.next_byte.is_ascii_alphanumeric() || self.next_byte == b'_' {
            self.advance()?;
        }

        let mut tok = self.token_from_buf(ctx, TokenType::Identifier);

        for key in &self.keyword_map {
            if tok.str == Some(key.0) {
                tok.typ = key.1;
                return Ok(tok);
            }
        }

        Ok(tok)
    }

    pub fn next(&mut self, ctx: &mut Ctx) -> std::io::Result<Token> {
        while self.next_byte.is_ascii_whitespace() {
            self.advance()?;
        }

        self.next_buf.clear();

        if self.next_byte.is_ascii_digit() {
            return self.number(ctx);
        }

        if self.next_byte.is_ascii_alphabetic() || self.next_byte == b'_' {
            return self.identifier(ctx);
        }

        // Always consume at least one character, so that we make forward progress.
        Ok(match self.advance()? {
            // The idea here is that our token representation should allow
            // for this to be very fast. It is essentially loading a single
            // 64-bit constant (?).
            b'(' => self.token(TokenType::LParen),
            b')' => self.token(TokenType::RParen),
            b'{' => self.token(TokenType::LBrace),
            b'}' => self.token(TokenType::RBrace),
            b';' => self.token(TokenType::Semicolon),

            _ => {
                if self.at_eof {
                    return Ok(self.token(TokenType::Eof));
                }
                // TODO: Honestly, panicking is likely the fastest way to
                // handle ALL the errors? Maybe we can set up a panic hook
                // and then eschew most other error handling?
                panic!("unrecognized character");
            }
        })
    }
}