//! The lexer.
//! 
//! This is a hand-implemented lexer. The main idea is to match tokens into
//! strings, which are then matched against keywords. Then, operators and other
//! special characters are handled specially.

use std::io::Read;

use crate::ctx::{Ctx, StrId};

pub struct Lexer<R: Read> {
    next_byte: u8,
    at_eof: bool,

    input: R,

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

    Bang    = b'!',
    Equal   = b'=',
    Plus    = b'+',
    Minus   = b'-',
    Star    = b'*',
    Slash   = b'/',
    Percent = b'%',
    Tilde   = b'~',
    Ampersand = b'&',
    Pipe      = b'|',
    Caret     = b'^',
    Less      = b'<',
    Greater   = b'>',
    
    BangEqual,
    EqualEqual,

    PlusPlus,
    MinusMinus,

    AmpersandAmpersand,
    PipePipe,

    LessLess,
    GreaterGreater,

    LessEqual,
    GreaterEqual,

    PlusEqual,
    MinusEqual,
    StarEqual,
    SlashEqual,
    PercentEqual,

    AmpersandEqual,
    PipeEqual,
    CaretEqual,
    LessLessEqual,
    GreaterGreaterEqual,

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
            TokenType::Bang    => "'!'",
            TokenType::Equal   => "'='",
            TokenType::Tilde   => "'~'",
            TokenType::Plus    => "'+'",
            TokenType::Minus   => "'-'",
            TokenType::Star    => "'*'",
            TokenType::Slash   => "'/'",
            TokenType::Percent => "'%'",
            TokenType::Ampersand => "'&'",
            TokenType::Pipe      => "'|'",
            TokenType::Caret     => "'^'",
            TokenType::Less      => "'<'",
            TokenType::Greater   => "'>'",
            TokenType::BangEqual => "'!='",
            TokenType::EqualEqual => "'=='",
            TokenType::PlusPlus => "'++'",
            TokenType::MinusMinus => "'--'",
            TokenType::AmpersandAmpersand => "'&&'",
            TokenType::PipePipe => "'||'",
            TokenType::LessLess => "'<<'",
            TokenType::GreaterGreater => "'>>'",
            TokenType::LessEqual => "'<='",
            TokenType::GreaterEqual => "'>='",

            // Arithmetic compound assignment
            TokenType::PlusEqual    => "'+='",
            TokenType::MinusEqual   => "'-='",
            TokenType::StarEqual    => "'*='",
            TokenType::SlashEqual   => "'/='",
            TokenType::PercentEqual => "'%='",

            // Bitwise compound assignment
            TokenType::AmpersandEqual      => "'&='",
            TokenType::PipeEqual           => "'|='",
            TokenType::CaretEqual          => "'^='",
            TokenType::LessLessEqual       => "'<<='",
            TokenType::GreaterGreaterEqual => "'>>='",

            // Eof
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

impl<R: Read> Lexer<R> {
    pub fn new(input: R, ctx: &mut Ctx) -> Self {
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

    fn advance(&mut self) -> u8 {
        let result = self.next_byte;
        self.next_buf.push(result);

        let mut buf = [0];
        // NOTE: We need to implement backtracking support or w/e for the panic.
        match self.input.read(&mut buf).unwrap() {
            0 => self.at_eof = true,
            _ => {}
        }
        self.next_byte = buf[0];

        result
    }

    fn match_(&mut self, expected_byte: u8) -> bool {
        if self.next_byte == expected_byte {
            self.advance();
            return true;
        }
        return false
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

    fn number(&mut self, ctx: &mut Ctx) -> Token {
        while self.next_byte.is_ascii_digit() {
            self.advance();
        }

        // For now, we are only doing integers.
        self.token_from_buf(ctx, TokenType::Constant)
    }

    fn identifier(&mut self, ctx: &mut Ctx) -> Token {
        while self.next_byte.is_ascii_alphanumeric() || self.next_byte == b'_' {
            self.advance();
        }

        let mut tok = self.token_from_buf(ctx, TokenType::Identifier);

        for key in &self.keyword_map {
            if tok.str == Some(key.0) {
                tok.typ = key.1;
                return tok;
            }
        }

        tok
    }

    #[inline(always)]
    fn choose_match_one(&mut self, expected_byte: u8, if_matches: TokenType, if_not: TokenType) -> Token {
        if self.match_(expected_byte) {
            self.token(if_matches)
        }
        else {
            self.token(if_not)
        }
    }

    // TODO: Consider writing a little macro for this, e.g.
    // match_next!(self, ctx, {
    //    b'+' => PlusPlus,
    //    b'=' => PlusEqual,
    //    _    => Plus,
    // })
    #[inline(always)]
    fn choose_match_two(&mut self, default: TokenType, a: u8, atype: TokenType, b: u8, btype: TokenType) -> Token {
        let typ = match self.next_byte {
            n if n == a => {
                self.advance();
                atype
            }
            n if n == b => {
                self.advance();
                btype
            }
            _ => {
                default
            }
        };
        self.token(typ)
    }

    pub fn next(&mut self, ctx: &mut Ctx) -> Token {
        while self.next_byte.is_ascii_whitespace() {
            self.advance();
        }

        self.next_buf.clear();

        if self.next_byte.is_ascii_digit() {
            return self.number(ctx);
        }

        if self.next_byte.is_ascii_alphabetic() || self.next_byte == b'_' {
            return self.identifier(ctx);
        }

        // Always consume at least one character, so that we make forward progress.
        match self.advance() {
            // The idea here is that our token representation should allow
            // for this to be very fast. It is essentially loading a single
            // 64-bit constant (?).
            b'(' => self.token(TokenType::LParen),
            b')' => self.token(TokenType::RParen),
            b'{' => self.token(TokenType::LBrace),
            b'}' => self.token(TokenType::RBrace),
            b';' => self.token(TokenType::Semicolon),
            b'~' => self.token(TokenType::Tilde),
            b'^' => self.choose_match_one(b'=',
                TokenType::CaretEqual, TokenType::Caret),
            b'=' => self.choose_match_one(b'=',
                TokenType::EqualEqual, TokenType::Equal),
            b'!' => self.choose_match_one(b'=',
                TokenType::BangEqual, TokenType::Bang),
            b'-' => self.choose_match_two(TokenType::Minus,
                b'-', TokenType::MinusMinus,
                b'=', TokenType::MinusEqual),
            b'+' => self.choose_match_two(TokenType::Plus,
                b'+', TokenType::PlusPlus,
                b'=', TokenType::PlusEqual),
            b'&' => self.choose_match_two(TokenType::Ampersand,
                b'&', TokenType::AmpersandAmpersand,
                b'=', TokenType::AmpersandEqual),
            b'|' => self.choose_match_two(TokenType::Pipe,
                b'|', TokenType::PipePipe,
                b'=', TokenType::PipeEqual),
            b'*' => self.choose_match_one(b'=',
                TokenType::StarEqual, TokenType::Star),
            b'%' => self.choose_match_one(b'=',
                TokenType::PercentEqual, TokenType::Percent),

            // TODO: Handle comments.
            b'/' => self.choose_match_one(b'=',
                TokenType::SlashEqual, TokenType::Slash),

            b'<' => match self.next_byte {
                b'<' => {
                    self.advance();
                    self.choose_match_one(b'=',
                        TokenType::LessLessEqual, TokenType::LessLess)
                }
                b'=' => {
                    self.advance();
                    self.token(TokenType::LessEqual)
                }
                // Don't advance.
                _ => self.token(TokenType::Less)
            }

            b'>' => match self.next_byte {
                b'>' => {
                    self.advance();
                    self.choose_match_one(b'=',
                        TokenType::GreaterGreaterEqual, TokenType::GreaterGreater)
                }
                b'=' => {
                    self.advance();
                    self.token(TokenType::GreaterEqual)
                }
                // Don't advance.
                _ => self.token(TokenType::Greater)
            }

            _ => {
                if self.at_eof {
                    return self.token(TokenType::Eof);
                }
                // TODO: Honestly, panicking is likely the fastest way to
                // handle ALL the errors? Maybe we can set up a panic hook
                // and then eschew most other error handling?
                panic!("unrecognized character");
            }
        }
    }
}