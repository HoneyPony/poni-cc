//! The lexer.
//! 
//! This is a hand-implemented lexer. The main idea is to match tokens into
//! strings, which are then matched against keywords. Then, operators and other
//! special characters are handled specially.

use std::{io::Read, num::NonZeroU64};

use crate::ctx::{Ctx, StrId};

pub struct Lexer<R: Read> {
    next_byte: u8,
    at_eof: bool,

    input: R,

    internal_buffer: [u8; INTERNAL_BUFFER_SIZE],
    internal_buf_len: usize,
    internal_buf_ptr: usize,

    next_buf: Vec<u8>,

    /// A Map from StrId's to TokenType's for our keywords. Use a Vec instead
    /// of a HashMap as there really aren't very many.
    keyword_map: Vec<(StrKey, TokenType)>,
}

#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    Identifier(StrKey),
    Constant(StrKey),
    
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
            TokenType::Identifier(_) => "identifier",
            TokenType::Constant(_) => "numerical constant",
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

const STRKEY_SIZE: usize = 7;

/// A key, sort of like StrId, except that can store small strings inline.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct StrKey(pub NonZeroU64);


//     Id(StrId),
//     Bytes(NonZeroU8, [u8; STRKEY_SIZE]),
// }

impl StrKey {
    pub fn from_known_bytes(bytes: &[u8]) -> StrKey {
        let len = bytes.len();
        assert!(len <= STRKEY_SIZE);

        let mut key = [0u8; STRKEY_SIZE + 1];

        key[key.len() - 1] = len as u8;

        let subslice: &mut[u8] = &mut key[0..len];
        
        subslice.copy_from_slice(bytes);

        StrKey(NonZeroU64::try_from(u64::from_le_bytes(key)).unwrap())
    }

    pub fn from_key(key: StrId) -> Self {
        let mut bytes = [0u8; 8];

        let as_u32 = key.to_nonzero_u32();
        let subslice = &mut bytes[0..4];
        subslice.copy_from_slice(&as_u32.get().to_le_bytes());

        // SAFETY: key is nonzero, so we must also be nonzero.
        unsafe { StrKey(NonZeroU64::try_from(u64::from_le_bytes(bytes)).unwrap_unchecked()) }
    }
}

const INTERNAL_BUFFER_SIZE: usize = 512;

impl<R: Read> Lexer<R> {
    pub fn new(input: R, _ctx: &mut Ctx) -> Self {
        Lexer {
            next_byte: b' ',
            at_eof: false,

            input,

            internal_buffer: [0; INTERNAL_BUFFER_SIZE],
            internal_buf_len: 0,
            internal_buf_ptr: 0,

            next_buf: Vec::new(),
            keyword_map: vec![
                // TODO: Optimization: When we're looking at an identifier,
                // if it is greater than the last keyword StrId, then we
                // know it can't be any keyword and can skip the rest of the
                // search.
                (StrKey::from_known_bytes(b"int"), TokenType::Int),
                (StrKey::from_known_bytes(b"void"), TokenType::Void),
                (StrKey::from_known_bytes(b"return"), TokenType::Return),
            ]
        }
    }

    fn advance_buffer(&mut self) {
        self.internal_buffer[0] = 0;
        self.internal_buf_ptr = 0;

        match self.input.read(&mut self.internal_buffer).unwrap() {
            0 => {
                self.at_eof = true;
                self.internal_buf_len = 0;
            }
            n @ _ => {
                self.internal_buf_len = n;
            }
        }
    }

    #[inline(always)]
    fn advance(&mut self) -> u8 {
        let result = self.next_byte;

        self.internal_buf_ptr += 1;
        if self.internal_buf_ptr >= self.internal_buf_len {
            self.advance_buffer();
        }
        self.next_byte = self.internal_buffer[self.internal_buf_ptr];

        result
    }

    fn match_(&mut self, expected_byte: u8) -> bool {
        if self.next_byte == expected_byte {
            self.advance();
            return true;
        }
        return false
    }

    // TODO: Delete this function
    #[inline(always)]
    fn token(&mut self, typ: TokenType) -> TokenType {
        typ
    }

    fn str_from_buf(&mut self, ctx: &mut Ctx) -> StrKey {
        //eprintln!("next_buf: '{}'", str::from_utf8(&self.next_buf).unwrap());
        if self.next_buf.len() <= STRKEY_SIZE {
            // Safe because we've checked its less than 11
            let len_us = self.next_buf.len();
            let len = len_us as u8;

            let mut buf: [u8; STRKEY_SIZE + 1] = [0; STRKEY_SIZE + 1];
            let subslice = &mut buf[0..len_us];
            subslice.copy_from_slice(&self.next_buf);

            buf[buf.len() - 1] = len;
            let as_u64 = u64::from_le_bytes(buf);

            return StrKey(NonZeroU64::try_from(as_u64).unwrap())
        }

        let str = ctx.put_and_clear_str(&mut self.next_buf);
        return StrKey::from_key(str)
    }

    fn token_from_buf(&mut self, ctx: &mut Ctx, is_ident: bool) -> TokenType {
        let str = self.str_from_buf(ctx);
        if is_ident { TokenType::Identifier(str) } else { TokenType::Constant(str) }
    }

    fn number(&mut self, ctx: &mut Ctx) -> TokenType {
        while self.next_byte.is_ascii_digit() {
            let b = self.advance();
            self.next_buf.push(b);
        }

        // For now, we are only doing integers.
        self.token_from_buf(ctx, false)
    }

    fn identifier(&mut self, ctx: &mut Ctx) -> TokenType {
        fn is_id(c: u8) -> bool {
            c.is_ascii_alphanumeric() || c == b'_'
        }

        let mut copy_from = self.internal_buf_ptr;
        let mut fast = true;
        while is_id(self.internal_buffer[self.internal_buf_ptr]) {
            self.internal_buf_ptr += 1;
            if self.internal_buf_ptr >= self.internal_buf_len {
                self.next_buf.extend_from_slice(&self.internal_buffer[copy_from..self.internal_buf_ptr]);
                self.advance_buffer();
                copy_from = 0;
                fast = false;
            }
        }
        let tok = if fast && (self.internal_buf_ptr - copy_from) <= STRKEY_SIZE {
            let len = self.internal_buf_ptr - copy_from;
            let mut buf = [0u8; STRKEY_SIZE + 1];
            let subslice = &mut buf[0..len];
            subslice.copy_from_slice(&self.internal_buffer[copy_from..self.internal_buf_ptr]);
            buf[buf.len() - 1] = len as u8;

            let as_u64 = u64::from_le_bytes(buf);
            let key = StrKey(NonZeroU64::try_from(as_u64).unwrap());

            TokenType::Identifier(key)
        }
        else {
            self.next_buf.extend_from_slice(&self.internal_buffer[copy_from..self.internal_buf_ptr]);
            self.token_from_buf(ctx, true)
        };
       
        self.next_byte = self.internal_buffer[self.internal_buf_ptr];

        // We also need the str...
        let TokenType::Identifier(str) = tok else { unreachable!() };

        for key in &self.keyword_map {
            if str == key.0 {
                return key.1;
            }
        }

        tok
    }

    #[inline(always)]
    fn choose_match_one(&mut self, expected_byte: u8, if_matches: TokenType, if_not: TokenType) -> TokenType {
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
    fn choose_match_two(&mut self, default: TokenType, a: u8, atype: TokenType, b: u8, btype: TokenType) -> TokenType {
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

    pub fn next(&mut self, ctx: &mut Ctx) -> TokenType {
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