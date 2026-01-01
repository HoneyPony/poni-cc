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

use crate::{ctx::Ctx, lexer::{Lexer, Token, TokenType}};
use crate::ir::*;

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

    fn promote_to_var(val: Val, ctx: &mut Ctx, into: &mut Vec<Instr>) -> Var {
        match val {
            Val::Constant(_) => {
                let var = ctx.tmp();
                into.push(Instr::Copy { src: val, dst: var.into() });
                var

            },
            Val::Var(str_id) => {
                str_id
            },
        }
    }

    pub fn program(&mut self, ctx: &mut Ctx) -> Vec<Function> {
        let mut result = Vec::new();

        result.push(self.function(ctx));
        
        self.expect(ctx, TokenType::Eof);

        // TODO: We'll probably want to codegen functions as we go, not
        // push them all to a thing first.
        result
    }

    pub fn function(&mut self, ctx: &mut Ctx) -> Function {
        let mut instructions: Vec<Instr> = Vec::new();

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
        Function { name: ident.str.unwrap(), body: instructions }
    }

    pub fn statement(&mut self, ctx: &mut Ctx, into: &mut Vec<Instr>) {
        self.expect(ctx, TokenType::Return);
        let return_val = self.expression(ctx, into);
        self.expect(ctx, TokenType::Semicolon);

        // Generate the code for return
        into.push(Instr::Return(return_val));
    }

    pub fn expression(&mut self, ctx: &mut Ctx, into: &mut Vec<Instr>) -> Val {
        self.climb_precedence(ctx, into, 0)
    }

    fn next_token_precedence(&self) -> Option<i32> {
        // See:
        // https://en.cppreference.com/w/c/language/operator_precedence.html
        match self.next_token.typ {
            // Comma => 0
            // Assignment => 5
            // Ternary => 10
            // || => 15
            // && => 20
            TokenType::Pipe => Some(25),
            TokenType::Caret => Some(30),
            TokenType::Ampersand => Some(35),
            // == & != => Some(40)
            // relation operators => Some(45)
            TokenType::LessLess | TokenType::GreaterGreater => Some(50),
            TokenType::Plus | TokenType::Minus => Some(55),
            TokenType::Star | TokenType::Slash | TokenType::Percent => Some(60),
            _ => None
        }
    }

    pub fn climb_precedence(&mut self, ctx: &mut Ctx, into: &mut Vec<Instr>, min_prec: i32) -> Val {
        let mut lhs = self.factor(ctx, into);

        while let Some(prec) = self.next_token_precedence() && prec >= min_prec {
            let op = self.advance(ctx);
            let op = BinaryOp::from(op.typ);
            let rhs = self.climb_precedence(ctx, into, prec + 1);
            let dst = ctx.tmp();

            into.push(Instr::Binary { op, dst, lhs, rhs });
            lhs = dst.into();
        }

        lhs
    }

    pub fn factor(&mut self, ctx: &mut Ctx, into: &mut Vec<Instr>) -> Val {
        match self.next_token.typ {
            TokenType::Constant => {
                let value = self.advance(ctx);
                // Same thing as above (we should delete the unwrap...)
                //
                // Also, TODO: Validate that this StrId is a valid integer constant,
                // and/or convert it if necessary.
                Val::Constant(value.str.unwrap())
            },
            // Unary operators
            op @ (TokenType::Tilde | TokenType::Minus) => {
                self.advance(ctx);

                // NOTE: There's actually no real reason to use a new tmp
                // here? We should probably just overwrite the src variable.
                //
                // Some thoughts on why there is a new tmp here:
                // If we didn't have a new tmp, we could end up generating
                // something like negl $123, which is obviously wrong.
                //
                // ...But, there is no reason to generate *any code at all*
                // in that case. I will probably end up accidentally implementing
                // a constant propogator + dead code eliminator or something?
                // 
                // Probably I want some sort of function like 'take this Val and
                // make it real'
                //
                // E.g. I guess it's not the end of the world if we end up
                // generating movl $123, %edi ; negl %edi ; etc. Of course it
                // feels like we are really just missing some sort of constant
                // propogator.
                //
                // (Actually, maybe more particularly... If we DO end up
                // generating Instr::Unary { op, reg: Val::Constant }), then
                // what we'd like to do is simply replace the constant val
                // there with the evaluated value. E.g. have something like
                // re-evaluate-this-constant-please. IDK. It's not exactly
                // clear).

                // src must be evaluated first
                let src = self.factor(ctx, into);

                let dst = Self::promote_to_var(src, ctx, into);
                let op = UnaryOp::from(op);
                into.push(Instr::Unary { op, dst });

                dst.into()
            },
            TokenType::LParen => {
                self.advance(ctx);

                let result = self.expression(ctx, into);

                self.expect(ctx, TokenType::RParen);
                result
            }
            _ => {
                panic!("expected expression");
            }
        }
    }
}