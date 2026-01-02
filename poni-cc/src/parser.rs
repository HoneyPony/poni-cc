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

use rustc_hash::FxHashMap;

use crate::{ctx::{Ctx, StrId}, lexer::{Lexer, Token, TokenType}};
use crate::ir::*;

/// A single scope of vars, e.g. there are two different VarScopes in the following:
/// ```c
/// int a = 30;
/// {
///     int a = 40; // valid
/// }
/// ```
struct VarScope {
    map: FxHashMap<StrId, crate::ir::Var>,
}

impl VarScope {
    fn new() -> Self {
        VarScope {
            map: FxHashMap::default()
        }
    }
}

pub struct Parser {
    next_token: Token,
    lexer: Lexer,
    variables: Vec<VarScope>,
}

impl Parser {
    pub fn new(input: Box<dyn Read>, ctx: &mut Ctx) -> Self {
        let mut result = Parser {
            next_token: Token { typ: TokenType::Eof, str: None },
            lexer: Lexer::new(input, ctx),
            // Start with the global scope. (?)
            variables: vec![VarScope::new()],
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

    fn match_(&mut self, ctx: &mut Ctx, typ: TokenType) -> bool {
        if self.next_token.typ == typ {
            self.advance(ctx);
            return true;
        }
        false
    }

    fn lookup_var(&self, var_name: StrId) -> Option<Var> {
        for scope in self.variables.iter().rev() {
            if let Some(var) = scope.map.get(&var_name) {
                return Some(*var);
            }
        }
        None
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

        // New scope for the function's variables
        self.variables.push(VarScope::new());

        self.expect(ctx, TokenType::Int);
        let ident = self.expect(ctx, TokenType::Identifier);
        self.expect(ctx, TokenType::LParen);
        self.expect(ctx, TokenType::Void);
        self.expect(ctx, TokenType::RParen);
        self.expect(ctx, TokenType::LBrace);

        while !matches!(self.next_token.typ, TokenType::RBrace | TokenType::Eof) {
            self.block_item(ctx, &mut instructions);
        }

        self.variables.pop();

        self.expect(ctx, TokenType::RBrace);

        // TODO: We probably want to make the ident.str just a property of
        // e.g. TokenType::Identifier, so that we don't have to call .unwrap()
        // here.
        Function { name: ident.str.unwrap(), body: instructions }
    }

    pub fn block_item(&mut self, ctx: &mut Ctx, into: &mut Vec<Instr>) {
        if self.match_(ctx, TokenType::Int) {
            // Declaration

            // In the future, this will have to parse a whole type somehow...
            let var_name = self.expect(ctx, TokenType::Identifier);
            let var_name = var_name.str.unwrap();
            if self.lookup_var(var_name).is_some() {
                panic!("duplicate variable '{}'", ctx.get(var_name));
            }

            let var = ctx.tmp();
            // It should be safe to unwrap the last() because we always have
            // a global scope.
            self.variables.last_mut().unwrap().map.insert(var_name, var);

            // Parse initializer
            if self.match_(ctx, TokenType::Equal) {
                let initializer = self.expression(ctx, into);
                into.push(Instr::Copy { src: initializer, dst: var });
            }

            self.expect(ctx, TokenType::Semicolon);
        }
        else {
            self.statement(ctx, into);
        }
    }

    pub fn statement(&mut self, ctx: &mut Ctx, into: &mut Vec<Instr>) {
        match self.next_token.typ {
            TokenType::Return => {
                self.expect(ctx, TokenType::Return);
                let return_val = self.expression(ctx, into);
                self.expect(ctx, TokenType::Semicolon);

                // Generate the code for return
                into.push(Instr::Return(return_val));
            },
            TokenType::Semicolon => {
                // Skip empty statements.
                self.advance(ctx);
            }
            _ => {
                self.expression(ctx, into);
                self.expect(ctx, TokenType::Semicolon);
            }
        }
        
    }

    pub fn expression(&mut self, ctx: &mut Ctx, into: &mut Vec<Instr>) -> Val {
        self.climb_precedence(ctx, into, 0)
    }

    fn next_token_precedence(&self) -> Option<i32> {
        // See:
        // https://en.cppreference.com/w/c/language/operator_precedence.html
        match self.next_token.typ {
            // Comma => 0
            TokenType::Equal | TokenType::PlusEqual | TokenType::MinusEqual |
                    TokenType::StarEqual | TokenType::SlashEqual |
                    TokenType::AmpersandEqual | TokenType::PipeEqual |
                    TokenType::CaretEqual  |
                    TokenType::LessLessEqual | TokenType::GreaterGreaterEqual => Some(5),
            // Ternary => 10
            TokenType::PipePipe => Some(15),
            TokenType::AmpersandAmpersand => Some(20),
            TokenType::Pipe => Some(25),
            TokenType::Caret => Some(30),
            TokenType::Ampersand => Some(35),
            TokenType::EqualEqual | TokenType::BangEqual => Some(40),
            TokenType::Less | TokenType::LessEqual | TokenType::Greater | TokenType::GreaterEqual => Some(45),
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

            // Handling for short circuiting operators is special.
            if matches!(op.typ, TokenType::AmpersandAmpersand | TokenType::PipePipe) {
                // TODO: We can probably do this in a better way, with maybe
                // a couple extra IR instructions.
                //
                // The approach from the book requires two labels, and one
                // unconditional jump in the false case. It looks like:
                // v1 = <eval e1>
                // JumpIfZero(v1, false_label)
                // v2 = <eval e2>
                // JumpIfZero(v2, false_label)
                // result = 1
                // Jump(end)
                // Label(false_label)
                // result = 0
                // Label(end)
                //
                // But it should be possible to do fewer jumps and labels using
                // something like:
                // v1 = <eval e1>
                // result = !!v1
                // JumpIfZero(result, end)
                // v2 = <eval e2>
                // result = !!v2
                // Label(end)
                //
                // In some cases, this !!v1 could even just directly evaluate
                // the condition flags, which would be nice.
                //
                // Anyway, for now we'll do it the inefficient way because it's
                // straightforward to implement.

                // Note to self: For ||, if we use the same approach as the book,
                // we will want to do:
                // v1 = <eval e1>
                // JumpIfNotZero(v1, true_label)
                // v2 = <eval e2>
                // JumpIfNotZero(v2, true_label)
                // result = 0
                // Jump(end)
                // Label(true_label)
                // result = 1
                // Label(end)

                let is_or = matches!(op.typ, TokenType::PipePipe);

                let tf_label = ctx.label(if is_or { "true" } else { "false" });
                let end_label = ctx.label("end");
                // Right now, we have the lhs evaluated. So, we need to jump_if_zero
                // to our false label.
                into.push(if is_or {
                    Instr::JumpIfNotZero { condition: lhs, target: tf_label }
                } else {
                    Instr::JumpIfZero { condition: lhs, target: tf_label }
                });

                // Now we evaluate the rhs. That way, it is NOT evaluated if
                // we had jumped.
                let rhs = self.climb_precedence(ctx, into, prec + 1);
                into.push(if is_or {
                    Instr::JumpIfNotZero { condition: rhs, target: tf_label }
                } else {
                    Instr::JumpIfZero { condition: rhs, target: tf_label }
                });


                // Create the true value.
                let dst = ctx.tmp();
                into.push(Instr::Copy { src: Val::Constant(if is_or { ctx.zero() } else { ctx.one() }), dst });
                // Jump to the end.
                into.push(Instr::Jump(end_label));

                // Create the false value.
                into.push(Instr::Label(tf_label));
                into.push(Instr::Copy { src: Val::Constant(if is_or { ctx.one() } else { ctx.zero() }), dst });
                into.push(Instr::Label(end_label));

                // Make sure to update lhs!
                lhs = dst.into();
            }
            else if matches!(op.typ,
                TokenType::Equal | TokenType::PlusEqual | TokenType::MinusEqual |
                    TokenType::StarEqual | TokenType::SlashEqual |
                    TokenType::AmpersandEqual | TokenType::PipeEqual |
                    TokenType::CaretEqual |
                    TokenType::LessLessEqual | TokenType::GreaterGreaterEqual
            ) {
                // Assignment
                
                // Assignment is right associative, so use prec instead of
                // prec + 1
                let rhs = self.climb_precedence(ctx, into, prec);

                if let Val::Var(var) = lhs {
                    if op.typ == TokenType::Equal {
                        into.push(Instr::Copy { src: rhs, dst: var });
                    }
                    else {
                        // This will probably be a bit tricky with other kinds
                        // of lvalues. For now, just generate a BinaryOp that
                        // is assigning to the destination.
                        into.push(Instr::Binary { op: BinaryOp::from(op.typ), dst: var, lhs, rhs })
                    }

                    // I believe LHS doesn't change in this case? We already
                    // have the value in the var...
                }
                else {
                    // TODO:
                    // To properly handle LValues, we miiiight need an AST?
                    // Maybe not.
                    panic!("assignment to non-lvalue");
                }
            }
            else {
                let op = BinaryOp::from(op.typ);
                let rhs = self.climb_precedence(ctx, into, prec + 1);
                let dst = ctx.tmp();

                into.push(Instr::Binary { op, dst, lhs, rhs });
                lhs = dst.into();
            }
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
            TokenType::Identifier => {
                let var_name = self.advance(ctx).str.unwrap();

                let Some(var) = self.lookup_var(var_name) else {
                    panic!("unresolved identifier '{}'", ctx.get(var_name));
                };

                Val::Var(var)
            }
            // Unary operators
            op @ (TokenType::Tilde | TokenType::Minus | TokenType::Bang) => {
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