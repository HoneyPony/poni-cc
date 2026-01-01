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

use crate::{ctx::Ctx, lexer::Lexer};

pub struct Parser {
    lexer: Lexer,
}

impl Parser {
    pub fn program(&mut self, ctx: &mut Ctx) {

    }
    
    pub fn function(&mut self, ctx: &mut Ctx) {

    }

    pub fn statement(&mut self, ctx: &mut Ctx) {

    }

    pub fn expression(&mut self, ctx: &mut Ctx) {

    }

    pub fn identifier(&mut self, ctx: &mut Ctx) {

    }

    pub fn int(&mut self, ctx: &mut Ctx) {

    }
}