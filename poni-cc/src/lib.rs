use std::{io::Write, io::Read};

use crate::{ctx::Ctx, parser::Parser};

mod ctx;
mod lexer;
mod parser;

// Backend architectures:
// We will likely (?) not support more than one, but I guess we'll see.
// (WASM when?)
mod x86_64;

/// Core compiler driver, for now. Takes a reader, which contains (preprocessed)
/// C code; outputs the generated assembly to the writer.
pub fn compile(input: Box<dyn Read>, mut output: Box<dyn Write>) -> std::io::Result<()> {
    let mut ctx = Ctx::new();
    let mut parser = Parser::new(input, &mut ctx);

    let program = parser.program(&mut ctx);
    program.write_as_text(&ctx, &mut output)?;

    Ok(())
}