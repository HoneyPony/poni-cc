use std::{io::Write, io::Read};

use crate::{ctx::Ctx, parser::Parser};

mod ctx;
mod ir;
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

    let ir_funs = parser.program(&mut ctx);
    let mut x86_funs = Vec::new();
    for fun in &ir_funs {
        let mut fun = x86_64::lower_function(fun);
        x86_64::replace_psuedoregister_pass(&mut fun);
        x86_64::fixup_pass(&mut fun);
        x86_funs.push(fun);
    }

    // TODO: Get rid of 'Program' thing, seems unnecessary? Idk
    let program = x86_64::Program {
        functions: x86_funs
    };
    program.write_as_text(&ctx, &mut output)?;

    Ok(())
}