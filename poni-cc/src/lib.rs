use std::{io::Write, io::Read};

// Backend architectures:
// We will likely (?) not support more than one, but I guess we'll see.
// (WASM when?)
mod x86_64;

/// Core compiler driver, for now. Takes a reader, which contains (preprocessed)
/// C code; outputs the generated assembly to the writer.
pub fn compile(mut input: Box<dyn Read>, mut output: Box<dyn Write>) -> std::io::Result<()> {
    x86_64::to_text(&mut output)?;

    Ok(())
}