use std::{io::Write, io::Read};

/// Core compiler driver, for now. Takes a reader, which contains (preprocessed)
/// C code; outputs the generated assembly to the writer.
pub fn compile(mut input: Box<dyn Read>, mut output: Box<dyn Write>) -> std::io::Result<()> {
    writeln!(output, ".globl main
main:
    xor %eax,%eax
    ret
.section .note.GNU-stack,\"\",@progbits")?;

    Ok(())
}