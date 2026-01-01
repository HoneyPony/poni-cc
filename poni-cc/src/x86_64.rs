//! Backend for x86_64. The main goal is for it to work on my computer.

use std::io::Write;

pub fn to_text(output: &mut Box<dyn Write>) -> std::io::Result<()> {
        writeln!(output, ".globl main
main:
    xor %eax,%eax
    ret
.section .note.GNU-stack,\"\",@progbits")?;

    Ok(())
}