# poni-cc

This is my implementation of a C compiler, following the book "Writing a C
Compiler" by Nora Sandler.

This project has a couple of main goals:
- Have fun! Fundamentally this is a learning project.
- Be fast: I would like to write a compiler that is about as fast as `tcc` (or
  faster). This is for a couple of reasons. One is that this compiler may
  eventually serve as a "debug mode" codegen backend for my custom language
  PonieScript. Another is just because writing a fast compiler is cool.
- Be sort of single pass: This is related to the previous goal. But I also
  just find single-pass compilation very cool, in particular syntax-directed
  translation. The compiler does currently make multiple passes over the assembly
  code, but my goal is for at least the frontend of the compiler to be single-pass.

For the foreseeable future, however, the compiler will be in very much a prototyped
phase. In particular, it is currently relying on an external C preprocessor and
an external ~~assembler and~~ linker; I hope to eventually implement all of these
components in the compiler, which will likely be necessary for it to have competitive
performance with `tcc`. (An internal assembler is now implemented!)
