use std::{fs::File, io::{BufReader, BufWriter}, path::PathBuf, process::{Command, Stdio}};

use argh::FromArgs;

#[derive(FromArgs)]
/// Compiles a single C file to x86_64 assembly, to be compiled by GNU GAS.
/// 
/// (Should this program be a compiler driver...?)
struct Cli {
    #[argh(option, short = 'i')]
    /// path to the input C file.
    input: PathBuf,
    #[argh(option, short = 'o')]
    /// path to the output S file.
    output: PathBuf,
    #[argh(switch)]
    /// whether to act as a compiler driver (automatically invoking gcc for
    /// preprocessing and assembly.)
    compile: bool,
}

fn compile_driver(args: &Cli) -> std::io::Result<()> {
    // These are just from a bit of experimentation. Obviously we would be
    // best off with our own preprocessor and assembler (although maybe not
    // our own linker (?))
    let cpp_program = "tcc";
    let asm_ld_program = "clang";

    let mut cpp = Command::new(cpp_program)
        .arg("-E")
        .arg("-P")
        //.arg("-x").arg("c")
        // Read from stdin
        .arg(&args.input)
        .stdout(Stdio::piped())
        .spawn()?;

    
    let mut asm = Command::new(asm_ld_program)
        .arg("-o").arg(&args.output)
        .arg("-x").arg("assembler")
        //.arg("-fuse-ld=mold")
        .arg("-")
        .stdin(Stdio::piped())
        .spawn()?;

    let asm_stdin = asm.stdin.take().unwrap();
    let cpp_stdout = cpp.stdout.take().unwrap();

    poni_cc::compile(
        BufReader::new(cpp_stdout),
        BufWriter::new(asm_stdin),
        true)
        .unwrap();

    drop(cpp); // kill the pipe (?)
    asm.wait()?;

    Ok(())
}

fn compile_to_asm(args: &Cli) -> std::io::Result<()> {
    let input = File::open(&args.input)?;
    // TODO: Maybe only create this file if we successfully compile..?
    let output = File::create(&args.output)?;

    let buf_read = BufReader::new(input);
    let buf_write = BufWriter::new(output);

    poni_cc::compile(buf_read, buf_write, true)?;

    Ok(())
}

fn main() {
    let args: Cli = argh::from_env();
    if args.compile {
        compile_driver(&args).unwrap();
    }
    else {
        match compile_to_asm(&args) {
            Ok(()) => { eprintln!("🦄 success"); }
            Err(err) => {
                eprintln!("🦄 io error: {}", err);
                std::process::exit(1);
            }
        }
    }
}
