use std::{fs::File, io::{BufReader, BufWriter}, path::PathBuf};

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
}

fn compile(args: &Cli) -> std::io::Result<()> {
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
    match compile(&args) {
        Ok(()) => { eprintln!("🦄 success"); }
        Err(err) => {
            eprintln!("🦄 io error: {}", err);
            std::process::exit(1);
        }
    }
}
