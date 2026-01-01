use std::{io::{Cursor, Write}, path::Path, process::{Command, Stdio}};

fn preprocess(use_cpp: bool, input: &[u8]) -> Vec<u8> {
    if use_cpp {
        let mut cpp = Command::new("gcc")
            .arg("-E")
            .arg("-P")
            .arg("-x").arg("c")
            // Read from stdin
            .arg("-")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn().unwrap();

        let mut stdin = cpp.stdin.take().unwrap();
        stdin.write_all(input).unwrap();
        drop(stdin);

        let output = cpp.wait_with_output().unwrap();
        return output.stdout;
    }

    return input.to_vec();
}

fn test_driver(use_cpp: bool, test_name: &str, input: &[u8], expected_ret: i32) {
    let output = preprocess(use_cpp, input);

    // We expect the exit code to be truncated.
    // Perhaps a more principled way of testing this would be to run an existing
    // C compiler on the same source and compare the return codes.
    let expected_ret = expected_ret as u8;
    let expected_ret = expected_ret as i32;

    let output_path = Path::new("../testing-ground").join(test_name);
    
    let mut asm = Command::new("gcc")
        .arg("-o").arg(&output_path)
        .arg("-x").arg("assembler")
        .arg("-")
        .stdin(Stdio::piped())
        .spawn().unwrap();

    let stdin = asm.stdin.take().unwrap();
    let output = Cursor::new(output);
    crate::compile(Box::new(output), Box::new(stdin))
        .unwrap();

    asm.wait().unwrap();

    // Now we should have a compiled executable.
    let mut tester = Command::new(&output_path)
        .spawn().unwrap();
    let exit = tester.wait().unwrap();
    let code = exit.code().unwrap();

    assert_eq!(code, expected_ret);
}

macro_rules! test {
    ($name:ident, $exit_code:expr, $c_src:expr) => {
        #[test]
        fn $name() {
            test_driver(true, stringify!($name), $c_src, $exit_code);
        }
    }
}

macro_rules! test_no_cpp {
    ($name:ident, $exit_code:expr, $c_src:expr) => {
        #[test]
        fn $name() {
            test_driver(false, stringify!($name), $c_src, $exit_code);
        }
    }
}

macro_rules! test_simple_expr {
    ($name:ident, $exit_code:expr, $c_src:expr) => {
        #[test]
        fn $name() {
            const C_SRC: &str = concat!("int main(void) { return ", $c_src, "; }");
            test_driver(false, stringify!($name), C_SRC.as_bytes(), $exit_code);
        }
    }
}

test!(simple_cpp, 145,
br"#define MY_VAL 145
int main(void) {
    return MY_VAL;
}
");

test_no_cpp!(simple, 123, b"int main(void) { return 123; }");

test_simple_expr!(unop_minus, -45, "-45");
test_simple_expr!(unop_tilde, -1, "~0");
test_simple_expr!(unop_minus_minus, 45, "- -45");
test_simple_expr!(unop_tilde_minus, 3, "~-4");