use std::{io::{Cursor, Write}, path::Path, process::{Command, Stdio}};

fn test_driver(test_name: &str, input: &[u8], expected_ret: i32) {
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

    let output_path = Path::new("../testing-ground").join(test_name);
    
    let mut asm = Command::new("gcc")
        .arg("-o").arg(&output_path)
        .arg("-x").arg("assembler")
        .arg("-")
        .stdin(Stdio::piped())
        .spawn().unwrap();

    let stdin = asm.stdin.take().unwrap();
    let output = Cursor::new(output.stdout);
    crate::compile(Box::new(output), Box::new(stdin))
        .unwrap();

    asm.wait().unwrap();

    // Now we should have a compiled executable.
    let mut tester = Command::new(&output_path)
        .spawn().unwrap();
    let exit = tester.wait().unwrap();
    let code = exit.code().unwrap();

    assert!(code == expected_ret);
}

#[test]
pub fn simple() {
    test_driver("simple", b"int main(void) { return 123; }", 123);
}