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

test_simple_expr!(binop_add, 30, "10 + 20");
test_simple_expr!(binop_sub, 10, "20 - 10");
test_simple_expr!(binop_mul, 36, "3 * 12");
test_simple_expr!(binop_div, 23, "70 / 3");
test_simple_expr!(binop_mod, 1, "70 % 3");

test_simple_expr!(binop_add_tt, 30, "~~10 + ~~20");
test_simple_expr!(binop_sub_tt, 10, "~~20 - ~~10");
test_simple_expr!(binop_mul_tt, 36, "~~3 * ~~12");
test_simple_expr!(binop_div_tt, 23, "~~70 / ~~3");
test_simple_expr!(binop_mod_tt, 1, "~~70 % ~~3");

test_simple_expr!(binop_prec1, 11, "1 + 2 * 3 + 4");
test_simple_expr!(binop_prec2, -21, "7 - 5 * 4 - 8");
test_simple_expr!(binop_prec3, 9, "1 + 60 / 3 - 12");
test_simple_expr!(binop_prec4, 42, "1 - 61 % 3 + 42");

test_simple_expr!(binop_prec1_paren, 21, "(1 + 2) * (3 + 4)");
test_simple_expr!(binop_prec2_paren, -8, "(7 - 5) * (4 - 8)");
test_simple_expr!(binop_prec3_paren, -6, "(1 + 60) / (3 - 12)");
test_simple_expr!(binop_prec4_paren, -15, "(1 - 61) % (3 + 42)");

test_simple_expr!(binop_many_ints, 150, r"
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1");

// A good test of bitwise operators is:
// 0011 op (3)
// 0101    (5)
// As this tests every combination at once.

test_simple_expr!(bitwise_and, 1, "3 & 5");
test_simple_expr!(bitwise_or, 7, "3 | 5");
test_simple_expr!(bitwise_xor, 6, "3 ^ 5");
test_simple_expr!(bitwise_lshift, 16, "1 << 4");
test_simple_expr!(bitwise_rshift, 77, "1234 >> 4");
test_simple_expr!(bitwise_rshift_neg, -78, "-1234 >> 4");

test_simple_expr!(bitwise_and_tt, 1, "~~3 & ~~5");
test_simple_expr!(bitwise_or_tt, 7, "~~3 | ~~5");
test_simple_expr!(bitwise_xor_tt, 6, "~~3 ^ ~~5");
test_simple_expr!(bitwise_lshift_tt, 16, "~~1 << ~~4");
test_simple_expr!(bitwise_rshift_tt, 77, "~~1234 >> ~~4");
test_simple_expr!(bitwise_rshift_neg_tt, -78, "~~-1234 >> ~~4");

test_simple_expr!(and_00, 0, "(1 + 1 - 2) && (3 + 3 - 6)");
test_simple_expr!(and_01, 0, "(1 + 1 - 2) && (3 + 4 - 6)");
test_simple_expr!(and_10, 0, "(1 + 2 - 2) && (3 + 3 - 6)");
test_simple_expr!(and_11, 1, "(1 + 2 - 2) && (3 + 4 - 6)");

test_simple_expr!(or_00, 0, "(1 + 1 - 2) || (3 + 3 - 6)");
test_simple_expr!(or_01, 1, "(1 + 1 - 2) || (3 + 4 - 6)");
test_simple_expr!(or_10, 1, "(1 + 2 - 2) || (3 + 3 - 6)");
test_simple_expr!(or_11, 1, "(1 + 2 - 2) || (3 + 4 - 6)");

test_simple_expr!(cmp_less_true, 1, "10 < 20");
test_simple_expr!(cmp_less_false, 0, "10 < 5");
test_simple_expr!(cmp_less_boundary, 0, "10 < 10");

test_simple_expr!(cmp_greater_true, 1, "40 > 30");
test_simple_expr!(cmp_greater_false, 0, "40 > 45");
test_simple_expr!(cmp_greater_boundary, 0, "40 > 40");

test_simple_expr!(cmp_lesseq_true, 1, "10 <= 20");
test_simple_expr!(cmp_lesseq_false, 0, "10 <= 5");
test_simple_expr!(cmp_lesseq_boundary, 1, "10 <= 10");

test_simple_expr!(cmp_greatereq_true, 1, "40 >= 30");
test_simple_expr!(cmp_greatereq_false, 0, "40 >= 45");
test_simple_expr!(cmp_greatereq_boundary, 1, "40 >= 40");

test_simple_expr!(cmp_equal_true, 1, "10 == 10");
test_simple_expr!(cmp_equal_false, 0, "10 != 5");

test_simple_expr!(cmp_less_true_tt, 1, "~~10 < ~~20");
test_simple_expr!(cmp_less_false_tt, 0, "~~10 < ~~5");
test_simple_expr!(cmp_less_boundary_tt, 0, "~~10 < ~~10");

test_simple_expr!(cmp_greater_true_tt, 1, "~~40 > ~~30");
test_simple_expr!(cmp_greater_false_tt, 0, "~~40 > ~~45");
test_simple_expr!(cmp_greater_boundary_tt, 0, "~~40 > ~~40");

test_simple_expr!(cmp_lesseq_true_tt, 1, "~~10 <= ~~20");
test_simple_expr!(cmp_lesseq_false_tt, 0, "~~10 <= ~~5");
test_simple_expr!(cmp_lesseq_boundary_tt, 1, "~~10 <= ~~10");

test_simple_expr!(cmp_greatereq_true_tt, 1, "~~40 >= ~~30");
test_simple_expr!(cmp_greatereq_false_tt, 0, "~~40 >= ~~45");
test_simple_expr!(cmp_greatereq_boundary_tt, 1, "~~40 >= ~~40");

test_simple_expr!(cmp_equal_true_tt, 1, "~~10 == ~~10");
test_simple_expr!(cmp_equal_false_tt, 0, "~~10 != ~~5");