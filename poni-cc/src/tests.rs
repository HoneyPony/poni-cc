use std::{fs::File, io::{Cursor, Write}, path::Path, process::{Command, Stdio}};

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

fn test_driver_manual_encoding(use_cpp: bool, test_name: &str, input: &[u8], expected_ret: i32) {
    let output = preprocess(use_cpp, input);

    // We expect the exit code to be truncated.
    // Perhaps a more principled way of testing this would be to run an existing
    // C compiler on the same source and compare the return codes.
    let expected_ret = expected_ret as u8;
    let expected_ret = expected_ret as i32;

    let test_name = format!("{}-pony", test_name);

    let output_path = Path::new("../testing-ground").join(&test_name);
    let obj_output_path = Path::new("../testing-ground").join(format!("{}.o", &test_name));
    let obj_output_file = File::create(&obj_output_path)
        .expect(".o file");
    
    let output = Cursor::new(output);

    // We can't use fastdrop here. In particular, the application needs to keep
    // living long enough for the code to finish compiling, and for us to
    // run the tester program.
    crate::compile(output, obj_output_file, false, true)
        .unwrap();

    let mut linker = Command::new("gcc")
        .arg("-o").arg(&output_path)
        .arg(&obj_output_path)
        .spawn().unwrap();
    linker.wait().expect("linker");

    // Now we should have a compiled executable.
    let mut tester = Command::new(&output_path)
        .spawn().unwrap();
    let exit = tester.wait().unwrap();
    let code = exit.code().unwrap();

    assert_eq!(code, expected_ret);
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

    // We can't use fastdrop here. In particular, the application needs to keep
    // living long enough for the code to finish compiling, and for us to
    // run the tester program.
    crate::compile(output, stdin, false, false)
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

        paste::paste! {
            #[test]
            fn [<enc_ $name>]() {
                // This is fine for now, although we might have our own CPP
                // in the future.
                test_driver_manual_encoding(true, stringify!($name), $c_src, $exit_code);
            }
        }
    }
}

macro_rules! test_no_cpp {
    ($name:ident, $exit_code:expr, $c_src:expr) => {
        #[test]
        fn $name() {
            test_driver(false, stringify!($name), $c_src, $exit_code);
        }

        // Unfortunately, pulling in the paste! macro seems to be the best
        // way to generate new names.
        //
        // (We can't just put them in e.g. mod encoding{} as it seems we're
        // not allowed to redefine a module. I guess maybe instead they could
        // each be impl {} for some struct??)
        paste::paste! {
            #[test]
            fn [<enc_ $name>]() {
                test_driver_manual_encoding(false, stringify!($name), $c_src, $exit_code);
            }
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

        paste::paste! {
            #[test]
            fn [<enc_ $name>]() {
                const C_SRC: &str = concat!("int main(void) { return ", $c_src, "; }");
                test_driver_manual_encoding(false, stringify!($name), C_SRC.as_bytes(), $exit_code);
            }
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

test_simple_expr!(not_0, 0, "!(1 + 2 + 3 + 4)");
test_simple_expr!(not_1, 1, "!(1 + 2 - 3 + 4 - 4)");

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
test_simple_expr!(cmp_equal_false, 0, "10 == 5");

test_simple_expr!(cmp_notequal_true, 1, "10 != 5");
test_simple_expr!(cmp_notequal_false, 0, "10 != 10");

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
test_simple_expr!(cmp_equal_false_tt, 0, "~~10 == ~~5");

test_simple_expr!(cmp_notequal_true_tt, 1, "~~10 != ~~5");
test_simple_expr!(cmp_notequal_false_tt, 0, "~~10 != ~~10");

test_no_cpp!(var_decl, 60, br"int main(void) {
    int a = 10;
    int b = 20;
    int c = 30;
    return a + b + c;
}");
test_no_cpp!(var_decl_assign, 24, br"int main(void) {
    int a = 10;
    int b = 20;
    int c = 30;
    a = 2;
    b = 15;
    c = 7;
    return a + b + c;
}");
test_no_cpp!(var_decl_assign_oo, 29, br"int main(void) {
    int a = 10;
    a = 2;
    int b = 20;
    int c = 30;
    b = 15;
    c = 7;
    b = a * c;
    int d = 4;
    d = d + 2;
    return a + b + c + d;
}");

test_no_cpp!(assign_compound1, 37, br"int main(void) {
    int a = 1;
    int b = 2;
    int c = 3;

    a += 5;
    b *= 4;
    c |= 22;

    return a + b + c;
}");

test_no_cpp!(assign_compound2, 2, br"int main(void) {
    int a = 1;
    int b = 2400;
    int c = 255;

    a -= 40;
    b /= 123;
    c &= 22;

    return a + b + c;
}");

test_no_cpp!(assign_compound3, 155, br"int main(void) {
    int a = 1;
    int b = 2400;
    int c = 123;
    int d = 40;

    a <<= 3;
    b >>= 6;
    c ^= 22;
    d %= 3;

    return a + b + c + d;
}");

test_no_cpp!(assign_compound1_tt, 37, br"int main(void) {
    int a = 1;
    int b = 2;
    int c = 3;

    a += ~~5;
    b *= ~~4;
    c |= ~~22;

    return a + b + c;
}");

test_no_cpp!(assign_compound2_tt, 2, br"int main(void) {
    int a = 1;
    int b = 2400;
    int c = 255;

    a -= ~~40;
    b /= ~~123;
    c &= ~~22;

    return a + b + c;
}");

test_no_cpp!(assign_compound3_tt, 155, br"int main(void) {
    int a = 1;
    int b = 2400;
    int c = 123;
    int d = 40;

    a <<= ~~3;
    b >>= ~~6;
    c ^= ~~22;
    d %= ~~3;

    return a + b + c + d;
}");

test_no_cpp!(increment_pre, 12, br"int main(void) {
    int a = 5;
    int b = ++a;

    return a + b;
}");

test_no_cpp!(increment_post, 11, br"int main(void) {
    int a = 5;
    int b = a++;

    return a + b;
}");

test_no_cpp!(decrement_pre, 8, br"int main(void) {
    int a = 5;
    int b = --a;

    return a + b;
}");

test_no_cpp!(decrement_post, 9, br"int main(void) {
    int a = 5;
    int b = a--;

    return a + b;
}");

test_no_cpp!(increment_pre_tt, 12, br"int main(void) {
    int a = ~~5;
    int b = ~~++a;

    return a + b;
}");

test_no_cpp!(increment_post_tt, 11, br"int main(void) {
    int a = ~~5;
    int b = ~~a++;

    return a + b;
}");

test_no_cpp!(decrement_pre_tt, 8, br"int main(void) {
    int a = ~~5;
    int b = ~~--a;

    return a + b;
}");

test_no_cpp!(decrement_post_tt, 9, br"int main(void) {
    int a = ~~5;
    int b = ~~a--;

    return a + b;
}");

test_no_cpp!(increment_pre_tt_parens, 12, br"int main(void) {
    int a = ~~5;
    int b = ~~(++((a)));

    return a + b;
}");

test_no_cpp!(increment_post_tt_parens, 11, br"int main(void) {
    int a = ~~5;
    int b = ~~(((a))++);

    return a + b;
}");

test_no_cpp!(decrement_pre_tt_parens, 8, br"int main(void) {
    int a = ~~5;
    int b = ~~(--((a)));

    return a + b;
}");

test_no_cpp!(decrement_post_tt_parens, 9, br"int main(void) {
    int a = ~~5;
    int b = ~~(((a))--);

    return a + b;
}");

test_no_cpp!(increment_pre_negate, 14, br"int main(void) {
    int a = -15;
    int b = -++a;
    return b;
}");

test_no_cpp!(increment_post_negate, 15, br"int main(void) {
    int a = -15;
    int b = -a++;
    return b;
}");

test_no_cpp!(decrement_pre_negate, 16, br"int main(void) {
    int a = -15;
    int b = - --a;
    return b;
}");

test_no_cpp!(decrement_post_negate, 15, br"int main(void) {
    int a = -15;
    int b = -a--;
    return b;
}");

test_no_cpp!(increment_pre_negate_parens, 14, br"int main(void) {
    int a = -15;
    int b = -++(a);
    return b;
}");

test_no_cpp!(increment_post_negate_parens, 15, br"int main(void) {
    int a = -15;
    int b = -(a)++;
    return b;
}");

test_no_cpp!(decrement_pre_negate_parens, 16, br"int main(void) {
    int a = -15;
    int b = - --(a);
    return b;
}");

test_no_cpp!(decrement_post_negate_parens, 15, br"int main(void) {
    int a = -15;
    int b = -(a)--;
    return b;
}");

test_no_cpp!(increment_pre_not, -14, br"int main(void) {
    int a = -15;
    int b = !++a;
    return a + b;
}");

test_no_cpp!(increment_post_not, -14, br"int main(void) {
    int a = -15;
    int b = !a++;
    return a + b;
}");

test_no_cpp!(inc_dec_complex, 1, br"int main(void) {
    int a = 1;
    int b = 2;
    int c1 = -++a;
    int d1 = !b--;
    int c2 = -++(a);
    int d2 = !(b)--;
    int d3 = !(b)--;

    return (a == 3 && b == -1 && c1 == -2 && d1 == 0 && c2 == -3 && d2 == 0 && d3 == 1);
}");

test_no_cpp!(if_simple, 40, br"int main(void) {
    int a = 20;
    if(1) a = 40;
    return a;
}");

test_no_cpp!(if_simple2, 20, br"int main(void) {
    int a = 20;
    if(0) a = 40;
    return a;
}");

test_no_cpp!(if_else_simple, 40, br"int main(void) {
    int a = 20;
    if(1) a = 40;
    else a = 60;
    return a;
}");

test_no_cpp!(if_else_simple2, 60, br"int main(void) {
    int a = 20;
    if(0) a = 40;
    else a = 60;
    return a;
}");

test_no_cpp!(if_chain_1, 1, br"int main(void) {
    int a = 20;
    int t = 1;
    int f = 0;
    if(t) a = 1;
    else if(t) a = 2;
    else if(t) a = 3;
    else a = 4;
    return a;
}");

test_no_cpp!(if_chain_2, 2, br"int main(void) {
    int a = 20;
    int t = 1;
    int f = 0;
    if(f) a = 1;
    else if(t) a = 2;
    else if(t) a = 3;
    else a = 4;
    return a;
}");

test_no_cpp!(if_chain_3, 3, br"int main(void) {
    int a = 20;
    int t = 1;
    int f = 0;
    if(f) a = 1;
    else if(f) a = 2;
    else if(t) a = 3;
    else a = 4;
    return a;
}");

test_no_cpp!(if_chain_4, 4, br"int main(void) {
    int a = 20;
    int t = 1;
    int f = 0;
    if(f) a = 1;
    else if(f) a = 2;
    else if(f) a = 3;
    else a = 4;
    return a;
}");

test_no_cpp!(tern_simple, 40, br"int main(void) {
    return 1 ? 40 : 20;
}");

test_no_cpp!(tern_simple2, 20, br"int main(void) {
    return 0 ? 40 : 20;
}");

test_no_cpp!(goto_forward, 25, br"int main(void) {
    int a = 30;
    goto no_add;
    a += 20;
no_add:
    a -= 5;
    return a;
}");

test_no_cpp!(goto_back, 47, br"int main(void) {
    int a = 30;
    int flag = 0;
backward:
    if(flag) a += 17;
    flag = 1;
    if(a < 31) goto backward;
    return a;
}");

test_no_cpp!(block_simple, 42, br"int main(void) {
    int sum = 0;
    int x = 30;
    {
        int x = 5;
        sum += x;
        {
            int x = 2;
            sum += x;
        }
        sum += x;
    }
    sum += x;

    return sum;
}");

test_no_cpp!(while_simple, 37, br"int main(void) {
    int counter = 0;
    int mul = 1;
    while(counter < 5) {
        mul *= 2;
        counter++;
    }
    return mul + counter;
}");

test_no_cpp!(while_with_break, 11, br"int main(void) {
    int counter = 0;
    int mul = 1;
    while(counter < 5) {
        mul *= 2;
        counter++;

        if(counter >= 3) break;
    }
    return mul + counter;
}");

test_no_cpp!(while_with_continue, 13, br"int main(void) {
    int counter = 0;
    int mul = 1;
    while(counter < 5) {
        if(counter < 2) {
            ++counter;
            continue;
        }

        mul *= 2;
        counter++;
    }
    return mul + counter;
}");

test_no_cpp!(dowhile_simple, 37, br"int main(void) {
    int counter = 0;
    int mul = 1;
    do {
        mul *= 2;
        counter++;
    } while(counter < 5);
    return mul + counter;
}");

test_no_cpp!(dowhile_once, 3, br"int main(void) {
    int counter = 0;
    int mul = 1;
    do {
        mul *= 2;
        counter++;
    } while(0);
    return mul + counter;
}");

test_no_cpp!(dowhile_once_var, 3, br"int main(void) {
    int counter = 0;
    int mul = 1;
    int zero = 0;
    do {
        mul *= 2;
        counter++;
    } while(zero);
    return mul + counter;
}");

test_no_cpp!(dowhile_with_break, 11, br"int main(void) {
    int counter = 0;
    int mul = 1;
    do {
        mul *= 2;
        counter++;

        if(counter >= 3) break;
    } while(counter < 5);
    return mul + counter;
}");

test_no_cpp!(dowhile_with_continue, 13, br"int main(void) {
    int counter = 0;
    int mul = 1;
    do {
        if(counter < 2) {
            ++counter;
            continue;
        }

        mul *= 2;
        counter++;
    } while(counter < 5);
    return mul + counter;
}");