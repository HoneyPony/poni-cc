use std::{fs::File, io::Write};

static TESTS: &[&str] = &[
    "binop/add.c",
    "binop/div.c",
    "binop/mod.c",
    "binop/mul.c",
    "binop/sub.c",
];

fn main() {
    // Only rerun when we update the build script source code
	println!("cargo::rerun-if-changed=build");

    let out_dir = std::env::var("OUT_DIR").unwrap();
    let mut file = File::create(format!("{}/tests.gen.rs", out_dir))
        .unwrap();

    for test in TESTS {
        let test_name = test
            .replace('/', "_")
            .replace('.', "_");
        
        writeln!(file, "#[test]").unwrap();
        writeln!(file, "fn test_{}() {{", test_name).unwrap();
        // NOTE: We could consider making the test case return code get built
        // at build time...?
        //
        // Another NOTE: Maybe our tests should just be manually written? For
        // these very simple test cases, it is probably easier to write them in
        // Rust then to create two files for each one...
        writeln!(file, "\trun_test(\"{}\", \"{}.out\")",
            test, test).unwrap();
        writeln!(file, "}}").unwrap();
    }
}