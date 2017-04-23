extern crate elang;
extern crate itertools;
extern crate tempdir;

use tempdir::TempDir;
use itertools::Itertools;

use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::path::PathBuf;

fn read_expected_output(path: &str) -> String {
    let file = File::open(path).expect("io error");
    let reader = BufReader::new(file);

    reader
        .lines()
        .map(|r| r.expect("io error"))
        .take_while(|line| line.starts_with("//"))
        .map(|line| format!("{}\n", &line[2..]))
        .join("")
}

fn run(path: &str) -> String {
    use elang::pipeline;
    use elang::outer;
    use elang::pipeline::{CompileOptions, OutputType};
    use elang::source_manager::SourceManager;

    let pathbuf = PathBuf::from(path);
    let tmp_dir = TempDir::new("elang-test").expect("dir error");
    let exec_path = tmp_dir.path().join("exec.out");

    let options = CompileOptions {
        global_dir: pathbuf.parent().unwrap().to_path_buf(),
        print_ast: false,
        print_ir: false,
        opt: true,
        output_type: OutputType::Exec,
        output_path: Some(exec_path.to_path_buf())
    };

    let mut source_manager = SourceManager::new();
    let tu = pipeline::process_main_path(path, &options, &mut source_manager).expect("diag error");
    outer::main_outer(tu, path, &options).expect("outer error");

    let output = std::process::Command::new(exec_path).output().expect("io error");

    String::from_utf8_lossy(&output.stdout).into_owned()
}

macro_rules! output_test {
    ( $name:ident, $path:expr ) => {
        #[test]
        fn $name() {
            let path = $path;
            let expected_output = read_expected_output(path);
            let found_output = run(path);

            assert_eq!(expected_output, found_output);
        }
    }
}

output_test!(func, "tests/output/func.li");
output_test!(parentheses, "tests/output/parentheses.li");
output_test!(precedence, "tests/output/precedence.li");
output_test!(primes, "tests/output/primes.li");
