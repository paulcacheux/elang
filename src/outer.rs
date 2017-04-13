use std;
use std::path::{Path, PathBuf};
use std::fs::File;
use std::io;

use tempdir::TempDir;

use pipeline::{CompileOptions, OutputType};
use ir;
use codegen;

fn get_writer(output_path: &Option<PathBuf>) -> io::Result<Box<std::io::Write>> {
    if let Some(ref output_path) = *output_path {
        Ok(Box::new(std::fs::File::create(output_path)?))
    } else {
        Ok(Box::new(std::io::stdout()))
    }
}

pub fn main_outer(tu: ir::TranslationUnit,
                  input_path: &str,
                  options: &CompileOptions)
                  -> io::Result<()> {
    match options.output_type {
        OutputType::None => Ok(()),
        OutputType::LLVM => {
            let mut writer = get_writer(&options.output_path)?;
            codegen::llvm_gen::gen_translation_unit(&mut writer, tu)
        }
        OutputType::Run => run_with_llvm(tu, input_path, options),
    }
}

fn executable_name(path: &str) -> String {
    let file_name = Path::new(path).file_name().unwrap().to_str().unwrap();

    let mut name_parts: Vec<_> = file_name.split('.').collect();
    let parts_len = name_parts.len();
    if parts_len > 1 {
        name_parts.pop();
    }

    name_parts.join(".")
}

fn run_with_llvm(tu: ir::TranslationUnit,
                 input_path: &str,
                 options: &CompileOptions)
                 -> io::Result<()> {
    fn path_to_str(path: &PathBuf) -> &str {
        path.to_str().unwrap()
    }

    let exec_name = executable_name(input_path);

    let tmp_dir = TempDir::new("elang-compiler")?;
    let llvm_path = tmp_dir.path().join(format!("{}.ll", exec_name));
    let obj_path = tmp_dir.path().join(format!("{}.o", exec_name));
    let exec_path = tmp_dir.path().join(exec_name);

    let mut llvm_file = File::create(&llvm_path)?;
    codegen::llvm_gen::gen_translation_unit(&mut llvm_file, tu).expect("error llvm gen");

    if options.opt &&
       !run_command("opt",
                    &["-O3", "-S", path_to_str(&llvm_path), "-o", path_to_str(&llvm_path)])?
                .success() {
        panic!("opt fail");
    }

    if !run_command("llc",
                    &["-filetype=obj", path_to_str(&llvm_path), "-o", path_to_str(&obj_path)])?
                .success() {
        panic!("llc fail");
    }

    if !run_command("clang",
                    &[path_to_str(&obj_path), "-o", path_to_str(&exec_path)])?
                .success() {
        panic!("clang fail");
    }

    if !run_command(exec_path, &[])?.success() {
        panic!("exec fail");
    }

    Ok(())
}

fn run_command<S: AsRef<std::ffi::OsStr>>(ex: S, args: &[&str]) -> io::Result<std::process::ExitStatus> {
    std::process::Command::new(ex).args(args).status()
}
