use std;
use std::path::{Path, PathBuf};
use std::fs::File;
use std::io;

use tempdir::TempDir;

use pipeline::{CompileOptions, OutputType};
use ir;
use codegen;

fn get_writer(output_path: &Option<PathBuf>) -> io::Result<Box<std::io::Write>>{
    if let Some(ref output_path) = *output_path {
        Ok(Box::new(std::fs::File::create(output_path)?))
    } else {
        Ok(Box::new(std::io::stdout()))
    }
}

pub fn main_outer(tu: ir::TranslationUnit, input_path: &str, options: &CompileOptions) -> io::Result<()> {
    match options.output_type {
        OutputType::None => Ok(()),
        OutputType::LLVM => {
            let mut writer = get_writer(&options.output_path)?;
            codegen::llvm_gen::gen_translation_unit(&mut writer, tu)
        }
        OutputType::Run => {
            run_with_llvm(tu, input_path, options)
        }
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

fn run_with_llvm(tu: ir::TranslationUnit, input_path: &str, options: &CompileOptions) -> io::Result<()> {
    let exec_name = executable_name(input_path);

    let tmp_dir = TempDir::new("elang-compiler")?;
    let llvm_path = tmp_dir.path().join(format!("{}.ll", exec_name));
    let obj_path = tmp_dir.path().join(format!("{}.o", exec_name));
    let exec_path = tmp_dir.path().join(exec_name);

    let mut llvm_file = File::create(&llvm_path)?;
    codegen::llvm_gen::gen_translation_unit(&mut llvm_file, tu).expect("error llvm gen");

    if options.opt {
        if !std::process::Command::new("opt")
            .arg("-O3")
            .arg("-S")
            .arg(llvm_path.to_str().unwrap())
            .arg("-o").arg(llvm_path.to_str().unwrap())
            .spawn()?
            .wait()?
            .success() {
            panic!("opt fail");
        }
    }

    if !std::process::Command::new("llc")
        .arg("-filetype=obj")
        .arg(llvm_path.to_str().unwrap())
        .arg("-o").arg(obj_path.to_str().unwrap())
        .spawn()?
        .wait()?
        .success() {
        panic!("llc fail");
    }

    if !std::process::Command::new("clang")
        .arg(obj_path.to_str().unwrap())
        .arg("-o").arg(exec_path.to_str().unwrap())
        .spawn()?
        .wait()?
        .success() {
        panic!("clang fail");
    }

    if !std::process::Command::new(exec_path)
        .spawn()?
        .wait()?
        .success() {
        panic!("exec fail");
    }

    Ok(())
}
