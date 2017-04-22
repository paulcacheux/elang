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

fn redirect_stream<R, W>(reader: &mut R, writer: &mut W) -> io::Result<()>
    where R: io::Read,
          W: io::Write
{
    let mut buffer = vec![0; 64 * 1024];

    loop {
        let len_read = try!(reader.read(&mut buffer));

        if len_read == 0 {
            return Ok(());
        }

        try!(writer.write_all(&buffer[..len_read]));
    }
}

pub fn main_outer(tu: ir::TranslationUnit,
                  input_path: &str,
                  options: &CompileOptions)
                  -> io::Result<()> {
    match options.output_type {
        OutputType::Check => Ok(()),
        OutputType::LLVM => {
            let tmp_dir = TempDir::new("elang-compiler")?;
            let llvm_path = output_llvm(tu, input_path, tmp_dir.path(), options)?;

            let mut reader = std::fs::File::open(llvm_path)?;
            let mut writer = get_writer(&options.output_path)?;
            redirect_stream(&mut reader, &mut writer)
        }
        OutputType::Run => {
            let tmp_dir = TempDir::new("elang-compiler")?;
            let exec_path = output_exec(tu, input_path, tmp_dir.path(), options)?;
            if !run_command(exec_path, &[])?.success() {
                panic!("exec fail");
            }
            Ok(())
        }
        OutputType::Exec => {
            let tmp_dir = TempDir::new("elang-compiler")?;
            let exec_path = output_exec(tu, input_path, tmp_dir.path(), options)?;
            let default_path = PathBuf::from("a.out");
            std::fs::copy(exec_path,
                          options.output_path.as_ref().unwrap_or(&default_path))?;
            Ok(())
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

fn path_to_str(path: &PathBuf) -> &str {
    path.to_str().unwrap()
}

fn output_llvm(tu: ir::TranslationUnit,
               input_path: &str,
               tmp_dir_path: &Path,
               options: &CompileOptions)
               -> io::Result<PathBuf> {
    let exec_name = executable_name(input_path);

    let llvm_path = tmp_dir_path.join(format!("{}.ll", exec_name));

    let mut llvm_file = File::create(&llvm_path)?;
    codegen::llvm_gen::gen_translation_unit(&mut llvm_file, tu).expect("error llvm gen");

    if options.opt &&
       !run_command("opt",
                    &["-O3", "-S", path_to_str(&llvm_path), "-o", path_to_str(&llvm_path)])
                ?
                .success() {
        panic!("opt fail");
    }

    Ok(llvm_path)
}

fn output_exec(tu: ir::TranslationUnit,
               input_path: &str,
               tmp_dir_path: &Path,
               options: &CompileOptions)
               -> io::Result<PathBuf> {
    let exec_name = executable_name(input_path);
    let llvm_path = output_llvm(tu, input_path, tmp_dir_path, options)?;
    let obj_path = tmp_dir_path.join(format!("{}.o", exec_name));
    let exec_path = tmp_dir_path.join(exec_name);

    if !run_command("llc",
                    &["-filetype=obj", path_to_str(&llvm_path), "-o", path_to_str(&obj_path)])
                ?
                .success() {
        panic!("llc fail");
    }

    if !run_command("clang",
                    &[path_to_str(&obj_path), "-o", path_to_str(&exec_path)])
                ?
                .success() {
        panic!("clang fail");
    }

    Ok(exec_path)
}

fn run_command<S: AsRef<std::ffi::OsStr>>(ex: S,
                                          args: &[&str])
                                          -> io::Result<std::process::ExitStatus> {
    std::process::Command::new(ex).args(args).status()
}
