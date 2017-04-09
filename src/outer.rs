use std;
use std::path::PathBuf;

use pipeline::{CompileOptions, OutputType};
use ir;
use codegen;

fn get_writer(output_path: &Option<PathBuf>) -> Box<std::io::Write>{
    if let Some(ref output_path) = *output_path {
        Box::new(std::fs::File::create(output_path).unwrap())
    } else {
        Box::new(std::io::stdout())
    }
}

pub fn main_outer(tu: ir::TranslationUnit, options: &CompileOptions) {
    match options.output_type {
        OutputType::None => {}
        OutputType::C => {
            let mut writer = get_writer(&options.output_path);
            codegen::c_gen::gen_translation_unit(&mut writer, tu).expect("error gen");
        }
        OutputType::LLVM => {
            let mut writer = get_writer(&options.output_path);
            codegen::llvm_gen::gen_translation_unit(&mut writer, tu).expect("error gen");
        }
        OutputType::Run => { unimplemented!() }
    }
}
