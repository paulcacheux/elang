extern crate clap;
extern crate itertools;
extern crate unicode_xid;
extern crate lalrpop_util;

use clap::{Arg, App};
use std::path::Path;

mod pipeline;
mod ast;
mod parser;
mod lexer;
mod diagnostics;
mod ir;
mod codegen;

fn main() {
    let matches = App::new("Elang Compiler")
        .version("0.1")
        .author("Paul CACHEUX <paulcacheux@gmail.com>")
        .arg(Arg::with_name("ast").long("ast").help("Dump AST"))
        .arg(Arg::with_name("ir").long("ir").help("Dump IR"))
        .arg(Arg::with_name("opt")
                 .short("O")
                 .help("Activate optimizations"))
        .arg(Arg::with_name("INPUT")
                 .help("Input file")
                 .required(true)
                 .index(1))
        .arg(Arg::with_name("OUTPUT")
                 .help("Output file")
                 .long("output")
                 .short("o")
                 .value_name("FILE")
                 .takes_value(true))
        .arg(Arg::with_name("output_type")
                 .help("Output type")
                 .long("type")
                 .short("t")
                 .takes_value(true)
                 .possible_values(&["c", "llvm", "none"]))
        .get_matches();

    let input_path = Path::new(matches.value_of("INPUT").unwrap());
    let global_dir = input_path.parent().unwrap();

    let options = pipeline::CompileOptions {
        global_dir: global_dir.to_path_buf(),
        print_ast: matches.is_present("ast"),
        print_ir: matches.is_present("ir"),
        opt: matches.is_present("opt"),
    };

    let tu = pipeline::process_main_path(input_path, &options);

    let mut output_writer: Box<std::io::Write> = if let Some(output_path) = matches.value_of("OUTPUT") {
        Box::new(std::fs::File::create(output_path).unwrap())
    } else {
        Box::new(std::io::stdout())
    };

    match matches.value_of("output_type").unwrap_or("none") {
        "c" => {
            codegen::c_gen::gen_translation_unit(&mut output_writer, tu).expect("error gen");
        },
        "llvm" => {
            codegen::llvm_gen::gen_translation_unit(&mut output_writer, tu).expect("error gen");
        },
        "none" => {}
        _ => unreachable!()
    }
}
