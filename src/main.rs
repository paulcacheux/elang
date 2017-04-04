extern crate clap;
extern crate itertools;
extern crate unicode_xid;
extern crate lalrpop_util;
use clap::{Arg, App};

use std::fs::File;
use std::io::Read;
use std::io;
use std::path::Path;

mod ast;
mod parser;
mod lexer;
mod diagnostics;
mod ir;
mod codegen;

fn read_file<P: AsRef<Path>>(path: P) -> io::Result<String> {
    let mut file = File::open(path)?;
    let mut buffer = String::new();
    file.read_to_string(&mut buffer)?;
    Ok(buffer)
}

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
        .get_matches();

    let input_path = matches.value_of("INPUT").unwrap();
    let input = read_file(input_path).expect("Can't read input file");
    let lex = lexer::Lexer::new(&input);
    let tu = match parser::parse_TranslationUnit(lex) {
        Ok(tu) => tu,
        Err(err) => {
            return diagnostics::print_diagnostic(&input, err);
        }
    };

    if matches.is_present("ast") {
        ast::printer::print_ast(&tu);
    }

    let mut tu = match ir::builder::build_translation_unit(tu) {
        Ok(tu) => tu,
        Err(err) => {
            return diagnostics::print_diagnostic(&input, err);
        }
    };

    if matches.is_present("opt") {
        ir::opt::opt_translation_unit(&mut tu);
    }

    if matches.is_present("ir") {
        ir::printer::print_ir(&tu);
    }

    let default_output_path = format!("{}.c", input_path);
    let output_path = matches.value_of("OUTPUT").unwrap_or(&default_output_path);
    let mut file = File::create(output_path).unwrap();
    codegen::c_gen::gen_translation_unit(&mut file, tu).expect("error gen");
}
