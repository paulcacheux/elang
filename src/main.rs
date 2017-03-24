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
        .arg(Arg::with_name("ast")
            .long("ast")
            .help("Dump AST"))
        .arg(Arg::with_name("INPUT")
            .help("Input file")
            .required(true)
            .index(1))
        .get_matches();

    let input = read_file(matches.value_of("INPUT").unwrap()).expect("Can't read input file");
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
}
