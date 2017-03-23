extern crate clap;
extern crate itertools;
use clap::{Arg, App};

use std::fs::File;
use std::io::Read;
use std::io;
use std::path::Path;

mod parser;
mod lexer;
mod ast;
mod ast_printer;

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
            println!("{:?}", err);
            return
        }
    };

    if matches.is_present("ast") {
        ast_printer::print_ast(&tu);
    }
}
