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

fn read_file<P: AsRef<Path>>(path: P) -> io::Result<String> {
    let mut file = File::open(path)?;
    let mut buffer = String::new();
    file.read_to_string(&mut buffer)?;
    Ok(buffer)
}

fn remove_comments(input: String) -> String {
    enum State {
        Main,
        OneSlash,
        LineComment,
        MultiStar(usize),
        MultiSlash(usize),
        MultiComment(usize),
    }

    let mut output = String::with_capacity(input.len());

    let mut state = State::Main;
    for c in input.chars() {
        state = match (state, c) {
            (State::Main, '/') => State::OneSlash,
            (State::OneSlash, '/') => State::LineComment,
            (State::OneSlash, '*') => State::MultiComment(0),
            (State::OneSlash, c) => {
                output.push('/');
                output.push(c);
                State::Main
            }
            (State::LineComment, '\n') => {
                output.push('\n');
                State::Main
            },
            (State::LineComment, _) => State::LineComment,
            (State::MultiComment(n), '*') => State::MultiStar(n),
            (State::MultiComment(n), '/') => State::MultiSlash(n),
            (State::MultiComment(n), _) => State::MultiComment(n),
            (State::MultiSlash(n), '*') => State::MultiComment(n + 1),
            (State::MultiSlash(n), '/') => State::MultiSlash(n),
            (State::MultiSlash(n), _) => State::MultiComment(n),
            (State::MultiStar(n), '/') if n == 0 => State::Main,
            (State::MultiStar(n), '/') => State::MultiComment(n - 1),
            (State::MultiStar(n), c) => {
                output.push('*');
                output.push(c);
                State::MultiComment(n)
            }
            (State::Main, c) => {
                output.push(c);
                State::Main
            }
        }
    }
    output
}

fn main() {
    let matches = App::new("Elang Compiler")
        .version("0.1")
        .author("Paul CACHEUX <paulcacheux@gmail.com>")
        .arg(Arg::with_name("ast")
            .long("ast")
            .help("Dump AST"))
        .arg(Arg::with_name("ir")
            .long("ir")
            .help("Dump IR"))
        .arg(Arg::with_name("opt")
            .short("O")
            .help("Activate optimizations"))
        .arg(Arg::with_name("INPUT")
            .help("Input file")
            .required(true)
            .index(1))
        .get_matches();

    let input = read_file(matches.value_of("INPUT").unwrap()).expect("Can't read input file");
    let input = remove_comments(input);
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
}
