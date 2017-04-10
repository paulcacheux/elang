use std::fs::File;
use std::io::Read;
use std::io;
use std::path::{Path, PathBuf};
use std::process::exit;

use lexer;
use parser;
use ast;
use diagnostics;
use ir;

use ir::symbol_table::SymbolTable;

#[derive(Debug, Clone, Copy)]
pub enum OutputType {
    None,
    C,
    LLVM,
    Run,
}

impl OutputType {
    pub fn new(ot: &str) -> Option<OutputType> {
        match ot {
            "none" => Some(OutputType::None),
            "c" => Some(OutputType::C),
            "llvm" => Some(OutputType::LLVM),
            "run" => Some(OutputType::Run),
            _ => None
        }
    }
}

#[derive(Debug)]
pub struct CompileOptions {
    pub global_dir: PathBuf,
    pub print_ast: bool,
    pub print_ir: bool,
    pub opt: bool,
    pub output_type: OutputType,
    pub output_path: Option<PathBuf>,
}

fn read_file<P: AsRef<Path>>(path: P) -> io::Result<String> {
    let mut file = File::open(path)?;
    let mut buffer = String::new();
    file.read_to_string(&mut buffer)?;
    Ok(buffer)
}

pub fn build_path(id: &String, options: &CompileOptions) -> PathBuf {
    let mut path = options.global_dir.clone();
    path.push(format!("{}.li", id));
    path
}

pub fn process_main_path<P: AsRef<Path>>(input_path: P,
                                         options: &CompileOptions)
                                         -> ir::TranslationUnit {
    let mut symbol_table = SymbolTable::new();
    let mut tu = process_path(input_path, options, &mut symbol_table);

    if options.opt {
        ir::opt::opt_translation_unit(&mut tu);
    }

    if options.print_ir {
        ir::printer::print_ir(&tu);
    }

    tu
}

pub fn process_path<P: AsRef<Path>>(input_path: P,
                                    options: &CompileOptions,
                                    symbol_table: &mut SymbolTable)
                                    -> ir::TranslationUnit {
    let input = read_file(&input_path).expect("Can't read input file");
    let lex = lexer::Lexer::new(&input);
    let ast_tu = match parser::parse_TranslationUnit(lex) {
        Ok(ast_tu) => ast_tu,
        Err(err) => {
            diagnostics::print_diagnostic(&input, input_path, err);
            exit(1);
        }
    };

    if options.print_ast {
        ast::printer::print_ast(&ast_tu);
    }

    match ir::builder::build_translation_unit(ast_tu, symbol_table, options) {
        Ok(tu) => tu,
        Err(err) => {
            diagnostics::print_diagnostic(&input, input_path, err);
            exit(1);
        }
    }
}
