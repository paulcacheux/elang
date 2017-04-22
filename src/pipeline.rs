use std::fs::File;
use std::io::Read;
use std::io;
use std::path::{Path, PathBuf};

use lexer;
use parser;
use ast;
use diagnostics;
use ir;

use ir::GlobalTable;

#[derive(Debug, Clone, Copy)]
pub enum OutputType {
    Check,
    LLVM,
    Run,
    Exec,
}

impl OutputType {
    pub fn new(ot: &str) -> Option<OutputType> {
        match ot {
            "check" => Some(OutputType::Check),
            "llvm" => Some(OutputType::LLVM),
            "run" => Some(OutputType::Run),
            "exec" => Some(OutputType::Exec),
            _ => None,
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

fn slurp_file<P: AsRef<Path>>(path: P) -> io::Result<String> {
    let mut file = File::open(path)?;
    let mut buffer = String::new();
    file.read_to_string(&mut buffer)?;
    Ok(buffer)
}

pub fn build_path(id: &str, options: &CompileOptions) -> PathBuf {
    let mut path = options.global_dir.clone();
    path.push(format!("{}.li", id));
    path
}

pub fn process_main_path<P: AsRef<Path>>(input_path: P,
                                         options: &CompileOptions)
                                         -> Result<ir::TranslationUnit, ()> {
    let mut globals_table = GlobalTable::new();
    let mut tu = process_path(input_path, options, &mut globals_table)?;

    if options.opt {
        ir::opt::opt_translation_unit(&mut tu);
    }

    if options.print_ir {
        ir::printer::print_ir(&tu);
    }

    Ok(tu)
}

pub fn process_path<P: AsRef<Path>>(input_path: P,
                                    options: &CompileOptions,
                                    globals_table: &mut GlobalTable)
                                    -> Result<ir::TranslationUnit, ()> {
    let input = slurp_file(&input_path).expect("Can't read input file");
    let lex = lexer::Lexer::new(&input);
    let mut ast_tu = match parser::parse_TranslationUnit(lex) {
        Ok(ast_tu) => ast_tu,
        Err(err) => {
            diagnostics::print_diagnostic(&input, input_path, err);
            return Err(())
        }
    };

    if options.print_ast {
        ast::printer::print_ast(&ast_tu);
    }

    let declarations = process_imports(&mut ast_tu, options, globals_table)?;
    match ir::builder::build_translation_unit(ast_tu, declarations, globals_table) {
        Ok(tu) => Ok(tu),
        Err(err) => {
            diagnostics::print_diagnostic(&input, input_path, err);
            Err(())
        }
    }
}

pub fn process_imports(tu: &mut ast::TranslationUnit,
                       options: &CompileOptions,
                       globals_table: &mut GlobalTable)
                       -> Result<Vec<ir::Declaration>, ()> {
    let mut declarations = Vec::new();

    for import in tu.imports.drain(..) {
        let path = build_path(&import, options);
        let imported_tu = process_path(path, options, globals_table)?;
        declarations.extend(imported_tu.declarations);
    }

    Ok(declarations)
}
