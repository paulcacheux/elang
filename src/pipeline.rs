use std::path::{Path, PathBuf};

use lexer;
use parser;
use ast;
use diagnostics;
use diagnostics::ToError;
use ir;

use ir::GlobalTable;
use source_manager::SourceManager;

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

pub fn build_path(id: &str, options: &CompileOptions) -> PathBuf {
    let mut path = options.global_dir.clone();
    path.push(format!("{}.li", id));
    path
}

pub fn process_main_path<P: AsRef<Path>>(input_path: P,
                                         options: &CompileOptions,
                                         source_manager: &mut SourceManager)
                                         -> Result<ir::TranslationUnit, diagnostics::Error> {
    let mut globals_table = GlobalTable::new();
    let mut tu = process_path(input_path, options, source_manager, &mut globals_table)?;

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
                                    source_manager: &mut SourceManager,
                                    globals_table: &mut GlobalTable)
                                    -> Result<ir::TranslationUnit, diagnostics::Error> {
    let input_index = source_manager
        .register_file(input_path)
        .expect("Can't read input file");

    let mut ast_tu = {
        let lex = lexer::Lexer::new(source_manager.get_input(input_index));
        match parser::parse_TranslationUnit(input_index, lex) {
            Ok(ast_tu) => ast_tu,
            Err(err) => return Err(err.convert(source_manager, input_index)),
        }
    };

    if options.print_ast {
        ast::printer::print_ast(&ast_tu);
    }

    let declarations = process_imports(&mut ast_tu, options, source_manager, globals_table)?;
    match ir::builder::build_translation_unit(ast_tu, declarations, globals_table) {
        Ok(tu) => Ok(tu),
        Err(err) => Err(err.convert(source_manager, input_index)),
    }
}

pub fn process_imports(tu: &mut ast::TranslationUnit,
                       options: &CompileOptions,
                       source_manager: &mut SourceManager,
                       globals_table: &mut GlobalTable)
                       -> Result<Vec<ir::Declaration>, diagnostics::Error> {
    let mut declarations = Vec::new();

    for import in tu.imports.drain(..) {
        let path = build_path(&import, options);
        let imported_tu = process_path(path, options, source_manager, globals_table)?;
        declarations.extend(imported_tu.declarations);
    }

    Ok(declarations)
}
