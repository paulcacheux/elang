extern crate itertools;
extern crate unicode_xid;
extern crate lalrpop_util;
extern crate tempdir;
extern crate rayon;

pub mod pipeline;
pub mod span;
pub mod lexer;
pub mod parser;
pub mod ast;
pub mod ir;
pub mod codegen;
pub mod diagnostics;
pub mod outer;
