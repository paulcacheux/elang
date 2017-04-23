use std;
use std::io::Write;

use lexer::{LexicalError, Token};
use lalrpop_util::ParseError;

use span::Span;
use ir::builder::SemanticError;
use source_manager::SourceManager;

#[derive(Debug)]
pub struct Error {
    source_index: usize,
    msg: String,
    lines: Vec<Line>,
}

#[derive(Debug)]
struct Line {
    n: usize,
    text: String,
    arrow: String,
}

pub trait ToError {
    fn convert(self, source_manager: &SourceManager, source_index: usize) -> Error;
}

impl ToError for ParseError<usize, Token, LexicalError> {
    fn convert(self, source_manager: &SourceManager, source_index: usize) -> Error {
        match self {
            ParseError::InvalidToken { location } => {
                Error {
                    source_index: source_index,
                    msg: "Invalid token.\n".to_string(),
                    lines: get_lines(source_manager, Span {
                        source_index: source_index,
                        lo: location,
                        hi: location + 1
                    }),
                }
            }
            ParseError::UnrecognizedToken { token, expected } => {
                let mut error = if let Some((start, tok, end)) = token {
                    Error {
                        source_index: source_index,
                        msg: format!("Unrecognized token: {:?}.\n", tok),
                        lines: get_lines(source_manager, Span {
                            source_index: source_index,
                            lo: start,
                            hi: end
                        }),
                    }
                } else {
                    Error {
                        source_index: source_index,
                        msg: "Unexpected eof.".to_string(),
                        lines: Vec::new(),
                    }
                };
                if !expected.is_empty() {
                    error
                        .msg
                        .push_str(&format!("Expected one of: {}", expected.join(", ")));
                }
                error
            }
            ParseError::ExtraToken { token: (start, tok, end) } => {
                Error {
                    source_index: source_index,
                    msg: format!("Extra token: {:?}.", tok),
                    lines: get_lines(source_manager, Span {
                        source_index: source_index,
                        lo: start,
                        hi: end
                    }),
                }
            }
            ParseError::User { error: LexicalError { msg, pos } } => {
                Error {
                    source_index: source_index,
                    msg: msg,
                    lines: get_lines(source_manager, Span {
                        source_index: source_index,
                        lo: pos,
                        hi: pos + 1
                    }),
                }
            }
        }
    }
}

impl ToError for SemanticError {
    fn convert(self, source_manager: &SourceManager, source_index: usize) -> Error {
        Error {
            source_index: source_index,
            msg: self.kind.to_string(),
            lines: get_lines(source_manager, self.span),
        }
    }
}

fn get_lines(source_manager: &SourceManager, span: Span) -> Vec<Line> {
    let input = source_manager.get_input(span.source_index);
    let mut arrow = String::with_capacity(input.len());

    for (i, c) in input.chars().enumerate() {
        arrow.push(match c {
                       '\n' => '\n',
                       _ if span.lo <= i && i < span.hi=> '^',
                       _ => ' ',
                   });
    }

    input
        .lines()
        .map(String::from)
        .zip(arrow.lines().map(String::from))
        .enumerate()
        .filter(|&(_, (_, ref arrow))| arrow.contains('^'))
        .map(|(n, (t, a))| {
                 Line {
                     n: n,
                     text: t,
                     arrow: a,
                 }
             })
        .collect()
}

macro_rules! eprintln {
    ($fmt:expr, $($arg:tt)*) => (writeln!(std::io::stderr(), $fmt, $($arg)*).unwrap())
}

pub fn print_diagnostic(source_manager: &SourceManager, error: Error) {
    eprintln!("Error in: {}", source_manager.get_file_path(error.source_index).display());
    eprintln!("{}", error.msg);
    for line in error.lines {
        eprintln!("{:<5}: {}", line.n + 1, line.text);
        eprintln!("       {}", line.arrow);
    }
}
