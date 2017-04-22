use std;
use std::io::Write;
use std::path::PathBuf;

use lexer::{LexicalError, Token};
use lalrpop_util::ParseError;

use span::Span;
use ir::builder::SemanticError;

#[derive(Debug)]
pub struct Error {
    file_path: PathBuf,
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
    fn convert(self, input: &str, file_path: PathBuf) -> Error;
}

impl ToError for ParseError<usize, Token, LexicalError> {
    fn convert(self, input: &str, file_path: PathBuf) -> Error {
        match self {
            ParseError::InvalidToken { location } => {
                Error {
                    file_path: file_path,
                    msg: "Invalid token.\n".to_string(),
                    lines: get_lines(input, Span(location, location + 1)),
                }
            }
            ParseError::UnrecognizedToken { token, expected } => {
                let mut error = if let Some((start, tok, end)) = token {
                    Error {
                        file_path: file_path,
                        msg: format!("Unrecognized token: {:?}.\n", tok),
                        lines: get_lines(input, Span(start, end)),
                    }
                } else {
                    Error {
                        file_path: file_path,
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
                    file_path: file_path,
                    msg: format!("Extra token: {:?}.", tok),
                    lines: get_lines(input, Span(start, end)),
                }
            }
            ParseError::User { error: LexicalError { msg, pos } } => {
                Error {
                    file_path: file_path,
                    msg: msg,
                    lines: get_lines(input, Span(pos, pos + 1)),
                }
            }
        }
    }
}

impl ToError for SemanticError {
    fn convert(self, input: &str, file_path: PathBuf) -> Error {
        Error {
            file_path: file_path,
            msg: self.kind.to_string(),
            lines: get_lines(input, self.span),
        }
    }
}

macro_rules! eprintln {
    ($fmt:expr, $($arg:tt)*) => (writeln!(std::io::stderr(), $fmt, $($arg)*).unwrap())
}

pub fn print_diagnostic(error: Error) {
    eprintln!("Error in: {}", error.file_path.display());
    eprintln!("{}", error.msg);
    for line in error.lines {
        eprintln!("{:<5}: {}", line.n + 1, line.text);
        eprintln!("       {}", line.arrow);
    }
}

fn get_lines(input: &str, span: Span) -> Vec<Line> {
    let mut arrow = String::with_capacity(input.len());

    for (i, c) in input.chars().enumerate() {
        arrow.push(match c {
                       '\n' => '\n',
                       _ if span.0 <= i && i < span.1 => '^',
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
