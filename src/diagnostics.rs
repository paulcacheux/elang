use std::path::Path;

use lexer::{LexicalError, Token};
use lalrpop_util::ParseError;

use span::Span;
use semantic_error::SemanticError;

pub struct Error<'a> {
    msg: String,
    lines: Vec<Line<'a>>,
}

struct Line<'a> {
    n: usize,
    text: &'a str,
    arrow: String,
}

pub trait ToError<'a> {
    fn convert(self, input: &'a str) -> Error<'a>;
}

impl<'a> ToError<'a> for ParseError<usize, Token, LexicalError> {
    fn convert(self, input: &'a str) -> Error<'a> {
        match self {
            ParseError::InvalidToken { location } => {
                Error {
                    msg: "Invalid token.\n".to_string(),
                    lines: get_lines(input, Span(location, location + 1)),
                }
            }
            ParseError::UnrecognizedToken { token, expected } => {
                let mut error = if let Some((start, tok, end)) = token {
                    Error {
                        msg: format!("Unrecognized token: {:?}.\n", tok),
                        lines: get_lines(input, Span(start, end)),
                    }
                } else {
                    Error {
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
                    msg: format!("Extra token: {:?}.", tok),
                    lines: get_lines(input, Span(start, end)),
                }
            }
            ParseError::User { error: LexicalError { msg, pos } } => {
                Error {
                    msg: msg,
                    lines: get_lines(input, Span(pos, pos + 1)),
                }
            }
        }
    }
}

impl<'a> ToError<'a> for SemanticError {
    fn convert(self, input: &'a str) -> Error<'a> {
        Error {
            msg: self.kind.to_string(),
            lines: get_lines(input, self.span),
        }
    }
}

pub fn print_diagnostic<'a, E: ToError<'a>, P: AsRef<Path>>(input: &'a str, input_path: P, error: E) {
    let error = error.convert(input);
    println!("Error in: {}", input_path.as_ref().display());
    println!("{}", error.msg);
    for line in error.lines {
        println!("{:<5}: {}", line.n + 1, line.text);
        println!("       {}", line.arrow);
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
