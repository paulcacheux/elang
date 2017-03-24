use lexer::{LexicalError, Token};
use lalrpop_util::ParseError;

pub fn print_diagnostic(input: &str, error: ParseError<usize, Token, LexicalError>) {
    let error_lines = match error {
        ParseError::InvalidToken { location } => {
            println!("Invalid token.");
            get_lines(input, (location, location + 1))
        },
        ParseError::UnrecognizedToken { token, expected } => {
            let lines = if let Some((start, tok, end)) = token {
                println!("Unrecognized token: {:?}.", tok);
                get_lines(input, (start, end))
            } else {
                println!("Unexpected eof.");
                Vec::new()
            };
            if expected.len() != 0 {
                println!("Expected one of: {}", expected.join(", "));
            }
            lines
        },
        ParseError::ExtraToken { token: (start, tok, end) } => {
            println!("Extra token: {:?}.", tok);
            get_lines(input, (start, end))
        },
        ParseError::User { error: LexicalError { msg, pos } } => {
            println!("{}", msg);
            get_lines(input, (pos, pos + 1))
        }
    };

    for (i, (line, arrow)) in error_lines {
        println!("{:<5}: {}", i+1, line);
        println!("       {}", arrow);
    }
}

fn get_lines<'a>(input: &'a str, span: (usize, usize)) -> Vec<(usize, (&'a str, String))> {
    let mut arrow = String::with_capacity(input.len());

    for (i, c) in input.chars().enumerate() {
        arrow.push(match c {
            '\n' => '\n',
            _ if span.0 <= i && i < span.1 => '^',
            _ => ' ',
        });
    }

    input.lines()
        .zip(arrow.lines().map(String::from))
        .enumerate()
        .filter(|&(_, (_, ref arrow))| {
            arrow.contains('^')
        })
        .collect()
}
