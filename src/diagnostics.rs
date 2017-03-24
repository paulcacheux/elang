use lexer::{LexicalError, Token};
use lalrpop_util::ParseError;

pub fn print_diagnostic(input: &str, error: ParseError<usize, Token, LexicalError>) {
    let mut error_lines = Vec::new();

    match error {
        ParseError::InvalidToken { location } => {
            error_lines.extend(get_lines(input, (location, location + 1)));
            println!("Invalid token.");
        },
        ParseError::UnrecognizedToken { token, expected } => {
            if let Some((start, tok, end)) = token {
                error_lines.extend(get_lines(input, (start, end)));
                println!("Unrecognized token: {:?}.", tok);
            } else {
                println!("Unexpected eof.");
            }
            println!("Expected one of: {}", expected.join(", "));
        },
        ParseError::ExtraToken { token: (start, tok, end) } => {
            error_lines.extend(get_lines(input, (start, end)));
            println!("Extra token: {:?}.", tok);
        },
        ParseError::User { error: LexicalError { msg, pos } } => {
            error_lines.extend(get_lines(input, (pos, pos + 1)));
            println!("{}", msg);
        }
    }

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
