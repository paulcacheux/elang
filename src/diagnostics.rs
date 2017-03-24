use lexer::{LexicalError, Token};
use lalrpop_util::ParseError;

pub fn print_diagnostic(input: &str, error: ParseError<usize, Token, LexicalError>) {
    let line_starts = build_line_starts(input);

    let mut error_lines = Vec::new();

    match error {
        ParseError::InvalidToken { location } => {
            error_lines.extend(get_lines(input, &line_starts, vec![location]));
            println!("Invalid token.");
        },
        ParseError::UnrecognizedToken { token, expected } => {
            if let Some((start, tok, end)) = token {
                error_lines.extend(get_lines(input, &line_starts, vec![start, end]));
                println!("Unrecognized token: {:?}.", tok);
            } else {
                println!("Unexpected eof.");
            }
            println!("Expected one of: {}", expected.join(", "));
        },
        ParseError::ExtraToken { token: (start, tok, end) } => {
            error_lines.extend(get_lines(input, &line_starts, vec![start, end]));
            println!("Extra token: {:?}.", tok);
        },
        ParseError::User { error: LexicalError { msg, pos } } => {
            error_lines.extend(get_lines(input, &line_starts, vec![pos]));
            println!("{}", msg);
        }
    }

    for (i, line) in error_lines {
        println!("{}: {}", i, line);
    }
}

fn build_line_starts(input: &str) -> Vec<usize> {
    let mut starts = vec![0];
    for (i, c) in input.char_indices() {
        if c == '\n' {
            starts.push(i);
        }
    }
    starts
}

fn get_lines<'a>(input: &'a str, starts: &Vec<usize>, positions: Vec<usize>) -> Vec<(usize, &'a str)> {
    let mut lines = Vec::new();

    for pos in positions {
        let real_start = &0;
        let (line, index) = starts.iter().take_while(|&&i| i < pos).enumerate().last().unwrap_or((0, real_start));
        let line_str = &input[*index+1..starts[line+1]];
        lines.push((line, line_str));
    }

    lines.dedup_by_key(|item| item.0);
    lines
}
