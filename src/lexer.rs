use std::str::CharIndices;
use std::iter::Peekable;
use std::str::FromStr;

use itertools::Itertools;
use unicode_xid::UnicodeXID;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    FnKeyword,
    LetKeyword,
    WhileKeyword,
    LoopKeyword,
    IfKeyword,
    ElseKeyword,
    ReturnKeyword,
    BreakKeyword,
    ContinueKeyword,
    PrintKeyword,
    OpenParen,
    CloseParen,
    OpenSquare,
    CloseSquare,
    OpenBracket,
    CloseBracket,
    Arrow,
    Comma,
    Dot,
    SemiColon,
    Colon,
    Plus,
    Minus,
    Star,
    Slash,
    Modulo,
    Bang,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    EqualEqual,
    Equal,
    BangEqual,
    Amp,
    AmpAmp,
    PipePipe,
    IntLit(i64),
    DoubleLit(f64),
    BoolLit(bool),
    Identifier(String)
}

#[derive(Debug, Clone)]
pub struct LexicalError {
    pub msg: String,
    pub pos: usize,
}

#[derive(Debug, Clone)]
pub struct Lexer<'input> {
    chars: Peekable<CharIndices<'input>>,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Lexer { chars: input.char_indices().peekable() }
    }

    fn if_next(&mut self, c: char, true_tok: Token, false_tok: Token) -> Result<Token, Token> {
        if let Some(&(_, p)) = self.chars.peek() {
            if p == c {
                self.chars.next();
                return Ok(true_tok);
            }
        }
        return Err(false_tok);
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<(usize, Token, usize), LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(&(_, c)) = self.chars.peek() {
            if !c.is_whitespace() {
                break
            }
            self.chars.next();
        }

        return match self.chars.next() {
            Some((i, '{')) => Some(Ok((i, Token::OpenBracket, i+1))),
            Some((i, '}')) => Some(Ok((i, Token::CloseBracket, i+1))),
            Some((i, '(')) => Some(Ok((i, Token::OpenParen, i+1))),
            Some((i, ')')) => Some(Ok((i, Token::CloseParen, i+1))),
            Some((i, '[')) => Some(Ok((i, Token::OpenSquare, i+1))),
            Some((i, ']')) => Some(Ok((i, Token::CloseSquare, i+1))),

            Some((i, '.')) => Some(Ok((i, Token::Dot, i+1))),
            Some((i, ',')) => Some(Ok((i, Token::Comma, i+1))),
            Some((i, ';')) => Some(Ok((i, Token::SemiColon, i+1))),
            Some((i, ':')) => Some(Ok((i, Token::Colon, i+1))),

            Some((i, '+')) => Some(Ok((i, Token::Plus, i+1))),
            Some((i, '*')) => Some(Ok((i, Token::Star, i+1))),
            Some((i, '/')) => Some(Ok((i, Token::Slash, i+1))),
            Some((i, '%')) => Some(Ok((i, Token::Modulo, i+1))),

            Some((i, '-')) => {
                Some(Ok(match self.if_next('>', Token::Arrow, Token::Minus) {
                    Ok(tok) => (i, tok, i+2),
                    Err(tok) => (i, tok, i+1),
                }))
            },
            Some((i, '<')) => {
                Some(Ok(match self.if_next('=', Token::LessEqual, Token::Less) {
                    Ok(tok) => (i, tok, i+2),
                    Err(tok) => (i, tok, i+1),
                }))
            },
            Some((i, '>')) => {
                Some(Ok(match self.if_next('=', Token::GreaterEqual, Token::Greater) {
                    Ok(tok) => (i, tok, i+2),
                    Err(tok) => (i, tok, i+1),
                }))
            },
            Some((i, '=')) => {
                Some(Ok(match self.if_next('=', Token::EqualEqual, Token::Equal) {
                    Ok(tok) => (i, tok, i+2),
                    Err(tok) => (i, tok, i+1),
                }))
            },
            Some((i, '!')) => {
                Some(Ok(match self.if_next('=', Token::BangEqual, Token::Bang) {
                    Ok(tok) => (i, tok, i+2),
                    Err(tok) => (i, tok, i+1),
                }))
            },
            Some((i, '&')) => {
                Some(Ok(match self.if_next('&', Token::AmpAmp, Token::Amp) {
                    Ok(tok) => (i, tok, i+2),
                    Err(tok) => (i, tok, i+1),
                }))
            },
            Some((i, '|')) => {
                if let Some(&(_, '|')) = self.chars.peek() {
                    self.chars.next();
                    Some(Ok((i, Token::PipePipe, i+2)))
                } else {
                    Some(Err(LexicalError {
                        msg: String::from("Unexpected '|', expected '||'."),
                        pos: i
                    }))
                }
            },
            Some((i, c)) if c.is_digit(10) => {
                let mut lit = c.to_string();
                lit.extend(self.chars.peeking_take_while(|c| c.1.is_digit(10)).map(|i| i.1));

                if let Some(&(_, '.')) = self.chars.peek() {
                    lit.push('.');
                    self.chars.next();
                    lit.extend(self.chars.peeking_take_while(|c| c.1.is_digit(10)).map(|i| i.1));

                    let len = lit.len();
                    Some(Ok((i, Token::DoubleLit(f64::from_str(&lit).unwrap()), i + len)))
                } else {
                    let len = lit.len();
                    Some(Ok((i, Token::IntLit(i64::from_str(&lit).unwrap()), i + len)))
                }

            },
            Some((i, c)) if UnicodeXID::is_xid_start(c) => {
                let mut id = c.to_string();
                id.extend(self.chars.peeking_take_while(|c| UnicodeXID::is_xid_continue(c.1)).map(|i| i.1));
                let len = id.len();
                Some(Ok((i, utils::identifier_or_keyword(id), i + len)))
            },
            Some((i, c)) => {
                Some(Err(LexicalError {
                    msg: format!("Unexpected '{}'.", c),
                    pos: i,
                }))
            },
            None => None
        }
    }
}

mod utils {
    use super::Token;
    pub fn identifier_or_keyword(s: String) -> Token {
        match s.as_str() {
            "fn" => Token::FnKeyword,
            "let" => Token::LetKeyword,
            "while" => Token::WhileKeyword,
            "loop" => Token::LoopKeyword,
            "if" => Token::IfKeyword,
            "else" => Token::ElseKeyword,
            "return" => Token::ReturnKeyword,
            "break" => Token::BreakKeyword,
            "continue" => Token::ContinueKeyword,
            "print" => Token::PrintKeyword,
            "true" => Token::BoolLit(true),
            "false" => Token::BoolLit(false),
            _ => Token::Identifier(s),
        }
    }
}
