use std::borrow::Borrow;
use std::iter::Peekable;
use std::str::Chars;

use crate::token::desugar::*;
use crate::token::Token;
use crate::token::TokenType;

/*
    Scanner
*/

pub struct Scanner<'a> {
    pub pos: usize,
    pub line: usize,
    iter: Peekable<Chars<'a>>,
    _next_pos: usize,
}

impl Scanner<'_> {
    fn new(content: &str) -> Scanner {
        Scanner {
            iter: content.chars().peekable(),
            pos: 0,
            line: 1,
            _next_pos: 0,
        }
    }

    fn new_line(&mut self) {
        self.line += 1;
    }

    fn next(&mut self) -> Option<char> {
        self._next_pos += 1;
        self.pos = self._next_pos - 1;
        self.iter.next()
    }

    fn peek(&mut self) -> Option<&char> {
        self.iter.peek()
    }
}

/*
    Errors
*/

// create list of errors
fn format_errors(tokens: &Vec<Token>) -> Vec<String> {
    tokens
        .iter()
        .filter(|token| matches!(token.token_type, TokenType::Error(_)))
        .map(|token| {
            format!(
                "({}:{}): {}",
                token.line,
                token.pos,
                match token.token_type.borrow() {
                    TokenType::Error(str) => str,
                    _ => "",
                }
            )
        })
        .collect()
}

/*
    Generate tokens
*/

pub fn scan(content: &str) -> Result<Vec<Token>, Vec<String>> {
    let mut scanner = Scanner::new(content);
    let mut tokens = Vec::<Token>::new();
    let mut errors = false;

    loop {
        match scanner.next() {
            None => break,
            Some(c) => match c {
                // comments, empty lines, spaces, etc..
                ' ' | '\r' | '\t' => (),
                '\n' => scanner.new_line(),
                '/' => scan_comment(&mut scanner),

                // actual program
                '0'..='9' => tokens.push(scan_number(&mut scanner, c)),
                '+' => tokens.push(make_token(TokenType::Add, &scanner)),
                '-' => tokens.push(make_token(TokenType::Sub, &scanner)),
                '*' => tokens.push(make_token(TokenType::Mult, &scanner)),

                // erors
                _ => {
                    let token = error_token(&format!("Unexpected character: {}", c), &scanner);
                    tokens.push(token);
                    errors = true;
                }
            },
        };
    }

    if errors {
        return Err(format_errors(&tokens));
    }

    Ok(tokens)
}

// skip comments
#[allow(dead_code)]
fn scan_comment(scanner: &mut Scanner) {
    if let Some('/') = scanner.peek() {
        while let Some(c) = scanner.next() {
            // exit line comment on line break
            if c == '\n' {
                return;
            }
        }
    }
}

// get whole number
#[allow(dead_code)]
fn scan_number(scanner: &mut Scanner, current: char) -> Token {
    let mut sequence = String::from(current);
    while let Some('0'..='9') = scanner.peek() {
        sequence.push(scanner.next().unwrap());
    }
    let number = sequence.parse().unwrap();
    make_token(TokenType::Num(number), scanner)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic() {
        println!(
            "{:?}",
            scan("230 - 10 \n\n\n // comments \n // comments \n 10 + 2 * 3 + 10")
        );
    }
}
