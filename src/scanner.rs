use std::borrow::Borrow;
use std::error;
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

    fn eat(&mut self, text: &str, correct_token: &str) -> Result<(), Token> {
        for c in text.chars() {
            match self.next() {
                Some(current) if current == c => (),
                _ => {
                    return Err(error_token(
                        &format!(
                            "Unexpected character: {}, did you mean {}?",
                            c, correct_token
                        ),
                        self,
                    ))
                }
            }
        }
        Ok(())
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
        let next = scanner.next();
        let peek = scanner.peek();
        match next {
            None => break,
            Some(c) => match c {
                // comments, empty lines, spaces, etc..
                ' ' | '\r' | '\t' => (),
                '\n' => {
                    scanner.new_line();
                    tokens.push(make_token(TokenType::NewLine, &scanner))
                }
                '#' => scan_comment(&mut scanner),

                // actual program
                // literal
                '0'..='9' => tokens.push(scan_number(&mut scanner, c)),
                'f' => tokens.push(eat_token(TokenType::False, &mut scanner, "alse", "false")),
                // var
                '\'' => tokens.push(scan_var(&mut scanner)),
                // binary op
                // booolean
                '&' => tokens.push(make_token(TokenType::And, &scanner)),
                '|' => tokens.push(make_token(TokenType::Or, &scanner)),
                '<' => tokens.push(make_token(TokenType::Sma, &scanner)),
                '>' => tokens.push(make_token(TokenType::Gta, &scanner)),
                // numbers
                '+' => tokens.push(make_token(TokenType::Add, &scanner)),
                '-' => tokens.push(make_token(TokenType::Sub, &scanner)),
                '*' => tokens.push(make_token(TokenType::Mult, &scanner)),
                '/' => tokens.push(make_token(TokenType::Div, &scanner)),
                '(' => tokens.push(make_token(TokenType::LeftParen, &scanner)),
                ')' => tokens.push(make_token(TokenType::RightParen, &scanner)),
                // if-then-else
                'i' => tokens.push(eat_token(TokenType::If, &mut scanner, "f", "if")),
                // let
                'l' => tokens.push(eat_token(TokenType::Let, &mut scanner, "et", "let")),

                // composable
                '=' => match peek {
                    Some('=') => tokens.push(eat_token(TokenType::Eq, &mut scanner, "=", "==")),
                    _ => tokens.push(make_token(TokenType::Assign, &scanner)),
                },

                'e' => match peek {
                    Some('l') => {
                        tokens.push(eat_token(TokenType::Else, &mut scanner, "lse", "else"))
                    }
                    Some('n') => tokens.push(eat_token(TokenType::End, &mut scanner, "nd", "end")),
                    _ => break,
                },

                't' => match peek {
                    Some('r') => {
                        tokens.push(eat_token(TokenType::True, &mut scanner, "rue", "true"))
                    }
                    Some('h') => {
                        tokens.push(eat_token(TokenType::Then, &mut scanner, "hen", "then"))
                    }
                    _ => break,
                },

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
    while let Some(c) = scanner.next() {
        // exit line comment on line break
        if c == '\n' {
            return;
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

// get variable name
#[allow(dead_code)]
fn scan_var(scanner: &mut Scanner) -> Token {
    let mut sequence = String::new();
    while let Some(p) = scanner.peek() {
        match p {
            'a'..='z' | 'A'..='Z' | '0'..='9' => sequence.push(scanner.next().unwrap()),
            '\'' => {
                scanner.next();
                break;
            }
            _ => return error_token("invalid character in variable name", scanner),
        }
    }
    make_token(TokenType::Var(sequence), scanner)
}

// eat syntax and make token from it
fn eat_token(
    token_type: TokenType,
    scanner: &mut Scanner,
    eated: &str,
    correct_token: &str,
) -> Token {
    match scanner.eat(eated, correct_token) {
        Ok(_) => make_token(token_type, scanner),
        Err(token) => token,
    }
}

// test with -- --nocapture
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic() {
        println!("{:?}", scan("# comments \n 10"));
    }

    #[test]
    fn boolean() {
        println!("{:?}", scan("# comments \n true \n false | true | false"));
    }

    #[test]
    fn numeric_bool() {
        println!("{:?}", scan("2 == 2"));
    }

    #[test]
    fn ite() {
        println!("{:?}", scan("if true then 1 else 2 end"));
    }

    #[test]
    fn var() {
        println!(
            "{:?}",
            scan("'fooO12' + 'fee' \n let 'a' = 10 + 2 \n 'a' = 2")
        );
    }
}
