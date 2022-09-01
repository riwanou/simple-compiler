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
        let next = scanner.next();
        let peek = scanner.peek();
        match next {
            None => break,
            Some(c) => match c {
                // comments, empty lines, spaces, etc..
                ' ' | '\r' | '\t' => (),
                '#' => scan_comment(&mut scanner),
                '\n' => {
                    scanner.new_line();
                    tokens.push(make_token(TokenType::NewLine, &scanner))
                }

                // actual program
                // literal
                '0'..='9' => tokens.push(scan_number(&mut scanner, c)),
                // comma
                ',' => tokens.push(make_token(TokenType::Comma, &scanner)),
                // binary op
                // for booolean
                '&' => tokens.push(make_token(TokenType::And, &scanner)),
                '|' => tokens.push(make_token(TokenType::Or, &scanner)),
                '<' => tokens.push(make_token(TokenType::Sma, &scanner)),
                '>' => tokens.push(make_token(TokenType::Gta, &scanner)),
                // for number
                '+' => tokens.push(make_token(TokenType::Add, &scanner)),
                '-' => tokens.push(make_token(TokenType::Sub, &scanner)),
                '*' => tokens.push(make_token(TokenType::Mult, &scanner)),
                '/' => tokens.push(make_token(TokenType::Div, &scanner)),
                '(' => tokens.push(make_token(TokenType::LeftParen, &scanner)),
                ')' => tokens.push(make_token(TokenType::RightParen, &scanner)),

                // assign, equal
                '=' => match peek {
                    Some('=') => {
                        scanner.next();
                        tokens.push(make_token(TokenType::Eq, &scanner))
                    }
                    _ => tokens.push(make_token(TokenType::Assign, &scanner)),
                },

                // identifier
                'a'..='z' | 'A'..='Z' | '_' => tokens.push(scan_identifier(&mut scanner, c)),

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

// check if given identifier is a keyword
// if not return none
fn check_keyword(
    token_type: TokenType,
    scanner: &mut Scanner,
    identifier: &str,
    to_check: &str,
) -> Option<Token> {
    if identifier == to_check {
        return Some(make_token(token_type, scanner));
    }
    None
}

// scan identifier
#[allow(dead_code)]
fn scan_identifier(scanner: &mut Scanner, current: char) -> Token {
    let mut identifier = String::from(current);

    // scan the whole identifier
    while let Some('a'..='z' | 'A'..='Z' | '_' | '0'..='9') = scanner.peek() {
        match scanner.next() {
            Some(c) => identifier.push(c),
            None => break,
        }
    }

    // check if identifier is a keyword
    // scan like a tries
    let mut iter = identifier.chars().into_iter();

    let keyword = match iter.next() {
        // one step
        Some('l') => check_keyword(TokenType::Let, scanner, &identifier, "let"),
        Some('b') => check_keyword(TokenType::Bool, scanner, &identifier, "bool"),
        Some('f') => check_keyword(TokenType::False, scanner, &identifier, "false"),
        // two step
        Some('i') => match iter.next() {
            Some('f') => check_keyword(TokenType::If, scanner, &identifier, "if"),
            Some('n') => check_keyword(TokenType::Int, scanner, &identifier, "int"),
            _ => None,
        },
        Some('e') => match iter.next() {
            Some('l') => check_keyword(TokenType::Else, scanner, &identifier, "else"),
            Some('n') => check_keyword(TokenType::End, scanner, &identifier, "end"),
            _ => None,
        },
        Some('t') => match iter.next() {
            Some('r') => check_keyword(TokenType::True, scanner, &identifier, "true"),
            Some('h') => check_keyword(TokenType::Then, scanner, &identifier, "then"),
            _ => None,
        },
        _ => None,
    };

    match keyword {
        Some(token) => token,
        None => make_token(TokenType::Var(identifier), scanner),
    }
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
    fn identifier() {
        println!("{:?}", scan("fun if then let foo fee12 myVar _main_var"));
    }

    #[test]
    fn var() {
        println!("{:?}", scan("fooO12 + fee \n let a = 10 + 2 \n a = 2"));
    }

    #[test]
    fn fun() {
        println!("{:?}", scan("fun main(a, b) \n 10 \n end"))
    }

    #[test]
    fn call_fun() {
        println!("{:?}", scan("fun(1, 2)"))
    }

    #[test]
    fn types() {
        println!("{:?}", scan("int bla, bool bloop"))
    }
}
