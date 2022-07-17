use std::borrow::Borrow;
use std::fmt;
use std::iter::Peekable;
use std::str::Chars;

use crate::syntax::Exp;

#[derive(Debug, Clone)]
enum TokenType {
    // Literals
    Num(i32),
    // Binary expressions
    Plus,
    Mult,
    // Parser
    Error(String),
}

struct Scanner<'a> {
    iter: Peekable<Chars<'a>>,
    _pos: usize,
    _next_pos: usize,
    _line: usize,
}

impl Scanner<'_> {
    fn new(content: &str) -> Scanner {
        Scanner {
            iter: content.chars().peekable(),
            _pos: 0,
            _next_pos: 0,
            _line: 1,
        }
    }

    fn next(&mut self) -> Option<char> {
        self._next_pos += 1;
        self._pos = self._next_pos - 1;
        self.iter.next()
    }

    fn peek(&mut self) -> Option<&char> {
        self.iter.peek()
    }
}

struct Token {
    _token_type: TokenType,
    _pos: usize,
    _line: usize,
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "({:?}, pos: {}, line: {})",
            self._token_type, self._pos, self._line
        )
    }
}

fn make_token(_token_type: TokenType, scanner: &Scanner) -> Token {
    Token {
        _token_type: _token_type,
        _pos: scanner._pos,
        _line: scanner._line,
    }
}

fn error_token(msg: &str, scanner: &Scanner) -> Token {
    Token {
        _token_type: TokenType::Error(msg.to_owned()),
        _pos: scanner._pos,
        _line: scanner._line,
    }
}

pub fn parse(content: &str) -> Result<Exp, Vec<String>> {
    // get tokens
    let mut tokens = Vec::<Token>::new();
    let mut scanner = Scanner::new(content);
    let mut contain_errors = false;

    loop {
        let char = scanner.next();
        // end of input
        if let None = char {
            break;
        }
        // still left
        if let Some(c) = char {
            // whitespace, new lines
            match c {
                ' ' | '\r' | '\t' => continue,
                '\n' => {
                    scanner._line += 1;
                    continue;
                }
                // comments
                '/' => {
                    if let Some(c_next) = scanner.peek() {
                        if *c_next == '/' {
                            loop {
                                match scanner.next() {
                                    Some(c_com) => match c_com {
                                        '\n' => {
                                            scanner._line += 1;
                                            break;
                                        }
                                        _ => (),
                                    },
                                    None => break,
                                };
                            }
                            continue;
                        }
                    }
                }
                _ => (),
            };

            // tokens
            tokens.push(match c {
                '0'..='9' => parse_number(c, &mut scanner),
                '+' => make_token(TokenType::Plus, &scanner),
                '*' => make_token(TokenType::Mult, &scanner),
                _ => {
                    contain_errors = true;
                    error_token(&format!("Unexpected character: {}", c), &scanner)
                }
            });
        }
    }

    // errors
    if contain_errors {
        let errors: Vec<_> = tokens
            .iter()
            .filter(|token| matches!(token._token_type, TokenType::Error(_)))
            .map(|token| {
                format!(
                    "({}:{}): {}",
                    token._line,
                    token._pos,
                    match token._token_type.borrow() {
                        TokenType::Error(e) => e,
                        _ => "",
                    },
                )
            })
            .collect();
        return Err(errors);
    }

    let ast: Exp;
    ast = parse_binary(tokens);
    println!("{:?}", ast);

    return Ok(ast);
}

// convert token numeric expression to prefix
// based on shunting yard algorithm
fn parse_to_prefix(tokens: Vec<TokenType>) -> Vec<TokenType> {
    let mut operator_stack = Vec::<TokenType>::new();
    let mut output = Vec::<TokenType>::new();

    for t in tokens {
        match t {
            TokenType::Num(_) => output.push(t),
            TokenType::Plus | TokenType::Mult => {
                loop {
                    if let Some(TokenType::Mult) = operator_stack.last() {
                        if let Some(TokenType::Plus | TokenType::Mult) = operator_stack.last() {
                            let last = operator_stack.pop().unwrap();
                            output.push(last);
                        }
                    }
                    break;
                }
                operator_stack.push(t);
            }
            _ => (),
        };
    }

    operator_stack.into_iter().for_each(|op| output.push(op));
    output
}

// convert prefix expression to ast
fn parse_prefix(prefix: Vec<TokenType>) -> Exp {
    let mut stack = Vec::<Exp>::new();
    for op in prefix {
        match op {
            TokenType::Plus | TokenType::Mult => {
                let b = stack.pop().unwrap();
                let a = stack.pop().unwrap();
                match op {
                    TokenType::Plus => stack.push(Exp::Add(Box::new(a), Box::new(b))),
                    TokenType::Mult => stack.push(Exp::Mult(Box::new(a), Box::new(b))),
                    _ => (),
                }
            }
            TokenType::Num(n) => stack.push(Exp::Num(n)),
            _ => (),
        }
    }
    stack.pop().unwrap()
}

// parse full binary expression
fn parse_binary(tokens: Vec<Token>) -> Exp {
    let mut it = tokens.iter().peekable();
    let mut numeric_tokens = Vec::<TokenType>::new();

    loop {
        if let Some(next) = it.peek().cloned() {
            match next._token_type {
                TokenType::Num(_) | TokenType::Mult | TokenType::Plus => {
                    it.next();
                    numeric_tokens.push(next._token_type.clone());
                }
                _ => break,
            }
        } else {
            break;
        }
    }

    let prefix = parse_to_prefix(numeric_tokens);
    parse_prefix(prefix)
}

fn parse_number(first: char, scanner: &mut Scanner) -> Token {
    let mut number = String::new();
    number.push(first);

    while let Some(c) = scanner.peek() {
        match c {
            '0'..='9' => number.push(scanner.next().unwrap()),
            _ => break,
        }
    }

    make_token(TokenType::Num(number.parse().unwrap()), scanner)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use Exp::*;

    #[test]
    fn num() {
        assert_eq!(parse("10"), Ok(Num(10)))
    }

    // #[test]
    // fn sya() {}
}
