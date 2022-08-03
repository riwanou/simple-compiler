use crate::scanner::Scanner;
use std::fmt;

/*
    expression -> literal
                | binary
                | grouping;

    literal -> NUMBER | true | false;
    grouping -> "(" expression ")";
    binary -> expression operator expression
    operator -> "+" | "-" | "*" | "+"

    comments -> "#"
*/

#[derive(Debug, Clone)]
pub enum TokenType {
    // Literals
    Num(i32),
    True,
    False,
    // Binary expressions
    Add,
    Sub,
    Mult,
    Div,
    // Grouping
    LeftParen,
    RightParen,
    // Parser
    Error(String),
}

pub struct Token {
    pub token_type: TokenType,
    pub pos: usize,
    pub line: usize,
}

pub mod desugar {
    use super::*;

    // create token with information
    pub fn make_token(token_type: TokenType, scanner: &Scanner) -> Token {
        Token {
            token_type: token_type,
            pos: scanner.pos,
            line: scanner.line,
        }
    }

    // create error token container error message
    pub fn error_token(msg: &str, scanner: &Scanner) -> Token {
        Token {
            token_type: TokenType::Error(msg.to_owned()),
            pos: scanner.pos,
            line: scanner.line,
        }
    }
}

// pretty print token
impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "({:?}, pos: {}, line: {})",
            self.token_type, self.pos, self.line
        )
    }
}
