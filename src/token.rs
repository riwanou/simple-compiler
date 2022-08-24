use crate::scanner::Scanner;
use std::fmt;

/*
    /!\ - [program need 'main' entry point definition]
    program -> [def]

    (function definition)
    def -> "fun" var(p1, p2, ..) exp "end"

    /!\ - [value expression have to be inline]
    (expression)
    exp -> literal
            | var
            | binary
            | grouping
            | end
            | ite (if-then-else)
            | call (function)

    literal -> NUMBER | true | false
    binary -> exp operator exp
    operator -> "+" | "-" | "*" | "/" | "==" | "<" | ">" | "&" | "|"
    grouping -> "(" exp ")"

    var -> 'var'
    let -> let 'var' = exp \n exp
    ite -> boolean "then" exp "else" exp "end"

    (var is function name in this case)
    call -> 'var'(p1, p2, ..)

    comments -> "#"
*/

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Literals
    Num(i32),
    True,
    False,
    // Variables
    Var(String),
    Let,
    Assign,
    // Binary expressions
    // Boolean
    And,
    Or,
    Eq,
    Sma,
    Gta,
    // Number
    Add,
    Sub,
    Mult,
    Div,
    // Grouping
    LeftParen,
    RightParen,
    // Function
    Fun,
    Call(String),
    Comma,
    // End
    End,
    NewLine,
    // If-then-else
    If,
    Then,
    Else,
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
