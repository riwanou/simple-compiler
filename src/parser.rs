use core::slice::Iter;
use std::iter::Peekable;
use std::vec;

use crate::syntax::Exp;
use crate::token::Token;
use crate::token::TokenType;

/*
    Errors
*/

fn parse_error(errors: &str) -> String {
    format!("Parsing error: {}", errors)
}

/*
    Prefix postfix prefix conversion
    use shunting yard algorithm to convert prefix to postfix
    then convert back to prefix
*/

// prescedence priority
pub fn prescedence_op(op: &TokenType) -> u32 {
    match op {
        TokenType::Add | TokenType::Sub => 1,
        TokenType::Mult | TokenType::Div => 2,
        _ => 0,
    }
}

pub fn convert_binary(tokens: Vec<TokenType>) -> Result<Exp, String> {
    let mut operator_stack = Vec::<TokenType>::new();
    let mut postfix = Vec::<TokenType>::new();
    let mut prefix_stack = Vec::<Exp>::new();

    // convert to postfix
    for t in tokens {
        match t {
            TokenType::Num(_) => postfix.push(t),
            TokenType::Add | TokenType::Sub | TokenType::Mult | TokenType::Div => {
                while let Some(top) = operator_stack.last() {
                    if prescedence_op(&t) <= prescedence_op(top) {
                        let last = operator_stack.pop().unwrap();
                        postfix.push(last);
                    } else {
                        break;
                    }
                }
                operator_stack.push(t.clone());
            }
            TokenType::LeftParen => operator_stack.push(t.clone()),
            TokenType::RightParen => {
                while let Some(top) = operator_stack.pop() {
                    match top {
                        TokenType::Add | TokenType::Sub | TokenType::Mult | TokenType::Div => {
                            postfix.push(top)
                        }
                        _ => break,
                    }
                }
            }
            _ => break,
        }
    }

    // add the remaining operator in stack to the outputs
    while let Some(op) = operator_stack.pop() {
        postfix.push(op);
    }

    // convert to prefix
    for op in postfix {
        match op {
            TokenType::Add | TokenType::Sub | TokenType::Mult | TokenType::Div => {
                let b = prefix_stack.pop().unwrap();
                let a = prefix_stack.pop().unwrap();
                match op {
                    TokenType::Add => prefix_stack.push(Exp::Add(Box::new(a), Box::new(b))),
                    TokenType::Sub => prefix_stack.push(Exp::Sub(Box::new(a), Box::new(b))),
                    TokenType::Mult => prefix_stack.push(Exp::Mult(Box::new(a), Box::new(b))),
                    TokenType::Div => prefix_stack.push(Exp::Div(Box::new(a), Box::new(b))),
                    _ => (),
                }
            }
            TokenType::Num(n) => prefix_stack.push(Exp::Num(n)),
            _ => (),
        }
    }

    match prefix_stack.pop() {
        Some(exp) => Ok(exp),
        None => Err(parse_error("binary expression")),
    }
}

/*
    Parser
*/

pub fn parse(tokens: &Vec<Token>) -> Result<Exp, Vec<String>> {
    let mut ast: Result<Exp, String> = Err(parse_error("program empty"));
    let mut iterator = tokens.iter().peekable();

    if let Some(Token { token_type, .. }) = iterator.next() {
        ast = match token_type {
            TokenType::Num(n) => parse_num(&mut iterator, *n),
            TokenType::LeftParen => parse_binary(&mut iterator, Some(TokenType::LeftParen)),
            TokenType::True => Ok(Exp::Bool(true)),
            TokenType::False => Ok(Exp::Bool(false)),
            _ => return Err(vec![parse_error("invalid expression at beginning")]),
        }
    }

    match ast {
        Ok(e) => {
            println!("{:?}", e);
            Ok(e)
        }
        Err(err) => Err(vec![err]),
    }
}

// parse num
fn parse_num(iterator: &mut Peekable<Iter<Token>>, current: i32) -> Result<Exp, String> {
    match iterator.clone().peek() {
        Some(_) => parse_binary(iterator, Some(TokenType::Num(current))),
        None => Ok(Exp::Num(current)),
    }
}

// parse binary
fn parse_binary(
    iterator: &mut Peekable<Iter<Token>>,
    left: Option<TokenType>,
) -> Result<Exp, String> {
    let mut exp_tokens = Vec::<TokenType>::new();

    if let Some(token_type) = left {
        exp_tokens.push(token_type);
    }

    loop {
        match iterator.peek() {
            Some(peek) => match peek.token_type {
                TokenType::Num(_)
                | TokenType::Add
                | TokenType::Sub
                | TokenType::Mult
                | TokenType::Div
                | TokenType::LeftParen
                | TokenType::RightParen => {
                    if let Some(next) = iterator.next() {
                        exp_tokens.push(next.token_type.clone())
                    }
                }
                _ => break,
            },
            None => break,
        }
    }

    // check if the number of parenthese is matching
    let mut left_paren = 0;
    let mut right_paren = 0;

    exp_tokens.iter().for_each(|token_type| match token_type {
        TokenType::LeftParen => left_paren += 1,
        TokenType::RightParen => right_paren += 1,
        _ => (),
    });

    if left_paren - right_paren != 0 {
        return Err(parse_error("number of parenthesis not matching"));
    }

    // convert to expression
    convert_binary(exp_tokens)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scanner::scan;
    use crate::syntax::desugars::*;
    use pretty_assertions::assert_eq;
    use Exp::*;

    #[test]
    fn num() {
        let tokens = scan("10").unwrap();
        assert_eq!(parse(&tokens), Ok(Num(10)))
    }

    #[test]
    fn sub() {
        let tokens = scan("10 - 5").unwrap();
        assert_eq!(parse(&tokens), Ok(d_sub(Num(10), Num(5))))
    }

    #[test]
    fn boolean() {
        let tokens = scan("true").unwrap();
        assert_eq!(parse(&tokens), Ok(Bool(true)))
    }

    #[test]
    fn numerics_op() {
        let tokens = scan("(2 * 1 + 2) / 2").unwrap();
        assert_eq!(
            parse(&tokens),
            Ok(d_div(d_add(d_mult(Num(2), Num(1)), Num(2)), Num(2)))
        )
    }
}
