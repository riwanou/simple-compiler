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
        TokenType::And | TokenType::Or | TokenType::Eq | TokenType::Sma | TokenType::Gta => 1,
        TokenType::Add | TokenType::Sub => 1,
        TokenType::Mult | TokenType::Div => 2,
        _ => 0,
    }
}

pub fn is_binary(token_type: &TokenType) -> bool {
    matches!(
        token_type,
        TokenType::Add
            | TokenType::Sub
            | TokenType::Mult
            | TokenType::Div
            | TokenType::And
            | TokenType::Or
            | TokenType::Eq
            | TokenType::Sma
            | TokenType::Gta
    )
}

pub fn convert_binary(tokens: Vec<TokenType>) -> Result<Exp, String> {
    let mut operator_stack = Vec::<TokenType>::new();
    let mut postfix = Vec::<TokenType>::new();
    let mut prefix_stack = Vec::<Exp>::new();

    // convert to postfix
    for t in tokens {
        match t {
            // basic
            TokenType::Num(_) | TokenType::True | TokenType::False => postfix.push(t),
            // num & num
            ref x if is_binary(x) => {
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
            // group
            TokenType::LeftParen => operator_stack.push(t.clone()),
            TokenType::RightParen => {
                while let Some(top) = operator_stack.pop() {
                    match top {
                        ref x if is_binary(x) => postfix.push(top),
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
            ref x if is_binary(x) => {
                let b = prefix_stack.pop().unwrap();
                let a = prefix_stack.pop().unwrap();
                match op {
                    // bool
                    TokenType::And => prefix_stack.push(Exp::And(Box::new(a), Box::new(b))),
                    TokenType::Or => prefix_stack.push(Exp::Or(Box::new(a), Box::new(b))),
                    TokenType::Eq => prefix_stack.push(Exp::Eq(Box::new(a), Box::new(b))),
                    TokenType::Sma => prefix_stack.push(Exp::Sma(Box::new(a), Box::new(b))),
                    TokenType::Gta => prefix_stack.push(Exp::Gta(Box::new(a), Box::new(b))),
                    // num
                    TokenType::Add => prefix_stack.push(Exp::Add(Box::new(a), Box::new(b))),
                    TokenType::Sub => prefix_stack.push(Exp::Sub(Box::new(a), Box::new(b))),
                    TokenType::Mult => prefix_stack.push(Exp::Mult(Box::new(a), Box::new(b))),
                    TokenType::Div => prefix_stack.push(Exp::Div(Box::new(a), Box::new(b))),
                    _ => (),
                }
            }
            // bool
            TokenType::True => prefix_stack.push(Exp::Bool(true)),
            TokenType::False => prefix_stack.push(Exp::Bool(false)),
            // num
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

    // recursive parsing
    if let Some(Token { token_type, .. }) = iterator.next() {
        ast = match token_type {
            TokenType::Num(n) => parse_num(&mut iterator, *n),
            TokenType::LeftParen => parse_binary(&mut iterator, Some(TokenType::LeftParen)),
            TokenType::True => parse_boolean(&mut iterator, true),
            TokenType::False => parse_boolean(&mut iterator, false),
            _ => return Err(vec![parse_error("invalid expression at beginning")]),
        }
    }

    // show parsed expression
    match ast {
        Ok(e) => {
            println!("{:?}", e);
            Ok(e)
        }
        Err(err) => Err(vec![err]),
    }
}

// parse boolean
fn parse_boolean(iterator: &mut Peekable<Iter<Token>>, current: bool) -> Result<Exp, String> {
    match iterator.clone().peek() {
        Some(_) => parse_binary(
            iterator,
            Some(if current {
                TokenType::True
            } else {
                TokenType::False
            }),
        ),
        None => Ok(Exp::Bool(current)),
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

    // is binary operation token?
    loop {
        match iterator.peek() {
            Some(peek) => match peek.token_type {
                // bool
                TokenType::True
                | TokenType::False
                | TokenType::And
                | TokenType::Or
                | TokenType::Eq
                | TokenType::Sma
                | TokenType::Gta
                // num
                | TokenType::Num(_)
                | TokenType::Add
                | TokenType::Sub
                | TokenType::Mult
                | TokenType::Div
                // group
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

    // check if the number of parentheses is matching
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
        let tokens = scan("true & (false | true)").unwrap();
        assert_eq!(
            parse(&tokens),
            Ok(d_and(Bool(true), d_or(Bool(false), Bool(true))))
        )
    }

    #[test]
    fn numeric_bool() {
        let tokens = scan("(1<2) & (2=2) & (3>2)").unwrap();
        assert_eq!(
            parse(&tokens),
            Ok(d_and(
                d_and(d_sma(Num(1), Num(2)), d_eq(Num(2), Num(2))),
                d_gta(Num(3), Num(2))
            ))
        )
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
