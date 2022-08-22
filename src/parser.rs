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
    Eat token
*/

fn eat_token(
    to_eat: &TokenType,
    error: &str,
    iterator: &mut Peekable<Iter<Token>>,
) -> Result<(), String> {
    if let Some(Token { token_type, .. }) = iterator.next() {
        if token_type == to_eat {
            Ok(())
        } else {
            Err(error.to_string())
        }
    } else {
        Err(error.to_string())
    }
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

    if prefix_stack.len() > 1 {
        return Err(parse_error("more than one binary expression"));
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
    ast = match parse_exp(&mut iterator) {
        Ok(exp) => Ok(exp),
        Err(err) => return Err(vec![err]),
    };

    // show parsed expression
    match ast {
        Ok(e) => {
            println!("{:?}", e);
            Ok(e)
        }
        Err(err) => Err(vec![err]),
    }
}

// parse expression
fn parse_exp(iterator: &mut Peekable<Iter<Token>>) -> Result<Exp, String> {
    if let Some(Token { token_type, .. }) = iterator.next() {
        match token_type {
            TokenType::Num(n) => parse_num(iterator, *n),
            TokenType::LeftParen => parse_binary(iterator, Some(TokenType::LeftParen)),
            TokenType::True | TokenType::False => parse_boolean(iterator, Some(token_type.clone())),
            TokenType::If => parse_ite(iterator),
            _ => return Err(parse_error("invalid expression at beginning")),
        }
    } else {
        return Err(parse_error("Nothing to parse"));
    }
}

// parse boolean
fn parse_boolean(
    iterator: &mut Peekable<Iter<Token>>,
    token_type: Option<TokenType>,
) -> Result<Exp, String> {
    match iterator.clone().peek() {
        Some(_) => parse_binary(iterator, token_type),
        None => Ok(Exp::Bool(matches!(token_type, Some(TokenType::True)))),
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

// parse if-then-else
fn parse_ite(iterator: &mut Peekable<Iter<Token>>) -> Result<Exp, String> {
    // boolean condition
    let condition = parse_boolean(iterator, None)?;

    // then
    eat_token(
        &TokenType::Then,
        "missing token 'then' in condition",
        iterator,
    )?;
    let then_block = parse_exp(iterator)?;

    // else
    eat_token(
        &TokenType::Else,
        "missing token 'else' in condition",
        iterator,
    )?;
    let else_block = parse_exp(iterator)?;

    // end
    eat_token(
        &TokenType::End,
        "missing token 'end' in end of condition",
        iterator,
    )?;

    // return ite
    Ok(Exp::Ite(
        Box::new(condition),
        Box::new(then_block),
        Box::new(else_block),
    ))
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

    #[test]
    fn ite() {
        let tokens = scan("if 1 < 2 then 1 else 2 end").unwrap();
        assert_eq!(
            parse(&tokens),
            Ok(d_ite(d_sma(Num(1), Num(2)), Num(1), Num(2)))
        )
    }
}
