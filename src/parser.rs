use core::slice::Iter;
use std::collections::HashSet;
use std::hash::Hash;
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
            Err(parse_error(error))
        }
    } else {
        Err(parse_error(error))
    }
}

fn eat_newline(iterator: &mut Peekable<Iter<Token>>) {
    loop {
        if let Some(Token { token_type, .. }) = iterator.peek() {
            match token_type {
                TokenType::NewLine => iterator.next(),
                _ => return,
            };
        } else {
            return;
        }
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
            TokenType::Num(_) | TokenType::Var(_) | TokenType::True | TokenType::False => {
                postfix.push(t)
            }
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
                let b = match prefix_stack.pop() {
                    Some(value) => value,
                    None => return Err(parse_error("non-valid binary expression")),
                };
                let a = match prefix_stack.pop() {
                    Some(value) => value,
                    None => return Err(parse_error("non-valid binary expression")),
                };
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
            // var
            TokenType::Var(var) => prefix_stack.push(Exp::Var(var)),
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

pub fn parse(tokens: &Vec<Token>, var_set: &mut HashSet<String>) -> Result<Exp, Vec<String>> {
    let mut iterator = tokens.iter().peekable();

    // recursive parsing
    let ast = match parse_exp(&mut iterator, var_set) {
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
fn parse_exp(
    iterator: &mut Peekable<Iter<Token>>,
    var_set: &mut HashSet<String>,
) -> Result<Exp, String> {
    loop {
        if let Some(Token { token_type, .. }) = iterator.next() {
            return match token_type {
                TokenType::NewLine => continue,
                TokenType::Num(n) => parse_num(iterator, *n),
                TokenType::Var(var) => parse_var(iterator, var, var_set),
                TokenType::Let => parse_let(iterator, None, var_set),
                TokenType::LeftParen => parse_binary(iterator, Some(TokenType::LeftParen)),
                TokenType::True | TokenType::False => {
                    parse_boolean(iterator, Some(token_type.clone()))
                }

                TokenType::If => parse_ite(iterator, var_set),
                _ => return Err(parse_error("invalid expression at beginning")),
            };
        } else {
            return Err(parse_error("Nothing to parse"));
        }
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
                // var
                | TokenType::Var(_)
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
fn parse_ite(
    iterator: &mut Peekable<Iter<Token>>,
    var_set: &mut HashSet<String>,
) -> Result<Exp, String> {
    eat_newline(iterator);

    // boolean condition
    let condition = parse_boolean(iterator, None)?;
    eat_newline(iterator);

    // then
    eat_token(
        &TokenType::Then,
        "missing token 'then' in condition",
        iterator,
    )?;
    let then_block = parse_exp(iterator, var_set)?;
    eat_newline(iterator);

    // else
    eat_token(
        &TokenType::Else,
        "missing token 'else' in condition",
        iterator,
    )?;
    let else_block = parse_exp(iterator, var_set)?;
    eat_newline(iterator);

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

// parse var
fn parse_var(
    iterator: &mut Peekable<Iter<Token>>,
    var: &str,
    var_set: &mut HashSet<String>,
) -> Result<Exp, String> {
    if let Some(peek) = iterator.clone().peek() {
        if (peek.token_type == TokenType::Assign) {
            return parse_let(iterator, Some(var), var_set);
        }
        if (is_binary(&peek.token_type)) {
            return parse_binary(iterator, Some(TokenType::Var(var.to_string())));
        }
    }

    if var_set.get(var).is_none() {
        return Err(parse_error(&format!("variable '{}' does not exist", var)));
    }

    Ok(Exp::Var(var.to_string()))
}

// parse let
fn parse_let(
    iterator: &mut Peekable<Iter<Token>>,
    var_name: Option<&str>,
    var_set: &mut HashSet<String>,
) -> Result<Exp, String> {
    // get variable name
    eat_newline(iterator);
    let mut var = "";
    match var_name {
        Some(name) => var = name,
        None => {
            if let Some(Token { token_type, .. }) = iterator.next() {
                match token_type {
                    TokenType::Var(var_name) => var = var_name,
                    _ => return Err(parse_error("invalid variable name in let assignment")),
                }
            } else {
                return Err(parse_error("missing variable name in let assignment"));
            }
        }
    }

    // assign token
    eat_newline(iterator);
    eat_token(
        &TokenType::Assign,
        "missing '=' in let assignment",
        iterator,
    )?;
    eat_newline(iterator);

    // value to assign to variable
    // stop at end of the line
    let val = parse_exp(iterator, var_set)?;

    // add to var set
    var_set.insert(var.to_string());

    // get body of let
    eat_token(
        &TokenType::NewLine,
        "missing new line between let assignement and body",
        iterator,
    )?;

    let body = match parse_exp(iterator, var_set) {
        Ok(res) => res,
        Err(err) => {
            return Err(parse_error(&format!(
                "missing body in let assignement for variable `{}`, body - {}",
                var, err
            )))
        }
    };

    Ok(Exp::Let(var.to_string(), Box::new(val), Box::new(body)))
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
        assert_eq!(parse(&tokens, &mut HashSet::new()), Ok(Num(10)))
    }

    #[test]
    fn sub() {
        let tokens = scan("10 - 5").unwrap();
        assert_eq!(
            parse(&tokens, &mut HashSet::new()),
            Ok(d_sub(Num(10), Num(5)))
        )
    }

    #[test]
    fn boolean() {
        let tokens = scan("true & (false | true)").unwrap();
        assert_eq!(
            parse(&tokens, &mut HashSet::new()),
            Ok(d_and(Bool(true), d_or(Bool(false), Bool(true))))
        )
    }

    #[test]
    fn numeric_bool() {
        let tokens = scan("(1<2) & (2==2) & (3>2)").unwrap();
        assert_eq!(
            parse(&tokens, &mut HashSet::new()),
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
            parse(&tokens, &mut HashSet::new()),
            Ok(d_div(d_add(d_mult(Num(2), Num(1)), Num(2)), Num(2)))
        )
    }

    #[test]
    fn ite() {
        let tokens = scan("\n \n if \n 1 < 2 \n then \n 1 \n else \n 2 \n \n end").unwrap();
        assert_eq!(
            parse(&tokens, &mut HashSet::new()),
            Ok(d_ite(d_sma(Num(1), Num(2)), Num(1), Num(2)))
        )
    }

    #[test]
    fn var() {
        let tokens = scan("let 'a' \n = \n 1 + 2 \n 'a' = 'a' + 2 \n 'a'").unwrap();
        assert_eq!(
            parse(&tokens, &mut HashSet::new()),
            Ok(d_let(
                "a",
                d_add(Num(1), Num(2)),
                d_let(
                    "a",
                    d_add(Var("a".to_string()), Num(2)),
                    Var("a".to_string())
                )
            ))
        )
    }
}
