use core::slice::Iter;
use std::iter::Peekable;
use std::vec;

use crate::syntax::Exp;
use crate::token::Token;
use crate::token::TokenType;

/*
    Prefix suffix prefix conversion
    use shunting yard algorithm to convert suffix to prefix
    then convert back to prefix
*/

pub fn convert_binary(tokens: Vec<TokenType>) -> Option<Exp> {
    let mut operator_stack = Vec::<TokenType>::new();
    let mut suffix = Vec::<TokenType>::new();
    let mut prefix_stack = Vec::<Exp>::new();

    // convert to suffix
    for t in tokens {
        match t {
            TokenType::Num(_) => suffix.push(t),
            TokenType::Add | TokenType::Sub => {
                loop {
                    if let Some(last) = operator_stack.last() {
                        // if there is some operators on the stack
                        // then we add them to the output
                        let last = operator_stack.pop().unwrap();
                        suffix.push(last);
                    } else {
                        break;
                    }
                }
                operator_stack.push(t.clone());
            }
            TokenType::Mult => {
                loop {
                    // high priority operations
                    if matches!(operator_stack.last(), Some(TokenType::Mult)) {
                        // if there is some operators on the stack
                        // then we add them to the output
                        if operator_stack.last().is_some() {
                            let last = operator_stack.pop().unwrap();
                            suffix.push(last);
                        }
                    }
                    break;
                }
                operator_stack.push(t.clone());
            }
            _ => break,
        }
    }

    // add the remaining operator in stack to the outputs
    operator_stack.into_iter().for_each(|op| suffix.push(op));
    println!("{:?}", suffix);

    // convert to prefix
    for op in suffix {
        match op {
            TokenType::Add | TokenType::Sub | TokenType::Mult => {
                let b = prefix_stack.pop().unwrap();
                let a = prefix_stack.pop().unwrap();
                match op {
                    TokenType::Add => prefix_stack.push(Exp::Add(Box::new(a), Box::new(b))),
                    TokenType::Sub => prefix_stack.push(Exp::Sub(Box::new(a), Box::new(b))),
                    TokenType::Mult => prefix_stack.push(Exp::Mult(Box::new(a), Box::new(b))),
                    _ => (),
                }
            }
            TokenType::Num(n) => prefix_stack.push(Exp::Num(n)),
            _ => (),
        }
    }

    prefix_stack.pop()
}

/*
    Parser
*/

pub fn parse(tokens: &Vec<Token>) -> Option<Exp> {
    let ast: Option<Exp>;
    let mut iterator = tokens.iter().peekable();

    ast = match iterator.next()?.token_type {
        TokenType::Num(n) => parse_num(&mut iterator, n),
        _ => None,
    };

    println!("{:?}", ast);
    ast
}

// parse num
fn parse_num(iterator: &mut Peekable<Iter<Token>>, current: i32) -> Option<Exp> {
    match iterator.clone().peek() {
        Some(_) => parse_binary(iterator, TokenType::Num(current)),
        None => Some(Exp::Num(current)),
    }
}

// parse binary
fn parse_binary(iterator: &mut Peekable<Iter<Token>>, left: TokenType) -> Option<Exp> {
    let mut exp_tokens = vec![left];
    loop {
        match iterator.peek() {
            Some(peek) => match peek.token_type {
                TokenType::Num(_) | TokenType::Mult | TokenType::Add | TokenType::Sub => {
                    let next = iterator.next()?;
                    exp_tokens.push(next.token_type.clone())
                }
                _ => break,
            },
            None => break,
        }
    }
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
        assert_eq!(parse(&tokens), Some(Num(10)))
    }

    #[test]
    fn sub() {
        let tokens = scan("10 - 5").unwrap();
        assert_eq!(parse(&tokens), Some(d_sub(Num(10), Num(5))))
    }

    #[test]
    fn numerics_op() {
        let tokens = scan("10 + 20 * 2 + 1").unwrap();
        assert_eq!(
            parse(&tokens),
            Some(d_add(Num(10), d_add(d_mult(Num(20), Num(2)), Num(1))))
        )
    }
}
