use crate::token::Token;
use crate::token::TokenType;

use crate::syntax::Exp;

pub fn parse(tokens: &Vec<Token>) -> Exp {
    let ast: Exp;
    ast = parse_binary(tokens);
    println!("{:?}", ast);
    ast
}

// convert token numeric expression to prefix
// based on shunting yard algorithm
fn parse_to_prefix(tokens: Vec<TokenType>) -> Vec<TokenType> {
    let mut operator_stack = Vec::<TokenType>::new();
    let mut output = Vec::<TokenType>::new();

    for t in tokens {
        match t {
            TokenType::Num(_) => output.push(t),
            TokenType::Add | TokenType::Mult => {
                loop {
                    if let Some(TokenType::Mult) = operator_stack.last() {
                        if let Some(TokenType::Add | TokenType::Mult) = operator_stack.last() {
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
            TokenType::Add | TokenType::Mult => {
                let b = stack.pop().unwrap();
                let a = stack.pop().unwrap();
                match op {
                    TokenType::Add => stack.push(Exp::Add(Box::new(a), Box::new(b))),
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
fn parse_binary(tokens: &Vec<Token>) -> Exp {
    let mut it = tokens.iter().peekable();
    let mut numeric_tokens = Vec::<TokenType>::new();

    loop {
        if let Some(next) = it.peek().cloned() {
            match next.token_type {
                TokenType::Num(_) | TokenType::Mult | TokenType::Add => {
                    it.next();
                    numeric_tokens.push(next.token_type.clone());
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
        assert_eq!(parse(&tokens), Num(10))
    }

    #[test]
    fn numerics_op() {
        let tokens = scan("10 + 20 * 2 + 1").unwrap();
        assert_eq!(
            parse(&tokens),
            d_add(Num(10), d_add(d_mult(Num(20), Num(2)), Num(1)))
        )
    }
}
