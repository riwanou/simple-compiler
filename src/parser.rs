use core::slice::Iter;
use std::collections::VecDeque;
use std::iter::Peekable;
use std::vec;

use crate::syntax::Def;
use crate::syntax::Exp;
use crate::syntax::Type;
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
    Parser
*/

pub fn parse(tokens: &Vec<Token>) -> Result<Vec<Def>, Vec<String>> {
    let mut iterator = tokens.iter().peekable();
    let mut defs = VecDeque::new();
    let mut errors = vec![];
    let mut contain_main = false;

    // recursive parsing
    // put main function as first element
    loop {
        match parse_def(&mut iterator) {
            Ok(res) => match res {
                Some(def) => {
                    // search for entry point "main"
                    match def.clone() {
                        Def::Fun(name, param, _) if name == "main" => {
                            // main function should not have parameters
                            if param.len() > 0 {
                                errors
                                    .push(parse_error("main function should not have parameters"));
                            }
                            contain_main = true;
                            defs.push_front(def);
                        }
                        _ => defs.push_back(def),
                    };
                    continue;
                }
                None => break,
            },
            Err(err) => errors.push(err),
        }
        break;
    }

    // no entry point..
    if !contain_main {
        errors.push(parse_error("progam need a 'main' entry function"));
    }

    // show parsed list of definition
    if defs.len() > 0 && errors.len() == 0 {
        Ok(defs.into())
    } else {
        Err(errors)
    }
}

// parse type
fn parse_type(iterator: &mut Peekable<Iter<Token>>) -> Option<Type> {
    if let Some(Token { token_type, .. }) = iterator.next() {
        match token_type {
            TokenType::Int => Some(Type::Int),
            TokenType::Bool => Some(Type::Bool),
            _ => None,
        }
    } else {
        None
    }
}

// parse parameters between parentheses
fn parse_param(
    iterator: &mut Peekable<Iter<Token>>,
    fun_name: &str,
) -> Result<Vec<(Type, String)>, String> {
    let mut params = vec![];
    loop {
        if let Some(Token { token_type, .. }) = iterator.peek() {
            match token_type {
                TokenType::Int | TokenType::Bool => {
                    // get type of param
                    let param_type;
                    let param_name;
                    // type must be bind with param name
                    match parse_type(iterator) {
                        Some(var_type) => {
                            param_type = var_type;
                            eat_newline(iterator);
                        }
                        None => {
                            return Err(parse_error(&format!(
                                "error while parsing parameter type: '{}'",
                                fun_name
                            )))
                        }
                    };
                    // check if var after type
                    if let Some(Token { token_type, .. }) = iterator.peek() {
                        match token_type {
                            TokenType::Var(var_name) => {
                                param_name = var_name;
                                iterator.next();
                                eat_newline(iterator);
                            }
                            _ => {
                                return Err(parse_error(&format!(
                                    "function declaration expect parameter name after type: '{}'",
                                    fun_name
                                )))
                            }
                        }
                    } else {
                        break;
                    }
                    // add parameter to list
                    params.push((param_type, param_name.to_string()));
                    // there should be a comma afterward if more parameters
                    // else break
                    if let Some(Token { token_type, .. }) = iterator.peek() {
                        match token_type {
                            TokenType::Comma => continue,
                            TokenType::RightParen => break,
                            _ => {
                                return Err(parse_error(&format!(
                                    "function declaration expect comma between parameters: '{}'",
                                    fun_name
                                )))
                            }
                        }
                    }
                }
                TokenType::Comma => {
                    iterator.next();
                    eat_newline(iterator);
                    // there should be a variable afterward, test for it
                    if let Some(Token { token_type, .. }) = iterator.peek() {
                        if matches!(token_type, TokenType::Int | TokenType::Bool) {
                            continue;
                        }
                    }
                    return Err(parse_error(&format!(
                        "function declaration expect parameter type after comma: '{}'",
                        fun_name
                    )));
                }
                _ => break,
            };
        }
        break;
    }
    Ok(params)
}

// parse definitions
fn parse_def(iterator: &mut Peekable<Iter<Token>>) -> Result<Option<Def>, String> {
    eat_newline(iterator);

    // end of file?
    if iterator.peek().is_none() {
        return Ok(None);
    }

    // fun keyword
    eat_token(
        &TokenType::Fun,
        "missing token 'fun' in function declaration",
        iterator,
    )?;
    eat_newline(iterator);

    // get function name
    let fun_name;
    if let Some(Token { token_type, .. }) = iterator.next() {
        match token_type {
            TokenType::Var(name) => fun_name = name,
            _ => return Err(parse_error("invalid variable name in function declaration")),
        }
    } else {
        return Err(parse_error("missing function name in function declaration"));
    }
    eat_newline(iterator);

    // parameters
    // left parenthese
    eat_token(
        &TokenType::LeftParen,
        "missing token '(' in function declaration",
        iterator,
    )?;

    // get params
    eat_newline(iterator);
    let params = parse_param(iterator, &fun_name)?;

    // right parenthese
    eat_token(
        &TokenType::RightParen,
        "missing token ')' in function declaration",
        iterator,
    )?;
    eat_newline(iterator);

    // get function body
    // get body
    let body = parse_exp(iterator)?;
    eat_newline(iterator);

    // get end token
    eat_token(
        &TokenType::End,
        &format!(
            "missing token 'end' in function declaration: '{}'",
            fun_name
        ),
        iterator,
    )?;

    Ok(Some(Def::Fun(fun_name.to_string(), params, Box::new(body))))
}

// parse expression
fn parse_exp(iterator: &mut Peekable<Iter<Token>>) -> Result<Exp, String> {
    loop {
        if let Some(Token { token_type, .. }) = iterator.next() {
            return match token_type {
                TokenType::NewLine => continue,
                TokenType::Num(n) => parse_num(iterator, *n),
                TokenType::Var(var) => parse_var(iterator, var),
                TokenType::Let => parse_let(iterator, None),
                TokenType::LeftParen => {
                    let tokens = collect_binary(iterator, Some(TokenType::LeftParen))?;
                    parse_binary(tokens)
                }
                TokenType::True | TokenType::False => {
                    parse_boolean(iterator, Some(token_type.clone()))
                }
                TokenType::If => parse_ite(iterator),
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
        Some(_) => {
            let tokens = collect_binary(iterator, token_type)?;
            parse_binary(tokens)
        }
        None => Ok(Exp::Bool(matches!(token_type, Some(TokenType::True)))),
    }
}

// parse num
fn parse_num(iterator: &mut Peekable<Iter<Token>>, current: i32) -> Result<Exp, String> {
    match iterator.clone().peek() {
        Some(_) => {
            let tokens = collect_binary(iterator, Some(TokenType::Num(current)))?;
            parse_binary(tokens)
        }
        None => Ok(Exp::Num(current)),
    }
}

// collect binary tokens
fn collect_binary(
    iterator: &mut Peekable<Iter<Token>>,
    left: Option<TokenType>,
) -> Result<Vec<TokenType>, String> {
    let mut exp_tokens = Vec::<TokenType>::new();

    // only right parent ')' if a left one was opened before
    // if not, then just quit, sub expression should not infer with parent one
    let mut opened_paren = 0;

    if let Some(token_type) = left {
        if matches!(token_type, TokenType::LeftParen) {
            opened_paren += 1;
        }
        exp_tokens.push(token_type);
    }

    // is binary operation token?
    loop {
        if let Some(Token { token_type, .. }) = iterator.peek() {
            match token_type.clone() {
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
                    match token_type {
                        TokenType::LeftParen => opened_paren += 1,
                        TokenType::RightParen => {
                            if opened_paren <= 0 {
                                break;
                            }
                            opened_paren -= 1;
                        }
                        _ => ()
                    };
                    if let Some(next) = iterator.next() {
                        exp_tokens.push(next.token_type.clone())
                    }
                }
                // var, test if function application
                TokenType::Var(var_name) => {
                    if let Some(next) = iterator.next() {
                        if let Some(Token {token_type, ..}) = iterator.peek() {
                            if matches!(token_type, TokenType::LeftParen) {
                                let token = collect_call(iterator, &var_name)?;
                                exp_tokens.push(token);
                                continue;
                            }

                        }
                        exp_tokens.push(next.token_type.clone());
                    }
                }
                _ => break,
            }
        } else {
            break;
        }
    }

    // check if the number of parentheses is matching
    if opened_paren != 0 {
        return Err(parse_error("number of parenthesis not matching"));
    }

    Ok(exp_tokens)
}

/*
    - Parse binary -
    prefix postfix prefix conversion
    use shunting yard algorithm to convert prefix to postfix
    then convert back to prefix
*/

// prescedence priority
pub fn prescedence_op(op: &TokenType) -> u32 {
    match op {
        TokenType::And | TokenType::Or => 1,
        TokenType::Eq | TokenType::Sma | TokenType::Gta => 2,
        TokenType::Add | TokenType::Sub => 3,
        TokenType::Mult | TokenType::Div => 4,
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

pub fn parse_binary(tokens: Vec<TokenType>) -> Result<Exp, String> {
    let mut operator_stack = Vec::<TokenType>::new();
    let mut postfix = Vec::<TokenType>::new();
    let mut prefix_stack = Vec::<Exp>::new();

    // convert to postfix
    for t in tokens {
        match t {
            // basic
            TokenType::Num(_)
            | TokenType::Var(_)
            | TokenType::Call(_, _)
            | TokenType::True
            | TokenType::False => postfix.push(t),
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
            // call
            TokenType::Call(fun_name, args) => prefix_stack.push(Exp::Call(fun_name, args)),
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

// parse if-then-else
fn parse_ite(iterator: &mut Peekable<Iter<Token>>) -> Result<Exp, String> {
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
    let then_block = parse_exp(iterator)?;
    eat_newline(iterator);

    // else
    eat_token(
        &TokenType::Else,
        "missing token 'else' in condition",
        iterator,
    )?;
    let else_block = parse_exp(iterator)?;
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

// parse arguments
fn parse_args(iterator: &mut Peekable<Iter<Token>>, fun_name: &str) -> Result<Vec<Exp>, String> {
    let mut args = vec![];
    loop {
        if let Some(Token { token_type, .. }) = iterator.peek() {
            match token_type {
                TokenType::Comma => {
                    iterator.next();
                    eat_newline(iterator);
                    // there should be an expression afterward, test for it
                    if let Some(Token { token_type, .. }) = iterator.peek() {
                        if !matches!(token_type, TokenType::RightParen) {
                            continue;
                        }
                    }
                    return Err(parse_error(&format!(
                        "function application expect arguments after comma: '{}'",
                        fun_name
                    )));
                }
                // end of args list
                TokenType::RightParen => break,
                // if is expression
                _ => {
                    let arg = parse_exp(iterator)?;
                    args.push(arg);
                    eat_newline(iterator);
                    // there should be a comma afterward if more parameters
                    // else break
                    if let Some(Token { token_type, .. }) = iterator.peek() {
                        match token_type {
                            TokenType::Comma => continue,
                            TokenType::RightParen => break,
                            _ => {
                                return Err(parse_error(&format!(
                                    "function application expect comma between parameters: '{}'",
                                    fun_name
                                )))
                            }
                        }
                    }
                }
            };
        }
        break;
    }
    Ok(args)
}

// collect function call token
fn collect_call(iterator: &mut Peekable<Iter<Token>>, fun_name: &str) -> Result<TokenType, String> {
    // left parenthese
    eat_token(
        &TokenType::LeftParen,
        "missing token '(' in function application",
        iterator,
    )?;

    eat_newline(iterator);
    let args = parse_args(iterator, fun_name)?;

    // right parenthese
    eat_token(
        &TokenType::RightParen,
        "missing token ')' in function application",
        iterator,
    )?;

    Ok(TokenType::Call(fun_name.to_string(), args))
}

// parse function call tokens
fn parse_call(iterator: &mut Peekable<Iter<Token>>, token: TokenType) -> Result<Exp, String> {
    // check if there is binary operation afterward, if so then let parse_binary handle it
    if let Some(peek) = iterator.clone().peek() {
        // (binary expression)
        if is_binary(&peek.token_type) {
            let tokens = collect_binary(iterator, Some(token))?;
            return parse_binary(tokens);
        }
    }

    // else simply return
    match token {
        TokenType::Call(fun_name, args) => Ok(Exp::Call(fun_name.to_string(), args)),
        _ => Err(parse_error("wrong token type for function application")),
    }
}

// parse var
fn parse_var(iterator: &mut Peekable<Iter<Token>>, var: &str) -> Result<Exp, String> {
    if let Some(peek) = iterator.clone().peek() {
        // variable = expression
        if peek.token_type == TokenType::Assign {
            return parse_assign(iterator, Some(var));
        }
        // variable + 1 (binary expression)
        if is_binary(&peek.token_type) {
            let tokens = collect_binary(iterator, Some(TokenType::Var(var.to_string())))?;
            return parse_binary(tokens);
        }
        // variable() (function application)
        if peek.token_type == TokenType::LeftParen {
            let token = collect_call(iterator, var)?;
            return parse_call(iterator, token);
        }
    }

    Ok(Exp::Var(var.to_string()))
}

// parse assign (= exp \n body)
fn assign_helper(
    iterator: &mut Peekable<Iter<Token>>,
    var_name: Option<&str>,
) -> Result<(String, Exp, Exp), String> {
    // get variable name
    eat_newline(iterator);
    let var;
    match var_name {
        Some(name) => var = name,
        None => {
            if let Some(Token { token_type, .. }) = iterator.next() {
                match token_type {
                    TokenType::Var(var_name) => var = var_name,
                    _ => return Err(parse_error("invalid variable name in assignment")),
                }
            } else {
                return Err(parse_error("missing variable name in assignment"));
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
    let val = parse_exp(iterator)?;

    // get body of let or assign
    eat_token(
        &TokenType::NewLine,
        "missing new line after assignement for body",
        iterator,
    )?;

    let body = match parse_exp(iterator) {
        Ok(res) => res,
        Err(err) => {
            return Err(parse_error(&format!(
                "invalid body in assignement for variable `{}`, body - {}",
                var, err
            )))
        }
    };

    Ok((var.to_owned(), val, body))
}

fn parse_assign(
    iterator: &mut Peekable<Iter<Token>>,
    var_name: Option<&str>,
) -> Result<Exp, String> {
    let (var, val, body) = assign_helper(iterator, var_name)?;
    Ok(Exp::Assign(var.to_string(), Box::new(val), Box::new(body)))
}

// parse let
fn parse_let(iterator: &mut Peekable<Iter<Token>>, var_name: Option<&str>) -> Result<Exp, String> {
    let (var, val, body) = assign_helper(iterator, var_name)?;
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
    fn nums() {
        let tokens = scan("fun main() 10 end").unwrap();
        assert_eq!(parse(&tokens), Ok(vec!(d_fun("main", &vec![], Num(10)))))
    }

    #[test]
    fn sub() {
        let tokens = scan("fun main() 10 - 5 end").unwrap();
        assert_eq!(
            parse(&tokens),
            Ok(vec!(d_fun("main", &vec![], d_sub(Num(10), Num(5)))))
        )
    }

    #[test]
    fn boolean() {
        let tokens = scan("fun main() true & (false | true) end").unwrap();
        assert_eq!(
            parse(&tokens),
            Ok(vec!(d_fun(
                "main",
                &vec![],
                d_and(Bool(true), d_or(Bool(false), Bool(true))),
            )))
        )
    }

    #[test]
    fn binary_bool() {
        let tokens = scan("fun main() 1 < 2 & 2 + 1 == 2 & 3> 2 end").unwrap();
        assert_eq!(
            parse(&tokens),
            Ok(vec!(d_fun(
                "main",
                &vec![],
                d_and(
                    d_and(d_sma(Num(1), Num(2)), d_eq(d_add(Num(2), Num(1)), Num(2))),
                    d_gta(Num(3), Num(2))
                ),
            )))
        )
    }

    #[test]
    fn numerics_op() {
        let tokens = scan("fun main() (2 * 1 + 2) / 2 end").unwrap();
        assert_eq!(
            parse(&tokens),
            Ok(vec!(d_fun(
                "main",
                &vec![],
                d_div(d_add(d_mult(Num(2), Num(1)), Num(2)), Num(2)),
            )))
        )
    }

    #[test]
    fn ite() {
        let tokens =
            scan("fun main\n (\n ) \n \n if \n 1 < 2 \n then \n 1 \n else \n 2 \n \n end end")
                .unwrap();
        assert_eq!(
            parse(&tokens),
            Ok(vec!(d_fun(
                "main",
                &vec![],
                d_ite(d_sma(Num(1), Num(2)), Num(1), Num(2)),
            )))
        )
    }

    #[test]
    fn var() {
        let tokens = scan("fun main() let a \n = \n 1 + 2 \n a = a + 2 \n a end").unwrap();
        assert_eq!(
            parse(&tokens),
            Ok(vec!(d_fun(
                "main",
                &vec![],
                d_let(
                    "a",
                    d_add(Num(1), Num(2)),
                    d_assign(
                        "a",
                        d_add(Var("a".to_string()), Num(2)),
                        Var("a".to_string())
                    )
                ),
            )))
        )
    }

    #[test]
    fn fun() {
        let tokens = scan("fun a() 1 end fun main() let a = 1 \n a end").unwrap();
        assert_eq!(
            parse(&tokens),
            Ok(vec!(
                d_fun("main", &vec![], d_let("a", Num(1), d_var("a"))),
                d_fun("a", &vec![], Num(1))
            ))
        )
    }

    #[test]
    fn call_fun() {
        let tokens = scan("fun a() 1 end fun main() a() + 1 end").unwrap();
        assert_eq!(
            parse(&tokens),
            Ok(vec!(
                d_fun("main", &vec![], d_add(d_call("a", &vec![]), Num(1))),
                d_fun("a", &vec![], Num(1))
            ))
        )
    }

    #[test]
    fn fun_par() {
        let tokens = scan("\nfun \nmain\n(\n\n)\n main(\n1\n,\n2\n) + fee \n end").unwrap();
        assert_eq!(
            parse(&tokens),
            Ok(vec!(d_fun(
                "main",
                &vec![],
                d_add(d_call("main", &vec!(Num(1), Num(2))), d_var("fee")),
            )))
        )
    }

    #[test]
    fn simple_type() {
        let tokens =
            scan("fun main() fee(10, true) end fun fee(\nint\n a\n, bool b) a end").unwrap();
        assert_eq!(
            parse(&tokens),
            Ok(vec!(
                d_fun("main", &vec![], d_call("fee", &vec!(Num(10), Bool(true)))),
                d_fun(
                    "fee",
                    &vec!((Type::Int, "a".into()), (Type::Bool, "b".into())),
                    d_var("a")
                )
            ))
        )
    }
}
