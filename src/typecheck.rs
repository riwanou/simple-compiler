use crate::syntax::{Def, Exp, Type};
use std::collections::HashMap;

pub struct FunInfo {
    params: HashMap<String, Type>,
    nb_locals: u32,
}

// errors handling
pub fn type_error(error: &str) -> String {
    format!("Typecheck error: {}", error)
}

pub fn collect_error(errors: &mut Vec<String>, err: &Result<(), String>) {
    match err {
        Ok(_) => (),
        Err(err) => errors.push(err.to_string()),
    };
}

pub fn assert_type(current: &Type, expected: &Type, error_msg: &str) -> Result<(), String> {
    if current == expected {
        Ok(())
    } else {
        Err(type_error(error_msg))
    }
}

// typecheck given program
// return a mapping giving number of locals of each funtions
pub fn typecheck(program: &Vec<Def>) -> Result<HashMap<String, u32>, Vec<String>> {
    let mut env = HashMap::<String, FunInfo>::new();

    // only check main function at first
    let main = program[0].to_owned();
    typecheck_def(&main, &mut env)?;

    let funs_locals = env
        .iter()
        .map(|(name, info)| (name.to_string(), info.nb_locals))
        .collect();

    Ok(funs_locals)
}

pub fn typecheck_def(fun: &Def, env: &mut HashMap<String, FunInfo>) -> Result<(), Vec<String>> {
    match fun {
        Def::Fun(name, params, body) => {
            let mut var_env = HashMap::<String, Type>::new();
            typecheck_exp(body, &mut var_env)?;
            let info = FunInfo {
                params: params
                    .iter()
                    .map(|param| (param.to_string(), Type::Int))
                    .collect(),
                nb_locals: (params.len() + var_env.len()) as u32,
            };
            env.insert(name.to_string(), info);
        }
    };
    Ok(())
}

pub fn typecheck_binary(
    a: &Exp,
    b: &Exp,
    _env: &mut HashMap<String, Type>,
    expected: Type,
    error: &str,
    errors: &mut Vec<String>,
) -> Result<Type, Vec<String>> {
    let left = typecheck_exp(a, _env)?;
    collect_error(errors, &assert_type(&left, &expected, error));
    let right = typecheck_exp(b, _env)?;
    collect_error(errors, &assert_type(&right, &expected, error));
    Ok(right)
}

pub fn typecheck_exp(exp: &Exp, _env: &mut HashMap<String, Type>) -> Result<Type, Vec<String>> {
    let mut errors = vec![];
    let exp_type = match exp {
        Exp::Num(_) => Type::Int,
        Exp::Bool(_) => Type::Bool,
        // numeric binary
        Exp::Add(a, b) | Exp::Sub(a, b) | Exp::Div(a, b) | Exp::Mult(a, b) => typecheck_binary(
            a,
            b,
            _env,
            Type::Int,
            "mixing of numeric and other type in binary operation",
            &mut errors,
        )?,
        // boolean binary
        Exp::And(a, b) | Exp::Or(a, b) | Exp::Eq(a, b) => typecheck_binary(
            a,
            b,
            _env,
            Type::Bool,
            "mixing of boolean and other type in binary operation",
            &mut errors,
        )?,
        _ => panic!("need typecheck implementation for this expression"),
    };
    if errors.len() > 0 {
        return Err(errors);
    }
    Ok(exp_type)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{parser::parse, scanner::scan};
    use pretty_assertions::assert_eq;

    #[test]
    pub fn add() {
        let tokens = scan("fun main() 1 + 1 + true end").unwrap();
        let program = parse(&tokens).unwrap();
        assert_eq!(
            typecheck(&program),
            Err(vec!(type_error(
                "mixing of numeric and other type in binary operation"
            )))
        );
    }

    #[test]
    pub fn and() {
        let tokens = scan("fun main() true & 1 & true end").unwrap();
        let program = parse(&tokens).unwrap();
        assert_eq!(
            typecheck(&program),
            Err(vec!(type_error(
                "mixing of boolean and other type in binary operation"
            )))
        );
    }
}
