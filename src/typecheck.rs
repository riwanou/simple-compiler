use crate::syntax::{Def, Exp, Type};
use std::collections::HashMap;

pub struct FunInfo {
    params: HashMap<String, Type>,
    nb_locals: usize,
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
pub fn typecheck(program: &Vec<Def>) -> Result<HashMap<String, usize>, Vec<String>> {
    let mut env = HashMap::<String, FunInfo>::new();

    // first pass, add functions definition
    program.iter().for_each(|def| match def {
        Def::Fun(name, params, _) => {
            let info = FunInfo {
                params: params
                    .iter()
                    .map(|param| (param.to_string(), Type::Int))
                    .collect(),
                nb_locals: 0,
            };
            env.insert(name.to_string(), info);
        }
    });

    // second pass, typecheck functions
    for def in program {
        typecheck_def(&def, &mut env)?;
    }

    let funs_locals = env
        .iter()
        .map(|(name, info)| (name.to_string(), info.nb_locals))
        .collect();

    Ok(funs_locals)
}

pub fn typecheck_def(fun: &Def, env: &mut HashMap<String, FunInfo>) -> Result<(), Vec<String>> {
    match fun {
        Def::Fun(name, _, body) => {
            let fun = env
                .get_mut(name)
                .expect("problem in typecheck, function not in env");
            let mut var_env = fun.params.clone();
            typecheck_exp(body, &mut var_env)?;
            fun.nb_locals = var_env.len();
        }
    };
    Ok(())
}

// expression
pub fn typecheck_exp(exp: &Exp, env: &mut HashMap<String, Type>) -> Result<Type, Vec<String>> {
    let mut errors = vec![];
    let exp_type = match exp {
        // basics
        Exp::Num(_) => Type::Int,
        Exp::Bool(_) => Type::Bool,
        Exp::Var(var_name) => typecheck_var(var_name, env)?,
        // numeric binary
        Exp::Add(a, b) | Exp::Sub(a, b) | Exp::Div(a, b) | Exp::Mult(a, b) => typecheck_binary(
            a,
            b,
            env,
            Type::Int,
            "mixing of numeric and other type in binary operation",
            &mut errors,
        )?,
        // boolean binary
        Exp::And(a, b) | Exp::Or(a, b) => {
            println!("A: {:?}, B: {:?}", a, b);
            typecheck_binary(
                a,
                b,
                env,
                Type::Bool,
                "mixing of boolean and other type in binary operation",
                &mut errors,
            )?
        }
        // comparison
        Exp::Sma(a, b) | Exp::Gta(a, b) | Exp::Eq(a, b) => {
            typecheck_binary(
                a,
                b,
                env,
                Type::Int,
                "mixing of numeric and other type in comparison",
                &mut errors,
            )?;
            Type::Bool
        }
        // if then else
        Exp::Ite(cond, the, els) => typecheck_ite(cond, the, els, env, &mut errors)?,
        // variables
        Exp::Assign(var, val, body) => typecheck_assign(var, val, body, env, &mut errors)?,
        Exp::Let(var, val, body) => typecheck_let(var, val, body, env)?,
        // call
        Exp::Call(_, _) => Type::Int,
    };
    if errors.len() > 0 {
        return Err(errors);
    }
    Ok(exp_type)
}

// binary
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

// variables
fn typecheck_var(var_name: &str, env: &mut HashMap<String, Type>) -> Result<Type, Vec<String>> {
    match env.get(var_name) {
        Some(var_type) => Ok(var_type.to_owned()),
        None => Err(vec![type_error(&format!(
            "variable not defined: '{}'",
            var_name
        ))]),
    }
}

// variable assignement
// define for first time variable
fn typecheck_let(
    var: &str,
    val: &Exp,
    body: &Exp,
    env: &mut HashMap<String, Type>,
) -> Result<Type, Vec<String>> {
    // infer variable type
    let val_type = typecheck_exp(val, env)?;
    env.insert(var.to_string(), val_type.clone());
    // check body with modified env
    typecheck_exp(body, env)?;
    // return variable type
    Ok(val_type)
}

// assign
// mutate variable
fn typecheck_assign(
    var: &str,
    val: &Exp,
    body: &Exp,
    env: &mut HashMap<String, Type>,
    errors: &mut Vec<String>,
) -> Result<Type, Vec<String>> {
    // check if variable was defined before
    let var_type = match env.get(var) {
        Some(res) => res.clone(),
        None => {
            return Err(vec![format!(
                "try to assign non defined variable: '{}'",
                var
            )])
        }
    };
    // infer variable type
    let val_type = typecheck_exp(val, env)?;
    // type must stricly be equal to defined one
    collect_error(
        errors,
        &assert_type(
            &val_type,
            &var_type,
            &format!("assign and defined type mismatch: '{}'", var),
        ),
    );
    // check body with modified env
    typecheck_exp(body, env)?;
    // return variable type
    Ok(val_type)
}

// if then else
fn typecheck_ite(
    cond: &Exp,
    _the: &Exp,
    _els: &Exp,
    env: &mut HashMap<String, Type>,
    errors: &mut Vec<String>,
) -> Result<Type, Vec<String>> {
    let cond_type = typecheck_exp(cond, env)?;
    collect_error(
        errors,
        &assert_type(
            &cond_type,
            &Type::Bool,
            "boolean expected for condition in if block",
        ),
    );
    Ok(cond_type)
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

    #[test]
    pub fn comparison() {
        let tokens = scan("fun main() true & 1 + 2 < true & 2 == 2 end").unwrap();
        let program = parse(&tokens).unwrap();
        assert_eq!(
            typecheck(&program),
            Err(vec!(type_error(
                "mixing of numeric and other type in comparison"
            )))
        );
    }

    #[test]
    pub fn non_defined_var() {
        let tokens = scan("fun main() let foo = 2 \n fee end").unwrap();
        let program = parse(&tokens).unwrap();
        assert_eq!(
            typecheck(&program),
            Err(vec!(type_error("variable not defined: 'fee'")))
        );
    }

    #[test]
    pub fn assign_var() {
        let tokens = scan("fun main() let foo = 2 \n foo = true \n foo end").unwrap();
        let program = parse(&tokens).unwrap();
        assert_eq!(
            typecheck(&program),
            Err(vec!(type_error("assign and defined type mismatch: 'foo'")))
        );
    }
}
