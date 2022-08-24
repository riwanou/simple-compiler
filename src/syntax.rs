// expression
#[allow(dead_code)]
#[derive(Debug, PartialEq, Clone)]
pub enum Exp {
    Bool(bool),
    Num(i32),
    // variable
    Var(String),
    Let(String, Box<Exp>, Box<Exp>),
    // Binary op
    // boolean
    And(Box<Exp>, Box<Exp>),
    Or(Box<Exp>, Box<Exp>),
    Eq(Box<Exp>, Box<Exp>),
    Sma(Box<Exp>, Box<Exp>),
    Gta(Box<Exp>, Box<Exp>),
    // number
    Add(Box<Exp>, Box<Exp>),
    Sub(Box<Exp>, Box<Exp>),
    Mult(Box<Exp>, Box<Exp>),
    Div(Box<Exp>, Box<Exp>),
    // if-then-else
    Ite(Box<Exp>, Box<Exp>, Box<Exp>),
}

// definition
#[allow(dead_code)]
#[derive(Debug, PartialEq, Clone)]
pub enum Def {
    // function
    Fun(String, Box<Exp>, usize),
}

// desugars methods to avoid box boilerplates
pub mod desugars {
    use super::*;

    // expressions
    // boolean
    #[allow(dead_code)]
    pub fn d_and(l: Exp, r: Exp) -> Exp {
        Exp::And(Box::new(l), Box::new(r))
    }
    #[allow(dead_code)]
    pub fn d_or(l: Exp, r: Exp) -> Exp {
        Exp::Or(Box::new(l), Box::new(r))
    }
    #[allow(dead_code)]
    pub fn d_eq(l: Exp, r: Exp) -> Exp {
        Exp::Eq(Box::new(l), Box::new(r))
    }
    #[allow(dead_code)]
    pub fn d_sma(l: Exp, r: Exp) -> Exp {
        Exp::Sma(Box::new(l), Box::new(r))
    }
    #[allow(dead_code)]
    pub fn d_gta(l: Exp, r: Exp) -> Exp {
        Exp::Gta(Box::new(l), Box::new(r))
    }

    // number
    #[allow(dead_code)]
    pub fn d_add(l: Exp, r: Exp) -> Exp {
        Exp::Add(Box::new(l), Box::new(r))
    }
    #[allow(dead_code)]
    pub fn d_sub(l: Exp, r: Exp) -> Exp {
        Exp::Sub(Box::new(l), Box::new(r))
    }
    #[allow(dead_code)]
    pub fn d_mult(l: Exp, r: Exp) -> Exp {
        Exp::Mult(Box::new(l), Box::new(r))
    }
    #[allow(dead_code)]
    pub fn d_div(l: Exp, r: Exp) -> Exp {
        Exp::Div(Box::new(l), Box::new(r))
    }

    // if-then-else
    #[allow(dead_code)]
    pub fn d_ite(b: Exp, t: Exp, e: Exp) -> Exp {
        Exp::Ite(Box::new(b), Box::new(t), Box::new(e))
    }

    // var
    #[allow(dead_code)]
    pub fn d_var(var: &str) -> Exp {
        Exp::Var(var.to_string())
    }
    #[allow(dead_code)]
    pub fn d_let(var: &str, val: Exp, body: Exp) -> Exp {
        Exp::Let(var.to_string(), Box::new(val), Box::new(body))
    }

    // definition
    // function
    #[allow(dead_code)]
    pub fn d_fun(name: &str, body: Exp, locals_nb: usize) -> Def {
        Def::Fun(name.to_string(), Box::new(body), locals_nb)
    }
}
