// Expressions
#[allow(dead_code)]
#[derive(Debug, PartialEq)]
pub enum Exp {
    Bool(bool),
    Num(i32),
    // Binary op
    // boolean
    And(Box<Exp>, Box<Exp>),
    Or(Box<Exp>, Box<Exp>),
    // number
    Add(Box<Exp>, Box<Exp>),
    Sub(Box<Exp>, Box<Exp>),
    Mult(Box<Exp>, Box<Exp>),
    Div(Box<Exp>, Box<Exp>),
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
}
