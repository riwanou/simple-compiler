// Expressions
#[allow(dead_code)]
#[derive(Debug, PartialEq)]
pub enum Exp {
    Bool(bool),
    Num(i32),
    // Binary op
    Add(Box<Exp>, Box<Exp>),
    Sub(Box<Exp>, Box<Exp>),
    Mult(Box<Exp>, Box<Exp>),
    Div(Box<Exp>, Box<Exp>),
}

// desugars methods to avoid box boilerplates
pub mod desugars {
    use super::*;

    // expressions
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
