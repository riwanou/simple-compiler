// Expressions
#[allow(dead_code)]
#[derive(Debug, PartialEq)]
pub enum Exp {
    Bool(bool),
    Num(i32),
    Add(Box<Exp>, Box<Exp>),
    Mult(Box<Exp>, Box<Exp>),
}

// desugars methods to avoid box boilerplates
pub mod desugars {
    use super::*;
    // expressions
    #[allow(dead_code)]
    pub fn d_add(l: Exp, r: Exp) -> Exp {
        Exp::Add(Box::new(l), Box::new(r))
    }
}
