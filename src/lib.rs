mod codegen;
mod parser;
mod scanner;
mod syntax;
mod token;

use std::collections::HashSet;

use codegen::codegen;
use parser::parse;
use scanner::scan;

pub fn compile(contents: &str) -> Result<Vec<u8>, Vec<String>> {
    // scan string and generate tokens
    let tokens = scan(contents)?;
    // parse the tokens
    // variables name key set
    let program = parse(&tokens)?;
    // generate program
    // return Ok(codegen(program, &var_set));
    todo!()
}
