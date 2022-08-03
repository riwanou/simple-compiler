mod codegen;
mod parser;
mod scanner;
mod syntax;
mod token;

use codegen::codegen;
use parser::parse;
use scanner::scan;

pub fn compile(contents: &str) -> Result<Vec<u8>, Vec<String>> {
    // scan string and generate tokens
    let tokens = scan(contents)?;
    // parse the tokens
    let program = parse(&tokens)?;
    // generate program
    return Ok(codegen(program));
}
