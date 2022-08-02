mod codegen;
mod parser;
mod scanner;
mod syntax;
mod token;

use codegen::codegen;
use parser::parse;
use scanner::scan;

pub fn compile(contents: &str) -> Result<Vec<u8>, Vec<String>> {
    // parse and etc..
    // let program = parse("+\n \n \n  // hello     10.5 + \n  10 ").unwrap();
    let tokens = scan(contents)?;
    let program = parse(&tokens);
    return Ok(codegen(program));
}
