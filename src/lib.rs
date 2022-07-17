mod codegen;
mod parser;
mod syntax;

use codegen::codegen;
use parser::parse;

pub fn compile(contents: &str) -> Result<Vec<u8>, Vec<String>> {
    // parse and etc..
    // let program = parse("+\n \n \n  // hello     10.5 + \n  10 ").unwrap();
    let program = parse(contents)?;
    return Ok(codegen(program));
}
