mod codegen;
mod parser;
mod scanner;
mod syntax;
mod token;
mod typecheck;

use codegen::codegen;
use parser::parse;
use scanner::scan;
use typecheck::typecheck;

pub fn compile(contents: &str) -> Result<Vec<u8>, Vec<String>> {
    // scan string and generate tokens
    let tokens = scan(contents)?;
    // parse the tokens
    // variables name key set
    let program = parse(&tokens)?;
    // typecheck program
    let localsnb_table = typecheck(&program)?;
    // generate program
    return Ok(codegen(&program, &localsnb_table));
}
