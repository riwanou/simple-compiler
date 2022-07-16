mod codegen;
mod syntax;

use codegen::codegen;
use syntax::desugars::*;
use syntax::Exp::*;

pub fn compile(_contents: &str) -> Vec<u8> {
    // parse and etc..
    let program = d_add(Num(1), Num(2));
    // generate binary code
    let bytes = codegen(program);
    return bytes;
}
