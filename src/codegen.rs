use wasm_encoder::{
    CodeSection, Export, ExportSection, Function, FunctionSection, Instruction, Module,
    TypeSection, ValType,
};

use crate::syntax::Exp;

// Generate a WASM module using body as main function
pub fn codegen_module(body: Function) -> Vec<u8> {
    // generate module
    let mut module = Module::new();
    // type section
    let mut types = TypeSection::new();
    let params = vec![];
    let results = vec![ValType::I32];
    types.function(params, results);
    module.section(&types);
    // function section
    let mut functions = FunctionSection::new();
    let type_index = 0;
    functions.function(type_index);
    module.section(&functions);
    // export section
    let mut exports = ExportSection::new();
    exports.export("main", Export::Function(0));
    module.section(&exports);
    // code section
    let mut codes = CodeSection::new();
    codes.function(&body);
    module.section(&codes);
    // encoded wasm bytes for this module
    let bytes = module.finish();
    return bytes;
}

// codegen given program
pub fn codegen(program: Exp) -> Vec<u8> {
    // main function
    let locals = vec![];
    let mut main = Function::new(locals);
    // populate function
    codegen_exp(&mut main, program);
    main.instruction(Instruction::End);
    // generate module
    codegen_module(main)
}

// codegen expression
pub fn codegen_exp(f: &mut Function, e: Exp) {
    match e {
        Exp::Bool(b) => f.instruction(Instruction::I32Const(b as i32)),
        Exp::Num(n) => f.instruction(Instruction::I32Const(n)),
        Exp::Add(l, r) => {
            codegen_exp(f, *l);
            codegen_exp(f, *r);
            f.instruction(Instruction::I32Add)
        }
        Exp::Sub(l, r) => {
            codegen_exp(f, *l);
            codegen_exp(f, *r);
            f.instruction(Instruction::I32Sub)
        }
        Exp::Mult(l, r) => {
            codegen_exp(f, *l);
            codegen_exp(f, *r);
            f.instruction(Instruction::I32Mul)
        }
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::desugars::*;
    use pretty_assertions::assert_eq;
    use wasmprinter::print_bytes;
    use Exp::*;

    // format code like wasmprinter
    fn code_format(body: &str) -> String {
        let body: Vec<&str> = body.split("\n").collect();
        [
            "(module\n  \
            (type (;0;) (func (result i32)))\n  \
            (func (;0;) (type 0) (result i32)\n    ",
            body.join("\n    ").as_str(),
            "\n  )\n  \
            (export \"main\" (func 0))\n\
        )",
        ]
        .concat()
    }

    #[test]
    fn add() {
        assert_eq!(
            print_bytes(&codegen(d_add(Num(10), d_add(Num(20), Num(5))))).unwrap(),
            code_format(
                "i32.const 10\n\
                i32.const 20\n\
                i32.const 5\n\
                i32.add\n\
                i32.add"
            )
        );
    }
}
