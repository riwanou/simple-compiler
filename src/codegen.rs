use wasm_encoder::{
    BlockType, CodeSection, Export, ExportSection, Function, FunctionSection, Instruction, Module,
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

// binary expression code generator
pub fn binary_exp(f: &mut Function, left: Exp, right: Exp, instruction: Instruction) {
    codegen_exp(f, left);
    codegen_exp(f, right);
    f.instruction(instruction);
}

// codegen expression
pub fn codegen_exp(f: &mut Function, e: Exp) {
    match e {
        // bool
        Exp::Bool(b) => {
            f.instruction(Instruction::I32Const(b as i32));
        }
        // binary bool
        Exp::And(l, r) => binary_exp(f, *l, *r, Instruction::I32And),
        Exp::Or(l, r) => binary_exp(f, *l, *r, Instruction::I32Or),
        Exp::Eq(l, r) => binary_exp(f, *l, *r, Instruction::I32Eq),
        Exp::Sma(l, r) => binary_exp(f, *l, *r, Instruction::I32LtS),
        Exp::Gta(l, r) => binary_exp(f, *l, *r, Instruction::I32GtS),
        // num
        Exp::Num(n) => {
            f.instruction(Instruction::I32Const(n));
        }
        // binary num
        Exp::Add(l, r) => binary_exp(f, *l, *r, Instruction::I32Add),
        Exp::Sub(l, r) => binary_exp(f, *l, *r, Instruction::I32Sub),
        Exp::Mult(l, r) => binary_exp(f, *l, *r, Instruction::I32Mul),
        Exp::Div(l, r) => binary_exp(f, *l, *r, Instruction::I32DivS),
        // ite
        Exp::Ite(b, t, e) => {
            codegen_exp(f, *b);
            f.instruction(Instruction::If(BlockType::Result(ValType::I32)));
            codegen_exp(f, *t);
            f.instruction(Instruction::Else);
            codegen_exp(f, *e);
            f.instruction(Instruction::End);
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

    #[test]
    fn ite() {
        assert_eq!(
            print_bytes(&codegen(d_ite(Bool(false), Num(1), Num(2)))).unwrap(),
            code_format(
                "i32.const 0\n\
                if  ;; label = @1\n  \
                i32.const 1\n\
                else\n  \
                i32.const 2\n\
                end"
            )
        );
    }
}
