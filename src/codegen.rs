use wasm_encoder::{
    BlockType, CodeSection, Export, ExportSection, Function, FunctionSection, Instruction, Module,
    TypeSection, ValType,
};

use crate::syntax::Def;
use crate::syntax::Exp;
use std::collections::{HashMap, HashSet};

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

/*
    local variable environment
*/
pub struct Env {
    map: HashMap<String, u32>,
    counter: u32,
}

impl Env {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
            counter: 0,
        }
    }

    pub fn add(&mut self, var: &str) -> u32 {
        self.map.insert(var.to_string(), self.counter);
        let last = self.counter;
        self.counter += 1;
        last
    }

    pub fn get(&self, var: &str) -> Option<&u32> {
        self.map.get(var)
    }
}

// codegen given program
pub fn codegen(program: &Vec<Def>) -> Vec<u8> {
    // create module
    let mut module = Module::new();

    // types and function section
    let mut types = TypeSection::new();
    let mut type_index = 0;
    let mut functions = FunctionSection::new();

    // generate main type
    types.function(vec![], vec![ValType::I32]);
    functions.function(type_index);

    // generate type for every functions
    for _ in 1..program.len() {
        type_index += 1;
        types.function(vec![], vec![ValType::I32]);
        functions.function(type_index);
    }

    // populate first part of module
    module.section(&types);
    module.section(&functions);

    // export main entry point
    let mut exports = ExportSection::new();
    exports.export("main", Export::Function(0));
    module.section(&exports);

    // generate code
    let mut codes = CodeSection::new();

    // generate functions
    for i in 0..program.len() {
        let fun = codegen_fun(program[i].to_owned());
        codes.function(&fun);
    }

    // populate code part of module
    module.section(&codes);

    // encode module
    let bytes = module.finish();
    return bytes;
}

// codegen function
// each function has it own environment
pub fn codegen_fun(f: Def) -> Function {
    match f {
        Def::Fun(name, body, locals_nb) => {
            // generate function
            let locals = vec![(locals_nb as u32, ValType::I32)];
            let mut fun = Function::new(locals);
            codegen_exp(&mut fun, *body, &mut Env::new());
            fun.instruction(Instruction::End);
            fun
        }
        // generic function, should not happen..
        _ => Function::new(vec![]),
    }
}

// binary expression code generator
pub fn binary_exp(
    f: &mut Function,
    left: Exp,
    right: Exp,
    instruction: Instruction,
    env: &mut Env,
) {
    codegen_exp(f, left, env);
    codegen_exp(f, right, env);
    f.instruction(instruction);
}

// codegen expression
pub fn codegen_exp(f: &mut Function, e: Exp, env: &mut Env) {
    match e {
        // bool
        Exp::Bool(b) => {
            f.instruction(Instruction::I32Const(b as i32));
        }
        // binary bool
        Exp::And(l, r) => binary_exp(f, *l, *r, Instruction::I32And, env),
        Exp::Or(l, r) => binary_exp(f, *l, *r, Instruction::I32Or, env),
        Exp::Eq(l, r) => binary_exp(f, *l, *r, Instruction::I32Eq, env),
        Exp::Sma(l, r) => binary_exp(f, *l, *r, Instruction::I32LtS, env),
        Exp::Gta(l, r) => binary_exp(f, *l, *r, Instruction::I32GtS, env),
        // num
        Exp::Num(n) => {
            f.instruction(Instruction::I32Const(n));
        }
        // binary num
        Exp::Add(l, r) => binary_exp(f, *l, *r, Instruction::I32Add, env),
        Exp::Sub(l, r) => binary_exp(f, *l, *r, Instruction::I32Sub, env),
        Exp::Mult(l, r) => binary_exp(f, *l, *r, Instruction::I32Mul, env),
        Exp::Div(l, r) => binary_exp(f, *l, *r, Instruction::I32DivS, env),
        // ite
        Exp::Ite(b, t, e) => {
            codegen_exp(f, *b, env);
            f.instruction(Instruction::If(BlockType::Result(ValType::I32)));
            codegen_exp(f, *t, env);
            f.instruction(Instruction::Else);
            codegen_exp(f, *e, env);
            f.instruction(Instruction::End);
        }
        // var
        Exp::Var(str) => {
            match env.get(&str) {
                Some(var) => f.instruction(Instruction::LocalGet(*var)),
                _ => f.instruction(Instruction::I32Const(0)),
            };
        }
        Exp::Let(str, val, body) => {
            let var = match env.get(&str) {
                Some(value) => value.clone(),
                None => env.add(&str),
            };
            codegen_exp(f, *val, env);
            f.instruction(Instruction::LocalSet(var));
            codegen_exp(f, *body, env)
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

    fn function_format(body: &str, index: char) -> String {
        let body: Vec<&str> = body.split("\n").collect();
        [
            &format!("(func (;{};) (type {}) (result i32)\n    ", index, index),
            body.join("\n    ").as_str(),
            "\n  )\n  ",
        ]
        .concat()
    }

    #[test]
    fn add() {
        assert_eq!(
            print_bytes(&codegen(&mut vec!(d_fun(
                "main",
                d_add(Num(10), d_add(Num(20), Num(5))),
                0
            ))))
            .unwrap(),
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
            print_bytes(&codegen(&vec!(d_fun(
                "main",
                d_ite(Bool(false), Num(1), Num(2)),
                0
            ))))
            .unwrap(),
            code_format(
                "i32.const 0\n\
                if (result i32)  ;; label = @1\n  \
                i32.const 1\n\
                else\n  \
                i32.const 2\n\
                end"
            )
        );
    }

    #[test]
    fn var() {
        assert_eq!(
            print_bytes(&codegen(&vec!(d_fun(
                "main",
                d_let("a", Num(1), Var("a".to_string())),
                0
            ))))
            .unwrap(),
            code_format(
                "i32.const 1\n\
                local.set 0\n\
                local.get 0"
            )
        );
    }

    #[test]
    fn multiple_fun() {
        assert_eq!(
            print_bytes(&codegen(&vec!(
                d_fun("main", Num(1), 0),
                d_fun("foo", Num(2), 0)
            )))
            .unwrap(),
            [
                "(module\n  \
                (type (;0;) (func (result i32)))\n  ",
                "(type (;1;) (func (result i32)))\n  ",
                &function_format("i32.const 1", '0'),
                &function_format("i32.const 2", '1'),
                "(export \"main\" (func 0))\n)"
            ]
            .concat()
        )
    }
}
