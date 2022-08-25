use wasm_encoder::{
    BlockType, CodeSection, Export, ExportSection, Function, FunctionSection, Instruction, Module,
    TypeSection, ValType,
};

use crate::syntax::Def;
use crate::syntax::Exp;
use std::collections::HashMap;

/*
    local environment
*/
#[derive(Debug)]
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
    let mut functions = FunctionSection::new();

    // function environment
    // map function name to their type index
    let mut fun_env = Env::new();

    // generate type for every functions
    for i in 0..program.len() {
        functions.function(fun_env.counter);
        match program[i].to_owned() {
            Def::Fun(name, params, _, _) => {
                types.function(vec![ValType::I32; params.len()], vec![ValType::I32]);
                fun_env.add(&name);
            }
        };
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
        let fun = codegen_fun(program[i].to_owned(), &fun_env);
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
pub fn codegen_fun(f: Def, fun_env: &Env) -> Function {
    match f {
        Def::Fun(_, param, body, locals_nb) => {
            // generate function
            let locals = vec![((locals_nb + param.len()) as u32, ValType::I32)];
            let mut fun = Function::new(locals);
            // populate env with function params
            let mut env = Env::new();
            param.iter().for_each(|param| {
                env.add(&param);
            });
            codegen_exp(&mut fun, *body, &mut env, fun_env);
            fun.instruction(Instruction::End);
            fun
        }
    }
}

// binary expression code generator
pub fn binary_exp(
    f: &mut Function,
    left: Exp,
    right: Exp,
    instruction: Instruction,
    env: &mut Env,
    fun_env: &Env,
) {
    codegen_exp(f, left, env, fun_env);
    codegen_exp(f, right, env, fun_env);
    f.instruction(instruction);
}

// codegen expression
pub fn codegen_exp(f: &mut Function, e: Exp, env: &mut Env, fun_env: &Env) {
    match e {
        // bool
        Exp::Bool(b) => {
            f.instruction(Instruction::I32Const(b as i32));
        }
        // binary bool
        Exp::And(l, r) => binary_exp(f, *l, *r, Instruction::I32And, env, fun_env),
        Exp::Or(l, r) => binary_exp(f, *l, *r, Instruction::I32Or, env, fun_env),
        Exp::Eq(l, r) => binary_exp(f, *l, *r, Instruction::I32Eq, env, fun_env),
        Exp::Sma(l, r) => binary_exp(f, *l, *r, Instruction::I32LtS, env, fun_env),
        Exp::Gta(l, r) => binary_exp(f, *l, *r, Instruction::I32GtS, env, fun_env),
        // num
        Exp::Num(n) => {
            f.instruction(Instruction::I32Const(n));
        }
        // binary num
        Exp::Add(l, r) => binary_exp(f, *l, *r, Instruction::I32Add, env, fun_env),
        Exp::Sub(l, r) => binary_exp(f, *l, *r, Instruction::I32Sub, env, fun_env),
        Exp::Mult(l, r) => binary_exp(f, *l, *r, Instruction::I32Mul, env, fun_env),
        Exp::Div(l, r) => binary_exp(f, *l, *r, Instruction::I32DivS, env, fun_env),
        // ite
        Exp::Ite(b, t, e) => {
            codegen_exp(f, *b, env, fun_env);
            f.instruction(Instruction::If(BlockType::Result(ValType::I32)));
            codegen_exp(f, *t, env, fun_env);
            f.instruction(Instruction::Else);
            codegen_exp(f, *e, env, fun_env);
            f.instruction(Instruction::End);
        }
        // call
        Exp::Call(fun_name, args) => {
            match fun_env.get(&fun_name) {
                Some(id) => {
                    // generate each arguments in order
                    for arg in args {
                        codegen_exp(f, arg, env, fun_env);
                    }
                    f.instruction(Instruction::Call(*id));
                }
                _ => panic!("codegen: try to call inexistent function name"),
            };
        }
        // var
        Exp::Var(str) => {
            match env.get(&str) {
                Some(var) => f.instruction(Instruction::LocalGet(*var)),
                _ => panic!("codegen: try to call inexistent variable name"),
            };
        }
        Exp::Let(str, val, body) => {
            let var = match env.get(&str) {
                Some(value) => value.clone(),
                None => env.add(&str),
            };
            codegen_exp(f, *val, env, fun_env);
            f.instruction(Instruction::LocalSet(var));
            codegen_exp(f, *body, env, fun_env)
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

    fn custom_fun_format(fun_sign: &str, body: &str, index: char) -> String {
        let body: Vec<&str> = body.split("\n").collect();
        [
            &format!("(func (;{};) (type {}) {}\n    ", index, index, fun_sign),
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
                &vec![],
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
                &vec![],
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
                &vec![],
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
                d_fun("main", &vec![], Num(1), 0),
                d_fun("foo", &vec![], Num(2), 0)
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

    #[test]
    fn call_fun() {
        assert_eq!(
            print_bytes(&codegen(&vec!(
                d_fun("main", &vec![], d_call("foo", &vec![]), 0),
                d_fun("foo", &vec![], Num(2), 0)
            )))
            .unwrap(),
            [
                "(module\n  \
                (type (;0;) (func (result i32)))\n  ",
                "(type (;1;) (func (result i32)))\n  ",
                &function_format("call 1", '0'),
                &function_format("i32.const 2", '1'),
                "(export \"main\" (func 0))\n)"
            ]
            .concat()
        )
    }

    #[test]
    fn fun_param() {
        assert_eq!(
            print_bytes(&codegen(&vec!(
                d_fun("main", &vec![], d_call("foo", &vec!(Num(1))), 0),
                d_fun("foo", &vec!("num".into()), d_var("num"), 0)
            )))
            .unwrap(),
            [
                "(module\n  \
                (type (;0;) (func (result i32)))\n  ",
                "(type (;1;) (func (param i32) (result i32)))\n  ",
                &function_format(
                    "i32.const 1\n\
                    call 1",
                    '0'
                ),
                &custom_fun_format(
                    "(param i32) (result i32)",
                    "(local i32)\n\
                    local.get 0",
                    '1'
                ),
                "(export \"main\" (func 0))\n)"
            ]
            .concat()
        )
    }
}
