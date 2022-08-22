use compiler_rust::compile;
use std::fs::File;
use std::io::prelude::Read;
use wasmi::{ImportsBuilder, ModuleInstance, NopExternals, RuntimeValue};

fn main() {
    // get file
    let filename = std::env::args().nth(1).expect("no filename given");
    // read from file
    let mut file = File::open(format!("files/{}", filename)).expect("failed to open file");
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("failed to read file as string");

    // compile file content
    match compile(&contents) {
        Ok(wasm_binary) => {
            // Load wasm binary and prepare it for instantiation.
            let module = wasmi::Module::from_buffer(&wasm_binary).expect("failed to load wasm");

            // Instantiate a module with empty imports and
            // assert that there is no `start` function.
            let instance = ModuleInstance::new(&module, &ImportsBuilder::default())
                .expect("failed to instantiate wasm module")
                .assert_no_start();

            // Finally, invoke the exported function "test" with no parameters
            // and empty external function executor.
            let result = instance
                .invoke_export("main", &[], &mut NopExternals)
                .expect("failed to execute export");

            match result {
                Some(res) => match res {
                    RuntimeValue::I32(n) => println!("{:?}", n),
                    _ => println!("{:?}", res),
                },
                None => println!("failed to run wasm file"),
            }
        }
        Err(errors) => {
            println!("error: could not compile `{}`:", filename);
            for err in errors {
                println!("-> {}", err);
            }
        }
    }
}
