use compiler_rust::compile;
use wasmi::{ImportsBuilder, ModuleInstance, NopExternals, RuntimeValue};

fn main() {
    // compile file content
    let wasm_binary = compile("program string");

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
