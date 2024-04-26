# Simple compiler in Rust

Simple rust compiler with a backend targeting [WebAssembly](https://webassembly.org/).

## Overview

- `scanner`: Converts source code into tokens.
- `syntax`: Contains language constructs.
- `parser`: Builds an abstract syntax tree (AST) from tokens.
- `typecheck`: Ensures type correctness of the AST.
- `codegen`: Generates WebAssembly code from on the AST.

## Libraries used

- [wasm-tools](https://github.com/bytecodealliance/wasm-tools) (by Bytecode Alliance) 
  - wasm-encoder for code generation.
  - wasmi for the interpreter.
  - helper functions.
- [pretty_assertions](https://github.com/rust-pretty-assertions/rust-pretty-assertions) (for colored test diffs)

## Commands

- Compile and run a `.lg` file:

```bash
cargo run -- filename.lg
```

- Run all the tests (nocapture is used for scanner test):

```bash
cargo test -- --nocapture
```

## Basics

- Every programs must have a `main` entry point function, returning an `int`.
- Numeric types are 32-bit integers, booleans are compiled as `0` or `1`.
- Types: `int` and `bool`.
- `let` and `=` body are defined by the next underneath line, *at least one line break*.
- Everything except functions are expressions.

## Example

- Returns `3` if `is_null` is true, otherwise returns `3`:

```
int main()
    let is_null = true
    let var = (foo(is_null, 5*2, 0-10) + 1) * 2
    var = var + 2 / 2
    var
end

int foo(bool null, int res1, int res2)
    if null then
        0
    else
        let boolean = if fee() then res1 else res2 end
        boolean
    end
end

bool fee()
    (1 < 2 & 4-2 > 3) == false & true 
end
```
