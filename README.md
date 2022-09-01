# Rewrite in Rust

The goal of this project is to reproduce the compiler in Rust. \
The compiler backend is [WebAssembly](https://webassembly.org/).

## Librairies used

- [wasm-tools](https://github.com/bytecodealliance/wasm-tools) (bytecodealliance) 
  - wasm-encoder for code generation 
  - wasmi for the interpreter
  - helpers
- [pretty_assertions](https://github.com/rust-pretty-assertions/rust-pretty-assertions) (for colored test diffs)

## Commands

- run a `.lg` file
```bash
cargo run -- filename.lg
```

- run all tests (nocapture should be used for scanner test)
```bash
cargo test -- --nocapture
```

## Basics

- All programs must have a `main` entry point function, it has to be type `int`
- All numerics are 32 bit integer, boolean are compiled as `0` or `1` 
- There are 2 types: `int` and `bool`
- `let` and `=` body are defined by the next line underneath *at least one line break*
- Everything except definition (function) are expressions


## Example

- return `23`, when `is_null` is true, return `3`
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
