Elang
=====

Elang is a simple language based on C functionalities but with a Rust-like syntax.
The ouput is heavily based on LLVM (4.0).

```
USAGE:
    elang_driver [FLAGS] [OPTIONS] <INPUT>

FLAGS:
        --ast        Dump AST
    -h, --help       Prints help information
        --ir         Dump IR
    -O               Activate optimizations
    -V, --version    Prints version information

OPTIONS:
    -o, --output <FILE>         Output file
    -t, --type <output_type>    Output type [values: check, llvm, run, exec]

ARGS:
    <INPUT>    Input file
```
