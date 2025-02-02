// src/main.rs

use moon::compiler::Compiler;
use moon::lexer::tokenize;
use moon::parser::parse;
use moon::vm::VM;

fn main() {
    let input = r#"
        fn fib(n:number) {
            if n == 0 {
                return 0
            } else {
                if n == 1 {
                    return 1
                } else {
                    return fib(n - 1) + fib(n - 2)
                }
            }
        }
        result:number = fib(8)
        print(result)
    "#;

    let tokens = match tokenize(input) {
        Ok(ts) => ts,
        Err(e) => {
            eprintln!("{}", e);
            return;
        }
    };

    let ast = match parse(&tokens) {
        Ok(stmts) => stmts,
        Err(e) => {
            eprintln!("{}", e);
            return;
        }
    };

    let mut compiler = Compiler::new();
    compiler.compile_program(&ast);
    println!("Bytecode: {:?}", compiler.code);
    println!("Constants: {:?}", compiler.constants);

    // Create a globals vector (one slot per variable).
    let globals = vec![/* preloaded globals (if any) */];
    let mut vm = VM::new(globals);
    match vm.run(&compiler.code, &compiler.constants) {
        Ok(result) => println!("Result: {}", result),
        Err(e) => eprintln!("Runtime error: {}", e),
    }
}
