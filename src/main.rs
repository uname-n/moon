use moon::compiler::Compiler;
use moon::lexer::tokenize;
use moon::parser::parse;
use moon::vm::VM;

fn main() {
    let input = "1 + 2 * 3";
    let tokens = match tokenize(input) {
        Ok(ts) => ts,
        Err(e) => {
            eprintln!("{}", e);
            return;
        }
    };

    let ast = match parse(&tokens) {
        Ok(expr) => expr,
        Err(e) => {
            eprintln!("{}", e);
            return;
        }
    };

    let mut compiler = Compiler::new();
    compiler.compile_expr(&ast);
    println!("Bytecode: {:?}", compiler.code);
    println!("Constants: {:?}", compiler.constants);

    let mut vm = VM::new();
    match vm.run(&compiler.code, &compiler.constants) {
        Ok(result) => println!("Result: {}", result),
        Err(e) => eprintln!("Runtime error: {}", e),
    }
}