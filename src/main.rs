use std::env;
use std::fs;
use std::process;
use moon::compiler::Compiler;
use moon::lexer::tokenize;
use moon::parser::parse;
use moon::vm::VM;
use moon::value::Value;
use moon::builtins::{builtin_print, builtin_type};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <script.moon>", args[0]);
        process::exit(1);
    }
    let filename = &args[1];
    let script = match fs::read_to_string(filename) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading {}: {}", filename, e);
            process::exit(1);
        }
    };
    let tokens = match tokenize(&script) {
        Ok(ts) => ts,
        Err(e) => {
            eprintln!("Lexing error: {}", e);
            process::exit(1);
        }
    };
    let ast = match parse(&tokens) {
        Ok(stmts) => stmts,
        Err(e) => {
            eprintln!("Parsing error: {}", e);
            process::exit(1);
        }
    };
    let mut compiler = Compiler::new();
    compiler.variables.push("print".to_string());
    compiler.variables.push("type".to_string());
    compiler.compile_program(&ast);
    let builtins = vec![
        Value::BuiltinFunction("print".to_string(), builtin_print),
        Value::BuiltinFunction("type".to_string(), builtin_type),
    ];
    let mut vm = VM::new(builtins);
    match vm.run(&compiler.code, &compiler.constants) {
        Ok(result) => println!("{}", result),
        Err(e) => eprintln!("Runtime error: {}", e),
    }
}