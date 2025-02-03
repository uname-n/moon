// src/main.rs

use std::env;
use std::fs;
use std::process;

use moon::builtins::register_builtins;
use moon::compiler::Compiler;
use moon::lexer::tokenize;
use moon::parser::parse;
use moon::vm::VM;

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

    let mut vm = VM::new();
    register_builtins(&mut vm);

    let mut compiler = Compiler::new();
    compiler.compile_program(&ast);

    match vm.run(&compiler.code, &compiler.constants) {
        Ok(result) => {
            println!("Program result: {}", result);
        }
        Err(e) => {
            match e {
                _ => eprintln!("Runtime error: {}", e),
            }
        }
    }
}
