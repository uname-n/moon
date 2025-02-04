use std::env;
use std::fs;
use std::process;

use moon::builtins::register_builtins;
use moon::compiler::Compiler;
use moon::disassembler;
use moon::lexer::tokenize;
use moon::parser::parse;
use moon::profiler::profile;
use moon::vm::VM;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <script.moon> [--disassemble] [--profile]", args[0]);
        process::exit(1);
    }
    let disassemble = args.contains(&"--disassemble".to_string());
    let profile_execution = args.contains(&"--profile".to_string());
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
    compiler.compile_program(&ast);
    if disassemble {
        println!("{}", disassembler::disassemble(&compiler.code, &compiler.constants));
    }
    let mut vm = VM::new();
    register_builtins(&mut vm);
    let result = if profile_execution {
        let (res, duration) = profile(|| vm.run(&compiler.code, &compiler.constants));
        println!("Execution time (μs): {}", duration);
        res
    } else {
        vm.run(&compiler.code, &compiler.constants)
    };
    match result {
        Ok(_result) => {},
        Err(e) => eprintln!("Runtime error: {}", e),
    }
}