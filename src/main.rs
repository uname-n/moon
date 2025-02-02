use moon::compiler::Compiler;
use moon::lexer::tokenize;
use moon::parser::parse;
use moon::vm::{VM, VMError};
use moon::value::Value;

fn builtin_print(args: &[Value]) -> Result<Value, VMError> {
    for v in args {
        print!("{} ", v);
    }
    println!();
    Ok(Value::Number(0.0))
}

fn builtin_type(args: &[Value]) -> Result<Value, VMError> {
    if args.is_empty() {
        return Ok(Value::Str("void".into()));
    }
    let s = match &args[0] {
        Value::Number(_) => "number",
        Value::Bool(_) => "bool",
        Value::Function(_) => "function",
        Value::BuiltinFunction(_, _) => "builtin",
        Value::Str(_) => "string",
    }.to_string();
    Ok(Value::Str(s))
}

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
        print(type(result))
    "#;

    // 1) Lex
    let tokens = match tokenize(input) {
        Ok(ts) => ts,
        Err(e) => {
            eprintln!("{}", e);
            return;
        }
    };

    // 2) Parse
    let ast = match parse(&tokens) {
        Ok(stmts) => stmts,
        Err(e) => {
            eprintln!("{}", e);
            return;
        }
    };

    // 3) Compile (with an empty variables list at first)
    let mut compiler = Compiler::new();
    // Manually add built-in names so they're at slots 0 and 1
    compiler.variables.push("print".to_string());
    compiler.variables.push("type".to_string());

    compiler.compile_program(&ast);

    // 4) Prepare builtins for the VM
    let builtins = vec![
        Value::BuiltinFunction("print".to_string(), builtin_print),
        Value::BuiltinFunction("type".to_string(), builtin_type),
    ];

    // 5) Run
    let mut vm = VM::new(builtins);
    match vm.run(&compiler.code, &compiler.constants) {
        Ok(result) => println!("Result: {}", result),
        Err(e) => eprintln!("Runtime error: {}", e),
    }
}