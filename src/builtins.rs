use crate::vm::VMError;
use crate::value::Value;

pub fn builtin_print(args: &[Value]) -> Result<Value, VMError> {
    for arg in args {
        print!("{} ", arg);
    }
    println!();
    Ok(Value::Number(0.0))
}

pub fn builtin_type(args: &[Value]) -> Result<Value, VMError> {
    if args.is_empty() {
        return Ok(Value::Str("void".to_string()));
    }
    let s = match &args[0] {
        Value::Number(_) => "number",
        Value::Bool(_) => "bool",
        Value::Function(_) => "function",
        Value::BuiltinFunction(_, _) => "builtin",
        Value::Str(_) => "string",
    }
    .to_string();
    Ok(Value::Str(s))
}