use crate::value::Value;
use crate::vm::{VMError, VM};
use num_bigint::BigInt;

pub fn builtin_print(args: &[Value]) -> Result<Value, VMError> {
    for arg in args {
        print!("{} ", arg);
    }
    println!();
    Ok(Value::Integer(BigInt::from(0)))
}

pub fn builtin_type(args: &[Value]) -> Result<Value, VMError> {
    if args.is_empty() {
        return Ok(Value::Str("void".to_string()));
    }
    let s = match &args[0] {
        Value::Integer(_) => "int",
        Value::Float(_) => "float",
        Value::Bool(_) => "bool",
        Value::Function(_) => "function",
        Value::BuiltinFunction(_, _) => "builtin",
        Value::Str(_) => "string",
    };
    Ok(Value::Str(s.to_string()))
}

pub fn register_builtins(vm: &mut VM) {
    vm.define_global("print", Value::BuiltinFunction("print".to_string(), builtin_print));
    vm.define_global("type", Value::BuiltinFunction("type".to_string(), builtin_type));
}