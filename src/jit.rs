use crate::bytecode::Instruction;
use crate::value::Value;

pub struct JitCompiler;

impl JitCompiler {
    pub fn new() -> Self {
        Self
    }

    pub fn compile(&self, _code: &[Instruction], _constants: &[Value]) -> Option<fn(&[Value]) -> Result<Value, ()>> {
        None
    }
}