# Moon

Moon is a small, statically-typed, interpreted language with first-class functions, integer and floating-point arithmetic, and closures. Inspired by modern scripting and functional languages, Moon provides these features:

• Static type hints: Variables and function parameters can be annotated with types (int, float, etc.).  
• First-class functions: Declare and call both user-defined and built-in functions. Functions can capture variables in closures.  
• Control flow constructs: if, else, while, logical and/or, plus comparison operators for conditionals.  
• Virtual Machine: Executes compiled bytecode, manages a call stack, local/global variables, and tail calls.  
• Compiler: Converts AST nodes into a bytecode representation, applies constant folding, dead code elimination, and simple inlining optimizations.  
• JIT stub: A placeholder for potential ahead-of-time or just-in-time compilation.  
• Disassembler: Allows optional bytecode printing (helpful for debugging).  
• Profiler: Times program execution, print results in microseconds.  

USAGE

To run a Moon script:
    moon <script.moon>

Optional flags:
    --disassemble  Show compiled bytecode.  
    --profile      Measure execution time and print results.

EXAMPLE

Given a script:
-------------------------
x: int = 5
while (x < 10) {
    x = x + 1
}
print(x)
-------------------------
• This code declares a global integer x, increments it in a loop until it reaches 10, then prints the result.
• The compiler emits bytecode for variable initialization, loop checks, assignments, and function calls. 
• The VM executes the bytecode, handling arithmetic, comparisons, and stack operations.