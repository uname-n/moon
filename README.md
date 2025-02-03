# Moon

Moon is a modern, concise, and expressive programming language built on a stack-based virtual machine. Designed for clarity and performance, Moon features first-class functions, closures, and robust error handling. Its clean syntax makes it ideal for scripting, rapid prototyping, and building complex applications.

Below is an example script that demonstrates Moon's capabilities in variable declaration, control flow, recursion, and function calls:

```moon
fn factorial(n: number) {
    if n <= 1 {
        return 1
    } else {
        return n * factorial(n - 1)
    }
}

fn fibonacci(n: number) {
    if n <= 1 {
        return n
    } else {
        return fibonacci(n - 1) + fibonacci(n - 2)
    }
}

x: number = 10
fact: number = factorial(x)
print("Factorial of ", x, " is ", fact)

fib10: number = fibonacci(10)
print("Fibonacci(10) is ", fib10)

i: number = 0
sum: number = 0
while (i < 5) {
    sum = sum + i
    i = i + 1
}
print("Sum of first 5 numbers is ", sum)
```