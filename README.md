# Rust Simple Calculator
Simple calculator written in Rust. Supports many arithmetic functions, boolean logic, lists, and basic functions/lambdas.

## Value types

The calculator supports four value types:

* Booleans (`true` and `false`)
* Lists (`[1, true, -4.1]`, `[]`, `[sin, cos, tan]`, `[true, false]`)
* String (`"double quoted string"`)
    - string is useless for calculator, but i still add the support 
* Numbers (examples: `1`, `3.2`, `-200`, `1.3333`, `1e-3`, `0xFFFFF`, `0b010011`, `0o777`) 
    - number support radix 2 (binary), 8 (octal), and 16 (hex)
* Functions (pre-defined and user-defined)

## Arithmetic Options

Supports the following operations:

* Arithmetic
    * Addition: `a + b`
    * Subtraction: `a - b`
    * Multiplication: `a * b`
    * Division: `a / b`
    * Rem / Module: `a % b`
    * BitAnd: `a & b`
    * BitOr: `a | b`
    * BitXor: `a ^ b`
    * Shr: `a >> b`
    * Shl: `a << b`
* Relational
    * Equal: `a == b`, `a != b`
    * Compare: `a < b`, `a <= b`, `a > b`, `a >= b`
* Logic
    * Conjunction: `a and b`
    * Disjunction: `a or b`
    * Negation: `not a`
    * Ternary: `a if b else c`


## Defining functions
Functions are first-call citizens and can be used like any other value.
They can be passed around like any other value, making thing like this possible.

```
>>> foo = sin == cos
  false
>>> index = -(sin if foo else cos)(pi)
  1  
>>> [sin, cos][index](pi)
  -1
```


New functions can be defined using the lambda syntax.

```
>>> foo = x => x + 1
```

Alternatively, they can be defined using the following special syntax.

```
>>> foo(x) = x + 1
```

Note that this syntax allows recursive definitions which is not possible with the lamda syntax.

```
>>> factorial(n) = factorial(n - 1) * n if n > 0 else 1
```



## Predefined constants and functions.

Supports the following constants.

* `pi`
* `tau`
* `e`
* `nan`
* `inf`
* `neginf`

The following common mathematical functions are supported.

* `sin(x)`, `cos(x)`, `tan(x)`
* `asin(x)`, `acos(x)`, `atan(x)`
* `ln(x)`, `log10(x)`, `log2(x)`, `log(x, base)`
* `round(x)`, `floor(x)`, `ceil(x)`
* `sqrt(x)`, `exp(x)`, `powf(x, e)`, `pow(x, e)`
* `abs(x)`, `min_num(x, y)`, `max_num(x, y)`

The following Python-like utility functions are included.

* `min(...)`: minimum of arguments.
* `max(...)`: maximum of arguments.
* `rand()`, `rand(stop)`, `rand(start, stop)`: random float (default range is 0.0 to 1.0).
* `range(stop)`, `range(start, stop)`: list `[start, start+1, ..., stop-1, stop]`.
* `map(f, list)`: applies `f` to each element of `list`.
* `sort(list)`: sort elements of `list`
* `length(list)`: length of `list`

The following common printing functions to stdout/stderr are supported.

* `print(value)`: print to stdout
* `println(value)`: print to stdout with new line at the end
* `eprint(value)`: print to stderr
* `eprintln(value)`: print to stderr with new line at the end
* `debug(expr)`: print debug representation for expression

The following common format number functions.

* `binary(value, _bool)`: format number as string in binary format (`0b111`)
* `octal(value, _bool)`: format number as string in octal format (`0o777`)
* `hex(value, _bool)`: format number as string in hex format (`0xFFF`)



## Examples

```
$ cargo run
>>> 1 + 1
 2
>>> factorial(n) = factorial(n - 1) * n if n > 0 else 1
 <function: factorial>
>>> factorial(6)
 720
>>> x = range(5)
 [0, 1, 2, 3, 4]
>>> map(factorial, x)
 [1, 1, 2, 6, 24]
>>> factorial(1000)
 error: stack overflow, call depth cannot exceed 512
```
