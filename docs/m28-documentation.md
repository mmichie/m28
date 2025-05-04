# M28: A Pythonic Lisp Language

M28 is a Lisp interpreter with Python-inspired syntax and features, combining the expressiveness of Lisp with familiar Python semantics. It features a REPL (Read-Eval-Print Loop) for interactive use and executes code from files.

## Contents
1. [Installation](#installation)
2. [Usage](#usage)
3. [Language Guide](#language-guide)
4. [Core Features](#core-features)
5. [Data Types](#data-types)
6. [Variables and Functions](#variables-and-functions)
7. [Control Flow](#control-flow)
8. [Collections](#collections)
9. [Error Handling](#error-handling)
10. [Examples](#examples)

## Installation

To use M28, follow these steps:

1. Ensure you have Go installed on your system
2. Clone the repository: `git clone https://github.com/mmichie/m28.git`
3. Navigate to the project directory: `cd m28`
4. Build the project: `make build`

## Usage

### REPL Mode

To start the interactive REPL:

```bash
make run
# or
./bin/m28
```

### Execute a File

To run a script:

```bash
./bin/m28 path/to/your/file.m28
```

### Run Tests

To execute the test suite:

```bash
make m28-test
```

## Language Guide

M28 blends Lisp's parenthesized syntax with Python's semantics and naming conventions.

### Key Differences from Standard Lisp

1. **Python-like Names**: Functions use `snake_case` rather than `kebab-case`
2. **Assignment**: Uses `=` instead of `set!` or `define`
3. **Truthiness**: Follows Python's truthiness rules (`0`, `""`, `None`, empty lists are falsy)
4. **Data Types**: Includes Python types like dictionaries and sets
5. **Boolean Syntax**: Uses `True` and `False` instead of `#t` and `#f`

## Core Features

M28 supports these fundamental operations:

### Arithmetic
- Addition: `(+ 1 2 3)` → `6`
- Subtraction: `(- 10 5)` → `5`
- Multiplication: `(* 2 3 4)` → `24`
- Division: `(/ 10 2)` → `5`
- Modulo: `(% 10 3)` → `1`
- Power: `(** 2 3)` → `8`

### Comparison
- Equality: `(== 5 5)` → `True`
- Inequality: `(!= 5 6)` → `True`
- Greater/Less: `(> 5 3)`, `(< 3 5)` → `True`
- Greater/Less Equal: `(>= 5 5)`, `(<= 5 5)` → `True`

### Logical Operations
- And: `(and condition1 condition2)`
- Or: `(or condition1 condition2)`
- Not: `(not condition)`

## Data Types

M28 supports the following data types:

### Primitive Types
- **Numbers**: `42`, `3.14`
- **Strings**: `"Hello, World!"`
- **Symbols**: `variable_name`
- **Booleans**: `True`, `False`
- **None**: `None`

### Collection Types
- **Lists**: `(list 1 2 3 4)`
- **Dictionaries**: `{"key": value, "key2": value2}`
- **Sets**: (Through functions)

## Variables and Functions

### Variable Assignment

```lisp
(= x 10)              # Assign 10 to x
(= y "hello")         # Assign "hello" to y
(= z (+ x 5))         # Assign the result of (+ x 5) to z
```

### Function Definition

```lisp
# Define a function to add two numbers
(def (add_two_numbers a b)
  (+ a b))

# Call the function
(add_two_numbers 5 3)  # Returns: 8
```

### Lambda Functions

```lisp
# Anonymous function
(= square (lambda (x) (* x x)))

# Use it
(square 5)  # Returns: 25
```

## Control Flow

### Conditionals

```lisp
# Simple if
(if condition
    true_expression
    false_expression)

# If-elif-else pattern
(if condition1
    result1
    (if condition2
        result2
        result3))
```

### Loops

```lisp
# For loop
(for item items
  (print item))

# While loop
(while condition
  expression1
  expression2)
```

## Collections

### List Operations

```lisp
# Create a list
(= numbers (list 1 2 3 4 5))

# Get list element by index
(get numbers 0)  # Returns: 1

# Concatenate lists
(+ (list 1 2) (list 3 4))  # Returns: (1 2 3 4)
```

### Dictionary Operations

```lisp
# Create a dictionary
(= person {"name": "John", "age": 30})

# Dictionary with dict function
(= person2 (dict "name" "Jane" "age" 25))

# Access dictionary values
(get person "name")  # Returns: "John"

# With default value for missing keys
(get person "address" "Unknown")  # Returns: "Unknown"
```

### Dictionary as Keyword Arguments

```lisp
# Print with custom parameters
(print "Hello" "World" {"sep": "-", "end": "!"})
# Prints: Hello-World!
```

## Error Handling

```lisp
# Basic try-except
(try
  (/ 10 0)
  (except ZeroDivisionError
    (print "Cannot divide by zero")))

# Assertions
(assert (> x 0) "x must be positive")
```

## Examples

### Example 1: Factorial

```lisp
(def (factorial n)
  (if (<= n 1)
    1
    (* n (factorial (- n 1)))))

(print (factorial 5))  # Outputs: 120
```

### Example 2: Dictionary Usage

```lisp
# Create dictionary with literal syntax
(= student {"name": "Alice", "grade": "A", "courses": (list "Math" "Science")})

# Access and modify
(print (get student "name"))  # Outputs: Alice
(= student "grade" "A+")      # Update grade

# Custom print with dictionary parameters
(print "Courses:" (get student "courses") {"sep": " -> "})
```

---

For detailed examples and more advanced usage, see the example files in the `examples/` directory.