# M28 Language Guide

M28 is a Lisp interpreter with Python-inspired syntax and features, combining the expressiveness of Lisp with familiar Python semantics. This guide provides a comprehensive overview of the language.

## Contents
1. [Introduction](#introduction)
2. [Installation](#installation)
3. [Usage](#usage)
4. [Core Features](#core-features)
5. [Data Types](#data-types)
6. [Variables and Functions](#variables-and-functions)
7. [Control Flow](#control-flow)
8. [Collections](#collections)
9. [Modules](#modules)
10. [Object-Oriented Programming](#object-oriented-programming)
11. [Error Handling](#error-handling)
12. [Advanced Features](#advanced-features)

## Introduction

M28 blends Lisp's parenthesized syntax with Python's semantics and naming conventions. It features a REPL (Read-Eval-Print Loop) for interactive use and executes code from files.

### Key Differences from Standard Lisp

1. **Python-like Names**: Functions use `snake_case` rather than `kebab-case`
2. **Assignment**: Uses `=` instead of `set!` or `define`
3. **Truthiness**: Follows Python's truthiness rules (`0`, `""`, `None`, empty lists are falsy)
4. **Data Types**: Includes Python types like dictionaries and sets
5. **Boolean Syntax**: Uses `True` and `False` instead of `#t` and `#f`

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
make test
```

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
- **Sets**: `(set 1 2 3 4)`
- **Tuples**: `(tuple 1 2 3 4)`

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
(def square (lambda (x) (* x x)))

# Use it
(square 5)  # Returns: 25

# Or inline
((lambda (x) (* x x)) 5)  # Returns: 25
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

# Break and continue
(for i (range 10)
  (if (== i 5)
    (continue))
  (if (== i 8)
    (break))
  (print i))
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

### Set Operations

```lisp
# Create a set
(= s (set 1 2 3 4))

# Check membership
(in 2 s)  # Returns: True

# Set operations
(= a (set 1 2 3))
(= b (set 2 3 4))
(set_union a b)      # Returns: (1 2 3 4)
(set_intersection a b)  # Returns: (2 3)
(set_difference a b)    # Returns: (1)
```

## Modules

### Importing Modules

```lisp
# Import entire module
(import "math")
(print math.pi)  # Access module attribute

# Import specific module
(import "examples/modules/simple_module")
(print (simple_module.add 5 3))  # Call module function
```

### Creating Modules

Modules are simply M28 files with function and variable definitions:

```lisp
# In math_utils.m28
(def PI 3.14159)

(def (square x)
  (* x x))

(def (cube x)
  (* x x x))
```

These can then be imported:

```lisp
(import "math_utils")
(print math_utils.PI)
(print (math_utils.square 5))
```

## Object-Oriented Programming

M28 implements objects using a closure-based approach:

### Creating Objects with Methods

```lisp
(def make-counter (lambda (initial-value)
  (= count initial-value)
  
  (def (get-count)
    count)
    
  (def (increment)
    (= count (+ count 1))
    count)
    
  (def (decrement)
    (= count (- count 1))
    count)
    
  (dict 
    "get-count" get-count
    "increment" increment
    "decrement" decrement)))

# Create an instance
(= counter (make-counter 0))

# Call methods
(print ((get counter "get-count")))  # 0
(print ((get counter "increment")))  # 1
(print ((get counter "increment")))  # 2
(print ((get counter "decrement")))  # 1
```

### Using Dot Notation

With M28's dot notation support:

```lisp
(= counter (make-counter 0))
(print (counter.get-count counter))  # 0
(print (counter.increment counter))  # 1
```

## Error Handling

```lisp
# Basic try-except
(try
  (/ 10 0)
  (except ZeroDivisionError
    (print "Cannot divide by zero")))

# With exception variable
(try
  (/ 10 0)
  (except ZeroDivisionError as err
    (print "Error:" err)))

# With finally
(try
  (print "Attempting operation")
  (except Exception
    (print "Handling error"))
  (finally
    (print "Cleanup code")))

# Assertions
(assert (> x 0) "x must be positive")
```

## Advanced Features

### List Comprehensions

```lisp
# Create a list of squares
(= squares 
  (for (x (range 10))
    (* x x)))
```

### Higher-Order Functions

```lisp
# Map
(map (lambda (x) (* x 2)) (list 1 2 3 4))  # Returns: (2 4 6 8)

# Filter
(filter (lambda (x) (> x 5)) (list 1 8 3 6 2 9))  # Returns: (8 6 9)

# Reduce
(reduce + (list 1 2 3 4 5))  # Returns: 15
```

### Context Managers (When Implemented)

```lisp
# File I/O with context manager
(with (open "example.txt" "r") as file
  (print (file.read)))
```

### Generators (When Implemented)

```lisp
(def (count-up-to n)
  (= i 0)
  (while (< i n)
    (yield i)
    (= i (+ i 1))))

(for x (count-up-to 5)
  (print x))
```

---

For detailed examples and more advanced usage, see the example files in the `/examples` directory.