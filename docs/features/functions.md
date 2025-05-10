# Functions in M28

This document explains how to define and use functions in M28, including named functions, anonymous functions (lambdas), closures, and higher-order functions.

## Contents
1. [Basic Function Definition](#basic-function-definition)
2. [Calling Functions](#calling-functions)
3. [Anonymous Functions (Lambda)](#anonymous-functions-lambda)
4. [Closures](#closures)
5. [Higher-Order Functions](#higher-order-functions)
6. [Recursion](#recursion)
7. [Function Parameters](#function-parameters)
8. [Default Parameters](#default-parameters)
9. [Examples](#examples)

## Basic Function Definition

Functions in M28 are defined using the `def` special form:

```lisp
(def (function-name param1 param2)
  body-expression1
  body-expression2
  ...)
```

Example:

```lisp
# Simple function to add two numbers
(def (add x y)
  (+ x y))

# Function to calculate the area of a circle
(def (circle-area radius)
  (* 3.14159 (* radius radius)))
```

Functions return the value of the last expression evaluated unless a specific return statement is used.

## Calling Functions

To call a function, place it at the beginning of a list followed by its arguments:

```lisp
(add 5 3)  # Returns: 8
(circle-area 5)  # Returns: 78.53975
```

## Anonymous Functions (Lambda)

Anonymous functions (lambdas) can be created using the `lambda` special form:

```lisp
(lambda (param1 param2) body-expression)
```

Example:

```lisp
# Create an anonymous function for squaring a number
(= square (lambda (x) (* x x)))

# Use the lambda directly
(square 5)  # Returns: 25

# Or use it inline
((lambda (x) (* x x)) 5)  # Returns: 25
```

Lambda functions are useful when you need a short function that doesn't need a name, or when passing functions as arguments.

## Closures

M28 supports closures, which are functions that capture variables from their enclosing scope:

```lisp
# Create a counter function that remembers its state
(def (make-counter)
  (= count 0)
  (lambda ()
    (= count (+ count 1))
    count))

# Create two independent counters
(= counter1 (make-counter))
(= counter2 (make-counter))

(counter1)  # Returns: 1
(counter1)  # Returns: 2
(counter2)  # Returns: 1 (independent state)
```

Closures are powerful for creating functions with private state.

## Higher-Order Functions

Higher-order functions are functions that take other functions as arguments or return functions:

```lisp
# Function that takes a function as an argument
(def (apply-twice fn x)
  (fn (fn x)))

(apply-twice (lambda (x) (* x 2)) 3)  # Returns: 12
(apply-twice square 3)  # Returns: 81

# Function that returns a function
(def (multiplier n)
  (lambda (x) (* n x)))

(= double (multiplier 2))
(= triple (multiplier 3))

(double 5)  # Returns: 10
(triple 5)  # Returns: 15
```

## Recursion

M28 supports recursive functions where a function calls itself:

```lisp
# Calculate factorial recursively
(def (factorial n)
  (if (<= n 1)
    1
    (* n (factorial (- n 1)))))

(factorial 5)  # Returns: 120
```

For deep recursion, consider using tail recursion to avoid stack overflow:

```lisp
# Tail-recursive factorial
(def (factorial-tail n acc)
  (if (<= n 1)
    acc
    (factorial-tail (- n 1) (* n acc))))

(def (factorial n)
  (factorial-tail n 1))

(factorial 5)  # Returns: 120
```

## Function Parameters

Functions can accept a fixed number of parameters:

```lisp
(def (greet name)
  (print "Hello," name))

(greet "Alice")  # Prints: Hello, Alice
```

## Default Parameters

M28 may support default parameters through wrapper functions:

```lisp
# Function with optional parameter
(def (greet name)
  (if (== name None)
    (greet "Guest")
    (print "Hello," name)))

(greet "Alice")  # Prints: Hello, Alice
(greet None)     # Prints: Hello, Guest
```

Alternatively, you can use dictionaries for keyword arguments with defaults:

```lisp
(def (configure options)
  (= debug (get options "debug" False))
  (= timeout (get options "timeout" 30))
  (= retries (get options "retries" 3))
  
  (print "Config: debug =" debug "timeout =" timeout "retries =" retries))

(configure {"debug": True})  # Uses default for timeout and retries
```

## Examples

### Example 1: Function Composition

```lisp
# Define the composition of two functions
(def (compose f g)
  (lambda (x) (f (g x))))

(= double (lambda (x) (* x 2)))
(= increment (lambda (x) (+ x 1)))

# Compose the functions
(= double-then-increment (compose increment double))
(= increment-then-double (compose double increment))

(double-then-increment 3)  # 3 * 2 + 1 = 7
(increment-then-double 3)  # (3 + 1) * 2 = 8
```

### Example 2: Closure for Memoization

```lisp
# Create a memoized version of the fibonacci function
(def (memoize fn)
  (= cache (dict))
  (lambda (n)
    (if (dict.has_key cache n)
      (get cache n)
      (begin
        (= result (fn n))
        (= cache n result)
        result))))

# Define the fibonacci function
(def (fib n)
  (if (<= n 1)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

# Create memoized version
(= memo-fib (memoize fib))

# This is much faster for larger values
(memo-fib 20)
```

### Example 3: Function as Protocol

```lisp
# Define functions that implement a protocol
(def (make-shape type)
  (if (== type "circle")
    (lambda (radius) 
      {"area": (* 3.14159 (* radius radius)),
       "perimeter": (* 2 3.14159 radius)})
    (if (== type "rectangle")
      (lambda (width height)
        {"area": (* width height),
         "perimeter": (* 2 (+ width height))})
      (lambda args
        {"error": "Unknown shape type"}))))

# Create specific shape functions
(= circle (make-shape "circle"))
(= rectangle (make-shape "rectangle"))

# Use them
(print (circle 5))  # {"area": 78.53975, "perimeter": 31.4159}
(print (rectangle 4 6))  # {"area": 24, "perimeter": 20}
```

## Implementation Notes

- Functions in M28 are first-class values that can be assigned, passed as arguments, and returned
- Closures properly capture their environment, maintaining lexical scoping
- M28's function implementation includes proper tail call optimization for deep recursion (if implemented)
- The evaluation of function arguments follows applicative-order evaluation (arguments are evaluated before the function call)