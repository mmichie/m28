# Pythonic Lisp: Language Guide and Reference

## Introduction
Pythonic Lisp is a unique language that combines the syntax of Lisp with semantics inspired by Python. This document serves as both a comprehensive guide for beginners and a reference for the language design and implementation.

## 1. Basic Syntax

Pythonic Lisp uses parentheses to group expressions, similar to traditional Lisp:

```lisp
(function-name argument1 argument2 ...)
```

## 2. Data Types

Pythonic Lisp supports several core data types:

- Numbers: `42`, `3.14`
- Strings: `"Hello, World!"`
- Symbols: `my-variable`
- Lists: `(1 2 3 4)`
- Booleans: `True`, `False`
- None: `None`

## 3. Variables and Functions

### Variable Assignment

```lisp
(def x 10)
```

### Function Definition

```lisp
(def (greet name) 
    (print "Hello," name))
```

### Lambda Functions (Anonymous)

```lisp
(def square (lambda (x) (* x x)))
```

## 4. Control Structures

### Conditional Execution

```lisp
(if (> x 0)
    (print "Positive")
    (print "Non-positive"))

;; With elif and else
(if condition1
    (do_something1)
    (elif condition2
        (do_something2)
        (else
            (do_something_else))))
```

### Loops

```lisp
;; For loop
(for x (range 5)
    (print x))

;; While loop
(while (< count 10)
    (print count)
    (def count (+ count 1)))

;; Break and continue
(for i (range 0 10)
    (if (== i 5)
        (continue))
    (print i)
    (if (== i 7)
        (break)))
```

## 5. Lists and Collections

### List Operations

```lisp
(def numbers (list 1 2 3 4 5))
(print (first numbers))
(print (rest numbers))
```

### List Comprehensions

```lisp
(def squares
    (for (x (range 10))
        (* x x)))
```

## 6. Higher-Order Functions

```lisp
(def numbers (list 1 2 3 4 5))
(def doubled (map (lambda (x) (* x 2)) numbers))
(def evens (filter (lambda (x) (== 0 (% x 2))) numbers))
(def sum (reduce + numbers))
```

## 7. Exception Handling

Pythonic Lisp provides a comprehensive exception handling system with `try`, `except`, `finally`, and `raise`:

### Basic Exception Handling

The simplest form of exception handling uses a `try` block with a generic `except` clause:

```lisp
(try
    (/ 10 0)  ; This will cause a division by zero error
    (except 
        (print "An error occurred")))
```

### Typed Exceptions

You can catch specific exception types using typed except clauses:

```lisp
(try
    (/ 10 0)
    (except ZeroDivisionError
        (print "Cannot divide by zero"))
    (except
        (print "Some other error occurred")))
```

### Accessing Exception Objects

Use the `as` keyword to bind the exception to a variable:

```lisp
(try
    (raise ValueError "The value is invalid")
    (except ValueError as e
        (print "Caught error:" e)))
```

### Finally Blocks

Use `finally` blocks to execute cleanup code, regardless of whether an exception occurred:

```lisp
(try
    (open-file "data.txt")
    (process-data)
    (except IOError
        (print "Error processing file"))
    (finally
        (close-file)))  ; This always executes
```

### Raising Exceptions

There are multiple ways to raise exceptions:

```lisp
; Raise a generic exception
(raise "Error message")

; Raise a specific exception type
(raise ValueError "Invalid value")

; Create custom exception types
(raise DatabaseError "Connection failed")
```

### Assertion Errors

Use assertions to validate conditions:

```lisp
; This raises an AssertionError if x is not positive
(assert (> x 0) "Value must be positive")
```

### Standard Exception Types

Pythonic Lisp includes standard exception types similar to Python:

- `Exception`: Base exception type
- `ValueError`: Invalid value
- `TypeError`: Wrong type
- `IndexError`: Index out of range
- `KeyError`: Key not found
- `NameError`: Name not defined
- `ZeroDivisionError`: Division by zero
- `AssertionError`: Assertion failed
- `RuntimeError`: Runtime error
- `IOError`: Input/output error
- `FileNotFoundError`: File not found

You can also create custom exception types by simply using a new name with the `raise` statement.

## 8. Modules and Imports

```lisp
(import math)
(print (math.sin (/ math.pi 2)))

(from module import something as alias)
```

## 9. Classes and Objects

Pythonic Lisp implements objects using a closure-based approach. This provides proper encapsulation with private state and public methods.

### Basic Class Definition

```lisp
(class Dog
    ; Instance variables with default values
    (= name "Unnamed")
    
    ; Constructor method
    (def (init self given-name)
        (= name given-name))
        
    ; Regular methods
    (def (bark self)
        (print (+ name " says Woof!"))))

; Create an instance
(= my-dog (Dog "Buddy"))

; Call a method
((get my-dog "bark"))
```

### More Complex Class Example

```lisp
(class Point
    ; Instance variables with default values
    (= x 0)
    (= y 0)

    ; Constructor
    (def (init self initial-x initial-y)
        (= x initial-x)
        (= y initial-y))

    ; Methods
    (def (move self dx dy)
        (= x (+ x dx))
        (= y (+ y dy)))

    (def (distance self other)
        (= other-x ((get other "get-x")))
        (= other-y ((get other "get-y")))
        (sqrt (+ (** (- x other-x) 2)
                 (** (- y other-y) 2))))
                 
    ; Accessors
    (def (get-x self) x)
    (def (get-y self) y))

; Create instances
(= p1 (Point 0 0))
(= p2 (Point 3 4))

; Call methods
((get p1 "move") p1 1 1)
(print ((get p1 "distance") p1 p2))
```

### Implementation Notes

Classes in Pythonic Lisp are implemented as factory functions that create dictionaries of methods. Each instance has its own private state thanks to lexical closures. Methods are stored in the instance dictionary and can be accessed using the `get` function.

The class syntax is syntactic sugar that generates a factory function pattern like:

```lisp
(def (make-counter initial-count)
  ; Private instance state
  (= count initial-count)
  
  ; Method implementations
  (def (get-count)
    count)
  
  (def (increment)
    (= count (+ count 1))
    count)
  
  ; Return dictionary of methods
  (= methods (dict
    "get-count" get-count
    "increment" increment))
    
  methods)
```

## 10. Context Managers and File I/O

Pythonic Lisp implements the context manager protocol, which provides a clean way to manage resources that need setup and cleanup, like file handles.

### Using with Statements

The `with` statement ensures that resources are properly managed:

```lisp
(with context-expression as variable
    body-expressions...)
```

### File Context Manager

The most common use of context managers is for file I/O:

```lisp
; Writing to a file
(with (open "example.txt" "w") as file
    (file.write "Hello, Pythonic Lisp!"))

; Reading from a file
(with (open "example.txt" "r") as file
    (print (file.read)))
```

### Automatic Cleanup

Context managers automatically handle cleanup, even when exceptions occur:

```lisp
(try
    (with (open "file.txt" "r") as file
        (process-file file)
        (if (error-condition)
            (raise "Something went wrong")))
    (except Exception as e
        (print "Error processing file:" e)))
; The file is still closed, even if an exception was raised
```

### Context Manager Protocol

Objects can implement the context manager protocol by providing `__enter__` and `__exit__` methods:

```lisp
(class MyContextManager
    (def (init self)
        (= self.resource None))
        
    (def (__enter__ self)
        (= self.resource (acquire-resource))
        self.resource)
        
    (def (__exit__ self exc)
        (release-resource self.resource)
        None))
```

## 11. Additional Features

### Scope Management

```lisp
(def my_function ()
    (global variable)
    (nonlocal another_variable))
```

### Identity and Membership

```lisp
(is var1 var2)  ; Identity check
(in element list)  ; Membership test
```

### Logical Operations

```lisp
(and condition1 condition2)
(or condition1 condition2)
(not condition)
```

### Generators

```lisp
(def generator ()
    (yield value1)
    (yield value2))
```

### Asynchronous Programming

```lisp
(async def async_function ()
    (await some_async_operation))

(print (await (async_function)))
```

## Implementation Reference

The following features are currently implemented in Pythonic Lisp:

### Core Features (Fully Implemented)

1. **Basic Expression Parsing**
   - Parse expressions with parentheses
   - Arithmetic operations (`+`, `-`, `*`, `/`, etc.)
   - Comparison operations (`==`, `!=`, `<`, `>`, `<=`, `>=`)
   - Logical operations (`and`, `or`, `not`)

2. **Variable Assignment and Scope Management**
   - Variable assignment with `=` 
   - Function definition with `def`
   - Lexical scoping with proper closures
   - Simple global/nonlocal declarations

3. **Control Flow**
   - Conditionals: `if`, `elif`, `else`
   - Loops: `for`, `while`
   - Flow control: `continue`, `break`, `pass`
   - Return statements

4. **Function Definitions and Calls**
   - Named function definitions with `def`
   - Anonymous functions with `lambda`
   - First-class functions and higher-order functions
   - Closures with proper state encapsulation

5. **Data Structures**
   - Lists with comprehensive operations
   - Dictionaries with key-value access
   - Sets with standard operations
   - Tuples (immutable sequences)
   - List comprehensions

6. **Module System**
   - Basic `import` functionality
   - Module namespaces
   - Direct and relative imports

### Partially Implemented Features

1. **Class System**
   - Class syntax sugar for closure-based objects ✅
   - Private state through lexical scoping ✅
   - Method definition and access ✅
   - No explicit inheritance mechanism yet ❌

2. **Exception Handling** ✅
   - Comprehensive try/except/finally blocks ✅
   - Exception raising with types ✅
   - Exception type hierarchy ✅
   - Exception variable binding with 'as' ✅

3. **Context Managers** ✅
   - with/as syntax for resource management ✅
   - Enter/exit protocol ✅
   - File I/O context managers ✅

4. **Generator Functions** ✅
   - Yield statements for iteration ✅
   - Lazy evaluation ✅
   - Generator state management ✅
   - Integration with for loops ✅

### Planned Features
   
1. **Additional Syntax Sugar**
   - Dot notation for method access
   - Property access syntax

2. **Standard Library Enhancements**
   - String operations
   - Collection utilities

## 12. Generators

Pythonic Lisp supports generators through the `yield` statement, allowing functions to pause execution and later resume where they left off.

### Basic Generator Functions

```lisp
; Generate a sequence of numbers
(def (count-up-to n)
  (= i 0)
  (while (< i n)
    (yield i)
    (= i (+ i 1))))

; Use the generator
(for x (count-up-to 5)
  (print x))
```

### Working with Generators

You can use generators with for loops or manually using next():

```lisp
; Using next() to manually iterate
(= gen (count-up-to 3))
(print (next gen))  ; 0
(print (next gen))  ; 1
(print (next gen))  ; 2
```

### Converting Generators to Collections

```lisp
(def (squares n)
  (for i (range n)
    (yield (* i i))))

; Convert generator to list
(= square-list (list (squares 5)))  ; [0, 1, 4, 9, 16]
```

### Infinite Sequences

Generators can represent infinite sequences, taking values only as needed:

```lisp
(def (fibonacci)
  (= a 0)
  (= b 1)
  (while True
    (yield a)
    (= temp (+ a b))
    (= a b)
    (= b temp)))

; Take first 10 Fibonacci numbers
(= fib (fibonacci))
(for i (range 10)
  (print (next fib)))
```

---

Happy coding in Pythonic Lisp! As you become more comfortable with these concepts, explore the example programs in the `/examples` directory for more advanced patterns and usage.