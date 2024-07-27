# Pythonic Lisp Design Document

**Objective**: Design a Lisp-like language with Python-inspired syntax, using parentheses for both expression grouping and list definitions. This document outlines the translation of Python keywords into this new language and provides a checklist for implementing the interpreter.

---

## Keywords and Translation

1. **False, True, None**
   - **Lisp**: `False`, `True`, `None`
   - **Pythonic Lisp**: `(False)`, `(True)`, `(None)`

2. **class**
   - **Lisp**: `class`
   - **Pythonic Lisp**: `(class MyClass () ...)`

3. **def**
   - **Lisp**: `def`
   - **Pythonic Lisp**: `(def (function-name args ...) ...)`

4. **return**
   - **Lisp**: `return`
   - **Pythonic Lisp**: `(return value)`

5. **if, elif, else**
   - **Lisp**: `if`, `elif`, `else`
   - **Pythonic Lisp**: 
     ```lisp
     (if condition1
         (do_something1)
         (elif condition2
             (do_something2)
             (else
                 (do_something_else))))
     ```

6. **for, while, continue, break**
   - **Lisp**: `for`, `while`, `continue`, `break`
   - **Pythonic Lisp**:
     ```lisp
     (for i (range 0 10)
         (if (== i 5)
             (continue))
         (print i)
         (if (== i 7)
             (break)))

     (while condition
         (do_something))
     ```

7. **try, except, finally, raise, assert**
   - **Lisp**: `try`, `except`, `finally`, `raise`, `assert`
   - **Pythonic Lisp**:
     ```lisp
     (try
         (do_something)
     (except Exception as e
         (print e))
     (finally
         (do_something_finally)))

     (raise (Exception "Error message"))
     (assert condition "Error message")
     ```

8. **import, from, as**
   - **Lisp**: `import`, `from`, `as`
   - **Pythonic Lisp**:
     ```lisp
     (import module)
     (from module import something as alias)
     ```

9. **lambda**
   - **Lisp**: `lambda`
   - **Pythonic Lisp**: `(lambda (args ...) body)`

10. **with, as**
    - **Lisp**: `with`, `as`
    - **Pythonic Lisp**:
      ```lisp
      (with (open "file.txt") as file
          (do_something_with file))
      ```

11. **global, nonlocal**
    - **Lisp**: `global`, `nonlocal`
    - **Pythonic Lisp**:
      ```lisp
      (def my_function ()
          (global variable)
          (nonlocal another_variable))
      ```

12. **await, async**
    - **Lisp**: `await`, `async`
    - **Pythonic Lisp**:
      ```lisp
      (async def async_function ()
          (await some_async_operation))
      ```

13. **del**
    - **Lisp**: `del`
    - **Pythonic Lisp**:
      ```lisp
      (del variable)
      ```

14. **is**
    - **Lisp**: `is`
    - **Pythonic Lisp**: `(is var1 var2)`

15. **in**
    - **Lisp**: `in`
    - **Pythonic Lisp**: `(in element list)`

16. **and, or, not**
    - **Lisp**: `and`, `or`, `not`
    - **Pythonic Lisp**: `(and condition1 condition2)`, `(or condition1 condition2)`, `(not condition)`

17. **yield**
    - **Lisp**: `yield`
    - **Pythonic Lisp**:
      ```lisp
      (def generator ()
          (yield value1)
          (yield value2))
      ```

---

## Implementation Checklist

1. **Basic Expression Parsing**
   - Parse basic expressions with parentheses
   - Implement arithmetic operations (`+`, `-`, `*`, `/`)
   - Implement comparison operations (`==`, `!=`, `<`, `>`, `<=`, `>=`)

2. **Variable Assignment and Scope Management**
   - Implement `=`
   - Handle variable scope (local, global, nonlocal)

3. **Control Flow**
   - Implement `if`, `elif`, `else`
   - Implement `for` loops
   - Implement `while` loops
   - Implement `continue`, `break`

4. **Function Definitions and Calls**
   - Implement `def` for function definitions
   - Handle function calls
   - Support `return` statements

5. **Class Definitions and Methods**
   - Implement `class` for class definitions
   - Handle instance creation and method calls
   - Support inheritance (`super`)

6. **Exception Handling**
   - Implement `try`, `except`, `finally`
   - Support `raise` for raising exceptions
   - Implement `assert` for assertions

7. **Import and Module Management**
   - Implement `import`
   - Handle `from ... import ... as ...`

8. **Asynchronous Programming**
   - Implement `async` and `await`

9. **Context Managers**
   - Implement `with ... as ...`

10. **Lambda Functions and Generators**
    - Implement `lambda`
    - Implement `yield` for generators

11. **Built-in Functions and List Manipulation**
    - Implement common built-in functions (`len`, `range`, `print`, etc.)
    - Handle list operations and slicing

12. **Additional Features**
    - Implement `del` for deletion
    - Support `is` for identity checks
    - Handle `in` for membership tests

---

## Example Usage

### Basic Example

```lisp
(def add_two_numbers (a b)
    (+ a b))

(print (add_two_numbers 3 5))
```

```lisp
(class Point ()
    (def __init__ (self x y)
        (= self.x x)
        (= self.y y))

    (def move (self dx dy)
        (= self.x (+ self.x dx))
        (= self.y (+ self.y dy)))

    (def distance (self other)
        (sqrt (+ (** (- self.x other.x) 2)
                 (** (- self.y other.y) 2)))))

(def p1 (Point 0 0))
(def p2 (Point 3 4))
(p1.move 1 1)
(print (p1.distance p2))
```

```lisp
(async def async_function ()
    (await some_async_operation))

(print (await (async_function)))
```
