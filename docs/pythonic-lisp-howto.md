# How to Program in Pythonic Lisp: A Beginner's Guide

Welcome to Pythonic Lisp! This guide will help you get started with the basics of programming in this unique language that combines elements of Lisp and Python.

## 1. Basic Syntax

Pythonic Lisp uses parentheses to group expressions, similar to traditional Lisp:

```lisp
(function-name argument1 argument2 ...)
```

## 2. Data Types

Pythonic Lisp supports several data types:

- Numbers: `42`, `3.14`
- Strings: `"Hello, World!"`
- Symbols: `my-variable`
- Lists: `(1 2 3 4)`
- Booleans: `True`, `False`
- None: `None`

## 3. Variables and Functions

Define variables and functions using `def`:

```lisp
(def x 10)
(def (greet name) (print "Hello," name))
```

Use `lambda` for anonymous functions:

```lisp
(def square (lambda (x) (* x x)))
```

## 4. Control Structures

Use `if` for conditional execution:

```lisp
(if (> x 0)
    (print "Positive")
    (print "Non-positive"))
```

Use `for` and `while` for loops:

```lisp
(for x (range 5)
  (print x))

(while (< count 10)
  (print count)
  (def count (+ count 1)))
```

## 5. Lists and List Comprehensions

Create and manipulate lists:

```lisp
(def numbers (list 1 2 3 4 5))
(print (first numbers))
(print (rest numbers))

; List comprehension
(def squares
  (for (x (range 10))
    (* x x)))
```

## 6. Higher-Order Functions

Use functions like `map`, `filter`, and `reduce`:

```lisp
(def numbers (list 1 2 3 4 5))
(def doubled (map (lambda (x) (* x 2)) numbers))
(def evens (filter (lambda (x) (== 0 (% x 2))) numbers))
(def sum (reduce + numbers))
```

## 7. Exception Handling

Use `try` and `except` for error handling:

```lisp
(try
  (/ 10 0)
  (except ZeroDivisionError
    (print "Cannot divide by zero")))
```

## 8. Working with Modules

Import and use modules:

```lisp
(import math)
(print (math.sin (/ math.pi 2)))
```

## 9. Classes and Objects

Define and use classes:

```lisp
(class Dog
  (def (init self name)
    (def self.name name))
  (def (bark self)
    (print (+ self.name " says Woof!"))))

(def my-dog (Dog "Buddy"))
(my-dog.bark)
```

## 10. File I/O

Read from and write to files:

```lisp
(with (open "example.txt" "w") as file
  (file.write "Hello, Pythonic Lisp!"))

(with (open "example.txt" "r") as file
  (print (file.read)))
```

This guide covers the basics of Pythonic Lisp. As you become more comfortable with these concepts, you can explore more advanced features and libraries available in the language. Happy coding!
