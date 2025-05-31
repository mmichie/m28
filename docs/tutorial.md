# M28 Tutorial

Welcome to M28! This tutorial will guide you through the basics of the language.

## Getting Started

### Your First M28 Program

Let's start with the classic "Hello, World!" program:

```lisp
(print "Hello, World!")
```

Save this in a file called `hello.m28` and run it:

```bash
./bin/m28 hello.m28
```

### The REPL

M28 comes with an interactive REPL (Read-Eval-Print Loop). Start it by running:

```bash
./bin/m28
```

You'll see a prompt where you can type expressions:

```
M28 REPL
Type 'exit' to quit, 'help' for more information
> (+ 1 2)
3
> (print "Hello from REPL!")
Hello from REPL!
None
```

## Basic Concepts

### Everything is an Expression

In M28, everything is an expression that returns a value. Even `print` returns `None`:

```lisp
> (print "Hi")
Hi
None
```

### Prefix Notation

M28 uses prefix notation (also called Polish notation) for all operations:

```lisp
(+ 1 2 3)       # Addition: 1 + 2 + 3 = 6
(* 2 3 4)       # Multiplication: 2 × 3 × 4 = 24
(- 10 3 2)      # Subtraction: 10 - 3 - 2 = 5
```

### Comments

Use `#` for comments (never use `;`):

```lisp
# This is a comment
(= x 10)  # This is also a comment
```

## Variables

### Assignment

Use `=` to assign variables (never use `def` for variables):

```lisp
(= name "Alice")
(= age 25)
(= pi 3.14159)
```

### Naming Rules

- Use `snake_case` for variables and functions
- Use `PascalCase` for classes
- Use `UPPER_SNAKE_CASE` for constants

```lisp
(= user_name "Bob")
(= MAX_SIZE 100)
```

## Data Types

### Numbers

M28 has unified numeric type (like Python 3):

```lisp
(= integer 42)
(= decimal 3.14)
(= negative -10)
(= scientific 1.23e-4)
```

### Strings

Strings use double quotes:

```lisp
(= simple "Hello")
(= escaped "She said \"Hi\"")
(= multiline "Line 1\nLine 2")

# String formatting (f-strings)
(= name "Alice")
(= greeting f"Hello, {name}!")

# Enhanced f-strings support nested quotes
(= data {"name": "Bob", "age": 25})
(print f"User: {data['name']}")  # Single quotes inside double
(print f'Age: {data["age"]}')    # Double quotes inside single
```

### Booleans

```lisp
(= yes True)   # Can also use true
(= no False)   # Can also use false
```

### None/Nil

```lisp
(= nothing None)  # Can also use nil
```

### Lists

Lists are mutable ordered collections:

```lisp
(= empty [])
(= numbers [1, 2, 3, 4, 5])
(= mixed [1, "two", 3.0, [4, 5]])

# Access elements
(print numbers.0)      # First element: 1
(print numbers.2)      # Third element: 3

# Modify lists
(append numbers 6)     # Add to end
(= numbers.0 10)       # Change first element
```

### Dictionaries

Dictionaries are key-value mappings:

```lisp
(= empty {})
(= person {"name": "Alice", "age": 30, "city": "NYC"})

# Access values
(print person.name)         # "Alice"
(print (get person "age"))  # 30

# Modify dictionaries
(= person.age 31)
(= person.email "alice@example.com")
```

### Sets

Sets are unordered collections of unique values:

```lisp
(= numbers {1, 2, 3})
(= empty (set))

# Operations
(add numbers 4)
(print (in 2 numbers))  # True
```

### Tuples

Tuples are immutable sequences:

```lisp
(= point (10, 20))
(= single (42,))  # Note the comma for single element

# Access elements
(print point.0)   # 10
(print point.1)   # 20
```

## Functions

### Defining Functions

Use `def` ONLY for functions:

```lisp
# Simple function
(def greet (name)
  (print f"Hello, {name}!"))

# Function with return value
(def add (a b)
  (+ a b))

# Function with default argument
(def greet (name="World")
  (print f"Hello, {name}!"))
```

### Calling Functions

```lisp
(greet "Alice")        # Hello, Alice!
(greet)                # Hello, World!

(= result (add 5 3))   # result = 8
```

### Lambda Functions

For anonymous functions:

```lisp
(= square (lambda (x) (* x x)))
(print (square 5))     # 25

# Inline usage
(map (lambda (x) (* x 2)) [1, 2, 3])  # [2, 4, 6]
```

## Control Flow

### If Statements

```lisp
# Simple if
(if (> age 18)
    (print "Adult"))

# if-else
(if (>= score 60)
    (print "Pass")
    (print "Fail"))

# if-elif-else
(if (> score 90)
    (print "A")
    (elif (> score 80)
        (print "B")
        (elif (> score 70)
            (print "C")
            (print "F"))))
```

### Loops

#### For Loops

```lisp
# Iterate over list
(for x in [1, 2, 3]
  (print x))

# Using range
(for i in (range 5)
  (print i))  # Prints 0, 1, 2, 3, 4

# Iterate over dictionary
(for (k, v) in (items person)
  (print f"{k}: {v}"))
```

#### While Loops

```lisp
(= count 0)
(while (< count 5)
  (print count)
  (= count (+ count 1)))

# With break and continue
(while True
  (= input (input "Enter number (q to quit): "))
  (if (== input "q")
      (break))
  (if (< (int input) 0)
      (continue))
  (print f"Square: {(** (int input) 2)}"))
```

## Working with Collections

### List Operations

```lisp
(= numbers [1, 2, 3, 4, 5])

# Length
(print (len numbers))           # 5

# Append
(append numbers 6)

# Map - apply function to each element
(= doubled (map (lambda (x) (* x 2)) numbers))

# Filter - keep elements that match condition
(= evens (filter (lambda (x) (== (% x 2) 0)) numbers))

# Reduce - combine elements
(= sum (reduce + numbers))      # 15
```

### List Comprehensions

```lisp
# Basic comprehension
(= squares [x**2 for x in (range 5)])  # [0, 1, 4, 9, 16]

# With condition
(= even_squares [x**2 for x in (range 10) if (== (% x 2) 0)])
```

### Dictionary Operations

```lisp
(= scores {"Alice": 95, "Bob": 87, "Charlie": 92})

# Get all keys
(print (keys scores))

# Get all values
(print (values scores))

# Get all items
(for (name, score) in (items scores)
  (print f"{name}: {score}"))

# Dictionary comprehension
(= squared {x: x**2 for x in (range 5)})
```

## Exception Handling

### Try-Except

```lisp
(try
  (= result (/ 10 0))
  (except ZeroDivisionError as e
    (print "Cannot divide by zero!")
    (= result 0)))

# Multiple exception types
(try
  (risky_operation)
  (except (ValueError, TypeError) as e
    (print f"Error: {e}"))
  (except Exception as e
    (print f"Unexpected error: {e}")))
```

### Raising Exceptions

```lisp
(def check_age (age)
  (if (< age 0)
      (raise (ValueError "Age cannot be negative"))
      (if (< age 18)
          "Minor"
          "Adult")))
```

## Classes

### Basic Class

```lisp
(class Person
  (def __init__ (self name age)
    (= self.name name)
    (= self.age age))
  
  (def introduce (self)
    (print f"Hi, I'm {self.name}, {self.age} years old"))
  
  (def birthday (self)
    (= self.age (+ self.age 1))
    (print f"Happy birthday! Now {self.age}")))

# Create instance
(= alice (Person "Alice" 30))

# Call methods
(alice.introduce)      # Hi, I'm Alice, 30 years old
(alice.birthday)       # Happy birthday! Now 31
```

### Inheritance

```lisp
(class Student (Person)
  (def __init__ (self name age school)
    (super.__init__ name age)
    (= self.school school))
  
  (def introduce (self)
    (super.introduce)
    (print f"I study at {self.school}")))

(= bob (Student "Bob" 20 "MIT"))
(bob.introduce)
# Hi, I'm Bob, 20 years old
# I study at MIT
```

## Modules

### Creating a Module

Save this as `math_utils.m28`:

```lisp
# math_utils.m28

(def square (x)
  (* x x))

(def cube (x)
  (* x x x))

(def factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

# Export specific functions
(= __all__ ["square", "cube", "factorial"])
```

### Using Modules

```lisp
# Import entire module
(import math_utils)
(print (math_utils.square 5))     # 25

# Import specific functions
(from math_utils import (square, cube))
(print (square 5))                # 25
(print (cube 3))                  # 27

# Import with alias
(import math_utils as mu)
(print (mu.factorial 5))          # 120
```

## File I/O

### Reading Files

```lisp
# Read entire file
(with (open "data.txt" "r") as f
  (= content (f.read))
  (print content))

# Read line by line
(with (open "data.txt" "r") as f
  (for line in f
    (print (strip line))))
```

### Writing Files

```lisp
# Write text
(with (open "output.txt" "w") as f
  (f.write "Hello, M28!\n")
  (f.write "This is a new line\n"))

# Write multiple lines
(= lines ["Line 1", "Line 2", "Line 3"])
(with (open "output.txt" "w") as f
  (for line in lines
    (f.write f"{line}\n")))
```

## Best Practices

1. **Use meaningful names**: `calculate_total` not `ct`
2. **Keep functions small**: Each function should do one thing
3. **Handle errors**: Use try-except for operations that might fail
4. **Use comprehensions**: They're more Pythonic than manual loops
5. **Close resources**: Use `with` statements for files
6. **Document your code**: Add comments for complex logic

## Next Steps

1. Explore the [Language Guide](language-guide.md) for more details
2. Check out the [Examples](../examples/) directory
3. Read about [Advanced Features](language-guide.md#advanced-features)
4. Try building a small project!

## Example Project: Todo List

Here's a simple todo list to practice what you've learned:

```lisp
# todo.m28

(= todos [])

(def add_todo (task)
  (append todos {"task": task, "done": False})
  (print f"Added: {task}"))

(def list_todos ()
  (if (== (len todos) 0)
      (print "No todos!")
      (for (i, todo) in (enumerate todos)
        (= status (if todo.done "✓" " "))
        (print f"{i}. [{status}] {todo.task}"))))

(def complete_todo (index)
  (if (and (>= index 0) (< index (len todos)))
      (do
        (= todos.index.done True)
        (print f"Completed: {todos.index.task}"))
      (print "Invalid index")))

(def main ()
  (while True
    (print "\n--- Todo List ---")
    (list_todos)
    (print "\nOptions: (a)dd, (c)omplete, (q)uit")
    (= choice (input "Choice: "))
    
    (if (== choice "a")
        (add_todo (input "Task: "))
        (elif (== choice "c")
            (complete_todo (int (input "Index: ")))
            (elif (== choice "q")
                (break)
                (print "Invalid choice"))))))

(if (== __name__ "__main__")
    (main))
```

Happy coding with M28!