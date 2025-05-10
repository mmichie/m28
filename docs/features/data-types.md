# Data Types in M28

This document covers the core data types in M28, including their creation, operations, and usage patterns.

## Contents
1. [Primitive Types](#primitive-types)
   - [Numbers](#numbers)
   - [Strings](#strings)
   - [Booleans](#booleans)
   - [None](#none)
2. [Collection Types](#collection-types)
   - [Lists](#lists)
   - [Dictionaries](#dictionaries)
   - [Sets](#sets)
   - [Tuples](#tuples)
3. [Function Types](#function-types)
4. [Type Conversions](#type-conversions)
5. [Type Testing](#type-testing)
6. [Examples](#examples)

## Primitive Types

### Numbers

M28 supports both integers and floating-point numbers:

```lisp
# Integers
(= a 42)
(= b -7)

# Floating-point numbers
(= pi 3.14159)
(= e 2.71828)
```

Operations:

```lisp
# Arithmetic
(= sum (+ 10 20))     # Addition: 30
(= diff (- 10 5))     # Subtraction: 5
(= product (* 4 5))   # Multiplication: 20
(= quotient (/ 10 2)) # Division: 5
(= remainder (% 10 3)) # Modulo: 1
(= power (** 2 3))    # Exponentiation: 8

# Numerical Functions
(= absolute (abs -5))           # Absolute value: 5
(= rounded (round 3.7))         # Rounding: 4
(= ceiling (ceil 3.2))          # Ceiling: 4
(= floor-value (floor 3.7))     # Floor: 3
(= max-value (max 1 5 3))       # Maximum: 5
(= min-value (min 1 5 3))       # Minimum: 1
```

### Strings

Strings are sequences of characters enclosed in double quotes:

```lisp
(= name "Alice")
(= greeting "Hello, World!")
```

Operations:

```lisp
# Concatenation
(= full-name (+ "John" " " "Doe"))  # "John Doe"

# String functions
(= length (len greeting))          # Length: 13
(= uppercase (str.upper name))     # Uppercase: "ALICE"
(= lowercase (str.lower name))     # Lowercase: "alice"
(= replaced (str.replace "Hello" "e" "a"))  # "Hallo"

# Indexing
(= first-char (get name 0))        # "A"
(= last-char (get name (- (len name) 1)))  # "e"

# Slicing (if implemented)
(= sub-string (str.slice greeting 0 5))  # "Hello"
```

### Booleans

Boolean values represent truth conditions:

```lisp
(= is-true True)
(= is-false False)
```

Operations:

```lisp
# Logical operators
(= and-result (and True False))    # False
(= or-result (or True False))      # True
(= not-result (not True))          # False

# Comparison operators
(= equal (== 5 5))                 # True
(= not-equal (!= 5 6))             # True
(= greater (> 10 5))               # True
(= less (< 5 10))                  # True
(= greater-equal (>= 5 5))         # True
(= less-equal (<= 5 5))            # True
```

### None

The `None` value represents the absence of a value:

```lisp
(= result None)
```

Usage:

```lisp
# Check for None
(if (== result None)
    (print "No result available"))

# Default values
(= value (if (== result None) "Default" result))
```

## Collection Types

### Lists

Lists are ordered collections of values:

```lisp
# Create a list
(= numbers (list 1 2 3 4 5))
(= names (list "Alice" "Bob" "Charlie"))
```

Operations:

```lisp
# Accessing elements
(= first (get numbers 0))          # 1
(= second (get names 1))           # "Bob"

# Length
(= count (len numbers))            # 5

# Adding elements
(= more-numbers (+ numbers (list 6 7)))  # (1 2 3 4 5 6 7)
(= with-appended (append numbers 6))     # (1 2 3 4 5 6)

# Removing elements
(= shorter (remove numbers 3))     # (1 2 4 5)

# Finding elements
(= position (index-of names "Bob"))  # 1
(= contains (in "Alice" names))      # True

# Transformations
(= doubled (map (lambda (x) (* x 2)) numbers))  # (2 4 6 8 10)
(= evens (filter (lambda (x) (== (% x 2) 0)) numbers))  # (2 4)
(= sum (reduce + numbers))         # 15
```

### Dictionaries

Dictionaries are key-value mappings:

```lisp
# Create a dictionary
(= person {"name": "John", "age": 30, "city": "New York"})
(= config (dict "host" "localhost" "port" 8080))
```

See [Dictionaries](dictionaries.md) for detailed information.

### Sets

Sets are unordered collections of unique elements:

```lisp
# Create a set
(= unique-numbers (set 1 2 3 2 1))  # (1 2 3)
```

Operations:

```lisp
# Set operations
(= a (set 1 2 3))
(= b (set 2 3 4))
(= union (set.union a b))         # (1 2 3 4)
(= intersection (set.intersection a b))  # (2 3)
(= difference (set.difference a b))      # (1)

# Membership
(= contains (set.contains a 2))    # True
(= size (set.size a))              # 3

# Adding/removing elements
(= with-added (set.add a 4))       # (1 2 3 4)
(= with-removed (set.remove a 2))  # (1 3)
```

### Tuples

Tuples are immutable sequences (if implemented):

```lisp
# Create a tuple
(= coordinates (tuple 10 20))
(= rgb (tuple 255 0 0))
```

Operations:

```lisp
# Accessing elements
(= x (get coordinates 0))          # 10
(= y (get coordinates 1))          # 20

# Length
(= size (len rgb))                 # 3

# Immutability
# Unlike lists, tuples cannot be modified after creation
```

## Function Types

Functions are first-class values in M28:

```lisp
# Named function
(def (add a b)
  (+ a b))

# Anonymous function
(= multiply (lambda (a b) (* a b)))
```

Operations:

```lisp
# Function calls
(add 2 3)                         # 5
(multiply 4 5)                    # 20

# Passing functions
(map add1 (list 1 2 3))           # (2 3 4)

# Returning functions
(def (make-adder n)
  (lambda (x) (+ x n)))
(= add5 (make-adder 5))
(add5 10)                         # 15
```

## Type Conversions

Convert between types:

```lisp
# String to number
(= n (int "42"))                  # 42
(= f (float "3.14"))              # 3.14

# Number to string
(= s (str 42))                    # "42"

# Collections
(= l (list (set 1 2 3)))          # (1 2 3)
(= d (dict (list (list "a" 1) (list "b" 2))))  # {"a": 1, "b": 2}
```

## Type Testing

Check value types:

```lisp
# Type predicates
(= is-num (number? 42))           # True
(= is-str (string? "hello"))      # True
(= is-list (list? (list 1 2 3)))  # True
(= is-dict (dict? {"a": 1}))      # True
(= is-func (function? add))       # True
```

## Examples

### Example 1: Working with Different Types

```lisp
# Mix different types in a list
(= mixed (list 10 "hello" True None))

# Process based on type
(def (describe-item item)
  (if (number? item)
      (+ "Number: " (str item))
      (if (string? item)
          (+ "String: " item)
          (if (bool? item)
              (+ "Boolean: " (str item))
              "None value"))))

# Apply to each item
(for item mixed
  (print (describe-item item)))
```

### Example 2: Data Transformation Pipeline

```lisp
# Start with a list of data
(= data (list 
  {"name": "Alice", "age": 25, "active": True}
  {"name": "Bob", "age": 30, "active": False}
  {"name": "Charlie", "age": 35, "active": True}
  {"name": "Diana", "age": 28, "active": True}))

# Filter active users
(= active-users (filter (lambda (user) (get user "active")) data))

# Extract ages
(= ages (map (lambda (user) (get user "age")) active-users))

# Calculate average age
(= total-age (reduce + ages))
(= avg-age (/ total-age (len ages)))

(print "Average age of active users:" avg-age)
```

### Example 3: Working with Nested Data Structures

```lisp
# Create a complex nested structure
(= organization {
  "name": "Acme Corp",
  "departments": (list
    {"name": "Engineering", 
     "employees": (list
       {"name": "Alice", "projects": (list "Alpha" "Beta")}
       {"name": "Bob", "projects": (list "Gamma")}
     )}
    {"name": "Marketing", 
     "employees": (list
       {"name": "Charlie", "projects": (list "Delta")}
     )}
  )
})

# Navigate the structure
(def (find-employees-on-project org project-name)
  (= results (list))
  (for dept (get org "departments")
    (for emp (get dept "employees")
      (if (in project-name (get emp "projects"))
          (= results (+ results (list (get emp "name")))))))
  results)

(= beta-team (find-employees-on-project organization "Beta"))
(print "Employees on Project Beta:" beta-team)
```

## Implementation Notes

M28's type system is dynamic and influenced by Python:

1. **Dynamic Typing**: Variables can hold values of any type
2. **Type Conversions**: Most type conversions are explicit
3. **Duck Typing**: Operations depend on behavior rather than explicit types
4. **Type Errors**: Type errors are generally detected at runtime
5. **Identity vs. Equality**: The `==` operator tests value equality, while `is` tests identity