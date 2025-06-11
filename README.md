# M28 - A Lispy-Pythonic Programming Language

[![Version](https://img.shields.io/badge/version-0.1.0-blue.svg)](CHANGELOG.md)
[![License](https://img.shields.io/badge/license-MIT-green.svg)](LICENSE)
[![Go Version](https://img.shields.io/badge/go-%3E%3D1.19-00ADD8.svg)](go.mod)

M28 is a modern programming language that blends the elegant s-expression syntax of Lisp with the pragmatic design and extensive ecosystem of Python. It offers the best of both worlds: the power of homoiconic code and macros with Python's clean semantics and familiar keywords.

## Quick Start

```lisp
# Variables use = (never def)
(= message "Hello, M28!")
(print message)

# Functions use def
(def greet (name)
  (print "Hello," name))

(greet "World")

# Python-style data structures with Lisp syntax
(= numbers [1, 2, 3, 4, 5])
(= doubled (map (lambda (x) (* x 2)) numbers))
(print doubled)  # [2, 4, 6, 8, 10]
```

## Installation

```bash
# Clone the repository
git clone https://github.com/mmichie/m28.git
cd m28

# Build the interpreter
make build

# Run the REPL
./bin/m28

# Run a script
./bin/m28 script.m28
```

## Language Features

### S-Expression Syntax
Everything is an expression in prefix notation:
```lisp
(+ 1 2 3)           # 6
(print "Hello")     # Hello
(= x 5)
(if (> x 0) "positive" "non-positive")  # "positive"
```

### Python Semantics
- `#` for comments (Python-style)
- `def` only for functions
- `=` only for variables
- Python keywords: `if`, `elif`, `else`, `for`, `in`, `while`, `try`, `except`, `class`, `import`, etc.
- Python built-ins: `len`, `range`, `sum`, `map`, `filter`, `print`, etc.

### Modern Data Structures
```lisp
# Lists
(= fruits ["apple", "banana", "orange"])

# Dictionaries  
(= person {"name": "Alice", "age": 30})

# Sets
(= unique {1, 2, 3})

# Tuples
(= point (tuple [10, 20]))
```

### Object-Oriented Programming
```lisp
(class Person
  (def __init__ (self name)
    (= self.name name))
  
  (def greet (self)
    (print f"Hi, I'm {self.name}")))

(= alice (Person "Alice"))
(alice.greet)  # Hi, I'm Alice
```

### Functional Programming
```lisp
# First-class functions
(= numbers [1, 2, 3, 4, 5])
(= evens (filter (lambda (x) (== (% x 2) 0)) numbers))
(= squared (map (lambda (x) (* x x)) evens))

# List comprehensions
(= squares [(** x 2) for x in (range 10) if (== (% x 2) 0)])
```

### Exception Handling
```lisp
(try
  (/ 1 0)  # This will raise an error
  (except
    (print "Caught an error!")))

# With finally block
(try
  (print "Attempting operation...")
  (/ 10 2)
  (except
    (print "Error occurred"))
  (finally
    (print "Cleanup complete")))
```

### Module System
```lisp
(import math)
(import pandas as pd)
(from datetime import date)

(= root (math.sqrt 16))
(= df (pd.DataFrame data))
```

## Documentation

- [Language Guide](docs/language-guide.md) - Comprehensive language reference
- [Tutorial](docs/tutorial.md) - Step-by-step introduction
- [Standard Library](docs/stdlib.md) - Built-in functions and modules
- [Examples](examples/) - Code examples and patterns

## Examples

### Hello World
```lisp
(print "Hello, World!")
```

### Factorial
```lisp
(def factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(print (factorial 5))  # 120
```

### File I/O
```lisp
(with (open "data.txt" "r") as f
  (for line in f
    (print (strip line))))
```

### Quick Sort
```lisp
(def quicksort (lst)
  (if (<= (len lst) 1)
      lst
      (let pivot (first lst)
           rest (rest lst)
           less (filter (lambda (x) (< x pivot)) rest)
           greater (filter (lambda (x) (>= x pivot)) rest)
        (+ (quicksort less) [pivot] (quicksort greater)))))

(print (quicksort [3, 1, 4, 1, 5, 9, 2, 6]))  # [1, 1, 2, 3, 4, 5, 6, 9]
```

## Why M28?

1. **S-expressions** - Code is data, enabling powerful metaprogramming
2. **Python semantics** - Familiar and pragmatic design choices
3. **Prefix notation** - Consistent, unambiguous syntax
4. **Functional programming** - First-class functions, map/filter/reduce
5. **Modern features** - List comprehensions, context managers, generators
6. **Simple implementation** - Easy to understand and extend

## Contributing

Contributions are welcome! Please read our [Contributing Guide](CONTRIBUTING.md) for details.

## License

M28 is licensed under the MIT License. See [LICENSE](LICENSE) for details.

## Acknowledgments

M28 stands on the shoulders of giants:
- Lisp for s-expressions and homoiconicity
- Python for pragmatic design and clear semantics
- Scheme for minimalism and elegance
- Clojure for modern Lisp innovations