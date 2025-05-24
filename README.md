# M28 v2.0: A Python-Like Lisp

M28 is a Lisp interpreter with Python-inspired syntax and semantics. It combines the power of Lisp's s-expressions with Python's familiar syntax and features.

## Features

### Core Language
- **Python-style syntax** in Lisp s-expressions
- **Dynamic typing** with Python-like type system
- **First-class functions** with lambda support
- **Lexical scoping** with proper closure support
- **Tail call optimization** for recursive functions

### Data Types
- Numbers (int/float unified)
- Strings with escape sequences
- Lists `[1, 2, 3]`
- Dictionaries `{"key": "value"}`
- Tuples `(1, 2, 3)`
- Booleans `true`/`false`
- None/nil

### Control Flow
- `if`/`elif`/`else` conditionals
- `while` loops with break/continue
- `for` loops with iterables
- `try`/`except`/`finally` exception handling
- `with` context managers

### Object System
- Dot notation for property access `obj.property`
- Method calls `obj.method(args)`
- Dictionary and list access `dict.key`, `list.0`
- Property assignment `obj.prop = value`

### Advanced Features
- **Generators** with `yield`
- **Classes** with inheritance
- **Module system** with import/export
- **File I/O** with Python-style API
- **Concurrent programming** with go/select
- **Multiple dispatch** for methods

## Installation

```bash
# Clone the repository
git clone https://github.com/mmichie/m28.git
cd m28

# Build the interpreter
make build

# Run the REPL
make run
```

## Quick Start

### REPL Mode
```bash
./bin/m28
m28> (print "Hello, World!")
Hello, World!
m28> (def x 42)
42
m28> (* x 2)
84
```

### Running Files
```bash
./bin/m28 script.m28
```

### Example Script
```lisp
; Function definition
(def greet (name)
    (print "Hello, " name "!"))

; Data structures
(def person {"name": "Alice", "age": 30})
(print person.name)  ; Dot notation

; List operations
(def numbers [1, 2, 3, 4, 5])
(for (n numbers)
    (print (* n n)))

; Exception handling
(try
    (/ 1 0)
    (except ZeroDivisionError
        (print "Cannot divide by zero!")))

; File I/O
(def f (open "output.txt" "w"))
(f.write "M28 is awesome!")
(f.close)
```

## Language Guide

### Basic Syntax
```lisp
; Comments start with semicolon
(def variable "value")              ; Variable definition
(= variable "new value")            ; Assignment
(+ 1 2 3)                          ; Arithmetic
(print "Hello" "World")            ; Multiple arguments
```

### Functions
```lisp
; Named function
(def square (x)
    (* x x))

; Lambda function
(def add (lambda (x y) (+ x y)))

; Function with multiple statements
(def factorial (n)
    (if (<= n 1)
        1
        (* n (factorial (- n 1)))))
```

### Data Structures
```lisp
; Lists
(def lst [1, 2, 3])
(print lst.0)                      ; First element
(append lst 4)                     ; Add element

; Dictionaries  
(def dict {"name": "Bob", "age": 25})
(print dict.name)                  ; Access value
(= dict.age 26)                    ; Update value

; Tuples
(def point (1, 2))
```

### Control Flow
```lisp
; Conditionals
(if (> x 10)
    (print "Greater than 10")
    (elif (> x 5)
        (print "Greater than 5")
        (else
            (print "5 or less"))))

; Loops
(while (< i 10)
    (print i)
    (= i (+ i 1)))

(for (item [1, 2, 3])
    (print (* item 2)))
```

### Exception Handling
```lisp
(try
    (risky-operation)
    (except ValueError (e)
        (print "Value error:" e))
    (except
        (print "Unknown error"))
    (finally
        (cleanup)))
```

### Classes
```lisp
(class Animal
    (def __init__ (self name)
        (= self.name name))
    
    (def speak (self)
        (print self.name "makes a sound")))

(class Dog (Animal)
    (def speak (self)
        (print self.name "barks!")))

(def dog (Dog "Rex"))
(dog.speak)  ; "Rex barks!"
```

### Modules
```lisp
; In math_utils.m28
(export (square cube))

(def square (x) (* x x))
(def cube (x) (* x x x))

; In main.m28
(import math_utils)
(print (math_utils.square 5))

; Or import specific functions
(from math_utils import square)
(print (square 5))
```

## Examples

See the `examples/` directory for more comprehensive examples:
- `dot_notation_demo.m28` - Demonstrates dot notation features
- `examples/oop/` - Object-oriented programming examples
- `examples/algorithms/` - Classic algorithms implementation
- `examples/advanced/` - Advanced features like generators and context managers

## Testing

```bash
# Run the test suite
make test

# Run specific test file
./bin/m28 tests/quick-sanity-test.m28
```

## Development

### Building from Source
```bash
make build        # Build the interpreter
make clean        # Clean build artifacts
make deps         # Install dependencies
```

### Project Structure
```
m28/
├── main.go           # Entry point
├── parser/           # S-expression parser
├── eval/             # Expression evaluator
├── core/             # Core types and values
├── builtin/          # Built-in functions
├── special_forms/    # Special forms (if, def, etc.)
├── examples/         # Example programs
└── tests/            # Test suite
```

## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests.

## License

MIT License - see LICENSE file for details.