# M28 Examples

Welcome to the M28 examples collection! These examples demonstrate the Lispy-Pythonic nature of M28, combining S-expressions with Python semantics.

## Quick Start

```lisp
# Run an example
./bin/m28 examples/00_basics/hello_world.m28

# Or from the REPL
(load "examples/00_basics/hello_world.m28")
```

## Organization

Examples are organized by topic and complexity:

- **00_basics/** - Start here! Basic syntax and operations
- **01_functions/** - Functions, lambdas, and functional concepts
- **02_control_flow/** - Conditionals, loops, and exceptions
- **03_data_structures/** - Lists, dictionaries, sets, and tuples
- **04_classes/** - Object-oriented programming
- **05_modules/** - Module system and code organization
- **06_file_io/** - File operations and I/O
- **07_functional/** - Advanced functional programming
- **08_projects/** - Complete example applications

See [index.md](index.md) for a detailed listing of all examples.

## M28 Syntax Reminder

```lisp
# Comments use # (not ;)
# This is a comment

# Variables use = (not def)
(= name "M28")
(= number 42)

# Functions use def
(def greet (name)
  (print f"Hello, {name}!"))

# Everything is prefix notation
(+ 1 2 3)           # 6
(print "Hello")     # Hello
```

## Featured Examples

### Hello World (00_basics/hello_world.m28)
```lisp
# The classic first program
(print "Hello, World!")
```

### Functions (01_functions/basic_functions.m28)
```lisp
# Define a function
(def factorial (n)
  (if (<= n 1)
    1
    (* n (factorial (- n 1)))))

(print (factorial 5))  # 120
```

### Classes (04_classes/basic_classes.m28)
```lisp
# Define a class
(class Person
  (def __init__ (self name)
    (= self.name name))
  
  (def greet (self)
    (print f"Hello, I'm {self.name}")))

(= alice (Person "Alice"))
(alice.greet)  # "Hello, I'm Alice"
```

### List Comprehension (03_data_structures/lists.m28)
```lisp
# List comprehension
(= squares [x**2 for x in (range 10)])
(print squares)  # [0, 1, 4, 9, 16, 25, 36, 49, 64, 81]
```

## Projects

Check out the complete projects in `08_projects/`:

- **todo_app.m28** - A full-featured command-line todo list manager
- **calculator.m28** - Scientific calculator with expression evaluation
- **text_adventure.m28** - Text-based adventure game engine

## Learning Path

1. Start with `00_basics/hello_world.m28`
2. Work through each directory in order
3. Try modifying examples to experiment
4. Build your own programs using these as templates

## Contributing

When adding new examples:
1. Follow the M28 syntax rules (# for comments, = for variables)
2. Place in the appropriate directory
3. Include helpful comments
4. Test before committing

Happy coding with M28!