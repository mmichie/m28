# M28 Documentation

Welcome to the M28 documentation! M28 is a Lispy-Pythonic programming language that combines s-expression syntax with Python semantics.

## Documentation Structure

### Core Documentation
- [Language Guide](language-guide.md) - Comprehensive language reference
- [Tutorial](tutorial.md) - Step-by-step introduction to M28
- [Quick Reference](quick-reference.md) - Quick syntax reference

### Feature Guides
- [Functions](features/functions.md) - Function definition and usage
- [Data Types](features/data-types.md) - Built-in data types
- [Control Flow](features/control-flow.md) - Conditionals and loops
- [Exception Handling](features/exception-handling.md) - Try/except/raise
- [Classes](features/classes.md) - Object-oriented programming
- [Modules](features/modules.md) - Module system and imports

### Advanced Topics
- [Functional Programming](advanced/functional.md) - Higher-order functions, composition
- [Generators](advanced/generators.md) - Yield and generator expressions
- [Context Managers](advanced/context-managers.md) - With statements
- [Concurrency](advanced/concurrency.md) - Async/await and goroutines

### Reference
- [Built-in Functions](reference/builtins.md) - All built-in functions
- [Standard Library](reference/stdlib.md) - Standard library modules
- [Python Interop](reference/python-interop.md) - Using Python libraries

## Key Concepts

### Syntax Rules
1. **Comments**: Always use `#` (never `;`)
2. **Variables**: Always use `=` (never `def`)
3. **Functions**: Only use `def` for functions
4. **Expressions**: Everything is prefix notation `(function arg1 arg2)`

### Example
```lisp
# This is a comment (always use #)

# Variables use =
(= name "Alice")
(= age 30)

# Functions use def
(def greet (person)
  (print f"Hello, {person}!"))

# Call the function
(greet name)

# Data structures
(= numbers [1, 2, 3, 4, 5])
(= person {"name": "Bob", "age": 25})

# Control flow
(if (> age 18)
    (print "Adult")
    (print "Minor"))

# Functional programming
(= doubled (map (lambda (x) (* x 2)) numbers))
```

## Getting Started

1. Read the [Tutorial](tutorial.md) for a gentle introduction
2. Reference the [Language Guide](language-guide.md) for comprehensive details
3. Check the [Quick Reference](quick-reference.md) for syntax reminders
4. Explore the [Examples](../examples/) directory for real code

## Design Philosophy

M28 combines:
- **Lisp's homoiconicity**: Code is data, enabling powerful metaprogramming
- **Python's pragmatism**: Familiar keywords, clear semantics, extensive ecosystem
- **Modern features**: Type hints, async/await, pattern matching (planned)
- **Simplicity**: One way to do things, clear and consistent