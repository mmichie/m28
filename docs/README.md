# M28 Documentation

Welcome to the M28 documentation! M28 is a Lispy-Pythonic programming language that combines s-expression syntax with Python semantics.

## Quick Links

- 🚀 [Tutorial](tutorial.md) - Start here if you're new to M28
- 📖 [Language Guide](language-guide.md) - Comprehensive language reference
- 📋 [Quick Reference](quick-reference.md) - Handy syntax cheat sheet
- 💡 [Examples](../examples/README.md) - Learn by example

## Documentation Overview

### Getting Started
- [Tutorial](tutorial.md) - Step-by-step introduction to M28
- [Quick Reference](quick-reference.md) - Common syntax and patterns
- [Installation Guide](installation.md) - How to install and set up M28

### Language Features
- [Functions](features/functions.md) - Function definition and usage
- [Data Types](features/data-types.md) - Lists, dicts, sets, tuples
- [Control Flow](features/control-flow.md) - if/elif/else, loops
- [Exception Handling](features/exception-handling.md) - try/except/finally
- [Dot Notation](features/dot-notation.md) - Property and method access
- [Dictionaries](features/dictionaries.md) - Python-style dictionaries
- [Concurrency](features/concurrency.md) - async/await, go, channels

### Module System
- [Modules](modules.md) - Creating and using modules
- [Standard Library](stdlib.md) - Built-in modules

### Advanced Topics
- [Class Syntax](class-syntax.md) - Object-oriented programming
- [F-String Enhancements](features/fstring-enhancements.md) - String formatting
- [List Mutability](features/list-mutability.md) - List behavior
- [Direct Property Access](direct-property-access.md) - Dot notation details

### Reference
- [Language Specification](specification/language-specification.md) - Formal specification
- [Built-in Functions](builtins.md) - All built-in functions
- [Testing Guide](testing-guide.md) - Writing tests in M28

## Key Syntax Rules

### The Three Golden Rules
1. **Comments**: Always use `#` (never `;`)
2. **Variables**: Always use `=` for assignment
3. **Functions**: Only use `def` for function definitions

### Basic Example
```lisp
# This is a comment (always use #)

# Variables use = for assignment
(= name "Alice")
(= age 30)

# Functions use def
(def greet (person)
  (print f"Hello, {person}!"))

# Everything is prefix notation
(greet name)  # Prints: Hello, Alice!

# Python-style data structures
(= numbers [1, 2, 3, 4, 5])
(= person {"name": "Bob", "age": 25})

# Control flow uses Python keywords
(if (> age 18)
  (print "Adult")
  (print "Minor"))

# Functional programming
(= doubled (map (lambda (x) (* x 2)) numbers))
(print doubled)  # [2, 4, 6, 8, 10]
```

## Design Philosophy

M28 combines the best of both worlds:

- **Lisp's Power**: S-expressions, homoiconicity, macros (planned)
- **Python's Pragmatism**: Familiar keywords, clear semantics, rich data types
- **Modern Features**: Dot notation, f-strings, async/await
- **Consistency**: One clear way to do things

## Version

This documentation is for M28 v0.1.0. See the [CHANGELOG](../CHANGELOG.md) for version history.

## Getting Help

- Check the [Tutorial](tutorial.md) for step-by-step learning
- Read the [Language Guide](language-guide.md) for detailed explanations
- Browse [Examples](../examples/) for practical code samples
- Consult the [Quick Reference](quick-reference.md) for syntax reminders

## Contributing

Found an error or want to improve the docs? Please submit an issue or PR on [GitHub](https://github.com/mmichie/m28).