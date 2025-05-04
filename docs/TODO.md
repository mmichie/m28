# M28 Language Implementation Roadmap

This document outlines the remaining features to be implemented in the M28 language core. Items are organized by priority level.

## High Priority

- **Complete core data structure support**
  - Finish dictionary implementation
  - Ensure all basic data structures have full functionality

- **Enhance control flow structures**
  - Ensure if/else works correctly in all contexts
  - Fix any issues with for/while loops

- **Implement exception handling**
  - Add try/except/finally for error handling
  - Support raising exceptions

- **Support module system**
  - Fix import functionality for importing modules from files
  - Implement proper module loading and caching

- **Enhance standard library**
  - Complete the core set of built-in functions
  - Ensure consistent behavior with Python where appropriate

- **Improve error reporting**
  - Implement better error messages with line/column information
  - Add call stack traces for debugging

- **Create comprehensive testing framework**
  - Fix remaining tests
  - Add coverage for all language features

## Medium Priority

- **Additional data types**
  - Add sets implementation (similar to Python's set data type)
  - Support tuple data type (immutable sequences)

- **Loop control**
  - Add 'break' and 'continue' for loop control
  - Support complex loop patterns

- **Import enhancements**
  - Implement 'from X import Y' syntax for selective imports
  - Support relative imports

- **Object-oriented programming**
  - Implement 'class' definition with Python-like syntax
  - Support method definition and instance creation

- **Performance optimizations**
  - Identify and fix performance bottlenecks
  - Optimize core data structures and operations

- **Extend built-in functions**
  - Implement more Python built-in functions (zip, map, filter, etc.)
  - Add string manipulation functions (split, join, etc.)
  - Support file I/O operations

- **Type system improvements**
  - Improve type checking and conversion
  - Support runtime type checking

- **Development experience**
  - Add REPL improvements (history, completion, etc.)
  - Enhance tooling for development

## Low Priority

- **Advanced OOP features**
  - Support inheritance and method overriding
  - Add support for metaclasses

- **Compilation improvements**
  - Implement bytecode compilation step
  - Add optimization passes to the evaluator

- **Asynchronous programming**
  - Add 'async/await' syntax
  - Support for coroutines and async I/O

- **Type annotations**
  - Add optional type hints
  - Support for type checking based on annotations

## Completed Features

- ✅ Python-style dictionary literals (`{"key": value}`)
- ✅ Dictionary functions (get, dict)
- ✅ Keyword arguments for functions via dictionaries
- ✅ Basic arithmetic and comparison operations
- ✅ Variable assignment with `=`
- ✅ Function definition with `def`
- ✅ Basic control flow (if, for, while)
- ✅ List operations
- ✅ Simple assert function

---

This roadmap will be updated as features are implemented and priorities change.