# M28 Language Implementation Roadmap

This document outlines the planned features and improvements for the M28 language. Items are organized by priority level and area of focus.

## High Priority

### Core Language Features

- **Exception Handling Enhancements**
  - Create a complete exception hierarchy
  - Implement custom exception types
  - Add exception propagation mechanisms
  - Improve traceback reporting

- **Control Flow Enhancements**
  - Improve `if/else` behavior in nested contexts
  - Add support for break/continue in loops
  - Add support for early returns in functions

- **Recursion Optimization**
  - Implement tail call optimization
  - Add proper stack management for deep recursion
  - Optimize recursive function calls

### Module System Improvements

- **Basic Module System**
  - Replace deprecated `ioutil.ReadFile` with `os.ReadFile`
  - Fix consistency issues in module symbol resolution
  - Implement proper error handling with detailed error messages
  - Add tests for edge cases in module loading

- **Namespace Control**
  - Implement a module-level `__exports__` mechanism
  - Add support for private symbols (prefixed with underscore)
  - Create clear separation between module-local and exported symbols
  - Add namespace introspection utilities

- **Import Enhancements**
  - Add support for aliased imports (`import module as alias`)
  - Implement multi-symbol imports (`from module import x, y, z`)
  - Add wildcard imports with filtering
  - Create an import hook system for extensibility

### Standard Library

- **Complete the core set of built-in functions**
  - Implement missing Python-equivalent functions
  - Ensure consistent behavior with Python equivalents
  - Add proper documentation for all built-ins

- **Data Structure Improvements**
  - Complete dictionary implementation
  - Enhance list operations (slicing, comprehensions)
  - Add set operations (union, intersection, etc.)
  - Implement tuples as immutable sequences

## Medium Priority

### Object-Oriented Programming

- **Class Syntax Sugar**
  - Add syntactic sugar for class definitions
  - Keep the current closure-based implementation underneath
  - Support method definition syntax
  - Support instance creation

- **Inheritance and Polymorphism**
  - Add support for class inheritance
  - Implement method overriding
  - Support mixins and multiple inheritance patterns
  - Implement constructor inheritance

### Advanced Language Features

- **Hierarchical Module Structure**
  - Add support for package directories (with `__init__.m28` files)
  - Implement nested module resolution for `import` statements
  - Create a proper module path resolution system
  - Add support for relative imports

- **Context Managers**
  - Implement `with` statement syntax
  - Create context manager protocol (`__enter__`/`__exit__`)
  - Add built-in context managers for common resources (files, etc.)
  - Support user-defined context managers

- **Generators**
  - Implement `yield` statements
  - Support iterable protocol for generators
  - Add generator expressions
  - Implement generator methods (`send`, `throw`, etc.)

### Development Experience

- **REPL Improvements**
  - Add line editing and history
  - Implement tab completion
  - Add documentation lookup
  - Support persistent history

- **Error Reporting**
  - Enhance error messages with precise location information
  - Add suggestions for common errors
  - Improve traceback formatting

## Low Priority

### Performance Optimizations

- **Interpreter Performance**
  - Identify and fix performance bottlenecks
  - Optimize core data structures and operations
  - Add benchmarking tools

- **Module Loading**
  - Make module search paths configurable
  - Add support for a module path environment variable
  - Implement circular import detection and prevention
  - Add module reloading capability for development

### Advanced Features

- **Asynchronous Programming**
  - Add 'async/await' syntax
  - Support for coroutines and async I/O
  - Implement async context managers

- **Type System**
  - Add optional type hints
  - Implement runtime type checking
  - Add gradual typing support

- **Meta-programming**
  - Implement macros for syntax extension
  - Add reflection capabilities
  - Support compile-time meta-programming

## Completed Features

- ✅ Basic syntax and expression parsing
- ✅ Python-style dictionary literals (`{"key": value}`)
- ✅ Dictionary functions (get, dict) and operations
- ✅ Keyword arguments for functions via dictionaries
- ✅ Basic arithmetic and comparison operations
- ✅ Variable assignment with `=`
- ✅ Function definition with `def`
- ✅ Basic control flow (if, for, while)
- ✅ List operations and manipulation
- ✅ Basic modules and import functionality
- ✅ Dot notation for property and method access
- ✅ Simple try-except exception handling
- ✅ Object representation via closures

## Design Principles

The M28 development roadmap is guided by these core principles:

1. **Simplicity**: Keep the implementation simple and maintainable
2. **Pythonic**: Follow Python's conventions where appropriate
3. **Lisp-compatible**: Maintain compatibility with Lisp-style code
4. **Robust**: Include comprehensive error handling
5. **Extensible**: Design for future enhancements

## Implementation Notes

### Backward Compatibility

- All changes should maintain backward compatibility with existing M28 code
- Deprecated features should be marked clearly
- Migration paths should be provided for any breaking changes

### Documentation Requirements

- Each new feature must be documented in a user-friendly way
- Examples should be provided for all functionality
- Documentation should cover both basic and advanced use cases

---

This roadmap will be updated as features are implemented and priorities change.