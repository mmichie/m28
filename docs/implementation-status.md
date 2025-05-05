# Pythonic Lisp Implementation Status

This document outlines the current implementation status of Pythonic Lisp features as described in the [Pythonic Lisp Guide](./pythonic-lisp-guide.md). It highlights discrepancies between the documentation and the actual implementation.

## Fully Implemented Features

The following features are fully implemented as described in the documentation:

1. **Basic Syntax and Data Types**
   - Lisp-style parenthesized expressions
   - Core data types (numbers, strings, booleans, None)

2. **Variable Assignment**
   - `def` for variable definitions
   - `=` for assignment

3. **Functions**
   - Function definition via `def`
   - Lambda expressions
   - Closures with proper state isolation

4. **Control Flow**
   - `if/elif/else` conditionals
   - `for` loops with iterables
   - `while` loops
   - `break` and `continue` statements

5. **Collection Operations**
   - Lists and list operations
   - Dictionaries
   - Sets
   - Tuples

6. **Module System**
   - Basic `import` functionality
   - Module namespaces

## Partially Implemented Features

The following features are implemented differently or have limitations compared to what's described in the documentation:

1. **Object-Oriented Programming**
   - **Documentation**: Shows Python-like class definitions with inheritance
   - **Implementation**: Uses a dictionary and closure-based approach for object creation
   - Example from docs:
     ```lisp
     (class Dog ()
       (def (init self name)
         (def self.name name))
       (def (bark self)
         (print (+ self.name " says Woof!"))))
     ```
   - Actual implementation pattern:
     ```lisp
     (def (make-dog name)
       (= dog-name name)
       (= dog-obj (dict))
       (def (bark)
         (print (+ dog-name " says Woof!")))
       (= dog-obj (dict "name" dog-name "bark" bark))
       dog-obj)
     ```

2. **Exception Handling**
   - **Documentation**: Shows full try/except/finally blocks with exception types
   - **Implementation**: Basic try/except implemented but with limited exception type handling
   - Missing proper support for custom exception types and the full Python exception hierarchy

3. **List Comprehensions**
   - **Documentation**: Shows concise list comprehension syntax
   - **Implementation**: Supported but with slight syntax differences

## Missing Features

The following features described in the documentation are not fully implemented:

1. **Class System**
   - No direct implementation of Python-like class syntax
   - No class inheritance mechanism
   - No built-in support for standard object methods (`__init__`, `__str__`, etc.)

2. **Context Managers**
   - `with` statement syntax shown in documentation
   - Limited or no actual implementation of context managers

3. **Generators**
   - `yield` statements shown in documentation
   - Limited implementation of generator functionality 

4. **Asynchronous Programming**
   - `async/await` syntax described in documentation
   - No implementation found in codebase

5. **Standard Library**
   - Documentation references various standard library features
   - Limited set of built-in functions compared to Python

## Implementation Notes

The m28 interpreter takes a pragmatic approach to implementing Pythonic Lisp, focusing on core functionality:

1. **Object System**: Instead of implementing a full class-based object system, the interpreter uses a pattern of closures and dictionaries to create objects with state and behavior.

2. **Module System**: The module system loads .m28 files and exposes their definitions but doesn't implement the full Python package hierarchy.

3. **Built-ins**: The interpreter implements a subset of Python's built-in functions, focusing on the most commonly used ones.

## Recommendations

To bring the implementation closer to the documented language:

1. **Class Syntax Sugar**: Add syntactic sugar for class definitions that internally compiles to the current closure-based approach.

2. **Enhanced Exception System**: Expand the exception handling system to support more Python-like exception hierarchies and patterns.

3. **Context Manager Protocol**: Implement the context manager protocol to support `with` statements.

4. **Generator Functions**: Enhance `yield` to support full generator functionality.

5. **Documentation Updates**: Consider updating the documentation to more accurately reflect the current implementation approach for objects and classes.