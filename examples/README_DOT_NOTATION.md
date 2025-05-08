# Dot Notation Examples

This directory contains examples demonstrating the use of dot notation in M28. 
Dot notation allows for a more intuitive and Pythonic way to access properties and methods on objects.

## Key Examples

### Basic Dictionary Dot Notation
- **dict_dot_example.m28**: Shows basic usage of dot notation with dictionaries, including property access, method calls, and nested properties.

### Module Dot Notation
- **module_dot_example.m28**: An example module that exports properties and methods.
- **module_import_test.m28**: Shows how to import a module and access its properties and methods using dot notation.

### Object Dot Notation
- **object_dot_example.m28**: Demonstrates using dictionaries with methods to create object-like structures with dot notation.

### Comprehensive Test Suite
- **dot_notation_test.m28**: A full test suite covering various aspects of dot notation across different object types.

## How Dot Notation Works

M28 supports two main forms of dot notation:

1. **Symbol-based Dot Notation**: Uses object.property syntax
   ```lisp
   person.name         ; Access property
   (person.greet self) ; Call method
   ```

2. **Functional Dot Notation**: Uses (dot object property) syntax
   ```lisp
   (. person "name")     ; Access property
   (dot person "name")   ; Alternative syntax
   ```

## Implementation Details

Objects can support dot notation by implementing the `DotAccessible` interface, which includes methods like:
- HasProperty
- GetProperty
- SetProperty
- HasMethod
- CallMethod

These methods allow for consistent behavior across different object types.

## Running the Examples

You can run these examples using the M28 interpreter:

```
./bin/m28 examples/dict_dot_example.m28
./bin/m28 examples/module_import_test.m28
./bin/m28 examples/object_dot_example.m28
./bin/m28 examples/dot_notation_test.m28
```

Start with the simpler examples before moving to the more comprehensive test suite.