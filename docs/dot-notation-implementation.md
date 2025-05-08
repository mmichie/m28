# Dot Notation Implementation in M28

This document describes the implementation of dot notation in M28, including the DotAccessible interface, property access, method calls, and nested property access.

## Overview

M28 supports Python-like dot notation for accessing properties and methods on objects. The implementation consists of several components:

1. **DotAccessible Interface**: A standardized interface for objects that support dot notation
2. **Symbol-based Dot Notation**: Support for expressions like `object.property`
3. **Functional Dot Notation**: Support for expressions like `(dot object "property")`
4. **Method Calls**: Support for method invocation like `(object.method object arg1 arg2)`
5. **Nested Property Access**: Support for accessing deep properties like `obj.a.b.c`

## DotAccessible Interface

The core of the dot notation implementation is the `DotAccessible` interface, defined in `core/types.go`:

```go
type DotAccessible interface {
    // HasProperty checks if the object has a property with the given name
    HasProperty(name string) bool
    
    // GetProperty retrieves a property from the object by name
    // Returns the property value and a boolean indicating if the property exists
    GetProperty(name string) (LispValue, bool)
    
    // SetProperty sets a property on the object
    // Returns an error if the property cannot be set (e.g., read-only object)
    SetProperty(name string, value LispValue) error
    
    // HasMethod checks if the object has a method with the given name
    HasMethod(name string) bool
    
    // CallMethod calls a method on the object with the given arguments
    // Returns the result of the method call or an error
    CallMethod(name string, args []LispValue) (LispValue, error)
}
```

This interface provides a standardized way for different object types to support dot notation, ensuring consistent behavior across the codebase.

## Symbol-Based Dot Notation

Symbol-based dot notation is handled in the evaluator's `Eval` method. When a symbol containing dots is encountered, it's parsed into components and the object's properties are accessed sequentially.

The implementation handles:
- Basic property access (`object.property`)
- Nested property access (`object.nested.property`)
- Method references (`object.method`)
- Special cases for builtins like `dict.keys`

## Functional Dot Notation

Functional dot notation is implemented through the `dot` and `.` special forms, defined in `special_forms/dot_fixed.go`. These forms allow property access and method calls through a function-like syntax:

```lisp
(dot object "property")
(. object "property")
```

The functional form is especially useful when the property name needs to be computed or for nested property access in a single expression:

```lisp
(dot object prop1 prop2 prop3)  ; Access obj.prop1.prop2.prop3
```

## Method Calls

Method calls via dot notation involve two steps:
1. Retrieving the method from the object
2. Applying the method with the object as first argument

For objects implementing the DotAccessible interface, the `CallMethod` function handles method invocation. For other objects, type-specific handling is employed.

## Module Dot Notation

Modules are implemented as dictionaries with special handling. When a module is imported, a dot handler is registered to enable dot notation for accessing module attributes and functions.

For example, in `special_forms/module.go`:
```go
// Create a dot handler for the module to allow attribute access
env.Define(core.LispSymbol(moduleBaseName+".__dot__"), core.BuiltinFunc(func(args []core.LispValue, env core.Environment) (core.LispValue, error) {
    // Handler implementation for module property access and method calls
}))
```

## Error Handling

Standardized error messages for dot notation operations are defined in `core/dot_errors.go`. This ensures consistent error reporting across different components of the dot notation implementation.

## Testing

Test cases for dot notation are provided in `tests/dot-notation-test.m28`. These tests verify:
- Property access on dictionaries, modules, and custom objects
- Method calls with and without arguments
- Nested property access
- Error handling

## Future Improvements

Potential future improvements for the dot notation implementation:
1. Implement a cleaner approach for binding method references (creating bound methods)
2. Add support for operator overloading via dot notation
3. Optimize nested property lookups
4. Extend DotAccessible to more core types