# Object Protocol and Property Access in M28

This document outlines the design and implementation of the unified Object Protocol system in M28,
including direct property access, method calling, and inheritance support.

## Overview

The Object Protocol system provides a clean, consistent interface for interacting with objects
in M28. It unifies property access and method calling across different types, making object-oriented
programming more intuitive and powerful.

## Implementation

The implementation consists of several key components:

1. **Unified Object Protocol Interface** (`core/obj_protocol.go`):
   - Defines `ObjProtocol` for uniform property access and method calling
   - Provides `AdaptableLispValue` interface for adapting existing types
   - Implements fast optimized access paths for common types

2. **Type Adapters**:
   - Adapters for built-in types (lists, strings, tuples, sets, etc.)
   - Adapters for custom types (PythonicObject, SuperObject, etc.)
   - Performance-optimized adapter cache for faster property access

3. **Enhanced Accessors** (`core/enhanced_access.go`):
   - Wraps the Object Protocol in user-friendly functions
   - Provides efficient dot notation for property access and method calls

4. **Direct Access API** (`core/direct_access.go`):
   - Intuitive helper functions like `getattr`, `setattr`, `hasattr`
   - Support for nested property access and item access
   - Python-like attribute handling for familiarity

5. **Super Method Calling** (`core/pythonic_class.go`):
   - Proper method inheritance with `super()`
   - Support for multi-level inheritance
   - Correct binding of `self` in inherited methods

## Recommended Usage Patterns

The M28 Object Protocol system now supports these preferred usage patterns:

### 1. Using getattr/setattr for Property Access

```lisp
# Create an object
(def person (dict "name" "Alice" "age" 30))

# Get property with default value
(def job (getattr person "job" "Unknown"))

# Set property
(setattr person "job" "Developer")

# Check if an attribute exists
(if (hasattr person "email")
    (print "Email:" (getattr person "email"))
    (print "No email found"))
```

### 2. Direct Dot Notation Access

```lisp
# Create a point object
(def p (Point 10 20))

# Access properties via dot notation
(print "Coordinates:" p.x p.y)

# Call methods via dot notation
(p.move 5 5)
(print "New position:" p.x p.y)

# Method chaining with dot notation
(p.move 1 1).normalize().scale 2)
```

### 3. Object-Oriented Programming with Inheritance

```lisp
# Base class
(class Animal ()
  # Constructor
  (def (init self name)
    (setattr self "name" name)
    (setattr self "species" "Unknown"))
  
  # Methods
  (def (speak self)
    (print (getattr self "name") "makes a sound"))
)

# Derived class with super() method calling
(class Dog (Animal)
  # Override constructor
  (def (init self name breed)
    # Call parent constructor
    ((getattr (super self) "init") name)
    (setattr self "breed" breed)
    (setattr self "species" "Dog"))
  
  # Override method
  (def (speak self)
    # First call parent method
    ((getattr (super self) "speak") self)
    # Then add specific behavior
    (print (getattr self "name") "the" 
           (getattr self "breed") "barks"))
)
```

### 4. Accessing Collection Items

```lisp
# Working with lists and dictionaries
(def numbers [10 20 30 40 50])
(def person (dict "name" "Alice" "age" 30))

# Get items
(print "Third number:" (getitem numbers 2))
(print "Person's name:" (getitem person "name"))

# Set items
(setitem numbers 2 99)
(setitem person "city" "New York")

# Delete items
(delitem person "age")
```

### 5. Using Object Methods

```lisp
# Get a method reference and call it
(def speak-method (getattr dog "speak"))
(speak-method dog)

# Work with built-in type methods
(def my-list [1 2 3])
(def append-method (getattr my-list "append"))
(def new-list (append-method 4))
(print new-list)  # [1 2 3 4]

# String methods
(def my-string "hello, world")
(print ((getattr my-string "upper")))  # "HELLO, WORLD"
```

## Best Practices

1. **Prefer Helper Functions**: Use `getattr`/`setattr` over direct dictionary access for clarity
2. **Use Dot Notation**: When available, dot notation is more readable for property access
3. **Leverage Type Methods**: All built-in types now expose useful methods via the object protocol
4. **Proper Inheritance**: Always use `super()` to call parent methods in derived classes
5. **Adapt Custom Types**: Implement `AdaptableLispValue` for custom types to work with the protocol

## Object-Oriented Programming in M28

M28 now provides a robust object-oriented programming system with:

1. **Class Definitions**: Create classes with properties and methods
2. **Inheritance**: Derive classes from parent classes to share behavior
3. **Method Overriding**: Override parent methods while still being able to call them
4. **Property Access**: Get/set properties via dot notation or helper functions
5. **Built-in Type Methods**: Access methods on lists, strings, tuples, etc.

## Performance Considerations

The Object Protocol system is designed for performance:

1. **Type Caching**: Repetitive access to the same type is optimized
2. **Fast Paths**: Common types have specialized fast paths for property access
3. **Reduced Indirection**: Method calling minimizes the number of function calls
4. **Optimized Access**: Direct property access for better performance

## Example Usage

See these examples for detailed usage patterns:

- `examples/enhanced_property_access.m28`: Shows the new property access helpers
- `examples/enhanced_object_protocol.m28`: Demonstrates method calling on built-in types
- `examples/oop/05_super_method_calling.m28`: Inheritance with proper super method calling