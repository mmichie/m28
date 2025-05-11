# Direct Property Access in M28

This document outlines the design and implementation of direct property access in M28 using the Object Protocol system.

## Overview

Direct property access allows accessing object properties through a clean, consistent interface. 
Instead of using various access methods depending on the object type, the Object Protocol 
provides a unified way to get and set properties.

## Implementation

The implementation consists of several key components:

1. **Object Protocol Interface** (`core/obj_protocol.go`):
   - Defines `ObjProtocol` for uniform property access
   - Provides `AdaptableLispValue` interface for adapting existing types

2. **Helper Functions**:
   - `DirectGetProp` for efficient direct property access
   - `DirectSetProp` for efficient direct property setting

3. **Enhanced Accessors** (`core/enhanced_access.go`):
   - Wraps the Object Protocol in user-friendly functions
   - Allows using direct property access in code

4. **Dot Notation Handling** (`special_forms/dot.go`):
   - Special form handlers for dot notation
   - Makes property access more intuitive

## Usage Patterns

The current M28 implementation works well with the following patterns:

### 1. Using dict.get/dict.set for Property Access

```lisp
# Create an object
(def obj (MyClass "value"))

# Get property
(def value (dict.get obj "property"))

# Set property
(dict.set obj "property" new-value)
```

### 2. Helper Functions for Direct Access

Creating helper functions makes property access cleaner:

```lisp
# Define helper functions
(def (get-prop obj name)
  (dict.get obj name))
  
(def (set-prop obj name value)
  (dict.set obj name value)
  value)

# Use helpers
(get-prop obj "property")
(set-prop obj "property" new-value)
```

### 3. Class Methods Using Properties

```lisp
(class Person ()
  # Constructor
  (def (init self name age)
    (dict.set self "name" name)
    (dict.set self "age" age))
  
  # Method using properties
  (def (birthday self)
    (dict.set self "age" (+ (dict.get self "age") 1))
    (print "Happy Birthday! Now" (dict.get self "name") 
           "is" (dict.get self "age") "years old"))
)
```

## Future Improvements

Future improvements to the direct property access system could include:

1. **Simplified Syntax**: A more concise syntax for property access (p.x)
2. **Dot Notation Enhancement**: Better handling of nested properties
3. **Type-Specific Optimization**: Specialized handling for common types
4. **Method Binding**: More seamless method binding using property access

## Implementation Status

The current implementation provides:

- A unified interface for property access via the Object Protocol
- Direct property access functions in the core package
- Adapters for different object types
- Special form handling for dot notation

Standard patterns using `dict.get` and `dict.set` work reliably and are recommended for accessing object properties.