# Dot Notation in M28

M28 supports Python-like dot notation for accessing properties and methods on objects. This document provides a comprehensive guide to understanding and using dot notation in M28.

## Contents
1. [Introduction](#introduction)
2. [Basic Property Access](#basic-property-access)
3. [Method Calls](#method-calls)
4. [Comparing with Functional Style](#comparing-with-functional-style)
5. [Creating Objects with Methods](#creating-objects-with-methods)
6. [Implementing Classes with Dot Notation](#implementing-classes-with-dot-notation)
7. [Functional Dot Notation](#functional-dot-notation)
8. [Dot Notation with Modules](#dot-notation-with-modules)
9. [Implementation Details](#implementation-details)
10. [Best Practices](#best-practices)

## Introduction

Dot notation allows you to access properties and methods of objects using a syntax similar to Python:

```lisp
object.property    # Access a property
(object.method arg1 arg2)  # Call a method
```

This syntax works with dictionaries, modules, and custom objects that implement the `DotAccessible` interface.

## Basic Property Access

Dot notation allows you to access properties of dictionaries, modules, and other objects using the `.` syntax:

```lisp
; Dictionary property access
(= person {"name" "Alice" "age" 30})
(print person.name)  ; Outputs: Alice

; Module property access
(import "math")
(print math.pi)      ; Outputs: 3.14159...

; Nested property access
(= data {"user" {"profile" {"email" "user@example.com"}}})
(print data.user.profile.email)  ; Outputs: user@example.com
```

## Method Calls

You can call methods on objects using dot notation:

```lisp
; Dictionary methods
(= dict {"a" 1 "b" 2})
(print (dict.get "c" "default"))  ; Outputs: default
(print (dict.keys))               ; Outputs: ["a", "b"]

; Method with arguments
(= numbers [1 2 3 4 5])
(print (numbers.count 3))         ; Outputs: 1

; Method chaining (when implemented)
(= result (dict.set "c" 3).get "c")
(print result)                   ; Outputs: 3
```

## Comparing with Functional Style

Dot notation is often more readable than the equivalent functional style, especially for deeply nested access:

```lisp
; Functional style
(= data {"config" {"settings" {"timeout" 30}}})
(= timeout (get (get (get data "config") "settings") "timeout"))

; Equivalent dot notation
(= timeout data.config.settings.timeout)
```

## Creating Objects with Methods

You can create objects with methods that are accessible via dot notation:

```lisp
(= counter {
  "count" 0
  "increment" (lambda (self)
    (= self.count (+ self.count 1))
    self.count
  )
  "reset" (lambda (self)
    (= self.count 0)
  )
})

(print (counter.increment counter))  ; Outputs: 1
(print (counter.increment counter))  ; Outputs: 2
(counter.reset counter)
(print counter.count)               ; Outputs: 0
```

## Implementing Classes with Dot Notation

You can create class-like patterns with constructor functions:

```lisp
(def make-person (lambda (name age)
  (def person {
    "name" name
    "age" age
    "greet" (lambda (self)
      (+ "Hello, my name is " self.name "!")
    )
    "birthday" (lambda (self)
      (= self.age (+ self.age 1))
      self.age
    )
  })
  person
))

(= alice (make-person "Alice" 30))
(print (alice.greet alice))         ; Outputs: Hello, my name is Alice!
(print (alice.birthday alice))      ; Outputs: 31
(print alice.age)                  ; Outputs: 31
```

## Functional Dot Notation

Besides the symbolic dot notation (`object.property`), M28 also supports a functional form for dot notation:

```lisp
; Symbolic form
(print person.name)

; Functional form
(print (. person "name"))
; or
(print (dot person "name"))
```

The functional form is useful when the property name needs to be computed:

```lisp
(= prop "name")
(print (. person prop))  ; Outputs: Alice
```

## Dot Notation with Modules

When working with modules, dot notation provides a clean way to access exported functions and values:

```lisp
(import "examples/math_utils")

; Access module functions
(print (math_utils.add 5 3))          ; Outputs: 8
(print (math_utils.multiply 4 2))     ; Outputs: 8

; Access module constants
(print math_utils.VERSION)            ; Outputs: the module version
```

## Implementation Details

### DotAccessible Interface

The core of the dot notation implementation is the `DotAccessible` interface:

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

Objects that implement this interface can be used with dot notation. The M28 interpreter implements this interface for dictionaries, modules, and other built-in types.

### Symbol-Based Vs. Functional Dot Notation

M28 provides two ways to use dot notation:

1. **Symbol-based Dot Notation**: Used when the property name is a literal (e.g., `object.property`)
2. **Functional Dot Notation**: Used when the property name is computed or for nested access (e.g., `(dot object prop)`)

Both approaches provide the same functionality, but they serve different use cases.

## Best Practices

1. **Use dot notation for readability** - especially with deeply nested structures
2. **Prefer symbolic form** (`obj.prop`) for static property names
3. **Use functional form** (`(. obj prop)`) when the property name is dynamic
4. **Pass the object as first argument** to method calls: `(obj.method obj arg1 arg2)`
5. **Implement the DotAccessible interface** for custom types if they should support dot notation

### Object Method Call Convention

When calling methods on objects, you need to pass the object itself as the first argument:

```lisp
; Define an object with a method
(= counter {"count" 0, "increment" (lambda (self) (+ self.count 1))})

; Call the method, passing the object as the first argument
(counter.increment counter)
```

This is because methods are not automatically bound to their objects in M28 (unlike in Python). This convention simulates the behavior of the `self` parameter in Python methods.