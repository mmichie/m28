# Dot Notation in M28

M28 supports Python-like dot notation for accessing properties and methods on objects. This document provides examples and best practices for using dot notation in M28 code.

## Basic Property Access

Dot notation allows you to access properties of dictionaries, modules, and other objects using the `.` syntax:

```python
; Dictionary property access
(def person {"name" "Alice" "age" 30})
(print person.name)  ; Outputs: Alice

; Module property access
(import "math")
(print math.pi)      ; Outputs: 3.14159...

; Nested property access
(def data {"user" {"profile" {"email" "user@example.com"}}})
(print data.user.profile.email)  ; Outputs: user@example.com
```

## Method Calls

You can call methods on objects using dot notation:

```python
; Dictionary methods
(def dict {"a" 1 "b" 2})
(print (dict.get "c" "default"))  ; Outputs: default
(print (dict.keys))               ; Outputs: ["a", "b"]

; Method with arguments
(def numbers [1 2 3 4 5])
(print (numbers.count 3))         ; Outputs: 1

; Method chaining
(def result (dict.set "c" 3).get "c")
(print result)                   ; Outputs: 3
```

## Comparing with Functional Style

Dot notation is often more readable than the equivalent functional style, especially for deeply nested access:

```python
; Functional style
(def data {"config" {"settings" {"timeout" 30}}})
(def timeout (get (get (get data "config") "settings") "timeout"))

; Equivalent dot notation
(def timeout data.config.settings.timeout)
```

## Creating Objects with Methods

You can create objects with methods that are accessible via dot notation:

```python
(def counter {
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

```python
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

(def alice (make-person "Alice" 30))
(print (alice.greet alice))         ; Outputs: Hello, my name is Alice!
(print (alice.birthday alice))      ; Outputs: 31
(print alice.age)                  ; Outputs: 31
```

## The Functional Dot Notation

Besides the symbolic dot notation (`object.property`), M28 also supports a functional form for dot notation:

```python
; Symbolic form
(print person.name)

; Functional form
(print (. person "name"))
; or
(print (dot person "name"))
```

The functional form is useful when the property name needs to be computed:

```python
(def prop "name")
(print (. person prop))  ; Outputs: Alice
```

## Dot Notation with Modules

When working with modules, dot notation provides a clean way to access exported functions and values:

```python
(import "examples/math_utils")

; Access module functions
(print (math_utils.add 5 3))          ; Outputs: 8
(print (math_utils.multiply 4 2))     ; Outputs: 8

; Access module constants
(print math_utils.VERSION)            ; Outputs: the module version
```

## Best Practices

1. **Use dot notation for readability** - especially with deeply nested structures
2. **Prefer symbolic form** (`obj.prop`) for static property names
3. **Use functional form** (`(. obj prop)`) when the property name is dynamic
4. **Pass the object as first argument** to method calls: `(obj.method obj arg1 arg2)`
5. **Implement the DotAccessible interface** for custom types if they should support dot notation

## Internal Implementation

For developers extending M28, objects can support dot notation by implementing the `DotAccessible` interface:

```go
type DotAccessible interface {
    HasProperty(name string) bool
    GetProperty(name string) (LispValue, bool)
    SetProperty(name string, value LispValue) error
    HasMethod(name string) bool
    CallMethod(name string, args []LispValue) (LispValue, error)
}
```

This interface ensures consistent behavior across different object types.