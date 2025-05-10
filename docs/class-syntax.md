# M28 Class Syntax

This document describes the object-oriented programming features in M28, focusing on the improved class syntax.

## Introduction

M28 provides a class-based object-oriented programming system that combines Lisp's flexibility with Python-like syntax and semantics. The implementation uses closures and dictionaries under the hood, but presents a clean, intuitive interface to the programmer.

## Class Definition

Classes are defined using the `class` special form:

```lisp
(class ClassName (ParentClass1 ParentClass2 ...)
  # Class attributes
  (= attribute1 value1)
  (= attribute2 value2)
  
  # Methods
  (def (method1 self arg1 arg2)
    # Method body
    )
  
  (def (method2 self)
    # Another method
    )
)
```

### Key Components:

- **Class Name**: A symbol naming the class
- **Parent Classes**: Optional list of parent classes for inheritance
- **Class Attributes**: Defined using the assignment form `(= name value)`
- **Methods**: Defined using the `def` special form with `self` as the first parameter

## Object Creation and Access

Objects are created by calling the class name as a function:

```lisp
(= obj (ClassName arg1 arg2))
```

The arguments are passed to the `init` method of the class.

### Attribute Access

Attributes can be accessed using dot notation:

```lisp
# Get an attribute
obj.attribute

# Set an attribute
(= obj.attribute new-value)
```

### Method Calls

Methods are called using dot notation:

```lisp
# Call a method without arguments
(obj.method)

# Call a method with arguments
(obj.method arg1 arg2)
```

## Constructor Method

The special `init` method serves as the constructor:

```lisp
(def (init self arg1 arg2)
  (= self.attribute1 arg1)
  (= self.attribute2 arg2))
```

If a class doesn't define an `init` method, a default one is used that doesn't take any arguments.

## Inheritance

M28 supports single and multiple inheritance. When a method is called, it is looked up first in the instance, then in the class, and finally in the parent classes in the order they were specified.

### Using the `super` Function

The `super` function is used to call methods from parent classes:

```lisp
(def (method self arg1)
  # Call parent's method
  (dot (super self) "method" arg1)
  # Additional code
  )
```

## Example: Simple Person Class

```lisp
(class Person ()
  # Class attributes (default values)
  (= name "Anonymous")
  (= age 0)
  
  # Constructor
  (def (init self new-name new-age)
    (= self.name new-name)
    (= self.age new-age))
  
  # Methods
  (def (greet self)
    (+ "Hello, my name is " self.name))
    
  (def (birthday self)
    (= self.age (+ self.age 1))
    (+ self.name " is now " (str self.age) " years old!"))
)

# Create an instance
(= alice (Person "Alice" 30))

# Access attributes
(print alice.name)  # Alice

# Call methods
(print (alice.greet))  # Hello, my name is Alice
```

## Example: Inheritance

```lisp
(class Employee (Person)
  # Additional attributes
  (= job "Unemployed")
  (= salary 0)
  
  # Override constructor
  (def (init self name age job salary)
    # Call parent constructor
    (dot (super self) "init" name age)
    # Initialize additional attributes
    (= self.job job)
    (= self.salary salary))
  
  # New method
  (def (work self)
    (+ self.name " is working as a " self.job))
  
  # Override method
  (def (greet self)
    (+ (dot (super self) "greet") " and I work as a " self.job))
)

# Create an employee
(= bob (Employee "Bob" 25 "Developer" 75000))

# Call methods
(print (bob.greet))  # Hello, my name is Bob and I work as a Developer
```

## Best Practices

1. **Class Names**: Use CamelCase for class names
2. **Method Names**: Use kebab-case for method names (like-this)
3. **Private Methods**: Prefix with underscore (_method) to indicate private methods
4. **Initialization**: Always initialize all attributes in the `init` method
5. **Self Parameter**: Always use `self` as the first parameter in methods for consistency

## Implementation Details

Under the hood, M28 classes are implemented using:

1. A class object that stores class attributes and method definitions
2. Instance objects that store instance-specific attributes
3. Method lookup that checks the instance, then the class, then parent classes
4. Dynamic binding of `self` to make dot notation and method calls work properly

The implementation is designed to balance performance with flexibility, providing a natural and intuitive object-oriented programming experience in M28.

## Advanced Features

M28's object system supports several advanced features:

1. **Multiple Inheritance**: A class can inherit from multiple parent classes
2. **Method Overriding**: Child classes can override methods from parent classes
3. **Dynamic Attribute Access**: Attributes can be accessed and modified at runtime
4. **Dot Notation**: Python-like dot notation for attribute and method access
5. **Built-in Reflection**: Functions like `hasattr` and `getattr` for reflection