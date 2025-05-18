# Unified Object Protocol Design

## 1. Overview

This document outlines a complete redesign of the object system in M28 to unify and simplify property access, method invocation, and class behavior. The current implementation has accumulated technical debt with multiple overlapping interfaces and access patterns, making the code hard to maintain and extend.

## 2. Objectives

- Create a single, coherent object protocol
- Eliminate redundant interfaces and duplicate code
- Simplify property access and method invocation
- Make the class system more Pythonic and intuitive
- Improve performance by reducing indirection

## 3. Current Problems

### Multiple Inconsistent Interfaces

- `DotAccessible`: Basic property/method access interface
- `ObjProtocol`: Newer property access system
- `EvaluatorAware`: Property/method access with evaluator context
- `MemberAccessor`: Yet another member access system

### Confusing Access Patterns

- Multiple helper functions: `GetPropFrom`, `DirectGetProp`, `SetPropOn`
- Special type handling for `PythonicDict`, `PythonicObject`, `Generator`
- Inconsistent method signatures and parameter ordering
- Complex fallback chains

### Code Duplication

- Similar code in dot notation handler, method dispatchers
- Type-specific handling logic repeated across multiple files
- Redundant error formation and handling

## 4. Proposed Solution

### 4.1 Core Interface: `ObjectProtocol`

```go
// ObjectProtocol is the unified interface for all objects
type ObjectProtocol interface {
    // Get an attribute (property or method)
    GetAttribute(name string, eval Evaluator, env Environment) (LispValue, error)
    
    // Set an attribute value
    SetAttribute(name string, value LispValue, eval Evaluator, env Environment) error
    
    // Call a method
    CallMethod(name string, args []LispValue, eval Evaluator, env Environment) (LispValue, error)
    
    // Check if an attribute exists
    HasAttribute(name string) bool
}
```

### 4.2 Base Implementation

```go
// BaseObject provides a standard implementation of ObjectProtocol
type BaseObject struct {
    attributes map[string]LispValue
    methods    map[string]Method
    class      *Class
}

// Method represents a callable object method
type Method interface {
    // Call the method with the given receiver and arguments
    Call(receiver LispValue, args []LispValue, eval Evaluator, env Environment) (LispValue, error)
}
```

### 4.3 Class System

```go
// Class represents a type definition
type Class struct {
    name       string
    parent     *Class
    attributes map[string]LispValue
    methods    map[string]Method
}

// NewClass creates a new class
func NewClass(name string, parent *Class) *Class {
    return &Class{
        name:       name,
        parent:     parent,
        attributes: make(map[string]LispValue),
        methods:    make(map[string]Method),
    }
}

// CreateInstance creates a new instance of this class
func (c *Class) CreateInstance(args []LispValue, eval Evaluator, env Environment) (LispValue, error) {
    // Create object
    obj := &BaseObject{
        attributes: make(map[string]LispValue),
        methods:    make(map[string]Method),
        class:      c,
    }
    
    // Call constructor if it exists
    if initMethod, ok := c.methods["init"]; ok {
        _, err := initMethod.Call(obj, args, eval, env)
        if err != nil {
            return nil, err
        }
    }
    
    return obj, nil
}
```

### 4.4 Method Implementation

```go
// UserMethod represents a user-defined method
type UserMethod struct {
    parameters []LispSymbol
    body       LispValue
    defaults   map[LispSymbol]LispValue
    closure    Environment
}

// Call implements the Method interface
func (m *UserMethod) Call(receiver LispValue, args []LispValue, eval Evaluator, env Environment) (LispValue, error) {
    // Create a new environment for the method call
    methodEnv := env.NewEnvironment(m.closure)
    
    // Bind the receiver to "self"
    methodEnv.Define("self", receiver)
    
    // Bind arguments to parameters
    if err := bindArguments(m.parameters, args, m.defaults, methodEnv); err != nil {
        return nil, err
    }
    
    // Evaluate the method body
    return eval.Eval(m.body, methodEnv)
}
```

### 4.5 Dot Notation Handler

```go
// DotHandler handles dot notation in expressions
func DotHandler(obj LispValue, attribute string, args []LispValue, eval Evaluator, env Environment) (LispValue, error) {
    // Get the object protocol
    objProto, ok := obj.(ObjectProtocol)
    if !ok {
        return nil, fmt.Errorf("object does not support dot notation: %T", obj)
    }
    
    // If we have arguments, it's a method call
    if len(args) > 0 {
        return objProto.CallMethod(attribute, args, eval, env)
    }
    
    // Otherwise it's an attribute access
    return objProto.GetAttribute(attribute, eval, env)
}
```

## 5. Type Implementations

### 5.1 PythonicDict

```go
// PythonicDict implements ObjectProtocol
func (d *PythonicDict) GetAttribute(name string, eval Evaluator, env Environment) (LispValue, error) {
    // Check for method
    if method, ok := d.methods[name]; ok {
        return &BoundMethod{
            method:   method,
            receiver: d,
            eval:     eval,
            env:      env,
        }, nil
    }
    
    // Check for property
    if value, ok := d.data[name]; ok {
        return value, nil
    }
    
    return nil, fmt.Errorf("dict has no attribute '%s'", name)
}

func (d *PythonicDict) SetAttribute(name string, value LispValue, eval Evaluator, env Environment) error {
    d.data[name] = value
    return nil
}

func (d *PythonicDict) CallMethod(name string, args []LispValue, eval Evaluator, env Environment) (LispValue, error) {
    method, ok := d.methods[name]
    if !ok {
        return nil, fmt.Errorf("dict has no method '%s'", name)
    }
    
    return method.Call(d, args, eval, env)
}

func (d *PythonicDict) HasAttribute(name string) bool {
    _, hasMethod := d.methods[name]
    _, hasProperty := d.data[name]
    return hasMethod || hasProperty
}
```

### 5.2 Class Implementation

```go
// Implement ObjectProtocol for Class
func (c *Class) GetAttribute(name string, eval Evaluator, env Environment) (LispValue, error) {
    // Check for class method
    if method, ok := c.methods[name]; ok {
        return &BoundMethod{
            method:   method,
            receiver: c,
            eval:     eval,
            env:      env,
        }, nil
    }
    
    // Check for class attribute
    if attr, ok := c.attributes[name]; ok {
        return attr, nil
    }
    
    // Check parent class
    if c.parent != nil {
        return c.parent.GetAttribute(name, eval, env)
    }
    
    return nil, fmt.Errorf("class '%s' has no attribute '%s'", c.name, name)
}
```

## 6. Built-in Types

### 6.1 String

```go
// StringObj implements ObjectProtocol for strings
type StringObj struct {
    value string
}

// Standard methods for strings
var stringMethods = map[string]Method{
    "upper": &BuiltinMethod{
        fn: func(receiver LispValue, args []LispValue, eval Evaluator, env Environment) (LispValue, error) {
            str := receiver.(*StringObj).value
            return &StringObj{value: strings.ToUpper(str)}, nil
        },
    },
    "lower": &BuiltinMethod{
        fn: func(receiver LispValue, args []LispValue, eval Evaluator, env Environment) (LispValue, error) {
            str := receiver.(*StringObj).value
            return &StringObj{value: strings.ToLower(str)}, nil
        },
    },
    // ... other string methods
}
```

## 7. Special Form Handling

### 7.1 Class Definition

```go
// HandleClassDefinition implements the 'class' special form
func HandleClassDefinition(args []LispValue, eval Evaluator, env Environment) (LispValue, error) {
    if len(args) < 2 {
        return nil, errors.New("class: not enough arguments")
    }
    
    // Get class name
    className, ok := args[0].(LispSymbol)
    if !ok {
        return nil, errors.New("class: first argument must be a symbol")
    }
    
    // Get parent classes (inheritance)
    var parentClass *Class
    if parentList, ok := args[1].(LispList); ok && len(parentList) > 0 {
        // Get first parent class for now (single inheritance)
        parentName, ok := parentList[0].(LispSymbol)
        if !ok {
            return nil, errors.New("class: parent must be a symbol")
        }
        
        // Look up parent class
        parentValue, exists := env.Get(parentName)
        if !exists {
            return nil, fmt.Errorf("class: parent '%s' not found", parentName)
        }
        
        parentClass, ok = parentValue.(*Class)
        if !ok {
            return nil, fmt.Errorf("class: '%s' is not a class", parentName)
        }
    }
    
    // Create the new class
    class := NewClass(string(className), parentClass)
    
    // Process class body (methods and attributes)
    for i := 2; i < len(args); i++ {
        expr := args[i]
        
        // Handle method definition
        if list, ok := expr.(LispList); ok && len(list) >= 3 {
            if defSym, ok := list[0].(LispSymbol); ok && defSym == "def" {
                if err := processMethodDefinition(class, list, eval, env); err != nil {
                    return nil, err
                }
            }
        }
        
        // Handle attribute assignment
        if list, ok := expr.(LispList); ok && len(list) == 3 {
            if assignSym, ok := list[0].(LispSymbol); ok && assignSym == "=" {
                if err := processAttributeDefinition(class, list, eval, env); err != nil {
                    return nil, err
                }
            }
        }
    }
    
    // Register the class in the environment
    env.Define(className, class)
    
    return class, nil
}
```

## 8. Implementation Plan

1. **Phase 1: Core Interfaces**
   - Define the `ObjectProtocol` interface
   - Implement `BaseObject` and `Method` types
   - Create the `Class` implementation

2. **Phase 2: Built-in Types**
   - Reimplement `PythonicDict` to use the new protocol
   - Add protocol support to common types (String, List, etc.)
   - Implement the `BoundMethod` type

3. **Phase 3: Evaluation Integration**
   - Modify the evaluator to use the new object protocol
   - Implement the dot notation handler
   - Update special forms for class definition

4. **Phase 4: Testing and Optimization**
   - Create comprehensive test suite
   - Benchmark performance
   - Optimize critical paths

## 9. Benefits

- **Simplified Architecture**: One clear protocol for all object interactions
- **Improved Performance**: Direct method calls without excessive indirection
- **Better Developer Experience**: More Pythonic class and object behavior
- **Easier Maintenance**: Cleaner code organization and less duplication
- **Future Extensibility**: Solid foundation for adding advanced features like decorators and metaclasses

## 10. Dictionary Implementation Issues and Solutions

### Current Dictionary Issues

During testing, we identified several specific issues with the dictionary implementation:

1. **Dictionary Variable Persistence**: Dictionary literals created with `{}` can't be reliably accessed in subsequent expressions, suggesting environment persistence issues.

2. **Dictionary Function vs. Literals Inconsistency**: The `dict` function and `{}` literals have inconsistent behavior.

3. **Object Protocol Implementation Gaps**: Some aspects of the object protocol aren't correctly implemented for dictionaries.

4. **Evaluator Reference Missing**: Dictionaries aren't always properly initialized with evaluator references.

### Specific Solutions

#### Fix Dictionary Evaluation

In `evalDict` function, ensure dictionaries are properly registered with the evaluator:

```go
func (e *Evaluator) evalDict(dict *core.PythonicDict, env core.Environment) (*core.PythonicDict, error) {
    result := core.NewPythonicDict()
    
    // Ensure the dictionary has a reference to the evaluator
    result.SetEvaluator(e)
    
    // Evaluate dictionary contents
    err := dict.Iterate(func(k, v core.LispValue) error {
        // [existing key/value evaluation logic]
        return nil
    })
    
    if err != nil {
        return nil, err
    }
    
    return result, nil
}
```

#### Improve Environment-Evaluator Linkage

Enhance the environment to properly handle complex types:

```go
// Update Environment to maintain evaluator reference
type Environment struct {
    vars      map[core.LispSymbol]core.LispValue
    outer     core.Environment
    evaluator core.Evaluator 
}

// Set properly handles evaluator-aware types
func (e *Environment) Set(symbol core.LispSymbol, value core.LispValue) {
    // Special handling for evaluator-aware types
    if evalAware, ok := value.(core.EvaluatorAware); ok && e.evaluator != nil {
        evalAware.SetEvaluator(e.evaluator)
    }
    
    e.vars[symbol] = value
}
```

#### Fix Dictionary Method Access

Ensure dictionary methods are properly bound:

```go
// Ensure method binding works properly
func (d *PythonicDict) GetMember(name string, eval Evaluator, env Environment) (LispValue, error) {
    // Store evaluator for future use
    d.SetEvaluator(eval)
    
    // Check for methods first
    if d.HasMethod(name) {
        // Wrap method in a properly bound function
        method, _ := d.methods[name]
        return BuiltinFunc(func(args []LispValue, callEnv Environment) (LispValue, error) {
            return method(d, args)
        }), nil
    }
    
    // Check for pseudo-properties
    switch name {
    case "length", "len", "size", "count":
        return float64(d.Size()), nil
    }
    
    // Check for attributes
    if value, exists := d.Get(name); exists {
        return value, nil
    }
    
    return nil, fmt.Errorf("dict has no attribute '%s'", name)
}
```

#### Initialization Sequence Improvements

Fix initialization sequence to ensure dependencies are properly set up:

```go
// Initialize in the correct order
func initialize() {
    // 1. Create evaluator
    evaluator := eval.NewEvaluator()
    
    // 2. Create environment with evaluator
    environment := env.NewEnvironment(nil)
    environment.SetEvaluator(evaluator)
    
    // 3. Register builtins with environment
    environment.SetupBuiltins()
    
    // 4. Register special forms
    special_forms.RegisterSpecialForms(environment)
    
    // 5. Initialize REPL with properly set up components
    repl := repl.NewREPL(environment, evaluator)
}
```

### Integration with Unified Object Protocol

The dictionary-specific fixes will be incorporated into the broader unified object protocol implementation. This ensures:

1. All objects (including dictionaries) follow the same property access and method dispatch patterns
2. Evaluator references are consistently maintained across all object types
3. Variable assignment correctly preserves evaluator references
4. Method dispatch works consistently for all object types

## 11. Conclusion

The proposed redesign eliminates the current confusion and inconsistency in M28's object system by implementing a single, unified protocol. This will make the codebase more maintainable, improve performance, and provide a better foundation for future enhancements. The specific solutions for dictionary implementation issues will ensure robust dictionary behavior in the language.