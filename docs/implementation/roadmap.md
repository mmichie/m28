# M28 Language Implementation Roadmap

> **NOTE**: This document has been updated with a new radical simplification plan. Previous unification proposals have been removed.

## Radical Simplification Plan

Since backward compatibility is not a concern, we've developed a complete redesign approach focused on simplicity and maintainability.

### Core Architecture

The new architecture is built around these fundamental components:

```go
// Base value interface
type Value interface {
    Type() Type
    String() string
}

// Unified object protocol
type Object interface {
    Value
    GetAttr(name string) (Value, bool)
    SetAttr(name string, value Value) error
    CallMethod(name string, args []Value, ctx *Context) (Value, error)
}

// Context-based evaluation system
type Context struct {
    Vars map[string]Value
    Outer *Context
    Global *Context
    CallStack []TraceEntry
}
```

## Detailed Implementation Roadmap

### Phase 1: Core Type System (2 weeks)

#### Week 1: Value Interface & Primitive Types
- **Days 1-2**: Design and implement core `Value` and `Type` interfaces
- **Days 3-4**: Implement primitive types
  - `NumberValue` for numeric data
  - `StringValue` for text
  - `BoolValue` for boolean logic
  - `SymbolValue` for identifiers
  - `NilValue` for null/None
- **Day 5**: Implement value equality, conversion, and string representation

#### Week 2: Object Protocol Foundation
- **Days 1-2**: Implement the `BaseObject` with standard attribute handling
- **Days 3-4**: Develop `TypeDescriptor` system for type metadata
- **Day 5**: Build method binding and callable interfaces

### Phase 2: Evaluation Engine (2 weeks)

#### Week 1: Context & Core Evaluation
- **Days 1-2**: Implement `Context` structure with variable environment
- **Days 3-4**: Create basic `Eval` function with expression dispatch
- **Day 5**: Add error handling with proper stack traces

#### Week 2: Special Forms
- **Day 1**: Implement basic control flow (`if`, `cond`)
- **Day 2**: Add variable definition and assignment forms
- **Day 3**: Implement function definition and lambda forms
- **Day 4**: Add looping constructs (`for`, `while`, `loop`)
- **Day 5**: Implement exception handling (`try`, `except`, `finally`)

### Phase 3: Collections & Standard Library (1.5 weeks)

#### Week 1: Collections
- **Days 1-2**: Implement `List` with the new object protocol
- **Days 3-4**: Create `Dict` implementation with standard methods
- **Day 5**: Add `Tuple` and `Set` implementations

#### Days 1-3 of Week 2: Standard Library
- **Day 1**: Add mathematical functions
- **Day 2**: Implement string manipulation functions
- **Day 3**: Create core utility functions (type checking, conversion, etc.)

### Phase 4: Advanced Features (1.5 weeks)

#### Days 4-5 of Week 2 & Week 3: Advanced Language Features
- **Day 4**: Implement module system with import/export
- **Day 5**: Build enhanced class system with inheritance
- **Day 1**: Add generator support with yield
- **Day 2**: Implement context managers
- **Day 3**: Create concurrency features (go, channels, select)
- **Days 4-5**: Build REPL and file execution system

### Phase 5: Testing & Optimization (1 week)

- **Days 1-2**: Port existing tests to validate new implementation
- **Day 3**: Create new tests for previously untested features
- **Day 4**: Performance optimization and benchmarking
- **Day 5**: Final validation and documentation

## Implementation Approach

Rather than creating parallel implementations or preserving parts of the old system, we'll completely replace the existing implementation with a clean, fresh codebase:

1. Preserve only the parser with minimal modifications
2. Replace ALL other files with the new, clean implementations
3. Keep the same directory structure but with entirely new code
4. Adapt tests to validate the new implementation

This "fresh start" approach ensures we won't be constrained by previous architectural decisions.

## Technical Details

### 1. Value System Implementation

The value system will use Go's interface-based polymorphism, but with cleaner type relationships:

```go
// Standard Go type implementation
type NumberValue float64

func (n NumberValue) Type() Type {
    return NumberType
}

func (n NumberValue) String() string {
    return fmt.Sprintf("%g", float64(n))
}

// Example of value operations
func Add(a, b Value) (Value, error) {
    // Type dispatch based on Type() method
    if a.Type() == NumberType && b.Type() == NumberType {
        return NumberValue(a.(NumberValue) + b.(NumberValue)), nil
    }
    return nil, fmt.Errorf("cannot add values of type %s and %s", a.Type().Name(), b.Type().Name())
}
```

### 2. Object Protocol Details

The object system will use composition to provide common behavior:

```go
// Object storage and property access
type BaseObject struct {
    attrs map[string]Value
    typ   Type
    mu    sync.RWMutex // For thread safety
}

// Attribute access with thread safety
func (o *BaseObject) GetAttr(name string) (Value, bool) {
    o.mu.RLock()
    defer o.mu.RUnlock()
    
    value, ok := o.attrs[name]
    return value, ok
}

// Method calls with proper binding
func (o *BaseObject) CallMethod(name string, args []Value, ctx *Context) (Value, error) {
    method, ok := o.GetAttr(name)
    if !ok {
        return nil, fmt.Errorf("no method named %s", name)
    }
    
    if callable, ok := method.(Callable); ok {
        return callable.Call(append([]Value{o}, args...), ctx)
    }
    
    return nil, fmt.Errorf("%s is not callable", name)
}
```

### 3. Context-Based Evaluation

The evaluation system will use a context object to maintain state:

```go
// Evaluating expressions
func Eval(expr Value, ctx *Context) (Value, error) {
    switch v := expr.(type) {
    case SymbolValue:
        // Variable lookup
        return ctx.Lookup(string(v))
        
    case ListValue:
        if len(v) == 0 {
            return EmptyList, nil
        }
        
        // Special form or function call
        first, err := Eval(v[0], ctx)
        if err != nil {
            return nil, err
        }
        
        // Check for special forms first
        if sym, ok := v[0].(SymbolValue); ok {
            if form, ok := specialForms[string(sym)]; ok {
                return form(v[1:], ctx)
            }
        }
        
        // Function call
        args := make([]Value, 0, len(v)-1)
        for _, arg := range v[1:] {
            evalArg, err := Eval(arg, ctx)
            if err != nil {
                return nil, err
            }
            args = append(args, evalArg)
        }
        
        if callable, ok := first.(Callable); ok {
            return callable.Call(args, ctx)
        }
        
        return nil, fmt.Errorf("not callable: %v", first)
        
    default:
        // Self-evaluating values
        return expr, nil
    }
}
```

### 4. Module System Design

The module system will use the object protocol for clean imports:

```go
// Module implementation
type Module struct {
    BaseObject
    name     string
    filename string
    loaded   bool
}

// Module loading
func LoadModule(name string, ctx *Context) (*Module, error) {
    // Check if already loaded
    if mod, ok := loadedModules[name]; ok {
        return mod, nil
    }
    
    // Locate module file
    filename, err := findModuleFile(name)
    if err != nil {
        return nil, err
    }
    
    // Create new module object
    module := &Module{
        BaseObject: *NewBaseObject(ModuleType),
        name:       name,
        filename:   filename,
        loaded:     false,
    }
    
    // Register in global modules registry
    loadedModules[name] = module
    
    // Create module execution context
    modCtx := NewContext(ctx.Global)
    
    // Execute module code in its context
    source, err := readFile(filename)
    if err != nil {
        return nil, err
    }
    
    _, err = EvalString(source, modCtx)
    if err != nil {
        return nil, err
    }
    
    // Copy exports to module object
    for k, v := range modCtx.Vars {
        module.SetAttr(k, v)
    }
    
    module.loaded = true
    return module, nil
}
```

## Key Benefits

1. **Unified Object System** - Single consistent interface for all objects
2. **Context-Based Evaluation** - Eliminates circular dependencies
3. **Clean Type Hierarchy** - Leverages Go's type system more effectively
4. **Reduced Boilerplate** - Common functionality in shared implementations
5. **Improved Concurrency** - Better support for concurrent execution
6. **Thread Safety** - Built-in from the beginning

This simplified approach eliminates architectural debt and provides a clean foundation for future development without being constrained by backward compatibility concerns.

## Original Implementation Status

The sections below are maintained for historical reference.

### Implementation Legend
- ✅ Feature implemented
- ⚠️ Feature partial, in progress, or needs improvement
- ❌ Feature not implemented

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
- ✅ Enhanced module imports (aliased, multi-symbol, wildcard)
- ✅ Dot notation for property and method access
- ✅ Exception handling with try/except
- ✅ Object representation via closures
- ✅ Tail call optimization for recursion
- ✅ Context managers with with/as syntax
- ✅ Generators with yield statement
- ✅ Basic exception hierarchy
- ✅ Module-level `__exports__` mechanism
- ✅ REPL improvements (line editing, history, tab completion)
- ✅ Error message enhancements