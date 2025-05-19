# Radical Simplification: New Object Protocol

> **NOTE**: This document supersedes previous design proposals with a cleaner, simpler approach that doesn't require backward compatibility.

## 1. Core Problem

The current M28 codebase suffers from multiple competing object access patterns and a complex type system with excessive indirection. This creates confusion, performance issues, and makes the code difficult to maintain.

## 2. Clean Slate Solution

Rather than trying to unify existing interfaces, we'll implement a new, minimal object system from scratch.

### 2.1 Value Interface

```go
// Value is the base interface for all values in the language
type Value interface {
    // Type returns the value's type descriptor
    Type() Type
    
    // String returns a string representation
    String() string
}

// Type represents a type in the language
type Type interface {
    Value
    
    // Name returns the type's name
    Name() string
    
    // IsSubtypeOf checks if this type is a subtype of another
    IsSubtypeOf(other Type) bool
}
```

### 2.2 Object Protocol

```go
// Object represents any value that can have attributes and methods
type Object interface {
    Value
    
    // GetAttr retrieves an attribute by name
    GetAttr(name string) (Value, bool)
    
    // SetAttr sets an attribute value
    SetAttr(name string, value Value) error
    
    // CallMethod calls a method with arguments in a context
    CallMethod(name string, args []Value, ctx *Context) (Value, error)
}
```

### 2.3 Context-Based Evaluation

```go
// Context holds the execution context for evaluation
type Context struct {
    // Variables in the current scope
    Vars map[string]Value
    
    // Parent scope
    Outer *Context
    
    // Global scope for module-level variables
    Global *Context
    
    // Call stack for debugging and error reporting
    CallStack []TraceEntry
}

// Eval evaluates an expression in a context
func Eval(expr Value, ctx *Context) (Value, error) {
    // Implementation based on expression type...
}
```

### 2.4 BaseObject Implementation

```go
// BaseObject provides a standard implementation of Object
type BaseObject struct {
    attrs  map[string]Value
    typeDesc *TypeDescriptor
    mu     sync.RWMutex  // For thread safety
}

func NewBaseObject(typeDesc *TypeDescriptor) *BaseObject {
    return &BaseObject{
        attrs: make(map[string]Value),
        typeDesc: typeDesc,
    }
}

// GetAttr implements Object.GetAttr
func (o *BaseObject) GetAttr(name string) (Value, bool) {
    o.mu.RLock()
    defer o.mu.RUnlock()
    
    // Check local attributes
    if val, ok := o.attrs[name]; ok {
        return val, true
    }
    
    // Check type for methods or properties
    if o.typeDesc != nil {
        if method, ok := o.typeDesc.methods[name]; ok {
            return method.Bind(o), true
        }
    }
    
    return nil, false
}

// Other methods follow a similar pattern...
```

## 3. Implementation Strategy

### Phase 1: Core Types (2 weeks)

1. Implement the base Value and Type interfaces
2. Create fundamental types (Number, String, Symbol, Bool, etc.)
3. Implement BaseObject and core object protocol

### Phase 2: Evaluation System (1 week)

1. Create the Context-based evaluation system
2. Implement special forms (if, def, lambda, etc.)
3. Build object property access and method dispatch

### Phase 3: Container Types (1 week)

1. Implement List, Dict, and Tuple with the new object protocol
2. Add standard methods for all container types
3. Integrate with the evaluation system

### Phase 4: Integration (1-2 weeks)

1. Revise parser integration to work with new type system
2. Build REPL using the new evaluation context
3. Implement module system with new object protocol

## 4. Benefits

1. **Radical Simplification** - Reducing the codebase size by 30-50%
2. **Improved Performance** - Eliminating indirection and simplifying method dispatch
3. **Clearer Code Structure** - Making the system easier to understand and extend
4. **Thread Safety** - Built into object implementations from the start
5. **Better Debugging** - Context-based evaluation allows for better error reporting

## 5. Implementation Notes

- Replace existing code completely for a true fresh start
- Keep the same directory structure but with entirely new implementations
- Preserve and adapt tests to validate the new implementation
- Reuse parser code with minimal changes
- Don't attempt backward compatibility - focus on clean design

This approach eliminates the need to reconcile multiple competing object models and provides a solid foundation for building advanced language features in the future.