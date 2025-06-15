# Type System Phase 2: Protocol Interfaces

## Overview
Building on Phase 1's type assertion helpers, Phase 2 introduces protocol interfaces to standardize operations across types. This will enable operator overloading, better polymorphism, and cleaner code.

## Core Protocols to Implement

### 1. Numeric Protocol
For types that support arithmetic operations:
```go
type Numeric interface {
    // Basic arithmetic
    Add(other Value) (Value, error)
    Subtract(other Value) (Value, error)
    Multiply(other Value) (Value, error)
    Divide(other Value) (Value, error)
    Modulo(other Value) (Value, error)
    Power(other Value) (Value, error)
    
    // Unary operations
    Negate() (Value, error)
    Absolute() (Value, error)
}
```

### 2. Indexable Protocol
For types that support indexing (list, dict, string):
```go
type Indexable interface {
    GetIndex(index Value) (Value, error)
    SetIndex(index Value, value Value) error
    HasIndex(index Value) bool
    DeleteIndex(index Value) error
}
```

### 3. Container Protocol
For types that have a size and can contain items:
```go
type Container interface {
    Size() int
    Contains(item Value) bool
    IsEmpty() bool
    Items() []Value  // For iteration
}
```

### 4. Comparable Protocol
For types that support comparison:
```go
type Comparable interface {
    Compare(other Value) (int, error)  // -1, 0, 1 like Go's compare
    Equal(other Value) bool
}
```

### 5. Hashable Protocol (already exists)
We'll extend the existing IsHashable with a protocol interface.

## Implementation Steps

### Step 1: Create Protocol Package
```
core/protocols/
├── numeric.go      # Numeric protocol and helpers
├── indexable.go    # Indexable protocol and helpers
├── container.go    # Container protocol and helpers
├── comparable.go   # Comparable protocol and helpers
└── protocols.go    # Common utilities
```

### Step 2: TypeSwitch Builder
Create a fluent builder for handling type switches elegantly:
```go
result, err := TypeSwitch(value).
    Case(IsNumber, func(n float64) (Value, error) {
        return NumberValue(n * 2), nil
    }).
    Case(IsString, func(s string) (Value, error) {
        return StringValue(s + s), nil
    }).
    Default(func(v Value) (Value, error) {
        return nil, TypeError("cannot double", v)
    }).
    Execute()
```

### Step 3: Dunder Method Utilities
Standardize calling Python-style dunder methods:
```go
// Check and call dunder methods
CallDunder(obj, "__add__", []Value{other}, ctx)
HasDunder(obj, "__len__")
GetDunder(obj, "__str__")

// Specific helpers
CallAdd(obj, other, ctx)     // Calls __add__ with fallback
CallLen(obj, ctx)            // Calls __len__ with validation
CallStr(obj, ctx)            // Calls __str__ with fallback to String()
```

### Step 4: Protocol Adoption
Update existing types to implement protocols:
- NumberValue → Numeric, Comparable
- StringValue → Indexable, Container, Comparable
- ListValue → Indexable, Container
- DictValue → Indexable, Container
- SetValue → Container

## Benefits

1. **Operator Overloading**: User-defined types can implement arithmetic
2. **Polymorphism**: Functions can accept any Indexable or Container
3. **Type Safety**: Compile-time protocol checking
4. **Code Reduction**: Replace type assertions with protocol checks
5. **Extensibility**: Easy to add new protocols

## Migration Example

### Before (manual type checking):
```go
func getLength(v Value) (int, error) {
    switch val := v.(type) {
    case StringValue:
        return len(string(val)), nil
    case ListValue:
        return len(val), nil
    case *DictValue:
        return val.Size(), nil
    case *SetValue:
        return val.Size(), nil
    default:
        // Check for __len__ method
        if obj, ok := v.(Object); ok {
            if lenMethod, exists := obj.GetAttr("__len__"); exists {
                // Call method...
            }
        }
        return 0, fmt.Errorf("object of type '%s' has no len()", v.Type())
    }
}
```

### After (with protocols):
```go
func getLength(v Value) (int, error) {
    // First try Container protocol
    if container, ok := AsContainer(v); ok {
        return container.Size(), nil
    }
    
    // Then try __len__ dunder method
    if result, ok, err := CallLen(v, ctx); ok {
        return result, err
    }
    
    return 0, TypeError("object has no len()", v)
}
```

## Success Metrics
- 50%+ reduction in operator implementation code
- All arithmetic operations use Numeric protocol
- All container operations use Container protocol
- TypeSwitch used in 80%+ of type switches
- Consistent dunder method handling