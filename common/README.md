# M28 Common Packages

This directory contains shared utilities and patterns that standardize code quality across the M28 codebase. These packages were created as part of Phase 1 of the code quality improvements to eliminate code duplication and establish consistent patterns.

## Packages

### errors/
Provides standardized error types matching Python's exception hierarchy while maintaining Go idioms.

**Key Features:**
- Type-safe error creation
- Consistent error formatting
- Python-compatible error types (TypeError, ValueError, etc.)
- Context preservation with details

**Usage:**
```go
import "github.com/mmichie/m28/common/errors"

// Create specific error types
err := errors.NewTypeError("len", "sequence", "number")
// Output: "TypeError: len: expected sequence, got number"

err := errors.NewArgumentError("abs", 1, 3)
// Output: "ArgumentError: abs: takes exactly 1 argument (3 given)"

// Add context to errors
err := errors.NewValueError("parse", "invalid syntax").
    WithDetail("line", 42).
    WithDetail("column", 10)
```

### validation/
Eliminates repetitive argument validation code with a fluent, intuitive API.

**Key Features:**
- Fluent argument validation
- Type-safe value extraction
- Comprehensive error messages
- Support for optional arguments

**Usage:**
```go
import "github.com/mmichie/m28/common/validation"

func AbsFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
    v := validation.NewArgs("abs", args)
    
    // Validate argument count
    if err := v.Exact(1); err != nil {
        return nil, err
    }
    
    // Extract typed value with validation
    num, err := v.GetNumber(0)
    if err != nil {
        return nil, err
    }
    
    // Business logic
    if num < 0 {
        return core.NumberValue(-num), nil
    }
    return core.NumberValue(num), nil
}
```

## Migration Guide

### Before (Manual Validation)
```go
func SomeFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
    // Manual argument count check
    if len(args) != 2 {
        return nil, fmt.Errorf("some_func() takes exactly 2 arguments (%d given)", len(args))
    }
    
    // Manual type checking
    str, ok := args[0].(core.StringValue)
    if !ok {
        return nil, fmt.Errorf("some_func() expects string as first argument, got %s", args[0].Type())
    }
    
    num, ok := args[1].(core.NumberValue)
    if !ok {
        return nil, fmt.Errorf("some_func() expects number as second argument, got %s", args[1].Type())
    }
    
    // Business logic...
}
```

### After (Using Common Packages)
```go
func SomeFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
    v := validation.NewArgs("some_func", args)
    
    // Clean validation
    if err := v.Exact(2); err != nil {
        return nil, err
    }
    
    // Type-safe extraction
    str, err := v.GetString(0)
    if err != nil {
        return nil, err
    }
    
    num, err := v.GetNumber(1)
    if err != nil {
        return nil, err
    }
    
    // Business logic...
}
```

## Design Principles

1. **Idiomatic Go**: Follow Go conventions while providing Python-like error semantics
2. **Zero Magic**: Explicit, predictable behavior with no hidden complexity
3. **Performance**: Minimal overhead - validation should be negligible compared to function logic
4. **Extensibility**: Easy to add new validation methods or error types
5. **Testability**: Comprehensive test coverage serves as documentation

## Common Patterns

### Exact Argument Count
```go
if err := v.Exact(2); err != nil {
    return nil, err
}
```

### Variable Arguments
```go
if err := v.Min(1); err != nil {  // At least 1
    return nil, err
}

if err := v.Range(1, 3); err != nil {  // 1 to 3 args
    return nil, err
}
```

### Optional Arguments
```go
// Get with default
name, err := v.GetStringOrDefault(1, "default")
if err != nil {
    return nil, err
}

// Manual check
var options *core.DictValue
if v.Count() > 1 {
    options, err = v.GetDict(1)
    if err != nil {
        return nil, err
    }
}
```

### Extract All Arguments
```go
// All must be numbers
numbers, err := v.ExtractNumbers()
if err != nil {
    return nil, err
}

// All must be strings
strings, err := v.ExtractStrings()
if err != nil {
    return nil, err
}
```

### Type Checking Patterns
```go
// Any sequence type
seq, err := v.GetSequence(0)
if err != nil {
    return nil, err
}

// Any iterable
iter, err := v.GetIterable(0)
if err != nil {
    return nil, err
}

// Callable
fn, err := v.GetCallable(0)
if err != nil {
    return nil, err
}
```

## Performance Considerations

The validation framework is designed to have minimal overhead:
- No allocations for successful validations
- Errors allocated only on failure
- Direct type assertions without reflection
- Inline-friendly method implementations

## Future Enhancements

Planned additions to these packages:
- `GetOptional*` methods that return (value, bool, error)
- Pattern matching for complex argument signatures
- Caching of common error messages
- Integration with function signatures for auto-validation

## Contributing

When adding new validation methods or error types:
1. Follow existing naming conventions
2. Add comprehensive tests
3. Update this documentation
4. Consider performance implications
5. Maintain backward compatibility