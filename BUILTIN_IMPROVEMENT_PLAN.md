# Builtin System Improvement Plan

## Executive Summary

The builtin system has several code smells that impact maintainability, consistency, and developer experience. This plan outlines a phased approach to address these issues without breaking existing functionality.

## Major Issues Identified

### 1. Code Duplication (Critical)
- **230+ instances** of manual argument count validation
- **Repeated type checking patterns** across all functions
- **No validation helpers** despite clear patterns

### 2. Inconsistent Error Handling (High)
- **No error type hierarchy** - all errors use fmt.Errorf
- **Inconsistent error messages** - different formats, capitalization
- **No context in errors** - missing function names, parameter info

### 3. File Organization Issues (Medium)
- **Oversized files** - string.go (1050 lines), pathlib.go (476 lines)
- **Mixed responsibilities** - modules split across directories
- **Duplicate registrations** - length in both string.go and list.go

### 4. Complex Functions (Medium)
- **Operator functions** handle multiple responsibilities
- **Deep nesting** for operator overloading checks
- **No abstraction** for common patterns

### 5. Missing Abstractions (High)
- **No argument validation framework**
- **No type extraction utilities**
- **No function builder patterns**

## Phased Improvement Plan

### Phase 1: Validation Framework (Week 1)

Create a new package `builtin/validation` with common helpers:

```go
// validation/args.go
package validation

import (
    "fmt"
    "github.com/mmichie/m28/core"
)

// ValidateExactArgs checks for exact argument count
func ValidateExactArgs(fnName string, args []core.Value, expected int) error {
    if len(args) != expected {
        return fmt.Errorf("%s() takes exactly %d argument(s) (%d given)", 
            fnName, expected, len(args))
    }
    return nil
}

// ValidateArgRange checks for argument count within range
func ValidateArgRange(fnName string, args []core.Value, min, max int) error {
    if len(args) < min || len(args) > max {
        if min == max {
            return ValidateExactArgs(fnName, args, min)
        }
        return fmt.Errorf("%s() takes %d to %d arguments (%d given)", 
            fnName, min, max, len(args))
    }
    return nil
}

// ValidateMinArgs checks for minimum argument count
func ValidateMinArgs(fnName string, args []core.Value, min int) error {
    if len(args) < min {
        return fmt.Errorf("%s() takes at least %d argument(s) (%d given)", 
            fnName, min, len(args))
    }
    return nil
}
```

```go
// validation/types.go
package validation

// ExtractString extracts a string value with error handling
func ExtractString(fnName string, arg core.Value, paramName string) (string, error) {
    if str, ok := arg.(core.StringValue); ok {
        return string(str), nil
    }
    return "", fmt.Errorf("%s() %s must be a string, not '%s'", 
        fnName, paramName, arg.Type())
}

// ExtractNumber extracts a number value with error handling
func ExtractNumber(fnName string, arg core.Value, paramName string) (float64, error) {
    if num, ok := arg.(core.NumberValue); ok {
        return float64(num), nil
    }
    return 0, fmt.Errorf("%s() %s must be a number, not '%s'", 
        fnName, paramName, arg.Type())
}

// ExtractInt extracts an integer with validation
func ExtractInt(fnName string, arg core.Value, paramName string) (int, error) {
    num, err := ExtractNumber(fnName, arg, paramName)
    if err != nil {
        return 0, err
    }
    intVal := int(num)
    if float64(intVal) != num {
        return 0, fmt.Errorf("%s() %s must be an integer", fnName, paramName)
    }
    return intVal, nil
}
```

### Phase 2: Error Type System (Week 1)

Create proper error types:

```go
// builtin/errors/types.go
package errors

type BuiltinError struct {
    FunctionName string
    Message      string
}

func (e *BuiltinError) Error() string {
    return fmt.Sprintf("%s: %s", e.FunctionName, e.Message)
}

type TypeError struct {
    BuiltinError
    Expected string
    Got      string
}

func NewTypeError(fnName, expected, got string) error {
    return &TypeError{
        BuiltinError: BuiltinError{FunctionName: fnName},
        Expected:     expected,
        Got:          got,
    }
}

type ValueError struct {
    BuiltinError
    Value interface{}
}

type ArgumentError struct {
    BuiltinError
    Expected int
    Got      int
}
```

### Phase 3: Function Builders (Week 2)

Create builders for common function patterns:

```go
// builtin/builders/unary.go
package builders

// UnaryNumberFunc creates a function that operates on a single number
func UnaryNumberFunc(name string, op func(float64) (core.Value, error)) BuiltinFunc {
    return func(args []core.Value, ctx *core.Context) (core.Value, error) {
        if err := validation.ValidateExactArgs(name, args, 1); err != nil {
            return nil, err
        }
        
        num, err := validation.ExtractNumber(name, args[0], "argument")
        if err != nil {
            return nil, err
        }
        
        return op(num)
    }
}

// UnaryStringFunc creates a function that operates on a single string
func UnaryStringFunc(name string, op func(string) (core.Value, error)) BuiltinFunc {
    return func(args []core.Value, ctx *core.Context) (core.Value, error) {
        if err := validation.ValidateExactArgs(name, args, 1); err != nil {
            return nil, err
        }
        
        str, err := validation.ExtractString(name, args[0], "argument")
        if err != nil {
            return nil, err
        }
        
        return op(str)
    }
}
```

### Phase 4: Refactor Existing Functions (Weeks 3-4)

Systematically update functions to use new abstractions:

**Before:**
```go
func AbsFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
    if len(args) != 1 {
        return nil, fmt.Errorf("abs() takes exactly one argument (%d given)", len(args))
    }
    
    num, ok := args[0].(core.NumberValue)
    if !ok {
        return nil, fmt.Errorf("abs() argument must be a number, not '%s'", args[0].Type())
    }
    
    if float64(num) < 0 {
        return core.NumberValue(-float64(num)), nil
    }
    return num, nil
}
```

**After:**
```go
func init() {
    RegisterBuiltin("abs", builders.UnaryNumberFunc("abs", func(n float64) (core.Value, error) {
        if n < 0 {
            return core.NumberValue(-n), nil
        }
        return core.NumberValue(n), nil
    }))
}
```

### Phase 5: File Reorganization (Week 5)

Split large files and reorganize by clear categories:

```
builtin/
├── validation/          # Validation utilities
│   ├── args.go
│   ├── types.go
│   └── kwargs.go
├── builders/           # Function builders
│   ├── unary.go
│   ├── binary.go
│   ├── variadic.go
│   └── operator.go
├── errors/            # Error types
│   ├── types.go
│   └── format.go
├── core/              # Core language functions
│   ├── types.go       # type, isinstance, etc.
│   ├── io.go          # print, input
│   ├── control.go     # error, assert
│   └── attributes.go  # getattr, setattr, etc.
├── collections/       # Collection operations
│   ├── list.go
│   ├── dict.go
│   ├── set.go
│   └── tuple.go
├── strings/          # String operations (split from 1050-line file)
│   ├── basic.go      # len, concat, etc.
│   ├── search.go     # find, index, startswith
│   ├── transform.go  # upper, lower, strip, split
│   └── format.go     # format, join
├── numeric/          # Numeric operations
│   ├── basic.go      # abs, round, etc.
│   ├── math.go       # sqrt, pow, etc.
│   └── aggregate.go  # sum, min, max
├── functional/       # Functional programming
│   ├── higher_order.go # map, filter, reduce
│   ├── itertools.go    # zip, enumerate, etc.
│   └── predicates.go   # all, any, callable
├── operators/        # Operators (already organized)
├── modules/          # Standard library modules
└── registry.go       # Registration logic
```

### Phase 6: Operator Overloading Abstraction (Week 6)

Create a clean abstraction for operator overloading:

```go
// builtin/operators/overload.go
package operators

// TryOperatorOverload attempts to call a dunder method on an object
func TryOperatorOverload(obj core.Value, method string, args []core.Value, ctx *core.Context) (core.Value, bool, error) {
    // Skip built-in types with optimized implementations
    switch obj.(type) {
    case core.NumberValue, core.StringValue, core.ListValue:
        return nil, false, nil
    }
    
    getter, ok := obj.(interface{ GetAttr(string) (core.Value, bool) })
    if !ok {
        return nil, false, nil
    }
    
    methodVal, found := getter.GetAttr(method)
    if !found {
        return nil, false, nil
    }
    
    callable, ok := methodVal.(interface{ Call([]core.Value, *core.Context) (core.Value, error) })
    if !ok {
        return nil, false, nil
    }
    
    result, err := callable.Call(args, ctx)
    return result, true, err
}

// ChainOperator chains operator calls for variadic operators
func ChainOperator(fnName, method string, args []core.Value, ctx *core.Context, 
    fallback func([]core.Value) (core.Value, error)) (core.Value, error) {
    
    if len(args) == 0 {
        return fallback(args)
    }
    
    // Try operator overloading first
    if len(args) >= 2 {
        result := args[0]
        for i := 1; i < len(args); i++ {
            overloaded, found, err := TryOperatorOverload(result, method, []core.Value{args[i]}, ctx)
            if err != nil {
                return nil, err
            }
            if found {
                result = overloaded
                continue
            }
            // Fall back to built-in implementation for this pair
            result, err = fallback([]core.Value{result, args[i]})
            if err != nil {
                return nil, err
            }
        }
        return result, nil
    }
    
    return fallback(args)
}
```

## Implementation Strategy

1. **Create new packages first** - Don't modify existing code initially
2. **Write comprehensive tests** - Ensure new abstractions work correctly
3. **Gradual migration** - Update functions one file at a time
4. **Maintain compatibility** - No breaking changes to public API
5. **Document patterns** - Create developer guide for new patterns

## Success Metrics

- **Reduce code duplication by 70%** - From 230+ manual validations to <70
- **Consistent error messages** - All errors follow same format
- **Smaller file sizes** - No file larger than 300 lines
- **Improved testability** - Unit tests for all abstractions
- **Better maintainability** - New functions follow established patterns

## Timeline

- Week 1: Validation framework + Error types
- Week 2: Function builders
- Weeks 3-4: Refactor existing functions
- Week 5: File reorganization
- Week 6: Operator overloading abstraction
- Week 7: Documentation and final cleanup

## Risks and Mitigations

1. **Risk**: Breaking existing functionality
   - **Mitigation**: Comprehensive test suite, gradual migration

2. **Risk**: Performance regression
   - **Mitigation**: Benchmark critical paths, optimize hot spots

3. **Risk**: Over-abstraction
   - **Mitigation**: Focus on real patterns, avoid premature optimization

## Next Steps

1. Review and approve this plan
2. Create validation package with initial helpers
3. Start migrating simple functions as proof of concept
4. Iterate based on learnings