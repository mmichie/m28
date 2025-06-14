# Function Builders Design

## Overview

Function builders eliminate boilerplate by encapsulating common patterns in builtin function implementations. They build on top of the validation framework from Phase 1.

## Design Principles

1. **Zero Boilerplate**: Developers only write business logic
2. **Type Safety**: Compile-time type checking where possible
3. **Composable**: Builders can be combined and extended
4. **Performance**: No runtime overhead compared to manual implementation
5. **Intuitive API**: Should be obvious how to use each builder

## Builder Types

### 1. UnaryFunc[T, R]
For functions that take one argument and return a result.
```go
// Examples: abs, sqrt, upper, lower, len
abs := builders.UnaryNumber("abs", func(n float64) (float64, error) {
    return math.Abs(n), nil
})
```

### 2. BinaryFunc[T1, T2, R]
For functions that take two arguments.
```go
// Examples: pow, atan2, max, min
pow := builders.BinaryNumber("pow", math.Pow)
```

### 3. VariadicFunc[T, R]
For functions that take multiple arguments of the same type.
```go
// Examples: sum, concat
sum := builders.VariadicNumber("sum", func(nums []float64) (float64, error) {
    total := 0.0
    for _, n := range nums {
        total += n
    }
    return total, nil
})
```

### 4. PredicateFunc[T]
For functions that test a condition and return boolean.
```go
// Examples: isinstance, callable, isdigit
isdigit := builders.PredicateString("isdigit", func(s string) bool {
    return len(s) > 0 && strings.IndexFunc(s, func(r rune) bool {
        return !unicode.IsDigit(r)
    }) == -1
})
```

### 5. OperatorFunc
For operators that support overloading.
```go
// Examples: +, -, *, /
add := builders.Operator("+", "__add__",
    builders.NumberHandler(func(a, b float64) (core.Value, error) {
        return core.NumberValue(a + b), nil
    }),
    builders.StringHandler(func(a, b string) (core.Value, error) {
        return core.StringValue(a + b), nil
    }),
    builders.ListHandler(func(a, b core.ListValue) (core.Value, error) {
        return append(a, b...), nil
    }),
)
```

## Type Specializations

To avoid too much generics complexity, we'll have specialized builders:

- `UnaryNumber`, `BinaryNumber`, `VariadicNumber` - for numeric functions
- `UnaryString`, `BinaryString` - for string functions  
- `UnarySequence` - for functions that work on any sequence
- `UnaryAny` - for functions that accept any type

## Error Handling

All builders will:
1. Use the validation framework for argument checking
2. Use the error types for consistent error messages
3. Wrap implementation errors with context

## Extensibility

Builders should be extensible through:
1. Custom validators
2. Pre/post processing hooks
3. Composition of multiple builders

## Implementation Plan

1. Create base builder interface
2. Implement type-specific builders
3. Add operator overloading support
4. Create helper functions for common patterns
5. Optimize for performance