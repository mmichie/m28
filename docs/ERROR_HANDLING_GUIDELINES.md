# Error Handling Guidelines

This document provides guidelines for standardized error handling in the M28 codebase.

## Overview

M28 has two error handling systems that should be used appropriately:

1. **`common/errors` package** - For builtin functions and modules that interact with M28 code
2. **`core` error types** - For core language operations and runtime errors

## When to Use Each System

### Use `common/errors` Package

Use for:
- Builtin functions (in `builtin/` package)
- Module implementations (in `modules/` package)
- User-facing validation errors
- Builder functions and operators

**Example:**
```go
import "github.com/mmichie/m28/common/errors"

func MyBuiltin(args []core.Value, ctx *core.Context) (core.Value, error) {
    if len(args) != 2 {
        return nil, errors.NewArgumentError("myfunction", 2, len(args))
    }

    if _, ok := args[0].(core.NumberValue); !ok {
        return nil, errors.NewTypeError("myfunction", "number", string(args[0].Type()))
    }

    return someValue, nil
}
```

### Use `core` Error Types

Use for:
- Core language operations (in `core/` and `eval/` packages)
- Type system errors
- Runtime errors during evaluation
- Errors that need source location tracking

**Example:**
```go
func evaluateIndex(list *core.ListValue, index int) (core.Value, error) {
    if index < 0 || index >= list.Len() {
        return nil, &core.IndexError{
            Index:  index,
            Length: list.Len(),
        }
    }
    return list.Items()[index], nil
}
```

## Error Comparison: Use `errors.As()`, Not String Matching

**Never** compare errors by string matching. Always use `errors.As()` or `errors.Is()`.

### ❌ Bad (String Matching)
```go
if err != nil {
    if strings.Contains(err.Error(), "StopIteration") {
        // Handle StopIteration
    }
}
```

### ✅ Good (Type-Safe Comparison)
```go
if err != nil {
    var stopIter *protocols.StopIteration
    if errors.As(err, &stopIter) {
        // Handle StopIteration
    }
}
```

### ✅ Good (Checking for KeyError)
```go
if err != nil {
    var keyErr *core.KeyError
    if errors.As(err, &keyErr) {
        // Handle KeyError
    }
}
```

## Available Error Types

### Common Errors Package

Located in `common/errors/errors.go`:

- `NewTypeError(function, expected, got)` - Type mismatches
- `NewTypeErrorf(function, format, args...)` - Custom type error message
- `NewValueError(function, message)` - Invalid values
- `NewValueErrorf(function, format, args...)` - Custom value error
- `NewArgumentError(function, expected, got)` - Wrong argument count
- `NewArgumentRangeError(function, min, max, got)` - Argument count range
- `NewAttributeError(typeName, attribute)` - Missing attributes
- `NewKeyError(key)` - Missing dictionary keys
- `NewIndexError(index, length)` - Index out of bounds
- `NewRuntimeError(function, message)` - Runtime errors
- `NewNameError(name)` - Undefined names
- `NewZeroDivisionError(function)` - Division by zero

### Core Error Types

Located in `core/dot_errors.go`:

- `*core.TypeError` - Type mismatches in core operations
- `*core.ValueError` - Invalid values
- `*core.KeyError` - Missing dictionary keys
- `*core.IndexError` - Index out of bounds
- `*core.AttributeError` - Missing attributes
- `*core.NameError` - Undefined variables
- `*core.ZeroDivisionError` - Division by zero
- `*core.ImportError` - Import failures
- `*core.ModuleNotFoundError` - Module not found
- `*core.AssertionError` - Failed assertions
- `*core.OSError` - OS/file errors
- `*core.FileNotFoundError` - File not found

### Special Error Types

- `*protocols.StopIteration` - Iterator exhausted (in `core/protocols/iterable.go`)
- `*core.StopIteration` - Generator exhausted (in `core/generator.go`)

## Error Context and Location Tracking

Always provide context with errors:

### Function Context

```go
// common/errors package automatically includes function name
return nil, errors.NewTypeError("len", "sized object", string(arg.Type()))
// Output: "TypeError: len: expected sized object, got str"
```

### Source Location

For core errors, add location when available:

```go
err := &core.IndexError{
    Index:  index,
    Length: length,
}
if location != nil {
    err.Location = location
}
return nil, err
```

## Error Wrapping

When wrapping errors, preserve the original error:

### ✅ Good (Preserves Error Type)
```go
if err != nil {
    return nil, fmt.Errorf("failed to load module %s: %w", name, err)
}
```

### ❌ Bad (Loses Error Type)
```go
if err != nil {
    return nil, fmt.Errorf("failed to load module %s: %v", name, err)
}
```

## Migration Status

As of 2025-11-27:

**Completed:**
- ✅ All string-based error matching replaced with `errors.As()`
- ✅ Builtin package using `common/errors` consistently
- ✅ Modules package using `common/errors` consistently

**Remaining Work:**
- 🔄 Core package (488 `fmt.Errorf` calls to migrate)
- 🔄 Eval package (401 `fmt.Errorf` calls to migrate)
- 📝 Add function context to all error sites
- 📝 Add location tracking to eval errors

## Best Practices

1. **Always use structured error types** instead of `fmt.Errorf()`
2. **Always check error types with `errors.As()`** instead of string matching
3. **Always include function name** in error context
4. **Always preserve error chains** using `%w` format verb
5. **Never panic** for recoverable errors - return errors instead
6. **Document error types** that functions can return

## Examples

### Complete Example: Builtin Function

```go
func MyBuiltinBuilder() builders.BuiltinFunc {
    return func(args []core.Value, ctx *core.Context) (core.Value, error) {
        v := validation.NewArgs("mybuiltin", args)

        // Argument count validation
        if err := v.Exact(2); err != nil {
            return nil, err
        }

        // Type validation
        num, err := types.RequireNumber(v.Get(0), "first argument")
        if err != nil {
            return nil, err
        }

        str, err := types.RequireString(v.Get(1), "second argument")
        if err != nil {
            return nil, err
        }

        // Business logic
        result := someOperation(num, str)
        return result, nil
    }
}
```

### Complete Example: Core Function with Location

```go
func evaluateBinaryOp(left, right core.Value, op string, loc *core.SourceLocation) (core.Value, error) {
    leftNum, ok1 := left.(core.NumberValue)
    rightNum, ok2 := right.(core.NumberValue)

    if !ok1 || !ok2 {
        err := &core.TypeError{
            Message: fmt.Sprintf("unsupported operand type(s) for %s", op),
            Location: loc,
        }
        return nil, err
    }

    if op == "/" && rightNum == 0 {
        err := &core.ZeroDivisionError{Location: loc}
        return nil, err
    }

    return leftNum / rightNum, nil
}
```

## References

- Common errors package: `common/errors/errors.go`
- Core error types: `core/dot_errors.go`
- Iterator protocol: `core/protocols/iterable.go`
- Error handling in loops: `eval/loop.go`
