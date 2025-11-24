# Error Handling and LocatedValue Pattern

## Overview

M28 uses `LocatedValue` wrappers to preserve source code location information throughout the evaluation pipeline. This enables high-quality error messages that pinpoint exactly where issues occur in the source code.

However, `LocatedValue` wrappers can cause runtime panics if not properly handled during type assertions. This document explains the pattern and best practices for working with `LocatedValue`.

## Why LocatedValue Exists

When the parser creates AST nodes and the AST is converted to IR (Intermediate Representation), values are often wrapped in `LocatedValue` to preserve their source location:

```go
type LocatedValue struct {
    Value    Value
    Location *SourceLocation
}

type SourceLocation struct {
    File    string
    Line    int
    Column  int
}
```

This allows the evaluator to produce error messages like:
```
error: expected symbol, got int at test.m28:15:22
```

Rather than just:
```
error: expected symbol, got int
```

## The Problem: Type Assertions Fail on Wrapped Values

Consider this code that will panic:

```go
// WRONG - Will panic if args.Items()[0] is a LocatedValue
func mySpecialForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
    // This panics if the value is wrapped in LocatedValue
    target := args.Items()[0].(core.SymbolValue)

    // Process the target symbol...
}
```

If `args.Items()[0]` is a `LocatedValue` wrapping a `SymbolValue`, the type assertion will fail because:
- `LocatedValue` is NOT a `SymbolValue`
- The type assertion expects exactly `SymbolValue`
- Go will panic with "interface conversion: Value is LocatedValue, not SymbolValue"

## The Solution: Always Unwrap Before Type Assertions

There are two approaches to safely handle `LocatedValue`:

### Approach 1: Manual Unwrapping (Recommended for Special Forms)

Use the `unwrapLocated()` helper function before type assertions:

```go
// CORRECT - Unwrap before type assertion
func mySpecialForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
    // Unwrap any LocatedValue wrappers
    unwrapped := unwrapLocated(args.Items()[0])

    // Now safe to type assert
    target, ok := unwrapped.(core.SymbolValue)
    if !ok {
        return nil, fmt.Errorf("expected symbol, got %v", unwrapped.Type())
    }

    // Process the target symbol...
}
```

The `unwrapLocated()` function is defined in `eval/util.go`:

```go
// unwrapLocated recursively unwraps LocatedValue wrappers from a value
func unwrapLocated(v core.Value) core.Value {
    for {
        if lv, ok := v.(core.LocatedValue); ok {
            v = lv.Value
        } else {
            return v
        }
    }
}
```

### Approach 2: Smart Accessors (Recommended for List Access)

When accessing items from a `ListValue`, use the smart accessor methods that automatically unwrap:

```go
// CORRECT - Use smart accessor
func mySpecialForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
    // GetItemAsSymbol automatically unwraps LocatedValue
    target, ok := args.GetItemAsSymbol(0)
    if !ok {
        unwrapped, _ := args.GetItemUnwrapped(0)
        return nil, fmt.Errorf("expected symbol, got %v", unwrapped.Type())
    }

    // Process the target symbol...
}
```

Available smart accessors in `core/containers.go`:
- `GetItemUnwrapped(index)` - Returns unwrapped value
- `GetItemAsSymbol(index)` - Returns `(SymbolValue, bool)`
- `GetItemAsString(index)` - Returns `(StringValue, bool)`
- `GetItemAsList(index)` - Returns `(*ListValue, bool)`

## When to Unwrap

You MUST unwrap in these situations:

### 1. Before Type Assertions

```go
// WRONG
target := value.(core.SymbolValue)

// CORRECT
unwrapped := unwrapLocated(value)
target, ok := unwrapped.(core.SymbolValue)
```

### 2. Before Type Switches

```go
// WRONG
switch t := value.(type) {
case core.SymbolValue:
    // ...
}

// CORRECT
unwrapped := unwrapLocated(value)
switch t := unwrapped.(type) {
case core.SymbolValue:
    // ...
}
```

### 3. Before Equality Comparisons with Specific Types

```go
// WRONG
if starMarker == core.SymbolValue("*unpack-iter") {
    // ...
}

// CORRECT
unwrapped := unwrapLocated(starMarker)
if sym, ok := unwrapped.(core.SymbolValue); ok && sym == "*unpack-iter" {
    // ...
}
```

### 4. When Extracting Values from Lists for Processing

```go
// WRONG
for _, item := range list.Items() {
    sym := item.(core.SymbolValue)
    // ...
}

// CORRECT
for _, item := range list.Items() {
    unwrapped := unwrapLocated(item)
    if sym, ok := unwrapped.(core.SymbolValue); ok {
        // ...
    }
}
```

## Real-World Examples from Recent Fixes

### Example 1: Star Unpacking (eval/util.go:798-803)

**Before (Buggy):**
```go
starTarget := target.(*core.ListValue)
starVar := starTarget.Items()[1].(core.SymbolValue)
```

**After (Fixed):**
```go
unwrappedTarget := unwrapLocated(target)
starTarget, ok := unwrappedTarget.(*core.ListValue)
if !ok {
    return nil, fmt.Errorf("star unpacking target must be a list, got %v", unwrappedTarget.Type())
}
starVar, ok := starTarget.GetItemAsSymbol(1)
if !ok {
    return nil, fmt.Errorf("star unpacking variable must be a symbol")
}
```

### Example 2: Walrus Operator (eval/util.go:1375)

**Before (Buggy):**
```go
target, ok := args.Items()[0].(core.SymbolValue)
if !ok {
    return nil, TypeError{Expected: "symbol", Got: args.Items()[0].Type()}
}
```

**After (Fixed):**
```go
target, ok := args.GetItemAsSymbol(0)
if !ok {
    unwrapped := unwrapLocated(args.Items()[0])
    return nil, TypeError{Expected: "symbol", Got: unwrapped.Type()}
}
```

### Example 3: Starred Unpacking Assignment

**Before (Buggy):**
```go
if target == starIndex {
    starVar := starTarget.Items()[1].(core.SymbolValue)
    // ...
}
```

**After (Fixed):**
```go
unwrappedTarget := unwrapLocated(target)
if intTarget, ok := unwrappedTarget.(core.IntValue); ok && intTarget == starIndex {
    starVar, ok := starTarget.GetItemAsSymbol(1)
    if !ok {
        return nil, fmt.Errorf("star unpacking variable must be a symbol")
    }
    // ...
}
```

## How to Test for LocatedValue Issues

### Symptoms of LocatedValue Bugs

1. **Panic messages** containing "interface conversion: Value is LocatedValue, not <Type>"
2. **Unexpected type errors** where the correct type should work
3. **Tests failing** when run with AST/IR pipeline but passing with direct S-expression evaluation
4. **Starred unpacking failures** or other complex syntax not working

### Testing Strategy

1. **Run CPython compatibility tests** - These exercise the full AST→IR pipeline:
   ```bash
   ./bin/m28 tests/cpython/test_*.py
   ```

2. **Test with Python syntax** - Python syntax always goes through AST→IR which wraps values:
   ```python
   # This will expose LocatedValue issues
   first, *rest = [1, 2, 3, 4]
   ```

3. **Add debug logging** to see when values are wrapped:
   ```go
   if _, ok := value.(core.LocatedValue); ok {
       fmt.Printf("DEBUG: Value is wrapped in LocatedValue at %v\n", value)
   }
   ```

4. **Create minimal reproduction tests**:
   ```python
   # test_located_value_unwrap.py
   # Tests that starred unpacking works (requires unwrapping)
   first, *rest = [1, 2, 3]
   assert first == 1
   assert rest == [2, 3]
   ```

## Debugging Runbook

When you encounter a potential LocatedValue issue:

1. **Identify the panic location** in the stack trace
   ```
   panic: interface conversion: core.Value is core.LocatedValue, not core.SymbolValue
   ```

2. **Find the type assertion** that's failing
   - Look for direct type assertions: `value.(SomeType)`
   - Look for type switches without unwrapping

3. **Check if the value could be wrapped**
   - Values from AST nodes are usually wrapped
   - Values from list.Items() might be wrapped
   - Values from arguments to special forms might be wrapped

4. **Apply the fix**
   - Add `unwrapLocated()` call before the assertion
   - OR use smart accessor methods like `GetItemAsSymbol()`

5. **Add proper error handling**
   - Always use comma-ok idiom: `value, ok := unwrapped.(Type)`
   - Provide helpful error messages with actual type
   - Consider using unwrapped value in error messages

6. **Test the fix**
   - Run the failing test
   - Run related CPython compatibility tests
   - Add a regression test if needed

## Best Practices Summary

1. **Always unwrap before type assertions** - This is the cardinal rule
2. **Use smart accessors when available** - They're safer and cleaner
3. **Use comma-ok idiom** - Never use bare type assertions like `value.(Type)`
4. **Include type info in errors** - Use `unwrapped.Type()` in error messages
5. **Test with CPython tests** - These expose LocatedValue issues early
6. **Document assumptions** - Note when you expect wrapped vs unwrapped values

## Common Patterns

### Pattern: Safe Type Assertion
```go
unwrapped := unwrapLocated(value)
if sym, ok := unwrapped.(core.SymbolValue); ok {
    // Use sym
} else {
    return fmt.Errorf("expected symbol, got %v", unwrapped.Type())
}
```

### Pattern: Safe List Access with Type Check
```go
if sym, ok := list.GetItemAsSymbol(index); ok {
    // Use sym
} else {
    unwrapped, _ := list.GetItemUnwrapped(index)
    return fmt.Errorf("expected symbol at index %d, got %v", index, unwrapped.Type())
}
```

### Pattern: Safe Type Switch
```go
unwrapped := unwrapLocated(value)
switch t := unwrapped.(type) {
case core.SymbolValue:
    // Handle symbol
case core.IntValue:
    // Handle int
default:
    return fmt.Errorf("unexpected type: %v", t.Type())
}
```

## See Also

- `core/located_value.go` - LocatedValue type definition
- `eval/util.go` - unwrapLocated() function
- `core/containers.go` - Smart accessor methods
- `docs/design/compiler-architecture.md` - How AST→IR pipeline works
