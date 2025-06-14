# Type Helpers Migration Report

This report identifies files in the M28 codebase that could benefit from using the new type helpers in `common/types/extraction.go` and `common/validation/validation.go`.

## Summary of Available Type Helpers

### Type Extraction (common/types/extraction.go)
- `AsNumber`, `AsString`, `AsBool`, `AsList`, `AsDict`, `AsSet`, `AsTuple`, `AsCallable`, `AsClass`, `AsInstance`, `AsIterable`, `AsRange`
- `RequireNumber`, `RequireString`, `RequireBool`, `RequireList`, `RequireDict`, `RequireSet`, `RequireTuple`, `RequireCallable`, `RequireClass`, `RequireInstance`, `RequireIterable`, `RequireRange`

### Validation (common/validation/validation.go)
- Argument count validation: `Exact()`, `Range()`, `Min()`, `Max()`
- Type extraction with validation: `GetNumber()`, `GetString()`, `GetBool()`, `GetList()`, `GetDict()`, `GetCallable()`, `GetSequence()`, `GetIterable()`
- Optional extraction with defaults: `GetNumberOrDefault()`, `GetStringOrDefault()`, `GetBoolOrDefault()`
- Bulk extraction: `ExtractNumbers()`, `ExtractStrings()`

## Files with High Migration Priority

### 1. builtin/numeric.go
**Current patterns:**
- Manual `len(args) != N` checks (lines 14, 28, 58, 84)
- Manual type assertions: `args[0].(core.NumberValue)` (lines 18, 32, 62-63, 88-89, 93-94)
- Manual error formatting

**Migration opportunities:**
```go
// Before:
if len(args) != 1 {
    return nil, fmt.Errorf("abs() takes exactly one argument (%d given)", len(args))
}
switch v := args[0].(type) {
case core.NumberValue:
    return core.NumberValue(math.Abs(float64(v))), nil
default:
    return nil, fmt.Errorf("abs() argument must be a number, not '%s'", v.Type())
}

// After:
v := validation.NewArgs("abs", args)
if err := v.Exact(1); err != nil {
    return nil, err
}
num, err := v.GetNumber(0)
if err != nil {
    return nil, err
}
return core.NumberValue(math.Abs(num)), nil
```

### 2. builtin/string_ops.go
**Current patterns:**
- Repeated `len(args) != 1` checks (lines 33, 47, 61, 80)
- Manual string type assertions (lines 37, 51, 65, 84)
- Duplicate error formatting

**Migration opportunities:**
```go
// Before:
if len(args) != 1 {
    return nil, fmt.Errorf("upper requires exactly 1 argument, got %d", len(args))
}
str, ok := args[0].(core.StringValue)
if !ok {
    return nil, fmt.Errorf("upper requires a string argument, got %s", args[0].Type())
}

// After:
v := validation.NewArgs("upper", args)
if err := v.Exact(1); err != nil {
    return nil, err
}
str, err := v.GetString(0)
if err != nil {
    return nil, err
}
```

### 3. builtin/collections.go
**Current patterns:**
- Complex type switches for iterable handling (lines 18-53, 66-80)
- Manual iterable checking
- Could use `AsIterable()` or `RequireIterable()`

### 4. eval/evaluator.go
**Current patterns:**
- Manual argument validation for special forms
- Type assertions in assignment forms (lines 341-349)
- Could benefit from structured validation

### 5. builtin/type_checking.go
**Current patterns:**
- Manual type checking implementations
- Could use the type extraction helpers internally

## Files with Medium Migration Priority

### 6. builtin/list.go
- Already partially migrated to use validation helpers
- Some functions could still benefit from more migration

### 7. builtin/iteration.go
- Manual iterable checking
- Type switches for different sequence types

### 8. builtin/functional.go
- Callable type checking
- Manual argument validation

### 9. builtin/attributes.go
- Object and instance type checking
- Attribute access validation

### 10. eval/dot_notation.go
- Complex type checking for dot notation
- Could benefit from `AsInstance()`, `AsDict()` helpers

## Migration Benefits

1. **Reduced Code Duplication**: Replace repetitive validation patterns
2. **Consistent Error Messages**: Standardized error formatting
3. **Type Safety**: Centralized type checking logic
4. **Maintainability**: Easier to update validation logic
5. **Readability**: Clearer intent with named methods

## Recommended Migration Strategy

1. Start with high-frequency functions in `builtin/numeric.go` and `builtin/string_ops.go`
2. Migrate one file at a time, testing after each migration
3. Update tests to verify error messages remain consistent
4. Consider creating additional helpers for common patterns discovered during migration
5. Document any edge cases or special handling required

## Potential New Helpers

Based on patterns observed:
1. `RequireInteger()` - for functions that need integer values
2. `GetOptionalString()` - for optional string parameters with None as default
3. `RequirePositiveNumber()` - for functions needing positive numbers
4. `AsSequence()` - unified sequence type checking