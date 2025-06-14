# Type Helpers Migration - Actionable Task List

## Quick Reference
- **Validation helpers**: `common/validation/validation.go`
- **Type extractors**: `common/types/extraction.go`
- **Example patterns**: `/Users/mim/src/m28/examples/type_helpers_migration_example.go`

## High Priority Files (Most Repetitive Patterns)

### 1. `builtin/numeric.go` - ~50% code reduction potential
- [x] `abs()` - Lines 14-24 ✓ Migrated
- [x] `round()` - Lines 28-54 ✓ Migrated
- [x] `divmod()` - Lines 58-80 ✓ Migrated
- [x] `pow()` - Lines 84-120 ✓ Migrated
- [x] `sum()` - Lines 119-155 ✓ Migrated
- Pattern: All use manual `len(args)` checks and type assertions

### 2. `builtin/string_ops.go` - ~40% code reduction potential
- [x] `UpperFunc()` - Lines 32-43 ✓ Migrated
- [x] `LowerFunc()` - Lines 46-57 ✓ Migrated
- [x] `TrimFunc()` - Lines 60-71 ✓ Migrated
- [x] `LStripFunc()` - Lines 79-101 ✓ Migrated
- [x] `RStripFunc()` - Lines 103-125 ✓ Migrated
- [x] `ReplaceFunc()` - Lines 128-160 ✓ Migrated
- [x] `SplitFunc()` - Lines 163-201 ✓ Migrated
- [x] `JoinFunc()` - Lines 204-237 ✓ Migrated
- [x] `SubstringFunc()` - Lines 240-315 ✓ Migrated
- Pattern: Identical validation patterns, string type assertions

### 3. `builtin/collections.go` - Complex type switches
- [ ] `list()` constructor - Lines 12-57
- [ ] `tuple()` constructor - Lines 60-95
- [ ] `dict()` constructor - Lines 98-140
- [ ] `set()` constructor - Lines 143-185
- Pattern: Complex iterable handling, could use `AsIterable()`

### 4. `builtin/type_checking.go` - Can use helpers internally
- [ ] `isinstance()` - Lines 15-35
- [ ] `type()` - Lines 38-48
- [ ] `callable()` - Lines 51-65
- Pattern: Manual type checking that helpers now provide

### 5. `builtin/functional.go` - Callable validation
- [ ] `map()` - Callable validation
- [ ] `filter()` - Callable validation  
- [ ] `reduce()` - Callable validation
- Pattern: All need callable as first argument

## Medium Priority Files

### 6. `builtin/iteration.go`
- [ ] Functions using iterable type switches
- [ ] Can use `GetIterable()` helper

### 7. `builtin/attributes.go`
- [ ] `hasattr()` - Object type checking
- [ ] `getattr()` - Object type checking
- [ ] `setattr()` - Object type checking
- Pattern: Need object/instance validation

### 8. `eval/evaluator.go`
- [ ] `assignForm()` - Line 333
- [ ] `defForm()` - Line 240
- Pattern: Special forms with manual validation

### 9. `builtin/modules/math.go`
- [ ] All math functions use similar numeric validation
- Pattern: Numeric argument validation

### 10. `builtin/list_kwargs.go`
- [ ] Already partially migrated
- [ ] Complete migration for consistency

## Migration Checklist for Each Function

1. **Before Migration**:
   - [ ] Identify all type assertions
   - [ ] Note error message format
   - [ ] Check for special cases

2. **During Migration**:
   - [ ] Replace `len(args)` checks with validation methods
   - [ ] Replace type assertions with extraction methods
   - [ ] Preserve exact error message semantics

3. **After Migration**:
   - [ ] Run existing tests
   - [ ] Verify error messages match
   - [ ] Check edge cases

## Code Patterns to Replace

### Pattern 1: Exact argument count
```go
// Before
if len(args) != 1 {
    return nil, fmt.Errorf("func() takes exactly one argument (%d given)", len(args))
}

// After
v := validation.NewArgs("func", args)
if err := v.Exact(1); err != nil {
    return nil, err
}
```

### Pattern 2: Type assertion with error
```go
// Before
str, ok := args[0].(core.StringValue)
if !ok {
    return nil, fmt.Errorf("func requires a string argument, got %s", args[0].Type())
}

// After
str, err := v.GetString(0)
if err != nil {
    return nil, err
}
```

### Pattern 3: Optional arguments
```go
// Before
var defaultVal = "default"
if len(args) >= 2 {
    if s, ok := args[1].(core.StringValue); ok {
        defaultVal = string(s)
    } else {
        return nil, fmt.Errorf("second argument must be string")
    }
}

// After
defaultVal, err := v.GetStringOrDefault(1, "default")
if err != nil {
    return nil, err
}
```

### Pattern 4: Variadic numeric functions
```go
// Before
for i, arg := range args {
    num, ok := arg.(core.NumberValue)
    if !ok {
        return nil, fmt.Errorf("argument %d must be number", i+1)
    }
    // process num
}

// After
numbers, err := v.ExtractNumbers()
if err != nil {
    return nil, err
}
for _, num := range numbers {
    // process num
}
```

## Success Metrics
- Reduced lines of code in validation logic
- Consistent error messages across all functions
- Easier to add new validation rules
- Improved maintainability