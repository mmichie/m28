# Type Migration Backlog

This file tracks remaining builtin files that need to be migrated to use the new type helpers.

## Migration Status Summary

### ✅ Already Migrated (Phase 1.5)
- [x] builtin/numeric.go
- [x] builtin/string_ops.go  
- [x] builtin/collections.go
- [x] builtin/type_checking.go
- [x] builtin/functional.go
- [x] builtin/iteration.go
- [x] builtin/attributes.go
- [x] builtin/list.go
- [x] builtin/dict.go
- [x] builtin/modules/math.go

### 🔴 High Priority - Needs Migration
These files have extensive manual type checking and would benefit most from migration:

- [ ] **builtin/string_search.go** (~42 manual type checks)
  - Functions: contains, index, find, startswith, endswith, count, etc.
  - Pattern: Every function has `len(args)` check + manual type assertions
  - Benefit: ~50% code reduction expected

- [ ] **builtin/os.go** (~35+ manual type checks)
  - Functions: File operations, path checking, environment vars
  - Pattern: Path string validation in every function
  - Benefit: Consistent path validation, cleaner code

- [ ] **builtin/json.go** (Complex validation)
  - Functions: json.dumps, json.loads with optional parameters
  - Pattern: Complex optional parameter handling
  - Benefit: Could use GetNumberOrDefault for indent

- [ ] **builtin/pathlib.go** (~20+ type checks)
  - Functions: Path methods with string arguments
  - Pattern: Repetitive string argument validation
  - Benefit: Unified path operation validation

### 🟡 Medium Priority
Moderate benefits from migration:

- [ ] **builtin/string_format.go**
  - Format string parsing and validation
  - Would benefit from cleaner validation patterns

- [ ] **builtin/errors.go**
  - Exception handling functions
  - Simple patterns but could be more consistent

- [ ] **builtin/misc.go**
  - Utility functions: repr, hash, id, etc.
  - Each has similar argument count validation

- [ ] **builtin/modules/os.go**
  - OS module functions
  - Similar patterns to builtin/os.go

- [ ] **builtin/modules/json.go**
  - JSON module implementation
  - Could share patterns with builtin/json.go

- [ ] **builtin/modules/pathlib.go**
  - Path module implementation
  - Complex but consistent patterns

### 🟢 Low Priority
Minor improvements possible:

- [ ] **builtin/assert.go** - Simple, only 2 checks
- [ ] **builtin/utilities.go** - Already simplified
- [ ] **builtin/essential_builtins.go** - Complex intertwined logic
- [ ] **builtin/datetime.go** - Date/time operations
- [ ] **builtin/time.go** - Time module
- [ ] **builtin/random.go** - Random number generation
- [ ] **builtin/shutil.go** - Shell utilities
- [ ] **builtin/async.go** - Async operations

## Migration Guidelines

When migrating a file:

1. Replace manual `len(args)` checks with `validation.NewArgs()`
2. Use `v.Exact()`, `v.Range()`, `v.Min()`, `v.Max()` for count validation
3. Replace type assertions with `v.GetString()`, `v.GetNumber()`, etc.
4. Use `As*` helpers for optional type conversions
5. Use `Require*` helpers when type is mandatory
6. Ensure consistent error messages via the errors package

## Example Migration Pattern

### Before:
```go
if len(args) != 2 {
    return nil, fmt.Errorf("function requires 2 arguments, got %d", len(args))
}
str, ok := args[0].(core.StringValue)
if !ok {
    return nil, fmt.Errorf("first argument must be string, got %s", args[0].Type())
}
```

### After:
```go
v := validation.NewArgs("function", args)
if err := v.Exact(2); err != nil {
    return nil, err
}
str, err := v.GetString(0)
if err != nil {
    return nil, err
}
```

## Benefits Tracking

Files migrated so far have shown:
- 40-50% reduction in validation code
- Consistent error messages
- Improved readability
- Easier maintenance
- Reduced chance of bugs in type checking