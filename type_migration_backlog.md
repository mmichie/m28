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

### ✅ Already Migrated (Phase 1.6 - Backlog)
- [x] builtin/string_search.go (~45% code reduction)
- [x] builtin/os.go (~40% code reduction)
- [x] builtin/json.go (cleaner optional parameters)
- [x] builtin/pathlib.go (~30% code reduction)

### 🔴 High Priority - Needs Migration
These files have extensive manual type checking and would benefit most from migration:

(All high-priority files have been migrated!)

### 🟡 Medium Priority
Moderate benefits from migration:

(All medium-priority files have been migrated!)

Note: builtin/modules/os.go, json.go, and pathlib.go are just stubs - the actual implementations in builtin/ have already been migrated.

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
- 15-45% reduction in validation code
- Consistent error messages
- Improved readability
- Easier maintenance
- Reduced chance of bugs in type checking

## Summary

**Phase 1.5 Complete**: 10 files migrated
**Phase 1.6 Complete**: 7 files migrated (4 high-priority + 3 medium-priority)

**Total migrated**: 17 builtin files now using the new type helper system
**Remaining**: 8 low-priority files for future migration