# Duplicate Builtin Registrations Report

Generated: 2025-06-13
Updated: 2025-06-13 (After Phase 1 Cleanup)

## Summary

- **Total builtins registered**: 94 (was 110)
- **Builtins with duplicates**: 22 (was 38) 
- **Total duplicate registrations**: 25 (was 41)

### Phase 1 Complete ✅
- Removed 16 operator duplicates by deleting legacy arithmetic.go and comparison.go files

## Duplicate Registrations by Category

### 1. Operators ✅ RESOLVED 

All 16 operator duplicates have been resolved by removing legacy files and using only the modular structure:

| Operator | First Registration | Second Registration |
|----------|-------------------|-------------------|
| `+` | operators/arithmetic.go:12 | arithmetic.go:14 |
| `-` | operators/arithmetic.go:13 | arithmetic.go:15 |
| `*` | operators/arithmetic.go:14 | arithmetic.go:16 |
| `/` | operators/arithmetic.go:15 | arithmetic.go:17 |
| `%` | operators/arithmetic.go:16 | arithmetic.go:18 |
| `**` | operators/arithmetic.go:17 | arithmetic.go:19 |
| `==` | operators/comparison.go:11 | comparison.go:14 |
| `!=` | operators/comparison.go:12 | comparison.go:15 |
| `<` | operators/comparison.go:13 | comparison.go:16 |
| `<=` | operators/comparison.go:14 | comparison.go:17 |
| `>` | operators/comparison.go:15 | comparison.go:18 |
| `>=` | operators/comparison.go:16 | comparison.go:19 |
| `and` | operators/logical.go:12 | comparison.go:23 |
| `or` | operators/logical.go:13 | comparison.go:24 |
| `not` | operators/logical.go:14 | comparison.go:22 |
| `in` | operators/logical.go:15 | comparison.go:27 |

### 2. Math Functions (3 duplicates)

Math functions duplicated between numeric.go and modules/math.go:

| Function | First Registration | Second Registration |
|----------|-------------------|-------------------|
| `abs` | numeric.go:13 | modules/math.go:98 |
| `pow` | numeric.go:83 | modules/math.go:152 |
| `sum` | numeric.go:117 | modules/math.go:114 |

### 3. Functional Programming (3 functions with multiple registrations)

These functions are registered in 3 different files:

| Function | Registrations |
|----------|--------------|
| `map` | functional.go:12, list.go:23, utilities.go:112 |
| `filter` | functional.go:61, list.go:24, utilities.go:157 |
| `reduce` | functional.go:123, list.go:25, utilities.go:220 |

### 4. Other Functions (19 duplicates)

Various utility functions registered in multiple places:

| Function | First Registration | Second Registration |
|----------|-------------------|-------------------|
| `assert` | errors.go:73 | assert.go:12 |
| `callable` | functional.go:178 | essential_builtins.go:323 |
| `concat` | list.go:21 | utilities.go:353 |
| `divmod` | numeric.go:57 | essential_builtins.go:179 |
| `enumerate` | iteration.go:53 | utilities.go:14 |
| `error` | errors.go:27 | essential_builtins.go:18 |
| `getattr` | attributes.go:56 | essential_builtins.go:242 |
| `hasattr` | attributes.go:34 | essential_builtins.go:207 |
| `length` | string.go:22 | list.go:22 |
| `list` | collections.go:12 | list.go:13 |
| `next` | iteration.go:42 | utilities.go:395 |
| `range` | iteration.go:94 | list.go:14 |
| `reversed` | iteration.go:160 | list.go:27 |
| `round` | numeric.go:27 | essential_builtins.go:150 |
| `setattr` | attributes.go:91 | essential_builtins.go:288 |
| `zip` | iteration.go:193 | utilities.go:57 |

## Root Cause

The duplicates appear to be caused by:

1. **Parallel refactoring efforts**: The codebase has both:
   - New modular structure (operators/, modules/, methods/)
   - Legacy files (arithmetic.go, comparison.go, etc.)

2. **RegisterAllBuiltins calls both sets**: In `builtin/registry.go`, the `RegisterAllBuiltins` function calls both the new registration functions and the legacy ones.

3. **Essential builtins duplication**: The `essential_builtins.go` file appears to re-implement many functions that already exist in specialized files.

4. **Utilities overlap**: The `utilities.go` file duplicates many functions from `functional.go`, `iteration.go`, and other specialized files.

## Impact

- The last registration wins, so behavior depends on registration order
- Maintenance confusion - which implementation is the "real" one?
- Potential for bugs if implementations differ
- Wasted memory and initialization time

## Recommendations

1. **Remove legacy files**: Delete arithmetic.go, comparison.go after verifying operators/ has all functionality
2. **Consolidate utilities**: Move unique functions from utilities.go to appropriate specialized files
3. **Review essential_builtins.go**: Remove duplicates or make it import from other modules
4. **Single source of truth**: Each builtin should be defined in exactly one place
5. **Enable strict checking in CI**: Set `StrictBuiltinChecking = true` in tests to catch future duplicates