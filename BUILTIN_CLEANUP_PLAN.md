# Builtin Cleanup Plan

## Overview
The M28 codebase currently has 38 builtin functions with duplicate registrations across multiple files. This document outlines a phased approach to eliminate these duplicates while maintaining functionality and test coverage.

## Current State
- **Total builtins**: 110
- **Duplicate registrations**: 38 functions (some registered 3 times)
- **Registry system**: Now tracks all registrations with location info
- **Test coverage**: All 36 tests passing (100% success rate)

## Phase 1: Critical Operator Duplicates (16 functions)

### Goal
Consolidate all operators into the modular `builtin/operators/` structure and remove legacy files.

### Operators to consolidate:
- **Arithmetic**: `+`, `-`, `*`, `/`, `%`, `**` 
- **Comparison**: `==`, `!=`, `<`, `<=`, `>`, `>=`
- **Logical**: `and`, `or`, `not`, `in`

### Steps:
1. **Verify operator implementations are equivalent**
   - Compare operators/arithmetic.go vs arithmetic.go
   - Compare operators/comparison.go vs comparison.go
   - Compare operators/logical.go vs comparison.go (logical ops)

2. **Update registry.go to remove legacy registrations**
   - Comment out `RegisterArithmeticFunctions(ctx)`
   - Comment out `RegisterComparisonFunctions(ctx)`
   - Remove logical operator registrations from comparison.go

3. **Run tests to ensure nothing breaks**
   - All arithmetic operations should work
   - All comparison operations should work
   - Logical operations should work

4. **Delete legacy files**
   - Remove `builtin/arithmetic.go`
   - Remove `builtin/comparison.go`
   - Update any imports

## Phase 2: High-Value Function Duplicates (6 functions)

### Goal
Consolidate functions that are registered 3 times into single implementations.

### Functions to consolidate:
- `map` - currently in functional.go, list.go, utilities.go
- `filter` - currently in functional.go, list.go, utilities.go  
- `reduce` - currently in functional.go, list.go, utilities.go

### Decision: Keep in functional.go
These are fundamentally functional programming constructs and belong in functional.go.

### Steps:
1. **Compare implementations**
   - Ensure functional.go has the most complete implementation
   - Note any differences in behavior

2. **Remove from other files**
   - Remove from list.go
   - Remove from utilities.go
   - Update registry.go if needed

3. **Test thoroughly**
   - Functional programming tests
   - List manipulation tests
   - Any code using map/filter/reduce

## Phase 3: Utility Function Cleanup (19 functions)

### Goal
Establish single source of truth for remaining duplicate functions.

### Functions to clean up:
| Function | Current Locations | Recommended Location |
|----------|------------------|---------------------|
| `abs` | numeric.go, modules/math.go | Keep in numeric.go |
| `assert` | errors.go, assert.go | Keep in assert.go |
| `callable` | functional.go, essential_builtins.go | Keep in functional.go |
| `concat` | list.go, utilities.go | Keep in list.go |
| `divmod` | numeric.go, essential_builtins.go | Keep in numeric.go |
| `enumerate` | iteration.go, utilities.go | Keep in iteration.go |
| `error` | errors.go, essential_builtins.go | Keep in errors.go |
| `getattr` | attributes.go, essential_builtins.go | Keep in attributes.go |
| `hasattr` | attributes.go, essential_builtins.go | Keep in attributes.go |
| `length` | string.go, list.go | Create in utilities.go (works on multiple types) |
| `list` | collections.go, list.go | Keep in collections.go |
| `next` | iteration.go, utilities.go | Keep in iteration.go |
| `pow` | numeric.go, modules/math.go | Keep in numeric.go |
| `range` | iteration.go, list.go | Keep in iteration.go |
| `reversed` | iteration.go, list.go | Keep in iteration.go |
| `round` | numeric.go, essential_builtins.go | Keep in numeric.go |
| `setattr` | attributes.go, essential_builtins.go | Keep in attributes.go |
| `sum` | numeric.go, modules/math.go | Keep in numeric.go |
| `zip` | iteration.go, utilities.go | Keep in iteration.go |

### Steps for each function:
1. Compare implementations for any differences
2. Keep the most complete/correct implementation
3. Remove from other locations
4. Update imports and registry
5. Test the specific function

## Phase 4: Essential Builtins Review

### Goal
Review essential_builtins.go and remove any remaining duplicates.

### Approach:
1. For each function in essential_builtins.go:
   - Check if it exists elsewhere
   - If duplicate, remove from essential_builtins.go
   - If unique, consider moving to appropriate module

2. Consider renaming essential_builtins.go to something more specific
   - Perhaps type_conversions.go if that's what remains
   - Or split into multiple focused files

## Phase 5: Utilities.go Consolidation

### Goal
Clean up utilities.go to only contain truly utility functions.

### Approach:
1. Move iteration-related functions to iteration.go
2. Move functional programming functions to functional.go
3. Move type-related functions to appropriate type files
4. Keep only cross-cutting utilities

## Testing Strategy

### Before each phase:
1. Run full test suite: `./test.sh`
2. Save output for comparison

### After each change:
1. Run tests again
2. Compare output to ensure no regressions
3. Test specific functionality that was changed

### Additional testing:
1. REPL testing of affected functions
2. Run example programs that use the functions
3. Performance comparison if implementations differ

## Migration Tools

### Helper script to find usages:
```bash
# Find all uses of a function
grep -r "function_name" --include="*.m28" tests/ examples/
```

### Registry inspection:
```go
// Add temporary debug code to print duplicates
fmt.Printf("Duplicates: %v\n", core.GetDuplicateBuiltins())
```

## Success Criteria

1. **No duplicate registrations**: Registry shows 0 duplicates
2. **All tests pass**: 100% success rate maintained
3. **No functionality lost**: All builtins still work
4. **Cleaner codebase**: Each builtin has single, clear location
5. **Better organization**: Related functions grouped together

## Rollback Plan

If issues arise:
1. Git history preserves all changes
2. Can selectively revert specific phases
3. Registry system helps identify what changed
4. Test suite validates functionality

## Timeline Estimate

- Phase 1 (Operators): 2-3 hours
- Phase 2 (map/filter/reduce): 1-2 hours  
- Phase 3 (Utilities): 3-4 hours
- Phase 4 (Essential review): 2-3 hours
- Phase 5 (Final cleanup): 1-2 hours

Total: 9-14 hours of focused work

## Next Steps

1. Start with Phase 1 (operators) as they're most critical
2. Create branch for each phase for easy rollback
3. Document any behavioral differences found
4. Update this plan as work progresses