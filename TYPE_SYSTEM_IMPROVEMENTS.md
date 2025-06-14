# Type System Improvements - Detailed Task Breakdown

## Overview
This document breaks down the Type System Improvements into specific, actionable tasks. The goal is to eliminate repetitive type checking code and establish clear patterns for type operations.

## Key Deliverables
1. **Type Extraction Utilities** - Replace hundreds of type assertions with simple helper functions
2. **Protocol Interfaces** - Define standard interfaces for numeric, container, and callable operations  
3. **TypeSwitch Builder** - Elegant handling of complex type switches
4. **Dunder Method Helpers** - Standardize Python protocol method calls
5. **Type Error Builder** - Consistent, user-friendly error messages
6. **Migration Guide** - Clear path for updating existing code

## Expected Impact
- **40-60% code reduction** in type-heavy functions
- **Eliminate 51+ repetitive type switches** across the codebase
- **Standardize error messages** for better developer experience
- **Enable future features** like operator overloading and protocols

## Phase 1: Type Assertion Helpers (Week 1)

### Task 1.1: Create Type Extraction Utilities
**File**: `common/types/extraction.go`
**Effort**: 2-3 hours

```go
package types

import "github.com/mmichie/m28/core"

// Basic type extractors
func AsNumber(v core.Value) (float64, bool)
func AsString(v core.Value) (string, bool)
func AsBool(v core.Value) (bool, bool)
func AsList(v core.Value) (core.ListValue, bool)
func AsDict(v core.Value) (core.DictValue, bool)
func AsSet(v core.Value) (core.SetValue, bool)
func AsTuple(v core.Value) (core.TupleValue, bool)
func AsFunction(v core.Value) (core.FunctionValue, bool)
func AsClass(v core.Value) (*core.ClassValue, bool)
func AsInstance(v core.Value) (*core.InstanceValue, bool)

// With error variants
func RequireNumber(v core.Value, context string) (float64, error)
func RequireString(v core.Value, context string) (string, error)
func RequireList(v core.Value, context string) (core.ListValue, error)
// ... etc
```

**Benefits**:
- Replace hundreds of `if val, ok := v.(core.NumberValue); ok` patterns
- Consistent error messages
- Single place to add type coercion logic later

### Task 1.2: Create Type Checking Predicates
**File**: `common/types/predicates.go`
**Effort**: 1-2 hours

```go
// Type checking functions that return bool
func IsNumber(v core.Value) bool
func IsString(v core.Value) bool
func IsContainer(v core.Value) bool  // List, Tuple, Dict, Set
func IsSequence(v core.Value) bool   // List, Tuple, String
func IsMapping(v core.Value) bool    // Dict
func IsCallable(v core.Value) bool   // Function, Class, or has __call__
func IsIterable(v core.Value) bool   // Has __iter__ or is sequence
func IsHashable(v core.Value) bool   // Already exists, but standardize
```

### Task 1.3: Migrate a Sample Package
**Target**: `builtin/numeric.go`
**Effort**: 2-3 hours

- Replace all type assertions with new helpers
- Measure code reduction
- Document patterns for other migrations

## Phase 2: Protocol Interfaces (Week 1-2)

### Task 2.1: Define Core Protocol Interfaces
**File**: `core/protocols/base.go`
**Effort**: 3-4 hours

```go
package protocols

// Numeric protocol for arithmetic operations
type Numeric interface {
    Add(other core.Value) (core.Value, error)
    Subtract(other core.Value) (core.Value, error)
    Multiply(other core.Value) (core.Value, error)
    Divide(other core.Value) (core.Value, error)
    // ... other numeric operations
}

// Indexable protocol for container access
type Indexable interface {
    GetIndex(index core.Value) (core.Value, error)
    SetIndex(index, value core.Value) error
    HasIndex(index core.Value) bool
}

// Sizable protocol for len() support
type Sizable interface {
    Size() int
}

// Container protocol combines common container operations
type Container interface {
    Sizable
    Contains(item core.Value) bool
    IsEmpty() bool
}
```

### Task 2.2: Create Protocol Checking Utilities
**File**: `common/types/protocols.go`
**Effort**: 2-3 hours

```go
// Check if value implements protocol
func ImplementsNumeric(v core.Value) bool
func ImplementsIndexable(v core.Value) bool
func ImplementsContainer(v core.Value) bool

// Get protocol implementation
func AsNumeric(v core.Value) (Numeric, bool)
func AsIndexable(v core.Value) (Indexable, bool)
func AsContainer(v core.Value) (Container, bool)
```

### Task 2.3: Implement Protocols for Existing Types
**Files**: Various type definitions
**Effort**: 4-5 hours

- Make NumberValue implement Numeric
- Make ListValue, DictValue, etc. implement Indexable and Container
- Ensure backward compatibility

## Phase 3: Type Switch Builder (Week 2)

### Task 3.1: Create TypeSwitch Builder
**File**: `common/types/switch.go`
**Effort**: 3-4 hours

```go
type TypeSwitch struct {
    value   core.Value
    handled bool
    result  core.Value
    err     error
}

func Switch(v core.Value) *TypeSwitch

func (ts *TypeSwitch) Number(fn func(float64) (core.Value, error)) *TypeSwitch
func (ts *TypeSwitch) String(fn func(string) (core.Value, error)) *TypeSwitch
func (ts *TypeSwitch) List(fn func(core.ListValue) (core.Value, error)) *TypeSwitch
func (ts *TypeSwitch) Default(fn func(core.Value) (core.Value, error)) *TypeSwitch
func (ts *TypeSwitch) Result() (core.Value, error)

// Usage example:
result, err := types.Switch(value).
    Number(func(n float64) (core.Value, error) {
        return core.NumberValue(n * 2), nil
    }).
    String(func(s string) (core.Value, error) {
        return core.StringValue(s + s), nil
    }).
    Default(func(v core.Value) (core.Value, error) {
        return nil, fmt.Errorf("unsupported type: %s", v.Type())
    }).
    Result()
```

### Task 3.2: Migrate Complex Type Switches
**Target**: `eval/evaluator.go` (tryForm function)
**Effort**: 3-4 hours

- Identify the most complex type switches
- Rewrite using TypeSwitch builder
- Document patterns

## Phase 4: Dunder Method Utilities (Week 2-3)

### Task 4.1: Create Dunder Method Helper
**File**: `common/types/dunder.go`
**Effort**: 3-4 hours

```go
// Call a dunder method if it exists
func CallDunder(obj core.Value, method string, args []core.Value, ctx *core.Context) (core.Value, bool, error)

// Check if dunder method exists
func HasDunder(obj core.Value, method string) bool

// Get dunder method
func GetDunder(obj core.Value, method string) (core.Value, bool)

// Common dunder method calls
func CallAdd(obj, other core.Value, ctx *core.Context) (core.Value, bool, error)
func CallLen(obj core.Value, ctx *core.Context) (int, bool, error)
func CallStr(obj core.Value, ctx *core.Context) (string, bool, error)
func CallRepr(obj core.Value, ctx *core.Context) (string, bool, error)
func CallBool(obj core.Value, ctx *core.Context) (bool, bool, error)
```

### Task 4.2: Refactor Operator Implementations
**Files**: `builtin/operators/*.go`
**Effort**: 4-5 hours

- Use dunder helpers instead of manual attribute lookups
- Ensure consistent behavior across all operators

## Phase 5: Type Error Standardization (Week 3)

### Task 5.1: Create Type Error Builder
**File**: `common/types/errors.go`
**Effort**: 2-3 hours

```go
type TypeErrorBuilder struct {
    function string
    position int
    expected []string
    got      core.Value
}

func NewTypeError(function string) *TypeErrorBuilder
func (b *TypeErrorBuilder) Position(pos int) *TypeErrorBuilder
func (b *TypeErrorBuilder) Expect(types ...string) *TypeErrorBuilder
func (b *TypeErrorBuilder) Got(value core.Value) *TypeErrorBuilder
func (b *TypeErrorBuilder) Build() error

// Usage:
err := types.NewTypeError("add").
    Position(1).
    Expect("number", "string").
    Got(value).
    Build()
// Produces: "add() argument 1 must be number or string, not list"
```

### Task 5.2: Create Common Error Patterns
**File**: `common/types/errors_common.go`
**Effort**: 2-3 hours

```go
// Pre-built error creators for common cases
func ArgumentCountError(fn string, expected, got int) error
func NotIterableError(fn string, value core.Value) error
func NotCallableError(value core.Value) error
func NotIndexableError(value core.Value) error
func IndexError(container core.Value, index core.Value) error
func KeyError(dict core.Value, key core.Value) error
```

## Phase 6: Migration and Documentation (Week 3-4)

### Task 6.1: Create Migration Guide
**File**: `common/types/MIGRATION.md`
**Effort**: 2-3 hours

- Document all new utilities
- Provide before/after examples
- Create decision tree for which utility to use

### Task 6.2: Systematic Migration
**Effort**: 1-2 days per package

**Order of migration**:
1. `builtin/type_checking.go` - Highest impact
2. `builtin/operators/` - Already uses builders
3. `eval/indexing.go` - Complex type switches
4. `special_forms/` - Various type checks
5. `builtin/methods/` - Container operations

### Task 6.3: Add Tests
**File**: `common/types/*_test.go`
**Effort**: 4-5 hours

- Unit tests for all utilities
- Integration tests showing real usage
- Benchmark comparisons

## Success Metrics

1. **Code Reduction**: Target 40-60% reduction in type-related code
2. **Performance**: No regression in benchmarks
3. **Consistency**: All type errors follow same format
4. **Coverage**: 80%+ of type assertions use new utilities
5. **Developer Experience**: Easier to add new type operations

## Example: Before and After

### Before (current code):
```go
func someFunction(args []core.Value) (core.Value, error) {
    if len(args) != 2 {
        return nil, fmt.Errorf("function takes 2 arguments, got %d", len(args))
    }
    
    var x, y float64
    if num, ok := args[0].(core.NumberValue); ok {
        x = float64(num)
    } else {
        return nil, fmt.Errorf("first argument must be number, not %s", args[0].Type())
    }
    
    if num, ok := args[1].(core.NumberValue); ok {
        y = float64(num)
    } else {
        return nil, fmt.Errorf("second argument must be number, not %s", args[1].Type())
    }
    
    return core.NumberValue(x + y), nil
}
```

### After (with improvements):
```go
func someFunction(args []core.Value) (core.Value, error) {
    if err := validation.Args("someFunction", args).Count(2); err != nil {
        return nil, err
    }
    
    x, err := types.RequireNumber(args[0], "someFunction() argument 1")
    if err != nil {
        return nil, err
    }
    
    y, err := types.RequireNumber(args[1], "someFunction() argument 2")
    if err != nil {
        return nil, err
    }
    
    return core.NumberValue(x + y), nil
}
```

### Or with TypeSwitch:
```go
return types.Switch(value).
    Number(func(n float64) (core.Value, error) {
        return core.NumberValue(n * 2), nil
    }).
    String(func(s string) (core.Value, error) {
        return core.StringValue(s + s), nil
    }).
    List(func(l core.ListValue) (core.Value, error) {
        return core.NumberValue(float64(len(l.Elements))), nil
    }).
    Default(func(v core.Value) (core.Value, error) {
        return nil, types.NotSupportedError("double", v)
    }).
    Result()
```

## Timeline Summary

- **Week 1**: Type assertion helpers and basic protocol interfaces
- **Week 2**: TypeSwitch builder and dunder method utilities
- **Week 3**: Error standardization and begin migration
- **Week 4**: Complete migration and documentation

Total effort: ~80-100 hours (2-3 weeks of focused work)