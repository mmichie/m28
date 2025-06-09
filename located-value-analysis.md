# LocatedValue Implementation Analysis

## Current State

The `LocatedValue` type exists in the codebase but is **completely unused**. It was designed to wrap values with source location information for better error reporting, but:

1. **No Usage**: There are no references to `LocatedValue`, `WithLocation`, or `GetValueLocation` outside of the definition file itself
2. **No Parser Integration**: The parser tracks line/column internally but never creates `LocatedValue` instances
3. **No Error Integration**: Error types don't use source locations, despite having fields for them in `EvalError`

## Design Issues with LocatedValue

### 1. Type System Pollution

The current design makes `LocatedValue` implement the `Value` interface, which creates several problems:

```go
type LocatedValue struct {
    Value    Value
    Location *SourceLocation
}

func (lv LocatedValue) Type() Type {
    return lv.Unwrap().Type()
}
```

**Problems:**
- Every type check would need to unwrap: `if _, ok := val.(LocatedValue); ok { val = val.Unwrap() }`
- Type assertions become unreliable: `val.(core.NumberValue)` fails if val is wrapped
- Performance overhead of constant unwrapping
- Nested wrapping possibility requires recursive unwrapping

### 2. Interface Compliance Issues

If LocatedValue were used, it would need to implement every interface its wrapped value implements:
- `Object` interface methods (GetAttr, SetAttr, CallMethod)
- `Callable` interface (Call)
- `Iterable` interface (Iterator)
- Container interfaces (for lists, dicts, etc.)

This leads to massive code duplication or complex delegation logic.

### 3. Value Equality Problems

```go
// These would not be equal even if they represent the same value:
val1 := core.NumberValue(42)
val2 := WithLocation(core.NumberValue(42), "file.m28", 1, 1)
// val1 != val2
```

### 4. Method Dispatch Complexity

Every builtin function and method would need to handle both wrapped and unwrapped values:

```go
func Add(args []core.Value, ctx *core.Context) (core.Value, error) {
    // Would need to unwrap each argument
    left := unwrapValue(args[0])
    right := unwrapValue(args[1])
    // ... perform addition
}
```

## Where Unwrapping Would Be Needed

If LocatedValue were actually used, unwrapping would be required in:

1. **Type Assertions** (everywhere)
   - eval/evaluator.go: ~15 places
   - builtin/*.go: ~200+ places
   - core/*_methods.go: ~100+ places

2. **Equality Comparisons**
   - builtin/comparison.go
   - eval/evaluator.go (pattern matching)
   - core/dict_methods.go (key lookups)

3. **Arithmetic Operations**
   - builtin/arithmetic.go: all functions
   - builtin/math.go: all functions

4. **Container Operations**
   - List indexing/slicing
   - Dict key access
   - Set membership tests

5. **Function Calls**
   - Checking if value is Callable
   - Method dispatch
   - Argument validation

6. **String Operations**
   - String concatenation
   - Format strings
   - String methods

## Better Design Alternatives

### Option 1: Metadata Side Table

Instead of wrapping values, maintain a separate mapping:

```go
type SourceTracker struct {
    locations map[core.Value]*SourceLocation
}

func (st *SourceTracker) SetLocation(val core.Value, loc *SourceLocation) {
    st.locations[val] = loc
}

func (st *SourceTracker) GetLocation(val core.Value) *SourceLocation {
    return st.locations[val]
}
```

**Pros:**
- No type system pollution
- No unwrapping needed
- Values remain unchanged

**Cons:**
- Memory overhead for map
- Value identity issues with mutable values

### Option 2: Context-Based Tracking

Track source location in the evaluation context:

```go
type Context struct {
    // ... existing fields
    CurrentLocation *SourceLocation
}

// During evaluation:
ctx.CurrentLocation = &SourceLocation{File: "test.m28", Line: 10, Column: 5}
result, err := Eval(expr, ctx)
if err != nil {
    // Attach location from context to error
    return WrapErrorWithLocation(err, ctx.CurrentLocation)
}
```

**Pros:**
- No value wrapping
- Natural integration with stack traces
- Already have context everywhere

**Cons:**
- Less precise (statement-level, not expression-level)
- Location might be stale

### Option 3: AST-Based Location Tracking

Keep locations only in the AST, not in runtime values:

```go
type ASTNode struct {
    Value    core.Value
    Location *SourceLocation
    Children []*ASTNode
}
```

**Pros:**
- Clean separation of parse-time and runtime
- No runtime overhead
- Natural for error reporting during parsing

**Cons:**
- Loses location info after evaluation
- Would need AST node types (not just Values)

### Option 4: Error-Time Location Resolution

Only track locations when errors occur:

```go
type EvalError struct {
    // ... existing fields
    Expression core.Value // Keep the original expression
}

// When formatting error, walk back through AST/context to find location
```

**Pros:**
- Zero overhead for successful execution
- Simpler value types

**Cons:**
- More complex error handling
- Need to preserve more context

## Recommendation

The best approach for M28 would be **Option 2 (Context-Based Tracking)** combined with **Option 3 (AST nodes)** for parsing:

1. Create proper AST nodes during parsing that include location info
2. Pass location through context during evaluation
3. Attach location to errors when they occur
4. Keep runtime values clean and unwrapped

This provides good error messages without polluting the type system or requiring pervasive unwrapping.

## Implementation Impact

To properly implement source location tracking:

1. **Parser Changes**: Create AST nodes with locations instead of raw Values
2. **Evaluator Changes**: Pass locations through context
3. **Error Changes**: Enhance error types to always include location
4. **No Value Changes**: Keep Value types clean

This would be a significant refactoring but would provide much better error messages without the complexity of value wrapping.