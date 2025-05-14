# For Loop Implementation Fix

## Issue Description

The implementation of the `for` loop in M28 had an issue where it would fail with the error message:

```
RuntimeError: iteration variable must be a symbol
```

This occurred because the parser wraps expressions with source location information (`LocatedValue`) for better error reporting, but the `EvalFor` function wasn't unwrapping this value before checking if it was a symbol.

## Implementation Details

The fix involved adding a helper function in `special_forms/control_flow.go` to unwrap `LocatedValue` instances:

```go
// unwrapLocatedValue extracts the value from a LocatedValue
func unwrapLocatedValue(expr core.LispValue) core.LispValue {
    if located, ok := expr.(core.LocatedValue); ok {
        return located.Value
    }
    return expr
}
```

Then, we modified the `EvalFor` function to use this helper when checking its arguments:

```go
func EvalFor(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
    if len(args) < 3 {
        return nil, fmt.Errorf("for loop requires at least 3 arguments")
    }

    // Unwrap the first argument in case it's wrapped in a LocatedValue
    unwrappedArg := unwrapLocatedValue(args[0])
    
    iterVar, ok := unwrappedArg.(core.LispSymbol)
    if !ok {
        return nil, fmt.Errorf("iteration variable must be a symbol, got %T", unwrappedArg)
    }

    // Rest of function remains the same...
}
```

## Testing

The fix was thoroughly tested with:

1. Basic for loop functionality
2. Nested for loops 
3. For loops with break/continue statements
4. For loops with complex flow control
5. Interaction with various data structures

The test cases can be found in:
- `/tests/for-loop-fix-test.m28`
- `/tests/break-continue-test.m28`
- `/tests/advanced-for-loop-test.m28`

## Considerations

This pattern of unwrapping `LocatedValue` before type checks may need to be applied to other special forms or functions that expect specific types. The evaluator already has similar unwrapping logic, but special forms that directly process their arguments need to handle this explicitly.

## Benefits

With this fix:

1. For loops now work correctly in all contexts
2. Break and continue statements work properly in all loop types
3. Source location information is still preserved for error reporting
4. The implementation is more robust and handles nested loops correctly

The fix ensures that for loops behave as expected while maintaining the benefits of source location tracking for better error messages.