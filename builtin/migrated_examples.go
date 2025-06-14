// Package builtin - Examples of migrated functions using the new validation framework.
// This file demonstrates how to refactor existing functions to use common/validation and common/errors.
// These examples should serve as templates for migrating the rest of the builtin functions.

package builtin

import (
	"math"

	"github.com/mmichie/m28/common/errors"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// EXAMPLE 1: Simple unary function (abs)
// Before: Manual validation with fmt.Errorf
// After: Clean validation with proper error types

// AbsMigrated demonstrates migrating a simple numeric function
func AbsMigrated(args []core.Value, ctx *core.Context) (core.Value, error) {
	// Create validator
	v := validation.NewArgs("abs", args)

	// Validate argument count
	if err := v.Exact(1); err != nil {
		return nil, err
	}

	// Extract and validate type
	num, err := v.GetNumber(0)
	if err != nil {
		return nil, err
	}

	// Business logic
	if num < 0 {
		return core.NumberValue(-num), nil
	}
	return core.NumberValue(num), nil
}

// EXAMPLE 2: Function with optional arguments (round)
// Demonstrates handling optional parameters with defaults

// RoundMigrated shows how to handle optional arguments
func RoundMigrated(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("round", args)

	// Validate 1 or 2 arguments
	if err := v.Range(1, 2); err != nil {
		return nil, err
	}

	// Get required number
	num, err := v.GetNumber(0)
	if err != nil {
		return nil, err
	}

	// Get optional precision (default 0)
	precision, err := getIntOrDefault(v, 1, 0)
	if err != nil {
		return nil, err
	}

	// Business logic
	if precision == 0 {
		return core.NumberValue(math.Round(num)), nil
	}

	// Round to specified decimal places
	multiplier := math.Pow(10, float64(precision))
	return core.NumberValue(math.Round(num*multiplier) / multiplier), nil
}

// getIntOrDefault is a helper since it's not in the validation package yet
func getIntOrDefault(v *validation.Args, index int, defaultVal int) (int, error) {
	if index >= v.Count() {
		return defaultVal, nil
	}
	return v.GetInt(index)
}

// EXAMPLE 3: Variadic function (min)
// Shows how to handle variable arguments

// MinMigrated demonstrates variadic argument handling
func MinMigrated(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("min", args)

	// Need at least one argument
	if err := v.Min(1); err != nil {
		return nil, err
	}

	// Handle single iterable argument
	if v.Count() == 1 {
		// Check if it's an iterable
		if iter, err := v.GetIterable(0); err == nil {
			// Convert iterable to args and recurse
			items, err := iterableToSlice(iter)
			if err != nil {
				return nil, errors.Wrap(err, errors.ValueError, "min")
			}
			if len(items) == 0 {
				return nil, errors.NewValueError("min", "min() arg is an empty sequence")
			}
			return MinMigrated(items, ctx)
		}
		// If not iterable, fall through to process as single number
	}

	// Extract all numbers
	numbers, err := v.ExtractNumbers()
	if err != nil {
		return nil, err
	}

	// Find minimum
	min := numbers[0]
	for _, n := range numbers[1:] {
		if n < min {
			min = n
		}
	}

	return core.NumberValue(min), nil
}

// EXAMPLE 4: Type checking function (isinstance)
// Shows complex type validation

// IsInstanceMigrated demonstrates type checking with proper errors
func IsInstanceMigrated(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("isinstance", args)

	// Exactly 2 arguments
	if err := v.Exact(2); err != nil {
		return nil, err
	}

	// First argument can be anything
	_ = v.Get(0) // obj - would be used in real implementation

	// Second argument must be a type or tuple of types
	_ = v.Get(1) // typeArg - would be used in real implementation

	// For now, just demonstrate the validation pattern
	// Actual type checking would use the real type system

	// This is a simplified example - real implementation would check actual types
	return core.True, nil
}

// EXAMPLE 5: String function with method (startswith)
// Shows string-specific validation and multiple allowed types

// StartsWithMigrated demonstrates string method implementation
func StartsWithMigrated(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("startswith", args)

	// 1-3 arguments (string, prefix, optional start, optional end)
	if err := v.Range(1, 3); err != nil {
		return nil, err
	}

	// Get the string
	str, err := v.GetString(0)
	if err != nil {
		return nil, err
	}

	// Get prefix (can be string or tuple of strings)
	prefix := v.Get(1)
	var prefixes []string

	switch p := prefix.(type) {
	case core.StringValue:
		prefixes = []string{string(p)}
	case core.TupleValue:
		// Extract all strings from tuple
		for i, item := range p {
			if s, ok := item.(core.StringValue); ok {
				prefixes = append(prefixes, string(s))
			} else {
				return nil, errors.NewTypeErrorf("startswith",
					"tuple must contain only strings, item %d is %s", i, string(item.Type()))
			}
		}
	default:
		return nil, errors.NewTypeError("startswith",
			"string or tuple of strings", string(prefix.Type()))
	}

	// Get optional start position
	start, err := getIntOrDefault(v, 2, 0)
	if err != nil {
		return nil, err
	}

	// Validate start position
	if start < 0 || start > len(str) {
		return nil, errors.NewValueError("startswith", "start position out of range")
	}

	// Check prefixes
	substr := str[start:]
	for _, p := range prefixes {
		if len(substr) >= len(p) && substr[:len(p)] == p {
			return core.True, nil
		}
	}

	return core.False, nil
}

// Helper functions

func iterableToSlice(val core.Value) ([]core.Value, error) {
	// This would be implemented to convert various iterable types to a slice
	// For now, just handle basic cases
	switch v := val.(type) {
	case core.ListValue:
		return []core.Value(v), nil
	case core.TupleValue:
		return []core.Value(v), nil
	default:
		return nil, errors.NewTypeError("min", "iterable", string(val.Type()))
	}
}

// Note: In real implementation, this would use the actual type checking system

// MIGRATION GUIDE:
//
// 1. Replace manual argument count checking:
//    OLD: if len(args) != 1 { return nil, fmt.Errorf(...) }
//    NEW: if err := v.Exact(1); err != nil { return nil, err }
//
// 2. Replace type extraction and validation:
//    OLD: num, ok := args[0].(core.NumberValue)
//         if !ok { return nil, fmt.Errorf(...) }
//    NEW: num, err := v.GetNumber(0)
//         if err != nil { return nil, err }
//
// 3. Use proper error types:
//    OLD: fmt.Errorf("type error: expected X got Y")
//    NEW: errors.NewTypeError(function, "expected", "got")
//
// 4. Handle optional arguments cleanly:
//    OLD: var precision int
//         if len(args) > 1 {
//             if p, ok := args[1].(core.NumberValue); ok {
//                 precision = int(p)
//             } else {
//                 return nil, fmt.Errorf(...)
//             }
//         }
//    NEW: precision, err := v.GetIntOrDefault(1, 0)
//         if err != nil { return nil, err }
//
// 5. Extract all arguments of same type:
//    OLD: for i, arg := range args {
//             if num, ok := arg.(core.NumberValue); ok {
//                 numbers = append(numbers, float64(num))
//             } else {
//                 return nil, fmt.Errorf(...)
//             }
//         }
//    NEW: numbers, err := v.ExtractNumbers()
//         if err != nil { return nil, err }
