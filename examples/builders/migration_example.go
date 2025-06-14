package builders_test

import (
	"errors"
	"fmt"
	"math"

	"github.com/mmichie/m28/common/builders"
	"github.com/mmichie/m28/core"
)

// This file demonstrates migrating real M28 builtin functions to use builders

// ============================================================================
// BEFORE: Original sqrt function from modules/math.go (12 lines)
// ============================================================================
func sqrtBefore(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("sqrt() takes exactly one argument (%d given)", len(args))
	}

	switch v := args[0].(type) {
	case core.NumberValue:
		return core.NumberValue(math.Sqrt(float64(v))), nil
	default:
		return nil, fmt.Errorf("sqrt() argument must be a number, not '%s'", v.Type())
	}
}

// ============================================================================
// AFTER: Using builders (1 line!)
// ============================================================================
var sqrtAfter = builders.UnaryNumberSimple("sqrt", math.Sqrt)

// ============================================================================
// BEFORE: Original pow function from modules/math.go (13 lines)
// ============================================================================
func powBefore(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("pow() takes exactly two arguments (%d given)", len(args))
	}

	x, ok1 := args[0].(core.NumberValue)
	y, ok2 := args[1].(core.NumberValue)
	if !ok1 || !ok2 {
		return nil, fmt.Errorf("pow() arguments must be numbers")
	}

	return core.NumberValue(math.Pow(float64(x), float64(y))), nil
}

// ============================================================================
// AFTER: Using builders (1 line!)
// ============================================================================
var powAfter = builders.BinaryNumberSimple("pow", math.Pow)

// ============================================================================
// BEFORE: Original sin function from modules/math.go (12 lines)
// ============================================================================
func sinBefore(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("sin() takes exactly one argument (%d given)", len(args))
	}

	switch v := args[0].(type) {
	case core.NumberValue:
		return core.NumberValue(math.Sin(float64(v))), nil
	default:
		return nil, fmt.Errorf("sin() argument must be a number, not '%s'", v.Type())
	}
}

// ============================================================================
// AFTER: Using builders (1 line!)
// ============================================================================
var sinAfter = builders.UnaryNumberSimple("sin", math.Sin)

// ============================================================================
// MIGRATING THE ENTIRE MATH MODULE
// ============================================================================

// Before: 100+ lines of boilerplate for basic math functions
// After: Clean, declarative registration
func RegisterMathModuleWithBuilders(ctx *core.Context) {
	mathModule := core.NewDict()

	// Trigonometric functions - each was 12 lines, now 1 line
	mathModule.Set("sin", core.NewBuiltinFunction(builders.UnaryNumberSimple("sin", math.Sin)))
	mathModule.Set("cos", core.NewBuiltinFunction(builders.UnaryNumberSimple("cos", math.Cos)))
	mathModule.Set("tan", core.NewBuiltinFunction(builders.UnaryNumberSimple("tan", math.Tan)))
	mathModule.Set("asin", core.NewBuiltinFunction(builders.UnaryNumberSimple("asin", math.Asin)))
	mathModule.Set("acos", core.NewBuiltinFunction(builders.UnaryNumberSimple("acos", math.Acos)))
	mathModule.Set("atan", core.NewBuiltinFunction(builders.UnaryNumberSimple("atan", math.Atan)))

	// Power and logarithmic functions
	mathModule.Set("sqrt", core.NewBuiltinFunction(builders.UnaryNumberSimple("sqrt", math.Sqrt)))
	mathModule.Set("pow", core.NewBuiltinFunction(builders.BinaryNumberSimple("pow", math.Pow)))
	mathModule.Set("exp", core.NewBuiltinFunction(builders.UnaryNumberSimple("exp", math.Exp)))
	mathModule.Set("log", core.NewBuiltinFunction(builders.UnaryNumberSimple("log", math.Log)))
	mathModule.Set("log10", core.NewBuiltinFunction(builders.UnaryNumberSimple("log10", math.Log10)))

	// Rounding functions
	mathModule.Set("ceil", core.NewBuiltinFunction(builders.UnaryNumberSimple("ceil", math.Ceil)))
	mathModule.Set("floor", core.NewBuiltinFunction(builders.UnaryNumberSimple("floor", math.Floor)))
	mathModule.Set("round", core.NewBuiltinFunction(builders.UnaryNumberSimple("round", math.Round)))
	mathModule.Set("trunc", core.NewBuiltinFunction(builders.UnaryNumberSimple("trunc", math.Trunc)))

	// Other math functions
	mathModule.Set("abs", core.NewBuiltinFunction(builders.UnaryNumberSimple("abs", math.Abs)))
	mathModule.Set("atan2", core.NewBuiltinFunction(builders.BinaryNumberSimple("atan2", math.Atan2)))

	// Constants
	mathModule.Set("pi", core.NumberValue(math.Pi))
	mathModule.Set("e", core.NumberValue(math.E))

	// Register the module
	registry := core.GetModuleRegistry()
	registry.StoreModule("math", mathModule, "<builtin>", []string{})
}

// ============================================================================
// MORE COMPLEX EXAMPLE: sqrt with domain validation
// ============================================================================

// Before: Would require ~20 lines with manual validation
// After: Clean separation of validation logic (7 lines)
var sqrtWithValidation = builders.UnaryNumber("sqrt", func(n float64) (float64, error) {
	if n < 0 {
		return 0, errors.New("math domain error")
	}
	return math.Sqrt(n), nil
})

// ============================================================================
// EXAMPLE: Custom max function with variadic args
// ============================================================================

// Before: 40+ lines with manual argument checking
// After: Just the algorithm (7 lines)
var maxFunc = builders.VariadicNumber("max", 1, func(nums []float64) (float64, error) {
	max := nums[0]
	for _, n := range nums[1:] {
		if n > max {
			max = n
		}
	}
	return max, nil
})

// ============================================================================
// STATISTICS: Migration Impact
// ============================================================================
//
// Original math module functions:
// - sqrt: 12 lines → 1 line (92% reduction)
// - pow: 13 lines → 1 line (92% reduction)
// - sin/cos/tan: 12 lines each → 1 line each (92% reduction)
//
// Total for 15 basic math functions:
// - Before: ~180 lines of code
// - After: 15 lines of code
// - Reduction: 165 lines (92%)
//
// Additional benefits:
// - Consistent error messages across all functions
// - Type safety guaranteed at compile time
// - Business logic clearly separated from validation
// - Easy to add new functions
// - Automatic support for future enhancements
