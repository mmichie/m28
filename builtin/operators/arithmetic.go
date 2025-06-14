package operators

import (
	"math"

	"github.com/mmichie/m28/common/builders"
	"github.com/mmichie/m28/core"
)

// RegisterArithmetic registers arithmetic operators using the builder framework
func RegisterArithmetic(ctx *core.Context) {
	// + addition operator
	// BEFORE: ~40 lines with type checking for numbers, strings, lists
	// AFTER: Using pre-built Add operator
	ctx.Define("+", core.NewBuiltinFunction(builders.Add()))

	// - subtraction operator
	// BEFORE: ~20 lines
	// AFTER: Using BinaryNumber builder
	ctx.Define("-", core.NewBuiltinFunction(builders.BinaryNumberSimple("-", func(a, b float64) float64 {
		return a - b
	})))

	// * multiplication operator
	// BEFORE: ~30 lines with string/list repetition
	// AFTER: Using pre-built Multiply operator
	ctx.Define("*", core.NewBuiltinFunction(builders.Multiply()))

	// / division operator
	// BEFORE: ~15 lines
	// AFTER: Using builders
	ctx.Define("/", core.NewBuiltinFunction(builders.BinaryNumber("/", func(a, b float64) (float64, error) {
		if b == 0 {
			return 0, &core.ZeroDivisionError{}
		}
		return a / b, nil
	})))

	// % modulo operator
	// BEFORE: ~15 lines
	// AFTER: Using builders
	ctx.Define("%", core.NewBuiltinFunction(builders.BinaryNumber("%", func(a, b float64) (float64, error) {
		if b == 0 {
			return 0, &core.ZeroDivisionError{}
		}
		return math.Mod(a, b), nil
	})))

	// ** power operator
	// BEFORE: ~10 lines
	// AFTER: Using builders
	ctx.Define("**", core.NewBuiltinFunction(builders.BinaryNumberSimple("**", math.Pow)))

	// - unary negation
	// BEFORE: ~12 lines
	// AFTER: Using UnaryNumber
	ctx.Define("unary-", core.NewBuiltinFunction(builders.UnaryNumberSimple("unary-", func(n float64) float64 {
		return -n
	})))
}

// Migration Statistics:
// Functions migrated: 7 arithmetic operators
// Original lines: ~160 lines
// Migrated lines: ~50 lines
// Reduction: ~69% with pre-built operators
