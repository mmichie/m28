package eval

import (
	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/parser"
	"os"
)

// EvalString parses and evaluates a string in the given context
func EvalString(input string, ctx *core.Context) (core.Value, error) {
	// Create a new parser
	p := parser.NewParser()

	// Parse the input
	expr, err := p.Parse(input)
	if err != nil {
		return nil, err
	}

	// Evaluate the parsed expression
	return Eval(expr, ctx)
}

// ReadFile reads the content of a file and returns it as a string
func ReadFile(filename string) (string, error) {
	content, err := os.ReadFile(filename)
	if err != nil {
		return "", err
	}
	return string(content), nil
}

// Already defined in evaluator.go

// RegisterAllBuiltins registers all built-in functions
func RegisterAllBuiltins(ctx *core.Context) {
	// Register basic arithmetic operations
	RegisterBuiltin(ctx, "+", AddFunction)
	RegisterBuiltin(ctx, "-", SubtractFunction)
	RegisterBuiltin(ctx, "*", MultiplyFunction)
	RegisterBuiltin(ctx, "/", DivideFunction)

	// Register comparison operations
	RegisterBuiltin(ctx, "==", EqualFunction)
	RegisterBuiltin(ctx, "<", LessThanFunction)
	RegisterBuiltin(ctx, ">", GreaterThanFunction)
	RegisterBuiltin(ctx, "<=", LessEqualFunction)
	RegisterBuiltin(ctx, ">=", GreaterEqualFunction)

	// Register list operations
	RegisterBuiltin(ctx, "list", ListFunction)
	RegisterBuiltin(ctx, "first", FirstFunction)
	RegisterBuiltin(ctx, "rest", RestFunction)
	RegisterBuiltin(ctx, "cons", ConsFunction)

	// Register IO operations
	RegisterBuiltin(ctx, "print", PrintFunction)

	// Register type operations
	RegisterBuiltin(ctx, "type", TypeFunction)
}

// RegisterBuiltin registers a built-in function in the given context
func RegisterBuiltin(ctx *core.Context, name string, fn func([]core.Value, *core.Context) (core.Value, error)) {
	ctx.Define(name, core.NewBuiltinFunction(fn))
}

// Built-in functions

// AddFunction implements the + function
func AddFunction(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) == 0 {
		return core.NumberValue(0), nil
	}

	// Check if first arg is a number or string (for concatenation)
	switch first := args[0].(type) {
	case core.NumberValue:
		// Numeric addition
		sum := float64(first)
		for _, arg := range args[1:] {
			if num, ok := arg.(core.NumberValue); ok {
				sum += float64(num)
			} else {
				return nil, ErrType("number", arg)
			}
		}
		return core.NumberValue(sum), nil

	case core.StringValue:
		// String concatenation
		result := string(first)
		for _, arg := range args[1:] {
			if str, ok := arg.(core.StringValue); ok {
				result += string(str)
			} else {
				result += arg.String()
			}
		}
		return core.StringValue(result), nil

	default:
		return nil, ErrType("number or string", args[0])
	}
}

// SubtractFunction implements the - function
func SubtractFunction(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) == 0 {
		return nil, ErrArgCount("- requires at least one argument")
	}

	if len(args) == 1 {
		// Unary negation
		if num, ok := args[0].(core.NumberValue); ok {
			return core.NumberValue(-float64(num)), nil
		}
		return nil, ErrType("number", args[0])
	}

	// Binary/variadic subtraction
	first, ok := args[0].(core.NumberValue)
	if !ok {
		return nil, ErrType("number", args[0])
	}

	result := float64(first)
	for _, arg := range args[1:] {
		if num, ok := arg.(core.NumberValue); ok {
			result -= float64(num)
		} else {
			return nil, ErrType("number", arg)
		}
	}

	return core.NumberValue(result), nil
}

// MultiplyFunction implements the * function
func MultiplyFunction(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) == 0 {
		return core.NumberValue(1), nil
	}

	result := 1.0
	for _, arg := range args {
		if num, ok := arg.(core.NumberValue); ok {
			result *= float64(num)
		} else {
			return nil, ErrType("number", arg)
		}
	}

	return core.NumberValue(result), nil
}

// DivideFunction implements the / function
func DivideFunction(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) == 0 {
		return nil, ErrArgCount("/ requires at least one argument")
	}

	if len(args) == 1 {
		// Unary division (reciprocal)
		if num, ok := args[0].(core.NumberValue); ok {
			if float64(num) == 0 {
				return nil, ErrDivByZero()
			}
			return core.NumberValue(1.0 / float64(num)), nil
		}
		return nil, ErrType("number", args[0])
	}

	// Binary/variadic division
	first, ok := args[0].(core.NumberValue)
	if !ok {
		return nil, ErrType("number", args[0])
	}

	result := float64(first)
	for _, arg := range args[1:] {
		if num, ok := arg.(core.NumberValue); ok {
			if float64(num) == 0 {
				return nil, ErrDivByZero()
			}
			result /= float64(num)
		} else {
			return nil, ErrType("number", arg)
		}
	}

	return core.NumberValue(result), nil
}

// EqualFunction implements the = function
func EqualFunction(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 2 {
		return nil, ErrArgCount("= requires at least two arguments")
	}

	for i := 1; i < len(args); i++ {
		if !core.EqualValues(args[0], args[i]) {
			return core.False, nil
		}
	}

	return core.True, nil
}

// LessThanFunction implements the < function
func LessThanFunction(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 2 {
		return nil, ErrArgCount("< requires at least two arguments")
	}

	for i := 1; i < len(args); i++ {
		if core.Compare(args[i-1], args[i]) >= 0 {
			return core.False, nil
		}
	}

	return core.True, nil
}

// GreaterThanFunction implements the > function
func GreaterThanFunction(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 2 {
		return nil, ErrArgCount("> requires at least two arguments")
	}

	for i := 1; i < len(args); i++ {
		if core.Compare(args[i-1], args[i]) <= 0 {
			return core.False, nil
		}
	}

	return core.True, nil
}

// LessEqualFunction implements the <= function
func LessEqualFunction(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 2 {
		return nil, ErrArgCount("<= requires at least two arguments")
	}

	for i := 1; i < len(args); i++ {
		if core.Compare(args[i-1], args[i]) > 0 {
			return core.False, nil
		}
	}

	return core.True, nil
}

// GreaterEqualFunction implements the >= function
func GreaterEqualFunction(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 2 {
		return nil, ErrArgCount(">= requires at least two arguments")
	}

	for i := 1; i < len(args); i++ {
		if core.Compare(args[i-1], args[i]) < 0 {
			return core.False, nil
		}
	}

	return core.True, nil
}

// ListFunction implements the list function
func ListFunction(args []core.Value, ctx *core.Context) (core.Value, error) {
	return core.ListValue(args), nil
}

// FirstFunction implements the first function (head of a list)
func FirstFunction(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, ErrArgCount("first requires exactly one argument")
	}

	switch list := args[0].(type) {
	case core.ListValue:
		if len(list) == 0 {
			return core.Nil, nil
		}
		return list[0], nil
	case core.TupleValue:
		if len(list) == 0 {
			return core.Nil, nil
		}
		return list[0], nil
	default:
		return nil, ErrType("list or tuple", args[0])
	}
}

// RestFunction implements the rest function (tail of a list)
func RestFunction(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, ErrArgCount("rest requires exactly one argument")
	}

	switch list := args[0].(type) {
	case core.ListValue:
		if len(list) <= 1 {
			return core.EmptyList, nil
		}
		return list[1:], nil
	case core.TupleValue:
		if len(list) <= 1 {
			return core.EmptyTuple, nil
		}
		return list[1:], nil
	default:
		return nil, ErrType("list or tuple", args[0])
	}
}

// ConsFunction implements the cons function (prepend an element to a list)
func ConsFunction(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, ErrArgCount("cons requires exactly two arguments")
	}

	switch list := args[1].(type) {
	case core.ListValue:
		return core.ListValue(append([]core.Value{args[0]}, list...)), nil
	case core.TupleValue:
		return core.TupleValue(append([]core.Value{args[0]}, list...)), nil
	default:
		return nil, ErrType("list or tuple", args[1])
	}
}

// PrintFunction implements the print function
func PrintFunction(args []core.Value, ctx *core.Context) (core.Value, error) {
	for i, arg := range args {
		if i > 0 {
			print(" ")
		}
		print(core.PrintValueWithoutQuotes(arg))
	}
	println()
	return core.Nil, nil
}

// TypeFunction implements the type function
func TypeFunction(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, ErrArgCount("type requires exactly one argument")
	}

	return core.StringValue(string(args[0].Type())), nil
}

// Helper functions for creating common errors

// ErrArgCount creates an error for incorrect argument count
func ErrArgCount(msg string) error {
	return ArgumentError{msg}
}

// ErrType creates an error for incorrect argument type
func ErrType(expected string, got core.Value) error {
	return TypeError{
		Expected: core.Type(expected),
		Got:      got.Type(),
	}
}

// ErrDivByZero creates a division by zero error
func ErrDivByZero() error {
	return ArithmeticError{"division by zero"}
}

// Error types

// ArgumentError represents an error with function arguments
type ArgumentError struct {
	Message string
}

func (e ArgumentError) Error() string {
	return e.Message
}

// TypeError represents a type error
type TypeError struct {
	Expected core.Type
	Got      core.Type
}

func (e TypeError) Error() string {
	return "expected " + string(e.Expected) + ", got " + string(e.Got)
}

// ArithmeticError represents an arithmetic error
type ArithmeticError struct {
	Message string
}

func (e ArithmeticError) Error() string {
	return "arithmetic error: " + e.Message
}
