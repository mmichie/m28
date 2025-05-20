// Package builtin provides built-in functions for the M28 language.
package builtin

import (
	"fmt"
	"strings"
	
	"github.com/mmichie/m28/core"
)

// RegisterAllBuiltins registers all built-in functions in the provided context
func RegisterAllBuiltins(ctx *core.Context) {
	// Register arithmetic functions
	RegisterArithmeticFunctions(ctx)
	
	// Register comparison functions
	RegisterComparisonFunctions(ctx)
	
	// Register string functions
	registerStringBuiltins(ctx)
	
	// Register IO functions
	registerIOBuiltins(ctx)
	
	// Register list functions
	registerListBuiltins(ctx)
	
	// Register dictionary functions
	registerDictBuiltins(ctx)
	
	// Register type functions
	registerTypeBuiltins(ctx)
}

// registerArithmeticBuiltins registers arithmetic functions
func registerArithmeticBuiltins(ctx *core.Context) {
	// Addition: (+ a b c ...)
	ctx.Define("+", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) == 0 {
			return core.NumberValue(0), nil
		}
		
		// Check if we're adding strings (string concatenation)
		if str, ok := args[0].(core.StringValue); ok {
			var sb strings.Builder
			sb.WriteString(string(str))
			
			for _, arg := range args[1:] {
				if str, ok := arg.(core.StringValue); ok {
					sb.WriteString(string(str))
				} else {
					sb.WriteString(core.PrintValueWithoutQuotes(arg))
				}
			}
			
			return core.StringValue(sb.String()), nil
		}
		
		// Numeric addition
		var sum float64
		for _, arg := range args {
			if num, ok := arg.(core.NumberValue); ok {
				sum += float64(num)
			} else {
				return nil, fmt.Errorf("+ expects numbers, got %v", arg.Type())
			}
		}
		
		return core.NumberValue(sum), nil
	}))
	
	// Subtraction: (- a b c ...)
	ctx.Define("-", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) == 0 {
			return nil, fmt.Errorf("- requires at least one argument")
		}
		
		if len(args) == 1 {
			// Unary negation
			if num, ok := args[0].(core.NumberValue); ok {
				return core.NumberValue(-float64(num)), nil
			}
			return nil, fmt.Errorf("- expects a number, got %v", args[0].Type())
		}
		
		// Start with the first number
		var result float64
		if num, ok := args[0].(core.NumberValue); ok {
			result = float64(num)
		} else {
			return nil, fmt.Errorf("- expects numbers, got %v", args[0].Type())
		}
		
		// Subtract the rest
		for _, arg := range args[1:] {
			if num, ok := arg.(core.NumberValue); ok {
				result -= float64(num)
			} else {
				return nil, fmt.Errorf("- expects numbers, got %v", arg.Type())
			}
		}
		
		return core.NumberValue(result), nil
	}))
	
	// Multiplication: (* a b c ...)
	ctx.Define("*", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) == 0 {
			return core.NumberValue(1), nil
		}
		
		// Start with 1
		var product float64 = 1
		
		// Multiply by each argument
		for _, arg := range args {
			if num, ok := arg.(core.NumberValue); ok {
				product *= float64(num)
			} else {
				return nil, fmt.Errorf("* expects numbers, got %v", arg.Type())
			}
		}
		
		return core.NumberValue(product), nil
	}))
	
	// Division: (/ a b c ...)
	ctx.Define("/", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) == 0 {
			return nil, fmt.Errorf("/ requires at least one argument")
		}
		
		if len(args) == 1 {
			// Reciprocal
			if num, ok := args[0].(core.NumberValue); ok {
				if float64(num) == 0 {
					return nil, fmt.Errorf("division by zero")
				}
				return core.NumberValue(1 / float64(num)), nil
			}
			return nil, fmt.Errorf("/ expects a number, got %v", args[0].Type())
		}
		
		// Start with the first number
		var result float64
		if num, ok := args[0].(core.NumberValue); ok {
			result = float64(num)
		} else {
			return nil, fmt.Errorf("/ expects numbers, got %v", args[0].Type())
		}
		
		// Divide by the rest
		for _, arg := range args[1:] {
			if num, ok := arg.(core.NumberValue); ok {
				if float64(num) == 0 {
					return nil, fmt.Errorf("division by zero")
				}
				result /= float64(num)
			} else {
				return nil, fmt.Errorf("/ expects numbers, got %v", arg.Type())
			}
		}
		
		return core.NumberValue(result), nil
	}))
}

// registerStringBuiltins registers string manipulation functions
func registerStringBuiltins(ctx *core.Context) {
	// String length: (str-len str)
	ctx.Define("str-len", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("str-len requires 1 argument")
		}
		
		if str, ok := args[0].(core.StringValue); ok {
			return core.NumberValue(len(str)), nil
		}
		
		return nil, fmt.Errorf("str-len expects a string, got %v", args[0].Type())
	}))
	
	// String concatenation: (str-concat a b c ...)
	ctx.Define("str-concat", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		var sb strings.Builder
		
		for _, arg := range args {
			sb.WriteString(core.PrintValueWithoutQuotes(arg))
		}
		
		return core.StringValue(sb.String()), nil
	}))
}

// registerIOBuiltins registers IO functions
func registerIOBuiltins(ctx *core.Context) {
	// Print: (print a b c ...)
	ctx.Define("print", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		for i, arg := range args {
			if i > 0 {
				fmt.Print(" ")
			}
			fmt.Print(core.PrintValueWithoutQuotes(arg))
		}
		fmt.Println()
		
		return core.Nil, nil
	}))
}

// registerListBuiltins registers list manipulation functions
func registerListBuiltins(ctx *core.Context) {
	// List length: (len lst)
	ctx.Define("len", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("len requires 1 argument")
		}
		
		switch v := args[0].(type) {
		case core.ListValue:
			return core.NumberValue(len(v)), nil
		case core.TupleValue:
			return core.NumberValue(len(v)), nil
		case core.StringValue:
			return core.NumberValue(len(v)), nil
		case *core.DictValue:
			return core.NumberValue(v.Size()), nil
		default:
			return nil, fmt.Errorf("len expects a collection, got %v", args[0].Type())
		}
	}))
	
	// nth: (nth lst idx)
	ctx.Define("nth", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, fmt.Errorf("nth requires 2 arguments")
		}
		
		// Check second argument is an index (number)
		var idx int
		if num, ok := args[1].(core.NumberValue); ok {
			idx = int(num)
		} else {
			return nil, fmt.Errorf("nth index must be a number")
		}
		
		// Access by index based on type
		switch v := args[0].(type) {
		case core.ListValue:
			if idx < 0 || idx >= len(v) {
				return nil, fmt.Errorf("index out of bounds: %d", idx)
			}
			return v[idx], nil
		case core.TupleValue:
			if idx < 0 || idx >= len(v) {
				return nil, fmt.Errorf("index out of bounds: %d", idx)
			}
			return v[idx], nil
		case core.StringValue:
			if idx < 0 || idx >= len(v) {
				return nil, fmt.Errorf("index out of bounds: %d", idx)
			}
			return core.StringValue(string(v)[idx : idx+1]), nil
		default:
			return nil, fmt.Errorf("nth expects a sequence, got %v", args[0].Type())
		}
	}))
}

// registerDictBuiltins registers dictionary manipulation functions
func registerDictBuiltins(ctx *core.Context) {
	// Get: (get dict key [default])
	ctx.Define("get", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 || len(args) > 3 {
			return nil, fmt.Errorf("get requires 2 or 3 arguments")
		}
		
		dict, ok := args[0].(*core.DictValue)
		if !ok {
			return nil, fmt.Errorf("get expects a dictionary as first argument")
		}
		
		key, ok := args[1].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("get expects a string key as second argument")
		}
		
		// Try to get the value
		if val, ok := dict.Get(string(key)); ok {
			return val, nil
		}
		
		// Return default if provided
		if len(args) > 2 {
			return args[2], nil
		}
		
		// Otherwise return nil
		return core.Nil, nil
	}))
}

// registerTypeBuiltins registers type-related functions
func registerTypeBuiltins(ctx *core.Context) {
	// Type: (type val)
	ctx.Define("type", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("type requires 1 argument")
		}
		
		typeName := args[0].Type()
		return core.StringValue(string(typeName)), nil
	}))
}