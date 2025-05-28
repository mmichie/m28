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
	RegisterStringFunctions(ctx)

	// Don't call registerIOBuiltins as RegisterIOFunctions handles it

	// Register list functions
	RegisterListFunctions(ctx)

	// Register dictionary functions
	RegisterDictFunctions(ctx)

	// Register type functions
	registerTypeBuiltins(ctx)
	
	// Register type checking functions (isinstance, issubclass, type conversions)
	registerTypeCheckingBuiltins(ctx)
	
	// Register mathematical functions
	RegisterMathFunctions(ctx)
	
	// Register utility functions
	RegisterUtilityFunctions(ctx)
	
	// Register async/concurrent functions
	RegisterAsyncBuiltins(ctx)
	
	// Register I/O functions
	RegisterIOFunctions(ctx)
	
	// Register assertion functions
	RegisterAssertBuiltins(ctx)
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

// Note: String functions have been moved to string.go
// and are registered via RegisterStringFunctions

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

// List functions are now in list.go

// Dictionary functions are now in dict.go

// registerTypeBuiltins registers type-related functions
func registerTypeBuiltins(ctx *core.Context) {
	// Type: (type val) - returns type object
	ctx.Define("type", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("type requires 1 argument")
		}

		// Get type descriptor
		desc := core.GetTypeDescriptorForValue(args[0])
		if desc == nil {
			// Fallback for types without descriptors
			typeName := args[0].Type()
			return core.StringValue(fmt.Sprintf("<type '%s'>", string(typeName))), nil
		}

		// Return a string representation of the type
		return core.StringValue(fmt.Sprintf("<type '%s'>", desc.PythonName)), nil
	}))

	// Python-style type checking functions
	// is_none - check if value is None/nil
	ctx.Define("is_none", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("is_none expects 1 argument, got %d", len(args))
		}
		_, isNil := args[0].(core.NilValue)
		return core.BoolValue(isNil), nil
	}))

	// NOTE: isinstance, issubclass, int, float, str, bool are now in type_checking.go

	// dir - list attributes of an object
	ctx.Define("dir", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("dir expects 1 argument, got %d", len(args))
		}

		desc := core.GetTypeDescriptorForValue(args[0])
		if desc == nil {
			// Return empty list for types without descriptors
			return core.EmptyList, nil
		}

		// Get all attribute names
		names := desc.GetAttributeNames()
		result := make(core.ListValue, len(names))
		for i, name := range names {
			result[i] = core.StringValue(name)
		}

		return result, nil
	}))

	// hasattr - check if object has attribute
	ctx.Define("hasattr", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, fmt.Errorf("hasattr expects 2 arguments, got %d", len(args))
		}

		attrName, ok := args[1].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("hasattr attribute name must be a string")
		}

		// Try to get the attribute
		if obj, ok := args[0].(interface {
			GetAttr(string) (core.Value, bool)
		}); ok {
			_, found := obj.GetAttr(string(attrName))
			return core.BoolValue(found), nil
		}

		return core.False, nil
	}))

	// getattr - get attribute from object
	ctx.Define("getattr", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 || len(args) > 3 {
			return nil, fmt.Errorf("getattr expects 2 or 3 arguments, got %d", len(args))
		}

		attrName, ok := args[1].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("getattr attribute name must be a string")
		}

		// Try to get the attribute
		if obj, ok := args[0].(interface {
			GetAttr(string) (core.Value, bool)
		}); ok {
			val, found := obj.GetAttr(string(attrName))
			if found {
				return val, nil
			}
		}

		// Return default if provided
		if len(args) == 3 {
			return args[2], nil
		}

		// Otherwise, attribute error
		desc := core.GetTypeDescriptorForValue(args[0])
		typeName := "object"
		if desc != nil {
			typeName = desc.PythonName
		}
		return nil, fmt.Errorf("'%s' object has no attribute '%s'", typeName, string(attrName))
	}))

	// Collection constructors
	// list - create a new list (overrides the one from RegisterListFunctions)
	ctx.Define("list", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) == 0 {
			return core.EmptyList, nil
		}
		if len(args) == 1 {
			// Python-style: Convert iterable to list
			switch v := args[0].(type) {
			case core.ListValue:
				// Copy the list
				result := make(core.ListValue, len(v))
				copy(result, v)
				return result, nil
			case core.TupleValue:
				// Convert tuple to list
				result := make(core.ListValue, len(v))
				copy(result, v)
				return result, nil
			case core.StringValue:
				// Convert string to list of characters
				str := string(v)
				result := make(core.ListValue, len(str))
				for i, ch := range str {
					result[i] = core.StringValue(string(ch))
				}
				return result, nil
			default:
				// If it's not an iterable, just create a list with this one element
				return core.ListValue{v}, nil
			}
		}
		// Multiple arguments: create a list from all arguments
		return core.ListValue(args), nil
	}))

	// dict - create a new dictionary
	ctx.Define("dict", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) == 0 {
			return core.NewDict(), nil
		}
		// TODO: Add support for dict(key=value) syntax later
		return nil, fmt.Errorf("dict() with arguments not yet implemented")
	}))

	// tuple - create a new tuple
	ctx.Define("tuple", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) == 0 {
			return core.EmptyTuple, nil
		}
		if len(args) == 1 {
			// Convert iterable to tuple
			switch v := args[0].(type) {
			case core.TupleValue:
				// Copy the tuple
				result := make(core.TupleValue, len(v))
				copy(result, v)
				return result, nil
			case core.ListValue:
				// Convert list to tuple
				result := make(core.TupleValue, len(v))
				copy(result, v)
				return result, nil
			case core.StringValue:
				// Convert string to tuple of characters
				str := string(v)
				result := make(core.TupleValue, len(str))
				for i, ch := range str {
					result[i] = core.StringValue(string(ch))
				}
				return result, nil
			default:
				return nil, fmt.Errorf("tuple() argument must be an iterable, not '%s'", v.Type())
			}
		}
		return nil, fmt.Errorf("tuple() takes at most 1 argument (%d given)", len(args))
	}))

	// len - get length of collection
	ctx.Define("len", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("len() takes exactly one argument (%d given)", len(args))
		}

		// Try to get __len__ method
		if obj, ok := args[0].(interface {
			GetAttr(string) (core.Value, bool)
		}); ok {
			if lenMethod, found := obj.GetAttr("__len__"); found {
				// Call the __len__ method
				if callable, ok := lenMethod.(interface {
					Call([]core.Value, *core.Context) (core.Value, error)
				}); ok {
					result, err := callable.Call([]core.Value{}, ctx)
					if err != nil {
						return nil, err
					}
					// Ensure it returns a number
					if num, ok := result.(core.NumberValue); ok {
						return num, nil
					}
					return nil, fmt.Errorf("__len__ should return an integer")
				}
			}
		}

		// Fallback for types without __len__
		switch v := args[0].(type) {
		case core.StringValue:
			return core.NumberValue(len(string(v))), nil
		case core.ListValue:
			return core.NumberValue(len(v)), nil
		case core.TupleValue:
			return core.NumberValue(len(v)), nil
		case *core.DictValue:
			return core.NumberValue(v.Size()), nil
		case *core.SetValue:
			return core.NumberValue(v.Size()), nil
		default:
			return nil, fmt.Errorf("object of type '%s' has no len()", v.Type())
		}
	}))

	// set - create a new set
	ctx.Define("set", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) == 0 {
			return core.NewSet(), nil
		}
		if len(args) == 1 {
			// Convert iterable to set
			set := core.NewSet()
			switch v := args[0].(type) {
			case core.ListValue:
				for _, elem := range v {
					set.Add(elem)
				}
				return set, nil
			case core.TupleValue:
				for _, elem := range v {
					set.Add(elem)
				}
				return set, nil
			case core.StringValue:
				// Convert string to set of characters
				str := string(v)
				for _, ch := range str {
					charVal := core.StringValue(string(ch))
					set.Add(charVal)
				}
				return set, nil
			default:
				return nil, fmt.Errorf("set() argument must be an iterable, not '%s'", v.Type())
			}
		}
		return nil, fmt.Errorf("set() takes at most 1 argument (%d given)", len(args))
	}))
}
