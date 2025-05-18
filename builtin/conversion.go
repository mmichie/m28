package builtin

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

func init() {
	core.RegisterBuiltin("bin", binFunc)
	core.RegisterBuiltin("bool", boolFunc)
	core.RegisterBuiltin("complex", complexFunc)
	core.RegisterBuiltin("hex", hexFunc)
	core.RegisterBuiltin("oct", octFunc)
	core.RegisterBuiltin("tuple", tupleFunc)
}

// tupleFunc creates a tuple from its arguments
// Usage: (tuple arg1 arg2 ...) or (tuple collection)
func tupleFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	// If no arguments, return an empty tuple
	if len(args) == 0 {
		return core.LispTuple{}, nil
	}

	// Single argument might be a collection we need to convert
	if len(args) == 1 {
		switch arg := args[0].(type) {
		case core.LispList:
			// Convert list to tuple
			tuple := make(core.LispTuple, len(arg))
			for i, item := range arg {
				tuple[i] = item
			}
			return tuple, nil
		case core.LispListLiteral:
			// Convert list literal to tuple
			tuple := make(core.LispTuple, len(arg))
			for i, item := range arg {
				tuple[i] = item
			}
			return tuple, nil
		case core.LispTuple:
			// If already a tuple, just return a copy
			tuple := make(core.LispTuple, len(arg))
			copy(tuple, arg)
			return tuple, nil
		case string:
			// Convert string characters to tuple
			tuple := make(core.LispTuple, len(arg))
			for i, char := range arg {
				tuple[i] = string(char)
			}
			return tuple, nil
		}
	}

	// Multiple arguments - create a tuple with the arguments as elements
	result := make(core.LispTuple, len(args))
	for i, arg := range args {
		result[i] = arg
	}
	return result, nil
}

func binFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("bin() takes exactly one argument")
	}
	switch v := args[0].(type) {
	case int:
		return fmt.Sprintf("0b%b", v), nil
	case float64:
		return fmt.Sprintf("0b%b", int(v)), nil
	default:
		return nil, fmt.Errorf("bin() argument must be an integer")
	}
}

func boolFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("bool() takes exactly one argument")
	}
	return core.PythonicBool(core.IsTruthy(args[0])), nil
}

func complexFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("complex() takes exactly two arguments")
	}
	real, ok1 := args[0].(float64)
	imag, ok2 := args[1].(float64)
	if !ok1 || !ok2 {
		return nil, fmt.Errorf("complex() arguments must be numbers")
	}
	return complex(real, imag), nil
}

func hexFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("hex() takes exactly one argument")
	}
	switch v := args[0].(type) {
	case float64:
		return fmt.Sprintf("0x%x", int(v)), nil
	case int:
		return fmt.Sprintf("0x%x", v), nil
	default:
		return nil, fmt.Errorf("hex() argument must be an integer")
	}
}

func octFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("oct() takes exactly one argument")
	}
	switch v := args[0].(type) {
	case float64:
		return fmt.Sprintf("0o%o", int(v)), nil
	case int:
		return fmt.Sprintf("0o%o", v), nil
	default:
		return nil, fmt.Errorf("oct() argument must be an integer")
	}
}
