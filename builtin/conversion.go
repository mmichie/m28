package builtin

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

func RegisterConversionBuiltins() {
	core.RegisterBuiltin("bin", binFunc)
	core.RegisterBuiltin("bool", boolFunc)
	core.RegisterBuiltin("complex", complexFunc)
	core.RegisterBuiltin("hex", hexFunc)
	core.RegisterBuiltin("oct", octFunc)
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
