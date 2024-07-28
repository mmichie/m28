package builtin

import (
	"fmt"
	"reflect"

	"github.com/mmichie/m28/core"
)

func RegisterTypeBuiltins() {
	core.RegisterBuiltin("callable", callableFunc)
	core.RegisterBuiltin("isinstance", isinstanceFunc)
	core.RegisterBuiltin("issubclass", issubclassFunc)
	core.RegisterBuiltin("type", typeFunc)
}

func callableFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("callable() takes exactly one argument")
	}
	_, ok := args[0].(*core.Lambda)
	return core.PythonicBool(ok), nil
}

func isinstanceFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	// Note: This is a placeholder. Implementing isinstance would require additional work on type system.
	return nil, fmt.Errorf("isinstance() is not implemented in this Lisp interpreter")
}

func issubclassFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	// Note: This is a placeholder. Implementing issubclass would require additional work on type system.
	return nil, fmt.Errorf("issubclass() is not implemented in this Lisp interpreter")
}

func typeFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("type() takes exactly one argument")
	}
	return reflect.TypeOf(args[0]).String(), nil
}
