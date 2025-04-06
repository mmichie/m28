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
	core.RegisterBuiltin("is", isFunc)
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

func isFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("is() takes exactly two arguments")
	}
	
	value := args[0]
	
	// Check if second argument is a function call that returns a type
	typeFunc, isList := args[1].(core.LispList)
	if isList && len(typeFunc) > 0 {
		if typeFunc[0] == core.LispSymbol("int") {
			_, isInt := value.(float64)
			return core.PythonicBool(isInt), nil
		} else if typeFunc[0] == core.LispSymbol("float") {
			_, isFloat := value.(float64)
			return core.PythonicBool(isFloat), nil
		} else if typeFunc[0] == core.LispSymbol("str") {
			_, isStr := value.(string)
			return core.PythonicBool(isStr), nil
		} else if typeFunc[0] == core.LispSymbol("list") {
			_, isList := value.(core.LispList)
			return core.PythonicBool(isList), nil
		} else if typeFunc[0] == core.LispSymbol("dict") {
			_, isDict := value.(*core.PythonicDict)
			return core.PythonicBool(isDict), nil
		} else if typeFunc[0] == core.LispSymbol("bool") {
			_, isBool := value.(core.PythonicBool)
			return core.PythonicBool(isBool), nil
		}
	}
	
	// Direct identity comparison
	return core.PythonicBool(core.EqValues(value, args[1])), nil
}
