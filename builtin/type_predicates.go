package builtin

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

func RegisterTypePredicates() {
	core.RegisterBuiltin("listp", listpFunc)
	core.RegisterBuiltin("arrayp", arraypFunc)
	core.RegisterBuiltin("vectorp", vectorpFunc)
	core.RegisterBuiltin("characterp", characterpFunc)
}

func listpFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("listp requires exactly one argument")
	}
	_, ok := args[0].(core.LispList)
	return ok, nil
}

func arraypFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("arrayp requires exactly one argument")
	}
	// In this implementation, we'll consider LispList as the only array type
	_, ok := args[0].(core.LispList)
	return ok, nil
}

func vectorpFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("vectorp requires exactly one argument")
	}
	// In this implementation, we'll consider LispList as the only vector type
	_, ok := args[0].(core.LispList)
	return ok, nil
}

func characterpFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("characterp requires exactly one argument")
	}
	str, ok := args[0].(string)
	return ok && len(str) == 1, nil
}
