package m28

import (
	"fmt"
)

func registerEqualityOps() {
	builtinFuncs["eq?"] = eqFunc
	builtinFuncs["equal?"] = equalFunc
}

func eqFunc(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("eq? expects exactly two arguments")
	}
	return args[0] == args[1], nil
}

func equalFunc(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("equal? expects exactly two arguments")
	}
	return EqualValues(args[0], args[1]), nil
}
