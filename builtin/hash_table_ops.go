package builtin

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

func RegisterHashTableOps() {
	core.RegisterBuiltin("make-hash-table", makeHashTableFunc)
	core.RegisterBuiltin("gethash", gethashFunc)
	core.RegisterBuiltin("sethash", sethashFunc)
	core.RegisterBuiltin("remhash", remhashFunc)
}

func makeHashTableFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	return core.NewPythonicDict(), nil
}

func gethashFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("gethash requires exactly 2 arguments")
	}

	ht, ok := args[1].(*core.PythonicDict)
	if !ok {
		return nil, fmt.Errorf("second argument to gethash must be a hash table")
	}

	value, found := ht.Get(args[0])
	if !found {
		return core.PythonicNone{}, nil
	}
	return value, nil
}

func sethashFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 3 {
		return nil, fmt.Errorf("sethash requires exactly 3 arguments")
	}

	ht, ok := args[2].(*core.PythonicDict)
	if !ok {
		return nil, fmt.Errorf("third argument to sethash must be a hash table")
	}

	ht.Set(args[0], args[1])
	return args[1], nil
}

func remhashFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("remhash requires exactly 2 arguments")
	}

	ht, ok := args[1].(*core.PythonicDict)
	if !ok {
		return nil, fmt.Errorf("second argument to remhash must be a hash table")
	}

	ht.Delete(args[0])
	return core.PythonicBool(true), nil
}
