package special_forms

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

func EvalSetf(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("setf requires exactly 2 arguments")
	}

	place := args[0]
	value, err := e.Eval(args[1], env)
	if err != nil {
		return nil, err
	}

	switch p := place.(type) {
	case core.LispSymbol:
		// Simple variable assignment
		if !env.SetMutable(p, value) {
			return nil, fmt.Errorf("cannot setf undefined variable: %s", p)
		}
		return value, nil

	case core.LispList:
		// Handle structure updates
		if len(p) < 2 {
			return nil, fmt.Errorf("invalid setf place: %v", p)
		}
		accessor, ok := p[0].(core.LispSymbol)
		if !ok {
			return nil, fmt.Errorf("invalid setf accessor: %v", p[0])
		}

		// Remove references to undefined functions
		return nil, fmt.Errorf("setf for %s not implemented", accessor)

	default:
		return nil, fmt.Errorf("invalid setf place: %v", place)
	}
}
