package builtin

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

func RegisterSequenceOps() {
	core.RegisterBuiltin("subseq", subseqFunc)
	core.RegisterBuiltin("reverse", reverseFunc)
	core.RegisterBuiltin("remove", removeFunc)
	core.RegisterBuiltin("remove-if", removeIfFunc)
	core.RegisterBuiltin("remove-if-not", removeIfNotFunc)
}

func subseqFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 || len(args) > 3 {
		return nil, fmt.Errorf("subseq requires 2 or 3 arguments")
	}

	sequence, ok := args[0].(core.LispList)
	if !ok {
		if str, ok := args[0].(string); ok {
			return subseqString(str, args[1:])
		}
		return nil, fmt.Errorf("subseq first argument must be a sequence")
	}

	start, ok := args[1].(float64)
	if !ok {
		return nil, fmt.Errorf("subseq start index must be a number")
	}

	end := float64(len(sequence))
	if len(args) == 3 {
		end, ok = args[2].(float64)
		if !ok {
			return nil, fmt.Errorf("subseq end index must be a number")
		}
	}

	if start < 0 || start > float64(len(sequence)) || end < start || end > float64(len(sequence)) {
		return nil, fmt.Errorf("subseq invalid index range")
	}

	return core.LispList(sequence[int(start):int(end)]), nil
}

func subseqString(str string, args []core.LispValue) (core.LispValue, error) {
	start, ok := args[0].(float64)
	if !ok {
		return nil, fmt.Errorf("subseq start index must be a number")
	}

	end := float64(len(str))
	if len(args) == 2 {
		end, ok = args[1].(float64)
		if !ok {
			return nil, fmt.Errorf("subseq end index must be a number")
		}
	}

	if start < 0 || start > float64(len(str)) || end < start || end > float64(len(str)) {
		return nil, fmt.Errorf("subseq invalid index range")
	}

	return str[int(start):int(end)], nil
}

func reverseFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("reverse requires exactly one argument")
	}

	switch seq := args[0].(type) {
	case core.LispList:
		reversed := make(core.LispList, len(seq))
		for i, j := 0, len(seq)-1; i < len(seq); i, j = i+1, j-1 {
			reversed[i] = seq[j]
		}
		return reversed, nil
	case string:
		runes := []rune(seq)
		for i, j := 0, len(runes)-1; i < j; i, j = i+1, j-1 {
			runes[i], runes[j] = runes[j], runes[i]
		}
		return string(runes), nil
	default:
		return nil, fmt.Errorf("reverse requires a sequence (list or string)")
	}
}

func removeFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("remove requires exactly two arguments")
	}

	item := args[0]
	seq, ok := args[1].(core.LispList)
	if !ok {
		return nil, fmt.Errorf("remove second argument must be a list")
	}

	result := make(core.LispList, 0, len(seq))
	for _, elem := range seq {
		if !core.EqualValues(elem, item) {
			result = append(result, elem)
		}
	}

	return result, nil
}

func removeIfFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	return removeIfHelper(args, env, true)
}

func removeIfNotFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	return removeIfHelper(args, env, false)
}

func removeIfHelper(args []core.LispValue, env core.Environment, removeIfTrue bool) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("remove-if/remove-if-not requires exactly two arguments")
	}

	predicate, ok := args[0].(*core.Lambda)
	if !ok {
		return nil, fmt.Errorf("first argument must be a function")
	}

	seq, ok := args[1].(core.LispList)
	if !ok {
		return nil, fmt.Errorf("second argument must be a list")
	}

	result := make(core.LispList, 0, len(seq))
	for _, elem := range seq {
		predicateResult, err := env.(core.Evaluator).Apply(predicate, []core.LispValue{elem}, env)
		if err != nil {
			return nil, err
		}

		if (removeIfTrue && !core.IsTruthy(predicateResult)) || (!removeIfTrue && core.IsTruthy(predicateResult)) {
			result = append(result, elem)
		}
	}

	return result, nil
}
