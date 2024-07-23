package builtin

import (
	"fmt"
	"sort"

	"github.com/mmichie/m28/core"
)

func RegisterSequenceOps() {
	core.RegisterBuiltin("subseq", subseqFunc)
	core.RegisterBuiltin("reverse", reverseFunc)
	core.RegisterBuiltin("remove", removeFunc)
	core.RegisterBuiltin("remove-if", removeIfFunc)
	core.RegisterBuiltin("remove-if-not", removeIfNotFunc)
	core.RegisterBuiltin("reduce", reduceFunc)
	core.RegisterBuiltin("count", countFunc)
	core.RegisterBuiltin("count-if", countIfFunc)
	core.RegisterBuiltin("remove-duplicates", removeDuplicatesFunc)
	core.RegisterBuiltin("substitute", substituteFunc)
	core.RegisterBuiltin("sort", sortFunc)
	core.RegisterBuiltin("stable-sort", stableSortFunc)
	core.RegisterBuiltin("position", positionFunc)
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

func reduceFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 || len(args) > 3 {
		return nil, fmt.Errorf("reduce requires 2 or 3 arguments")
	}

	fn, ok := args[0].(*core.Lambda)
	if !ok {
		return nil, fmt.Errorf("first argument to reduce must be a function")
	}

	sequence, ok := args[1].(core.LispList)
	if !ok {
		return nil, fmt.Errorf("second argument to reduce must be a sequence")
	}

	var result core.LispValue
	if len(args) == 3 {
		result = args[2]
	} else if len(sequence) > 0 {
		result = sequence[0]
		sequence = sequence[1:]
	} else {
		return nil, fmt.Errorf("reduce on empty sequence with no initial value")
	}

	e, err := getEvaluator()
	if err != nil {
		return nil, err
	}

	for _, item := range sequence {
		result, err = e.Apply(fn, []core.LispValue{result, item}, env)
		if err != nil {
			return nil, err
		}
	}

	return result, nil
}

func countFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("count requires exactly 2 arguments")
	}

	item := args[0]
	sequence, ok := args[1].(core.LispList)
	if !ok {
		return nil, fmt.Errorf("second argument to count must be a sequence")
	}

	count := 0
	for _, elem := range sequence {
		if core.EqualValues(elem, item) {
			count++
		}
	}

	return float64(count), nil
}

func countIfFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("count-if requires exactly 2 arguments")
	}

	predicate, ok := args[0].(*core.Lambda)
	if !ok {
		return nil, fmt.Errorf("first argument to count-if must be a function")
	}

	sequence, ok := args[1].(core.LispList)
	if !ok {
		return nil, fmt.Errorf("second argument to count-if must be a sequence")
	}

	count := 0
	e, err := getEvaluator()
	if err != nil {
		return nil, err
	}

	for _, item := range sequence {
		result, err := e.Apply(predicate, []core.LispValue{item}, env)
		if err != nil {
			return nil, err
		}
		if core.IsTruthy(result) {
			count++
		}
	}

	return float64(count), nil
}

func removeDuplicatesFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("remove-duplicates requires exactly 1 argument")
	}

	sequence, ok := args[0].(core.LispList)
	if !ok {
		return nil, fmt.Errorf("argument to remove-duplicates must be a sequence")
	}

	seen := make(map[core.LispValue]bool)
	result := make(core.LispList, 0, len(sequence))

	for _, item := range sequence {
		if !seen[item] {
			seen[item] = true
			result = append(result, item)
		}
	}

	return result, nil
}

func substituteFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 3 {
		return nil, fmt.Errorf("substitute requires exactly 3 arguments")
	}

	newItem := args[0]
	oldItem := args[1]
	sequence, ok := args[2].(core.LispList)
	if !ok {
		return nil, fmt.Errorf("third argument to substitute must be a sequence")
	}

	result := make(core.LispList, len(sequence))
	for i, item := range sequence {
		if core.EqualValues(item, oldItem) {
			result[i] = newItem
		} else {
			result[i] = item
		}
	}

	return result, nil
}

func sortFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	return sortHelper(args, env, false)
}

func stableSortFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	return sortHelper(args, env, true)
}

func sortHelper(args []core.LispValue, env core.Environment, stable bool) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("sort/stable-sort requires exactly 2 arguments")
	}

	sequence, ok := args[0].(core.LispList)
	if !ok {
		return nil, fmt.Errorf("first argument to sort/stable-sort must be a sequence")
	}

	predicate, ok := args[1].(*core.Lambda)
	if !ok {
		return nil, fmt.Errorf("second argument to sort/stable-sort must be a function")
	}

	e, err := getEvaluator()
	if err != nil {
		return nil, err
	}

	result := make(core.LispList, len(sequence))
	copy(result, sequence)

	less := func(i, j int) bool {
		compareResult, err := e.Apply(predicate, []core.LispValue{result[i], result[j]}, env)
		if err != nil {
			return false
		}
		return core.IsTruthy(compareResult)
	}

	if stable {
		sort.SliceStable(result, less)
	} else {
		sort.Slice(result, less)
	}

	return result, nil
}

func positionFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 2 && len(args) != 3 {
		return nil, fmt.Errorf("position requires 2 or 3 arguments")
	}

	item := args[0]
	sequence, ok := args[1].(core.LispList)
	if !ok {
		return nil, fmt.Errorf("second argument to position must be a sequence")
	}

	testFn := core.EqualValues
	if len(args) == 3 {
		fn, ok := args[2].(*core.Lambda)
		if !ok {
			return nil, fmt.Errorf("third argument to position must be a function")
		}
		testFn = func(a, b core.LispValue) bool {
			result, err := env.(core.Evaluator).Apply(fn, []core.LispValue{a, b}, env)
			if err != nil {
				return false
			}
			return core.IsTruthy(result)
		}
	}

	for i, elem := range sequence {
		if testFn(item, elem) {
			return float64(i), nil
		}
	}

	return nil, nil
}
