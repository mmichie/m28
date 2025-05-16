package core

import "fmt"

// This file implements ObjProtocol adapters for LispList and LispListLiteral types

// LispListAdapter adapts LispList to ObjProtocol
type LispListAdapter struct {
	list LispList
}

// NewLispListAdapter creates a new adapter for LispList
func NewLispListAdapter(list LispList) *LispListAdapter {
	return &LispListAdapter{list: list}
}

// GetProp retrieves a property or method
func (a *LispListAdapter) GetProp(name string) (LispValue, bool) {
	// Methods
	switch name {
	case "length", "len":
		return BuiltinFunc(func(args []LispValue, env Environment) (LispValue, error) {
			return float64(len(a.list)), nil
		}), true
	case "append":
		return BuiltinFunc(func(args []LispValue, env Environment) (LispValue, error) {
			if len(args) != 1 {
				return nil, ErrWrongArgCount("append", 1, len(args))
			}
			return append(a.list, args[0]), nil
		}), true
	case "extend":
		return BuiltinFunc(func(args []LispValue, env Environment) (LispValue, error) {
			if len(args) != 1 {
				return nil, ErrWrongArgCount("extend", 1, len(args))
			}

			switch items := args[0].(type) {
			case LispList:
				return append(a.list, items...), nil
			case LispListLiteral:
				return append(a.list, LispList(items)...), nil
			case LispTuple:
				return append(a.list, LispList(items)...), nil
			default:
				return nil, ErrTypeMismatch("list-like", TypeOf(items))
			}
		}), true
	case "get", "__getitem__":
		return BuiltinFunc(func(args []LispValue, env Environment) (LispValue, error) {
			if len(args) != 1 {
				return nil, ErrWrongArgCount("get", 1, len(args))
			}

			idx, ok := args[0].(float64)
			if !ok {
				return nil, ErrTypeMismatch("number", TypeOf(args[0]))
			}

			intIdx := int(idx)
			if intIdx < 0 || intIdx >= len(a.list) {
				return nil, ErrIndexOutOfRange(intIdx, len(a.list))
			}

			return a.list[intIdx], nil
		}), true
	case "map":
		return BuiltinFunc(func(args []LispValue, env Environment) (LispValue, error) {
			if len(args) != 1 {
				return nil, ErrWrongArgCount("map", 1, len(args))
			}

			fn, ok := args[0].(Applicable)
			if !ok {
				return nil, ErrTypeMismatch("function", TypeOf(args[0]))
			}

			// Get evaluator from environment
			evalVal, exists := env.Get("EVALUATOR")
			if !exists {
				return nil, ErrDotEvaluatorMissingf("map")
			}

			eval, ok := evalVal.(Evaluator)
			if !ok {
				return nil, ErrTypeMismatch("evaluator", TypeOf(evalVal))
			}

			result := make(LispList, len(a.list))
			for i, item := range a.list {
				mapped, err := eval.Apply(fn, []LispValue{item}, env)
				if err != nil {
					return nil, err
				}
				result[i] = mapped
			}

			return result, nil
		}), true
	case "filter":
		return BuiltinFunc(func(args []LispValue, env Environment) (LispValue, error) {
			if len(args) != 1 {
				return nil, ErrWrongArgCount("filter", 1, len(args))
			}

			fn, ok := args[0].(Applicable)
			if !ok {
				return nil, ErrTypeMismatch("function", TypeOf(args[0]))
			}

			// Get evaluator from environment
			evalVal, exists := env.Get("EVALUATOR")
			if !exists {
				return nil, ErrDotEvaluatorMissingf("filter")
			}

			eval, ok := evalVal.(Evaluator)
			if !ok {
				return nil, ErrTypeMismatch("evaluator", TypeOf(evalVal))
			}

			result := make(LispList, 0)
			for _, item := range a.list {
				keep, err := eval.Apply(fn, []LispValue{item}, env)
				if err != nil {
					return nil, err
				}

				if IsTruthy(keep) {
					result = append(result, item)
				}
			}

			return result, nil
		}), true
	case "join":
		return BuiltinFunc(func(args []LispValue, env Environment) (LispValue, error) {
			if len(args) != 1 {
				return nil, ErrWrongArgCount("join", 1, len(args))
			}

			sep, ok := args[0].(string)
			if !ok {
				return nil, ErrTypeMismatch("string", TypeOf(args[0]))
			}

			// Convert all items to strings
			strItems := make([]string, len(a.list))
			for i, item := range a.list {
				strItems[i] = PrintValue(item)
			}

			return JoinStrings(strItems, sep), nil
		}), true
	}

	// Item access by index for numeric property names
	if idx, err := ParseIndex(name); err == nil && idx >= 0 && idx < len(a.list) {
		return a.list[idx], true
	}

	return nil, false
}

// SetProp sets a property value
func (a *LispListAdapter) SetProp(name string, value LispValue) error {
	// Set item by index for numeric property names
	if idx, err := ParseIndex(name); err == nil {
		if idx < 0 || idx >= len(a.list) {
			return ErrIndexOutOfRange(idx, len(a.list))
		}
		a.list[idx] = value
		return nil
	}

	return ErrDotNoPropertyf(name)
}

// HasMethodP checks if a method exists
func (a *LispListAdapter) HasMethodP(name string) bool {
	switch name {
	case "length", "len", "append", "extend", "get", "__getitem__", "map", "filter", "join":
		return true
	}
	return false
}

// CallMethodP calls a method with arguments
func (a *LispListAdapter) CallMethodP(name string, args []LispValue, eval Evaluator, env Environment) (LispValue, error) {
	switch name {
	case "length", "len":
		if len(args) != 0 {
			return nil, ErrWrongArgCount("len", 0, len(args))
		}
		return float64(len(a.list)), nil
	case "append":
		if len(args) != 1 {
			return nil, ErrWrongArgCount("append", 1, len(args))
		}
		return append(a.list, args[0]), nil
	case "extend":
		if len(args) != 1 {
			return nil, ErrWrongArgCount("extend", 1, len(args))
		}

		switch items := args[0].(type) {
		case LispList:
			return append(a.list, items...), nil
		case LispListLiteral:
			return append(a.list, LispList(items)...), nil
		case LispTuple:
			return append(a.list, LispList(items)...), nil
		default:
			return nil, ErrTypeMismatch("list-like", TypeOf(items))
		}
	case "get", "__getitem__":
		if len(args) != 1 {
			return nil, ErrWrongArgCount("get", 1, len(args))
		}

		idx, ok := args[0].(float64)
		if !ok {
			return nil, ErrTypeMismatch("number", TypeOf(args[0]))
		}

		intIdx := int(idx)
		if intIdx < 0 || intIdx >= len(a.list) {
			return nil, ErrIndexOutOfRange(intIdx, len(a.list))
		}

		return a.list[intIdx], nil
	case "map":
		if len(args) != 1 {
			return nil, ErrWrongArgCount("map", 1, len(args))
		}

		fn, ok := args[0].(Applicable)
		if !ok {
			return nil, ErrTypeMismatch("function", TypeOf(args[0]))
		}

		result := make(LispList, len(a.list))
		for i, item := range a.list {
			mapped, err := eval.Apply(fn, []LispValue{item}, env)
			if err != nil {
				return nil, err
			}
			result[i] = mapped
		}

		return result, nil
	case "filter":
		if len(args) != 1 {
			return nil, ErrWrongArgCount("filter", 1, len(args))
		}

		fn, ok := args[0].(Applicable)
		if !ok {
			return nil, ErrTypeMismatch("function", TypeOf(args[0]))
		}

		result := make(LispList, 0)
		for _, item := range a.list {
			keep, err := eval.Apply(fn, []LispValue{item}, env)
			if err != nil {
				return nil, err
			}

			if IsTruthy(keep) {
				result = append(result, item)
			}
		}

		return result, nil
	case "join":
		if len(args) != 1 {
			return nil, ErrWrongArgCount("join", 1, len(args))
		}

		sep, ok := args[0].(string)
		if !ok {
			return nil, ErrTypeMismatch("string", TypeOf(args[0]))
		}

		// Convert all items to strings
		strItems := make([]string, len(a.list))
		for i, item := range a.list {
			strItems[i] = PrintValue(item)
		}

		return JoinStrings(strItems, sep), nil
	}

	return nil, ErrDotNoMethodf(name)
}

// Ensure LispListAdapter implements ObjProtocol
var _ ObjProtocol = (*LispListAdapter)(nil)

// Make LispList implement AdaptableLispValue
func (list LispList) AsObject() ObjProtocol {
	return &LispListAdapter{list: list}
}

// Ensure LispList implements AdaptableLispValue
var _ AdaptableLispValue = (LispList)(nil)

// Make LispListLiteral implement AdaptableLispValue
func (list LispListLiteral) AsObject() ObjProtocol {
	return &LispListAdapter{list: LispList(list)}
}

// Ensure LispListLiteral implements AdaptableLispValue
var _ AdaptableLispValue = (LispListLiteral)(nil)

// Helper function to parse an index from a string
func ParseIndex(s string) (int, error) {
	var idx float64
	_, err := fmt.Sscanf(s, "%f", &idx)
	if err != nil {
		return -1, err
	}
	return int(idx), nil
}
