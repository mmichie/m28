package core

// This file implements ObjProtocol adapters for LispTuple type

// LispTupleAdapter adapts LispTuple to ObjProtocol
type LispTupleAdapter struct {
	tuple LispTuple
}

// NewLispTupleAdapter creates a new adapter for LispTuple
func NewLispTupleAdapter(tuple LispTuple) *LispTupleAdapter {
	return &LispTupleAdapter{tuple: tuple}
}

// GetProp retrieves a property or method
func (a *LispTupleAdapter) GetProp(name string) (LispValue, bool) {
	// Methods
	switch name {
	case "length", "len":
		return BuiltinFunc(func(args []LispValue, env Environment) (LispValue, error) {
			return float64(len(a.tuple)), nil
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
			if intIdx < 0 || intIdx >= len(a.tuple) {
				return nil, ErrIndexOutOfRange(intIdx, len(a.tuple))
			}

			return a.tuple[intIdx], nil
		}), true
	case "tolist":
		return BuiltinFunc(func(args []LispValue, env Environment) (LispValue, error) {
			if len(args) != 0 {
				return nil, ErrWrongArgCount("tolist", 0, len(args))
			}

			// Create a list from the tuple elements
			result := make(LispList, len(a.tuple))
			for i, item := range a.tuple {
				result[i] = item
			}

			return result, nil
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

			result := make(LispList, len(a.tuple))
			for i, item := range a.tuple {
				mapped, err := eval.Apply(fn, []LispValue{item}, env)
				if err != nil {
					return nil, err
				}
				result[i] = mapped
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
			strItems := make([]string, len(a.tuple))
			for i, item := range a.tuple {
				strItems[i] = PrintValue(item)
			}

			return JoinStrings(strItems, sep), nil
		}), true
	}

	// Item access by index for numeric property names
	if idx, err := ParseIndex(name); err == nil && idx >= 0 && idx < len(a.tuple) {
		return a.tuple[idx], true
	}

	return nil, false
}

// SetProp sets a property value
func (a *LispTupleAdapter) SetProp(name string, value LispValue) error {
	// Tuples are immutable, so we can't set properties
	return ErrDotNoPropertyf(name)
}

// HasMethodP checks if a method exists
func (a *LispTupleAdapter) HasMethodP(name string) bool {
	switch name {
	case "length", "len", "get", "__getitem__", "tolist", "map", "join":
		return true
	}
	return false
}

// CallMethodP calls a method with arguments
func (a *LispTupleAdapter) CallMethodP(name string, args []LispValue, eval Evaluator, env Environment) (LispValue, error) {
	switch name {
	case "length", "len":
		if len(args) != 0 {
			return nil, ErrWrongArgCount("len", 0, len(args))
		}
		return float64(len(a.tuple)), nil
	case "get", "__getitem__":
		if len(args) != 1 {
			return nil, ErrWrongArgCount("get", 1, len(args))
		}

		idx, ok := args[0].(float64)
		if !ok {
			return nil, ErrTypeMismatch("number", TypeOf(args[0]))
		}

		intIdx := int(idx)
		if intIdx < 0 || intIdx >= len(a.tuple) {
			return nil, ErrIndexOutOfRange(intIdx, len(a.tuple))
		}

		return a.tuple[intIdx], nil
	case "tolist":
		if len(args) != 0 {
			return nil, ErrWrongArgCount("tolist", 0, len(args))
		}

		// Create a list from the tuple elements
		result := make(LispList, len(a.tuple))
		for i, item := range a.tuple {
			result[i] = item
		}

		return result, nil
	case "map":
		if len(args) != 1 {
			return nil, ErrWrongArgCount("map", 1, len(args))
		}

		fn, ok := args[0].(Applicable)
		if !ok {
			return nil, ErrTypeMismatch("function", TypeOf(args[0]))
		}

		result := make(LispList, len(a.tuple))
		for i, item := range a.tuple {
			mapped, err := eval.Apply(fn, []LispValue{item}, env)
			if err != nil {
				return nil, err
			}
			result[i] = mapped
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
		strItems := make([]string, len(a.tuple))
		for i, item := range a.tuple {
			strItems[i] = PrintValue(item)
		}

		return JoinStrings(strItems, sep), nil
	}

	return nil, ErrDotNoMethodf(name)
}

// Ensure LispTupleAdapter implements ObjProtocol
var _ ObjProtocol = (*LispTupleAdapter)(nil)

// Make LispTuple implement AdaptableLispValue
func (tuple LispTuple) AsObject() ObjProtocol {
	return &LispTupleAdapter{tuple: tuple}
}

// Ensure LispTuple implements AdaptableLispValue
var _ AdaptableLispValue = (LispTuple)(nil)
