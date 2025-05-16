package core

// ObjAdapter is a helper struct that makes it easy to implement the ObjProtocol
// This can be embedded in existing types or used as an adapter
type ObjAdapter struct {
	// Property access functions
	GetPropFn func(name string) (LispValue, bool)
	SetPropFn func(name string, value LispValue) error

	// Method access functions
	HasMethodPFn  func(name string) bool
	CallMethodPFn func(name string, args []LispValue, eval Evaluator, env Environment) (LispValue, error)
}

// Implementation of the ObjProtocol interface for ObjAdapter
func (a *ObjAdapter) GetProp(name string) (LispValue, bool) {
	if a.GetPropFn != nil {
		return a.GetPropFn(name)
	}
	return nil, false
}

func (a *ObjAdapter) SetProp(name string, value LispValue) error {
	if a.SetPropFn != nil {
		return a.SetPropFn(name, value)
	}
	return ErrDotNoPropertyf(name)
}

func (a *ObjAdapter) HasMethodP(name string) bool {
	if a.HasMethodPFn != nil {
		return a.HasMethodPFn(name)
	}
	return false
}

func (a *ObjAdapter) CallMethodP(name string, args []LispValue, eval Evaluator, env Environment) (LispValue, error) {
	if a.CallMethodPFn != nil {
		return a.CallMethodPFn(name, args, eval, env)
	}
	return nil, ErrDotNoMethodf(name)
}

// EnsureAdapter makes sure a value has an ObjProtocol implementation
// If it already implements ObjProtocol, it returns that
// Otherwise, it creates an ObjAdapter based on the available interfaces
func EnsureAdapter(obj LispValue) ObjProtocol {
	// If it's already an ObjProtocol, return it
	if asObj, ok := obj.(ObjProtocol); ok {
		return asObj
	}

	// If it's adaptable, use its adapter
	if adaptable, ok := obj.(AdaptableLispValue); ok {
		return adaptable.AsObject()
	}

	// Create the appropriate adapter based on the type
	switch typedObj := obj.(type) {
	case *PythonicDict:
		return dictAdapter(typedObj)
	case *PythonicObject:
		return objectAdapter(typedObj)
	case *SuperObject:
		return superObjectAdapter(typedObj)
	case LispList:
		return &LispListAdapter{list: typedObj}
	case LispListLiteral:
		return &LispListAdapter{list: LispList(typedObj)}
	case LispTuple:
		return &LispTupleAdapter{tuple: typedObj}
	case string:
		return &StringAdapter{str: typedObj}
	case *PythonicSet:
		return &PythonicSetAdapter{set: typedObj}
	case DotAccessible:
		return dotAccessibleAdapter(typedObj)
	}

	// Default adapter with minimal functionality
	return &ObjAdapter{}
}

// Adapter for PythonicDict
func dictAdapter(dict *PythonicDict) ObjProtocol {
	return &ObjAdapter{
		GetPropFn: func(name string) (LispValue, bool) {
			// Check for method
			if dict.HasMethod(name) {
				method, _ := dict.methods[name]
				// We wrap it in a BuiltinFunc for later calling
				return BuiltinFunc(func(args []LispValue, callEnv Environment) (LispValue, error) {
					return method(dict, args)
				}), true
			}

			// Check for property/attribute
			return dict.Get(name)
		},

		SetPropFn: func(name string, value LispValue) error {
			dict.Set(name, value)
			return nil
		},

		HasMethodPFn: func(name string) bool {
			return dict.HasMethod(name)
		},

		CallMethodPFn: func(name string, args []LispValue, eval Evaluator, env Environment) (LispValue, error) {
			return dict.CallMethod(name, args)
		},
	}
}

// Adapter for PythonicObject
func objectAdapter(obj *PythonicObject) ObjProtocol {
	return &ObjAdapter{
		GetPropFn: func(name string) (LispValue, bool) {
			return obj.GetAttribute(name)
		},

		SetPropFn: func(name string, value LispValue) error {
			return obj.SetProperty(name, value)
		},

		HasMethodPFn: func(name string) bool {
			return obj.HasMethod(name)
		},

		CallMethodPFn: func(name string, args []LispValue, eval Evaluator, env Environment) (LispValue, error) {
			obj.SetEvaluator(eval)
			return obj.CallMethod(name, args)
		},
	}
}

// Adapter for DotAccessible
func dotAccessibleAdapter(obj DotAccessible) ObjProtocol {
	return &ObjAdapter{
		GetPropFn: func(name string) (LispValue, bool) {
			// Check for property
			if obj.HasProperty(name) {
				return obj.GetProperty(name)
			}

			// Check for method
			if obj.HasMethod(name) {
				return BuiltinFunc(func(args []LispValue, callEnv Environment) (LispValue, error) {
					return obj.CallMethod(name, args)
				}), true
			}

			return nil, false
		},

		SetPropFn: func(name string, value LispValue) error {
			return obj.SetProperty(name, value)
		},

		HasMethodPFn: func(name string) bool {
			return obj.HasMethod(name)
		},

		CallMethodPFn: func(name string, args []LispValue, eval Evaluator, env Environment) (LispValue, error) {
			return obj.CallMethod(name, args)
		},
	}
}

// Adapter for SuperObject
func superObjectAdapter(obj *SuperObject) ObjProtocol {
	return &ObjAdapter{
		GetPropFn: func(name string) (LispValue, bool) {
			// Check parent class methods directly to create proper bound methods
			for _, parent := range obj.Object.Class.Parents {
				if method, exists := parent.GetMethod(name); exists {
					// Create a bound method with the original instance
					boundMethod := NewBoundMethod(method, obj.Object, obj.Object.GetEvaluator())
					return boundMethod, true
				}
			}

			// Check parent class attributes
			for _, parent := range obj.Object.Class.Parents {
				if attr, exists := parent.GetAttribute(name); exists {
					return attr, true
				}
			}

			return nil, false
		},

		SetPropFn: func(name string, value LispValue) error {
			return obj.SetProperty(name, value)
		},

		HasMethodPFn: func(name string) bool {
			return obj.HasMethod(name)
		},

		CallMethodPFn: func(name string, args []LispValue, eval Evaluator, env Environment) (LispValue, error) {
			// Store evaluator in the super object
			obj.SetEvaluator(eval)

			// Check parent class methods specifically to create bound methods with proper 'self'
			for _, parent := range obj.Object.Class.Parents {
				if method, exists := parent.GetMethod(name); exists {
					// Create a bound method with the original instance
					boundMethod := NewBoundMethod(method, obj.Object, eval)
					// Apply with the evaluator passed to ensure context is maintained
					return boundMethod.Apply(eval, args, env)
				}
			}

			return nil, ErrDotNoMethodf(name)
		},
	}
}
