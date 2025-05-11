package core

// ModuleAdapter implements the ObjProtocol for module types (PythonicDict)
// This allows modules to be used with dot notation in a more consistent way
type ModuleAdapter struct {
	Module *PythonicDict
}

// NewModuleAdapter creates a new adapter for a module
func NewModuleAdapter(module *PythonicDict) *ModuleAdapter {
	return &ModuleAdapter{Module: module}
}

// GetProp implements the ObjProtocol.GetProp method
func (ma *ModuleAdapter) GetProp(name string) (LispValue, bool) {
	// Modules are dictionaries, so we can just get the property directly
	if value, exists := ma.Module.Get(name); exists {
		return value, true
	}
	return nil, false
}

// SetProp implements the ObjProtocol.SetProp method
func (ma *ModuleAdapter) SetProp(name string, value LispValue) error {
	// Modules are dictionaries, so we can just set the property directly
	ma.Module.Set(name, value)
	return nil
}

// HasMethodP implements the ObjProtocol.HasMethodP method
func (ma *ModuleAdapter) HasMethodP(name string) bool {
	// First check if it's a direct method on the dict
	if ma.Module.HasMethod(name) {
		return true
	}

	// Then check if it's a callable property
	if value, exists := ma.Module.Get(name); exists {
		switch value.(type) {
		case Applicable, BuiltinFunc, *Lambda:
			return true
		}
	}

	return false
}

// CallMethodP implements the ObjProtocol.CallMethodP method
func (ma *ModuleAdapter) CallMethodP(name string, args []LispValue, eval Evaluator, env Environment) (LispValue, error) {
	// First try to call it as a dict method
	if ma.Module.HasMethod(name) {
		return ma.Module.CallMethod(name, args)
	}

	// Otherwise, try to get and call the property as a function
	if value, exists := ma.Module.Get(name); exists {
		switch fn := value.(type) {
		case Applicable:
			return fn.Apply(eval, args, env)
		case BuiltinFunc:
			return fn(args, env)
		case *Lambda:
			return eval.Apply(fn, args, env)
		}
	}

	return nil, ErrDotNoMethodf(name)
}

// Extend PythonicDict to implement AdaptableLispValue
func (d *PythonicDict) AsObject() ObjProtocol {
	// Return module adapter for enhanced module behavior
	if _, ok := d.Get("__name__"); ok {
		// If it has a __name__ property, it's likely a module
		return NewModuleAdapter(d)
	}
	// Otherwise use standard dict adapter
	return dictAdapter(d)
}

// Make sure PythonicDict implements AdaptableLispValue interface
var _ AdaptableLispValue = (*PythonicDict)(nil)
