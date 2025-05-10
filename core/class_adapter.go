package core

import "fmt"

// ClassAdapter implements the ObjProtocol for PythonicClass types
type ClassAdapter struct {
	Class *PythonicClass
}

// NewClassAdapter creates a new adapter for a PythonicClass
func NewClassAdapter(class *PythonicClass) *ClassAdapter {
	return &ClassAdapter{Class: class}
}

// GetProp implements the ObjProtocol.GetProp method
func (ca *ClassAdapter) GetProp(name string) (LispValue, bool) {
	// Class attributes
	return ca.Class.GetAttribute(name)
}

// SetProp implements the ObjProtocol.SetProp method
func (ca *ClassAdapter) SetProp(name string, value LispValue) error {
	ca.Class.AddAttribute(name, value)
	return nil
}

// HasMethodP implements the ObjProtocol.HasMethodP method
func (ca *ClassAdapter) HasMethodP(name string) bool {
	_, exists := ca.Class.GetMethod(name)
	return exists
}

// CallMethodP implements the ObjProtocol.CallMethodP method
func (ca *ClassAdapter) CallMethodP(name string, args []LispValue, eval Evaluator, env Environment) (LispValue, error) {
	method, exists := ca.Class.GetMethod(name)
	if !exists {
		return nil, fmt.Errorf("method '%s' not found on class %s", name, ca.Class.Name)
	}
	
	// Call the class method (static method)
	return eval.Apply(method, args, env)
}

// ObjectAdapter implements the ObjProtocol for PythonicObject types
type ObjectAdapter struct {
	Object *PythonicObject
}

// NewObjectAdapter creates a new adapter for a PythonicObject
func NewObjectAdapter(obj *PythonicObject) *ObjectAdapter {
	return &ObjectAdapter{Object: obj}
}

// GetProp implements the ObjProtocol.GetProp method
func (oa *ObjectAdapter) GetProp(name string) (LispValue, bool) {
	// Special case for Attributes field directly
	if name == "Attributes" {
		return oa.Object.Attributes, true
	}

	// First check instance attributes
	if oa.Object.Attributes != nil {
		if val, exists := oa.Object.Attributes.Get(name); exists {
			return val, true
		}
	}

	// Then check class attributes
	if oa.Object.Class != nil {
		return oa.Object.Class.GetAttribute(name)
	}

	return nil, false
}

// SetProp implements the ObjProtocol.SetProp method
func (oa *ObjectAdapter) SetProp(name string, value LispValue) error {
	// Special case for setting Attributes field directly
	if name == "Attributes" {
		if dict, ok := value.(*PythonicDict); ok {
			oa.Object.Attributes = dict
			return nil
		}
		return fmt.Errorf("Attributes must be a dictionary")
	}

	oa.Object.Attributes.Set(name, value)
	return nil
}

// HasMethodP implements the ObjProtocol.HasMethodP method
func (oa *ObjectAdapter) HasMethodP(name string) bool {
	return oa.Object.HasMethod(name)
}

// CallMethodP implements the ObjProtocol.CallMethodP method
func (oa *ObjectAdapter) CallMethodP(name string, args []LispValue, eval Evaluator, env Environment) (LispValue, error) {
	// Update the evaluator reference
	oa.Object.SetEvaluator(eval)
	
	return oa.Object.CallMethod(name, args)
}

// Extend PythonicClass to implement AdaptableLispValue
func (c *PythonicClass) AsObject() ObjProtocol {
	return NewClassAdapter(c)
}

// Extend PythonicObject to implement AdaptableLispValue
func (o *PythonicObject) AsObject() ObjProtocol {
	return NewObjectAdapter(o)
}

// Make sure PythonicClass and PythonicObject implement AdaptableLispValue interface
var _ AdaptableLispValue = (*PythonicClass)(nil)
var _ AdaptableLispValue = (*PythonicObject)(nil)