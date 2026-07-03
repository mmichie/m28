package core

import "fmt"

// This file implements allocation-free descriptor dispatch for the built-in
// descriptor kinds. The generic protocol probes ("does this value's GetAttr
// expose __set__?") materialize a closure per probe — three allocations per
// attribute access just to answer "no" for plain methods. Instead, the
// concrete descriptor types (property, slot, classmethod, staticmethod) are
// type-switched and executed directly; only user-defined descriptor classes
// (instances) still go through the duck-typed protocol, probed cheaply via
// their class namespace before any binding happens.

// dataDescriptorGet resolves the data-descriptor stage of instance attribute
// lookup for a value found in the class namespace. handled reports that the
// value is a data descriptor and the access is fully resolved (value or
// error). Plain methods, functions and constants return handled=false with
// zero allocations.
func (i *Instance) dataDescriptorGet(classAttr Value) (Value, bool, error) {
	switch d := classAttr.(type) {
	case *PropertyValue:
		// Properties are always data descriptors: a missing setter still
		// intercepts writes ("can't set attribute").
		if d.Getter == nil {
			return nil, true, fmt.Errorf("unreadable attribute")
		}
		callable, ok := d.Getter.(Callable)
		if !ok {
			return nil, true, fmt.Errorf("property getter is not callable")
		}
		v, err := callable.Call([]Value{i}, nil)
		if err != nil {
			return nil, true, err
		}
		return v, true, nil
	case *SlotDescriptor:
		if i.SlotValues == nil || d.SlotIndex >= len(i.SlotValues) {
			return nil, true, fmt.Errorf("slot index out of range")
		}
		v := i.SlotValues[d.SlotIndex]
		if v == nil {
			return nil, true, &AttributeError{ObjType: i.Class.Name, AttrName: d.Name}
		}
		return v, true, nil
	case *Instance:
		// User-defined descriptor. Probe its class namespace for
		// __set__/__delete__ (no binding, no allocation); only a hit pays
		// the bound-call machinery.
		if d.Class == nil {
			return nil, false, nil
		}
		_, _, hasSet := d.Class.GetMethodWithClass("__set__")
		hasDelete := false
		if !hasSet {
			_, _, hasDelete = d.Class.GetMethodWithClass("__delete__")
		}
		if !hasSet && !hasDelete {
			return nil, false, nil
		}
		if getMethod, ok := d.GetAttr("__get__"); ok {
			if callable, ok := getMethod.(Callable); ok {
				v, err := callable.Call([]Value{i, i.Class}, nil)
				if err != nil {
					return nil, true, err
				}
				return v, true, nil
			}
		}
		// Data descriptor without __get__: the descriptor itself is returned.
		return classAttr, true, nil
	}
	return nil, false, nil
}

// bindClassValue resolves the non-data-descriptor stage: static/class-method
// unwrapping, user descriptors' __get__, and binding plain callables as
// methods — without protocol probes for the common cases.
func (i *Instance) bindClassValue(classAttr Value, defClass *Class) (Value, error) {
	switch m := classAttr.(type) {
	case *StaticMethodValue:
		return m.Function, nil
	case *ClassMethodValue:
		return &BoundClassMethod{Class: i.Class, Function: m.Function}, nil
	case *PropertyValue:
		// Normally resolved at the data-descriptor stage; kept for callers
		// that reach here directly.
		v, _, err := i.dataDescriptorGet(m)
		return v, err
	case *SlotDescriptor:
		v, _, err := i.dataDescriptorGet(m)
		return v, err
	case *Instance:
		// User-defined non-data descriptor: honor __get__ when its class
		// defines one; otherwise the stored instance is the attribute.
		if m.Class != nil {
			if _, _, ok := m.Class.GetMethodWithClass("__get__"); ok {
				if getMethod, found := m.GetAttr("__get__"); found {
					if callable, ok := getMethod.(Callable); ok {
						v, err := callable.Call([]Value{i, i.Class}, nil)
						if err != nil {
							return nil, err
						}
						return v, nil
					}
				}
			}
		}
		return m, nil
	default:
		if callable, ok := classAttr.(interface {
			Call([]Value, *Context) (Value, error)
		}); ok {
			return &BoundInstanceMethod{
				Instance:      i,
				Method:        callable,
				DefiningClass: defClass,
			}, nil
		}
		return classAttr, nil
	}
}

// dataDescriptorSet resolves the write path against a class-namespace value.
// handled reports the value is a data descriptor that consumed the write.
func (i *Instance) dataDescriptorSet(classAttr Value, value Value) (bool, error) {
	switch d := classAttr.(type) {
	case *PropertyValue:
		if d.Setter == nil {
			return true, fmt.Errorf("can't set attribute")
		}
		callable, ok := d.Setter.(Callable)
		if !ok {
			return true, fmt.Errorf("property setter is not callable")
		}
		_, err := callable.Call([]Value{i, value}, nil)
		return true, err
	case *SlotDescriptor:
		if i.SlotValues == nil {
			return true, fmt.Errorf("'%s' object has no slots", i.Class.Name)
		}
		if d.SlotIndex >= len(i.SlotValues) {
			return true, fmt.Errorf("slot index out of range (index %d, len %d)", d.SlotIndex, len(i.SlotValues))
		}
		i.SlotValues[d.SlotIndex] = value
		return true, nil
	case *Instance:
		if d.Class == nil {
			return false, nil
		}
		if _, _, ok := d.Class.GetMethodWithClass("__set__"); ok {
			if setMethod, found := d.GetAttr("__set__"); found {
				if callable, ok := setMethod.(Callable); ok {
					_, err := callable.Call([]Value{i, value}, nil)
					return true, err
				}
			}
		}
	}
	return false, nil
}

// dataDescriptorDelete resolves the delete path against a class-namespace
// value. handled reports the value is a data descriptor that consumed the
// deletion.
func (i *Instance) dataDescriptorDelete(classAttr Value) (bool, error) {
	switch d := classAttr.(type) {
	case *PropertyValue:
		if d.Deleter == nil {
			return true, fmt.Errorf("can't delete attribute")
		}
		callable, ok := d.Deleter.(Callable)
		if !ok {
			return true, fmt.Errorf("property deleter is not callable")
		}
		_, err := callable.Call([]Value{i}, nil)
		return true, err
	case *SlotDescriptor:
		if i.SlotValues == nil {
			return true, fmt.Errorf("'%s' object has no slots", i.Class.Name)
		}
		if d.SlotIndex >= len(i.SlotValues) {
			return true, fmt.Errorf("slot index out of range")
		}
		i.SlotValues[d.SlotIndex] = nil
		return true, nil
	case *Instance:
		if d.Class == nil {
			return false, nil
		}
		if _, _, ok := d.Class.GetMethodWithClass("__delete__"); ok {
			if delMethod, found := d.GetAttr("__delete__"); found {
				if callable, ok := delMethod.(Callable); ok {
					_, err := callable.Call([]Value{i}, nil)
					return true, err
				}
			}
		}
	}
	return false, nil
}
