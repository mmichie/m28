package types

import (
	"github.com/mmichie/m28/common/errors"
	"github.com/mmichie/m28/core"
)

// Basic type extractors - return the value and a boolean indicating success

// AsNumber extracts a float64 from a NumberValue or int subclass instance
func AsNumber(v core.Value) (float64, bool) {
	if num, ok := v.(core.NumberValue); ok {
		return float64(num), true
	}
	// Check for int subclass instances (like _NamedIntConstant)
	// These are Instance objects with __value__ attribute
	if inst, ok := v.(*core.Instance); ok {
		if valueAttr, exists := inst.Attributes["__value__"]; exists {
			if num, ok := valueAttr.(core.NumberValue); ok {
				return float64(num), true
			}
		}
	}
	return 0, false
}

// AsComplex extracts a complex128 from a ComplexValue
func AsComplex(v core.Value) (complex128, bool) {
	if c, ok := v.(core.ComplexValue); ok {
		return complex128(c), true
	}
	return 0, false
}

// AsString extracts a string from a StringValue
func AsString(v core.Value) (string, bool) {
	if str, ok := v.(core.StringValue); ok {
		return string(str), true
	}
	return "", false
}

// AsBool extracts a bool from a BoolValue
func AsBool(v core.Value) (bool, bool) {
	if b, ok := v.(core.BoolValue); ok {
		return bool(b), true
	}
	return false, false
}

// AsList extracts a ListValue
func AsList(v core.Value) (*core.ListValue, bool) {
	if list, ok := v.(*core.ListValue); ok {
		return list, true
	}
	// Handle ListInstance (subclasses of list)
	if listInst, ok := v.(*core.ListInstance); ok {
		return listInst.Data, true
	}
	return nil, false
}

// AsDict extracts a DictValue
func AsDict(v core.Value) (*core.DictValue, bool) {
	if dict, ok := v.(*core.DictValue); ok {
		return dict, true
	}
	return nil, false
}

// AsSet extracts a SetValue
func AsSet(v core.Value) (*core.SetValue, bool) {
	if set, ok := v.(*core.SetValue); ok {
		return set, true
	}
	return nil, false
}

// AsTuple extracts a TupleValue
func AsTuple(v core.Value) (core.TupleValue, bool) {
	if tuple, ok := v.(core.TupleValue); ok {
		return tuple, true
	}
	return core.TupleValue{}, false
}

// AsCallable extracts a Callable (function, class, or callable object)
func AsCallable(v core.Value) (core.Callable, bool) {
	if fn, ok := v.(core.Callable); ok {
		return fn, true
	}
	return nil, false
}

// AsBuiltinFunction extracts a BuiltinFunction
func AsBuiltinFunction(v core.Value) (*core.BuiltinFunction, bool) {
	if fn, ok := v.(*core.BuiltinFunction); ok {
		return fn, true
	}
	return nil, false
}

// AsClass extracts a Class
func AsClass(v core.Value) (*core.Class, bool) {
	if class, ok := v.(*core.Class); ok {
		return class, true
	}
	return nil, false
}

// AsInstance extracts an Instance
func AsInstance(v core.Value) (*core.Instance, bool) {
	if instance, ok := v.(*core.Instance); ok {
		return instance, true
	}
	return nil, false
}

// AsIterable extracts an Iterable
func AsIterable(v core.Value) (core.Iterable, bool) {
	if iter, ok := v.(core.Iterable); ok {
		return iter, true
	}

	// Handle strings specially - they are iterable in Python
	if str, ok := v.(core.StringValue); ok {
		return &stringIterableWrapper{str}, true
	}

	// Check if the object has __iter__ method (Python protocol)
	// Any object with __iter__ is iterable in Python
	if obj, ok := v.(interface {
		GetAttr(string) (core.Value, bool)
	}); ok {
		if iterMethod, hasIter := obj.GetAttr("__iter__"); hasIter {
			// Wrap it to make it iterable
			return &iterableWrapper{obj: v, iterMethod: iterMethod}, true
		}
	}

	return nil, false
}

// iterableWrapper wraps an object with __iter__ to make it Iterable
type iterableWrapper struct {
	obj        core.Value
	iterMethod core.Value
}

func (w *iterableWrapper) Type() core.Type {
	return w.obj.Type()
}

func (w *iterableWrapper) String() string {
	return w.obj.String()
}

func (w *iterableWrapper) Iterator() core.Iterator {
	// Call __iter__() to get an iterator object
	callable, ok := w.iterMethod.(interface {
		Call([]core.Value, *core.Context) (core.Value, error)
	})
	if !ok {
		// Fallback: return empty iterator
		return &emptyIterator{}
	}

	// Call __iter__() with no arguments (it's bound to self already if it's a method)
	iterObj, err := callable.Call([]core.Value{}, nil)
	if err != nil {
		// Fallback: return empty iterator
		return &emptyIterator{}
	}

	// The iterator object should have __next__ method
	return &pythonIterator{iterObj: iterObj}
}

// pythonIterator wraps a Python iterator object (has __next__ method)
type pythonIterator struct {
	iterObj core.Value
}

func (pi *pythonIterator) Next() (core.Value, bool) {
	// Get __next__ method
	if obj, ok := pi.iterObj.(interface {
		GetAttr(string) (core.Value, bool)
	}); ok {
		if nextMethod, hasNext := obj.GetAttr("__next__"); hasNext {
			if callable, ok := nextMethod.(interface {
				Call([]core.Value, *core.Context) (core.Value, error)
			}); ok {
				val, err := callable.Call([]core.Value{}, nil)
				if err != nil {
					// StopIteration or other error - iteration done
					return nil, false
				}
				return val, true
			}
		}
	}
	return nil, false
}

func (pi *pythonIterator) Reset() {
	// Python iterators don't support reset
	// This is a limitation - calling Reset() won't actually reset the iterator
}

// emptyIterator returns no items
type emptyIterator struct{}

func (ei *emptyIterator) Next() (core.Value, bool) {
	return nil, false
}

func (ei *emptyIterator) Reset() {}

// stringIterableWrapper wraps a StringValue to implement core.Iterable
type stringIterableWrapper struct {
	str core.StringValue
}

func (w *stringIterableWrapper) Type() core.Type {
	return core.StringType
}

func (w *stringIterableWrapper) String() string {
	return w.str.String()
}

func (w *stringIterableWrapper) Iterator() core.Iterator {
	runes := []rune(string(w.str))
	return &stringIterator{
		runes: runes,
		index: 0,
	}
}

// stringIterator implements core.Iterator for strings
type stringIterator struct {
	runes []rune
	index int
}

func (si *stringIterator) Next() (core.Value, bool) {
	if si.index >= len(si.runes) {
		return nil, false
	}
	char := core.StringValue(string(si.runes[si.index]))
	si.index++
	return char, true
}

func (si *stringIterator) Reset() {
	si.index = 0
}

// AsRange extracts a RangeValue
func AsRange(v core.Value) (*core.RangeValue, bool) {
	if r, ok := v.(*core.RangeValue); ok {
		return r, true
	}
	return nil, false
}

// Error variants - return the value or an error

// RequireNumber extracts a float64 or returns a type error
func RequireNumber(v core.Value, context string) (float64, error) {
	if num, ok := AsNumber(v); ok {
		return num, nil
	}
	return 0, errors.NewTypeError(context, "number", string(v.Type()))
}

// RequireString extracts a string or returns a type error
func RequireString(v core.Value, context string) (string, error) {
	if str, ok := AsString(v); ok {
		return str, nil
	}
	return "", errors.NewTypeError(context, "string", string(v.Type()))
}

// RequireBool extracts a bool or returns a type error
func RequireBool(v core.Value, context string) (bool, error) {
	if b, ok := AsBool(v); ok {
		return b, nil
	}
	return false, errors.NewTypeError(context, "bool", string(v.Type()))
}

// RequireList extracts a ListValue or returns a type error
func RequireList(v core.Value, context string) (*core.ListValue, error) {
	if list, ok := AsList(v); ok {
		return list, nil
	}
	return nil, errors.NewTypeError(context, "list", string(v.Type()))
}

// RequireDict extracts a DictValue or returns a type error
func RequireDict(v core.Value, context string) (*core.DictValue, error) {
	if dict, ok := AsDict(v); ok {
		return dict, nil
	}
	return nil, errors.NewTypeError(context, "dict", string(v.Type()))
}

// RequireSet extracts a SetValue or returns a type error
func RequireSet(v core.Value, context string) (*core.SetValue, error) {
	if set, ok := AsSet(v); ok {
		return set, nil
	}
	return nil, errors.NewTypeError(context, "set", string(v.Type()))
}

// RequireTuple extracts a TupleValue or returns a type error
func RequireTuple(v core.Value, context string) (core.TupleValue, error) {
	if tuple, ok := AsTuple(v); ok {
		return tuple, nil
	}
	return core.TupleValue{}, errors.NewTypeError(context, "tuple", string(v.Type()))
}

// RequireCallable extracts a Callable or returns a type error
func RequireCallable(v core.Value, context string) (core.Callable, error) {
	if fn, ok := AsCallable(v); ok {
		return fn, nil
	}
	return nil, errors.NewTypeError(context, "callable", string(v.Type()))
}

// RequireClass extracts a Class or returns a type error
func RequireClass(v core.Value, context string) (*core.Class, error) {
	if class, ok := AsClass(v); ok {
		return class, nil
	}
	return nil, errors.NewTypeError(context, "class", string(v.Type()))
}

// RequireInstance extracts an Instance or returns a type error
func RequireInstance(v core.Value, context string) (*core.Instance, error) {
	if instance, ok := AsInstance(v); ok {
		return instance, nil
	}
	return nil, errors.NewTypeError(context, "instance", string(v.Type()))
}

// RequireIterable extracts an Iterable or returns a type error
func RequireIterable(v core.Value, context string) (core.Iterable, error) {
	if iter, ok := AsIterable(v); ok {
		return iter, nil
	}
	return nil, errors.NewTypeError(context, "iterable", string(v.Type()))
}

// RequireRange extracts a RangeValue or returns a type error
func RequireRange(v core.Value, context string) (*core.RangeValue, error) {
	if r, ok := AsRange(v); ok {
		return r, nil
	}
	return nil, errors.NewTypeError(context, "range", string(v.Type()))
}
