package types

import (
	"github.com/mmichie/m28/common/errors"
	"github.com/mmichie/m28/core"
)

// Basic type extractors - return the value and a boolean indicating success

// AsNumber extracts a float64 from a NumberValue
func AsNumber(v core.Value) (float64, bool) {
	if num, ok := v.(core.NumberValue); ok {
		return float64(num), true
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
func AsList(v core.Value) (core.ListValue, bool) {
	if list, ok := v.(core.ListValue); ok {
		return list, true
	}
	return core.ListValue{}, false
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
	return nil, false
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
func RequireList(v core.Value, context string) (core.ListValue, error) {
	if list, ok := AsList(v); ok {
		return list, nil
	}
	return core.ListValue{}, errors.NewTypeError(context, "list", string(v.Type()))
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
