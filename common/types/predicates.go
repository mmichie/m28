package types

import "github.com/mmichie/m28/core"

// Type checking predicates - return bool for easy conditional logic

// IsNumber checks if value is a NumberValue
func IsNumber(v core.Value) bool {
	_, ok := v.(core.NumberValue)
	return ok
}

// IsString checks if value is a StringValue
func IsString(v core.Value) bool {
	_, ok := v.(core.StringValue)
	return ok
}

// IsBool checks if value is a BoolValue
func IsBool(v core.Value) bool {
	_, ok := v.(core.BoolValue)
	return ok
}

// IsList checks if value is a ListValue
func IsList(v core.Value) bool {
	_, ok := v.(core.ListValue)
	return ok
}

// IsDict checks if value is a DictValue
func IsDict(v core.Value) bool {
	_, ok := v.(*core.DictValue)
	return ok
}

// IsSet checks if value is a SetValue
func IsSet(v core.Value) bool {
	_, ok := v.(*core.SetValue)
	return ok
}

// IsTuple checks if value is a TupleValue
func IsTuple(v core.Value) bool {
	_, ok := v.(core.TupleValue)
	return ok
}

// IsFunction checks if value is callable (implements Callable interface)
func IsFunction(v core.Value) bool {
	_, ok := v.(core.Callable)
	return ok
}

// IsClass checks if value is a Class
func IsClass(v core.Value) bool {
	_, ok := v.(*core.Class)
	return ok
}

// IsInstance checks if value is an Instance
func IsInstance(v core.Value) bool {
	_, ok := v.(*core.Instance)
	return ok
}

// IsNil checks if value is nil
func IsNil(v core.Value) bool {
	return v == core.Nil
}

// IsContainer checks if value is any container type (List, Tuple, Dict, Set)
func IsContainer(v core.Value) bool {
	switch v.(type) {
	case core.ListValue, core.TupleValue, *core.DictValue, *core.SetValue:
		return true
	default:
		return false
	}
}

// IsSequence checks if value is a sequence type (List, Tuple, String, Range)
func IsSequence(v core.Value) bool {
	switch v.(type) {
	case core.ListValue, core.TupleValue, core.StringValue, *core.RangeValue:
		return true
	default:
		return false
	}
}

// IsMapping checks if value is a mapping type (Dict)
func IsMapping(v core.Value) bool {
	_, ok := v.(*core.DictValue)
	return ok
}

// IsCallable checks if value is callable (Function, Class, or has __call__)
func IsCallable(v core.Value) bool {
	// Check if implements Callable interface
	if _, ok := v.(core.Callable); ok {
		return true
	}

	// Check for classes (which are callable)
	if _, ok := v.(*core.Class); ok {
		return true
	}

	// Check for __call__ method on instances
	if inst, ok := v.(*core.Instance); ok {
		if inst.Class != nil && inst.Class.Methods != nil {
			_, hasCall := inst.Class.Methods["__call__"]
			return hasCall
		}
	}

	return false
}

// IsIterable checks if value implements the Iterable interface
func IsIterable(v core.Value) bool {
	_, ok := v.(core.Iterable)
	return ok
}

// IsHashable checks if value can be used as a dict key or set member
// This is already implemented in core/utilities.go, so we'll use that
func IsHashable(v core.Value) bool {
	return core.IsHashable(v)
}

// IsNumeric checks if value can be used in numeric operations
func IsNumeric(v core.Value) bool {
	_, ok := v.(core.NumberValue)
	return ok
}

// IsTruthy checks if value evaluates to true in boolean context
// This is already implemented in core/utilities.go, so we'll use that
func IsTruthy(v core.Value) bool {
	return core.IsTruthy(v)
}
