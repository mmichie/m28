package types

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// CallDunder calls a dunder (double underscore) method on an object if it exists
// Returns (result, found, error) where found indicates if the method was found
func CallDunder(obj core.Value, method string, args []core.Value, ctx *core.Context) (core.Value, bool, error) {
	// Check if object has attributes
	attrObj, ok := obj.(interface {
		GetAttr(string) (core.Value, bool)
	})
	if !ok {
		return nil, false, nil
	}

	// Look for the dunder method
	methodVal, exists := attrObj.GetAttr(method)
	if !exists {
		return nil, false, nil
	}

	// Check if it's callable
	callable, ok := methodVal.(interface {
		Call([]core.Value, *core.Context) (core.Value, error)
	})
	if !ok {
		return nil, true, fmt.Errorf("'%s' object attribute '%s' is not callable", obj.Type(), method)
	}

	// Call the method
	result, err := callable.Call(args, ctx)

	// If the method returns NotImplemented, treat it as not found
	// This allows the reflected operation to be tried
	if err == nil && result == core.NotImplemented {
		return nil, false, nil
	}

	return result, true, err
}

// HasDunder checks if an object has a dunder method
func HasDunder(obj core.Value, method string) bool {
	attrObj, ok := obj.(interface {
		GetAttr(string) (core.Value, bool)
	})
	if !ok {
		return false
	}

	methodVal, exists := attrObj.GetAttr(method)
	if !exists {
		return false
	}

	// Check if it's callable
	_, ok = methodVal.(interface {
		Call([]core.Value, *core.Context) (core.Value, error)
	})
	return ok
}

// GetDunder gets a dunder method from an object
func GetDunder(obj core.Value, method string) (core.Value, bool) {
	attrObj, ok := obj.(interface {
		GetAttr(string) (core.Value, bool)
	})
	if !ok {
		return nil, false
	}

	return attrObj.GetAttr(method)
}

// Common dunder method helpers

// CallAdd calls __add__ on an object (self + other)
func CallAdd(self, other core.Value, ctx *core.Context) (core.Value, bool, error) {
	return CallDunder(self, "__add__", []core.Value{other}, ctx)
}

// CallRadd calls __radd__ on an object (other + self)
func CallRadd(self, other core.Value, ctx *core.Context) (core.Value, bool, error) {
	return CallDunder(self, "__radd__", []core.Value{other}, ctx)
}

// CallSub calls __sub__ on an object (self - other)
func CallSub(self, other core.Value, ctx *core.Context) (core.Value, bool, error) {
	return CallDunder(self, "__sub__", []core.Value{other}, ctx)
}

// CallRsub calls __rsub__ on an object (other - self)
func CallRsub(self, other core.Value, ctx *core.Context) (core.Value, bool, error) {
	return CallDunder(self, "__rsub__", []core.Value{other}, ctx)
}

// CallMul calls __mul__ on an object (self * other)
func CallMul(self, other core.Value, ctx *core.Context) (core.Value, bool, error) {
	return CallDunder(self, "__mul__", []core.Value{other}, ctx)
}

// CallRmul calls __rmul__ on an object (other * self)
func CallRmul(self, other core.Value, ctx *core.Context) (core.Value, bool, error) {
	return CallDunder(self, "__rmul__", []core.Value{other}, ctx)
}

// CallDiv calls __truediv__ on an object (self / other)
func CallDiv(self, other core.Value, ctx *core.Context) (core.Value, bool, error) {
	return CallDunder(self, "__truediv__", []core.Value{other}, ctx)
}

// CallRdiv calls __rtruediv__ on an object (other / self)
func CallRdiv(self, other core.Value, ctx *core.Context) (core.Value, bool, error) {
	return CallDunder(self, "__rtruediv__", []core.Value{other}, ctx)
}

// CallLen calls __len__ on an object and returns an integer
func CallLen(obj core.Value, ctx *core.Context) (int, bool, error) {
	result, found, err := CallDunder(obj, "__len__", []core.Value{}, ctx)
	if !found || err != nil {
		return 0, found, err
	}

	// Ensure result is a number
	num, ok := AsNumber(result)
	if !ok {
		return 0, true, fmt.Errorf("__len__ returned non-numeric value: %s", result.Type())
	}

	// Ensure it's a non-negative integer
	length := int(num)
	if float64(length) != num || length < 0 {
		return 0, true, fmt.Errorf("__len__ should return an integer >= 0")
	}

	return length, true, nil
}

// CallStr calls __str__ on an object
func CallStr(obj core.Value, ctx *core.Context) (string, bool, error) {
	result, found, err := CallDunder(obj, "__str__", []core.Value{}, ctx)
	if !found || err != nil {
		return "", found, err
	}

	// Ensure result is a string
	str, ok := AsString(result)
	if !ok {
		return "", true, fmt.Errorf("__str__ returned non-string value: %s", result.Type())
	}

	return str, true, nil
}

// CallRepr calls __repr__ on an object
func CallRepr(obj core.Value, ctx *core.Context) (string, bool, error) {
	result, found, err := CallDunder(obj, "__repr__", []core.Value{}, ctx)
	if !found || err != nil {
		return "", found, err
	}

	// Ensure result is a string
	str, ok := AsString(result)
	if !ok {
		return "", true, fmt.Errorf("__repr__ returned non-string value: %s", result.Type())
	}

	return str, true, nil
}

// CallBool calls __bool__ on an object
func CallBool(obj core.Value, ctx *core.Context) (bool, bool, error) {
	result, found, err := CallDunder(obj, "__bool__", []core.Value{}, ctx)
	if !found || err != nil {
		return false, found, err
	}

	// Ensure result is a bool
	b, ok := AsBool(result)
	if !ok {
		return false, true, fmt.Errorf("__bool__ should return bool, returned %s", result.Type())
	}

	return b, true, nil
}

// CallIndex calls __index__ on an object to convert it to an integer index
// Used for slicing, indexing, and functions like bin(), hex(), oct()
func CallIndex(obj core.Value, ctx *core.Context) (int, bool, error) {
	result, found, err := CallDunder(obj, "__index__", []core.Value{}, ctx)
	if !found || err != nil {
		return 0, found, err
	}

	// Ensure result is an integer
	num, ok := result.(core.NumberValue)
	if !ok {
		return 0, true, fmt.Errorf("__index__ returned non-int type %s", result.Type())
	}

	// Check if it's an integer value (not a float)
	intVal := int(num)
	if float64(intVal) != float64(num) {
		return 0, true, fmt.Errorf("__index__ returned non-integer value %v", num)
	}

	return intVal, true, nil
}

// CallContains calls __contains__ on an object (item in self)
func CallContains(self, item core.Value, ctx *core.Context) (bool, bool, error) {
	result, found, err := CallDunder(self, "__contains__", []core.Value{item}, ctx)
	if !found || err != nil {
		return false, found, err
	}

	// Convert result to bool using truthiness
	return IsTruthy(result), true, nil
}

// CallGetItem calls __getitem__ on an object (self[key])
func CallGetItem(self, key core.Value, ctx *core.Context) (core.Value, bool, error) {
	return CallDunder(self, "__getitem__", []core.Value{key}, ctx)
}

// CallSetItem calls __setitem__ on an object (self[key] = value)
func CallSetItem(self, key, value core.Value, ctx *core.Context) (bool, error) {
	_, found, err := CallDunder(self, "__setitem__", []core.Value{key, value}, ctx)
	return found, err
}

// CallDelItem calls __delitem__ on an object (del self[key])
func CallDelItem(self, key core.Value, ctx *core.Context) (bool, error) {
	_, found, err := CallDunder(self, "__delitem__", []core.Value{key}, ctx)
	return found, err
}

// CallIter calls __iter__ on an object to get an iterator
func CallIter(obj core.Value, ctx *core.Context) (core.Value, bool, error) {
	return CallDunder(obj, "__iter__", []core.Value{}, ctx)
}

// CallNext calls __next__ on an iterator
func CallNext(obj core.Value, ctx *core.Context) (core.Value, bool, error) {
	return CallDunder(obj, "__next__", []core.Value{}, ctx)
}

// CallCall calls __call__ on an object (makes it callable)
func CallCall(obj core.Value, args []core.Value, ctx *core.Context) (core.Value, bool, error) {
	return CallDunder(obj, "__call__", args, ctx)
}

// CallGet calls __get__ on a descriptor (descriptor protocol)
// descriptor.__get__(instance, owner_type)
func CallGet(descriptor, instance, ownerType core.Value, ctx *core.Context) (core.Value, bool, error) {
	return CallDunder(descriptor, "__get__", []core.Value{instance, ownerType}, ctx)
}

// CallSet calls __set__ on a descriptor (descriptor protocol)
// descriptor.__set__(instance, value)
func CallSet(descriptor, instance, value core.Value, ctx *core.Context) (core.Value, bool, error) {
	return CallDunder(descriptor, "__set__", []core.Value{instance, value}, ctx)
}

// CallDelete calls __delete__ on a descriptor (descriptor protocol)
// descriptor.__delete__(instance)
func CallDelete(descriptor, instance core.Value, ctx *core.Context) (core.Value, bool, error) {
	return CallDunder(descriptor, "__delete__", []core.Value{instance}, ctx)
}

// HasSet checks if a descriptor has __set__ method (data descriptor check)
func HasSet(obj core.Value) bool {
	return HasDunder(obj, "__set__")
}

// HasDelete checks if a descriptor has __delete__ method (data descriptor check)
func HasDelete(obj core.Value) bool {
	return HasDunder(obj, "__delete__")
}

// HasGet checks if a descriptor has __get__ method
func HasGet(obj core.Value) bool {
	return HasDunder(obj, "__get__")
}

// CallEq calls __eq__ on an object (self == other)
func CallEq(self, other core.Value, ctx *core.Context) (bool, bool, error) {
	result, found, err := CallDunder(self, "__eq__", []core.Value{other}, ctx)
	if !found || err != nil {
		return false, found, err
	}

	// Convert result to bool
	if b, ok := AsBool(result); ok {
		return b, true, nil
	}

	// Use truthiness as fallback
	return IsTruthy(result), true, nil
}

// CallLt calls __lt__ on an object (self < other)
func CallLt(self, other core.Value, ctx *core.Context) (bool, bool, error) {
	result, found, err := CallDunder(self, "__lt__", []core.Value{other}, ctx)
	if !found || err != nil {
		return false, found, err
	}

	// Convert result to bool
	if b, ok := AsBool(result); ok {
		return b, true, nil
	}

	// Use truthiness as fallback
	return IsTruthy(result), true, nil
}

// CallInt calls __int__ on an object to convert it to an integer
func CallInt(obj core.Value, ctx *core.Context) (int, bool, error) {
	result, found, err := CallDunder(obj, "__int__", []core.Value{}, ctx)
	if !found || err != nil {
		return 0, found, err
	}

	// Ensure result is a number
	num, ok := AsNumber(result)
	if !ok {
		return 0, true, fmt.Errorf("__int__ returned non-int type %s", result.Type())
	}

	// Check if it's an integer value (not a float)
	intVal := int(num)
	if float64(intVal) != num {
		return 0, true, fmt.Errorf("__int__ returned non-integer value %v", num)
	}

	return intVal, true, nil
}

// CallFloat calls __float__ on an object to convert it to a float
func CallFloat(obj core.Value, ctx *core.Context) (float64, bool, error) {
	result, found, err := CallDunder(obj, "__float__", []core.Value{}, ctx)
	if !found || err != nil {
		return 0, found, err
	}

	// Ensure result is a number
	num, ok := AsNumber(result)
	if !ok {
		return 0, true, fmt.Errorf("__float__ returned non-numeric type %s", result.Type())
	}

	return num, true, nil
}

// CallFormat calls __format__ on an object with a format specifier
func CallFormat(obj core.Value, formatSpec string, ctx *core.Context) (string, bool, error) {
	result, found, err := CallDunder(obj, "__format__", []core.Value{core.StringValue(formatSpec)}, ctx)
	if !found || err != nil {
		return "", found, err
	}

	// Ensure result is a string
	str, ok := AsString(result)
	if !ok {
		return "", true, fmt.Errorf("__format__ returned non-string value: %s", result.Type())
	}

	return str, true, nil
}

// CallAbs calls __abs__ on an object to get its absolute value
func CallAbs(obj core.Value, ctx *core.Context) (core.Value, bool, error) {
	return CallDunder(obj, "__abs__", []core.Value{}, ctx)
}

// CallRound calls __round__ on an object with optional ndigits
func CallRound(obj core.Value, ndigits core.Value, ctx *core.Context) (core.Value, bool, error) {
	args := []core.Value{}
	if ndigits != nil {
		args = append(args, ndigits)
	}
	return CallDunder(obj, "__round__", args, ctx)
}

// CallHash calls __hash__ on an object to get its hash value
func CallHash(obj core.Value, ctx *core.Context) (int, bool, error) {
	result, found, err := CallDunder(obj, "__hash__", []core.Value{}, ctx)
	if !found || err != nil {
		return 0, found, err
	}

	// Ensure result is a number
	num, ok := AsNumber(result)
	if !ok {
		return 0, true, fmt.Errorf("__hash__ returned non-numeric value: %s", result.Type())
	}

	// Convert to int
	hashVal := int(num)
	if float64(hashVal) != num {
		return 0, true, fmt.Errorf("__hash__ should return an integer")
	}

	return hashVal, true, nil
}

// ToIndex converts a value to an integer index using __index__ if available
// This is the standard way to convert values to indices for slicing/indexing
// Returns (intValue, error)
func ToIndex(obj core.Value, ctx *core.Context) (int, error) {
	// First try __index__ dunder method
	if intVal, found, err := CallIndex(obj, ctx); found {
		if err != nil {
			return 0, err
		}
		return intVal, nil
	}

	// Fall back to NumberValue for built-in numeric types
	if num, ok := obj.(core.NumberValue); ok {
		intVal := int(num)
		// Check if it's actually an integer
		if float64(intVal) != float64(num) {
			return 0, fmt.Errorf("slice indices must be integers or None, not float")
		}
		return intVal, nil
	}

	// Not convertible to index
	return 0, fmt.Errorf("'%s' object cannot be interpreted as an integer", obj.Type())
}
