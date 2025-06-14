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

// CallMul calls __mul__ on an object (self * other)
func CallMul(self, other core.Value, ctx *core.Context) (core.Value, bool, error) {
	return CallDunder(self, "__mul__", []core.Value{other}, ctx)
}

// CallDiv calls __truediv__ on an object (self / other)
func CallDiv(self, other core.Value, ctx *core.Context) (core.Value, bool, error) {
	return CallDunder(self, "__truediv__", []core.Value{other}, ctx)
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
