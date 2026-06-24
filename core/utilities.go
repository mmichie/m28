package core

import (
	"fmt"
	"runtime"
	"strconv"
	"strings"
	"sync"
)

// HasUserEq reports whether v is an instance whose class hierarchy defines
// __eq__ somewhere other than the built-in object/str bases. Such an override
// must take precedence over comparing str-subclass instances by value.
func HasUserEq(v Value) bool {
	if inst, ok := v.(*Instance); ok {
		if _, defining, ok := inst.Class.GetMethodWithClass("__eq__"); ok {
			switch defining.Name {
			case "object", "str", "string", "int", "float":
				return false
			}
			return true
		}
	}
	return false
}

// HasUserNe reports whether v is an instance whose class hierarchy defines a
// custom __ne__ (outside the built-in bases). The built-in default __ne__ is
// unreliable for operands whose __eq__ returns NotImplemented, so callers should
// prefer negating __eq__ unless a real override exists.
func HasUserNe(v Value) bool {
	if inst, ok := v.(*Instance); ok {
		if _, defining, ok := inst.Class.GetMethodWithClass("__ne__"); ok {
			switch defining.Name {
			case "object", "str", "string", "int", "float":
				return false
			}
			return true
		}
	}
	return false
}

// EqualValuesWithError compares two values for equality and returns any error from __eq__.
// This is used by dict operations where __eq__ exceptions should propagate.
func EqualValuesWithError(a, b Value, ctx *Context) (bool, error) {
	// Check nil values first
	if a == nil && b == nil {
		return true, nil
	}
	if a == nil || b == nil {
		return false, nil
	}

	// str-subclass instances compare by their backing string (the inherited
	// str.__eq__ behavior), unless either side defines a custom __eq__.
	if as, aok := StrBacking(a); aok {
		if bs, bok := StrBacking(b); bok && !HasUserEq(a) && !HasUserEq(b) {
			return as == bs, nil
		}
	}
	// int/float-subclass instances compare by their numeric __value__ (only when
	// an Instance is involved, so plain-number BigInt logic below is preserved).
	if _, ai := a.(*Instance); ai {
		if an, ok := NumBacking(a); ok {
			if bn, ok := NumBacking(b); ok && !HasUserEq(a) && !HasUserEq(b) {
				return an == bn, nil
			}
		}
	} else if _, bi := b.(*Instance); bi {
		if bn, ok := NumBacking(b); ok {
			if an, ok := NumBacking(a); ok && !HasUserEq(a) && !HasUserEq(b) {
				return an == bn, nil
			}
		}
	}

	// For instances, check if they have a custom __eq__ method
	if aInst, ok := a.(*Instance); ok {
		if eqMethod, ok := aInst.GetAttr("__eq__"); ok {
			if callable, ok := eqMethod.(interface {
				Call([]Value, *Context) (Value, error)
			}); ok {
				// Call __eq__(b)
				result, err := callable.Call([]Value{b}, ctx)
				if err != nil {
					// Propagate the error from __eq__!
					return false, err
				}
				if boolResult, ok := result.(BoolValue); ok {
					return bool(boolResult), nil
				}
				// NotImplemented should try reflected operation
				if result == NotImplemented {
					// Try b.__eq__(a) if b is an instance
					if bInst, ok := b.(*Instance); ok {
						if eqMethodB, ok := bInst.GetAttr("__eq__"); ok {
							if callableB, ok := eqMethodB.(interface {
								Call([]Value, *Context) (Value, error)
							}); ok {
								resultB, errB := callableB.Call([]Value{a}, ctx)
								if errB != nil {
									return false, errB
								}
								if resultB != NotImplemented {
									if boolResult, ok := resultB.(BoolValue); ok {
										return bool(boolResult), nil
									}
									return IsTruthy(resultB), nil
								}
							}
						}
						return aInst == bInst, nil
					}
					return false, nil
				}
				// For other results, use truthiness
				return IsTruthy(result), nil
			}
		}
		// No __eq__ method, use identity comparison
		if bInst, ok := b.(*Instance); ok {
			return aInst == bInst, nil
		}
		return false, nil
	}

	// For compound types, recursively compare with error propagation
	if aT, ok := a.(TupleValue); ok {
		if bT, ok := b.(TupleValue); ok {
			if len(aT) != len(bT) {
				return false, nil
			}
			for i := range aT {
				eq, err := EqualValuesWithError(aT[i], bT[i], ctx)
				if err != nil {
					return false, err
				}
				if !eq {
					return false, nil
				}
			}
			return true, nil
		}
		return false, nil
	}
	// Helper: extract *ListValue from either *ListValue or *ListInstance
	asListData := func(v Value) (*ListValue, bool) {
		if lv, ok := v.(*ListValue); ok {
			return lv, true
		}
		if li, ok := v.(*ListInstance); ok {
			return li.Data, true
		}
		return nil, false
	}
	if aL, ok := asListData(a); ok {
		if bL, ok := asListData(b); ok {
			// Compare element-wise live (not via snapshot) to match CPython semantics:
			// mutations during __eq__ are visible in subsequent bound checks.
			i := 0
			for i < aL.Len() && i < bL.Len() {
				eq, err := EqualValuesWithError(aL.items[i], bL.items[i], ctx)
				if err != nil {
					return false, err
				}
				if !eq {
					break
				}
				i++
			}
			// If we exhausted one (or both) lists, compare current sizes.
			// Mutations during __eq__ can change sizes, matching CPython behavior.
			if i >= aL.Len() || i >= bL.Len() {
				return aL.Len() == bL.Len(), nil
			}
			return false, nil
		}
		return false, nil
	}

	// For non-instances, use existing EqualValues (which doesn't raise errors)
	return EqualValues(a, b), nil
}

// equalWithReflection compares two values with full Python __eq__ semantics including
// reflected operation (b.__eq__(a) when a.__eq__(b) returns NotImplemented).
// Used for dict value comparison where Python semantics require reflection.
func equalWithReflection(a, b Value, ctx *Context) (bool, error) {
	// First try a.__eq__(b)
	if aInst, ok := a.(*Instance); ok {
		if eqMethod, ok := aInst.GetAttr("__eq__"); ok {
			if callable, ok := eqMethod.(interface {
				Call([]Value, *Context) (Value, error)
			}); ok {
				result, err := callable.Call([]Value{b}, ctx)
				if err != nil {
					return false, err
				}
				if result != NotImplemented {
					if boolResult, ok := result.(BoolValue); ok {
						return bool(boolResult), nil
					}
					return IsTruthy(result), nil
				}
			}
		}
	}
	// Try b.__eq__(a) (reflected)
	if bInst, ok := b.(*Instance); ok {
		if eqMethod, ok := bInst.GetAttr("__eq__"); ok {
			if callable, ok := eqMethod.(interface {
				Call([]Value, *Context) (Value, error)
			}); ok {
				result, err := callable.Call([]Value{a}, ctx)
				if err != nil {
					return false, err
				}
				if result != NotImplemented {
					if boolResult, ok := result.(BoolValue); ok {
						return bool(boolResult), nil
					}
					return IsTruthy(result), nil
				}
			}
		}
	}
	return EqualValues(a, b), nil
}

// DictEqualWithError compares two dicts for equality, propagating errors from key/value __eq__.
// It uses the full Python equality semantics: for each key k1 in d1, finds k2 in d2 with
// k1==k2 (using __eq__), then checks if the values are equal.
func DictEqualWithError(d1, d2 *DictValue, ctx *Context) (bool, error) {
	if d1.Size() != d2.Size() {
		return false, nil
	}
	// For each key in d1, find matching key in d2
	for internalKey, v1 := range d1.entries {
		origKey1 := d1.keys[internalKey]
		if origKey1 == nil {
			origKey1 = StringValue(internalKey)
		}
		// Try direct key lookup first (fast path for non-instance keys)
		if v2, exists := d2.entries[internalKey]; exists {
			// Keys match by string representation, compare values with full Python semantics
			eq, err := equalWithReflection(v1, v2, ctx)
			if err != nil {
				return false, err
			}
			if !eq {
				return false, nil
			}
			continue
		}
		// Slow path: search d2 for a key that's equal to origKey1 using __eq__
		found := false
		for internalKey2, v2 := range d2.entries {
			origKey2 := d2.keys[internalKey2]
			if origKey2 == nil {
				origKey2 = StringValue(internalKey2)
			}
			// For key comparison, use EqualValuesWithError (not reflected)
			// since keys should be self-equalizing via __eq__
			eq, err := EqualValuesWithError(origKey1, origKey2, ctx)
			if err != nil {
				return false, err
			}
			if eq {
				// Keys match, compare values with full Python semantics
				valEq, err := equalWithReflection(v1, v2, ctx)
				if err != nil {
					return false, err
				}
				if !valEq {
					return false, nil
				}
				found = true
				break
			}
		}
		if !found {
			return false, nil
		}
	}
	return true, nil
}

// ComputeHash computes the hash of a value, calling __hash__ for instances
// Returns the hash value and any error from __hash__
func ComputeHash(v Value, ctx *Context) (int64, error) {
	switch val := v.(type) {
	case NumberValue:
		// Hash of a number is the number itself (for integers)
		// This matches Python's behavior
		return int64(val), nil
	case StringValue:
		// Use Go's string hash
		h := int64(0)
		for _, c := range string(val) {
			h = 31*h + int64(c)
		}
		return h, nil
	case BoolValue:
		if bool(val) {
			return 1, nil
		}
		return 0, nil
	case NilValue:
		return 0, nil
	case BytesValue:
		// Use similar hash to strings
		h := int64(0)
		for _, b := range val {
			h = 31*h + int64(b)
		}
		return h, nil
	case *Instance:
		// Check for custom __hash__ method
		if hashMethod, ok := val.GetAttr("__hash__"); ok {
			if callable, ok := hashMethod.(interface {
				Call([]Value, *Context) (Value, error)
			}); ok {
				result, err := callable.Call([]Value{}, ctx)
				if err != nil {
					return 0, err
				}
				if numResult, ok := result.(NumberValue); ok {
					return int64(numResult), nil
				}
				return 0, fmt.Errorf("__hash__ returned non-integer: %T", result)
			}
		}
		// Default: use pointer address as hash (identity hash)
		var hashNum int64
		fmt.Sscanf(fmt.Sprintf("%p", v), "%x", &hashNum)
		return hashNum, nil
	default:
		// For other types, use pointer-based hash
		var hashNum int64
		fmt.Sscanf(fmt.Sprintf("%p", v), "%x", &hashNum)
		return hashNum, nil
	}
}

// EqualValues compares two values for equality
func EqualValues(a, b Value) bool {
	// Check nil values first
	if a == nil && b == nil {
		return true
	}
	if a == nil || b == nil {
		return false
	}

	// str-subclass instances compare by their backing string unless either side
	// defines a custom __eq__ (mirrors EqualValuesWithError).
	if as, aok := StrBacking(a); aok {
		if bs, bok := StrBacking(b); bok && !HasUserEq(a) && !HasUserEq(b) {
			return as == bs
		}
	}
	// int/float-subclass instances compare by their numeric __value__ (only when
	// an Instance is involved, so plain-number BigInt logic below is preserved).
	if _, ai := a.(*Instance); ai {
		if an, ok := NumBacking(a); ok {
			if bn, ok := NumBacking(b); ok && !HasUserEq(a) && !HasUserEq(b) {
				return an == bn
			}
		}
	} else if _, bi := b.(*Instance); bi {
		if bn, ok := NumBacking(b); ok {
			if an, ok := NumBacking(a); ok && !HasUserEq(a) && !HasUserEq(b) {
				return an == bn
			}
		}
	}

	// Check by type
	switch aVal := a.(type) {
	case NumberValue:
		if bVal, ok := b.(NumberValue); ok {
			return float64(aVal) == float64(bVal)
		}
		// Allow comparison with BigInt
		if bVal, ok := b.(BigIntValue); ok {
			// Convert NumberValue to BigInt and compare
			aB := PromoteToBigInt(aVal)
			return aB.GetBigInt().Cmp(bVal.GetBigInt()) == 0
		}
		// Python bool is a subclass of int, so 0 == False and 1 == True
		if bVal, ok := b.(BoolValue); ok {
			if bool(bVal) {
				return float64(aVal) == 1.0
			}
			return float64(aVal) == 0.0
		}
		// Number can be compared to complex (as complex with imaginary part 0)
		if bVal, ok := b.(ComplexValue); ok {
			return complex(float64(aVal), 0) == complex128(bVal)
		}
	case BigIntValue:
		if bVal, ok := b.(BigIntValue); ok {
			return aVal.GetBigInt().Cmp(bVal.GetBigInt()) == 0
		}
		// Allow comparison with NumberValue
		if bVal, ok := b.(NumberValue); ok {
			bBig := PromoteToBigInt(bVal)
			return aVal.GetBigInt().Cmp(bBig.GetBigInt()) == 0
		}
	case StringValue:
		if bVal, ok := b.(StringValue); ok {
			return string(aVal) == string(bVal)
		}
		// Also compare string to Class name for backwards compatibility
		if bVal, ok := b.(*Class); ok {
			return string(aVal) == bVal.Name
		}
	case BytesValue:
		if bVal, ok := b.(BytesValue); ok {
			if len(aVal) != len(bVal) {
				return false
			}
			for i := range aVal {
				if aVal[i] != bVal[i] {
					return false
				}
			}
			return true
		}
	case *Class:
		// Compare Class to StringValue or another Class
		if bVal, ok := b.(StringValue); ok {
			return aVal.Name == string(bVal)
		}
		if bVal, ok := b.(*Class); ok {
			return aVal == bVal // Same class instance
		}
		// Also handle types that wrap Class (like IntType, StrType, etc.)
		if bWrapper, ok := b.(interface{ GetClass() *Class }); ok {
			bClass := bWrapper.GetClass()
			return aVal == bClass
		}
	case BoolValue:
		if bVal, ok := b.(BoolValue); ok {
			return bool(aVal) == bool(bVal)
		}
		// Python bool is a subclass of int, so False == 0 and True == 1
		if bVal, ok := b.(NumberValue); ok {
			if bool(aVal) {
				return float64(bVal) == 1.0
			}
			return float64(bVal) == 0.0
		}
		// Bool can also be compared to complex
		if bVal, ok := b.(ComplexValue); ok {
			expected := complex(0, 0)
			if bool(aVal) {
				expected = complex(1, 0)
			}
			return complex128(bVal) == expected
		}
	case ComplexValue:
		if bVal, ok := b.(ComplexValue); ok {
			// Compare complex to complex
			return complex128(aVal) == complex128(bVal)
		}
		// Complex can be compared to numbers (real numbers with imaginary part 0)
		if bVal, ok := b.(NumberValue); ok {
			return complex128(aVal) == complex(float64(bVal), 0)
		}
		// Complex can be compared to bool (0 or 1 with imaginary part 0)
		if bVal, ok := b.(BoolValue); ok {
			expected := complex(0, 0)
			if bool(bVal) {
				expected = complex(1, 0)
			}
			return complex128(aVal) == expected
		}
	case NilValue:
		_, ok := b.(NilValue)
		return ok
	case *ListValue:
		var bData *ListValue
		if bVal, ok := b.(*ListValue); ok {
			bData = bVal
		} else if bVal, ok := b.(*ListInstance); ok {
			bData = bVal.Data
		}
		if bData != nil {
			if aVal.Len() != bData.Len() {
				return false
			}
			aItems := aVal.Items()
			bItems := bData.Items()
			for i := range aItems {
				if !EqualValues(aItems[i], bItems[i]) {
					return false
				}
			}
			return true
		}
	case *ListInstance:
		var bData *ListValue
		if bVal, ok := b.(*ListValue); ok {
			bData = bVal
		} else if bVal, ok := b.(*ListInstance); ok {
			bData = bVal.Data
		}
		if bData != nil {
			if aVal.Data.Len() != bData.Len() {
				return false
			}
			aItems := aVal.Data.Items()
			bItems := bData.Items()
			for i := range aItems {
				if !EqualValues(aItems[i], bItems[i]) {
					return false
				}
			}
			return true
		}
	case TupleValue:
		if bVal, ok := b.(TupleValue); ok {
			if len(aVal) != len(bVal) {
				return false
			}
			for i := range aVal {
				if !EqualValues(aVal[i], bVal[i]) {
					return false
				}
			}
			return true
		}
	case *SetValue:
		if bVal, ok := b.(*SetValue); ok {
			if aVal.Size() != bVal.Size() {
				return false
			}
			// Check if all items in a are in b
			for _, v := range aVal.items {
				if !bVal.Contains(v) {
					return false
				}
			}
			return true
		}
	case *FrozenSetValue:
		if bVal, ok := b.(*FrozenSetValue); ok {
			if aVal.Size() != bVal.Size() {
				return false
			}
			// Check if all items in a are in b
			for _, v := range aVal.items {
				if !bVal.Contains(v) {
					return false
				}
			}
			return true
		}
	case *DictValue:
		if bVal, ok := b.(*DictValue); ok {
			if aVal.Size() != bVal.Size() {
				return false
			}
			// Check if all key-value pairs in a are in b
			for k, v1 := range aVal.entries {
				v2, exists := bVal.entries[k]
				if !exists || !EqualValues(v1, v2) {
					return false
				}
			}
			return true
		}
	case *SliceValue:
		if bVal, ok := b.(*SliceValue); ok {
			return EqualValues(aVal.Start, bVal.Start) &&
				EqualValues(aVal.Stop, bVal.Stop) &&
				EqualValues(aVal.Step, bVal.Step)
		}
	case *Instance:
		// For instances, check if they have a custom __eq__ method
		if eqMethod, ok := aVal.GetAttr("__eq__"); ok {
			if callable, ok := eqMethod.(interface {
				Call([]Value, *Context) (Value, error)
			}); ok {
				// Call __eq__(a, b)
				result, err := callable.Call([]Value{b}, NewContext(nil))
				if err == nil {
					if boolResult, ok := result.(BoolValue); ok {
						return bool(boolResult)
					}
				}
			}
		}
		// For enum instances (and other singletons), use identity comparison
		// This is correct for IntEnum which uses singleton pattern
		if bVal, ok := b.(*Instance); ok {
			return aVal == bVal
		}
	default:
		// Handle types that wrap Class (like IntType, StrType, etc.)
		if aWrapper, ok := a.(interface{ GetClass() *Class }); ok {
			aClass := aWrapper.GetClass()
			// Compare with another class
			if bVal, ok := b.(*Class); ok {
				return aClass == bVal
			}
			// Compare with another wrapper
			if bWrapper, ok := b.(interface{ GetClass() *Class }); ok {
				bClass := bWrapper.GetClass()
				return aClass == bClass
			}
		}
	}

	return false
}

// IsHashable determines if a value can be used as a dictionary key
func IsHashable(v Value) bool {
	switch v.(type) {
	case NumberValue, StringValue, BoolValue, NilValue, TupleValue, *FrozenSetValue, *Class, BytesValue, ComplexValue, *Module:
		// Modules (like classes and functions) are hashable by identity in
		// Python, so they can be used as dict keys / set members. ValueToKey
		// and ComputeHash already key them by pointer.
		return true
	case *Instance:
		// In Python, instances are hashable by default (using object ID)
		// unless they define __hash__ = None or __eq__ without __hash__
		inst := v.(*Instance)
		if inst.Class == nil {
			return true // No class, use default behavior
		}

		// Check the class's OWN definitions (not inherited) for __hash__ and __eq__
		// Methods are stored in Methods map, attributes in Attributes map
		// This is crucial: GetClassAttr searches parent classes, but Python 3's
		// unhashability rule is about the class's OWN definitions

		// Check for __hash__ in both Methods (def __hash__) and Attributes (__hash__ = None)
		ownHashAttr, hasOwnHashAttr := inst.Class.Attributes["__hash__"]
		_, hasOwnHashMethod := inst.Class.Methods["__hash__"]
		hasOwnHash := hasOwnHashAttr || hasOwnHashMethod

		// Check for __eq__ in Methods (def __eq__)
		_, hasOwnEqMethod := inst.Class.Methods["__eq__"]

		// Check if __hash__ is explicitly set to None (unhashable)
		if hasOwnHashAttr {
			if _, isNil := ownHashAttr.(NilValue); isNil || ownHashAttr == Nil {
				return false // Explicitly unhashable
			}
		}

		// Check if class has custom __eq__ without custom __hash__
		// In Python 3, defining __eq__ without __hash__ makes the class unhashable
		if hasOwnEqMethod && !hasOwnHash {
			return false // Custom __eq__ without __hash__ is unhashable
		}

		return true
	default:
		// Check if it's a Callable (function) - functions are hashable by identity
		if _, ok := v.(Callable); ok {
			return true
		}
		return false
	}
}

// getGoroutineID returns a unique identifier for the current goroutine
func getGoroutineID() uint64 {
	b := make([]byte, 64)
	b = b[:runtime.Stack(b, false)]
	// Format: "goroutine 123 [running]:"
	b = b[len("goroutine "):]
	b = b[:strings.IndexByte(string(b), ' ')]
	n, _ := strconv.ParseUint(string(b), 10, 64)
	return n
}

// Thread-local recursion guard for ValueToKey
// Use a simple counter per goroutine
var valueToKeyInProgress sync.Map // map[goroutineID]int

// ValueToKey converts a value to a string key for use in sets and dicts
func ValueToKey(v Value) string {
	// Get goroutine ID (approximation using stack pointer)
	goid := getGoroutineID()

	// Check if we're already inside ValueToKey for this goroutine
	val, _ := valueToKeyInProgress.Load(goid)
	depth := 0
	if val != nil {
		depth = val.(int)
	}

	if depth > 5 {
		// Too deep - use simple pointer representation
		return "p:recursive"
	}

	valueToKeyInProgress.Store(goid, depth+1)
	defer func() {
		if depth == 0 {
			valueToKeyInProgress.Delete(goid)
		} else {
			valueToKeyInProgress.Store(goid, depth)
		}
	}()

	// str-subclass instances hash and key like the string they wrap, so that
	// d[S("a")] finds d["a"] and S("a") in {"a"} is true.
	if inst, ok := v.(*Instance); ok && inst.BackingStr != nil {
		return fmt.Sprintf("s:%s", string(*inst.BackingStr))
	}

	// For primitive types
	switch val := v.(type) {
	case NumberValue:
		return fmt.Sprintf("n:%g", float64(val))
	case StringValue:
		return fmt.Sprintf("s:%s", string(val))
	case BoolValue:
		// In Python, True == 1 and False == 0, and they share the same hash
		// So True and 1 should be the same dict key
		if bool(val) {
			return "n:1"
		}
		return "n:0"
	case NilValue:
		return "nil"
	case BytesValue:
		return fmt.Sprintf("bytes:%x", []byte(val))
	case ComplexValue:
		r := real(complex128(val))
		i := imag(complex128(val))
		return fmt.Sprintf("c:%g+%gj", r, i)
	}

	switch val := v.(type) {
	case TupleValue:
		// For tuples, create a key based on their content
		elements := make([]string, len(val))
		for i, elem := range val {
			elements[i] = ValueToKey(elem)
		}
		return fmt.Sprintf("t:(%s)", strings.Join(elements, ","))
	case *FrozenSetValue:
		// For frozensets, use the hash value
		return fmt.Sprintf("fs:%d", val.Hash())
	case *Class:
		// For classes, use the class name and pointer address for uniqueness
		// Classes are hashable by identity in Python
		return fmt.Sprintf("cls:%s:%p", val.Name, v)
	case *Instance:
		// For instances of hashable types (like int subclasses),
		// use the underlying value if available
		if val.Class != nil {
			// Check if it's an int subclass
			for _, parent := range val.Class.Parents {
				if parent.Name == "int" {
					// Get the __value__ attribute which stores the actual int value
					if numVal, ok := val.Attributes["__value__"].(NumberValue); ok {
						return fmt.Sprintf("n:%g", float64(numVal))
					}
				}
			}
		}
		// For other instances, use pointer address
		return fmt.Sprintf("p:%p", v)
	default:
		// Check if it's a Callable (function) - use pointer address for identity
		if _, ok := v.(Callable); ok {
			return fmt.Sprintf("fn:%p", v)
		}
		// For non-hashable types, use pointer address
		return fmt.Sprintf("p:%p", v)
	}
}

// Compare compares two values and returns:
// -1 if a < b
// 0 if a == b
// 1 if a > b
func Compare(a, b Value) int {
	// Handle same types
	switch aVal := a.(type) {
	case NumberValue:
		if bVal, ok := b.(NumberValue); ok {
			av, bv := float64(aVal), float64(bVal)
			if av < bv {
				return -1
			} else if av > bv {
				return 1
			}
			return 0
		}
	case StringValue:
		if bVal, ok := b.(StringValue); ok {
			as, bs := string(aVal), string(bVal)
			if as < bs {
				return -1
			} else if as > bs {
				return 1
			}
			return 0
		}
	}

	// Fall back to string comparison for different types
	as, bs := a.String(), b.String()
	if as < bs {
		return -1
	} else if as > bs {
		return 1
	}
	return 0
}

// IsTruthy determines if a value is truthy
func IsTruthy(v Value) bool {
	// Nil checks
	if v == nil {
		return false
	}

	// First, check if the value has a __bool__ or __len__ method. Use the narrow
	// GetAttr interface rather than the full Object interface: Go-backed types
	// like collections.deque expose __bool__/__len__ via GetAttr but don't
	// implement CallMethod, so requiring Object would skip them and fall through
	// to the default "truthy", making e.g. `while deque:` loop forever.
	if obj, ok := v.(interface {
		GetAttr(string) (Value, bool)
	}); ok {
		if boolMethod, hasBool := obj.GetAttr("__bool__"); hasBool {
			if callable, isCallable := boolMethod.(interface {
				Call([]Value, *Context) (Value, error)
			}); isCallable {
				// Call __bool__ with no arguments
				result, err := callable.Call([]Value{}, nil)
				if err == nil {
					// __bool__ must return a boolean
					if boolVal, isBool := result.(BoolValue); isBool {
						return bool(boolVal)
					}
					// If __bool__ doesn't return a boolean, that's an error
					// For now, we'll treat it as truthy to avoid breaking existing code
					// In a strict implementation, this should raise an error
					return true
				}
			}
		}

		// If no __bool__, check for __len__ as fallback
		if lenMethod, hasLen := obj.GetAttr("__len__"); hasLen {
			if callable, isCallable := lenMethod.(interface {
				Call([]Value, *Context) (Value, error)
			}); isCallable {
				// Call __len__ with no arguments
				result, err := callable.Call([]Value{}, nil)
				if err == nil {
					// __len__ should return a number
					if numVal, isNum := result.(NumberValue); isNum {
						return float64(numVal) != 0
					}
				}
			}
		}
	}

	// Fall back to type-specific behavior
	switch val := v.(type) {
	case BoolValue:
		return bool(val)
	case NumberValue:
		return float64(val) != 0
	case StringValue:
		return string(val) != ""
	case *ListValue:
		return val.Len() > 0
	case TupleValue:
		return len(val) > 0
	case NilValue:
		return false
	case *DictValue:
		// Dictionary is truthy if it has at least one entry
		if val == nil {
			return false
		}
		return val.Size() > 0
	case *SetValue:
		// Set is truthy if it has at least one element
		if val == nil {
			return false
		}
		return val.Size() > 0
	default:
		// For other object types, we consider them truthy if they exist
		return true
	}
}

// PrintValue returns a string representation of a value
func PrintValue(val Value) string {
	if val == nil {
		return "nil"
	}
	return val.String()
}

// PrintValueWithoutQuotes returns a string representation without quotes for strings
func PrintValueWithoutQuotes(val Value) string {
	if str, ok := val.(StringValue); ok {
		return string(str)
	}
	return PrintValue(val)
}

// PrintSExpr returns an S-expression representation of a value (lists use () not [])
func PrintSExpr(val Value) string {
	if val == nil {
		return "nil"
	}
	switch v := val.(type) {
	case LocatedValue:
		return PrintSExpr(v.Value)
	case *ListValue:
		if v == nil || len(v.Items()) == 0 {
			return "()"
		}
		elements := make([]string, len(v.Items()))
		for i, item := range v.Items() {
			elements[i] = PrintSExpr(item)
		}
		return "(" + strings.Join(elements, " ") + ")"
	case StringValue:
		return string(v)
	case SymbolValue:
		return string(v)
	default:
		return val.String()
	}
}

// ProcessEscapeSequences processes escape sequences in a string
func ProcessEscapeSequences(s string) string {
	// For now, just return the string as is
	return s
}
