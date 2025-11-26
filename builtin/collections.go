package builtin

import (
	"fmt"

	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// convertToSlice converts an iterable value to a slice of values
// This is shared logic for list(), tuple(), and set() constructors
func convertToSlice(arg core.Value) ([]core.Value, error) {
	// Try to convert from different types
	if list, ok := types.AsList(arg); ok {
		// Copy the list
		result := make([]core.Value, list.Len())
		copy(result, list.Items())
		return result, nil
	}

	if tuple, ok := types.AsTuple(arg); ok {
		// Convert tuple to slice
		result := make([]core.Value, len(tuple))
		copy(result, tuple)
		return result, nil
	}

	if str, ok := types.AsString(arg); ok {
		// Convert string to slice of characters
		result := make([]core.Value, len(str))
		for i, ch := range str {
			result[i] = core.StringValue(string(ch))
		}
		return result, nil
	}

	// Check if it implements Iterable interface
	if iterable, ok := types.AsIterable(arg); ok {
		result := make([]core.Value, 0)
		iter := iterable.Iterator()
		for {
			val, hasNext := iter.Next()
			if !hasNext {
				break
			}
			result = append(result, val)
		}
		return result, nil
	}

	// If it's not an iterable, just create a slice with this one element
	return []core.Value{arg}, nil
}

// RegisterCollections registers collection constructor functions
func RegisterCollections(ctx *core.Context) {
	// list - Python list class
	// Create as a class so list.copy and other class methods can be accessed
	listClass := createListClass()
	ctx.Define("list", listClass)
	// Set the list class as the type object for ListType so that
	// [1,2].__class__ returns the same object as the global list
	core.GetTypeDescriptor(core.ListType).SetTypeObject(listClass)

	// tuple - create as a class (like dict) so tuple.__new__ works correctly
	tupleClass := &TupleType{Class: TupleTypeClass}
	ctx.Define("tuple", tupleClass)
	// Set the tuple class as the type object for TupleType so that
	// (1,).__class__ returns the same object as the global tuple
	core.GetTypeDescriptor(core.TupleType).SetTypeObject(tupleClass)

	// dict - Python dict class
	// Create as a class so dict.fromkeys and other class methods can be accessed
	dictClass := createDictClass()
	SetDictTypeClass(dictClass) // Store globally for type() function
	ctx.Define("dict", dictClass)

	// set - Python set class
	// Create as a class so set.copy and other class methods can be accessed
	setClass := createSetClass()
	ctx.Define("set", setClass)

	// frozenset - create a new immutable frozenset
	ctx.Define("frozenset", core.NewNamedBuiltinFunction("frozenset", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Use the type descriptor's constructor
		desc := core.GetTypeDescriptor(core.FrozenSetType)
		if desc != nil && desc.Constructor != nil {
			return desc.Constructor(args, ctx)
		}
		return nil, fmt.Errorf("frozenset type not registered")
	}))

	// slice - create a slice object
	ctx.Define("slice", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("slice", args)
		if err := v.Range(1, 3); err != nil {
			return nil, err
		}

		var start, stop, step core.Value

		switch v.Count() {
		case 1:
			// slice(stop)
			start = core.Nil
			stop = args[0]
			step = core.Nil
		case 2:
			// slice(start, stop)
			start = args[0]
			stop = args[1]
			step = core.Nil
		case 3:
			// slice(start, stop, step)
			start = args[0]
			stop = args[1]
			step = args[2]
		}

		// Validate that arguments are None or integers
		if start != core.Nil {
			if !types.IsNumber(start) {
				return nil, &core.TypeError{Message: fmt.Sprintf("slice indices must be integers or None, not %s", start.Type())}
			}
		}
		if stop != core.Nil {
			if !types.IsNumber(stop) {
				return nil, &core.TypeError{Message: fmt.Sprintf("slice indices must be integers or None, not %s", stop.Type())}
			}
		}
		if step != core.Nil {
			if !types.IsNumber(step) {
				return nil, &core.TypeError{Message: fmt.Sprintf("slice indices must be integers or None, not %s", step.Type())}
			}
			// Check that step is not zero
			if num, ok := types.AsNumber(step); ok && int64(num) == 0 {
				return nil, &core.ValueError{Message: "slice step cannot be zero"}
			}
		}

		return &core.SliceValue{
			Start: start,
			Stop:  stop,
			Step:  step,
		}, nil
	}))

	// len - get length of collection
	ctx.Define("len", core.NewNamedBuiltinFunction("len", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("len", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		arg := args[0]

		// Try to get __len__ method
		if obj, ok := arg.(interface {
			GetAttr(string) (core.Value, bool)
		}); ok {
			if lenMethod, found := obj.GetAttr("__len__"); found {
				// Call the __len__ method
				if callable, ok := types.AsCallable(lenMethod); ok {
					result, err := callable.Call([]core.Value{}, ctx)
					if err != nil {
						return nil, err
					}
					// Ensure it returns a number
					if num, ok := types.AsNumber(result); ok {
						// Check if it's an integer
						if num != float64(int(num)) {
							return nil, &core.TypeError{
								Message: fmt.Sprintf("'float' object cannot be interpreted as an integer"),
							}
						}
						// Validate it's non-negative
						if num < 0 {
							return nil, &core.ValueError{
								Message: "__len__() should return >= 0",
							}
						}
						return core.NumberValue(num), nil
					}
					return nil, &core.TypeError{
						Message: fmt.Sprintf("'%s' object cannot be interpreted as an integer", result.Type()),
					}
				}
			}
		}

		// Fallback for types without __len__
		if str, ok := types.AsString(arg); ok {
			return core.NumberValue(len(str)), nil
		}
		if list, ok := types.AsList(arg); ok {
			return core.NumberValue(list.Len()), nil
		}
		if tuple, ok := types.AsTuple(arg); ok {
			return core.NumberValue(len(tuple)), nil
		}
		if dict, ok := types.AsDict(arg); ok {
			return core.NumberValue(dict.Size()), nil
		}
		if set, ok := types.AsSet(arg); ok {
			return core.NumberValue(set.Size()), nil
		}

		return nil, &core.TypeError{Message: fmt.Sprintf("object of type '%s' has no len()", arg.Type())}
	}))

	// bytes - create a new bytes object
	ctx.Define("bytes", core.NewNamedBuiltinFunction("bytes", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Get the type descriptor for bytes to use its constructor
		desc := core.GetTypeDescriptor(core.BytesType)
		if desc == nil {
			return nil, fmt.Errorf("bytes type not registered")
		}
		return desc.Constructor(args, ctx)
	}))

	// bytearray - Python bytearray class
	// Create as a class so bytearray.copy can be accessed
	bytearrayClass := createByteArrayClass()
	ctx.Define("bytearray", bytearrayClass)

	// memoryview - create a memoryview object (stub implementation)
	// For now, just return the input object wrapped as a memoryview-like type
	ctx.Define("memoryview", core.NewNamedBuiltinFunction("memoryview", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("memoryview() takes exactly 1 argument (%d given)", len(args))
		}
		// For now, just return a simple wrapper
		// Python's memoryview is mainly used for buffer protocol
		// We can implement more functionality later if needed
		return args[0], nil
	}))
}

// ListType represents the list class that can be called and used as a base class
type ListType struct {
	*core.Class
}

// GetClass returns the embedded Class for use as a parent class
func (l *ListType) GetClass() *core.Class {
	return l.Class
}

// GetAttr delegates to the embedded Class to expose methods like __new__
func (l *ListType) GetAttr(name string) (core.Value, bool) {
	return l.Class.GetAttr(name)
}

// Call implements the callable interface for list() construction
func (l *ListType) Call(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) == 0 {
		return core.NewList(), nil
	}
	if len(args) == 1 {
		// Python-style: Convert iterable to list
		items, err := convertToSlice(args[0])
		if err != nil {
			return nil, err
		}
		return core.NewList(items...), nil
	}
	// Multiple arguments: create a list from all arguments
	return core.NewList(args...), nil
}

// CallWithKeywords overrides the embedded Class's CallWithKeywords
// to ensure our custom Call is used instead of the generic class instantiation
func (l *ListType) CallWithKeywords(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
	// For list, keyword arguments are not supported in the constructor
	if len(kwargs) > 0 {
		return nil, &core.TypeError{Message: "list() does not accept keyword arguments"}
	}
	// Use the custom Call method which handles multiple arguments
	// e.g., list(1, 2, 3) creates [1, 2, 3]
	// List subclasses will call Class.CallWithKeywords directly, not through ListType
	return l.Call(args, ctx)
}

// TupleType represents the tuple class that can be called and has __new__
type TupleType struct {
	*core.Class
}

// GetClass returns the embedded Class for use as a parent class
func (t *TupleType) GetClass() *core.Class {
	return t.Class
}

// Call implements the callable interface for tuple() construction
func (t *TupleType) Call(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) == 0 {
		return core.EmptyTuple, nil
	}
	if len(args) == 1 {
		// Python-style: Convert iterable to tuple
		items, err := convertToSlice(args[0])
		if err != nil {
			return nil, err
		}
		return core.TupleValue(items), nil
	}
	// Multiple arguments: create a tuple from all arguments
	return core.TupleValue(args), nil
}

// CallWithKeywords overrides the embedded Class's CallWithKeywords
// to ensure our custom Call is used instead of the generic class instantiation
func (t *TupleType) CallWithKeywords(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
	// For tuple, keyword arguments are not supported in the constructor
	// Just ignore kwargs and call our custom Call method
	if len(kwargs) > 0 {
		return nil, &core.TypeError{Message: "tuple() does not accept keyword arguments"}
	}
	return t.Call(args, ctx)
}

// Type returns the type of the TupleType
func (t *TupleType) Type() core.Type {
	return core.ClassType
}

// String returns the string representation
func (t *TupleType) String() string {
	return "<class 'tuple'>"
}

// SetType represents the set class that can be called and used as a base class
type SetType struct {
	*core.Class
}

// GetClass returns the embedded Class for use as a parent class
func (s *SetType) GetClass() *core.Class {
	return s.Class
}

// Call implements the callable interface for set() construction
func (s *SetType) Call(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("set", args)
	if err := v.Max(1); err != nil {
		return nil, err
	}

	if v.Count() == 0 {
		return core.NewSet(), nil
	}

	// Convert iterable to set
	set := core.NewSet()
	arg := args[0]

	if list, ok := types.AsList(arg); ok {
		for _, elem := range list.Items() {
			set.Add(elem)
		}
		return set, nil
	}

	if tuple, ok := types.AsTuple(arg); ok {
		for _, elem := range tuple {
			set.Add(elem)
		}
		return set, nil
	}

	if str, ok := types.AsString(arg); ok {
		// Convert string to set of characters
		for _, ch := range str {
			charVal := core.StringValue(string(ch))
			set.Add(charVal)
		}
		return set, nil
	}

	// Check if it implements Iterable interface
	if iterable, ok := types.AsIterable(arg); ok {
		iter := iterable.Iterator()
		for {
			val, hasNext := iter.Next()
			if !hasNext {
				break
			}
			set.Add(val)
		}
		return set, nil
	}

	return nil, &core.TypeError{Message: fmt.Sprintf("set() argument must be an iterable, not '%s'", arg.Type())}
}

// CallWithKeywords overrides the embedded Class's CallWithKeywords
// to ensure our custom Call is used instead of the generic class instantiation
func (s *SetType) CallWithKeywords(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
	// For set, keyword arguments are not supported in the constructor
	// Just ignore kwargs and call our custom Call method
	if len(kwargs) > 0 {
		return nil, &core.TypeError{Message: "set() does not accept keyword arguments"}
	}
	return s.Call(args, ctx)
}

// Type returns the type of SetType
func (s *SetType) Type() core.Type {
	return core.ClassType
}

// String returns the string representation
func (s *SetType) String() string {
	return "<class 'set'>"
}

// ByteArrayTypeClass represents the bytearray class that can be called and used as a base class
type ByteArrayTypeClass struct {
	*core.Class
}

// GetClass returns the embedded Class for use as a parent class
func (b *ByteArrayTypeClass) GetClass() *core.Class {
	return b.Class
}

// Call implements the callable interface for bytearray() construction
func (b *ByteArrayTypeClass) Call(args []core.Value, ctx *core.Context) (core.Value, error) {
	// Get the type descriptor for bytearray to use its constructor
	desc := core.GetTypeDescriptor(core.ByteArrayType)
	if desc == nil {
		return nil, fmt.Errorf("bytearray type not registered")
	}
	return desc.Constructor(args, ctx)
}

// CallWithKeywords overrides the embedded Class's CallWithKeywords
// to ensure our custom Call is used instead of the generic class instantiation
func (b *ByteArrayTypeClass) CallWithKeywords(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
	// For bytearray, keyword arguments are not supported in the constructor
	// Just ignore kwargs and call our custom Call method
	if len(kwargs) > 0 {
		return nil, fmt.Errorf("bytearray() does not accept keyword arguments")
	}
	return b.Call(args, ctx)
}

// Type returns the type of ByteArrayTypeClass
func (b *ByteArrayTypeClass) Type() core.Type {
	return core.ClassType
}

// String returns the string representation
func (b *ByteArrayTypeClass) String() string {
	return "<class 'bytearray'>"
}

// DictType represents the dict class that can be called and has class methods
type DictType struct {
	*core.Class
}

// GetClass returns the embedded Class for use as a parent class
func (d *DictType) GetClass() *core.Class {
	return d.Class
}

// Call implements the callable interface for dict() construction
func (d *DictType) Call(args []core.Value, ctx *core.Context) (core.Value, error) {
	dict := core.NewDict()

	// Handle different argument patterns:
	// 1. dict() - empty dict
	// 2. dict(iterable) - from iterable of pairs
	// 3. dict(key1, val1, key2, val2, ...) - traditional key-value pairs

	if len(args) == 0 {
		// Empty dict
		return dict, nil
	}

	// Check if args are key-value pairs (even number of args > 1)
	if len(args) > 1 && len(args)%2 == 0 {
		// Traditional key-value pairs syntax: dict("key1", val1, "key2", val2)
		for i := 0; i < len(args); i += 2 {
			keyVal := args[i]
			value := args[i+1]

			// Use SetValue for proper key conversion
			if err := dict.SetValue(keyVal, value); err != nil {
				return nil, err
			}
		}
		return dict, nil
	}

	// Single argument - should be an iterable
	if len(args) == 1 {
		// Convert from another dict or iterable of pairs
		switch v := args[0].(type) {
		case *core.DictValue:
			// Copy the dictionary by iterating over items
			for _, key := range v.Keys() {
				if val, ok := v.Get(key); ok {
					dict.Set(key, val)
				}
			}
		case *core.ListValue:
			// Expect list of pairs
			for i, item := range v.Items() {
				var key, value core.Value

				// Item can be a list or tuple
				if pair, ok := item.(*core.ListValue); ok {
					if pair.Len() != 2 {
						return nil, fmt.Errorf("dict update sequence element #%d has length %d; 2 is required", i, pair.Len())
					}
					key = pair.Items()[0]
					value = pair.Items()[1]
				} else if tuple, ok := item.(core.TupleValue); ok {
					if len(tuple) != 2 {
						return nil, fmt.Errorf("dict update sequence element #%d has length %d; 2 is required", i, len(tuple))
					}
					key = tuple[0]
					value = tuple[1]
				} else {
					return nil, fmt.Errorf("dict update sequence element #%d is not a sequence", i)
				}

				// Use SetValue for proper key handling (supports any hashable type)
				if err := dict.SetValue(key, value); err != nil {
					return nil, fmt.Errorf("error setting dict key: %v", err)
				}
			}
		default:
			// Try to treat as a dict-like object with .items() or .keys() method
			// This handles OrderedDict and other dict-like types
			if objWithAttr, ok := args[0].(interface {
				GetAttr(string) (core.Value, bool)
			}); ok {
				// Try .items() method first
				if itemsMethod, hasItems := objWithAttr.GetAttr("items"); hasItems {
					if callable, ok := itemsMethod.(core.Callable); ok {
						// Call .items()
						itemsResult, err := callable.Call([]core.Value{}, ctx)
						if err == nil {
							// itemsResult should be an iterable of (key, value) pairs
							if iterable, ok := itemsResult.(core.Iterable); ok {
								iter := iterable.Iterator()
								for {
									item, hasNext := iter.Next()
									if !hasNext {
										break
									}
									// Item should be a pair (tuple or list)
									var key, value core.Value
									if pair, ok := item.(*core.ListValue); ok && pair.Len() == 2 {
										key = pair.Items()[0]
										value = pair.Items()[1]
									} else if tuple, ok := item.(core.TupleValue); ok && len(tuple) == 2 {
										key = tuple[0]
										value = tuple[1]
									} else {
										return nil, fmt.Errorf("dict .items() yielded non-pair")
									}
									if err := dict.SetValue(key, value); err != nil {
										return nil, err
									}
								}
								return dict, nil
							}
						}
					}
				}
			}

			// Try to treat as a general iterable (handles generators, iterators, etc.)
			if iterable, ok := args[0].(core.Iterable); ok {
				iter := iterable.Iterator()
				i := 0
				for {
					item, hasNext := iter.Next()
					if !hasNext {
						break
					}
					// Item should be a pair (tuple or list)
					var key, value core.Value
					if pair, ok := item.(*core.ListValue); ok {
						if pair.Len() != 2 {
							return nil, fmt.Errorf("dict update sequence element #%d has length %d; 2 is required", i, pair.Len())
						}
						key = pair.Items()[0]
						value = pair.Items()[1]
					} else if tuple, ok := item.(core.TupleValue); ok {
						if len(tuple) != 2 {
							return nil, fmt.Errorf("dict update sequence element #%d has length %d; 2 is required", i, len(tuple))
						}
						key = tuple[0]
						value = tuple[1]
					} else {
						return nil, fmt.Errorf("dict update sequence element #%d is not a sequence", i)
					}
					if err := dict.SetValue(key, value); err != nil {
						return nil, fmt.Errorf("error setting dict key: %v", err)
					}
					i++
				}
				return dict, nil
			}

			return nil, &core.TypeError{Message: "dict() argument must be a dict or iterable of pairs"}
		}
		return dict, nil
	}

	// Odd number of args > 1
	return nil, fmt.Errorf("dict() takes an even number of positional arguments for key-value pairs")
}

// CallWithKeywords overrides the embedded Class's CallWithKeywords
// to ensure our custom Call is used instead of the generic class instantiation
func (d *DictType) CallWithKeywords(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
	// First create dict from positional arguments
	dict, err := d.Call(args, ctx)
	if err != nil {
		return nil, err
	}

	// Then add keyword arguments as key-value pairs
	// dict(a=1, b=2) creates {'a': 1, 'b': 2}
	if len(kwargs) > 0 {
		dictVal, ok := dict.(*core.DictValue)
		if !ok {
			return nil, fmt.Errorf("dict() Call did not return a DictValue")
		}
		for key, value := range kwargs {
			// Use SetValue to properly format the key with ValueToKey
			err := dictVal.SetValue(core.StringValue(key), value)
			if err != nil {
				return nil, fmt.Errorf("error setting dict kwarg %s: %v", key, err)
			}
		}
	}

	return dict, nil
}

// createByteArrayClass creates the bytearray class that can be used as a base class
func createByteArrayClass() *ByteArrayTypeClass {
	class := core.NewClass("bytearray", nil)

	// Add .copy unbound method that can be accessed as bytearray.copy
	class.SetMethod("copy", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("descriptor 'copy' of 'bytearray' object needs an argument")
		}

		// Get the bytearray instance
		baInst, ok := args[0].(*core.ByteArrayValue)
		if !ok {
			return nil, fmt.Errorf("descriptor 'copy' for 'bytearray' objects doesn't apply to '%s' object", args[0].Type())
		}

		// Return a shallow copy by using the .copy() instance method if it exists
		if copyMethod, ok := baInst.GetAttr("copy"); ok {
			if callable, ok := copyMethod.(core.Callable); ok {
				return callable.Call([]core.Value{}, ctx)
			}
		}

		// If no instance method, create a copy manually
		// Get the data and make a copy
		data := baInst.GetData()
		newData := make([]byte, len(data))
		copy(newData, data)
		return core.NewByteArray(newData), nil
	}))

	return &ByteArrayTypeClass{Class: class}
}

// createSetClass creates the set class that can be used as a base class
func createSetClass() *SetType {
	class := core.NewClass("set", nil)

	// Add .copy unbound method that can be accessed as set.copy
	class.SetMethod("copy", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("descriptor 'copy' of 'set' object needs an argument")
		}

		// Get the set instance
		setInst, ok := args[0].(*core.SetValue)
		if !ok {
			return nil, fmt.Errorf("descriptor 'copy' for 'set' objects doesn't apply to '%s' object", args[0].Type())
		}

		// Return a shallow copy by using the .copy() instance method
		if copyMethod, ok := setInst.GetAttr("copy"); ok {
			if callable, ok := copyMethod.(core.Callable); ok {
				return callable.Call([]core.Value{}, ctx)
			}
		}
		return nil, fmt.Errorf("set.copy method not found")
	}))

	return &SetType{Class: class}
}

// createListClass creates the list class that can be used as a base class
func createListClass() *ListType {
	// Use the global ListTypeClass which already has __new__ defined in types.go
	// Add .copy unbound method that can be accessed as list.copy
	// This returns a descriptor-like object that can be called
	ListTypeClass.SetMethod("copy", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("descriptor 'copy' of 'list' object needs an argument")
		}

		// Get the list instance
		listInst, ok := args[0].(*core.ListValue)
		if !ok {
			return nil, fmt.Errorf("descriptor 'copy' for 'list' objects doesn't apply to '%s' object", args[0].Type())
		}

		// Return a shallow copy by using the .copy() instance method
		if copyMethod, ok := listInst.GetAttr("copy"); ok {
			if callable, ok := copyMethod.(core.Callable); ok {
				return callable.Call([]core.Value{}, ctx)
			}
		}
		return nil, fmt.Errorf("list.copy method not found")
	}))

	return &ListType{Class: ListTypeClass}
}

// createDictClass creates the dict class with class methods like fromkeys
func createDictClass() *DictType {
	class := core.NewClass("dict", nil)

	// Add .copy unbound method that can be accessed as dict.copy
	class.SetMethod("copy", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("descriptor 'copy' of 'dict' object needs an argument")
		}

		// Get the dict instance
		dictInst, ok := args[0].(*core.DictValue)
		if !ok {
			return nil, fmt.Errorf("descriptor 'copy' for 'dict' objects doesn't apply to '%s' object", args[0].Type())
		}

		// Return a shallow copy by using the .copy() instance method
		// which is already implemented on DictValue
		if copyMethod, ok := dictInst.GetAttr("copy"); ok {
			if callable, ok := copyMethod.(core.Callable); ok {
				return callable.Call([]core.Value{}, ctx)
			}
		}
		return nil, fmt.Errorf("dict.copy method not found")
	}))

	// Add fromkeys class method
	// dict.fromkeys(iterable, value=None) -> new dict with keys from iterable
	class.SetMethod("fromkeys", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 || len(args) > 2 {
			return nil, fmt.Errorf("fromkeys() takes 1 or 2 positional arguments (%d given)", len(args))
		}

		iterable := args[0]
		var value core.Value = core.None
		if len(args) == 2 {
			value = args[1]
		}

		dict := core.NewDict()

		// Convert iterable to list of keys
		switch v := iterable.(type) {
		case *core.ListValue:
			for _, key := range v.Items() {
				keyStr, ok := key.(core.StringValue)
				if !ok {
					keyStr = core.StringValue(key.String())
				}
				dict.Set(string(keyStr), value)
			}
		case core.TupleValue:
			for _, key := range v {
				keyStr, ok := key.(core.StringValue)
				if !ok {
					keyStr = core.StringValue(key.String())
				}
				dict.Set(string(keyStr), value)
			}
		case core.StringValue:
			// String is iterable in Python - each character is a key
			for _, ch := range string(v) {
				dict.Set(string(ch), value)
			}
		default:
			return nil, fmt.Errorf("fromkeys() argument must be an iterable")
		}

		return dict, nil
	}))

	return &DictType{Class: class}
}
