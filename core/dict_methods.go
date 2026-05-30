package core

import (
	"fmt"
	"log"
	"unsafe"
)

// iterableToSliceForDict converts any iterable value (list, tuple, set, string,
// generator) into a []Value so dict.fromkeys can iterate it.
func iterableToSliceForDict(v Value) ([]Value, error) {
	// Dicts iterate over keys (using OriginalKeyValue to get the real key).
	if d, ok := v.(*DictValue); ok {
		out := make([]Value, 0, d.Size())
		for _, k := range d.Keys() {
			if origKey, ok := d.keys[k]; ok {
				out = append(out, origKey)
			}
		}
		return out, nil
	}
	// Dict-subclass instances delegate to their backing dict.
	if inst, ok := v.(*Instance); ok && inst.BackingDict != nil {
		out := make([]Value, 0, inst.BackingDict.Size())
		for _, k := range inst.BackingDict.Keys() {
			if origKey, ok := inst.BackingDict.keys[k]; ok {
				out = append(out, origKey)
			}
		}
		return out, nil
	}
	return iterableValues(v)
}

// InstanceNeedsEqComparison is the exported version of instanceNeedsEqComparison.
func InstanceNeedsEqComparison(v Value) bool {
	return instanceNeedsEqComparison(v)
}

// ComputeInstanceKey is the exported version of computeInstanceKey.
func ComputeInstanceKey(v *Instance, ctx *Context) (string, error) {
	return computeInstanceKey(v, ctx)
}

// instanceNeedsEqComparison checks if an instance has a custom __hash__ that
// could cause hash collisions requiring __eq__ comparison
func instanceNeedsEqComparison(v Value) bool {
	inst, ok := v.(*Instance)
	if !ok {
		return false
	}
	if inst.Class == nil {
		return false
	}
	// Check if class defines __hash__ (not inherited from object)
	_, hasOwnHash := inst.Class.Methods["__hash__"]
	return hasOwnHash
}

// computeInstanceKey calls __hash__ on an instance that defines it, then
// returns a string key derived from the hash value. Propagates exceptions.
func computeInstanceKey(v *Instance, ctx *Context) (string, error) {
	hashAttr, found := v.GetAttr("__hash__")
	if !found {
		return ValueToKey(v), nil
	}
	callable, ok := hashAttr.(interface {
		Call([]Value, *Context) (Value, error)
	})
	if !ok {
		return ValueToKey(v), nil
	}
	result, err := callable.Call(nil, ctx)
	if err != nil {
		return "", err
	}
	if num, ok := result.(NumberValue); ok {
		return fmt.Sprintf("p:%d", int64(float64(num))), nil
	}
	return ValueToKey(v), nil
}

// dictFindKeyWithEq finds a key in the dict by comparing with __eq__
// Returns the internal key string, the value, found bool, and any error from __eq__
func dictFindKeyWithEq(dict *DictValue, searchKey Value, ctx *Context) (string, Value, bool, error) {
	// For instances with custom __hash__, we need to compare with all stored keys
	// using __eq__ because they might have the same hash
	for internalKey, storedKey := range dict.keys {
		// Use EqualValuesWithError to properly propagate __eq__ exceptions
		equal, err := EqualValuesWithError(storedKey, searchKey, ctx)
		if err != nil {
			// Propagate exception from __eq__
			return "", nil, false, err
		}
		if equal {
			val, _ := dict.Get(internalKey)
			return internalKey, val, true, nil
		}
	}
	return "", nil, false, nil
}

// InitDictMethods adds additional methods to the dict type descriptor
func InitDictMethods() {
	dictType := GetTypeDescriptor("dict")
	if dictType == nil {
		log.Fatal("dict type not found in registry")
	}

	// Add update method
	dictUpdatePositional := func(receiver Value, args []Value, ctx *Context) (Value, error) {
			dict := receiver.(*DictValue)

			if len(args) == 0 {
				return None, nil
			}
			if len(args) > 1 {
				return nil, &TypeError{Message: fmt.Sprintf("update() takes at most 1 argument (%d given)", len(args))}
			}

			// Real dict — fast path that preserves original key values.
			if other, ok := args[0].(*DictValue); ok {
				for _, k := range other.Keys() {
					v, _ := other.Get(k)
					origKey, exists := other.keys[k]
					if !exists {
						origKey = StringValue(k)
					}
					if instanceNeedsEqComparison(origKey) {
						internalKey, _, found, err := dictFindKeyWithEq(dict, origKey, ctx)
						if err != nil {
							return nil, err
						}
						if found {
							dict.SetWithKey(internalKey, origKey, v)
						} else {
							dict.SetWithKey(k, origKey, v)
						}
					} else {
						dict.SetWithKey(k, origKey, v)
					}
				}
				return None, nil
			}

			// Dict subclass instance (BackingDict).
			if inst, ok := args[0].(*Instance); ok && inst.BackingDict != nil {
				for _, k := range inst.BackingDict.Keys() {
					v, _ := inst.BackingDict.Get(k)
					if orig, ok := inst.BackingDict.keys[k]; ok {
						dict.SetWithKey(k, orig, v)
					} else {
						dict.Set(k, v)
					}
				}
				return None, nil
			}

			// Mapping protocol: anything with .keys() is treated as a mapping;
			// values come from .__getitem__(k). Per CPython, .keys() runs
			// before __getitem__ is required, so its exceptions propagate.
			if obj, ok := args[0].(interface{ GetAttr(string) (Value, bool) }); ok {
				if keysAttr, found := obj.GetAttr("keys"); found {
					if keysCall, ok := keysAttr.(interface {
						Call([]Value, *Context) (Value, error)
					}); ok {
						keys, err := keysCall.Call(nil, ctx)
						if err != nil {
							return nil, err
						}
						items, err := iterableValuesCtx(keys, ctx)
						if err != nil {
							return nil, err
						}
						getitemAttr, hasGetitem := obj.GetAttr("__getitem__")
						getitemCall, _ := getitemAttr.(interface {
							Call([]Value, *Context) (Value, error)
						})
						if !hasGetitem || getitemCall == nil {
							return nil, &AttributeError{ObjType: string(args[0].Type()), Message: fmt.Sprintf("'%s' object has no attribute '__getitem__'", args[0].Type())}
						}
						for _, k := range items {
							v, err := getitemCall.Call([]Value{k}, ctx)
							if err != nil {
								return nil, err
							}
							if err := dict.SetValue(k, v); err != nil {
								return nil, err
							}
						}
						return None, nil
					}
				}
			}

			// Iterable of 2-element sequences.
			items, err := iterableValuesCtx(args[0], ctx)
			if err != nil {
				return nil, err
			}
			for i, item := range items {
				if pair, ok := item.(TupleValue); ok && len(pair) == 2 {
					if err := dict.SetValue(pair[0], pair[1]); err != nil {
						return nil, err
					}
					continue
				}
				if lst, ok := item.(*ListValue); ok && lst.Len() == 2 {
					if err := dict.SetValue(lst.Items()[0], lst.Items()[1]); err != nil {
						return nil, err
					}
					continue
				}
				return nil, &ValueError{Message: fmt.Sprintf("dictionary update sequence element #%d has length %d; 2 is required", i, sequenceLength(item))}
			}

			return None, nil
	}
	dictType.Methods["update"] = &MethodDescriptor{
		Name:    "update",
		Arity:   -1,
		Doc:     "Update dict with key/value pairs from another dict, mapping, or iterable of pairs, plus keyword args (mutates in place, returns None)",
		Builtin: true,
		Handler: dictUpdatePositional,
		KwargHandler: func(receiver Value, args []Value, kwargs map[string]Value, ctx *Context) (Value, error) {
			if _, err := dictUpdatePositional(receiver, args, ctx); err != nil {
				return nil, err
			}
			dict := receiver.(*DictValue)
			for k, v := range kwargs {
				if err := dict.SetValue(StringValue(k), v); err != nil {
					return nil, err
				}
			}
			return None, nil
		},
	}

	// Add __setitem__ method (critical for dict[key] = value assignments)
	dictType.Methods["__setitem__"] = &MethodDescriptor{
		Name:    "__setitem__",
		Arity:   2,
		Doc:     "Set item by key (d[k] = v)",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) != 2 {
				return nil, &TypeError{Message: "__setitem__ takes exactly 2 arguments"}
			}
			dict := receiver.(*DictValue)
			key := args[0]
			value := args[1]

			// Check if key is hashable
			if !IsHashable(key) {
				return nil, &TypeError{Message: fmt.Sprintf("unhashable type: '%s'", key.Type())}
			}

			// For instances with custom __hash__, call __hash__ first (may raise),
			// then check for existing key with __eq__
			if inst, ok := key.(*Instance); ok && instanceNeedsEqComparison(key) {
				if _, err := computeInstanceKey(inst, ctx); err != nil {
					return nil, err
				}
				internalKey, _, found, err := dictFindKeyWithEq(dict, key, ctx)
				if err != nil {
					return nil, err // Propagate __eq__ exception
				}
				if found {
					// Update existing key
					dict.SetWithKey(internalKey, key, value)
					return value, nil
				}
			}

			// Use SetWithKey to properly track the original key
			keyRepr := ValueToKey(key)
			dict.SetWithKey(keyRepr, key, value)
			return value, nil
		},
	}

	// Add get method
	dictType.Methods["get"] = &MethodDescriptor{
		Name:    "get",
		Arity:   -1, // 1 or 2 args
		Doc:     "Get value for key, or default if not present",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) < 1 || len(args) > 2 {
				return nil, &TypeError{Message: "get() takes 1 or 2 arguments"}
			}
			dict := receiver.(*DictValue)
			searchKey := args[0]

			// Check if key is hashable
			if !IsHashable(searchKey) {
				return nil, &TypeError{Message: fmt.Sprintf("unhashable type: '%s'", searchKey.Type())}
			}

			// For instances with custom __hash__, call __hash__ first then use __eq__
			if inst, ok := searchKey.(*Instance); ok && instanceNeedsEqComparison(searchKey) {
				if _, err := computeInstanceKey(inst, ctx); err != nil {
					return nil, err
				}
				_, val, found, err := dictFindKeyWithEq(dict, searchKey, ctx)
				if err != nil {
					return nil, err // Propagate __eq__ exception
				}
				if found {
					return val, nil
				}
				if len(args) == 2 {
					return args[1], nil
				}
				return Nil, nil
			}

			key := ValueToKey(searchKey)
			if val, exists := dict.Get(key); exists {
				return val, nil
			}
			if len(args) == 2 {
				return args[1], nil
			}
			return Nil, nil
		},
	}

	// Add pop method
	dictType.Methods["pop"] = &MethodDescriptor{
		Name:    "pop",
		Arity:   -1, // 1 or 2 args
		Doc:     "Remove key and return its value. If key not found, return default or raise error",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			dict := receiver.(*DictValue)
			if len(args) < 1 || len(args) > 2 {
				return nil, &TypeError{Message: "pop expects 1 or 2 arguments"}
			}

			searchKey := args[0]

			// Check if key is hashable
			if !IsHashable(searchKey) {
				return nil, &TypeError{Message: fmt.Sprintf("unhashable type: '%s'", searchKey.Type())}
			}

			// For instances with custom __hash__, call __hash__ first (may raise),
			// then use __eq__ comparison
			if inst, ok := searchKey.(*Instance); ok && instanceNeedsEqComparison(searchKey) {
				if _, err := computeInstanceKey(inst, ctx); err != nil {
					return nil, err
				}
				internalKey, val, found, err := dictFindKeyWithEq(dict, searchKey, ctx)
				if err != nil {
					return nil, err // Propagate __eq__ exception
				}
				if found {
					dict.Delete(internalKey)
					return val, nil
				}
				if len(args) > 1 {
					return args[1], nil
				}
				return nil, &KeyError{Key: searchKey}
			}

			// Convert key to string representation
			keyStr := ValueToKey(searchKey)

			// Try to get and remove the value
			val, found := dict.Get(keyStr)
			if !found {
				if len(args) > 1 {
					return args[1], nil
				}
				return nil, &KeyError{Key: searchKey}
			}

			// Remove the key from the dictionary
			dict.Delete(keyStr)

			return val, nil
		},
	}

	// Add setdefault method
	dictType.Methods["setdefault"] = &MethodDescriptor{
		Name:    "setdefault",
		Arity:   -1, // 1 or 2 args
		Doc:     "Get value of key. If key doesn't exist, set it to default and return default",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			dict := receiver.(*DictValue)
			if len(args) < 1 || len(args) > 2 {
				return nil, &TypeError{Message: "setdefault expects 1 or 2 arguments"}
			}

			searchKey := args[0]

			// Check if key is hashable
			if !IsHashable(searchKey) {
				return nil, &TypeError{Message: fmt.Sprintf("unhashable type: '%s'", searchKey.Type())}
			}

			// For instances with custom __hash__, call __hash__ first (may raise),
			// then use __eq__ comparison
			if inst, ok := searchKey.(*Instance); ok && instanceNeedsEqComparison(searchKey) {
				if _, err := computeInstanceKey(inst, ctx); err != nil {
					return nil, err
				}
				_, val, found, err := dictFindKeyWithEq(dict, searchKey, ctx)
				if err != nil {
					return nil, err // Propagate __eq__ exception
				}
				if found {
					return val, nil
				}
				// Key not found, set default
				var defaultVal Value = None
				if len(args) > 1 {
					defaultVal = args[1]
				}
				keyStr := ValueToKey(searchKey)
				dict.SetWithKey(keyStr, searchKey, defaultVal)
				return defaultVal, nil
			}

			// Convert key to string representation
			keyStr := ValueToKey(searchKey)

			val, found := dict.Get(keyStr)
			if found {
				return val, nil
			}

			// Key not found, use default
			var defaultVal Value = None
			if len(args) > 1 {
				defaultVal = args[1]
			}

			// Set the default value in the dictionary
			dict.SetWithKey(keyStr, searchKey, defaultVal)

			return defaultVal, nil
		},
	}

	// Add clear method
	dictType.Methods["clear"] = &MethodDescriptor{
		Name:    "clear",
		Arity:   0,
		Doc:     "Remove all items from dict (mutates in-place, returns None)",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			dict := receiver.(*DictValue)
			// Clear all entries in-place
			keys := dict.Keys()
			for _, key := range keys {
				dict.Delete(key)
			}
			// Return None (Python's dict.clear() returns None)
			return Nil, nil
		},
	}

	// Add popitem method
	dictType.Methods["popitem"] = &MethodDescriptor{
		Name:    "popitem",
		Arity:   0,
		Doc:     "Remove and return an arbitrary (key, value) pair as a tuple. Raises KeyError if dict is empty",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) != 0 {
				return nil, &TypeError{Message: fmt.Sprintf("popitem() takes no arguments (%d given)", len(args))}
			}
			dict := receiver.(*DictValue)

			keys := dict.Keys()
			if len(keys) == 0 {
				return nil, &KeyError{Key: StringValue("dictionary is empty"), Message: "popitem(): dictionary is empty"}
			}

			// Get the last key (LIFO behavior like Python 3.7+)
			keyStr := keys[len(keys)-1]

			// Get the value
			val, _ := dict.Get(keyStr)

			// Get the original key object from dict.keys map
			var keyVal Value
			if dict.keys != nil {
				if origKey, ok := dict.keys[keyStr]; ok {
					keyVal = origKey
				} else {
					// Fall back to string key
					keyVal = StringValue(keyStr)
				}
			} else {
				keyVal = StringValue(keyStr)
			}

			// Remove the key from the dictionary
			dict.Delete(keyStr)

			// Return (key, value) as a tuple
			return TupleValue([]Value{keyVal, val}), nil
		},
	}

	// Add copy method
	dictType.Methods["copy"] = &MethodDescriptor{
		Name:    "copy",
		Arity:   0,
		Doc:     "Return a shallow copy of the dict",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			dict := receiver.(*DictValue)
			newDict := NewDict()
			for _, k := range dict.Keys() {
				v, _ := dict.Get(k)
				if origKey, exists := dict.keys[k]; exists {
					newDict.SetWithKey(k, origKey, v)
				} else {
					newDict.Set(k, v)
				}
			}
			return newDict, nil
		},
	}

	// fromkeys is a classmethod in Python — d.fromkeys(iter, value=None) builds
	// a new dict with each item of iter as a key, all sharing `value`. We accept
	// the call on both the class and any instance.
	dictType.Methods["fromkeys"] = &MethodDescriptor{
		Name:    "fromkeys",
		Arity:   -1,
		Doc:     "Create a new dict with keys from iterable and values set to value.",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) < 1 {
				return nil, &TypeError{Message: "fromkeys() requires at least 1 argument"}
			}
			var defaultValue Value = None
			if len(args) >= 2 {
				defaultValue = args[1]
			}
			newDict := NewDict()
			items, err := iterableToSliceForDict(args[0])
			if err != nil {
				return nil, err
			}
			for _, k := range items {
				if err := newDict.SetValue(k, defaultValue); err != nil {
					return nil, err
				}
			}
			return newDict, nil
		},
	}

	// Add items method - returns a dict_items view
	dictType.Methods["items"] = &MethodDescriptor{
		Name:    "items",
		Arity:   0,
		Doc:     "Return a new view of the dictionary's items (key, value) pairs",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) != 0 {
				return nil, &TypeError{Message: fmt.Sprintf("items() takes no arguments (%d given)", len(args))}
			}
			return NewDictView(receiver.(*DictValue), DictItemsViewKind), nil
		},
	}

	// Add keys method - returns a dict_keys view
	dictType.Methods["keys"] = &MethodDescriptor{
		Name:    "keys",
		Arity:   0,
		Doc:     "Return a new view of the dictionary's keys",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) != 0 {
				return nil, &TypeError{Message: fmt.Sprintf("keys() takes no arguments (%d given)", len(args))}
			}
			return NewDictView(receiver.(*DictValue), DictKeysViewKind), nil
		},
	}

	// Add values method - returns a dict_values view
	dictType.Methods["values"] = &MethodDescriptor{
		Name:    "values",
		Arity:   0,
		Doc:     "Return a new view of the dictionary's values",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) != 0 {
				return nil, &TypeError{Message: fmt.Sprintf("values() takes no arguments (%d given)", len(args))}
			}
			return NewDictView(receiver.(*DictValue), DictValuesViewKind), nil
		},
	}

	// Dunder methods for operator support

	// Add __len__ method
	dictType.Methods["__len__"] = &MethodDescriptor{
		Name:    "__len__",
		Arity:   0,
		Doc:     "Return the number of items in the dictionary",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			dict := receiver.(*DictValue)
			return NumberValue(dict.Size()), nil
		},
	}

	// Add __getitem__ method
	dictType.Methods["__getitem__"] = &MethodDescriptor{
		Name:    "__getitem__",
		Arity:   1,
		Doc:     "Get item by key (d[k])",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, &TypeError{Message: "__getitem__ takes exactly 1 argument"}
			}
			dict := receiver.(*DictValue)
			searchKey := args[0]

			// For instances with custom __hash__, call __hash__ first then use __eq__
			if inst, ok := searchKey.(*Instance); ok && instanceNeedsEqComparison(searchKey) {
				if _, err := computeInstanceKey(inst, ctx); err != nil {
					return nil, err
				}
				_, val, found, err := dictFindKeyWithEq(dict, searchKey, ctx)
				if err != nil {
					return nil, err // Propagate __eq__ exception
				}
				if found {
					return val, nil
				}
				return nil, &KeyError{Key: searchKey}
			}

			key := ValueToKey(searchKey)
			if val, exists := dict.Get(key); exists {
				return val, nil
			}
			return nil, &KeyError{Key: searchKey}
		},
	}

	// Add __delitem__ method
	dictType.Methods["__delitem__"] = &MethodDescriptor{
		Name:    "__delitem__",
		Arity:   1,
		Doc:     "Delete item by key (del d[k])",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, &TypeError{Message: "__delitem__ takes exactly 1 argument"}
			}
			dict := receiver.(*DictValue)
			searchKey := args[0]

			// For instances with custom __hash__, call __hash__ first then use __eq__
			if inst, ok := searchKey.(*Instance); ok && instanceNeedsEqComparison(searchKey) {
				if _, err := computeInstanceKey(inst, ctx); err != nil {
					return nil, err
				}
				internalKey, _, found, err := dictFindKeyWithEq(dict, searchKey, ctx)
				if err != nil {
					return nil, err // Propagate __eq__ exception
				}
				if !found {
					return nil, &KeyError{Key: searchKey}
				}
				dict.Delete(internalKey)
				delete(dict.keys, internalKey)
				return None, nil
			}

			key := ValueToKey(searchKey)
			if _, exists := dict.Get(key); !exists {
				return nil, &KeyError{Key: searchKey}
			}
			dict.Delete(key)
			// Also remove from keys map
			delete(dict.keys, key)
			return None, nil
		},
	}

	// Add __contains__ method
	dictType.Methods["__contains__"] = &MethodDescriptor{
		Name:    "__contains__",
		Arity:   1,
		Doc:     "Check if key is in dictionary (k in d)",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, &TypeError{Message: "__contains__ takes exactly 1 argument"}
			}
			dict := receiver.(*DictValue)
			searchKey := args[0]

			// For instances with custom __hash__, call __hash__ first then use __eq__
			if inst, ok := searchKey.(*Instance); ok && instanceNeedsEqComparison(searchKey) {
				if _, err := computeInstanceKey(inst, ctx); err != nil {
					return nil, err
				}
				_, _, found, err := dictFindKeyWithEq(dict, searchKey, ctx)
				if err != nil {
					return nil, err // Propagate __eq__ exception
				}
				return BoolValue(found), nil
			}

			key := ValueToKey(searchKey)
			_, exists := dict.Get(key)
			return BoolValue(exists), nil
		},
	}

	// Add __reversed__ method
	dictType.Methods["__reversed__"] = &MethodDescriptor{
		Name:    "__reversed__",
		Arity:   0,
		Doc:     "Return a reverse iterator over the dictionary keys",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			dict := receiver.(*DictValue)
			keys := dict.OriginalKeys()
			reversed := make([]Value, len(keys))
			for i, k := range keys {
				reversed[len(keys)-1-i] = k
			}
			return &dictViewIterator{items: reversed, index: 0, dict: nil, startMod: 0}, nil
		},
	}

	// Add __iter__ method
	dictType.Methods["__iter__"] = &MethodDescriptor{
		Name:    "__iter__",
		Arity:   0,
		Doc:     "Return an iterator over the dictionary keys",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			dict := receiver.(*DictValue)
			return dict.Iterator().(*dictIterator), nil
		},
	}

	// Add __or__ method (dict | other)
	// Per Python, only dict on the RHS is accepted; everything else returns
	// NotImplemented so the interpreter can try other.__ror__.
	dictType.Methods["__or__"] = &MethodDescriptor{
		Name:    "__or__",
		Arity:   1,
		Doc:     "Return a new dict with items from both dicts (d1 | d2)",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, &TypeError{Message: "__or__ takes exactly 1 argument"}
			}
			dict := receiver.(*DictValue)
			other, ok := args[0].(*DictValue)
			if !ok {
				return NotImplemented, nil
			}

			result := NewDict()
			for _, k := range dict.Keys() {
				v, _ := dict.Get(k)
				if origKey, exists := dict.keys[k]; exists {
					result.SetWithKey(k, origKey, v)
				} else {
					result.Set(k, v)
				}
			}

			for _, k := range other.Keys() {
				v, _ := other.Get(k)
				if origKey, exists := other.keys[k]; exists {
					result.SetWithKey(k, origKey, v)
				} else {
					result.Set(k, v)
				}
			}

			return result, nil
		},
	}

	// Add __ror__ method so that 'other | dict' delegates correctly.
	dictType.Methods["__ror__"] = &MethodDescriptor{
		Name:    "__ror__",
		Arity:   1,
		Doc:     "Return a new dict with items from other and self (other | d)",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, &TypeError{Message: "__ror__ takes exactly 1 argument"}
			}
			dict := receiver.(*DictValue)
			other, ok := args[0].(*DictValue)
			if !ok {
				return NotImplemented, nil
			}
			result := NewDict()
			for _, k := range other.Keys() {
				v, _ := other.Get(k)
				if origKey, exists := other.keys[k]; exists {
					result.SetWithKey(k, origKey, v)
				} else {
					result.Set(k, v)
				}
			}
			for _, k := range dict.Keys() {
				v, _ := dict.Get(k)
				if origKey, exists := dict.keys[k]; exists {
					result.SetWithKey(k, origKey, v)
				} else {
					result.Set(k, v)
				}
			}
			return result, nil
		},
	}

	// Add __ior__ method (dict |= other) — accepts any dict or iterable of pairs.
	dictType.Methods["__ior__"] = &MethodDescriptor{
		Name:    "__ior__",
		Arity:   1,
		Doc:     "Update dict with items from other (d1 |= d2)",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, &TypeError{Message: "__ior__ takes exactly 1 argument"}
			}
			dict := receiver.(*DictValue)
			if other, ok := args[0].(*DictValue); ok {
				for _, k := range other.Keys() {
					v, _ := other.Get(k)
					if origKey, exists := other.keys[k]; exists {
						dict.SetWithKey(k, origKey, v)
					} else {
						dict.Set(k, v)
					}
				}
				return dict, nil
			}
			// Iterable of pairs.
			items, err := iterableValues(args[0])
			if err != nil {
				return nil, &TypeError{Message: fmt.Sprintf("unsupported operand type(s) for |=: 'dict' and '%s'", args[0].Type())}
			}
			for i, item := range items {
				if pair, ok := item.(TupleValue); ok && len(pair) == 2 {
					dict.SetValue(pair[0], pair[1])
					continue
				}
				if lst, ok := item.(*ListValue); ok && lst.Len() == 2 {
					dict.SetValue(lst.Items()[0], lst.Items()[1])
					continue
				}
				length := -1
				if pair, ok := item.(TupleValue); ok {
					length = len(pair)
				} else if lst, ok := item.(*ListValue); ok {
					length = lst.Len()
				}
				return nil, &ValueError{Message: fmt.Sprintf("dictionary update sequence element #%d has length %d; 2 is required", i, length)}
			}
			return dict, nil
		},
	}

	// Add __repr__ method - propagates exceptions from values' __repr__
	dictType.Methods["__repr__"] = &MethodDescriptor{
		Name:    "__repr__",
		Arity:   0,
		Doc:     "Return a string representation of the dict",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			dict := receiver.(*DictValue)
			s, err := dictReprWithCtx(dict, ctx)
			if err != nil {
				return nil, err
			}
			return StringValue(s), nil
		},
	}
}

const maxDictReprDepth = 200

// dictReprWithCtx produces repr(d) while propagating any exceptions from
// __repr__ methods of keys or values.
func dictReprWithCtx(dict *DictValue, ctx *Context) (string, error) {
	return dictReprWithDepth(dict, ctx, 0)
}

func dictReprWithDepth(dict *DictValue, ctx *Context, depth int) (string, error) {
	if depth > maxDictReprDepth {
		return "", &RecursionError{Message: "maximum recursion depth exceeded while getting the repr of an object"}
	}
	if len(dict.orderedKeys) == 0 {
		return "{}", nil
	}
	s, isCycle, err := withCycleDetectionErr(uintptr(unsafe.Pointer(dict)), "{...}", func() (string, error) {
		result := "{"
		first := true
		for _, k := range dict.orderedKeys {
			v, exists := dict.entries[k]
			if !exists {
				continue
			}
			origKey, _ := dict.keys[k]
			if origKey == nil {
				origKey = StringValue(k)
			}
			if !first {
				result += ", "
			}
			first = false
			keyRepr, err := reprWithDepth(origKey, ctx, depth+1)
			if err != nil {
				return "", err
			}
			valRepr, err := reprWithDepth(v, ctx, depth+1)
			if err != nil {
				return "", err
			}
			result += keyRepr + ": " + valRepr
		}
		return result + "}", nil
	})
	if isCycle || err != nil {
		return s, err
	}
	return s, nil
}

// reprWithCtx calls __repr__ on val with the given context, propagating errors.
// Only calls __repr__ for Python class instances; built-in types use Repr().
func reprWithCtx(val Value, ctx *Context) (string, error) {
	return reprWithDepth(val, ctx, 0)
}

func reprWithDepth(val Value, ctx *Context, depth int) (string, error) {
	if val == nil {
		return "None", nil
	}
	// For nested dicts, recurse through dictReprWithDepth to propagate errors.
	if d, ok := val.(*DictValue); ok {
		return dictReprWithDepth(d, ctx, depth)
	}
	// Only use context-aware repr for Python instances (not built-in types).
	// Built-in types (list, etc.) use Repr() which has cycle detection.
	if inst, ok := val.(*Instance); ok {
		if reprMethod, found := inst.GetAttr("__repr__"); found {
			if callable, ok := reprMethod.(interface {
				Call([]Value, *Context) (Value, error)
			}); ok {
				result, err := callable.Call(nil, ctx)
				if err != nil {
					return "", err
				}
				if s, ok := result.(StringValue); ok {
					return string(s), nil
				}
			}
		}
	}
	return Repr(val), nil
}
