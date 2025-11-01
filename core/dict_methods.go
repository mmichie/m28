package core

import (
	"fmt"
	"log"
	"strconv"
)

// InitDictMethods adds additional methods to the dict type descriptor
func InitDictMethods() {
	dictType := GetTypeDescriptor("dict")
	if dictType == nil {
		log.Fatal("dict type not found in registry")
	}

	// Add update method
	dictType.Methods["update"] = &MethodDescriptor{
		Name:    "update",
		Arity:   -1, // 0 or 1 args
		Doc:     "Update dict with key/value pairs from another dict (mutates in place, returns None)",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			dict := receiver.(*DictValue)

			// If no args, just return None
			if len(args) == 0 {
				return None, nil
			}

			if len(args) > 1 {
				return nil, fmt.Errorf("update() takes at most 1 argument (%d given)", len(args))
			}

			other, ok := args[0].(*DictValue)
			if !ok {
				return nil, fmt.Errorf("update expects a dict, got %s", args[0].Type())
			}

			// Update dict in place by copying all entries from other
			for _, k := range other.Keys() {
				v, _ := other.Get(k)
				// Also copy the original key value
				if origKey, exists := other.keys[k]; exists {
					dict.SetWithKey(k, origKey, v)
				} else {
					dict.Set(k, v)
				}
			}

			// Return None (Python's dict.update() returns None)
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
				return nil, fmt.Errorf("__setitem__ takes exactly 2 arguments")
			}
			dict := receiver.(*DictValue)
			key := args[0]
			value := args[1]

			// Check if key is hashable
			if !IsHashable(key) {
				return nil, fmt.Errorf("unhashable type: '%s'", key.Type())
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
				return nil, fmt.Errorf("get() takes 1 or 2 arguments")
			}
			dict := receiver.(*DictValue)

			// Check if key is hashable
			if !IsHashable(args[0]) {
				return nil, fmt.Errorf("unhashable type: '%s'", args[0].Type())
			}

			key := ValueToKey(args[0])
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
				return nil, fmt.Errorf("pop expects 1 or 2 arguments")
			}

			// Check if key is hashable
			if !IsHashable(args[0]) {
				return nil, fmt.Errorf("unhashable type: '%s'", args[0].Type())
			}

			// Convert key to string representation
			keyStr := ValueToKey(args[0])

			// Try to get and remove the value
			val, found := dict.Get(keyStr)
			if !found {
				if len(args) > 1 {
					return args[1], nil
				}
				return nil, &KeyError{Key: args[0]}
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
				return nil, fmt.Errorf("setdefault expects 1 or 2 arguments")
			}

			// Check if key is hashable
			if !IsHashable(args[0]) {
				return nil, fmt.Errorf("unhashable type: '%s'", args[0].Type())
			}

			// Convert key to string representation
			keyStr := ValueToKey(args[0])

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
			dict.SetWithKey(keyStr, args[0], defaultVal)

			return defaultVal, nil
		},
	}

	// Add clear method
	dictType.Methods["clear"] = &MethodDescriptor{
		Name:    "clear",
		Arity:   0,
		Doc:     "Remove all items from dict (returns empty dict)",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			// Return a new empty dict
			return NewDict(), nil
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

			// Copy all key-value pairs
			for _, k := range dict.Keys() {
				v, _ := dict.Get(k)
				newDict.Set(k, v)
			}

			return newDict, nil
		},
	}

	// Add items method
	dictType.Methods["items"] = &MethodDescriptor{
		Name:    "items",
		Arity:   0,
		Doc:     "Return a list of (key, value) tuples",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			dict := receiver.(*DictValue)
			items := make([]Value, 0, dict.Size())

			// Create tuples for each key-value pair
			for _, k := range dict.Keys() {
				v, _ := dict.Get(k)
				// Get the original key value
				var keyVal Value
				if origKey, exists := dict.keys[k]; exists {
					keyVal = origKey
				} else {
					keyVal = StringValue(k)
				}
				pair := TupleValue{keyVal, v}
				items = append(items, pair)
			}

			return NewList(items...), nil
		},
	}

	// Add keys method
	dictType.Methods["keys"] = &MethodDescriptor{
		Name:    "keys",
		Arity:   0,
		Doc:     "Return a list of all keys",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			dict := receiver.(*DictValue)
			keys := dict.Keys()
			keyValues := make([]Value, len(keys))
			for i, k := range keys {
				// Get the original key value from the keys map
				if origKey, exists := dict.keys[k]; exists {
					keyValues[i] = origKey
				} else {
					// Fallback: reconstruct original key from internal representation
					// This handles cases where keys weren't tracked (legacy code paths)
					var cleanKey Value
					if len(k) > 2 && k[1] == ':' {
						// Has a type prefix like "s:", "i:", "f:", etc.
						prefix := k[0:2]
						keyStr := k[2:]
						switch prefix {
						case "s:":
							cleanKey = StringValue(keyStr)
						case "i:":
							// Try to parse back to int
							if i, err := strconv.ParseInt(keyStr, 10, 64); err == nil {
								cleanKey = NumberValue(i)
							} else {
								cleanKey = StringValue(keyStr)
							}
						case "f:":
							// Try to parse back to float
							if f, err := strconv.ParseFloat(keyStr, 64); err == nil {
								cleanKey = NumberValue(f)
							} else {
								cleanKey = StringValue(keyStr)
							}
						case "b:":
							cleanKey = BoolValue(keyStr == "true")
						case "n:":
							cleanKey = Nil
						default:
							cleanKey = StringValue(k)
						}
					} else {
						cleanKey = StringValue(k)
					}
					keyValues[i] = cleanKey
				}
			}
			return NewList(keyValues...), nil
		},
	}

	// Add values method
	dictType.Methods["values"] = &MethodDescriptor{
		Name:    "values",
		Arity:   0,
		Doc:     "Return a list of all values",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			dict := receiver.(*DictValue)
			values := make([]Value, 0, dict.Size())

			for _, k := range dict.Keys() {
				v, _ := dict.Get(k)
				values = append(values, v)
			}

			return NewList(values...), nil
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
				return nil, fmt.Errorf("__getitem__ takes exactly 1 argument")
			}
			dict := receiver.(*DictValue)
			key := ValueToKey(args[0])
			if val, exists := dict.Get(key); exists {
				return val, nil
			}
			return nil, &KeyError{Key: args[0]}
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
				return nil, fmt.Errorf("__delitem__ takes exactly 1 argument")
			}
			dict := receiver.(*DictValue)
			key := ValueToKey(args[0])
			if _, exists := dict.Get(key); !exists {
				return nil, &KeyError{Key: args[0]}
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
				return nil, fmt.Errorf("__contains__ takes exactly 1 argument")
			}
			dict := receiver.(*DictValue)
			key := ValueToKey(args[0])
			_, exists := dict.Get(key)
			return BoolValue(exists), nil
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
			// Create an iterator that returns keys
			keys := dict.Keys()
			keyValues := make([]Value, len(keys))
			for i, k := range keys {
				// Get the original key value from the keys map
				if origKey, exists := dict.keys[k]; exists {
					keyValues[i] = origKey
				} else {
					// Fallback: reconstruct original key
					if len(k) > 2 && k[1] == ':' {
						keyValues[i] = StringValue(k[2:])
					} else {
						keyValues[i] = StringValue(k)
					}
				}
			}
			// Create a list and call __iter__ on it to get a proper iterator
			keysList := NewList(keyValues...)
			if iter, ok := keysList.GetAttr("__iter__"); ok {
				if callable, ok := iter.(interface {
					Call([]Value, *Context) (Value, error)
				}); ok {
					return callable.Call([]Value{}, ctx)
				}
			}
			// Fallback: return the list if __iter__ not found
			return keysList, nil
		},
	}

	// Add __or__ method (dict | other)
	dictType.Methods["__or__"] = &MethodDescriptor{
		Name:    "__or__",
		Arity:   1,
		Doc:     "Return a new dict with items from both dicts (d1 | d2)",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("__or__ takes exactly 1 argument")
			}
			dict := receiver.(*DictValue)
			other, ok := args[0].(*DictValue)
			if !ok {
				return nil, fmt.Errorf("unsupported operand type(s) for |: 'dict' and '%s'", args[0].Type())
			}

			// Create a new dict with all entries from self
			result := NewDict()
			for _, k := range dict.Keys() {
				v, _ := dict.Get(k)
				if origKey, exists := dict.keys[k]; exists {
					result.SetWithKey(k, origKey, v)
				} else {
					result.Set(k, v)
				}
			}

			// Add/override with entries from other
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

	// Add __ior__ method (dict |= other)
	dictType.Methods["__ior__"] = &MethodDescriptor{
		Name:    "__ior__",
		Arity:   1,
		Doc:     "Update dict with items from other (d1 |= d2)",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("__ior__ takes exactly 1 argument")
			}
			dict := receiver.(*DictValue)
			other, ok := args[0].(*DictValue)
			if !ok {
				return nil, fmt.Errorf("unsupported operand type(s) for |=: 'dict' and '%s'", args[0].Type())
			}

			// Update dict in place with entries from other
			for _, k := range other.Keys() {
				v, _ := other.Get(k)
				if origKey, exists := other.keys[k]; exists {
					dict.SetWithKey(k, origKey, v)
				} else {
					dict.Set(k, v)
				}
			}

			return dict, nil
		},
	}

	// Add fromkeys class method (would need special handling)
	// For now, we'll skip this as it requires class method support
}
