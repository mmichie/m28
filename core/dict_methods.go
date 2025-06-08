package core

import "fmt"

// InitDictMethods adds additional methods to the dict type descriptor
func InitDictMethods() {
	dictType := GetTypeDescriptor("dict")
	if dictType == nil {
		panic("dict type not found in registry")
	}

	// Add update method
	dictType.Methods["update"] = &MethodDescriptor{
		Name:    "update",
		Arity:   1,
		Doc:     "Update dict with key/value pairs from another dict, returning new dict",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			dict := receiver.(*DictValue)
			other, ok := args[0].(*DictValue)
			if !ok {
				return nil, fmt.Errorf("update expects a dict, got %s", args[0].Type())
			}

			// Create new dict with updates
			newDict := NewDict()

			// Copy all from original
			for _, k := range dict.Keys() {
				v, _ := dict.Get(k)
				newDict.Set(k, v)
			}

			// Update/add from other
			for _, k := range other.Keys() {
				v, _ := other.Get(k)
				newDict.Set(k, v)
			}

			return newDict, nil
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

			return ListValue(items), nil
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
					// Fallback to string representation if original key not found
					keyValues[i] = StringValue(k)
				}
			}
			return ListValue(keyValues), nil
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

			return ListValue(values), nil
		},
	}

	// Add fromkeys class method (would need special handling)
	// For now, we'll skip this as it requires class method support
}
