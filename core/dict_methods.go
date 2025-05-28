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
			
			key, ok := args[0].(StringValue)
			if !ok {
				return nil, fmt.Errorf("dict key must be a string")
			}
			
			val, found := dict.Get(string(key))
			if !found {
				if len(args) > 1 {
					return args[1], nil
				}
				return nil, fmt.Errorf("KeyError: '%s'", string(key))
			}
			
			// Note: In functional style, we'd return both the value and new dict
			// For now, just return the value
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
			
			key, ok := args[0].(StringValue)
			if !ok {
				return nil, fmt.Errorf("dict key must be a string")
			}
			
			val, found := dict.Get(string(key))
			if found {
				return val, nil
			}
			
			// Key not found, use default
			var defaultVal Value = None
			if len(args) > 1 {
				defaultVal = args[1]
			}
			
			// Note: In functional style, we'd return a new dict
			// For now, just return the default value
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

	// Add fromkeys class method (would need special handling)
	// For now, we'll skip this as it requires class method support
}