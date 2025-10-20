package core

import (
	"fmt"
	"strings"
)

// registerDictType registers the dict type descriptor with all its methods
func registerDictType() {
	RegisterType(&TypeDescriptor{
		Name:       "dict",
		PythonName: "dict",
		BaseType:   DictType,
		Methods:    getDictMethods(),
		Constructor: func(args []Value, ctx *Context) (Value, error) {
			if len(args) == 0 {
				return NewDict(), nil
			}
			if len(args) == 1 {
				// Try to convert from another dict or iterable of pairs
				arg := args[0]
				if dict, ok := arg.(*DictValue); ok {
					// Make a copy
					result := NewDict()
					for k, v := range dict.entries {
						result.entries[k] = v
					}
					return result, nil
				}
				// TODO: Handle iterable of pairs
				return nil, fmt.Errorf("dict() argument must be a dict or iterable of pairs")
			}
			return nil, fmt.Errorf("dict() takes at most 1 argument (%d given)", len(args))
		},
		Repr: func(v Value) string {
			dict := v.(*DictValue)
			if dict.Size() == 0 {
				return "{}"
			}
			var items []string
			for k, val := range dict.entries {
				// Extract the actual key value from the string representation
				var keyRepr string
				if strings.HasPrefix(k, "s:") {
					keyRepr = fmt.Sprintf("%q", k[2:])
				} else if strings.HasPrefix(k, "n:") {
					keyRepr = k[2:]
				} else {
					keyRepr = k
				}
				items = append(items, fmt.Sprintf("%s: %s", keyRepr, Repr(val)))
			}
			return "{" + strings.Join(items, ", ") + "}"
		},
		Str: func(v Value) string {
			// Same as Repr for dicts
			dict := v.(*DictValue)
			if dict.Size() == 0 {
				return "{}"
			}
			var items []string
			for k, val := range dict.entries {
				// Extract the actual key value from the string representation
				var keyRepr string
				if strings.HasPrefix(k, "s:") {
					keyRepr = fmt.Sprintf("%q", k[2:])
				} else if strings.HasPrefix(k, "n:") {
					keyRepr = k[2:]
				} else {
					keyRepr = k
				}
				items = append(items, fmt.Sprintf("%s: %s", keyRepr, Repr(val)))
			}
			return "{" + strings.Join(items, ", ") + "}"
		},
		Doc: "dict() -> new empty dictionary\ndict(mapping) -> new dictionary initialized from a mapping object's (key, value) pairs",
	})
}

// getDictMethods returns all dict methods
func getDictMethods() map[string]*MethodDescriptor {
	return map[string]*MethodDescriptor{
		"get": {
			Name:    "get",
			Arity:   -1,
			Doc:     "Get value for key, or default if not present",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) < 1 || len(args) > 2 {
					return nil, fmt.Errorf("get() takes 1 or 2 arguments")
				}
				dict := receiver.(*DictValue)
				key := ValueToKey(args[0])
				if val, exists := dict.entries[key]; exists {
					return val, nil
				}
				if len(args) == 2 {
					return args[1], nil
				}
				return Nil, nil
			},
		},
		"keys": {
			Name:    "keys",
			Arity:   0,
			Doc:     "Return a list of dictionary keys",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				dict := receiver.(*DictValue)
				keys := make([]Value, 0, dict.Size())
				for k := range dict.entries {
					// Convert back from internal key representation
					if strings.HasPrefix(k, "s:") {
						keys = append(keys, StringValue(k[2:]))
					} else if strings.HasPrefix(k, "n:") {
						if f, ok := ParseFloat(k[2:]); ok {
							keys = append(keys, NumberValue(f))
						}
					}
					// TODO: Handle other key types
				}
				return NewList(keys...), nil
			},
		},
		"values": {
			Name:    "values",
			Arity:   0,
			Doc:     "Return a list of dictionary values",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				dict := receiver.(*DictValue)
				values := make([]Value, 0, dict.Size())
				for _, v := range dict.entries {
					values = append(values, v)
				}
				return NewList(values...), nil
			},
		},
		"items": {
			Name:    "items",
			Arity:   0,
			Doc:     "Return a list of (key, value) tuples",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				dict := receiver.(*DictValue)
				items := make([]Value, 0, dict.Size())
				for k, v := range dict.entries {
					// Convert back from internal key representation
					var key Value
					if strings.HasPrefix(k, "s:") {
						key = StringValue(k[2:])
					} else if strings.HasPrefix(k, "n:") {
						if f, ok := ParseFloat(k[2:]); ok {
							key = NumberValue(f)
						}
					}
					// Create a tuple for the key-value pair
					items = append(items, TupleValue{key, v})
				}
				return NewList(items...), nil
			},
		},
		"pop": {
			Name:    "pop",
			Arity:   -1,
			Doc:     "Remove and return value for key, or default",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) < 1 || len(args) > 2 {
					return nil, fmt.Errorf("pop() takes 1 or 2 arguments")
				}
				dict := receiver.(*DictValue)
				key := ValueToKey(args[0])
				if val, exists := dict.entries[key]; exists {
					// Remove the key and return the value
					delete(dict.entries, key)
					return val, nil
				}
				if len(args) == 2 {
					return args[1], nil
				}
				return nil, fmt.Errorf("KeyError: %v", args[0])
			},
		},
		"clear": {
			Name:    "clear",
			Arity:   0,
			Doc:     "Remove all items from dictionary",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				dict := receiver.(*DictValue)
				// Clear all entries from the dictionary
				dict.entries = make(map[string]Value)
				return Nil, nil
			},
		},
		"setdefault": {
			Name:    "setdefault",
			Arity:   -1,
			Doc:     "Get value for key, setting it to default if not present",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) < 1 || len(args) > 2 {
					return nil, fmt.Errorf("setdefault() takes 1 or 2 arguments")
				}
				dict := receiver.(*DictValue)
				key := ValueToKey(args[0])
				if val, exists := dict.entries[key]; exists {
					return val, nil
				}
				var defaultVal Value = Nil
				if len(args) == 2 {
					defaultVal = args[1]
				}
				// Set the key to the default value and return it
				dict.entries[key] = defaultVal
				return defaultVal, nil
			},
		},
		"update": {
			Name:    "update",
			Arity:   1,
			Doc:     "Update dictionary with key/value pairs from other",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("update() takes exactly 1 argument")
				}
				dict := receiver.(*DictValue)
				other, ok := args[0].(*DictValue)
				if !ok {
					return nil, fmt.Errorf("update() argument must be a dict")
				}
				// Update dict in place with entries from other
				for k, v := range other.entries {
					dict.entries[k] = v
				}
				return Nil, nil
			},
		},
		"copy": {
			Name:    "copy",
			Arity:   0,
			Doc:     "Return a shallow copy of the dictionary",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				dict := receiver.(*DictValue)
				result := NewDict()
				for k, v := range dict.entries {
					result.entries[k] = v
				}
				return result, nil
			},
		},
		"__len__": {
			Name:    "__len__",
			Arity:   0,
			Doc:     "Return the number of items in dictionary",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				dict := receiver.(*DictValue)
				return NumberValue(dict.Size()), nil
			},
		},
		"__getitem__": {
			Name:    "__getitem__",
			Arity:   1,
			Doc:     "Get item by key",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("__getitem__ takes exactly one argument")
				}
				dict := receiver.(*DictValue)
				key := ValueToKey(args[0])
				if val, exists := dict.entries[key]; exists {
					return val, nil
				}
				return nil, fmt.Errorf("KeyError: %v", args[0])
			},
		},
		"__setitem__": {
			Name:    "__setitem__",
			Arity:   2,
			Doc:     "Set item by key",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 2 {
					return nil, fmt.Errorf("__setitem__ takes exactly 2 arguments")
				}
				dict := receiver.(*DictValue)
				// Mutate the dictionary in place (matching original behavior)
				dict.entries[ValueToKey(args[0])] = args[1]
				return args[1], nil
			},
		},
		"__delitem__": {
			Name:    "__delitem__",
			Arity:   1,
			Doc:     "Delete item by key",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("__delitem__ takes exactly one argument")
				}
				dict := receiver.(*DictValue)
				key := ValueToKey(args[0])
				if _, exists := dict.entries[key]; !exists {
					return nil, fmt.Errorf("KeyError: %v", args[0])
				}
				// Delete the key from the dictionary in place
				delete(dict.entries, key)
				return Nil, nil
			},
		},
		"__contains__": {
			Name:    "__contains__",
			Arity:   1,
			Doc:     "Check if key is in dictionary",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("__contains__ takes exactly one argument")
				}
				dict := receiver.(*DictValue)
				key := ValueToKey(args[0])
				_, exists := dict.entries[key]
				return BoolValue(exists), nil
			},
		},
		"__iter__": {
			Name:    "__iter__",
			Arity:   0,
			Doc:     "Return an iterator over the keys",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				// For now, return the keys as a list
				dict := receiver.(*DictValue)
				keys := make([]Value, 0, dict.Size())
				for k := range dict.entries {
					// Convert back from internal key representation
					if strings.HasPrefix(k, "s:") {
						keys = append(keys, StringValue(k[2:]))
					} else if strings.HasPrefix(k, "n:") {
						if f, ok := ParseFloat(k[2:]); ok {
							keys = append(keys, NumberValue(f))
						}
					}
					// TODO: Handle other key types
				}
				return NewList(keys...), nil
			},
		},
	}
}
