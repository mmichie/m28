package core

import (
	"fmt"
	"strings"
)

// registerDictType registers the dict type descriptor
// Note: Methods are added separately by InitDictMethods() in dict_methods.go
func registerDictType() {
	RegisterType(&TypeDescriptor{
		Name:       "dict",
		PythonName: "dict",
		BaseType:   DictType,
		Methods:    make(map[string]*MethodDescriptor), // Empty - filled by InitDictMethods()
		Constructor: func(args []Value, ctx *Context) (Value, error) {
			if len(args) == 0 {
				return NewDict(), nil
			}
			if len(args) == 1 {
				// Try to convert from another dict or iterable of pairs
				arg := args[0]
				if dict, ok := arg.(*DictValue); ok {
					// Make a copy with proper key tracking
					result := NewDict()
					for _, k := range dict.Keys() {
						v, _ := dict.Get(k)
						// Copy original key tracking
						if origKey, exists := dict.keys[k]; exists {
							result.SetWithKey(k, origKey, v)
						} else {
							result.Set(k, v)
						}
					}
					return result, nil
				}

				// Handle iterable of pairs (list/tuple of 2-element sequences)
				var items []Value
				switch v := arg.(type) {
				case *ListValue:
					items = v.Items()
				case TupleValue:
					items = []Value(v)
				default:
					return nil, fmt.Errorf("dict() argument must be a dict or iterable of pairs")
				}

				result := NewDict()
				for i, item := range items {
					// Each item must be a 2-element sequence (key, value)
					var key, val Value
					switch pair := item.(type) {
					case TupleValue:
						if len(pair) != 2 {
							return nil, fmt.Errorf("dictionary update sequence element #%d has length %d; 2 is required", i, len(pair))
						}
						key, val = pair[0], pair[1]
					case *ListValue:
						pairItems := pair.Items()
						if len(pairItems) != 2 {
							return nil, fmt.Errorf("dictionary update sequence element #%d has length %d; 2 is required", i, len(pairItems))
						}
						key, val = pairItems[0], pairItems[1]
					default:
						return nil, fmt.Errorf("cannot convert dictionary update sequence element #%d to a sequence", i)
					}

					if !IsHashable(key) {
						return nil, fmt.Errorf("unhashable type: '%s'", key.Type())
					}
					if err := result.SetValue(key, val); err != nil {
						return nil, err
					}
				}
				return result, nil
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

// Note: All dict methods are now defined in dict_methods.go
// This file only handles type registration (constructor, repr, str)
