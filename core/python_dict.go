package core

import (
	"fmt"
	"sort"
	"sync"
)

// DictMethod represents a method function for dictionaries
type DictMethod func(self *PythonicDict, args []LispValue) (LispValue, error)

type PythonicDict struct {
	data      map[LispValue]LispValue
	mu        sync.RWMutex
	methods   map[string]DictMethod
	evaluator Evaluator // Store evaluator reference for method calls
}

func NewPythonicDict() *PythonicDict {
	dict := &PythonicDict{
		data:    make(map[LispValue]LispValue),
		methods: make(map[string]DictMethod),
	}
	dict.registerMethods()
	return dict
}

// HasMethod checks if a method exists on the dictionary
func (d *PythonicDict) HasMethod(methodName string) bool {
	d.mu.RLock()
	_, exists := d.methods[methodName]
	d.mu.RUnlock()
	return exists
}

// CallMethod calls a method on the dictionary
func (d *PythonicDict) CallMethod(methodName string, args []LispValue) (LispValue, error) {
	d.mu.RLock()
	method, exists := d.methods[methodName]
	d.mu.RUnlock()

	if !exists {
		return nil, fmt.Errorf("dictionary has no method '%s'", methodName)
	}

	return method(d, args)
}

// registerMethods initializes the built-in methods for the dictionary
func (d *PythonicDict) registerMethods() {
	d.methods = map[string]DictMethod{
		"get": func(self *PythonicDict, args []LispValue) (LispValue, error) {
			if len(args) < 1 {
				return nil, fmt.Errorf("dict.get requires at least a key argument")
			}

			key := args[0]
			value, exists := self.Get(key)

			if !exists && len(args) > 1 {
				// Return default value if provided
				return args[1], nil
			} else if !exists {
				// Return nil if no default and key doesn't exist
				return nil, nil
			}

			return value, nil
		},

		"set": func(self *PythonicDict, args []LispValue) (LispValue, error) {
			if len(args) < 2 {
				return nil, fmt.Errorf("dict.set requires key and value arguments")
			}

			key := args[0]
			value := args[1]

			self.Set(key, value)
			return self, nil // Return the dict for method chaining
		},

		"update": func(self *PythonicDict, args []LispValue) (LispValue, error) {
			if len(args) < 1 {
				return nil, fmt.Errorf("dict.update requires a dictionary argument")
			}

			otherDict, ok := args[0].(*PythonicDict)
			if !ok {
				return nil, fmt.Errorf("dict.update requires a dictionary argument, got %T", args[0])
			}

			otherDict.Iterate(func(k, v LispValue) error {
				self.Set(k, v)
				return nil
			})

			return self, nil // Return the dict for method chaining
		},

		"keys": func(self *PythonicDict, args []LispValue) (LispValue, error) {
			keys := self.SortedKeys()
			result := make(LispList, len(keys))
			for i, key := range keys {
				result[i] = key
			}
			return result, nil
		},

		"values": func(self *PythonicDict, args []LispValue) (LispValue, error) {
			keys := self.SortedKeys()
			result := make(LispList, len(keys))
			for i, key := range keys {
				value, _ := self.Get(key)
				result[i] = value
			}
			return result, nil
		},

		"items": func(self *PythonicDict, args []LispValue) (LispValue, error) {
			keys := self.SortedKeys()
			result := make(LispList, len(keys))
			for i, key := range keys {
				value, _ := self.Get(key)
				pair := LispList{key, value}
				result[i] = pair
			}
			return result, nil
		},

		"delete": func(self *PythonicDict, args []LispValue) (LispValue, error) {
			if len(args) < 1 {
				return nil, fmt.Errorf("dict.delete requires a key argument")
			}

			self.Delete(args[0])
			return self, nil
		},

		"clear": func(self *PythonicDict, args []LispValue) (LispValue, error) {
			self.mu.Lock()
			self.data = make(map[LispValue]LispValue)
			self.mu.Unlock()
			return self, nil
		},

		"has_key": func(self *PythonicDict, args []LispValue) (LispValue, error) {
			if len(args) < 1 {
				return nil, fmt.Errorf("dict.has_key requires a key argument")
			}

			_, exists := self.Get(args[0])
			return exists, nil
		},

		"contains?": func(self *PythonicDict, args []LispValue) (LispValue, error) {
			if len(args) < 1 {
				return nil, fmt.Errorf("dict.contains? requires a key argument")
			}

			_, exists := self.Get(args[0])
			return exists, nil
		},

		"pop": func(self *PythonicDict, args []LispValue) (LispValue, error) {
			if len(args) < 1 {
				return nil, fmt.Errorf("dict.pop requires a key argument")
			}

			key := args[0]
			value, exists := self.Get(key)

			if !exists {
				if len(args) > 1 {
					return args[1], nil // Return default if provided
				}
				return nil, fmt.Errorf("KeyError: key %v not found", key)
			}

			self.Delete(key)
			return value, nil
		},
	}
}

// Get retrieves a value from the dictionary by key
func (d *PythonicDict) Get(key LispValue) (LispValue, bool) {
	d.mu.RLock()
	defer d.mu.RUnlock()
	value, ok := d.data[key]
	return value, ok
}

// Set sets a key-value pair in the dictionary
func (d *PythonicDict) Set(key, value LispValue) {
	d.mu.Lock()
	defer d.mu.Unlock()
	d.data[key] = value
}

// Delete removes a key from the dictionary
func (d *PythonicDict) Delete(key LispValue) {
	d.mu.Lock()
	defer d.mu.Unlock()
	delete(d.data, key)
}

// HasKey checks if a key exists in the dictionary
func (d *PythonicDict) HasKey(key LispValue) bool {
	d.mu.RLock()
	defer d.mu.RUnlock()
	_, exists := d.data[key]
	return exists
}

// Contains is an alias for HasKey
func (d *PythonicDict) Contains(key LispValue) bool {
	return d.HasKey(key)
}

// Size returns the number of elements in the dictionary
func (d *PythonicDict) Size() int {
	d.mu.RLock()
	defer d.mu.RUnlock()
	return len(d.data)
}

// Iterate safely iterates over the dictionary's contents
func (d *PythonicDict) Iterate(f func(key, value LispValue) error) error {
	d.mu.RLock()
	// Create a copy of the keys and values to iterate safely
	keys := make([]LispValue, 0, len(d.data))
	values := make([]LispValue, 0, len(d.data))

	for k, v := range d.data {
		keys = append(keys, k)
		values = append(values, v)
	}
	d.mu.RUnlock()

	// Now iterate using the copies
	for i := range keys {
		if err := f(keys[i], values[i]); err != nil {
			return err
		}
	}
	return nil
}

// SortedKeys returns the dictionary keys in sorted order
func (d *PythonicDict) SortedKeys() []LispValue {
	d.mu.RLock()
	keys := make([]LispValue, 0, len(d.data))
	for k := range d.data {
		keys = append(keys, k)
	}
	d.mu.RUnlock()

	sort.Slice(keys, func(i, j int) bool {
		return Compare(keys[i], keys[j]) < 0
	})
	return keys
}

// Implementation of ObjProtocol interface

// HasMember checks if a property or method exists
func (d *PythonicDict) HasMember(name string) bool {
	// First check for methods
	if d.HasMethod(name) {
		return true
	}

	// Then check for common dictionary pseudo-properties
	switch name {
	case "length", "len", "size", "count":
		return true
	}

	// Finally check for attributes as keys
	_, exists := d.Get(name)
	return exists
}

// GetMember implements the ObjProtocol interface
func (d *PythonicDict) GetMember(name string, eval Evaluator, env Environment) (LispValue, error) {
	// Store evaluator reference
	d.SetEvaluator(eval)

	// First check for methods
	if d.HasMethod(name) {
		method, _ := d.methods[name]
		// For method calls without arguments, wrap it to be called later
		return BuiltinFunc(func(args []LispValue, callEnv Environment) (LispValue, error) {
			return method(d, args)
		}), nil
	}

	// Check for common dictionary pseudo-properties
	switch name {
	case "length", "len", "size", "count":
		return float64(d.Size()), nil
	}

	// Then check for attributes
	if value, exists := d.Get(name); exists {
		return value, nil
	}

	return nil, fmt.Errorf("dict has no attribute '%s'", name)
}

// SetMember implements the ObjProtocol interface
func (d *PythonicDict) SetMember(name string, value LispValue, eval Evaluator, env Environment) error {
	// Store evaluator reference
	d.SetEvaluator(eval)

	// Set the attribute in the dictionary
	d.Set(name, value)
	return nil
}

// Ensure PythonicDict implements EvaluatorAware and ObjProtocol
var _ EvaluatorAware = (*PythonicDict)(nil)
var _ ObjProtocol = (*PythonicDict)(nil)

// SetEvaluator stores a reference to the evaluator
func (d *PythonicDict) SetEvaluator(eval Evaluator) {
	d.mu.Lock()
	defer d.mu.Unlock()
	d.evaluator = eval
}

// GetEvaluator retrieves the stored evaluator reference
func (d *PythonicDict) GetEvaluator() Evaluator {
	d.mu.RLock()
	defer d.mu.RUnlock()
	return d.evaluator
}

// AsObject implementation for obj protocol
func (d *PythonicDict) AsObject() ObjProtocol {
	return d
}

// String implements the fmt.Stringer interface for proper dictionary representation
func (d *PythonicDict) String() string {
	d.mu.RLock()
	defer d.mu.RUnlock()

	if len(d.data) == 0 {
		return "{}"
	}

	keys := d.SortedKeys()
	result := "{"

	for i, key := range keys {
		value, _ := d.data[key]
		result += fmt.Sprintf("%v: %v", key, value)
		if i < len(keys)-1 {
			result += ", "
		}
	}

	result += "}"
	return result
}

// Make sure dictionaries are properly printed by the print function
func (d *PythonicDict) PrintValue() string {
	return d.String()
}
