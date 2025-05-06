package core

import (
	"fmt"
	"sort"
	"sync"
)

// DictMethod represents a method function for dictionaries
type DictMethod func(self *PythonicDict, args []LispValue) (LispValue, error)

type PythonicDict struct {
	data    map[LispValue]LispValue
	mu      sync.RWMutex
	methods map[string]DictMethod
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
	}
}

func (d *PythonicDict) Get(key LispValue) (LispValue, bool) {
	d.mu.RLock()
	defer d.mu.RUnlock()
	value, ok := d.data[key]
	return value, ok
}

func (d *PythonicDict) Set(key, value LispValue) {
	d.mu.Lock()
	defer d.mu.Unlock()
	d.data[key] = value
}

func (d *PythonicDict) Delete(key LispValue) {
	d.mu.Lock()
	defer d.mu.Unlock()
	delete(d.data, key)
}

// Add the Size method
func (d *PythonicDict) Size() int {
	d.mu.RLock()
	defer d.mu.RUnlock()
	return len(d.data)
}

func (d *PythonicDict) Iterate(f func(key, value LispValue) error) error {
	d.mu.RLock()
	defer d.mu.RUnlock()
	for k, v := range d.data {
		if err := f(k, v); err != nil {
			return err
		}
	}
	return nil
}

// Helper method for PythonicDict to get sorted keys
func (d *PythonicDict) sortedKeys() []LispValue {
	keys := make([]LispValue, 0, d.Size())
	d.Iterate(func(k, v LispValue) error {
		keys = append(keys, k)
		return nil
	})
	sort.Slice(keys, func(i, j int) bool {
		return Compare(keys[i], keys[j]) < 0
	})
	return keys
}

// SortedKeys returns the dictionary keys in sorted order (exported version)
func (d *PythonicDict) SortedKeys() []LispValue {
	return d.sortedKeys()
}
