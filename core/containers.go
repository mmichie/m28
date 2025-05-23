package core

import (
	"fmt"
	"sort"
	"strings"
)

// ListValue represents a list of values
type ListValue []Value

// Type implements Value.Type
func (l ListValue) Type() Type {
	return ListType
}

// String implements Value.String
func (l ListValue) String() string {
	elements := make([]string, len(l))
	for i, v := range l {
		elements[i] = PrintValue(v)
	}
	return "[" + strings.Join(elements, ", ") + "]"
}

// GetAttr implements Object interface for list methods
func (l ListValue) GetAttr(name string) (Value, bool) {
	switch name {
	case "length":
		return NumberValue(len(l)), true
	case "append":
		return &BuiltinMethod{
			BaseObject: *NewBaseObject(MethodType),
			fn: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("append expects 1 argument, got %d", len(args))
				}
				list := receiver.(ListValue)
				return ListValue(append(list, args[0])), nil
			},
			receiver: l,
		}, true
	case "extend":
		return &BuiltinMethod{
			BaseObject: *NewBaseObject(MethodType),
			fn: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("extend expects 1 argument, got %d", len(args))
				}

				list := receiver.(ListValue)
				other, ok := args[0].(ListValue)
				if !ok {
					return nil, fmt.Errorf("extend expects a list, got %s", args[0].Type())
				}

				return ListValue(append(list, other...)), nil
			},
			receiver: l,
		}, true
	case "map":
		return &BuiltinMethod{
			BaseObject: *NewBaseObject(MethodType),
			fn: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("map expects 1 argument, got %d", len(args))
				}

				fn, ok := args[0].(Callable)
				if !ok {
					return nil, fmt.Errorf("map expects a callable, got %s", args[0].Type())
				}

				list := receiver.(ListValue)
				result := make(ListValue, len(list))

				for i, item := range list {
					mappedValue, err := fn.Call([]Value{item}, ctx)
					if err != nil {
						return nil, err
					}
					result[i] = mappedValue
				}

				return result, nil
			},
			receiver: l,
		}, true
	case "filter":
		return &BuiltinMethod{
			BaseObject: *NewBaseObject(MethodType),
			fn: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("filter expects 1 argument, got %d", len(args))
				}

				fn, ok := args[0].(Callable)
				if !ok {
					return nil, fmt.Errorf("filter expects a callable, got %s", args[0].Type())
				}

				list := receiver.(ListValue)
				result := make(ListValue, 0, len(list))

				for _, item := range list {
					pred, err := fn.Call([]Value{item}, ctx)
					if err != nil {
						return nil, err
					}

					if IsTruthy(pred) {
						result = append(result, item)
					}
				}

				return result, nil
			},
			receiver: l,
		}, true
	default:
		return nil, false
	}
}

// SetAttr implements Object.SetAttr (not supported for lists)
func (l ListValue) SetAttr(name string, value Value) error {
	return fmt.Errorf("cannot set attributes on lists")
}

// CallMethod implements Object.CallMethod
func (l ListValue) CallMethod(name string, args []Value, ctx *Context) (Value, error) {
	method, ok := l.GetAttr(name)
	if !ok {
		return nil, fmt.Errorf("list has no method named %s", name)
	}

	callable, ok := method.(Callable)
	if !ok {
		return nil, fmt.Errorf("%s is not callable", name)
	}

	return callable.Call(args, ctx)
}

// DictValue represents a dictionary mapping keys to values
type DictValue struct {
	BaseObject
	entries map[string]Value
}

// NewDict creates a new dictionary
func NewDict() *DictValue {
	return &DictValue{
		BaseObject: *NewBaseObject(DictType),
		entries:    make(map[string]Value),
	}
}

// Get retrieves a value from the dictionary
func (d *DictValue) Get(key string) (Value, bool) {
	value, ok := d.entries[key]
	return value, ok
}

// Set sets a value in the dictionary
func (d *DictValue) Set(key string, value Value) {
	d.entries[key] = value
}

// Delete removes a key-value pair from the dictionary
func (d *DictValue) Delete(key string) bool {
	if _, ok := d.entries[key]; ok {
		delete(d.entries, key)
		return true
	}
	return false
}

// Has checks if a key exists in the dictionary
func (d *DictValue) Has(key string) bool {
	_, ok := d.entries[key]
	return ok
}

// Keys returns all keys in the dictionary
func (d *DictValue) Keys() []string {
	keys := make([]string, 0, len(d.entries))
	for k := range d.entries {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	return keys
}

// Size returns the number of entries in the dictionary
func (d *DictValue) Size() int {
	return len(d.entries)
}

// String implements Value.String
func (d *DictValue) String() string {
	if len(d.entries) == 0 {
		return "{}"
	}

	// Get sorted keys for consistent output
	keys := make([]string, 0, len(d.entries))
	for k := range d.entries {
		keys = append(keys, k)
	}
	sort.Strings(keys)

	// Build the string representation
	pairs := make([]string, len(keys))
	for i, k := range keys {
		pairs[i] = fmt.Sprintf("%q: %s", k, PrintValue(d.entries[k]))
	}

	return "{" + strings.Join(pairs, ", ") + "}"
}

// GetAttr implements Object.GetAttr
func (d *DictValue) GetAttr(name string) (Value, bool) {
	// First check for methods
	switch name {
	case "get":
		return &BuiltinMethod{
			BaseObject: *NewBaseObject(MethodType),
			fn: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) < 1 || len(args) > 2 {
					return nil, fmt.Errorf("get expects 1 or 2 arguments, got %d", len(args))
				}

				dict := receiver.(*DictValue)
				keyStr, ok := args[0].(StringValue)
				if !ok {
					return nil, fmt.Errorf("key must be a string, got %s", args[0].Type())
				}

				if value, ok := dict.Get(string(keyStr)); ok {
					return value, nil
				}

				// Return default value if provided
				if len(args) > 1 {
					return args[1], nil
				}

				return Nil, nil
			},
			receiver: d,
		}, true
	case "keys":
		return &BuiltinMethod{
			BaseObject: *NewBaseObject(MethodType),
			fn: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 0 {
					return nil, fmt.Errorf("keys expects 0 arguments, got %d", len(args))
				}

				dict := receiver.(*DictValue)
				keys := dict.Keys()

				keyList := make(ListValue, len(keys))
				for i, k := range keys {
					keyList[i] = StringValue(k)
				}

				return keyList, nil
			},
			receiver: d,
		}, true
	case "values":
		return &BuiltinMethod{
			BaseObject: *NewBaseObject(MethodType),
			fn: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 0 {
					return nil, fmt.Errorf("values expects 0 arguments, got %d", len(args))
				}

				dict := receiver.(*DictValue)

				values := make(ListValue, 0, len(dict.entries))
				for _, v := range dict.entries {
					values = append(values, v)
				}

				return values, nil
			},
			receiver: d,
		}, true
	case "items":
		return &BuiltinMethod{
			BaseObject: *NewBaseObject(MethodType),
			fn: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 0 {
					return nil, fmt.Errorf("items expects 0 arguments, got %d", len(args))
				}

				dict := receiver.(*DictValue)

				items := make(ListValue, 0, len(dict.entries))
				for k, v := range dict.entries {
					items = append(items, ListValue{StringValue(k), v})
				}

				return items, nil
			},
			receiver: d,
		}, true
	case "length":
	case "size":
		return &BuiltinMethod{
			BaseObject: *NewBaseObject(MethodType),
			fn: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 0 {
					return nil, fmt.Errorf("size expects 0 arguments, got %d", len(args))
				}

				dict := receiver.(*DictValue)
				return NumberValue(dict.Size()), nil
			},
			receiver: d,
		}, true
	default:
		// Then check for entries in the map
		if value, ok := d.Get(name); ok {
			return value, true
		}

		// Finally check the BaseObject attributes
		return d.BaseObject.GetAttr(name)
	}
	return nil, false
}

// TupleValue represents an immutable sequence of values
type TupleValue []Value

// Type implements Value.Type
func (t TupleValue) Type() Type {
	return TupleType
}

// String implements Value.String
func (t TupleValue) String() string {
	elements := make([]string, len(t))
	for i, v := range t {
		elements[i] = PrintValue(v)
	}
	return "(" + strings.Join(elements, ", ") + ")"
}

// GetAttr implements Object interface for tuple methods
func (t TupleValue) GetAttr(name string) (Value, bool) {
	switch name {
	case "length":
		return NumberValue(len(t)), true
	default:
		return nil, false
	}
}

// SetAttr implements Object.SetAttr (not supported for tuples)
func (t TupleValue) SetAttr(name string, value Value) error {
	return fmt.Errorf("cannot set attributes on tuples")
}

// CallMethod implements Object.CallMethod
func (t TupleValue) CallMethod(name string, args []Value, ctx *Context) (Value, error) {
	method, ok := t.GetAttr(name)
	if !ok {
		return nil, fmt.Errorf("tuple has no method named %s", name)
	}

	callable, ok := method.(Callable)
	if !ok {
		return nil, fmt.Errorf("%s is not callable", name)
	}

	return callable.Call(args, ctx)
}

// SetValue represents a set of unique values
type SetValue struct {
	BaseObject
	elements map[string]Value // Map keys are string representations of values
}

// NewSet creates a new empty set
func NewSet() *SetValue {
	return &SetValue{
		BaseObject: *NewBaseObject(SetType),
		elements:   make(map[string]Value),
	}
}

// String implements Value.String
func (s *SetValue) String() string {
	if len(s.elements) == 0 {
		return "{}"
	}

	// Get elements and sort them
	elements := make([]Value, 0, len(s.elements))
	for _, v := range s.elements {
		elements = append(elements, v)
	}

	// Sort elements for consistent output
	sort.Slice(elements, func(i, j int) bool {
		return Compare(elements[i], elements[j]) < 0
	})

	// Build the string representation
	elemStrings := make([]string, len(elements))
	for i, elem := range elements {
		elemStrings[i] = PrintValue(elem)
	}

	return "{" + strings.Join(elemStrings, ", ") + "}"
}

// Add adds an element to the set
func (s *SetValue) Add(value Value) {
	key := PrintValue(value)
	s.elements[key] = value
}

// Remove removes an element from the set
func (s *SetValue) Remove(value Value) bool {
	key := PrintValue(value)
	if _, ok := s.elements[key]; ok {
		delete(s.elements, key)
		return true
	}
	return false
}

// Contains checks if an element is in the set
func (s *SetValue) Contains(value Value) bool {
	key := PrintValue(value)
	_, ok := s.elements[key]
	return ok
}

// Size returns the number of elements in the set
func (s *SetValue) Size() int {
	return len(s.elements)
}

// Elements returns all elements in the set as a slice
func (s *SetValue) Elements() []Value {
	result := make([]Value, 0, len(s.elements))
	for _, v := range s.elements {
		result = append(result, v)
	}
	return result
}

// GetAttr implements Object.GetAttr for set methods
func (s *SetValue) GetAttr(name string) (Value, bool) {
	switch name {
	case "add":
		return &BuiltinMethod{
			BaseObject: *NewBaseObject(MethodType),
			fn: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("add expects 1 argument, got %d", len(args))
				}

				set := receiver.(*SetValue)
				set.Add(args[0])
				return set, nil
			},
			receiver: s,
		}, true
	case "remove":
		return &BuiltinMethod{
			BaseObject: *NewBaseObject(MethodType),
			fn: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("remove expects 1 argument, got %d", len(args))
				}

				set := receiver.(*SetValue)
				success := set.Remove(args[0])

				if !success {
					return nil, fmt.Errorf("element not found in set")
				}

				return set, nil
			},
			receiver: s,
		}, true
	case "contains":
		return &BuiltinMethod{
			BaseObject: *NewBaseObject(MethodType),
			fn: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("contains expects 1 argument, got %d", len(args))
				}

				set := receiver.(*SetValue)
				return BoolValue(set.Contains(args[0])), nil
			},
			receiver: s,
		}, true
	case "size":
	case "length":
		return &BuiltinMethod{
			BaseObject: *NewBaseObject(MethodType),
			fn: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 0 {
					return nil, fmt.Errorf("size expects 0 arguments, got %d", len(args))
				}

				set := receiver.(*SetValue)
				return NumberValue(set.Size()), nil
			},
			receiver: s,
		}, true
	case "elements":
		return &BuiltinMethod{
			BaseObject: *NewBaseObject(MethodType),
			fn: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 0 {
					return nil, fmt.Errorf("elements expects 0 arguments, got %d", len(args))
				}

				set := receiver.(*SetValue)
				return ListValue(set.Elements()), nil
			},
			receiver: s,
		}, true
	default:
		return s.BaseObject.GetAttr(name)
	}
	return nil, false
}

// Commonly used empty collections
var (
	EmptyList  = ListValue{}
	EmptyTuple = TupleValue{}
	EmptyDict  = NewDict()
	EmptySet   = NewSet()
)
