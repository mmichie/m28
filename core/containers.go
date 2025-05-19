package core

import (
	"fmt"
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
		elements[i] = v.String()
	}
	return fmt.Sprintf("[%s]", strings.Join(elements, ", "))
}

// GetAttr implements Object interface for list methods
func (l ListValue) GetAttr(name string) (Value, bool) {
	switch name {
	case "length":
		return NumberValue(len(l)), true
	case "append":
		return &BuiltinMethod{
			fn: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("append expects 1 argument, got %d", len(args))
				}
				list := receiver.(ListValue)
				return ListValue(append(list, args[0])), nil
			},
		}, true
	case "extend":
		return &BuiltinMethod{
			fn: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("extend expects 1 argument, got %d", len(args))
				}
				
				list := receiver.(ListValue)
				other, ok := args[0].(ListValue)
				if !ok {
					return nil, fmt.Errorf("extend expects a list, got %s", args[0].Type().Name())
				}
				
				return ListValue(append(list, other...)), nil
			},
		}, true
	case "map":
		return &BuiltinMethod{
			fn: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("map expects 1 argument, got %d", len(args))
				}
				
				fn, ok := args[0].(Callable)
				if !ok {
					return nil, fmt.Errorf("map expects a callable, got %s", args[0].Type().Name())
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
		}, true
	case "filter":
		return &BuiltinMethod{
			fn: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("filter expects 1 argument, got %d", len(args))
				}
				
				fn, ok := args[0].(Callable)
				if !ok {
					return nil, fmt.Errorf("filter expects a callable, got %s", args[0].Type().Name())
				}
				
				list := receiver.(ListValue)
				result := make(ListValue, 0, len(list))
				
				for _, item := range list {
					pred, err := fn.Call([]Value{item}, ctx)
					if err != nil {
						return nil, err
					}
					
					if predBool, ok := pred.(BoolValue); ok && bool(predBool) {
						result = append(result, item)
					}
				}
				
				return result, nil
			},
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
	d.mu.RLock()
	defer d.mu.RUnlock()
	
	value, ok := d.entries[key]
	return value, ok
}

// Set sets a value in the dictionary
func (d *DictValue) Set(key string, value Value) {
	d.mu.Lock()
	defer d.mu.Unlock()
	
	d.entries[key] = value
}

// String implements Value.String
func (d *DictValue) String() string {
	d.mu.RLock()
	defer d.mu.RUnlock()
	
	pairs := make([]string, 0, len(d.entries))
	for k, v := range d.entries {
		pairs = append(pairs, fmt.Sprintf("%q: %s", k, v.String()))
	}
	return fmt.Sprintf("{%s}", strings.Join(pairs, ", "))
}

// GetAttr implements Object.GetAttr
func (d *DictValue) GetAttr(name string) (Value, bool) {
	// First check for methods
	switch name {
	case "get":
		return &BuiltinMethod{
			fn: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) < 1 || len(args) > 2 {
					return nil, fmt.Errorf("get expects 1 or 2 arguments, got %d", len(args))
				}
				
				dict := receiver.(*DictValue)
				keyStr, ok := args[0].(StringValue)
				if !ok {
					return nil, fmt.Errorf("key must be a string, got %s", args[0].Type().Name())
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
		}, true
	case "keys":
		return &BuiltinMethod{
			fn: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 0 {
					return nil, fmt.Errorf("keys expects 0 arguments, got %d", len(args))
				}
				
				dict := receiver.(*DictValue)
				dict.mu.RLock()
				defer dict.mu.RUnlock()
				
				keys := make(ListValue, 0, len(dict.entries))
				for k := range dict.entries {
					keys = append(keys, StringValue(k))
				}
				
				return keys, nil
			},
		}, true
	case "values":
		return &BuiltinMethod{
			fn: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 0 {
					return nil, fmt.Errorf("values expects 0 arguments, got %d", len(args))
				}
				
				dict := receiver.(*DictValue)
				dict.mu.RLock()
				defer dict.mu.RUnlock()
				
				values := make(ListValue, 0, len(dict.entries))
				for _, v := range dict.entries {
					values = append(values, v)
				}
				
				return values, nil
			},
		}, true
	case "items":
		return &BuiltinMethod{
			fn: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 0 {
					return nil, fmt.Errorf("items expects 0 arguments, got %d", len(args))
				}
				
				dict := receiver.(*DictValue)
				dict.mu.RLock()
				defer dict.mu.RUnlock()
				
				items := make(ListValue, 0, len(dict.entries))
				for k, v := range dict.entries {
					items = append(items, ListValue{StringValue(k), v})
				}
				
				return items, nil
			},
		}, true
	case "length":
		return &BuiltinMethod{
			fn: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 0 {
					return nil, fmt.Errorf("length expects 0 arguments, got %d", len(args))
				}
				
				dict := receiver.(*DictValue)
				dict.mu.RLock()
				defer dict.mu.RUnlock()
				
				return NumberValue(len(dict.entries)), nil
			},
		}, true
	default:
		// Then check for entries in the map
		if value, ok := d.Get(name); ok {
			return value, true
		}
		
		// Finally check the BaseObject attributes
		return d.BaseObject.GetAttr(name)
	}
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
		elements[i] = v.String()
	}
	return fmt.Sprintf("(%s)", strings.Join(elements, ", "))
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

// Commonly used empty collections
var (
	EmptyList  = ListValue{}
	EmptyTuple = TupleValue{}
	EmptyDict  = NewDict()
)