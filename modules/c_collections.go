package modules

import (
	"fmt"
	"sort"

	"github.com/mmichie/m28/core"
)

// InitCollectionsModule creates and returns the collections module
func InitCollectionsModule() *core.DictValue {
	collectionsModule := core.NewDict()

	// Register Counter class
	collectionsModule.Set("Counter", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Counter() or Counter(iterable) or Counter(mapping)
		counter := NewCounter()

		if len(args) > 0 {
			arg := args[0]

			// If it's a dict, copy the counts
			if dict, ok := arg.(*core.DictValue); ok {
				for _, key := range dict.Keys() {
					if val, ok := dict.Get(key); ok {
						if num, ok := val.(core.NumberValue); ok {
							counter.counts.Set(key, num)
						}
					}
				}
			} else if _, ok := arg.(core.ListValue); ok {
				// Count elements from iterable
				counter.Update([]core.Value{arg})
			} else if str, ok := arg.(core.StringValue); ok {
				// Count characters in string - convert to list of chars
				chars := make([]core.Value, 0)
				for _, ch := range string(str) {
					chars = append(chars, core.StringValue(string(ch)))
				}
				counter.Update([]core.Value{core.ListValue(chars)})
			}
		}

		return counter, nil
	}))

	// Register defaultdict class
	collectionsModule.Set("defaultdict", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("defaultdict requires at least 1 argument (default_factory)")
		}

		// First argument is the default factory
		factory := args[0]

		dd := NewDefaultDict(factory)

		// If there's a second argument, it should be a dict or iterable of pairs
		if len(args) > 1 {
			if dict, ok := args[1].(*core.DictValue); ok {
				for _, key := range dict.Keys() {
					if val, ok := dict.Get(key); ok {
						dd.dict.Set(key, val)
					}
				}
			}
		}

		return dd, nil
	}))

	// Register deque class
	collectionsModule.Set("deque", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		dq := NewDeque()

		// If there's an iterable argument, add all items
		if len(args) > 0 {
			if list, ok := args[0].(core.ListValue); ok {
				for _, item := range list {
					dq.Append(item)
				}
			}
		}

		// Check for maxlen argument
		if len(args) > 1 {
			if num, ok := args[1].(core.NumberValue); ok {
				dq.maxlen = int(num)
				// Trim if necessary
				for len(dq.items) > dq.maxlen && dq.maxlen > 0 {
					dq.items = dq.items[1:]
				}
			}
		}

		return dq, nil
	}))

	return collectionsModule
}

// Counter is a dict subclass for counting hashable objects
type Counter struct {
	counts *core.DictValue
}

// NewCounter creates a new Counter
func NewCounter() *Counter {
	return &Counter{
		counts: core.NewDict(),
	}
}

// Type implements Value.Type
func (c *Counter) Type() core.Type {
	return "Counter"
}

// String implements Value.String
func (c *Counter) String() string {
	return fmt.Sprintf("Counter(%s)", c.counts.String())
}

// GetAttr implements attribute access
func (c *Counter) GetAttr(name string) (core.Value, bool) {
	switch name {
	case "most_common":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			n := -1
			if len(args) > 0 {
				if num, ok := args[0].(core.NumberValue); ok {
					n = int(num)
				}
			}
			return c.MostCommon(n), nil
		}), true
	case "elements":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return c.Elements(), nil
		}), true
	case "update":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) > 0 {
				c.Update(args)
			}
			return core.Nil, nil
		}), true
	case "total":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return c.Total(), nil
		}), true
	case "__getitem__":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("__getitem__ requires exactly 1 argument")
			}
			key := core.ValueToKey(args[0])
			if val, ok := c.counts.Get(key); ok {
				return val, nil
			}
			return core.NumberValue(0), nil
		}), true
	case "__setitem__":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) != 2 {
				return nil, fmt.Errorf("__setitem__ requires exactly 2 arguments")
			}
			key := core.ValueToKey(args[0])
			c.counts.Set(key, args[1])
			return core.Nil, nil
		}), true
	}
	return nil, false
}

// SetAttr implements attribute setting
func (c *Counter) SetAttr(name string, value core.Value) error {
	return fmt.Errorf("cannot set attribute '%s' on Counter", name)
}

// Update adds counts from an iterable or mapping
func (c *Counter) Update(args []core.Value) {
	if len(args) == 0 {
		return
	}

	arg := args[0]

	// If it's a list, count each element
	if list, ok := arg.(core.ListValue); ok {
		for _, item := range list {
			key := core.ValueToKey(item)
			count := core.NumberValue(0)
			if val, ok := c.counts.Get(key); ok {
				if num, ok := val.(core.NumberValue); ok {
					count = num
				}
			}
			c.counts.Set(key, count+1)
		}
	}
}

// MostCommon returns a list of the n most common elements and their counts
func (c *Counter) MostCommon(n int) core.Value {
	type pair struct {
		key   string
		value float64
	}

	// Collect all key-value pairs
	pairs := []pair{}
	for _, key := range c.counts.Keys() {
		if val, ok := c.counts.Get(key); ok {
			if num, ok := val.(core.NumberValue); ok {
				pairs = append(pairs, pair{key, float64(num)})
			}
		}
	}

	// Sort by count descending
	sort.Slice(pairs, func(i, j int) bool {
		return pairs[i].value > pairs[j].value
	})

	// Take top n if specified
	if n >= 0 && n < len(pairs) {
		pairs = pairs[:n]
	}

	// Convert to list of tuples
	result := make(core.ListValue, len(pairs))
	for i, p := range pairs {
		// Reconstruct the key as a Value
		keyVal := core.StringValue(p.key) // Simplified - assumes string keys
		result[i] = core.TupleValue{keyVal, core.NumberValue(p.value)}
	}

	return result
}

// Elements returns an iterator over elements repeating each as many times as its count
func (c *Counter) Elements() core.Value {
	result := make(core.ListValue, 0)
	for _, key := range c.counts.Keys() {
		if val, ok := c.counts.Get(key); ok {
			if num, ok := val.(core.NumberValue); ok {
				count := int(num)
				for i := 0; i < count; i++ {
					// Reconstruct the key as a Value
					keyVal := core.StringValue(key) // Simplified
					result = append(result, keyVal)
				}
			}
		}
	}
	return result
}

// Total returns the sum of all counts
func (c *Counter) Total() core.Value {
	total := 0.0
	for _, key := range c.counts.Keys() {
		if val, ok := c.counts.Get(key); ok {
			if num, ok := val.(core.NumberValue); ok {
				total += float64(num)
			}
		}
	}
	return core.NumberValue(total)
}

// DefaultDict is a dict subclass that calls a factory function for missing keys
type DefaultDict struct {
	dict           *core.DictValue
	defaultFactory core.Value
}

// NewDefaultDict creates a new defaultdict
func NewDefaultDict(factory core.Value) *DefaultDict {
	return &DefaultDict{
		dict:           core.NewDict(),
		defaultFactory: factory,
	}
}

// Type implements Value.Type
func (dd *DefaultDict) Type() core.Type {
	return "defaultdict"
}

// String implements Value.String
func (dd *DefaultDict) String() string {
	return fmt.Sprintf("defaultdict(%s, %s)", dd.defaultFactory.String(), dd.dict.String())
}

// GetAttr implements attribute access
func (dd *DefaultDict) GetAttr(name string) (core.Value, bool) {
	switch name {
	case "__getitem__":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("__getitem__ requires exactly 1 argument")
			}
			return dd.Get(args[0], ctx)
		}), true
	case "__setitem__":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) != 2 {
				return nil, fmt.Errorf("__setitem__ requires exactly 2 arguments")
			}
			key := core.ValueToKey(args[0])
			dd.dict.Set(key, args[1])
			return core.Nil, nil
		}), true
	case "keys":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			keys := dd.dict.Keys()
			result := make(core.ListValue, len(keys))
			for i, k := range keys {
				result[i] = core.StringValue(k)
			}
			return result, nil
		}), true
	case "values":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			values := make(core.ListValue, 0)
			for _, key := range dd.dict.Keys() {
				if val, ok := dd.dict.Get(key); ok {
					values = append(values, val)
				}
			}
			return values, nil
		}), true
	}
	return nil, false
}

// SetAttr implements attribute setting
func (dd *DefaultDict) SetAttr(name string, value core.Value) error {
	return fmt.Errorf("cannot set attribute '%s' on defaultdict", name)
}

// Get returns the value for a key, calling the factory if missing
func (dd *DefaultDict) Get(key core.Value, ctx *core.Context) (core.Value, error) {
	k := core.ValueToKey(key)
	if val, ok := dd.dict.Get(k); ok {
		return val, nil
	}

	// Call the factory function with the provided context
	if callable, ok := dd.defaultFactory.(interface {
		Call([]core.Value, *core.Context) (core.Value, error)
	}); ok {
		val, err := callable.Call([]core.Value{}, ctx)
		if err != nil {
			return nil, err
		}
		dd.dict.Set(k, val)
		return val, nil
	}

	return nil, fmt.Errorf("default_factory is not callable")
}

// Deque is a double-ended queue
type Deque struct {
	items  []core.Value
	maxlen int // -1 means no limit
}

// NewDeque creates a new deque
func NewDeque() *Deque {
	return &Deque{
		items:  make([]core.Value, 0),
		maxlen: -1,
	}
}

// Type implements Value.Type
func (dq *Deque) Type() core.Type {
	return "deque"
}

// String implements Value.String
func (dq *Deque) String() string {
	return fmt.Sprintf("deque(%v)", core.ListValue(dq.items).String())
}

// GetAttr implements attribute access
func (dq *Deque) GetAttr(name string) (core.Value, bool) {
	switch name {
	case "append":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("append() requires exactly 1 argument")
			}
			dq.Append(args[0])
			return core.Nil, nil
		}), true
	case "appendleft":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("appendleft() requires exactly 1 argument")
			}
			dq.AppendLeft(args[0])
			return core.Nil, nil
		}), true
	case "pop":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return dq.Pop()
		}), true
	case "popleft":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return dq.PopLeft()
		}), true
	case "extend":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("extend() requires exactly 1 argument")
			}
			if list, ok := args[0].(core.ListValue); ok {
				for _, item := range list {
					dq.Append(item)
				}
			}
			return core.Nil, nil
		}), true
	case "extendleft":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("extendleft() requires exactly 1 argument")
			}
			if list, ok := args[0].(core.ListValue); ok {
				for _, item := range list {
					dq.AppendLeft(item)
				}
			}
			return core.Nil, nil
		}), true
	case "clear":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			dq.items = make([]core.Value, 0)
			return core.Nil, nil
		}), true
	case "rotate":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			n := 1
			if len(args) > 0 {
				if num, ok := args[0].(core.NumberValue); ok {
					n = int(num)
				}
			}
			dq.Rotate(n)
			return core.Nil, nil
		}), true
	case "__len__":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.NumberValue(len(dq.items)), nil
		}), true
	case "__getitem__":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("__getitem__ requires exactly 1 argument")
			}
			if idx, ok := args[0].(core.NumberValue); ok {
				i := int(idx)
				if i < 0 {
					i = len(dq.items) + i
				}
				if i < 0 || i >= len(dq.items) {
					return nil, fmt.Errorf("deque index out of range")
				}
				return dq.items[i], nil
			}
			return nil, fmt.Errorf("deque indices must be integers")
		}), true
	}
	return nil, false
}

// SetAttr implements attribute setting
func (dq *Deque) SetAttr(name string, value core.Value) error {
	return fmt.Errorf("cannot set attribute '%s' on deque", name)
}

// Append adds an element to the right side
func (dq *Deque) Append(item core.Value) {
	dq.items = append(dq.items, item)
	if dq.maxlen > 0 && len(dq.items) > dq.maxlen {
		dq.items = dq.items[1:]
	}
}

// AppendLeft adds an element to the left side
func (dq *Deque) AppendLeft(item core.Value) {
	dq.items = append([]core.Value{item}, dq.items...)
	if dq.maxlen > 0 && len(dq.items) > dq.maxlen {
		dq.items = dq.items[:len(dq.items)-1]
	}
}

// Pop removes and returns an element from the right side
func (dq *Deque) Pop() (core.Value, error) {
	if len(dq.items) == 0 {
		return nil, fmt.Errorf("pop from an empty deque")
	}
	item := dq.items[len(dq.items)-1]
	dq.items = dq.items[:len(dq.items)-1]
	return item, nil
}

// PopLeft removes and returns an element from the left side
func (dq *Deque) PopLeft() (core.Value, error) {
	if len(dq.items) == 0 {
		return nil, fmt.Errorf("pop from an empty deque")
	}
	item := dq.items[0]
	dq.items = dq.items[1:]
	return item, nil
}

// Rotate rotates the deque n steps to the right (negative for left)
func (dq *Deque) Rotate(n int) {
	if len(dq.items) == 0 {
		return
	}

	// Normalize n
	n = n % len(dq.items)
	if n < 0 {
		n = len(dq.items) + n
	}

	// Rotate by moving elements
	if n > 0 {
		dq.items = append(dq.items[len(dq.items)-n:], dq.items[:len(dq.items)-n]...)
	}
}
