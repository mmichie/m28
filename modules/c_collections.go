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
			} else if _, ok := arg.(*core.ListValue); ok {
				// Count elements from iterable
				counter.Update([]core.Value{arg})
			} else if str, ok := arg.(core.StringValue); ok {
				// Count characters in string - convert to list of chars
				chars := make([]core.Value, 0)
				for _, ch := range string(str) {
					chars = append(chars, core.StringValue(string(ch)))
				}
				counter.Update([]core.Value{core.NewList(chars...)})
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
			if list, ok := args[0].(*core.ListValue); ok {
				for _, item := range list.Items() {
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

	// Register _deque_iterator type (constructor function)
	// This is the type returned by iter(deque)
	collectionsModule.Set("_deque_iterator", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("_deque_iterator() requires exactly 1 argument")
		}
		dq, ok := args[0].(*Deque)
		if !ok {
			return nil, fmt.Errorf("_deque_iterator() argument must be a deque")
		}
		return NewDequeIterator(dq), nil
	}))

	// Register OrderedDict class
	collectionsModule.Set("OrderedDict", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		od := NewOrderedDict()

		// If there's an argument, it should be a dict or iterable of pairs
		if len(args) > 0 {
			if dict, ok := args[0].(*core.DictValue); ok {
				for _, key := range dict.Keys() {
					if val, ok := dict.Get(key); ok {
						od.dict.Set(key, val)
					}
				}
			}
		}

		return od, nil
	}))

	// Register _tuplegetter type (for namedtuple field access)
	// This is a callable that retrieves a specific index from a tuple
	collectionsModule.Set("_tuplegetter", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("_tuplegetter() requires at least 1 argument (index)")
		}
		index, ok := args[0].(core.NumberValue)
		if !ok {
			return nil, fmt.Errorf("_tuplegetter() index must be a number")
		}

		var doc string
		if len(args) > 1 {
			if docVal, ok := args[1].(core.StringValue); ok {
				doc = string(docVal)
			}
		}

		return NewTupleGetter(int(index), doc), nil
	}))

	// Register _count_elements helper function for Counter
	// This tallies elements from an iterable into a mapping
	collectionsModule.Set("_count_elements", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, fmt.Errorf("_count_elements() requires exactly 2 arguments (mapping, iterable)")
		}

		// Get the mapping (should support __getitem__ and __setitem__)
		mapping := args[0]

		// Get the iterable
		iterable, ok := args[1].(core.Iterable)
		if !ok {
			return nil, fmt.Errorf("_count_elements() second argument must be iterable")
		}

		// Iterate and count elements
		iter := iterable.Iterator()
		for {
			elem, hasNext := iter.Next()
			if !hasNext {
				break
			}

			// Get current count for this element
			var count core.NumberValue = 0
			if getitemAttr, ok := mapping.(interface {
				GetAttr(string) (core.Value, bool)
			}); ok {
				if getitem, ok := getitemAttr.GetAttr("__getitem__"); ok {
					if callable, ok := getitem.(interface {
						Call([]core.Value, *core.Context) (core.Value, error)
					}); ok {
						if val, err := callable.Call([]core.Value{elem}, ctx); err == nil {
							if num, ok := val.(core.NumberValue); ok {
								count = num
							}
						}
					}
				}
			}

			// Set new count
			if setitemAttr, ok := mapping.(interface {
				GetAttr(string) (core.Value, bool)
			}); ok {
				if setitem, ok := setitemAttr.GetAttr("__setitem__"); ok {
					if callable, ok := setitem.(interface {
						Call([]core.Value, *core.Context) (core.Value, error)
					}); ok {
						callable.Call([]core.Value{elem, count + 1}, ctx)
					}
				}
			}
		}

		return core.Nil, nil
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
	if list, ok := arg.(*core.ListValue); ok {
		for _, item := range list.Items() {
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
	result := make([]core.Value, len(pairs))
	for i, p := range pairs {
		// Reconstruct the key as a Value
		keyVal := core.StringValue(p.key) // Simplified - assumes string keys
		result[i] = core.TupleValue{keyVal, core.NumberValue(p.value)}
	}

	return core.NewList(result...)
}

// Elements returns an iterator over elements repeating each as many times as its count
func (c *Counter) Elements() core.Value {
	result := make([]core.Value, 0)
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
	return core.NewList(result...)
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
			result := make([]core.Value, len(keys))
			for i, k := range keys {
				result[i] = core.StringValue(k)
			}
			return core.NewList(result...), nil
		}), true
	case "values":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			values := make([]core.Value, 0)
			for _, key := range dd.dict.Keys() {
				if val, ok := dd.dict.Get(key); ok {
					values = append(values, val)
				}
			}
			return core.NewList(values...), nil
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
	return fmt.Sprintf("deque(%v)", core.NewList(dq.items...).String())
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
			if list, ok := args[0].(*core.ListValue); ok {
				for _, item := range list.Items() {
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
			if list, ok := args[0].(*core.ListValue); ok {
				for _, item := range list.Items() {
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

// DequeIterator is an iterator over deque elements
type DequeIterator struct {
	deque *Deque
	index int
}

// NewDequeIterator creates a new deque iterator
func NewDequeIterator(dq *Deque) *DequeIterator {
	return &DequeIterator{
		deque: dq,
		index: 0,
	}
}

// Type implements Value.Type
func (di *DequeIterator) Type() core.Type {
	return "_deque_iterator"
}

// String implements Value.String
func (di *DequeIterator) String() string {
	return "<deque_iterator>"
}

// Iterator implements Iterable.Iterator
func (di *DequeIterator) Iterator() core.Iterator {
	return di
}

// Next implements Iterator.Next
func (di *DequeIterator) Next() (core.Value, bool) {
	if di.index >= len(di.deque.items) {
		return nil, false
	}
	val := di.deque.items[di.index]
	di.index++
	return val, true
}

// Reset implements Iterator.Reset
func (di *DequeIterator) Reset() {
	di.index = 0
}

// OrderedDict is a dictionary that maintains insertion order
// Since M28's DictValue already maintains order, this is essentially a wrapper
type OrderedDict struct {
	dict *core.DictValue
}

// NewOrderedDict creates a new OrderedDict
func NewOrderedDict() *OrderedDict {
	return &OrderedDict{
		dict: core.NewDict(),
	}
}

// Type implements Value.Type
func (od *OrderedDict) Type() core.Type {
	return "OrderedDict"
}

// String implements Value.String
func (od *OrderedDict) String() string {
	return fmt.Sprintf("OrderedDict(%s)", od.dict.String())
}

// GetAttr implements attribute access
func (od *OrderedDict) GetAttr(name string) (core.Value, bool) {
	switch name {
	case "__getitem__":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("__getitem__ requires exactly 1 argument")
			}
			key := core.ValueToKey(args[0])
			if val, ok := od.dict.Get(key); ok {
				return val, nil
			}
			return nil, fmt.Errorf("KeyError: %s", key)
		}), true
	case "__setitem__":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) != 2 {
				return nil, fmt.Errorf("__setitem__ requires exactly 2 arguments")
			}
			key := core.ValueToKey(args[0])
			od.dict.Set(key, args[1])
			return core.Nil, nil
		}), true
	case "keys":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			keys := od.dict.Keys()
			result := make([]core.Value, len(keys))
			for i, k := range keys {
				result[i] = core.StringValue(k)
			}
			return core.NewList(result...), nil
		}), true
	case "values":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			values := make([]core.Value, 0)
			for _, key := range od.dict.Keys() {
				if val, ok := od.dict.Get(key); ok {
					values = append(values, val)
				}
			}
			return core.NewList(values...), nil
		}), true
	case "items":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			items := make([]core.Value, 0)
			for _, key := range od.dict.Keys() {
				if val, ok := od.dict.Get(key); ok {
					items = append(items, core.TupleValue{core.StringValue(key), val})
				}
			}
			return core.NewList(items...), nil
		}), true
	case "get":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) < 1 {
				return nil, fmt.Errorf("get() requires at least 1 argument")
			}
			key := core.ValueToKey(args[0])
			if val, ok := od.dict.Get(key); ok {
				return val, nil
			}
			if len(args) > 1 {
				return args[1], nil // Return default
			}
			return core.None, nil
		}), true
	}
	return nil, false
}

// SetAttr implements attribute setting
func (od *OrderedDict) SetAttr(name string, value core.Value) error {
	return fmt.Errorf("cannot set attribute '%s' on OrderedDict", name)
}

// TupleGetter is a callable that retrieves a specific index from a tuple
// This is used by namedtuple for field access
type TupleGetter struct {
	index int
	doc   string
}

// NewTupleGetter creates a new TupleGetter
func NewTupleGetter(index int, doc string) *TupleGetter {
	return &TupleGetter{
		index: index,
		doc:   doc,
	}
}

// Type implements Value.Type
func (tg *TupleGetter) Type() core.Type {
	return "tuplegetter"
}

// String implements Value.String
func (tg *TupleGetter) String() string {
	return fmt.Sprintf("<tuplegetter index=%d>", tg.index)
}

// Call implements Callable.Call
func (tg *TupleGetter) Call(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("tuplegetter requires exactly 1 argument")
	}

	// Accept tuples, lists, and tuple instances (namedtuples)
	var items []core.Value
	switch v := args[0].(type) {
	case core.TupleValue:
		items = []core.Value(v)
	case *core.ListValue:
		items = v.Items()
	case *core.TupleInstance:
		items = v.Items()
	default:
		return nil, fmt.Errorf("tuplegetter argument must be a tuple or list, not %s", v.Type())
	}

	if tg.index < 0 || tg.index >= len(items) {
		return nil, fmt.Errorf("tuple index out of range")
	}

	return items[tg.index], nil
}

// GetAttr implements attribute access
func (tg *TupleGetter) GetAttr(name string) (core.Value, bool) {
	switch name {
	case "__doc__":
		if tg.doc != "" {
			return core.StringValue(tg.doc), true
		}
		return core.None, true
	case "__get__":
		// Implement descriptor protocol
		// __get__(self, instance, owner) -> value
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			// args[0] is self (the TupleGetter)
			// args[1] is instance (the namedtuple instance or None)
			// args[2] is owner (the namedtuple class) - optional
			if len(args) < 2 {
				return nil, fmt.Errorf("__get__ requires at least 2 arguments")
			}

			instance := args[1]

			// If instance is None/Nil or is a Class, return the descriptor itself
			// This happens when accessing the attribute from the class (e.g., Point.x)
			// rather than from an instance (e.g., p.x)
			if instance == core.None || instance == core.Nil {
				return tg, nil
			}
			if _, isClass := instance.(*core.Class); isClass {
				return tg, nil
			}

			// Call the tuplegetter with the instance
			return tg.Call([]core.Value{instance}, ctx)
		}), true
	}
	return nil, false
}

// SetAttr implements attribute setting
func (tg *TupleGetter) SetAttr(name string, value core.Value) error {
	return fmt.Errorf("cannot set attribute '%s' on tuplegetter", name)
}
