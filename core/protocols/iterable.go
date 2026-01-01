package protocols

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// StopIteration is raised when an iterator has no more values
type StopIteration struct {
	Message string
}

func (s *StopIteration) Error() string {
	if s.Message != "" {
		return fmt.Sprintf("StopIteration: %s", s.Message)
	}
	return "StopIteration"
}

// IteratorAdapter wraps an Iterator to handle StopIteration
type IteratorAdapter struct {
	iter Iterator
}

// Next returns the next value or raises StopIteration
func (i *IteratorAdapter) Next() (core.Value, error) {
	if i.iter.HasNext() {
		val, err := i.iter.Next()
		if err != nil {
			return nil, err
		}
		return val, nil
	}
	return nil, &StopIteration{}
}

// HasNext checks if there are more values
func (i *IteratorAdapter) HasNext() bool {
	return i.iter.HasNext()
}

// ListIterator iterates over list elements
type ListIterator struct {
	list  *core.ListValue
	index int
}

// NewListIterator creates an iterator for lists
func NewListIterator(list *core.ListValue) *ListIterator {
	return &ListIterator{list: list, index: 0}
}

// Next returns the next value
func (l *ListIterator) Next() (core.Value, error) {
	if l.index < l.list.Len() {
		val := l.list.Items()[l.index]
		l.index++
		return val, nil
	}
	return nil, &StopIteration{}
}

// HasNext checks if there are more values
func (l *ListIterator) HasNext() bool {
	return l.index < l.list.Len()
}

// Type implements Value.Type
func (l *ListIterator) Type() core.Type {
	return "list_iterator"
}

// String implements Value.String
func (l *ListIterator) String() string {
	return fmt.Sprintf("<list_iterator at %d>", l.index)
}

// GetAttr implements Object interface for iterator protocol
func (l *ListIterator) GetAttr(name string) (core.Value, bool) {
	if name == "__next__" {
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return l.Next()
		}), true
	}
	return nil, false
}

// Iterator makes ListIterator implement core.Iterable
// In Python, calling iter() on an iterator returns the iterator itself
func (l *ListIterator) Iterator() core.Iterator {
	return &listIteratorAdapter{iter: l}
}

// listIteratorAdapter adapts protocols.ListIterator to core.Iterator
type listIteratorAdapter struct {
	iter *ListIterator
}

func (a *listIteratorAdapter) Next() (core.Value, bool) {
	val, err := a.iter.Next()
	if err != nil {
		return nil, false
	}
	return val, true
}

func (a *listIteratorAdapter) Reset() {
	a.iter.index = 0
}

// DictIterator iterates over dict keys (Python behavior)
type DictIterator struct {
	dict  *core.DictValue
	keys  []core.Value
	index int
}

// NewDictIterator creates an iterator for dicts
func NewDictIterator(dict *core.DictValue) *DictIterator {
	return &DictIterator{
		dict:  dict,
		keys:  dict.OriginalKeys(),
		index: 0,
	}
}

// Next returns the next key
func (d *DictIterator) Next() (core.Value, error) {
	if d.index < len(d.keys) {
		key := d.keys[d.index]
		d.index++
		return key, nil
	}
	return nil, &StopIteration{}
}

// HasNext checks if there are more keys
func (d *DictIterator) HasNext() bool {
	return d.index < len(d.keys)
}

// Type implements Value.Type
func (d *DictIterator) Type() core.Type {
	return "dict_keyiterator"
}

// String implements Value.String
func (d *DictIterator) String() string {
	return "<dict_keyiterator>"
}

// GetAttr implements Object interface for iterator protocol
func (d *DictIterator) GetAttr(name string) (core.Value, bool) {
	if name == "__next__" {
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return d.Next()
		}), true
	}
	return nil, false
}

// TupleIterator iterates over tuple elements
type TupleIterator struct {
	tuple core.TupleValue
	index int
}

// NewTupleIterator creates an iterator for tuples
func NewTupleIterator(tuple core.TupleValue) *TupleIterator {
	return &TupleIterator{tuple: tuple, index: 0}
}

// Next returns the next value
func (t *TupleIterator) Next() (core.Value, error) {
	if t.index < len(t.tuple) {
		val := t.tuple[t.index]
		t.index++
		return val, nil
	}
	return nil, &StopIteration{}
}

// HasNext checks if there are more values
func (t *TupleIterator) HasNext() bool {
	return t.index < len(t.tuple)
}

// Type implements Value.Type
func (t *TupleIterator) Type() core.Type {
	return "tuple_iterator"
}

// String implements Value.String
func (t *TupleIterator) String() string {
	return fmt.Sprintf("<tuple_iterator at %d>", t.index)
}

// GetAttr implements Object interface for iterator protocol
func (t *TupleIterator) GetAttr(name string) (core.Value, bool) {
	if name == "__next__" {
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return t.Next()
		}), true
	}
	return nil, false
}

// StringIterator iterates over string characters
type StringIterator struct {
	str   string
	runes []rune
	index int
}

// NewStringIterator creates an iterator for strings
func NewStringIterator(str core.StringValue) *StringIterator {
	runes := []rune(string(str))
	return &StringIterator{
		str:   string(str),
		runes: runes,
		index: 0,
	}
}

// Next returns the next character
func (s *StringIterator) Next() (core.Value, error) {
	if s.index < len(s.runes) {
		char := s.runes[s.index]
		s.index++
		return core.StringValue(string(char)), nil
	}
	return nil, &StopIteration{}
}

// HasNext checks if there are more characters
func (s *StringIterator) HasNext() bool {
	return s.index < len(s.runes)
}

// Type implements Value.Type
func (s *StringIterator) Type() core.Type {
	return "str_iterator"
}

// String implements Value.String
func (s *StringIterator) String() string {
	return "<str_iterator>"
}

// GetAttr implements Object interface for iterator protocol
func (s *StringIterator) GetAttr(name string) (core.Value, bool) {
	if name == "__next__" {
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return s.Next()
		}), true
	}
	return nil, false
}

// BytesIterator iterates over bytes
type BytesIterator struct {
	bytes []byte
	index int
}

// NewBytesIterator creates an iterator for bytes
func NewBytesIterator(b core.BytesValue) *BytesIterator {
	return &BytesIterator{
		bytes: []byte(b),
		index: 0,
	}
}

// Next returns the next byte as a NumberValue
func (b *BytesIterator) Next() (core.Value, error) {
	if b.index < len(b.bytes) {
		val := core.NumberValue(b.bytes[b.index])
		b.index++
		return val, nil
	}
	return nil, &StopIteration{}
}

// HasNext checks if there are more bytes
func (b *BytesIterator) HasNext() bool {
	return b.index < len(b.bytes)
}

// Type implements Value.Type
func (b *BytesIterator) Type() core.Type {
	return "bytes_iterator"
}

// String implements Value.String
func (b *BytesIterator) String() string {
	return "<bytes_iterator>"
}

// GetAttr implements Object interface for iterator protocol
func (b *BytesIterator) GetAttr(name string) (core.Value, bool) {
	if name == "__next__" {
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return b.Next()
		}), true
	}
	return nil, false
}

// ByteArrayIterator iterates over bytearray
type ByteArrayIterator struct {
	bytearray *core.ByteArrayValue
	index     int
}

// NewByteArrayIterator creates an iterator for bytearray
func NewByteArrayIterator(ba *core.ByteArrayValue) *ByteArrayIterator {
	return &ByteArrayIterator{
		bytearray: ba,
		index:     0,
	}
}

// Next returns the next byte as a NumberValue
func (b *ByteArrayIterator) Next() (core.Value, error) {
	data := b.bytearray.GetData()
	if b.index < len(data) {
		val := core.NumberValue(data[b.index])
		b.index++
		return val, nil
	}
	return nil, &StopIteration{}
}

// HasNext checks if there are more bytes
func (b *ByteArrayIterator) HasNext() bool {
	return b.index < len(b.bytearray.GetData())
}

// Type implements Value.Type
func (b *ByteArrayIterator) Type() core.Type {
	return "bytearray_iterator"
}

// String implements Value.String
func (b *ByteArrayIterator) String() string {
	return "<bytearray_iterator>"
}

// GetAttr implements Object interface for iterator protocol
func (b *ByteArrayIterator) GetAttr(name string) (core.Value, bool) {
	if name == "__next__" {
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return b.Next()
		}), true
	}
	return nil, false
}

// RangeIterator iterates over range values
type RangeIterator struct {
	rang    *core.RangeValue
	current float64
}

// NewRangeIterator creates an iterator for ranges
func NewRangeIterator(rang *core.RangeValue) *RangeIterator {
	return &RangeIterator{
		rang:    rang,
		current: rang.Start,
	}
}

// Next returns the next value
func (r *RangeIterator) Next() (core.Value, error) {
	if r.rang.Step > 0 {
		if r.current >= r.rang.Stop {
			return nil, &StopIteration{}
		}
	} else {
		if r.current <= r.rang.Stop {
			return nil, &StopIteration{}
		}
	}

	val := core.NumberValue(r.current)
	r.current += r.rang.Step
	return val, nil
}

// HasNext checks if there are more values
func (r *RangeIterator) HasNext() bool {
	if r.rang.Step > 0 {
		return r.current < r.rang.Stop
	} else {
		return r.current > r.rang.Stop
	}
}

// Type implements Value.Type
func (r *RangeIterator) Type() core.Type {
	return "range_iterator"
}

// String implements Value.String
func (r *RangeIterator) String() string {
	return "<range_iterator>"
}

// GetAttr implements Object interface for iterator protocol
func (r *RangeIterator) GetAttr(name string) (core.Value, bool) {
	if name == "__next__" {
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return r.Next()
		}), true
	}
	return nil, false
}

// SetIterator iterates over set values
type SetIterator struct {
	items []core.Value
	index int
}

// NewSetIterator creates an iterator for sets
func NewSetIterator(s *core.SetValue) *SetIterator {
	return &SetIterator{
		items: s.Items(),
		index: 0,
	}
}

// Next returns the next value
func (s *SetIterator) Next() (core.Value, error) {
	if s.index < len(s.items) {
		val := s.items[s.index]
		s.index++
		return val, nil
	}
	return nil, &StopIteration{}
}

// HasNext checks if there are more values
func (s *SetIterator) HasNext() bool {
	return s.index < len(s.items)
}

// Type implements Value.Type
func (s *SetIterator) Type() core.Type {
	return "set_iterator"
}

// String implements Value.String
func (s *SetIterator) String() string {
	return "<set_iterator>"
}

// GetAttr implements Object interface for iterator protocol
func (s *SetIterator) GetAttr(name string) (core.Value, bool) {
	if name == "__next__" {
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return s.Next()
		}), true
	}
	return nil, false
}

// DunderIterator wraps objects with __iter__/__next__ methods
type DunderIterator struct {
	iterator   core.Value    // The iterator returned by __iter__
	ctx        *core.Context // Context for method calls
	exhausted  bool          // Whether the iterator is exhausted
	cachedNext core.Value    // Cached next value for HasNext
	cacheValid bool          // Whether cached value is valid
}

// NewDunderIterator creates an iterator from an object with __iter__
func NewDunderIterator(obj core.Value, ctx *core.Context) (*DunderIterator, error) {
	// Call __iter__ to get the iterator
	iterObj, ok := obj.(core.Object)
	if !ok {
		return nil, fmt.Errorf("object does not support iteration")
	}

	iterMethod, exists := iterObj.GetAttr("__iter__")
	if !exists {
		return nil, fmt.Errorf("object has no __iter__ method")
	}

	callable, ok := iterMethod.(interface {
		Call([]core.Value, *core.Context) (core.Value, error)
	})
	if !ok {
		return nil, fmt.Errorf("__iter__ is not callable")
	}

	// Call __iter__() to get the iterator object
	iterator, err := callable.Call([]core.Value{}, ctx)
	if err != nil {
		return nil, err
	}

	// Verify the iterator has __next__ method
	iteratorObj, ok := iterator.(core.Object)
	if !ok {
		return nil, fmt.Errorf("__iter__ returned non-object type %T (cannot check for __next__)", iterator)
	}

	if _, hasNext := iteratorObj.GetAttr("__next__"); !hasNext {
		return nil, &core.TypeError{Message: fmt.Sprintf("iter() returned non-iterator of type '%s'", iterator.Type())}
	}

	return &DunderIterator{
		iterator:   iterator,
		ctx:        ctx,
		exhausted:  false,
		cacheValid: false,
	}, nil
}

// Next returns the next value or StopIteration
func (d *DunderIterator) Next() (core.Value, error) {
	if d.exhausted {
		return nil, &StopIteration{}
	}

	// If we have a cached value from HasNext, return it
	if d.cacheValid {
		d.cacheValid = false
		return d.cachedNext, nil
	}

	// Call __next__ on the iterator
	iterObj, ok := d.iterator.(core.Object)
	if !ok {
		d.exhausted = true
		return nil, &StopIteration{}
	}

	nextMethod, exists := iterObj.GetAttr("__next__")
	if !exists {
		d.exhausted = true
		return nil, &StopIteration{}
	}

	callable, ok := nextMethod.(interface {
		Call([]core.Value, *core.Context) (core.Value, error)
	})
	if !ok {
		return nil, fmt.Errorf("__next__ is not callable")
	}

	val, err := callable.Call([]core.Value{}, d.ctx)
	if err != nil {
		// Check if it's a StopIteration
		if _, ok := err.(*StopIteration); ok {
			d.exhausted = true
			return nil, err
		}
		// Other errors are propagated
		return nil, err
	}

	return val, nil
}

// HasNext checks if there are more values by attempting to fetch the next one
func (d *DunderIterator) HasNext() bool {
	if d.exhausted {
		return false
	}

	// If we already have a cached value, we know there's a next
	if d.cacheValid {
		return true
	}

	// Try to get the next value and cache it
	val, err := d.Next()
	if err != nil {
		// StopIteration or any error means no more values
		d.exhausted = true
		return false
	}

	// Cache the value for the actual Next() call
	d.cachedNext = val
	d.cacheValid = true
	return true
}

// Type implements Value.Type
func (d *DunderIterator) Type() core.Type {
	return "dunder_iterator"
}

// String implements Value.String
func (d *DunderIterator) String() string {
	return "<dunder_iterator>"
}

// GetIterableOps returns an iterator for a value if possible
func GetIterableOps(v core.Value) (Iterator, bool) {
	return GetIterableOpsWithCtx(v, nil)
}

// GetIterableOpsWithCtx returns an iterator for a value with context support
func GetIterableOpsWithCtx(v core.Value, ctx *core.Context) (Iterator, bool) {
	switch val := v.(type) {
	case *core.ListValue:
		return NewListIterator(val), true
	case *core.DictValue:
		return NewDictIterator(val), true
	case *core.SetValue:
		return NewSetIterator(val), true
	case core.TupleValue:
		return NewTupleIterator(val), true
	case core.StringValue:
		return NewStringIterator(val), true
	case core.BytesValue:
		return NewBytesIterator(val), true
	case *core.ByteArrayValue:
		return NewByteArrayIterator(val), true
	case *core.RangeValue:
		return NewRangeIterator(val), true
	default:
		// Check if value implements Iterator directly
		if iter, ok := v.(Iterator); ok {
			return iter, true
		}
		// Check if value has __iter__ method
		if obj, ok := v.(core.Object); ok {
			if _, exists := obj.GetAttr("__iter__"); exists {
				// Return a DunderIterator wrapper
				iter, err := NewDunderIterator(v, ctx)
				if err == nil {
					return iter, true
				}
			}
		}
		return nil, false
	}
}

// EnumerateIterator provides lazy iteration with index
type EnumerateIterator struct {
	iter  Iterator
	index int
}

// NewEnumerateIterator creates an enumerate iterator
func NewEnumerateIterator(iter Iterator, start int) *EnumerateIterator {
	return &EnumerateIterator{
		iter:  iter,
		index: start,
	}
}

// Next returns the next (index, value) tuple
func (e *EnumerateIterator) Next() (core.Value, error) {
	val, err := e.iter.Next()
	if err != nil {
		return nil, err
	}
	result := core.TupleValue{core.NumberValue(e.index), val}
	e.index++
	return result, nil
}

// HasNext checks if there are more values
func (e *EnumerateIterator) HasNext() bool {
	return e.iter.HasNext()
}

// Type implements Value.Type
func (e *EnumerateIterator) Type() core.Type {
	return "enumerate"
}

// String implements Value.String
func (e *EnumerateIterator) String() string {
	return "<enumerate object>"
}

// GetAttr implements Object interface for iterator protocol
func (e *EnumerateIterator) GetAttr(name string) (core.Value, bool) {
	switch name {
	case "__next__":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return e.Next()
		}), true
	case "__iter__":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return e, nil
		}), true
	}
	return nil, false
}
