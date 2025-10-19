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
	list  core.ListValue
	index int
}

// NewListIterator creates an iterator for lists
func NewListIterator(list core.ListValue) *ListIterator {
	return &ListIterator{list: list, index: 0}
}

// Next returns the next value
func (l *ListIterator) Next() (core.Value, error) {
	if l.index < len(l.list) {
		val := l.list[l.index]
		l.index++
		return val, nil
	}
	return nil, &StopIteration{}
}

// HasNext checks if there are more values
func (l *ListIterator) HasNext() bool {
	return l.index < len(l.list)
}

// Type implements Value.Type
func (l *ListIterator) Type() core.Type {
	return "list_iterator"
}

// String implements Value.String
func (l *ListIterator) String() string {
	return fmt.Sprintf("<list_iterator at %d>", l.index)
}

// DictIterator iterates over dict keys (Python behavior)
type DictIterator struct {
	dict  *core.DictValue
	keys  []string
	index int
}

// NewDictIterator creates an iterator for dicts
func NewDictIterator(dict *core.DictValue) *DictIterator {
	return &DictIterator{
		dict:  dict,
		keys:  dict.Keys(),
		index: 0,
	}
}

// Next returns the next key
func (d *DictIterator) Next() (core.Value, error) {
	if d.index < len(d.keys) {
		key := d.keys[d.index]
		d.index++
		// Try to get the original key value
		// Note: DictValue.Keys() returns internal string representations
		// We need to return the original key values when iterating
		// For now, just return the string key
		return core.StringValue(key), nil
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
		return nil, fmt.Errorf("__iter__ did not return an object with __next__")
	}

	if _, hasNext := iteratorObj.GetAttr("__next__"); !hasNext {
		return nil, fmt.Errorf("iterator has no __next__ method")
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
	switch val := v.(type) {
	case core.ListValue:
		return NewListIterator(val), true
	case *core.DictValue:
		return NewDictIterator(val), true
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
				// Note: We need a context, but don't have one here
				// For now, create with nil context - callers should handle this
				iter, err := NewDunderIterator(v, nil)
				if err == nil {
					return iter, true
				}
			}
		}
		return nil, false
	}
}
