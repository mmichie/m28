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
	default:
		// Check if value implements Iterator directly
		if iter, ok := v.(Iterator); ok {
			return iter, true
		}
		// Check if value has __iter__ method
		if obj, ok := v.(core.Object); ok {
			if _, exists := obj.GetAttr("__iter__"); exists {
				// TODO: Return a DunderIterator wrapper
				return nil, false
			}
		}
		return nil, false
	}
}
