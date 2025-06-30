package protocols

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// ListIndexable adapts ListValue to Indexable protocol
type ListIndexable struct {
	list core.ListValue
}

// NewListIndexable creates an Indexable adapter for lists
func NewListIndexable(list core.ListValue) Indexable {
	return &ListIndexable{list: list}
}

// GetIndex retrieves value at index
func (l *ListIndexable) GetIndex(index core.Value) (core.Value, error) {
	idx, ok := index.(core.NumberValue)
	if !ok {
		return nil, fmt.Errorf("list indices must be integers, not %s", index.Type())
	}

	i := int(idx)
	if i < 0 {
		i = len(l.list) + i
	}

	if i < 0 || i >= len(l.list) {
		return nil, &core.IndexError{Index: i, Length: len(l.list)}
	}

	return l.list[i], nil
}

// SetIndex returns error as lists are immutable
func (l *ListIndexable) SetIndex(index, value core.Value) error {
	return fmt.Errorf("'list' object does not support item assignment")
}

// HasIndex checks if index exists
func (l *ListIndexable) HasIndex(index core.Value) bool {
	idx, ok := index.(core.NumberValue)
	if !ok {
		return false
	}

	i := int(idx)
	if i < 0 {
		i = len(l.list) + i
	}

	return i >= 0 && i < len(l.list)
}

// DeleteIndex returns error as lists are immutable
func (l *ListIndexable) DeleteIndex(index core.Value) error {
	return fmt.Errorf("'list' object does not support item deletion")
}

// DictIndexable adapts DictValue to Indexable protocol
type DictIndexable struct {
	dict *core.DictValue
}

// NewDictIndexable creates an Indexable adapter for dictionaries
func NewDictIndexable(dict *core.DictValue) Indexable {
	return &DictIndexable{dict: dict}
}

// GetIndex retrieves value by key
func (d *DictIndexable) GetIndex(key core.Value) (core.Value, error) {
	if !core.IsHashable(key) {
		return nil, fmt.Errorf("unhashable type: '%s'", key.Type())
	}

	keyStr := core.ValueToKey(key)
	if val, exists := d.dict.Get(keyStr); exists {
		return val, nil
	}

	return nil, &core.KeyError{Key: key}
}

// SetIndex sets value by key
func (d *DictIndexable) SetIndex(key, value core.Value) error {
	if !core.IsHashable(key) {
		return fmt.Errorf("unhashable type: '%s'", key.Type())
	}

	keyStr := core.ValueToKey(key)
	d.dict.SetWithKey(keyStr, key, value)
	return nil
}

// HasIndex checks if key exists
func (d *DictIndexable) HasIndex(key core.Value) bool {
	if !core.IsHashable(key) {
		return false
	}

	keyStr := core.ValueToKey(key)
	_, exists := d.dict.Get(keyStr)
	return exists
}

// DeleteIndex removes key-value pair
func (d *DictIndexable) DeleteIndex(key core.Value) error {
	if !core.IsHashable(key) {
		return fmt.Errorf("unhashable type: '%s'", key.Type())
	}

	keyStr := core.ValueToKey(key)
	if _, exists := d.dict.Get(keyStr); !exists {
		return &core.KeyError{Key: key}
	}

	d.dict.Delete(keyStr)
	return nil
}

// TupleIndexable adapts TupleValue to Indexable protocol (read-only)
type TupleIndexable struct {
	tuple core.TupleValue
}

// NewTupleIndexable creates an Indexable adapter for tuples
func NewTupleIndexable(tuple core.TupleValue) Indexable {
	return &TupleIndexable{tuple: tuple}
}

// GetIndex retrieves value at index
func (t *TupleIndexable) GetIndex(index core.Value) (core.Value, error) {
	idx, ok := index.(core.NumberValue)
	if !ok {
		return nil, fmt.Errorf("tuple indices must be integers, not %s", index.Type())
	}

	i := int(idx)
	if i < 0 {
		i = len(t.tuple) + i
	}

	if i < 0 || i >= len(t.tuple) {
		return nil, &core.IndexError{Index: i, Length: len(t.tuple)}
	}

	return t.tuple[i], nil
}

// SetIndex returns error as tuples are immutable
func (t *TupleIndexable) SetIndex(index, value core.Value) error {
	return fmt.Errorf("'tuple' object does not support item assignment")
}

// HasIndex checks if index exists
func (t *TupleIndexable) HasIndex(index core.Value) bool {
	idx, ok := index.(core.NumberValue)
	if !ok {
		return false
	}

	i := int(idx)
	if i < 0 {
		i = len(t.tuple) + i
	}

	return i >= 0 && i < len(t.tuple)
}

// DeleteIndex returns error as tuples are immutable
func (t *TupleIndexable) DeleteIndex(index core.Value) error {
	return fmt.Errorf("'tuple' object doesn't support item deletion")
}

// StringIndexable adapts StringValue to Indexable protocol (read-only)
type StringIndexable struct {
	str string
}

// NewStringIndexable creates an Indexable adapter for strings
func NewStringIndexable(str core.StringValue) Indexable {
	return &StringIndexable{str: string(str)}
}

// GetIndex retrieves character at index
func (s *StringIndexable) GetIndex(index core.Value) (core.Value, error) {
	idx, ok := index.(core.NumberValue)
	if !ok {
		return nil, fmt.Errorf("string indices must be integers, not %s", index.Type())
	}

	i := int(idx)
	if i < 0 {
		i = len(s.str) + i
	}

	if i < 0 || i >= len(s.str) {
		return nil, &core.IndexError{Index: i, Length: len(s.str)}
	}

	return core.StringValue(s.str[i : i+1]), nil
}

// SetIndex returns error as strings are immutable
func (s *StringIndexable) SetIndex(index, value core.Value) error {
	return fmt.Errorf("'str' object does not support item assignment")
}

// HasIndex checks if index exists
func (s *StringIndexable) HasIndex(index core.Value) bool {
	idx, ok := index.(core.NumberValue)
	if !ok {
		return false
	}

	i := int(idx)
	if i < 0 {
		i = len(s.str) + i
	}

	return i >= 0 && i < len(s.str)
}

// DeleteIndex returns error as strings are immutable
func (s *StringIndexable) DeleteIndex(index core.Value) error {
	return fmt.Errorf("'str' object doesn't support item deletion")
}

// GetIndexableOps returns an Indexable implementation for a value if possible
func GetIndexableOps(v core.Value) (Indexable, bool) {
	switch val := v.(type) {
	case core.ListValue:
		return NewListIndexable(val), true
	case *core.DictValue:
		return NewDictIndexable(val), true
	case core.TupleValue:
		return NewTupleIndexable(val), true
	case core.StringValue:
		return NewStringIndexable(val), true
	default:
		// Check if value implements Indexable directly
		if indexable, ok := v.(Indexable); ok {
			return indexable, true
		}
		// Check if value has __getitem__ method
		if obj, ok := v.(core.Object); ok {
			if _, exists := obj.GetAttr("__getitem__"); exists {
				// TODO: Return a DunderIndexable wrapper
				return nil, false
			}
		}
		return nil, false
	}
}
