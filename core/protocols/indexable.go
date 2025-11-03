package protocols

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// ListIndexable adapts ListValue to Indexable protocol
type ListIndexable struct {
	list *core.ListValue
}

// NewListIndexable creates an Indexable adapter for lists
func NewListIndexable(list *core.ListValue) Indexable {
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
		i = l.list.Len() + i
	}

	if i < 0 || i >= l.list.Len() {
		return nil, &core.IndexError{Index: i, Length: l.list.Len()}
	}

	return l.list.Items()[i], nil
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
		i = l.list.Len() + i
	}

	return i >= 0 && i < l.list.Len()
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

// GetIndex retrieves character at index (rune-based, not byte-based)
func (s *StringIndexable) GetIndex(index core.Value) (core.Value, error) {
	idx, ok := index.(core.NumberValue)
	if !ok {
		return nil, fmt.Errorf("string indices must be integers, not %s", index.Type())
	}

	// Convert string to rune slice for proper Unicode indexing
	runes := []rune(s.str)
	runeLen := len(runes)

	i := int(idx)
	if i < 0 {
		i = runeLen + i
	}

	if i < 0 || i >= runeLen {
		return nil, &core.IndexError{Index: i, Length: runeLen}
	}

	return core.StringValue(string(runes[i])), nil
}

// SetIndex returns error as strings are immutable
func (s *StringIndexable) SetIndex(index, value core.Value) error {
	return fmt.Errorf("'str' object does not support item assignment")
}

// HasIndex checks if index exists (rune-based)
func (s *StringIndexable) HasIndex(index core.Value) bool {
	idx, ok := index.(core.NumberValue)
	if !ok {
		return false
	}

	// Use rune length for proper Unicode indexing
	runeLen := len([]rune(s.str))

	i := int(idx)
	if i < 0 {
		i = runeLen + i
	}

	return i >= 0 && i < runeLen
}

// DeleteIndex returns error as strings are immutable
func (s *StringIndexable) DeleteIndex(index core.Value) error {
	return fmt.Errorf("'str' object doesn't support item deletion")
}

// BytesIndexable adapts BytesValue to Indexable protocol
type BytesIndexable struct {
	bytes core.BytesValue
}

// NewBytesIndexable creates an Indexable adapter for bytes
func NewBytesIndexable(bytes core.BytesValue) Indexable {
	return &BytesIndexable{bytes: bytes}
}

// GetIndex retrieves byte at index
func (b *BytesIndexable) GetIndex(index core.Value) (core.Value, error) {
	idx, ok := index.(core.NumberValue)
	if !ok {
		return nil, fmt.Errorf("bytes indices must be integers, not %s", index.Type())
	}

	return b.bytes.GetItem(int(idx))
}

// SetIndex returns error as bytes are immutable
func (b *BytesIndexable) SetIndex(index, value core.Value) error {
	return fmt.Errorf("'bytes' object does not support item assignment")
}

// HasIndex checks if index exists
func (b *BytesIndexable) HasIndex(index core.Value) bool {
	idx, ok := index.(core.NumberValue)
	if !ok {
		return false
	}

	i := int(idx)
	if i < 0 {
		i = len(b.bytes) + i
	}

	return i >= 0 && i < len(b.bytes)
}

// DeleteIndex returns error as bytes are immutable
func (b *BytesIndexable) DeleteIndex(index core.Value) error {
	return fmt.Errorf("'bytes' object does not support item deletion")
}

// ByteArrayIndexable adapts ByteArrayValue to Indexable protocol
type ByteArrayIndexable struct {
	bytearray *core.ByteArrayValue
}

// NewByteArrayIndexable creates an Indexable adapter for bytearray
func NewByteArrayIndexable(bytearray *core.ByteArrayValue) Indexable {
	return &ByteArrayIndexable{bytearray: bytearray}
}

// GetIndex retrieves byte at index
func (b *ByteArrayIndexable) GetIndex(index core.Value) (core.Value, error) {
	idx, ok := index.(core.NumberValue)
	if !ok {
		return nil, fmt.Errorf("bytearray indices must be integers, not %s", index.Type())
	}

	return b.bytearray.GetItem(int(idx))
}

// SetIndex sets byte at index
func (b *ByteArrayIndexable) SetIndex(index, value core.Value) error {
	idx, ok := index.(core.NumberValue)
	if !ok {
		return fmt.Errorf("bytearray indices must be integers, not %s", index.Type())
	}

	return b.bytearray.SetItem(int(idx), value)
}

// HasIndex checks if index exists
func (b *ByteArrayIndexable) HasIndex(index core.Value) bool {
	idx, ok := index.(core.NumberValue)
	if !ok {
		return false
	}

	i := int(idx)
	data := b.bytearray.GetData()
	if i < 0 {
		i = len(data) + i
	}

	return i >= 0 && i < len(data)
}

// DeleteIndex returns error as bytearray doesn't support item deletion
func (b *ByteArrayIndexable) DeleteIndex(index core.Value) error {
	return fmt.Errorf("'bytearray' object doesn't support item deletion")
}

// DunderIndexable wraps objects with indexing dunder methods
type DunderIndexable struct {
	obj core.Object
	ctx *core.Context
}

// NewDunderIndexable creates an Indexable wrapper for an object with indexing methods
func NewDunderIndexable(obj core.Object, ctx *core.Context) *DunderIndexable {
	return &DunderIndexable{obj: obj, ctx: ctx}
}

// callDunder is a helper to call a dunder method on the wrapped object
func (d *DunderIndexable) callDunder(method string, args []core.Value) (core.Value, error) {
	methodVal, exists := d.obj.GetAttr(method)
	if !exists {
		return nil, fmt.Errorf("object has no %s method", method)
	}

	callable, ok := methodVal.(interface {
		Call([]core.Value, *core.Context) (core.Value, error)
	})
	if !ok {
		return nil, fmt.Errorf("%s is not callable", method)
	}

	return callable.Call(args, d.ctx)
}

// GetIndex implements Indexable.GetIndex by calling __getitem__
func (d *DunderIndexable) GetIndex(index core.Value) (core.Value, error) {
	return d.callDunder("__getitem__", []core.Value{index})
}

// SetIndex implements Indexable.SetIndex by calling __setitem__
func (d *DunderIndexable) SetIndex(index, value core.Value) error {
	_, err := d.callDunder("__setitem__", []core.Value{index, value})
	return err
}

// HasIndex implements Indexable.HasIndex by attempting GetIndex
func (d *DunderIndexable) HasIndex(index core.Value) bool {
	// Try to get the item - if it succeeds, the index exists
	_, err := d.GetIndex(index)
	return err == nil
}

// DeleteIndex implements Indexable.DeleteIndex by calling __delitem__
func (d *DunderIndexable) DeleteIndex(index core.Value) error {
	_, err := d.callDunder("__delitem__", []core.Value{index})
	return err
}

// GetIndexableOps returns an Indexable implementation for a value if possible
func GetIndexableOps(v core.Value) (Indexable, bool) {
	switch val := v.(type) {
	case *core.ListValue:
		return NewListIndexable(val), true
	case *core.DictValue:
		return NewDictIndexable(val), true
	case core.TupleValue:
		return NewTupleIndexable(val), true
	case core.StringValue:
		return NewStringIndexable(val), true
	case core.BytesValue:
		return NewBytesIndexable(val), true
	case *core.ByteArrayValue:
		return NewByteArrayIndexable(val), true
	default:
		// Check if value implements Indexable directly
		if indexable, ok := v.(Indexable); ok {
			return indexable, true
		}
		// Check if value has __getitem__ method
		if obj, ok := v.(core.Object); ok {
			if _, exists := obj.GetAttr("__getitem__"); exists {
				// Return a DunderIndexable wrapper
				return NewDunderIndexable(obj, nil), true
			}
		}
		return nil, false
	}
}
