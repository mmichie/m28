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

// GetAttr implements Object interface using TypeDescriptor
func (l ListValue) GetAttr(name string) (Value, bool) {
	desc := GetTypeDescriptor(ListType)
	if desc != nil {
		val, err := desc.GetAttribute(l, name)
		if err == nil {
			return val, true
		}
	}
	return nil, false
}

// SetAttr implements Object.SetAttr (not supported for lists)
func (l ListValue) SetAttr(name string, value Value) error {
	return fmt.Errorf("cannot set attributes on lists")
}

// CallMethod implements Object.CallMethod
func (l ListValue) CallMethod(name string, args []Value, ctx *Context) (Value, error) {
	desc := GetTypeDescriptor(ListType)
	if desc != nil {
		return desc.CallMethod(l, name, args, ctx)
	}
	return nil, fmt.Errorf("list has no method named %s", name)
}

// GetItem gets an item by index
func (l ListValue) GetItem(index int) (Value, error) {
	if index < 0 {
		index = len(l) + index
	}
	if index < 0 || index >= len(l) {
		return nil, &IndexError{Index: index, Length: len(l)}
	}
	return l[index], nil
}

// SetItem sets an item by index
func (l ListValue) SetItem(index int, value Value) error {
	if index < 0 {
		index = len(l) + index
	}
	if index < 0 || index >= len(l) {
		return &IndexError{Index: index, Length: len(l)}
	}
	l[index] = value
	return nil
}

// Iterator implements Iterable
func (l ListValue) Iterator() Iterator {
	return &listIterator{
		list:  l,
		index: 0,
	}
}

type listIterator struct {
	list  ListValue
	index int
}

func (it *listIterator) Next() (Value, bool) {
	if it.index >= len(it.list) {
		return nil, false
	}
	val := it.list[it.index]
	it.index++
	return val, true
}

func (it *listIterator) Reset() {
	it.index = 0
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

// Type implements Value.Type
func (d *DictValue) Type() Type {
	return DictType
}

// String implements Value.String
func (d *DictValue) String() string {
	if len(d.entries) == 0 {
		return "{}"
	}

	// Sort keys for consistent output
	keys := make([]string, 0, len(d.entries))
	for k := range d.entries {
		keys = append(keys, k)
	}
	sort.Strings(keys)

	parts := make([]string, 0, len(keys))
	for _, k := range keys {
		v := d.entries[k]
		parts = append(parts, fmt.Sprintf("%q: %s", k, PrintValue(v)))
	}

	return "{" + strings.Join(parts, ", ") + "}"
}

// Get retrieves a value by key
func (d *DictValue) Get(key string) (Value, bool) {
	val, ok := d.entries[key]
	return val, ok
}

// Set sets a value by key
func (d *DictValue) Set(key string, value Value) {
	d.entries[key] = value
}

// Delete removes a key
func (d *DictValue) Delete(key string) {
	delete(d.entries, key)
}

// Keys returns all keys
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

// GetAttr implements Object interface using TypeDescriptor
func (d *DictValue) GetAttr(name string) (Value, bool) {
	// First check dictionary entries (for key access)
	if val, exists := d.Get(name); exists {
		return val, true
	}
	
	// Then check TypeDescriptor for methods
	desc := GetTypeDescriptor(DictType)
	if desc != nil {
		val, err := desc.GetAttribute(d, name)
		if err == nil {
			return val, true
		}
	}
	
	// Finally check BaseObject
	return d.BaseObject.GetAttr(name)
}

// SetAttr implements Object.SetAttr for dictionary key assignment
func (d *DictValue) SetAttr(name string, value Value) error {
	// Set as dictionary entry
	d.Set(name, value)
	return nil
}

// TupleValue represents an immutable sequence
type TupleValue []Value

// Type implements Value.Type
func (t TupleValue) Type() Type {
	return TupleType
}

// String implements Value.String
func (t TupleValue) String() string {
	if len(t) == 0 {
		return "()"
	}
	if len(t) == 1 {
		return "(" + PrintValue(t[0]) + ",)"
	}
	
	elements := make([]string, len(t))
	for i, v := range t {
		elements[i] = PrintValue(v)
	}
	return "(" + strings.Join(elements, ", ") + ")"
}

// GetAttr implements Object interface using TypeDescriptor
func (t TupleValue) GetAttr(name string) (Value, bool) {
	desc := GetTypeDescriptor(TupleType)
	if desc != nil {
		val, err := desc.GetAttribute(t, name)
		if err == nil {
			return val, true
		}
	}
	return nil, false
}

// GetItem gets an item by index
func (t TupleValue) GetItem(index int) (Value, error) {
	if index < 0 {
		index = len(t) + index
	}
	if index < 0 || index >= len(t) {
		return nil, &IndexError{Index: index, Length: len(t)}
	}
	return t[index], nil
}

// Iterator implements Iterable
func (t TupleValue) Iterator() Iterator {
	return &tupleIterator{
		tuple: t,
		index: 0,
	}
}

type tupleIterator struct {
	tuple TupleValue
	index int
}

func (it *tupleIterator) Next() (Value, bool) {
	if it.index >= len(it.tuple) {
		return nil, false
	}
	val := it.tuple[it.index]
	it.index++
	return val, true
}

func (it *tupleIterator) Reset() {
	it.index = 0
}

// SetValue represents a set of unique values
type SetValue struct {
	BaseObject
	items map[string]Value // Key is the string representation
}

// NewSet creates a new set
func NewSet() *SetValue {
	return &SetValue{
		BaseObject: *NewBaseObject(SetType),
		items:      make(map[string]Value),
	}
}

// Type implements Value.Type
func (s *SetValue) Type() Type {
	return SetType
}

// String implements Value.String
func (s *SetValue) String() string {
	if len(s.items) == 0 {
		return "set()"
	}

	// Sort for consistent output
	keys := make([]string, 0, len(s.items))
	for k := range s.items {
		keys = append(keys, k)
	}
	sort.Strings(keys)

	elements := make([]string, 0, len(keys))
	for _, k := range keys {
		elements = append(elements, PrintValue(s.items[k]))
	}

	return "{" + strings.Join(elements, ", ") + "}"
}

// Add adds a value to the set
func (s *SetValue) Add(value Value) {
	key := PrintValue(value)
	s.items[key] = value
}

// Contains checks if a value is in the set
func (s *SetValue) Contains(value Value) bool {
	key := PrintValue(value)
	_, ok := s.items[key]
	return ok
}

// Remove removes a value from the set
func (s *SetValue) Remove(value Value) bool {
	key := PrintValue(value)
	if _, ok := s.items[key]; ok {
		delete(s.items, key)
		return true
	}
	return false
}

// Size returns the number of elements
func (s *SetValue) Size() int {
	return len(s.items)
}

// GetAttr implements Object interface using TypeDescriptor
func (s *SetValue) GetAttr(name string) (Value, bool) {
	desc := GetTypeDescriptor(SetType)
	if desc != nil {
		val, err := desc.GetAttribute(s, name)
		if err == nil {
			return val, true
		}
	}
	return s.BaseObject.GetAttr(name)
}

// Predefined empty collections
var (
	EmptyList  = ListValue{}
	EmptyTuple = TupleValue{}
)