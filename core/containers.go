package core

import (
	"fmt"
	"hash/fnv"
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
	keys    map[string]Value // Maps string representation to original key value
}

// NewDict creates a new dictionary
func NewDict() *DictValue {
	return &DictValue{
		BaseObject: *NewBaseObject(DictType),
		entries:    make(map[string]Value),
		keys:       make(map[string]Value),
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
		// Use original key if available, otherwise use string representation
		if origKey, hasOrig := d.keys[k]; hasOrig {
			parts = append(parts, fmt.Sprintf("%s: %s", PrintValue(origKey), PrintValue(v)))
		} else {
			parts = append(parts, fmt.Sprintf("%q: %s", k, PrintValue(v)))
		}
	}

	return "{" + strings.Join(parts, ", ") + "}"
}

// Get retrieves a value by internal key representation
// INTERNAL: Callers should use ValueToKey() to convert keys first
func (d *DictValue) Get(key string) (Value, bool) {
	val, ok := d.entries[key]
	return val, ok
}

// Set sets a value by internal key representation
// INTERNAL: Use SetWithKey for proper key tracking
// Deprecated: This method doesn't track original keys
func (d *DictValue) Set(key string, value Value) {
	d.entries[key] = value
}

// SetWithKey sets a value with both key representation and original key
// This is the preferred method for setting dict values
func (d *DictValue) SetWithKey(keyRepr string, origKey Value, value Value) {
	d.entries[keyRepr] = value
	d.keys[keyRepr] = origKey
}

// Delete removes a key by internal representation
// INTERNAL: Callers should use ValueToKey() to convert keys first
func (d *DictValue) Delete(key string) {
	delete(d.entries, key)
	delete(d.keys, key)
}

// Keys returns all internal key representations
// INTERNAL: Use dict.keys() method instead which returns original keys
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

// GetValue retrieves a value by M28 value key (handles conversion)
// This is the preferred public API for getting dict values
func (d *DictValue) GetValue(key Value) (Value, bool) {
	if !IsHashable(key) {
		return nil, false
	}
	keyStr := ValueToKey(key)
	return d.Get(keyStr)
}

// SetValue sets a value by M28 value key (handles conversion)
// This is the preferred public API for setting dict values
func (d *DictValue) SetValue(key Value, value Value) error {
	if !IsHashable(key) {
		return fmt.Errorf("unhashable type: '%s'", key.Type())
	}
	keyStr := ValueToKey(key)
	d.SetWithKey(keyStr, key, value)
	return nil
}

// DeleteValue removes a key by M28 value (handles conversion)
// This is the preferred public API for deleting dict values
func (d *DictValue) DeleteValue(key Value) bool {
	if !IsHashable(key) {
		return false
	}
	keyStr := ValueToKey(key)
	if _, exists := d.entries[keyStr]; exists {
		d.Delete(keyStr)
		return true
	}
	return false
}

// OriginalKeys returns all original key values (not internal representations)
// This is what dict.keys() method uses
func (d *DictValue) OriginalKeys() []Value {
	internalKeys := d.Keys()
	result := make([]Value, 0, len(internalKeys))
	for _, k := range internalKeys {
		if origKey, exists := d.keys[k]; exists {
			result = append(result, origKey)
		}
	}
	return result
}

// Iterator implements Iterable for dicts (iterates over keys)
func (d *DictValue) Iterator() Iterator {
	return &dictIterator{
		dict:  d,
		keys:  d.OriginalKeys(),
		index: 0,
	}
}

type dictIterator struct {
	dict  *DictValue
	keys  []Value
	index int
}

func (it *dictIterator) Next() (Value, bool) {
	if it.index >= len(it.keys) {
		return nil, false
	}
	val := it.keys[it.index]
	it.index++
	return val, true
}

func (it *dictIterator) Reset() {
	it.index = 0
}

// GetAttr implements Object interface using TypeDescriptor
func (d *DictValue) GetAttr(name string) (Value, bool) {
	// First try to find the key with string prefix
	// This handles dot notation access like dict.key
	stringKey := fmt.Sprintf("s:%s", name)
	if val, exists := d.entries[stringKey]; exists {
		return val, true
	}

	// Also check without prefix (for backwards compatibility)
	if val, exists := d.entries[name]; exists {
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
	// Set as dictionary entry with proper key conversion
	// Use string key since dot notation keys are always strings
	stringKey := ValueToKey(StringValue(name))
	d.SetWithKey(stringKey, StringValue(name), value)
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

// SliceValue represents a slice object with start, stop, and step
type SliceValue struct {
	Start Value
	Stop  Value
	Step  Value
}

// Type implements Value.Type
func (s *SliceValue) Type() Type {
	return SliceType
}

// String implements Value.String
func (s *SliceValue) String() string {
	startStr := "None"
	stopStr := "None"
	stepStr := "None"

	if s.Start != nil && s.Start != Nil {
		startStr = PrintValue(s.Start)
	}
	if s.Stop != nil && s.Stop != Nil {
		stopStr = PrintValue(s.Stop)
	}
	if s.Step != nil && s.Step != Nil {
		stepStr = PrintValue(s.Step)
	}

	return fmt.Sprintf("slice(%s, %s, %s)", startStr, stopStr, stepStr)
}

// GetAttr implements Object interface
func (s *SliceValue) GetAttr(name string) (Value, bool) {
	switch name {
	case "start":
		if s.Start == nil {
			return Nil, true
		}
		return s.Start, true
	case "stop":
		if s.Stop == nil {
			return Nil, true
		}
		return s.Stop, true
	case "step":
		if s.Step == nil {
			return Nil, true
		}
		return s.Step, true
	default:
		return nil, false
	}
}

// SetAttr implements Object.SetAttr (not supported for slices)
func (s *SliceValue) SetAttr(name string, value Value) error {
	return fmt.Errorf("cannot set attributes on slice objects")
}

// CallMethod implements Object.CallMethod
func (s *SliceValue) CallMethod(name string, args []Value, ctx *Context) (Value, error) {
	return nil, fmt.Errorf("slice has no method named %s", name)
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

// Iterator implements Iterable
func (s *SetValue) Iterator() Iterator {
	// Create a slice of values from the set
	values := make([]Value, 0, len(s.items))
	for _, v := range s.items {
		values = append(values, v)
	}

	return &setIterator{
		values: values,
		index:  0,
	}
}

type setIterator struct {
	values []Value
	index  int
}

func (it *setIterator) Next() (Value, bool) {
	if it.index >= len(it.values) {
		return nil, false
	}
	val := it.values[it.index]
	it.index++
	return val, true
}

func (it *setIterator) Reset() {
	it.index = 0
}

// FrozenSetValue represents an immutable set of unique values
type FrozenSetValue struct {
	BaseObject
	items map[string]Value // Key is the string representation
	hash  uint64           // Cached hash value
}

// NewFrozenSet creates a new frozenset
func NewFrozenSet() *FrozenSetValue {
	return &FrozenSetValue{
		BaseObject: *NewBaseObject(FrozenSetType),
		items:      make(map[string]Value),
		hash:       0, // Will be computed on first access
	}
}

// Type implements Value.Type
func (fs *FrozenSetValue) Type() Type {
	return FrozenSetType
}

// String implements Value.String
func (fs *FrozenSetValue) String() string {
	if len(fs.items) == 0 {
		return "frozenset()"
	}

	// Sort for consistent output
	keys := make([]string, 0, len(fs.items))
	for k := range fs.items {
		keys = append(keys, k)
	}
	sort.Strings(keys)

	elements := make([]string, 0, len(keys))
	for _, k := range keys {
		elements = append(elements, PrintValue(fs.items[k]))
	}

	return "frozenset({" + strings.Join(elements, ", ") + "})"
}

// Add adds a value to the frozenset (internal use only during construction)
func (fs *FrozenSetValue) Add(value Value) {
	key := PrintValue(value)
	fs.items[key] = value
	fs.hash = 0 // Invalidate cached hash
}

// Contains checks if a value is in the frozenset
func (fs *FrozenSetValue) Contains(value Value) bool {
	key := PrintValue(value)
	_, ok := fs.items[key]
	return ok
}

// Size returns the number of elements
func (fs *FrozenSetValue) Size() int {
	return len(fs.items)
}

// Hash computes and caches the hash value for the frozenset
func (fs *FrozenSetValue) Hash() uint64 {
	if fs.hash != 0 {
		return fs.hash
	}

	// Create a stable hash by sorting keys and hashing them
	keys := make([]string, 0, len(fs.items))
	for k := range fs.items {
		keys = append(keys, k)
	}
	sort.Strings(keys)

	h := fnv.New64a()
	for _, k := range keys {
		h.Write([]byte(k))
		h.Write([]byte{0}) // Separator
	}

	fs.hash = h.Sum64()
	if fs.hash == 0 {
		fs.hash = 1 // Avoid 0 as it's our "not computed" marker
	}

	return fs.hash
}

// GetAttr implements Object interface using TypeDescriptor
func (fs *FrozenSetValue) GetAttr(name string) (Value, bool) {
	desc := GetTypeDescriptor(FrozenSetType)
	if desc != nil {
		val, err := desc.GetAttribute(fs, name)
		if err == nil {
			return val, true
		}
	}
	return fs.BaseObject.GetAttr(name)
}

// Iterator implements Iterable
func (fs *FrozenSetValue) Iterator() Iterator {
	// Create a slice of values from the frozenset
	values := make([]Value, 0, len(fs.items))
	for _, v := range fs.items {
		values = append(values, v)
	}

	return &setIterator{
		values: values,
		index:  0,
	}
}

// Predefined empty collections
var (
	EmptyList  = ListValue{}
	EmptyTuple = TupleValue{}
)
