package core

import (
	"fmt"
	"hash/fnv"
	"sort"
	"strconv"
	"strings"
	"sync"
	"unsafe"
)

// Cycle detection for container string representation
var (
	visitedMu sync.Mutex
	visited   = make(map[uint64]map[uintptr]bool) // goroutine ID -> visited set
)

// withCycleDetection runs f with cycle detection for the given pointer
func withCycleDetection(ptr uintptr, f func() string) string {
	gid := getGoroutineID()

	visitedMu.Lock()
	if visited[gid] == nil {
		visited[gid] = make(map[uintptr]bool)
	}
	goroutineVisited := visited[gid]

	if goroutineVisited[ptr] {
		visitedMu.Unlock()
		return "{...}" // Circular reference
	}

	goroutineVisited[ptr] = true
	visitedMu.Unlock()

	defer func() {
		visitedMu.Lock()
		delete(goroutineVisited, ptr)
		if len(goroutineVisited) == 0 {
			delete(visited, gid)
		}
		visitedMu.Unlock()
	}()

	return f()
}

// ListValue represents a mutable list of values
type ListValue struct {
	BaseObject
	items []Value
}

// NewList creates a new mutable list from values
func NewList(values ...Value) *ListValue {
	// Create a copy of the values slice to prevent external mutation
	items := make([]Value, len(values))
	copy(items, values)
	return &ListValue{
		BaseObject: *NewBaseObject(ListType),
		items:      items,
	}
}

// EmptyList is a shared empty list value
var EmptyList = NewList()

// Len returns the length of the list
func (l *ListValue) Len() int {
	if l == nil {
		return 0
	}
	return len(l.items)
}

// Items returns a copy of the underlying slice (for safe iteration)
func (l *ListValue) Items() []Value {
	if l == nil {
		return nil
	}
	// Return a copy to prevent external mutation
	items := make([]Value, len(l.items))
	copy(items, l.items)
	return items
}

// Type implements Value.Type
func (l *ListValue) Type() Type {
	return ListType
}

// String implements Value.String
func (l *ListValue) String() string {
	if l == nil {
		return "[]"
	}

	// Use cycle detection to prevent infinite recursion
	return withCycleDetection(uintptr(unsafe.Pointer(l)), func() string {
		elements := make([]string, len(l.items))
		for i, v := range l.items {
			elements[i] = PrintValue(v)
		}
		return "[" + strings.Join(elements, ", ") + "]"
	})
}

// GetAttr implements Object interface using TypeDescriptor
func (l *ListValue) GetAttr(name string) (Value, bool) {
	// Special M28 type protocol attributes that auto-call or return properties
	// These are handled specially by getListAttr in eval/dot_notation.go
	switch name {
	case "length", "len", "pop", "reverse", "sort", "copy":
		// Return not found so these fall through to getListAttr
		// which handles them as auto-calling methods
		return nil, false
	}

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
func (l *ListValue) SetAttr(name string, value Value) error {
	return fmt.Errorf("cannot set attributes on lists")
}

// CallMethod implements Object.CallMethod
func (l *ListValue) CallMethod(name string, args []Value, ctx *Context) (Value, error) {
	desc := GetTypeDescriptor(ListType)
	if desc != nil {
		return desc.CallMethod(l, name, args, ctx)
	}
	return nil, fmt.Errorf("list has no method named %s", name)
}

// GetItem gets an item by index
func (l *ListValue) GetItem(index int) (Value, error) {
	if l == nil {
		return nil, &IndexError{Index: index, Length: 0}
	}
	if index < 0 {
		index = len(l.items) + index
	}
	if index < 0 || index >= len(l.items) {
		return nil, &IndexError{Index: index, Length: len(l.items)}
	}
	return l.items[index], nil
}

// SetItem sets an item by index (mutates the list)
func (l *ListValue) SetItem(index int, value Value) error {
	if l == nil {
		return fmt.Errorf("cannot set item on nil list")
	}
	if index < 0 {
		index = len(l.items) + index
	}
	if index < 0 || index >= len(l.items) {
		return &IndexError{Index: index, Length: len(l.items)}
	}
	l.items[index] = value
	return nil
}

// SetSlice sets a slice of items (mutates the list) - implements x[start:end] = values
func (l *ListValue) SetSlice(start, end *int, values *ListValue) error {
	if l == nil {
		return fmt.Errorf("cannot set slice on nil list")
	}

	length := len(l.items)

	// Handle nil (unbounded) slice indices
	startIdx := 0
	if start != nil {
		startIdx = *start
		if startIdx < 0 {
			startIdx = length + startIdx
		}
		if startIdx < 0 {
			startIdx = 0
		}
		if startIdx > length {
			startIdx = length
		}
	}

	endIdx := length
	if end != nil {
		endIdx = *end
		if endIdx < 0 {
			endIdx = length + endIdx
		}
		if endIdx < 0 {
			endIdx = 0
		}
		if endIdx > length {
			endIdx = length
		}
	}

	// Replace the slice
	newItems := make([]Value, 0, len(l.items)-endIdx+startIdx+len(values.items))
	newItems = append(newItems, l.items[:startIdx]...)
	newItems = append(newItems, values.items...)
	newItems = append(newItems, l.items[endIdx:]...)
	l.items = newItems

	return nil
}

// Append adds an item to the end (mutates the list)
func (l *ListValue) Append(value Value) {
	if l != nil {
		l.items = append(l.items, value)
	}
}

// Extend adds multiple items to the end (mutates the list)
func (l *ListValue) Extend(values []Value) {
	if l != nil {
		l.items = append(l.items, values...)
	}
}

// Iterator implements Iterable
func (l *ListValue) Iterator() Iterator {
	return &listIterator{
		list:  l,
		index: 0,
	}
}

type listIterator struct {
	list  *ListValue
	index int
}

func (it *listIterator) Next() (Value, bool) {
	if it.list == nil || it.index >= len(it.list.items) {
		return nil, false
	}
	val := it.list.items[it.index]
	it.index++
	return val, true
}

func (it *listIterator) Reset() {
	it.index = 0
}

// Smart Accessors - Auto-unwrap LocatedValue for cleaner evaluator code
// These methods eliminate the need for explicit unwrapLocated() calls

// GetItemUnwrapped gets an item by index and auto-unwraps LocatedValue
// Returns the unwrapped value or error if index out of bounds
func (l *ListValue) GetItemUnwrapped(index int) (Value, error) {
	val, err := l.GetItem(index)
	if err != nil {
		return nil, err
	}
	// Auto-unwrap LocatedValue
	if lv, ok := val.(LocatedValue); ok {
		return lv.Unwrap(), nil
	}
	return val, nil
}

// GetItemAsSymbol gets an item as SymbolValue (auto-unwraps)
// Returns (symbol, true) if successful, (empty, false) otherwise
func (l *ListValue) GetItemAsSymbol(index int) (SymbolValue, bool) {
	val, err := l.GetItemUnwrapped(index)
	if err != nil {
		return "", false
	}
	sym, ok := val.(SymbolValue)
	return sym, ok
}

// GetItemAsString gets an item as StringValue (auto-unwraps)
func (l *ListValue) GetItemAsString(index int) (StringValue, bool) {
	val, err := l.GetItemUnwrapped(index)
	if err != nil {
		return "", false
	}
	str, ok := val.(StringValue)
	return str, ok
}

// GetItemAsList gets an item as *ListValue (auto-unwraps)
func (l *ListValue) GetItemAsList(index int) (*ListValue, bool) {
	val, err := l.GetItemUnwrapped(index)
	if err != nil {
		return nil, false
	}
	list, ok := val.(*ListValue)
	return list, ok
}

// DictValue represents a dictionary mapping keys to values
type DictValue struct {
	BaseObject
	entries     map[string]Value
	keys        map[string]Value // Maps string representation to original key value
	orderedKeys []string         // Tracks insertion order of keys
}

// NewDict creates a new dictionary
func NewDict() *DictValue {
	return &DictValue{
		BaseObject:  *NewBaseObject(DictType),
		entries:     make(map[string]Value),
		keys:        make(map[string]Value),
		orderedKeys: make([]string, 0),
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

	// Use cycle detection to prevent infinite recursion
	return withCycleDetection(uintptr(unsafe.Pointer(d)), func() string {
		// Use insertion order for consistent output (Python 3.7+ behavior)
		parts := make([]string, 0, len(d.orderedKeys))
		for _, k := range d.orderedKeys {
			v := d.entries[k]
			// Use original key if available, otherwise use string representation
			if origKey, hasOrig := d.keys[k]; hasOrig {
				parts = append(parts, fmt.Sprintf("%s: %s", PrintValue(origKey), PrintValue(v)))
			} else {
				parts = append(parts, fmt.Sprintf("%q: %s", k, PrintValue(v)))
			}
		}

		return "{" + strings.Join(parts, ", ") + "}"
	})
}

// Get retrieves a value by internal key representation
// INTERNAL: Callers should use ValueToKey() to convert keys first
func (d *DictValue) Get(key string) (Value, bool) {
	val, ok := d.entries[key]
	return val, ok
}

// Set sets a value by internal key representation
// INTERNAL: Use SetWithKey for proper key tracking when you have the original key
// This method will reconstruct the original key from the internal representation
func (d *DictValue) Set(key string, value Value) {
	// Track insertion order for new keys
	if _, exists := d.entries[key]; !exists {
		d.orderedKeys = append(d.orderedKeys, key)
	}
	d.entries[key] = value
	// Reconstruct original key from internal representation if not already tracked
	if _, exists := d.keys[key]; !exists {
		// Extract original key by stripping type prefix
		var origKey Value
		if len(key) > 2 && key[1] == ':' {
			// Has a type prefix like "s:", "i:", "f:", etc.
			prefix := key[0:2]
			cleanKey := key[2:]
			switch prefix {
			case "s:":
				origKey = StringValue(cleanKey)
			case "i:":
				// Try to parse back to int
				if i, err := strconv.ParseInt(cleanKey, 10, 64); err == nil {
					origKey = NumberValue(i)
				} else {
					origKey = StringValue(cleanKey) // Fallback
				}
			case "f:":
				// Try to parse back to float
				if f, err := strconv.ParseFloat(cleanKey, 64); err == nil {
					origKey = NumberValue(f)
				} else {
					origKey = StringValue(cleanKey) // Fallback
				}
			case "b:":
				origKey = BoolValue(cleanKey == "true")
			case "n:":
				origKey = Nil
			default:
				// Unknown prefix - use string
				origKey = StringValue(key)
			}
		} else {
			// No prefix - use as string
			origKey = StringValue(key)
		}
		d.keys[key] = origKey
	}
}

// SetWithKey sets a value with both key representation and original key
// This is the preferred method for setting dict values
func (d *DictValue) SetWithKey(keyRepr string, origKey Value, value Value) {
	// Track insertion order for new keys
	if _, exists := d.entries[keyRepr]; !exists {
		d.orderedKeys = append(d.orderedKeys, keyRepr)
	}
	d.entries[keyRepr] = value
	d.keys[keyRepr] = origKey
}

// Delete removes a key by internal representation
// INTERNAL: Callers should use ValueToKey() to convert keys first
func (d *DictValue) Delete(key string) {
	delete(d.entries, key)
	delete(d.keys, key)
	// Remove from orderedKeys
	for i, k := range d.orderedKeys {
		if k == key {
			d.orderedKeys = append(d.orderedKeys[:i], d.orderedKeys[i+1:]...)
			break
		}
	}
}

// Keys returns all internal key representations in insertion order
// INTERNAL: Use dict.keys() method instead which returns original keys
func (d *DictValue) Keys() []string {
	// Return keys in insertion order (Python 3.7+ behavior)
	return d.orderedKeys
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
		return &TypeError{Message: fmt.Sprintf("unhashable type: '%s'", key.Type())}
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

// Type implements Value.Type for dict iterator
func (it *dictIterator) Type() Type {
	return "dict_keyiterator"
}

// String implements Value.String for dict iterator
func (it *dictIterator) String() string {
	return "<dict_keyiterator>"
}

// GetAttr implements Object interface for dict iterator protocol
func (it *dictIterator) GetAttr(name string) (Value, bool) {
	if name == "__iter__" {
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			return it, nil
		}), true
	}
	if name == "__next__" {
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			val, ok := it.Next()
			if !ok {
				return nil, &stopIterationError{}
			}
			return val, nil
		}), true
	}
	if name == "__length_hint__" {
		// PEP 424: Return estimated remaining length
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			remaining := len(it.keys) - it.index
			if remaining < 0 {
				remaining = 0
			}
			return NumberValue(remaining), nil
		}), true
	}
	return nil, false
}

// GetAttr implements Object interface using TypeDescriptor
func (d *DictValue) GetAttr(name string) (Value, bool) {
	// Special M28 type protocol attributes that auto-call or return properties
	// These are handled specially by getDictAttr in eval/dot_notation.go
	switch name {
	case "length", "len", "contains", "keys", "values", "items", "set", "delete", "clear", "update":
		// Return not found so these fall through to getDictAttr
		return nil, false
	}

	// IMPORTANT: Check methods FIRST, then keys
	// This matches Python behavior where d.get always returns the method,
	// even if the dict contains a key named "get".
	// Use d['get'] to access keys that shadow methods.

	// First check TypeDescriptor for methods
	desc := GetTypeDescriptor(DictType)
	if desc != nil {
		val, err := desc.GetAttribute(d, name)
		if err == nil {
			return val, true
		}
	}

	// Then try to find the key with string prefix
	// This handles dot notation access like dict.key for non-method names
	stringKey := fmt.Sprintf("s:%s", name)
	if val, exists := d.entries[stringKey]; exists {
		return val, true
	}

	// Also check without prefix (for backwards compatibility)
	if val, exists := d.entries[name]; exists {
		return val, true
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
	// Special M28 type protocol attributes that auto-call or return properties
	switch name {
	case "length", "len", "get", "contains", "tolist":
		return nil, false
	}

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

// Type implements Value.Type for tuple iterator
func (it *tupleIterator) Type() Type {
	return "tuple_iterator"
}

// String implements Value.String for tuple iterator
func (it *tupleIterator) String() string {
	return "<tuple_iterator>"
}

// GetAttr implements Object interface for tuple iterator protocol
func (it *tupleIterator) GetAttr(name string) (Value, bool) {
	if name == "__iter__" {
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			return it, nil
		}), true
	}
	if name == "__next__" {
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			val, ok := it.Next()
			if !ok {
				return nil, &stopIterationError{}
			}
			return val, nil
		}), true
	}
	if name == "__length_hint__" {
		// PEP 424: Return estimated remaining length
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			remaining := len(it.tuple) - it.index
			if remaining < 0 {
				remaining = 0
			}
			return NumberValue(remaining), nil
		}), true
	}
	return nil, false
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
	key := ValueToKey(value)
	s.items[key] = value
}

// Contains checks if a value is in the set
func (s *SetValue) Contains(value Value) bool {
	key := ValueToKey(value)
	_, ok := s.items[key]
	return ok
}

// Remove removes a value from the set
func (s *SetValue) Remove(value Value) bool {
	key := ValueToKey(value)
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

// Items returns all values in the set as a slice
func (s *SetValue) Items() []Value {
	values := make([]Value, 0, len(s.items))
	for _, v := range s.items {
		values = append(values, v)
	}
	return values
}

// GetAttr implements Object interface using TypeDescriptor
func (s *SetValue) GetAttr(name string) (Value, bool) {
	// Special M28 type protocol attributes that auto-call or return properties
	// These are handled specially in eval
	switch name {
	case "length", "len", "contains", "add", "remove", "union", "intersection", "difference":
		return nil, false
	}

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

// IteratorValue returns the iterator as a Value
func (s *SetValue) IteratorValue() Value {
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

// Type implements Value.Type
func (it *setIterator) Type() Type {
	return "set_iterator"
}

// String implements Value.String
func (it *setIterator) String() string {
	return "<set_iterator>"
}

// stopIterationError is a local type to signal iterator exhaustion
// This matches protocols.StopIteration but avoids circular imports
type stopIterationError struct{}

func (e *stopIterationError) Error() string {
	return "StopIteration"
}

// GetAttr implements Object interface for iterator protocol
func (it *setIterator) GetAttr(name string) (Value, bool) {
	if name == "__iter__" {
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			return it, nil
		}), true
	}
	if name == "__next__" {
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			val, ok := it.Next()
			if !ok {
				// Return StopIteration error
				return nil, &stopIterationError{}
			}
			return val, nil
		}), true
	}
	if name == "__length_hint__" {
		// PEP 424: Return estimated remaining length
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			remaining := len(it.values) - it.index
			if remaining < 0 {
				remaining = 0
			}
			return NumberValue(remaining), nil
		}), true
	}
	return nil, false
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
	EmptyTuple = TupleValue{}
)
