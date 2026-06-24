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

// reprDepthMu protects the reprDepth map
var reprDepthMu sync.Mutex

// reprDepth tracks the current repr recursion depth per goroutine
var reprDepthMap = make(map[uint64]int)

const maxListReprDepth = 200

// getReprDepth returns the current repr depth for this goroutine
func getReprDepth() int {
	gid := getGoroutineID()
	reprDepthMu.Lock()
	d := reprDepthMap[gid]
	reprDepthMu.Unlock()
	return d
}

// pushReprDepth increments the repr depth and returns the new depth
func pushReprDepth() int {
	gid := getGoroutineID()
	reprDepthMu.Lock()
	reprDepthMap[gid]++
	d := reprDepthMap[gid]
	reprDepthMu.Unlock()
	return d
}

// popReprDepth decrements the repr depth
func popReprDepth() {
	gid := getGoroutineID()
	reprDepthMu.Lock()
	if reprDepthMap[gid] > 0 {
		reprDepthMap[gid]--
	}
	if reprDepthMap[gid] == 0 {
		delete(reprDepthMap, gid)
	}
	reprDepthMu.Unlock()
}

// withCycleDetectionErr runs f with cycle detection, returning (placeholder, nil) on cycles
// and propagating errors from f.
func withCycleDetectionErr(ptr uintptr, placeholder string, f func() (string, error)) (string, bool, error) {
	gid := getGoroutineID()

	visitedMu.Lock()
	if visited[gid] == nil {
		visited[gid] = make(map[uintptr]bool)
	}
	goroutineVisited := visited[gid]

	if goroutineVisited[ptr] {
		visitedMu.Unlock()
		return placeholder, true, nil
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

	s, err := f()
	return s, false, err
}

// withCycleDetection runs f with cycle detection for the given pointer
func withCycleDetection(ptr uintptr, f func() string) string {
	return withCycleDetectionPlaceholder(ptr, "{...}", f)
}

// withCycleDetectionPlaceholder runs f with cycle detection, returning placeholder on cycles
func withCycleDetectionPlaceholder(ptr uintptr, placeholder string, f func() string) string {
	gid := getGoroutineID()

	visitedMu.Lock()
	if visited[gid] == nil {
		visited[gid] = make(map[uintptr]bool)
	}
	goroutineVisited := visited[gid]

	if goroutineVisited[ptr] {
		visitedMu.Unlock()
		return placeholder // Circular reference
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

	// Python's str(list) == repr(list); both use repr() on each element so
	// strings inside print as 'foo' (single-quoted, Python default).
	return withCycleDetectionPlaceholder(uintptr(unsafe.Pointer(l)), "[...]", func() string {
		elements := make([]string, len(l.items))
		for i, v := range l.items {
			elements[i] = Repr(v)
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

	// CPython clamps stop to start when stop < start: slice is empty, insertion at start.
	if endIdx < startIdx {
		endIdx = startIdx
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
	// Use simpleListIterator which detaches from the list when exhausted,
	// matching CPython semantics (once exhausted, new elements aren't seen).
	return &simpleListIterator{
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
	modCount    uint64           // Structural-modification counter (insert/delete of keys)
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
		d.modCount++
	}
	d.entries[key] = value
	// Reconstruct original key from internal representation if not already tracked.
	// NOTE: ValueToKey uses "n:" for numbers and "nil" (no prefix) for None.
	if _, exists := d.keys[key]; !exists {
		var origKey Value
		if key == "nil" {
			origKey = Nil
		} else if len(key) > 2 && key[1] == ':' {
			prefix := key[0:2]
			cleanKey := key[2:]
			switch prefix {
			case "s:":
				origKey = StringValue(cleanKey)
			case "n:":
				// Numbers — parsed as float, but use the simplest int form when exact.
				if f, err := strconv.ParseFloat(cleanKey, 64); err == nil {
					if f == float64(int64(f)) {
						origKey = NumberValue(int64(f))
					} else {
						origKey = NumberValue(f)
					}
				} else {
					origKey = StringValue(cleanKey)
				}
			case "i:":
				if i, err := strconv.ParseInt(cleanKey, 10, 64); err == nil {
					origKey = NumberValue(i)
				} else {
					origKey = StringValue(cleanKey)
				}
			case "f:":
				if f, err := strconv.ParseFloat(cleanKey, 64); err == nil {
					origKey = NumberValue(f)
				} else {
					origKey = StringValue(cleanKey)
				}
			case "b:":
				origKey = BoolValue(cleanKey == "true")
			default:
				origKey = StringValue(key)
			}
		} else {
			origKey = StringValue(key)
		}
		d.keys[key] = origKey
	}
}

// SetWithKey sets a value with both key representation and original key
// This is the preferred method for setting dict values
func (d *DictValue) SetWithKey(keyRepr string, origKey Value, value Value) {
	// Track insertion order and structural changes for new keys
	if _, exists := d.entries[keyRepr]; !exists {
		d.orderedKeys = append(d.orderedKeys, keyRepr)
		d.modCount++
	}
	d.entries[keyRepr] = value
	d.keys[keyRepr] = origKey
}

// Update copies every entry from other into d (like dict.update), preserving
// each entry's original key object and insertion order.
func (d *DictValue) Update(other *DictValue) {
	for _, keyRepr := range other.orderedKeys {
		d.SetWithKey(keyRepr, other.keys[keyRepr], other.entries[keyRepr])
	}
}

// Delete removes a key by internal representation
// INTERNAL: Callers should use ValueToKey() to convert keys first
func (d *DictValue) Delete(key string) {
	if _, existed := d.entries[key]; existed {
		d.modCount++
	}
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

// OriginalKeyValue returns the original (pre-conversion) key value for an
// internal key representation, if known. Returns (nil, false) otherwise.
func (d *DictValue) OriginalKeyValue(internalKey string) (Value, bool) {
	v, ok := d.keys[internalKey]
	return v, ok
}

// Clear removes all entries from the dict in place.
func (d *DictValue) Clear() {
	for _, k := range d.Keys() {
		d.Delete(k)
	}
}

// Iterator implements Iterable for dicts (iterates over keys)
func (d *DictValue) Iterator() Iterator {
	return &dictIterator{
		dict:     d,
		keys:     d.OriginalKeys(),
		index:    0,
		startMod: d.modCount,
	}
}

type dictIterator struct {
	dict     *DictValue
	keys     []Value
	index    int
	startMod uint64
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
			if it.dict != nil && it.dict.modCount != it.startMod {
				return nil, &RuntimeError{Message: "dictionary changed size during iteration"}
			}
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
	// In Python, str(tuple) == repr(tuple) - both use repr() on each element.
	// This produces single-quoted strings (Python default), matching CPython.
	if len(t) == 0 {
		return "()"
	}
	if len(t) == 1 {
		return "(" + Repr(t[0]) + ",)"
	}

	elements := make([]string, len(t))
	for i, v := range t {
		elements[i] = Repr(v)
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
	case "indices":
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			return s.indices(args, ctx)
		}), true
	default:
		return nil, false
	}
}

// indices implements slice.indices(length): returns the (start, stop, step)
// tuple to use when slicing a sequence of the given length, applying __index__,
// negative-index wrapping, and clamping exactly as CPython does.
func (s *SliceValue) indices(args []Value, ctx *Context) (Value, error) {
	if len(args) != 1 {
		return nil, &TypeError{Message: fmt.Sprintf("indices() takes exactly one argument (%d given)", len(args))}
	}
	length, err := toIndex(args[0], ctx)
	if err != nil {
		return nil, err
	}
	if length < 0 {
		return nil, &ValueError{Message: "length should not be negative"}
	}

	step := 1
	if s.Step != nil && s.Step != Nil {
		if step, err = toIndex(s.Step, ctx); err != nil {
			return nil, err
		}
		if step == 0 {
			return nil, &ValueError{Message: "slice step cannot be zero"}
		}
	}

	lower, upper := 0, length
	if step < 0 {
		lower, upper = -1, length-1
	}

	clamp := func(v Value, noneDefault int) (int, error) {
		if v == nil || v == Nil {
			return noneDefault, nil
		}
		i, err := toIndex(v, ctx)
		if err != nil {
			return 0, err
		}
		if i < 0 {
			i += length
			if i < lower {
				i = lower
			}
		} else if i > upper {
			i = upper
		}
		return i, nil
	}

	startDefault, stopDefault := lower, upper
	if step < 0 {
		startDefault, stopDefault = upper, lower
	}
	start, err := clamp(s.Start, startDefault)
	if err != nil {
		return nil, err
	}
	stop, err := clamp(s.Stop, stopDefault)
	if err != nil {
		return nil, err
	}
	return TupleValue{NumberValue(start), NumberValue(stop), NumberValue(step)}, nil
}

// SetAttr implements Object.SetAttr (not supported for slices)
func (s *SliceValue) SetAttr(name string, value Value) error {
	return fmt.Errorf("cannot set attributes on slice objects")
}

// CallMethod implements Object.CallMethod
func (s *SliceValue) CallMethod(name string, args []Value, ctx *Context) (Value, error) {
	if name == "indices" {
		return s.indices(args, ctx)
	}
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

// DictViewKind identifies the kind of dict view (keys, values, or items).
type DictViewKind int

const (
	DictKeysViewKind DictViewKind = iota
	DictValuesViewKind
	DictItemsViewKind
)

// Dict view type constants (used as Python type names).
const (
	DictKeysType   Type = "dict_keys"
	DictValuesType Type = "dict_values"
	DictItemsType  Type = "dict_items"
)

// DictView is a live view into a dict's keys, values, or items.
// Like CPython's dict_keys/dict_values/dict_items, mutations of the
// underlying dict are visible through the view.
type DictView struct {
	BaseObject
	dict *DictValue
	kind DictViewKind
}

// NewDictView constructs a view over d of the given kind.
func NewDictView(d *DictValue, kind DictViewKind) *DictView {
	var typ Type
	switch kind {
	case DictKeysViewKind:
		typ = DictKeysType
	case DictValuesViewKind:
		typ = DictValuesType
	case DictItemsViewKind:
		typ = DictItemsType
	}
	return &DictView{
		BaseObject: *NewBaseObject(typ),
		dict:       d,
		kind:       kind,
	}
}

// Type returns the view's Python type name.
func (v *DictView) Type() Type {
	switch v.kind {
	case DictKeysViewKind:
		return DictKeysType
	case DictValuesViewKind:
		return DictValuesType
	case DictItemsViewKind:
		return DictItemsType
	}
	return "dict_view"
}

// Kind returns which kind of view this is.
func (v *DictView) Kind() DictViewKind {
	return v.kind
}

// Dict returns the backing dict.
func (v *DictView) Dict() *DictValue {
	return v.dict
}

// snapshot returns the current items the view exposes.
func (v *DictView) snapshot() []Value {
	keys := v.dict.Keys()
	out := make([]Value, 0, len(keys))
	for _, k := range keys {
		switch v.kind {
		case DictKeysViewKind:
			if orig, ok := v.dict.keys[k]; ok {
				out = append(out, orig)
			} else {
				out = append(out, StringValue(k))
			}
		case DictValuesViewKind:
			if val, ok := v.dict.Get(k); ok {
				out = append(out, val)
			}
		case DictItemsViewKind:
			var keyVal Value
			if orig, ok := v.dict.keys[k]; ok {
				keyVal = orig
			} else {
				keyVal = StringValue(k)
			}
			val, _ := v.dict.Get(k)
			out = append(out, TupleValue{keyVal, val})
		}
	}
	return out
}

// Size returns the number of items in the view.
func (v *DictView) Size() int {
	return v.dict.Size()
}

// Contains reports whether the view contains x.
// For keys: x must equal a key. For values: x must equal a value.
// For items: x must be a 2-tuple matching some (key, value).
func (v *DictView) Contains(x Value) bool {
	switch v.kind {
	case DictKeysViewKind:
		_, ok := v.dict.GetValue(x)
		return ok
	case DictValuesViewKind:
		for _, k := range v.dict.Keys() {
			val, _ := v.dict.Get(k)
			if EqualValues(val, x) {
				return true
			}
		}
		return false
	case DictItemsViewKind:
		pair, ok := x.(TupleValue)
		if !ok || len(pair) != 2 {
			return false
		}
		val, ok := v.dict.GetValue(pair[0])
		if !ok {
			return false
		}
		return EqualValues(val, pair[1])
	}
	return false
}

// String renders the view as Python does: "dict_keys([...])" etc.
func (v *DictView) String() string {
	items := v.snapshot()
	parts := make([]string, len(items))
	for i, it := range items {
		parts[i] = Repr(it)
	}
	var name string
	switch v.kind {
	case DictKeysViewKind:
		name = "dict_keys"
	case DictValuesViewKind:
		name = "dict_values"
	case DictItemsViewKind:
		name = "dict_items"
	}
	return name + "([" + strings.Join(parts, ", ") + "])"
}

// GetAttr exposes dunder methods (__len__, __iter__, __contains__) and
// .mapping per Python's view protocol.
func (v *DictView) GetAttr(name string) (Value, bool) {
	switch name {
	case "mapping":
		return v.dict, true
	case "__len__":
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			return NumberValue(v.Size()), nil
		}), true
	case "__iter__":
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			return v.Iterator().(Value), nil
		}), true
	case "__contains__":
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, &TypeError{Message: "__contains__ takes exactly one argument"}
			}
			return BoolValue(v.Contains(args[0])), nil
		}), true
	case "__reversed__":
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			items := v.snapshot()
			rev := make([]Value, len(items))
			for i, it := range items {
				rev[len(items)-1-i] = it
			}
			return &dictViewIterator{items: rev, index: 0}, nil
		}), true
	case "__eq__":
		if v.kind == DictValuesViewKind {
			return nil, false
		}
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, &TypeError{Message: "__eq__ takes exactly one argument"}
			}
			result, err := dictViewSetCompareCtx(v, args[0], "==", ctx); if err != nil { return nil, err }; return BoolValue(result), nil
		}), true
	case "__ne__":
		if v.kind == DictValuesViewKind {
			return nil, false
		}
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, &TypeError{Message: "__ne__ takes exactly one argument"}
			}
			result, err := dictViewSetCompareCtx(v, args[0], "==", ctx); if err != nil { return nil, err }; return BoolValue(!result), nil
		}), true
	case "__lt__":
		if v.kind == DictValuesViewKind {
			return nil, false
		}
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, &TypeError{Message: "__lt__ takes exactly one argument"}
			}
			result, err := dictViewSetCompareCtx(v, args[0], "<", ctx); if err != nil { return nil, err }; return BoolValue(result), nil
		}), true
	case "__le__":
		if v.kind == DictValuesViewKind {
			return nil, false
		}
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, &TypeError{Message: "__le__ takes exactly one argument"}
			}
			result, err := dictViewSetCompareCtx(v, args[0], "<=", ctx); if err != nil { return nil, err }; return BoolValue(result), nil
		}), true
	case "__gt__":
		if v.kind == DictValuesViewKind {
			return nil, false
		}
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, &TypeError{Message: "__gt__ takes exactly one argument"}
			}
			result, err := dictViewSetCompareCtx(v, args[0], ">", ctx); if err != nil { return nil, err }; return BoolValue(result), nil
		}), true
	case "__ge__":
		if v.kind == DictValuesViewKind {
			return nil, false
		}
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, &TypeError{Message: "__ge__ takes exactly one argument"}
			}
			result, err := dictViewSetCompareCtx(v, args[0], ">=", ctx); if err != nil { return nil, err }; return BoolValue(result), nil
		}), true
	case "__sub__":
		if v.kind == DictValuesViewKind {
			return nil, false
		}
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, &TypeError{Message: "__sub__ takes exactly one argument"}
			}
			return dictViewSetOp(v, args[0], "-")
		}), true
	case "__or__":
		if v.kind == DictValuesViewKind {
			return nil, false
		}
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, &TypeError{Message: "__or__ takes exactly one argument"}
			}
			return dictViewSetOp(v, args[0], "|")
		}), true
	case "__and__":
		if v.kind == DictValuesViewKind {
			return nil, false
		}
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, &TypeError{Message: "__and__ takes exactly one argument"}
			}
			return dictViewSetOp(v, args[0], "&")
		}), true
	case "__xor__":
		if v.kind == DictValuesViewKind {
			return nil, false
		}
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, &TypeError{Message: "__xor__ takes exactly one argument"}
			}
			return dictViewSetOp(v, args[0], "^")
		}), true
	case "isdisjoint":
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, &TypeError{Message: "isdisjoint() takes exactly one argument"}
			}
			otherItems, err := iterableValues(args[0])
			if err != nil {
				return nil, err
			}
			myItems := v.snapshot()
			for _, x := range otherItems {
				for _, y := range myItems {
					if EqualValues(x, y) {
						return BoolValue(false), nil
					}
				}
			}
			return BoolValue(true), nil
		}), true
	}
	return v.BaseObject.GetAttr(name)
}

// iterableValues converts any iterable Value to a []Value.
func iterableValues(v Value) ([]Value, error) {
	switch x := v.(type) {
	case *ListValue:
		return x.Items(), nil
	case TupleValue:
		out := make([]Value, len(x))
		copy(out, x)
		return out, nil
	case *SetValue:
		return x.Items(), nil
	case *FrozenSetValue:
		out := make([]Value, 0, len(x.items))
		for _, item := range x.items {
			out = append(out, item)
		}
		return out, nil
	case *DictView:
		return x.snapshot(), nil
	case *DictValue:
		view := NewDictView(x, DictKeysViewKind)
		return view.snapshot(), nil
	}
	if obj, ok := v.(interface{ Iterator() Iterator }); ok {
		iter := obj.Iterator()
		var out []Value
		for {
			val, more := iter.Next()
			if !more {
				break
			}
			out = append(out, val)
		}
		return out, nil
	}
	// Python iterator protocol: call __iter__ to get an iterator, then
	// drive it with __next__ until StopIteration. Lets user-defined
	// classes (Instances) work as iterables without implementing the Go
	// Iterator interface.
	if obj, ok := v.(interface {
		GetAttr(string) (Value, bool)
	}); ok {
		iterAttr, found := obj.GetAttr("__iter__")
		if found {
			if callable, ok := iterAttr.(interface {
				Call([]Value, *Context) (Value, error)
			}); ok {
				iter, err := callable.Call(nil, nil)
				if err != nil {
					return nil, err
				}
				return drainPythonIterator(iter)
			}
		}
	}
	return nil, &TypeError{Message: fmt.Sprintf("'%s' object is not iterable", v.Type())}
}

// IterableValuesCtx is the exported version of iterableValuesCtx.
func IterableValuesCtx(v Value, ctx *Context) ([]Value, error) {
	return iterableValuesCtx(v, ctx)
}

// iterableValuesCtx is like iterableValues but passes a context to Python method calls.
func iterableValuesCtx(v Value, ctx *Context) ([]Value, error) {
	switch x := v.(type) {
	case *ListValue:
		return x.Items(), nil
	case TupleValue:
		out := make([]Value, len(x))
		copy(out, x)
		return out, nil
	case *SetValue:
		return x.Items(), nil
	case *FrozenSetValue:
		out := make([]Value, 0, len(x.items))
		for _, item := range x.items {
			out = append(out, item)
		}
		return out, nil
	case *DictView:
		return x.snapshot(), nil
	case *DictValue:
		view := NewDictView(x, DictKeysViewKind)
		return view.snapshot(), nil
	}
	if obj, ok := v.(interface{ Iterator() Iterator }); ok {
		iter := obj.Iterator()
		var out []Value
		for {
			val, more := iter.Next()
			if !more {
				break
			}
			out = append(out, val)
		}
		return out, nil
	}
	if obj, ok := v.(interface {
		GetAttr(string) (Value, bool)
	}); ok {
		iterAttr, found := obj.GetAttr("__iter__")
		if found {
			if callable, ok := iterAttr.(interface {
				Call([]Value, *Context) (Value, error)
			}); ok {
				iter, err := callable.Call(nil, ctx)
				if err != nil {
					return nil, err
				}
				return drainPythonIteratorCtx(iter, ctx)
			}
		}
	}
	return nil, &TypeError{Message: fmt.Sprintf("'%s' object is not iterable", v.Type())}
}

// drainPythonIteratorCtx drives a Python iterator with a context.
func drainPythonIteratorCtx(iter Value, ctx *Context) ([]Value, error) {
	if iter == nil {
		return nil, &TypeError{Message: "iter returned nil"}
	}
	if it, ok := iter.(Iterator); ok {
		var out []Value
		for {
			val, more := it.Next()
			if !more {
				break
			}
			out = append(out, val)
		}
		return out, nil
	}
	obj, ok := iter.(interface {
		GetAttr(string) (Value, bool)
	})
	if !ok {
		return nil, &TypeError{Message: fmt.Sprintf("'%s' object is not an iterator", iter.Type())}
	}
	nextAttr, found := obj.GetAttr("__next__")
	if !found {
		return nil, &TypeError{Message: fmt.Sprintf("'%s' object is not an iterator", iter.Type())}
	}
	callable, ok := nextAttr.(interface {
		Call([]Value, *Context) (Value, error)
	})
	if !ok {
		return nil, &TypeError{Message: "__next__ is not callable"}
	}
	var out []Value
	for {
		val, err := callable.Call(nil, ctx)
		if err != nil {
			if _, isStop := err.(*StopIteration); isStop {
				break
			}
			if _, isStop := err.(*stopIterationError); isStop {
				break
			}
			if err.Error() == "StopIteration" {
				break
			}
			return nil, err
		}
		out = append(out, val)
	}
	return out, nil
}

// drainPythonIterator drives a Python iterator (an object with __next__)
// until StopIteration. Used to drain user-defined iterator instances.
func drainPythonIterator(iter Value) ([]Value, error) {
	if iter == nil {
		return nil, &TypeError{Message: "iter returned nil"}
	}
	// Native Iterator? Just walk it.
	if it, ok := iter.(Iterator); ok {
		var out []Value
		for {
			val, more := it.Next()
			if !more {
				break
			}
			out = append(out, val)
		}
		return out, nil
	}
	obj, ok := iter.(interface {
		GetAttr(string) (Value, bool)
	})
	if !ok {
		return nil, &TypeError{Message: fmt.Sprintf("'%s' object is not an iterator", iter.Type())}
	}
	nextAttr, found := obj.GetAttr("__next__")
	if !found {
		return nil, &TypeError{Message: fmt.Sprintf("'%s' object is not an iterator", iter.Type())}
	}
	callable, ok := nextAttr.(interface {
		Call([]Value, *Context) (Value, error)
	})
	if !ok {
		return nil, &TypeError{Message: "__next__ is not callable"}
	}
	var out []Value
	for {
		val, err := callable.Call(nil, nil)
		if err != nil {
			// StopIteration ends iteration; any other error propagates.
			if _, isStop := err.(*StopIteration); isStop {
				break
			}
			if _, isStop := err.(*stopIterationError); isStop {
				break
			}
			// Match by error message as a fallback for wrapped exceptions.
			if err.Error() == "StopIteration" {
				break
			}
			return nil, err
		}
		out = append(out, val)
	}
	return out, nil
}

// dictViewSetOp implements set operations (&, |, -, ^) on dict views,
// returning a new set.
func dictViewSetOp(v *DictView, other Value, op string) (Value, error) {
	otherItems, err := iterableValues(other)
	if err != nil {
		return nil, &TypeError{Message: fmt.Sprintf("unsupported operand type(s) for %s: '%s' and '%s'", op, v.Type(), other.Type())}
	}
	myItems := v.snapshot()
	result := NewSet()
	switch op {
	case "-":
		for _, x := range myItems {
			found := false
			for _, y := range otherItems {
				if EqualValues(x, y) {
					found = true
					break
				}
			}
			if !found {
				result.Add(x)
			}
		}
	case "|":
		for _, x := range myItems {
			result.Add(x)
		}
		for _, y := range otherItems {
			result.Add(y)
		}
	case "&":
		for _, x := range myItems {
			for _, y := range otherItems {
				if EqualValues(x, y) {
					result.Add(x)
					break
				}
			}
		}
	case "^":
		for _, x := range myItems {
			found := false
			for _, y := range otherItems {
				if EqualValues(x, y) {
					found = true
					break
				}
			}
			if !found {
				result.Add(x)
			}
		}
		for _, y := range otherItems {
			found := false
			for _, x := range myItems {
				if EqualValues(x, y) {
					found = true
					break
				}
			}
			if !found {
				result.Add(y)
			}
		}
	}
	return result, nil
}

// PopulateDictFromArgs is the exported form of populateDictFromArgs, used by the
// dict class's __init__ (in the builtin package) so super().__init__(...) in a
// dict subclass populates the backing dict.
func PopulateDictFromArgs(d *DictValue, args []Value, kwargs map[string]Value) error {
	return populateDictFromArgs(d, args, kwargs)
}

// populateDictFromArgs fills d with entries from positional + keyword
// args following Python's dict() constructor rules:
//   - 0 args -> empty dict (caller already handled)
//   - 1 arg that's a mapping -> copy items
//   - 1 arg that's an iterable of pairs -> add each pair
//   - kwargs always overlay last
func populateDictFromArgs(d *DictValue, args []Value, kwargs map[string]Value) error {
	if len(args) > 1 {
		return &TypeError{Message: fmt.Sprintf("dict expected at most 1 argument, got %d", len(args))}
	}
	if len(args) == 1 {
		switch src := args[0].(type) {
		case *DictValue:
			for _, k := range src.Keys() {
				v, _ := src.Get(k)
				if orig, ok := src.keys[k]; ok {
					d.SetWithKey(k, orig, v)
				} else {
					d.Set(k, v)
				}
			}
		case *Instance:
			if src.BackingDict != nil {
				for _, k := range src.BackingDict.Keys() {
					v, _ := src.BackingDict.Get(k)
					if orig, ok := src.BackingDict.keys[k]; ok {
						d.SetWithKey(k, orig, v)
					} else {
						d.Set(k, v)
					}
				}
			} else {
				return &TypeError{Message: "dict() argument must be a dict or iterable of pairs"}
			}
		default:
			items, err := iterableValues(args[0])
			if err != nil {
				return &TypeError{Message: "dict() argument must be a dict or iterable of pairs"}
			}
			for i, item := range items {
				if pair, ok := item.(TupleValue); ok && len(pair) == 2 {
					d.SetValue(pair[0], pair[1])
					continue
				}
				if lst, ok := item.(*ListValue); ok && lst.Len() == 2 {
					d.SetValue(lst.Items()[0], lst.Items()[1])
					continue
				}
				return &ValueError{Message: fmt.Sprintf("dictionary update sequence element #%d has length %d; 2 is required", i, sequenceLength(item))}
			}
		}
	}
	for k, v := range kwargs {
		d.SetValue(StringValue(k), v)
	}
	return nil
}

func sequenceLength(v Value) int {
	switch x := v.(type) {
	case *ListValue:
		return x.Len()
	case TupleValue:
		return len(x)
	case StringValue:
		return len(string(x))
	}
	return -1
}

// dictViewSetCompare implements comparison ops (==, <, <=, >, >=) on
// dict views, treating them as sets of their elements.
func dictViewSetCompare(v *DictView, other Value, op string) bool {
	result, _ := dictViewSetCompareCtx(v, other, op, nil)
	return result
}

// dictViewSetCompareCtx is the context-aware version that propagates __eq__ errors.
func dictViewSetCompareCtx(v *DictView, other Value, op string, ctx *Context) (bool, error) {
	otherItems, err := iterableValuesCtx(other, ctx)
	if err != nil {
		return false, err
	}
	myItems := v.snapshot()
	containsAll := func(super, sub []Value) (bool, error) {
		for _, x := range sub {
			found := false
			for _, y := range super {
				eq, err := EqualValuesWithError(x, y, ctx)
				if err != nil {
					return false, err
				}
				if eq {
					found = true
					break
				}
			}
			if !found {
				return false, nil
			}
		}
		return true, nil
	}
	mySubsetOf, err := containsAll(otherItems, myItems)
	if err != nil {
		return false, err
	}
	otherSubsetOfMe, err := containsAll(myItems, otherItems)
	if err != nil {
		return false, err
	}
	equal := mySubsetOf && otherSubsetOfMe
	switch op {
	case "==":
		return equal, nil
	case "<":
		return mySubsetOf && !equal, nil
	case "<=":
		return mySubsetOf, nil
	case ">":
		return otherSubsetOfMe && !equal, nil
	case ">=":
		return otherSubsetOfMe, nil
	}
	return false, nil
}

// Iterator implements Iterable.
func (v *DictView) Iterator() Iterator {
	return &dictViewIterator{items: v.snapshot(), index: 0, dict: v.dict, startMod: v.dict.modCount}
}

type dictViewIterator struct {
	items    []Value
	index    int
	dict     *DictValue // backing dict for mutation detection (may be nil)
	startMod uint64
}

func (it *dictViewIterator) Next() (Value, bool) {
	if it.index >= len(it.items) {
		return nil, false
	}
	val := it.items[it.index]
	it.index++
	return val, true
}

func (it *dictViewIterator) Reset() {
	it.index = 0
}

func (it *dictViewIterator) Type() Type {
	return "dict_view_iterator"
}

func (it *dictViewIterator) String() string {
	return "<dict_view_iterator>"
}

func (it *dictViewIterator) GetAttr(name string) (Value, bool) {
	switch name {
	case "__iter__":
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			return it, nil
		}), true
	case "__next__":
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if it.dict != nil && it.dict.modCount != it.startMod {
				return nil, &RuntimeError{Message: "dictionary changed size during iteration"}
			}
			val, ok := it.Next()
			if !ok {
				return nil, &stopIterationError{}
			}
			return val, nil
		}), true
	case "__length_hint__":
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			remaining := len(it.items) - it.index
			if remaining < 0 {
				remaining = 0
			}
			return NumberValue(remaining), nil
		}), true
	}
	return nil, false
}
