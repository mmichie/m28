package core

import (
	"fmt"

	"reflect"

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

// WithReprCycleDetection runs f with per-goroutine cycle detection keyed on the
// identity of obj (which must be a pointer), returning placeholder when obj is
// already being repr'd in the current goroutine. Container types implemented
// outside the core package (e.g. collections.defaultdict) use this to break
// repr recursion the same way dict/list do internally.
func WithReprCycleDetection(obj any, placeholder string, f func() string) string {
	rv := reflect.ValueOf(obj)
	if rv.Kind() != reflect.Ptr {
		return f()
	}
	return withCycleDetectionPlaceholder(rv.Pointer(), placeholder, f)
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

// ItemsRef returns the underlying items slice WITHOUT copying. The caller must
// treat it as read-only: do not mutate or retain it, as it aliases the list's
// storage. Use Items() whenever the result might be mutated or stored. This
// exists for hot read-only paths (e.g. call dispatch) where Items()'s defensive
// copy dominates allocation profiles.
func (l *ListValue) ItemsRef() []Value {
	if l == nil {
		return nil
	}
	return l.items
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
		orig:  l,
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
				return nil, &StopIteration{}
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
	case "__hash__":
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			return s.hashValue(ctx)
		}), true
	default:
		return nil, false
	}
}

// hashValue computes the hash of a slice from its (start, stop, step). Slices
// are hashable (CPython 3.12+); a bound that is itself unhashable makes the
// slice unhashable, raising TypeError like CPython.
func (s *SliceValue) hashValue(ctx *Context) (Value, error) {
	var h int64
	for _, part := range []Value{s.Start, s.Stop, s.Step} {
		p := part
		if p == nil {
			p = Nil
		}
		if !IsHashable(p) {
			return nil, &TypeError{Message: fmt.Sprintf("unhashable type: '%s'", p.Type())}
		}
		ph, err := ComputeHash(p, ctx)
		if err != nil {
			return nil, err
		}
		h = 31*h + ph
	}
	return NumberValue(h), nil
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
				return nil, &StopIteration{}
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
	out := make([]Value, 0, v.dict.Size())
	v.dict.ForEach(func(k, val Value) bool {
		switch v.kind {
		case DictKeysViewKind:
			out = append(out, k)
		case DictValuesViewKind:
			out = append(out, val)
		case DictItemsViewKind:
			out = append(out, TupleValue{k, val})
		}
		return true
	})
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
			result, err := dictViewSetCompareCtx(v, args[0], "==", ctx)
			if err != nil {
				return nil, err
			}
			return BoolValue(result), nil
		}), true
	case "__ne__":
		if v.kind == DictValuesViewKind {
			return nil, false
		}
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, &TypeError{Message: "__ne__ takes exactly one argument"}
			}
			result, err := dictViewSetCompareCtx(v, args[0], "==", ctx)
			if err != nil {
				return nil, err
			}
			return BoolValue(!result), nil
		}), true
	case "__lt__":
		if v.kind == DictValuesViewKind {
			return nil, false
		}
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, &TypeError{Message: "__lt__ takes exactly one argument"}
			}
			result, err := dictViewSetCompareCtx(v, args[0], "<", ctx)
			if err != nil {
				return nil, err
			}
			return BoolValue(result), nil
		}), true
	case "__le__":
		if v.kind == DictValuesViewKind {
			return nil, false
		}
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, &TypeError{Message: "__le__ takes exactly one argument"}
			}
			result, err := dictViewSetCompareCtx(v, args[0], "<=", ctx)
			if err != nil {
				return nil, err
			}
			return BoolValue(result), nil
		}), true
	case "__gt__":
		if v.kind == DictValuesViewKind {
			return nil, false
		}
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, &TypeError{Message: "__gt__ takes exactly one argument"}
			}
			result, err := dictViewSetCompareCtx(v, args[0], ">", ctx)
			if err != nil {
				return nil, err
			}
			return BoolValue(result), nil
		}), true
	case "__ge__":
		if v.kind == DictValuesViewKind {
			return nil, false
		}
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, &TypeError{Message: "__ge__ takes exactly one argument"}
			}
			result, err := dictViewSetCompareCtx(v, args[0], ">=", ctx)
			if err != nil {
				return nil, err
			}
			return BoolValue(result), nil
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
		return x.Items(), nil
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
		return x.Items(), nil
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
			if _, isStop := err.(*StopIteration); isStop {
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
			if _, isStop := err.(*StopIteration); isStop {
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
func PopulateDictFromArgs(d *DictValue, args []Value, kwargs *Kwargs) error {
	return populateDictFromArgs(d, args, kwargs)
}

// populateDictFromArgs fills d with entries from positional + keyword
// args following Python's dict() constructor rules:
//   - 0 args -> empty dict (caller already handled)
//   - 1 arg that's a mapping -> copy items
//   - 1 arg that's an iterable of pairs -> add each pair
//   - kwargs always overlay last
func populateDictFromArgs(d *DictValue, args []Value, kwargs *Kwargs) error {
	if len(args) > 1 {
		return &TypeError{Message: fmt.Sprintf("dict expected at most 1 argument, got %d", len(args))}
	}
	if len(args) == 1 {
		switch src := args[0].(type) {
		case *DictValue:
			d.Update(src)
		case *Instance:
			if src.BackingDict != nil {
				d.Update(src.BackingDict)
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
	for _, e := range kwargs.Entries() {
		d.SetStr(e.Name, e.Value)
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

// dictViewSetCompareCtx is the context-aware version that propagates __eq__ errors.
func dictViewSetCompareCtx(v *DictView, other Value, op string, ctx *Context) (bool, error) {
	otherItems, err := iterableValuesCtx(other, ctx)
	if err != nil {
		return false, err
	}
	myItems := v.snapshot()
	// Hash-based subset tests on the set engine: O(n+m) instead of the old
	// pairwise O(n*m) EqualValues sweeps. Unhashable elements (an items()
	// view can hold dict/list values) fall back to identity buckets inside
	// Add/Contains, and equality still decides matches within a bucket.
	toSet := func(items []Value) *SetValue {
		s := NewSet()
		for _, it := range items {
			s.Add(it)
		}
		return s
	}
	containsAll := func(superSet *SetValue, superItems []Value, sub []Value) (bool, error) {
		for _, x := range sub {
			in, err := superSet.ContainsWithError(x, ctx)
			if err == nil {
				if !in {
					return false, nil
				}
				continue
			}
			// Unhashable element (items() pairs carry arbitrary values;
			// CPython's view comparison never hashes them): linear equality
			// scan for this element only.
			found := false
			for _, y := range superItems {
				eq, eqErr := EqualValuesWithError(x, y, ctx)
				if eqErr != nil {
					return false, eqErr
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
	mySet, otherSet := toSet(myItems), toSet(otherItems)
	mySubsetOf, err := containsAll(otherSet, otherItems, myItems)
	if err != nil {
		return false, err
	}
	otherSubsetOfMe, err := containsAll(mySet, myItems, otherItems)
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
				return nil, &StopIteration{}
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
