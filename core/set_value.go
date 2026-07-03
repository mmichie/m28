package core

// SetValue and FrozenSetValue on the dict hash-table engine (phase 2 of
// docs/dict-engine.md). Element identity is hash + equality — the same
// invariants as dict keys — replacing two different string encodings
// (ValueToKey for sets, PrintValue/repr for frozensets) that collided on
// tuple smuggling, deep nesting, and instance elements.
//
// Entries store the element as both key and value; iteration yields
// insertion order (Python sets are unordered, so any stable order is fine).
// The legacy no-context API (Add/Contains/Remove) hashes instances with user
// __hash__ by identity; ctx-aware paths (set methods, literals) use the
// *WithError variants for full semantics including error propagation.

// SetValue represents a mutable set of unique values
type SetValue struct {
	BaseObject
	table hashTable
}

// NewSet creates a new set. Storage is allocated lazily on first insert.
func NewSet() *SetValue {
	return &SetValue{BaseObject: *NewBaseObject(SetType)}
}

// Type implements Value.Type
func (s *SetValue) Type() Type {
	return SetType
}

// String implements Value.String
func (s *SetValue) String() string {
	// In Python str(set) == repr(set): elements use repr (single-quoted strings).
	// Delegate to formatSetRepr so str()/print() agree with repr().
	return formatSetRepr(s)
}

// Add adds a value to the set. Unhashable values fall back to identity
// (matching the legacy encoding's pointer keys); error-aware callers use
// AddWithError.
func (s *SetValue) Add(value Value) {
	hash, err := HashValue(value, nil)
	if err != nil {
		hash = identityHash(value)
	}
	s.table.insert(value, hash, value, nil)
}

// AddWithError adds a value with full Python semantics: unhashable values
// return TypeError, user __hash__/__eq__ run with ctx and may raise.
func (s *SetValue) AddWithError(value Value, ctx *Context) error {
	hash, err := HashValue(value, ctx)
	if err != nil {
		return err
	}
	_, err = s.table.insert(value, hash, value, ctx)
	return err
}

// Contains checks if a value is in the set.
func (s *SetValue) Contains(value Value) bool {
	hash, err := HashValue(value, nil)
	if err != nil {
		hash = identityHash(value)
	}
	pos, _ := s.table.lookup(value, hash, nil)
	return pos >= 0
}

// ContainsWithError is Contains with full Python semantics (unhashable
// probes raise TypeError; user __eq__ errors propagate).
func (s *SetValue) ContainsWithError(value Value, ctx *Context) (bool, error) {
	hash, err := HashValue(value, ctx)
	if err != nil {
		return false, err
	}
	pos, err := s.table.lookup(value, hash, ctx)
	if err != nil {
		return false, err
	}
	return pos >= 0, nil
}

// Remove removes a value from the set, reporting whether it was present.
func (s *SetValue) Remove(value Value) bool {
	hash, err := HashValue(value, nil)
	if err != nil {
		hash = identityHash(value)
	}
	_, removed, _ := s.table.remove(value, hash, nil)
	return removed
}

// RemoveWithError is Remove with full Python semantics.
func (s *SetValue) RemoveWithError(value Value, ctx *Context) (bool, error) {
	hash, err := HashValue(value, ctx)
	if err != nil {
		return false, err
	}
	_, removed, err := s.table.remove(value, hash, ctx)
	return removed, err
}

// Size returns the number of elements
func (s *SetValue) Size() int {
	return s.table.used
}

// Items returns all values in the set as a slice (insertion order).
func (s *SetValue) Items() []Value {
	values := make([]Value, 0, s.table.used)
	s.table.forEach(func(k, _ Value) bool {
		values = append(values, k)
		return true
	})
	return values
}

// Clear removes all elements in place.
func (s *SetValue) Clear() {
	s.table.clear()
}

// ForEach visits elements in insertion order; return false to stop.
func (s *SetValue) ForEach(fn func(elem Value) bool) {
	s.table.forEach(func(k, _ Value) bool {
		return fn(k)
	})
}

// forEachEntry visits elements with their cached hashes, so set algebra can
// probe other tables without rehashing.
func (s *SetValue) forEachEntry(fn func(hash uint64, elem Value) bool) {
	s.table.forEachEntry(func(h uint64, k, _ Value) bool {
		return fn(h, k)
	})
}

// addEntry inserts an element whose hash is already known (from another
// set's entry). ctx may be nil for structural (no user __eq__) merges.
func (s *SetValue) addEntry(elem Value, hash uint64, ctx *Context) error {
	_, err := s.table.insert(elem, hash, elem, ctx)
	return err
}

// containsHashed probes with a caller-provided hash (no rehash).
func (s *SetValue) containsHashed(elem Value, hash uint64, ctx *Context) (bool, error) {
	pos, err := s.table.lookup(elem, hash, ctx)
	if err != nil {
		return false, err
	}
	return pos >= 0, nil
}

// removeHashed removes with a caller-provided hash (no rehash).
func (s *SetValue) removeHashed(elem Value, hash uint64, ctx *Context) (bool, error) {
	_, removed, err := s.table.remove(elem, hash, ctx)
	return removed, err
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
	return &setIterator{values: s.Items(), index: 0}
}

// IteratorValue returns the iterator as a Value
func (s *SetValue) IteratorValue() Value {
	return &setIterator{values: s.Items(), index: 0}
}

// FrozenSetValue represents an immutable set of unique values
type FrozenSetValue struct {
	BaseObject
	table hashTable
	hash  uint64 // Cached hash value (0 = not computed)
}

// NewFrozenSet creates a new frozenset
func NewFrozenSet() *FrozenSetValue {
	return &FrozenSetValue{BaseObject: *NewBaseObject(FrozenSetType)}
}

// Type implements Value.Type
func (fs *FrozenSetValue) Type() Type {
	return FrozenSetType
}

// String implements Value.String
func (fs *FrozenSetValue) String() string {
	if fs.table.used == 0 {
		return "frozenset()"
	}
	return "frozenset(" + formatSetElems(fs.Items()) + ")"
}

// Add adds a value to the frozenset (internal use only during construction)
func (fs *FrozenSetValue) Add(value Value) {
	hash, err := HashValue(value, nil)
	if err != nil {
		hash = identityHash(value)
	}
	fs.table.insert(value, hash, value, nil)
	fs.hash = 0 // Invalidate cached hash
}

// AddWithError adds during construction with full Python semantics.
func (fs *FrozenSetValue) AddWithError(value Value, ctx *Context) error {
	hash, err := HashValue(value, ctx)
	if err != nil {
		return err
	}
	_, err = fs.table.insert(value, hash, value, ctx)
	fs.hash = 0
	return err
}

// Contains checks if a value is in the frozenset
func (fs *FrozenSetValue) Contains(value Value) bool {
	hash, err := HashValue(value, nil)
	if err != nil {
		hash = identityHash(value)
	}
	pos, _ := fs.table.lookup(value, hash, nil)
	return pos >= 0
}

// ContainsWithError is Contains with full Python semantics.
func (fs *FrozenSetValue) ContainsWithError(value Value, ctx *Context) (bool, error) {
	hash, err := HashValue(value, ctx)
	if err != nil {
		return false, err
	}
	pos, err := fs.table.lookup(value, hash, ctx)
	if err != nil {
		return false, err
	}
	return pos >= 0, nil
}

// Size returns the number of elements
func (fs *FrozenSetValue) Size() int {
	return fs.table.used
}

// Items returns all values as a slice (insertion order).
func (fs *FrozenSetValue) Items() []Value {
	values := make([]Value, 0, fs.table.used)
	fs.table.forEach(func(k, _ Value) bool {
		values = append(values, k)
		return true
	})
	return values
}

// ForEach visits elements in insertion order; return false to stop.
func (fs *FrozenSetValue) ForEach(fn func(elem Value) bool) {
	fs.table.forEach(func(k, _ Value) bool {
		return fn(k)
	})
}

// forEachEntry visits elements with their cached hashes.
func (fs *FrozenSetValue) forEachEntry(fn func(hash uint64, elem Value) bool) {
	fs.table.forEachEntry(func(h uint64, k, _ Value) bool {
		return fn(h, k)
	})
}

// addEntry inserts an element whose hash is already known.
func (fs *FrozenSetValue) addEntry(elem Value, hash uint64, ctx *Context) error {
	_, err := fs.table.insert(elem, hash, elem, ctx)
	fs.hash = 0
	return err
}

// containsHashed probes with a caller-provided hash.
func (fs *FrozenSetValue) containsHashed(elem Value, hash uint64, ctx *Context) (bool, error) {
	pos, err := fs.table.lookup(elem, hash, ctx)
	if err != nil {
		return false, err
	}
	return pos >= 0, nil
}

// Hash computes and caches the hash value for the frozenset
func (fs *FrozenSetValue) Hash() uint64 {
	// Delegates to the CPython-style element hash (cached); no ctx means
	// instance elements with user __hash__ hash by identity, which is all
	// this legacy entry point ever supported.
	h, err := fs.hashWithCtx(nil)
	if err != nil {
		return 1 // degraded bucket; equality still decides matches
	}
	return h
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
	return &setIterator{values: fs.Items(), index: 0}
}
