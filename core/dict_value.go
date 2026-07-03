package core

// DictValue is M28's dict: a compact insertion-ordered hash table (see
// docs/dict-engine.md). Key identity is hash + equality — never a serialized
// encoding. The ctx-aware API (GetItem/SetItem/DelItem) implements full
// Python semantics including user __hash__/__eq__; the string API
// (GetStr/SetStr) is the allocation-free hot path for name lookups; the
// legacy no-ctx API (GetValue/SetValue/DeleteValue) serves primitive-keyed
// callers and degrades instances to identity semantics.
type DictValue struct {
	BaseObject
	table    hashTable
	modCount uint64 // structural-modification counter (insert/delete of keys)
}

// NewDict creates a new empty dictionary. Storage is allocated lazily on
// first insert, so empty dicts are cheap.
func NewDict() *DictValue {
	return &DictValue{BaseObject: *NewBaseObject(DictType)}
}

// Type implements Value.Type
func (d *DictValue) Type() Type {
	return DictType
}

// String implements Value.String
func (d *DictValue) String() string {
	// In Python str(dict) == repr(dict): keys and values use repr (single-quoted
	// strings). Delegate to formatDictRepr so str()/print() agree with repr().
	return formatDictRepr(d)
}

// Size returns the number of entries in the dictionary.
func (d *DictValue) Size() int {
	return d.table.used
}

// --- Full-fidelity API (ctx-aware) ------------------------------------------

// GetItem looks up key with full Python semantics. Errors come from
// unhashable keys or user __hash__/__eq__ raising.
func (d *DictValue) GetItem(key Value, ctx *Context) (Value, bool, error) {
	hash, err := HashValue(key, ctx)
	if err != nil {
		return nil, false, err
	}
	pos, err := d.table.lookup(key, hash, ctx)
	if err != nil || pos < 0 {
		return nil, false, err
	}
	return d.table.entries[pos].value, true, nil
}

// SetItem inserts or replaces key with full Python semantics. An existing
// equal key keeps its first-inserted key object and order position.
func (d *DictValue) SetItem(key, value Value, ctx *Context) error {
	hash, err := HashValue(key, ctx)
	if err != nil {
		return err
	}
	created, err := d.table.insert(key, hash, value, ctx)
	if err != nil {
		return err
	}
	if created {
		d.modCount++
	}
	return nil
}

// DelItem removes key with full Python semantics, reporting whether it was
// present.
func (d *DictValue) DelItem(key Value, ctx *Context) (bool, error) {
	hash, err := HashValue(key, ctx)
	if err != nil {
		return false, err
	}
	_, removed, err := d.table.remove(key, hash, ctx)
	if err != nil {
		return false, err
	}
	if removed {
		d.modCount++
	}
	return removed, nil
}

// Pop removes key and returns its previous value (dict.pop).
func (d *DictValue) Pop(key Value, ctx *Context) (Value, bool, error) {
	hash, err := HashValue(key, ctx)
	if err != nil {
		return nil, false, err
	}
	val, removed, err := d.table.remove(key, hash, ctx)
	if err != nil {
		return nil, false, err
	}
	if removed {
		d.modCount++
	}
	return val, removed, nil
}

// StoredKey returns the first-inserted key object equal to key, if present.
func (d *DictValue) StoredKey(key Value, ctx *Context) (Value, bool, error) {
	hash, err := HashValue(key, ctx)
	if err != nil {
		return nil, false, err
	}
	pos, err := d.table.lookup(key, hash, ctx)
	if err != nil || pos < 0 {
		return nil, false, err
	}
	return d.table.entries[pos].key, true, nil
}

// ForEach visits entries in insertion order; return false to stop. The walk
// is live — do not mutate d inside fn (snapshot first, as the iterators do).
func (d *DictValue) ForEach(fn func(key, value Value) bool) {
	d.table.forEach(fn)
}

// PopLast removes and returns the most recently inserted entry (popitem).
func (d *DictValue) PopLast() (Value, Value, bool) {
	k, v, ok := d.table.popLast()
	if ok {
		d.modCount++
	}
	return k, v, ok
}

// Clear removes all entries from the dict in place.
func (d *DictValue) Clear() {
	if d.table.used > 0 {
		d.modCount++
	}
	d.table.clear()
}

// Update copies every entry from other into d (like dict.update for real
// dicts), preserving other's key objects and reusing its cached hashes.
// User __eq__ is not consulted (no ctx); dict.update() goes through
// UpdateWithContext.
func (d *DictValue) Update(other *DictValue) {
	d.updateFrom(other, nil)
}

// UpdateWithContext is Update with user __eq__/__hash__ semantics.
func (d *DictValue) UpdateWithContext(other *DictValue, ctx *Context) error {
	return d.updateFrom(other, ctx)
}

func (d *DictValue) updateFrom(other *DictValue, ctx *Context) error {
	for i := range other.table.entries {
		e := &other.table.entries[i]
		if e.key == nil {
			continue
		}
		created, err := d.table.insert(e.key, e.hash, e.value, ctx)
		if err != nil {
			return err
		}
		if created {
			d.modCount++
		}
	}
	return nil
}

// --- String fast path ---------------------------------------------------
//
// Module globals, class namespaces and kwargs are name -> value tables; name
// resolution runs through here on nearly every lookup, so these avoid
// HashValue dispatch and never allocate.

// HashStr exposes the engine's string hash so hot paths (name resolution)
// can hash a name once and probe many scopes with GetStrHashed.
func HashStr(name string) uint64 {
	return hashString(name)
}

// GetStr looks up the value stored under a plain string key.
func (d *DictValue) GetStr(name string) (Value, bool) {
	return d.GetStrHashed(hashString(name), name)
}

// GetStrHashed is GetStr with a caller-precomputed hash (see HashStr).
func (d *DictValue) GetStrHashed(hash uint64, name string) (Value, bool) {
	t := &d.table
	if t.index == nil {
		return nil, false
	}
	mask := uint64(len(t.index) - 1)
	perturb := hash
	i := hash & mask
	for {
		slot := t.index[i]
		if slot == slotEmpty {
			return nil, false
		}
		if slot != slotDummy {
			e := &t.entries[slot]
			if e.hash == hash {
				if s, ok := e.key.(StringValue); ok {
					if string(s) == name {
						return e.value, true
					}
				} else if bs, ok := StrBacking(e.key); ok && string(bs) == name {
					return e.value, true
				}
			}
		}
		perturb >>= perturbShift
		i = (5*i + perturb + 1) & mask
	}
}

// SetStr stores value under a plain string key.
func (d *DictValue) SetStr(name string, value Value) {
	d.SetStrHashed(hashString(name), name, value)
}

// SetStrHashed is SetStr with a caller-precomputed hash (see HashStr);
// module-tier IR nodes compute the hash once at compile time.
func (d *DictValue) SetStrHashed(hash uint64, name string, value Value) {
	created, _ := d.table.insert(StringValue(name), hash, value, nil)
	if created {
		d.modCount++
	}
}

// DeleteStr removes a plain string key, reporting whether it was present.
func (d *DictValue) DeleteStr(name string) bool {
	_, removed, _ := d.table.remove(StringValue(name), hashString(name), nil)
	if removed {
		d.modCount++
	}
	return removed
}

// --- Legacy no-context API ---------------------------------------------
//
// Exact for primitive keys. Instance keys with user dunders degrade to
// identity semantics (no user code runs without a ctx); Python-visible dict
// operations all pass a ctx.

// GetValue retrieves a value by M28 value key.
func (d *DictValue) GetValue(key Value) (Value, bool) {
	hash, err := HashValue(key, nil)
	if err != nil {
		return nil, false
	}
	pos, _ := d.table.lookup(key, hash, nil)
	if pos < 0 {
		return nil, false
	}
	return d.table.entries[pos].value, true
}

// SetValue sets a value by M28 value key. Unhashable keys return TypeError.
func (d *DictValue) SetValue(key Value, value Value) error {
	hash, err := HashValue(key, nil)
	if err != nil {
		return err
	}
	created, err := d.table.insert(key, hash, value, nil)
	if err != nil {
		return err
	}
	if created {
		d.modCount++
	}
	return nil
}

// DeleteValue removes a key by M28 value, reporting whether it was present.
func (d *DictValue) DeleteValue(key Value) bool {
	hash, err := HashValue(key, nil)
	if err != nil {
		return false
	}
	_, removed, _ := d.table.remove(key, hash, nil)
	if removed {
		d.modCount++
	}
	return removed
}

// OriginalKeys returns the key objects in insertion order.
func (d *DictValue) OriginalKeys() []Value {
	out := make([]Value, 0, d.table.used)
	d.table.forEach(func(k, _ Value) bool {
		out = append(out, k)
		return true
	})
	return out
}

// --- Deprecated keyRepr adapters -----------------------------------------
//
// Compatibility with the old string-serialized key API; see hashtable.go.
// New code should use GetItem/SetItem/GetStr/SetStr/ForEach.

// Get retrieves a value by legacy internal key representation. Unparseable
// representations fall back to a plain string key, mirroring Set.
//
// Deprecated: use GetItem, GetValue or GetStr.
func (d *DictValue) Get(keyRepr string) (Value, bool) {
	if len(keyRepr) >= 2 && keyRepr[0] == 's' && keyRepr[1] == ':' {
		return d.GetStr(keyRepr[2:])
	}
	key, ok := parseKeyRepr(keyRepr)
	if !ok {
		return d.GetStr(keyRepr)
	}
	return d.GetValue(key)
}

// Set sets a value by legacy internal key representation.
//
// Deprecated: use SetItem, SetValue or SetStr.
func (d *DictValue) Set(keyRepr string, value Value) {
	if len(keyRepr) >= 2 && keyRepr[0] == 's' && keyRepr[1] == ':' {
		d.SetStr(keyRepr[2:], value)
		return
	}
	key, ok := parseKeyRepr(keyRepr)
	if !ok {
		// Match the old reconstruction fallback: treat the repr as a string.
		key = StringValue(keyRepr)
	}
	_ = d.SetValue(key, value)
}

// SetWithKey sets a value given both the legacy key representation and the
// original key. The representation is ignored — the original key carries all
// information — so this adapter is always correct.
//
// Deprecated: use SetItem, SetValue or SetStr.
func (d *DictValue) SetWithKey(keyRepr string, origKey Value, value Value) {
	_ = d.SetValue(origKey, value)
}

// Delete removes a key by legacy internal representation. Unparseable
// representations fall back to a plain string key, mirroring Set.
//
// Deprecated: use DelItem, DeleteValue or DeleteStr.
func (d *DictValue) Delete(keyRepr string) {
	if len(keyRepr) >= 2 && keyRepr[0] == 's' && keyRepr[1] == ':' {
		d.DeleteStr(keyRepr[2:])
		return
	}
	key, ok := parseKeyRepr(keyRepr)
	if !ok {
		key = StringValue(keyRepr)
	}
	d.DeleteValue(key)
}

// Keys returns legacy internal key representations in insertion order.
//
// Deprecated: use ForEach or OriginalKeys.
func (d *DictValue) Keys() []string {
	out := make([]string, 0, d.table.used)
	d.table.forEach(func(k, _ Value) bool {
		out = append(out, serializeKeyRepr(k))
		return true
	})
	return out
}

// OriginalKeyValue returns the stored key object for a legacy internal key
// representation, if resolvable.
//
// Deprecated: use StoredKey.
func (d *DictValue) OriginalKeyValue(keyRepr string) (Value, bool) {
	key, ok := parseKeyRepr(keyRepr)
	if !ok {
		return nil, false
	}
	k, found, _ := d.StoredKey(key, nil)
	if !found {
		return nil, false
	}
	return k, true
}

// --- Iteration -----------------------------------------------------------

// Iterator implements Iterable for dicts (iterates over keys). The key list
// is snapshotted; structural modification during iteration is detected via
// modCount by the __next__ wrapper.
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
				return nil, &StopIteration{}
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

	// Then fall back to string-keyed entries so dot notation (dict.key)
	// reaches plain string keys.
	if val, exists := d.GetStr(name); exists {
		return val, true
	}

	// Finally check BaseObject
	return d.BaseObject.GetAttr(name)
}

// SetAttr implements Object.SetAttr for dictionary key assignment
func (d *DictValue) SetAttr(name string, value Value) error {
	// Dot-notation keys are always plain strings.
	d.SetStr(name, value)
	return nil
}
