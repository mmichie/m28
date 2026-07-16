package core

// hashTable is the storage engine behind DictValue (and, in a later phase,
// sets): a CPython-shaped compact ordered hash table. Entries live in
// insertion order in a slice; an open-addressed index of power-of-two size
// maps probe slots to entry positions. Deleted entries become tombstones
// (key == nil) and dummy index slots, compacted away on growth.
//
// Key identity is hash + equality (SameObject, then __eq__ semantics via
// dictKeysEqual). A ctx is required only when user __eq__/__hash__ can run;
// nil-ctx callers get structural equality, which is exact for everything
// except instances with user dunders.

const (
	dictMinSize  = 8
	perturbShift = 5
	slotEmpty    = int32(-1)
	slotDummy    = int32(-2)
)

type dictEntry struct {
	hash  uint64
	key   Value // nil marks a tombstone
	value Value
}

type hashTable struct {
	entries []dictEntry
	index   []int32
	used    int // live entries
	fill    int // used + dummy slots referenced by the index
}

// dictKeysEqual decides key identity during probing. Hash equality has
// already been checked. Stored key goes first, matching CPython's comparison
// order for user __eq__.
func dictKeysEqual(stored, search Value, ctx *Context) (bool, error) {
	if ss, ok := stored.(StringValue); ok {
		if tt, ok := search.(StringValue); ok {
			return ss == tt, nil
		}
	}
	if SameObject(stored, search) {
		return true, nil
	}
	if ctx != nil {
		_, si := stored.(*Instance)
		_, ti := search.(*Instance)
		if si {
			return EqualValuesWithError(stored, search, ctx)
		}
		if ti {
			// EqualValuesWithError dispatches on (and reflects from) its FIRST
			// operand, so when only the search key is an Instance put it first;
			// otherwise its custom __eq__ is skipped and any exception it raises
			// swallowed (a dict lookup would wrongly miss / stay silent). Mirrors
			// core.ContainsEqual, the same fix for the `in` operator.
			return EqualValuesWithError(search, stored, ctx)
		}
	}
	return EqualValues(stored, search), nil
}

// lookup returns the entries position for key, or -1.
func (t *hashTable) lookup(key Value, hash uint64, ctx *Context) (int, error) {
	if t.index == nil {
		return -1, nil
	}
	mask := uint64(len(t.index) - 1)
	perturb := hash
	i := hash & mask
	for {
		slot := t.index[i]
		if slot == slotEmpty {
			return -1, nil
		}
		if slot != slotDummy {
			e := &t.entries[slot]
			if e.hash == hash {
				eq, err := dictKeysEqual(e.key, key, ctx)
				if err != nil {
					return -1, err
				}
				if eq {
					return int(slot), nil
				}
			}
		}
		perturb >>= perturbShift
		i = (5*i + perturb + 1) & mask
	}
}

// insert adds or replaces key. Existing keys keep their first-inserted key
// object and order slot; only the value is replaced. Returns true when a new
// entry was created (a structural change).
func (t *hashTable) insert(key Value, hash uint64, value Value, ctx *Context) (bool, error) {
	if t.index == nil {
		t.rebuild(dictMinSize)
	}
	mask := uint64(len(t.index) - 1)
	perturb := hash
	i := hash & mask
	freeSlot := -1
	for {
		slot := t.index[i]
		if slot == slotEmpty {
			break
		}
		if slot == slotDummy {
			if freeSlot < 0 {
				freeSlot = int(i)
			}
		} else {
			e := &t.entries[slot]
			if e.hash == hash {
				eq, err := dictKeysEqual(e.key, key, ctx)
				if err != nil {
					return false, err
				}
				if eq {
					e.value = value
					return false, nil
				}
			}
		}
		perturb >>= perturbShift
		i = (5*i + perturb + 1) & mask
	}

	// New key. Grow first if the index is too full; growth compacts and
	// re-probes, so insert cleanly afterwards.
	if (t.fill+1)*3 >= len(t.index)*2 {
		t.rebuild(growSize(t.used + 1))
		t.entries = append(t.entries, dictEntry{hash: hash, key: key, value: value})
		t.linkEntry(len(t.entries) - 1)
		t.used++
		t.fill++
		return true, nil
	}

	t.entries = append(t.entries, dictEntry{hash: hash, key: key, value: value})
	if freeSlot >= 0 {
		t.index[freeSlot] = int32(len(t.entries) - 1) // consume a dummy: fill unchanged
	} else {
		t.index[i] = int32(len(t.entries) - 1)
		t.fill++
	}
	t.used++
	return true, nil
}

// remove deletes key, returning its value. O(1): the index slot becomes a
// dummy and the entry a tombstone.
func (t *hashTable) remove(key Value, hash uint64, ctx *Context) (Value, bool, error) {
	if t.index == nil {
		return nil, false, nil
	}
	mask := uint64(len(t.index) - 1)
	perturb := hash
	i := hash & mask
	for {
		slot := t.index[i]
		if slot == slotEmpty {
			return nil, false, nil
		}
		if slot != slotDummy {
			e := &t.entries[slot]
			if e.hash == hash {
				eq, err := dictKeysEqual(e.key, key, ctx)
				if err != nil {
					return nil, false, err
				}
				if eq {
					val := e.value
					t.index[i] = slotDummy
					t.entries[slot] = dictEntry{}
					t.used--
					t.trimAndCompact()
					return val, true, nil
				}
			}
		}
		perturb >>= perturbShift
		i = (5*i + perturb + 1) & mask
	}
}

// unlinkEntry removes a known live entry (by position) from the index and
// tombstones it. Used by popLast, where no key comparison is needed.
func (t *hashTable) unlinkEntry(pos int) {
	hash := t.entries[pos].hash
	mask := uint64(len(t.index) - 1)
	perturb := hash
	i := hash & mask
	for {
		if t.index[i] == int32(pos) {
			t.index[i] = slotDummy
			break
		}
		perturb >>= perturbShift
		i = (5*i + perturb + 1) & mask
	}
	t.entries[pos] = dictEntry{}
	t.used--
	t.trimAndCompact()
}

// trimAndCompact drops trailing tombstones (keeps popLast/append cheap for
// queue-like usage) and rebuilds when tombstones dominate the entries slice.
func (t *hashTable) trimAndCompact() {
	n := len(t.entries)
	for n > 0 && t.entries[n-1].key == nil {
		n--
	}
	t.entries = t.entries[:n]
	if len(t.entries) >= 32 && (len(t.entries)-t.used)*2 > len(t.entries) {
		t.rebuild(growSize(t.used))
	}
}

// linkEntry probes for the first free index slot for a known-distinct entry.
// Only used on a freshly rebuilt index (no dummies, no duplicate keys), so no
// equality calls are needed.
func (t *hashTable) linkEntry(pos int) {
	hash := t.entries[pos].hash
	mask := uint64(len(t.index) - 1)
	perturb := hash
	i := hash & mask
	for t.index[i] != slotEmpty {
		perturb >>= perturbShift
		i = (5*i + perturb + 1) & mask
	}
	t.index[i] = int32(pos)
}

// rebuild compacts entries (dropping tombstones, preserving order) and
// rebuilds the index at the given size.
func (t *hashTable) rebuild(size int) {
	if size < dictMinSize {
		size = dictMinSize
	}
	live := t.entries
	if t.used != len(t.entries) {
		live = make([]dictEntry, 0, t.used)
		for _, e := range t.entries {
			if e.key != nil {
				live = append(live, e)
			}
		}
	}
	t.entries = live
	t.index = make([]int32, size)
	for i := range t.index {
		t.index[i] = slotEmpty
	}
	t.fill = t.used
	for pos := range t.entries {
		t.linkEntry(pos)
	}
}

// growSize returns the index size for n entries: the smallest power of two
// holding n*3 slots (so the table lands at ~1/3 load after growth).
func growSize(n int) int {
	size := dictMinSize
	for size < n*3 {
		size <<= 1
	}
	return size
}

// forEach visits live entries in insertion order. Return false to stop.
// Mutating the table during iteration is not supported at this layer;
// callers that allow it snapshot first.
func (t *hashTable) forEach(fn func(key, value Value) bool) {
	for i := range t.entries {
		e := &t.entries[i]
		if e.key == nil {
			continue
		}
		if !fn(e.key, e.value) {
			return
		}
	}
}

// forEachEntry visits live entries in insertion order together with their
// cached hashes, so set algebra can probe other tables without rehashing.
func (t *hashTable) forEachEntry(fn func(hash uint64, key, value Value) bool) {
	for i := range t.entries {
		e := &t.entries[i]
		if e.key == nil {
			continue
		}
		if !fn(e.hash, e.key, e.value) {
			return
		}
	}
}

// popLast removes and returns the most recently inserted live entry.
func (t *hashTable) popLast() (Value, Value, bool) {
	for i := len(t.entries) - 1; i >= 0; i-- {
		if t.entries[i].key != nil {
			k, v := t.entries[i].key, t.entries[i].value
			t.unlinkEntry(i)
			return k, v, true
		}
	}
	return nil, nil, false
}

func (t *hashTable) clear() {
	t.entries = nil
	t.index = nil
	t.used = 0
	t.fill = 0
}

// serializeKeyRepr renders a key in the legacy keyRepr form ("s:name", "n:1",
// ...). It is retained only to give set/frozenset repr a deterministic,
// byte-stable element ordering (see formatSetElems in repr.go); it is no longer
// used to identify keys in the hash table.
func serializeKeyRepr(key Value) string {
	switch k := key.(type) {
	case StringValue:
		return "s:" + string(k)
	case NumberValue:
		return numberKey(float64(k))
	case FloatValue:
		return numberKey(float64(k))
	case BoolValue:
		if bool(k) {
			return "n:1"
		}
		return "n:0"
	case NilValue:
		return "nil"
	default:
		return ValueToKey(key)
	}
}
