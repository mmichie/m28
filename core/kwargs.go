package core

// Kwargs is the insertion-ordered keyword-argument collection threaded through
// every kwargs-capable call path. PEP 468 requires **kwargs (and dict(a=1, ...))
// to observe call-site keyword order, which a Go map cannot provide.
//
// Representation: a small entry slice. Keyword sets are almost always tiny, so
// linear scans beat hashing; a lazy name index keeps large **d forwarding
// linear instead of quadratic.
//
// Reader methods (Len, Get, Has, Entries) are nil-receiver safe so call paths
// can pass nil for "no keyword arguments", mirroring the old nil-map idiom.
// Writers (Set, Delete) require a non-nil receiver from NewKwargs.
type Kwargs struct {
	entries []KwEntry
	idx     map[string]int // name -> entries index; built lazily past kwargsIdxThreshold
}

// KwEntry is a single keyword argument.
type KwEntry struct {
	Name  string
	Value Value
}

// kwargsIdxThreshold is the entry count above which name lookups switch from
// linear scan to a maintained index map.
const kwargsIdxThreshold = 12

// NewKwargs creates an empty keyword-argument collection with the given
// capacity hint.
func NewKwargs(capacity int) *Kwargs {
	if capacity <= 0 {
		return &Kwargs{}
	}
	return &Kwargs{entries: make([]KwEntry, 0, capacity)}
}

// Len returns the number of keyword arguments. Safe on nil.
func (k *Kwargs) Len() int {
	if k == nil {
		return 0
	}
	return len(k.entries)
}

// find returns the entry index for name, or -1.
func (k *Kwargs) find(name string) int {
	if k.idx != nil {
		if i, ok := k.idx[name]; ok {
			return i
		}
		return -1
	}
	for i := range k.entries {
		if k.entries[i].Name == name {
			return i
		}
	}
	return -1
}

// Get returns the value bound to name. Safe on nil.
func (k *Kwargs) Get(name string) (Value, bool) {
	if k == nil {
		return nil, false
	}
	if i := k.find(name); i >= 0 {
		return k.entries[i].Value, true
	}
	return nil, false
}

// Has reports whether name is present. Safe on nil.
func (k *Kwargs) Has(name string) bool {
	if k == nil {
		return false
	}
	return k.find(name) >= 0
}

// Set binds name to value. A new name appends (preserving insertion order); an
// existing name is overwritten in place, keeping its original position — the
// same rule as dict assignment.
func (k *Kwargs) Set(name string, value Value) {
	if i := k.find(name); i >= 0 {
		k.entries[i].Value = value
		return
	}
	k.entries = append(k.entries, KwEntry{Name: name, Value: value})
	if k.idx != nil {
		k.idx[name] = len(k.entries) - 1
	} else if len(k.entries) > kwargsIdxThreshold {
		k.idx = make(map[string]int, len(k.entries)*2)
		for i := range k.entries {
			k.idx[k.entries[i].Name] = i
		}
	}
}

// Delete removes name, preserving the order of the remaining entries. Returns
// whether the name was present. Deletion is rare and keyword sets are small,
// so the index (if any) is rebuilt rather than patched.
func (k *Kwargs) Delete(name string) bool {
	i := k.find(name)
	if i < 0 {
		return false
	}
	k.entries = append(k.entries[:i], k.entries[i+1:]...)
	if k.idx != nil {
		if len(k.entries) <= kwargsIdxThreshold {
			k.idx = nil
		} else {
			k.idx = make(map[string]int, len(k.entries)*2)
			for j := range k.entries {
				k.idx[k.entries[j].Name] = j
			}
		}
	}
	return true
}

// Entries returns the keyword arguments in insertion order. The slice is
// borrowed from the receiver: callers must not mutate it. Safe on nil.
func (k *Kwargs) Entries() []KwEntry {
	if k == nil {
		return nil
	}
	return k.entries
}

// Clone returns an independent copy. Safe on nil (returns an empty collection).
func (k *Kwargs) Clone() *Kwargs {
	out := NewKwargs(k.Len())
	if k != nil {
		out.entries = append(out.entries, k.entries...)
		if k.idx != nil {
			out.idx = make(map[string]int, len(k.idx))
			for name, i := range k.idx {
				out.idx[name] = i
			}
		}
	}
	return out
}

// ToDict builds a fresh string-keyed DictValue in insertion order. This is the
// dict a **kwargs parameter receives; it is always a new object so callee
// mutations cannot alias the call site's keywords. Safe on nil.
func (k *Kwargs) ToDict() *DictValue {
	d := NewDict()
	if k != nil {
		for i := range k.entries {
			d.SetStr(k.entries[i].Name, k.entries[i].Value)
		}
	}
	return d
}
