package core

import (
	"fmt"
)

// HashWrapper is used to create a consistent hashable wrapper for dictionary keys
type HashWrapper struct {
	Value LispValue
	hash  uint32
}

// Hash returns the hash code for this wrapper
func (hw *HashWrapper) Hash() uint32 {
	if hw.hash == 0 {
		hw.hash = calculateHash(hw.Value)
	}
	return hw.hash
}

// Equals checks if two wrappers are equal based on the underlying values
func (hw *HashWrapper) Equals(other *HashWrapper) bool {
	return EqualValues(hw.Value, other.Value)
}

// String returns a string representation of the wrapper
func (hw *HashWrapper) String() string {
	return fmt.Sprintf("%v", hw.Value)
}

// createHashWrapper creates a new HashWrapper for the given value
func createHashWrapper(value LispValue) *HashWrapper {
	return &HashWrapper{
		Value: value,
	}
}

// calculateHash computes a hash code for any LispValue
func calculateHash(value LispValue) uint32 {
	// Check if value implements Hashable interface
	if h, ok := value.(Hashable); ok {
		return h.Hash()
	}

	// Fall back to string-based hash for other types
	strRep := PrintValue(value)
	h := uint32(2166136261) // FNV-1a offset basis
	for i := 0; i < len(strRep); i++ {
		h = (h * 16777619) ^ uint32(strRep[i]) // FNV-1a prime
	}
	return h
}

// DictDataStore provides a storage mechanism for PythonicDict with proper equality semantics
type DictDataStore struct {
	entries []*dictEntry
}

type dictEntry struct {
	key   *HashWrapper
	value LispValue
}

// NewDictDataStore creates a new empty data store
func NewDictDataStore() *DictDataStore {
	return &DictDataStore{
		entries: make([]*dictEntry, 0),
	}
}

// Get retrieves a value by key, returning the value and whether it was found
func (ds *DictDataStore) Get(key LispValue) (LispValue, bool) {
	hw := createHashWrapper(key)

	for _, entry := range ds.entries {
		if entry.key.Equals(hw) {
			return entry.value, true
		}
	}

	return nil, false
}

// Set sets or updates a key-value pair
func (ds *DictDataStore) Set(key LispValue, value LispValue) {
	hw := createHashWrapper(key)

	// Check if key exists
	for _, entry := range ds.entries {
		if entry.key.Equals(hw) {
			entry.value = value
			return
		}
	}

	// Add new entry
	ds.entries = append(ds.entries, &dictEntry{
		key:   hw,
		value: value,
	})
}

// Delete removes a key-value pair
func (ds *DictDataStore) Delete(key LispValue) {
	hw := createHashWrapper(key)

	for i, entry := range ds.entries {
		if entry.key.Equals(hw) {
			// Remove entry by swapping with last element and truncating
			lastIdx := len(ds.entries) - 1
			ds.entries[i] = ds.entries[lastIdx]
			ds.entries = ds.entries[:lastIdx]
			return
		}
	}
}

// Contains checks if a key exists
func (ds *DictDataStore) Contains(key LispValue) bool {
	hw := createHashWrapper(key)

	for _, entry := range ds.entries {
		if entry.key.Equals(hw) {
			return true
		}
	}

	return false
}

// Size returns the number of entries
func (ds *DictDataStore) Size() int {
	return len(ds.entries)
}

// Keys returns all keys
func (ds *DictDataStore) Keys() []LispValue {
	keys := make([]LispValue, len(ds.entries))

	for i, entry := range ds.entries {
		keys[i] = entry.key.Value
	}

	return keys
}

// Clear removes all entries
func (ds *DictDataStore) Clear() {
	ds.entries = make([]*dictEntry, 0)
}

// Iterate applies a function to each key-value pair
func (ds *DictDataStore) Iterate(f func(k, v LispValue) error) error {
	for _, entry := range ds.entries {
		if err := f(entry.key.Value, entry.value); err != nil {
			return err
		}
	}
	return nil
}
