package core

import (
	"fmt"
	"runtime"
	"strings"
	"sync"
)

// RegistryEntry tracks where an item was registered
type RegistryEntry struct {
	Value    interface{}
	Location string // File:Line where it was registered
}

// Registry provides thread-safe registration with duplicate detection
type Registry struct {
	name    string
	entries map[string]RegistryEntry
	mu      sync.RWMutex
}

// NewRegistry creates a new registry with the given name
func NewRegistry(name string) *Registry {
	return &Registry{
		name:    name,
		entries: make(map[string]RegistryEntry),
	}
}

// Register adds an entry to the registry, returning an error if it already exists
func (r *Registry) Register(key string, value interface{}) error {
	r.mu.Lock()
	defer r.mu.Unlock()

	// Check if already registered
	if existing, exists := r.entries[key]; exists {
		return fmt.Errorf("%s '%s' already registered at %s, attempted to re-register at %s",
			r.name, key, existing.Location, getCallerLocation(2))
	}

	// Register new entry
	r.entries[key] = RegistryEntry{
		Value:    value,
		Location: getCallerLocation(2),
	}
	return nil
}

// MustRegister is like Register but panics on error
func (r *Registry) MustRegister(key string, value interface{}) {
	if err := r.Register(key, value); err != nil {
		panic(err)
	}
}

// Get retrieves a value from the registry
func (r *Registry) Get(key string) (interface{}, bool) {
	r.mu.RLock()
	defer r.mu.RUnlock()

	entry, exists := r.entries[key]
	if !exists {
		return nil, false
	}
	return entry.Value, true
}

// GetAll returns a copy of all entries
func (r *Registry) GetAll() map[string]interface{} {
	r.mu.RLock()
	defer r.mu.RUnlock()

	result := make(map[string]interface{}, len(r.entries))
	for k, v := range r.entries {
		result[k] = v.Value
	}
	return result
}

// Keys returns all registered keys
func (r *Registry) Keys() []string {
	r.mu.RLock()
	defer r.mu.RUnlock()

	keys := make([]string, 0, len(r.entries))
	for k := range r.entries {
		keys = append(keys, k)
	}
	return keys
}

// Clear removes all entries (useful for testing)
func (r *Registry) Clear() {
	r.mu.Lock()
	defer r.mu.Unlock()

	r.entries = make(map[string]RegistryEntry)
}

// getCallerLocation returns the file:line of the caller at the given depth
func getCallerLocation(depth int) string {
	_, file, line, ok := runtime.Caller(depth)
	if !ok {
		return "unknown"
	}

	// Simplify the path to be relative to the project root
	if idx := strings.Index(file, "m28/"); idx >= 0 {
		file = file[idx:]
	}

	return fmt.Sprintf("%s:%d", file, line)
}
