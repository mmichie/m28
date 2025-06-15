// Package protocols defines standard interfaces for M28 types to implement
// common operations like arithmetic, indexing, and iteration.
package protocols

import "github.com/mmichie/m28/core"

// Numeric protocol for types that support arithmetic operations
type Numeric interface {
	// Basic arithmetic operations
	Add(other core.Value) (core.Value, error)
	Subtract(other core.Value) (core.Value, error)
	Multiply(other core.Value) (core.Value, error)
	Divide(other core.Value) (core.Value, error)
	Modulo(other core.Value) (core.Value, error)
	Power(other core.Value) (core.Value, error)

	// Unary operations
	Negate() (core.Value, error)
	Absolute() (core.Value, error)
}

// Indexable protocol for types that support indexing operations
type Indexable interface {
	// Get value at index
	GetIndex(index core.Value) (core.Value, error)
	// Set value at index
	SetIndex(index core.Value, value core.Value) error
	// Check if index exists
	HasIndex(index core.Value) bool
	// Delete value at index
	DeleteIndex(index core.Value) error
}

// Container protocol for types that have size and can contain items
type Container interface {
	// Get the number of items
	Size() int
	// Check if container contains an item
	Contains(item core.Value) bool
	// Check if container is empty
	IsEmpty() bool
}

// Iterable protocol for types that can be iterated over
type Iterable interface {
	// Get an iterator for this value
	Iter() (Iterator, error)
}

// Iterator protocol for iteration state
type Iterator interface {
	// Get the next value
	Next() (core.Value, error)
	// Check if there are more values
	HasNext() bool
}

// Comparable protocol for types that support ordering
type Comparable interface {
	// Compare returns -1 if self < other, 0 if equal, 1 if self > other
	Compare(other core.Value) (int, error)
	// Equal checks equality (may be more efficient than Compare)
	Equal(other core.Value) bool
}

// Hashable protocol for types that can be used as dict keys
type Hashable interface {
	// Hash returns a string representation suitable for use as a dict key
	Hash() string
}

// Representable protocol for types with string representations
type Representable interface {
	// String returns a human-readable string representation
	String() string
	// Repr returns a developer-friendly representation
	Repr() string
}

// Protocol checking functions

// AsNumeric attempts to get the Numeric protocol from a value
func AsNumeric(v core.Value) (Numeric, bool) {
	if n, ok := v.(Numeric); ok {
		return n, true
	}
	return nil, false
}

// AsIndexable attempts to get the Indexable protocol from a value
func AsIndexable(v core.Value) (Indexable, bool) {
	if idx, ok := v.(Indexable); ok {
		return idx, true
	}
	return nil, false
}

// AsContainer attempts to get the Container protocol from a value
func AsContainer(v core.Value) (Container, bool) {
	if c, ok := v.(Container); ok {
		return c, true
	}
	return nil, false
}

// AsIterable attempts to get the Iterable protocol from a value
func AsIterable(v core.Value) (Iterable, bool) {
	if iter, ok := v.(Iterable); ok {
		return iter, true
	}
	return nil, false
}

// AsComparable attempts to get the Comparable protocol from a value
func AsComparable(v core.Value) (Comparable, bool) {
	if c, ok := v.(Comparable); ok {
		return c, true
	}
	return nil, false
}

// AsHashable attempts to get the Hashable protocol from a value
func AsHashable(v core.Value) (Hashable, bool) {
	if h, ok := v.(Hashable); ok {
		return h, true
	}
	return nil, false
}
