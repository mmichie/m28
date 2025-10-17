package core

import (
	"sync"
)

// IRMetadata stores metadata about IR nodes that can't be embedded in the Value itself.
// This keeps Value types lightweight and avoids the "LocatedValue wrapper" problem.
//
// The metadata is stored in separate maps indexed by the Value pointer.
// This allows us to track:
// - Source locations (for error messages)
// - Type annotations (for gradual typing)
// - Comments (for documentation)
// - Original syntax (Python vs Lisp for error display)
//
// Usage:
//
//	metadata := NewIRMetadata()
//	ir := core.ListValue{...}
//	metadata.SetLocation(ir, &core.SourceLocation{File: "test.py", Line: 10, Col: 5})
//	loc := metadata.GetLocation(ir)  // Retrieve later
type IRMetadata struct {
	mu        sync.RWMutex
	locations map[Value]*SourceLocation
	types     map[Value]interface{} // Will hold *ast.TypeInfo once ast package imports
	comments  map[Value][]string
	syntax    map[Value]int // SyntaxKind (int to avoid import cycle)
}

// NewIRMetadata creates a new metadata table
func NewIRMetadata() *IRMetadata {
	return &IRMetadata{
		locations: make(map[Value]*SourceLocation),
		types:     make(map[Value]interface{}),
		comments:  make(map[Value][]string),
		syntax:    make(map[Value]int),
	}
}

// SetLocation stores the source location for an IR value
func (m *IRMetadata) SetLocation(val Value, loc *SourceLocation) {
	if m == nil || val == nil || loc == nil {
		return
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	m.locations[val] = loc
}

// GetLocation retrieves the source location for an IR value
func (m *IRMetadata) GetLocation(val Value) *SourceLocation {
	if m == nil || val == nil {
		return nil
	}
	m.mu.RLock()
	defer m.mu.RUnlock()
	return m.locations[val]
}

// SetTypeAnnotation stores type annotation for an IR value
func (m *IRMetadata) SetTypeAnnotation(val Value, typeInfo interface{}) {
	if m == nil || val == nil || typeInfo == nil {
		return
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	m.types[val] = typeInfo
}

// GetTypeAnnotation retrieves type annotation for an IR value
func (m *IRMetadata) GetTypeAnnotation(val Value) interface{} {
	if m == nil || val == nil {
		return nil
	}
	m.mu.RLock()
	defer m.mu.RUnlock()
	return m.types[val]
}

// SetComments stores comments for an IR value
func (m *IRMetadata) SetComments(val Value, comments []string) {
	if m == nil || val == nil || len(comments) == 0 {
		return
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	m.comments[val] = comments
}

// GetComments retrieves comments for an IR value
func (m *IRMetadata) GetComments(val Value) []string {
	if m == nil || val == nil {
		return nil
	}
	m.mu.RLock()
	defer m.mu.RUnlock()
	return m.comments[val]
}

// SetSyntaxKind stores the original syntax kind for an IR value
func (m *IRMetadata) SetSyntaxKind(val Value, kind int) {
	if m == nil || val == nil {
		return
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	m.syntax[val] = kind
}

// GetSyntaxKind retrieves the original syntax kind for an IR value
func (m *IRMetadata) GetSyntaxKind(val Value) (int, bool) {
	if m == nil || val == nil {
		return 0, false
	}
	m.mu.RLock()
	defer m.mu.RUnlock()
	kind, ok := m.syntax[val]
	return kind, ok
}

// Clear removes all metadata (useful for optimization passes)
func (m *IRMetadata) Clear() {
	if m == nil {
		return
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	m.locations = make(map[Value]*SourceLocation)
	m.types = make(map[Value]interface{})
	m.comments = make(map[Value][]string)
	m.syntax = make(map[Value]int)
}

// Size returns the number of tracked values
func (m *IRMetadata) Size() int {
	if m == nil {
		return 0
	}
	m.mu.RLock()
	defer m.mu.RUnlock()
	return len(m.locations)
}

// Merge combines metadata from another table (useful for module imports)
func (m *IRMetadata) Merge(other *IRMetadata) {
	if m == nil || other == nil {
		return
	}

	other.mu.RLock()
	defer other.mu.RUnlock()

	m.mu.Lock()
	defer m.mu.Unlock()

	for k, v := range other.locations {
		m.locations[k] = v
	}
	for k, v := range other.types {
		m.types[k] = v
	}
	for k, v := range other.comments {
		m.comments[k] = v
	}
	for k, v := range other.syntax {
		m.syntax[k] = v
	}
}
