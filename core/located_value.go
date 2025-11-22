// Package core provides the fundamental types and interfaces for the M28 language.
package core

import "fmt"

// SourceLocation represents a position in source code
type SourceLocation struct {
	File    string
	Line    int
	Column  int
	EndLine int // Optional: end line for multi-line spans
	EndCol  int // Optional: end column for range highlighting
}

// String returns a human-readable representation of the source location
func (sl *SourceLocation) String() string {
	if sl == nil {
		return "<unknown>"
	}
	if sl.Column > 0 {
		return fmt.Sprintf("%s:%d:%d", sl.File, sl.Line, sl.Column)
	}
	return fmt.Sprintf("%s:%d", sl.File, sl.Line)
}

// LocatedValue wraps a Value with source location information
type LocatedValue struct {
	Value    Value
	Location *SourceLocation
}

// Unwrap returns the underlying value, handling nested LocatedValues
func (lv LocatedValue) Unwrap() Value {
	val := lv.Value
	for {
		if located, ok := val.(LocatedValue); ok {
			val = located.Value
		} else {
			return val
		}
	}
}

// Type returns the type of the underlying value
func (lv LocatedValue) Type() Type {
	return lv.Unwrap().Type()
}

// String returns the string representation of the underlying value
func (lv LocatedValue) String() string {
	return lv.Unwrap().String()
}

// GetLocation returns the source location of this value
func (lv LocatedValue) GetLocation() *SourceLocation {
	return lv.Location
}

// WithLocation wraps a value with source location information
func WithLocation(value Value, file string, line, column int) Value {
	return LocatedValue{
		Value: value,
		Location: &SourceLocation{
			File:   file,
			Line:   line,
			Column: column,
		},
	}
}

// GetValueLocation extracts source location from a value if it has one
func GetValueLocation(value Value) *SourceLocation {
	if lv, ok := value.(LocatedValue); ok {
		return lv.Location
	}
	return nil
}
