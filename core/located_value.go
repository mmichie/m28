// Package core provides the fundamental types and interfaces for the M28 language.
package core

import "fmt"

// SourceLocation represents a position in source code.
// This is used to track where values originated for better error messages.
//
// Example error message with source location:
//
//	error: type mismatch at test.m28:15:22
//	  expected: int
//	  got: string
type SourceLocation struct {
	File    string // Source file path
	Line    int    // Line number (1-indexed)
	Column  int    // Column number (1-indexed)
	EndLine int    // Optional: end line for multi-line spans
	EndCol  int    // Optional: end column for range highlighting
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

// LocatedValue wraps a Value with source location information.
//
// IMPORTANT: LocatedValue is transparent for most operations (Type(), String())
// but will cause type assertions to fail. Always unwrap before type assertions!
//
// Why LocatedValue exists:
//   - Parser creates AST nodes with source locations
//   - AST.ToIR() wraps values in LocatedValue to preserve location info
//   - Evaluator can produce precise error messages pointing to source code
//
// The Problem:
//
//	Type assertions fail when values are wrapped:
//	  sym := value.(SymbolValue)  // PANICS if value is LocatedValue(SymbolValue)
//
// The Solution:
//
//	Always unwrap before type assertions:
//	  unwrapped := unwrapLocated(value)
//	  sym, ok := unwrapped.(SymbolValue)  // Safe!
//
//	Or use smart accessors:
//	  sym, ok := list.GetItemAsSymbol(0)  // Automatically unwraps
//
// See docs/development/error-handling.md for complete guide and examples.
type LocatedValue struct {
	Value    Value           // The actual value being wrapped
	Location *SourceLocation // Where this value came from in source code
}

// Unwrap returns the underlying value, handling nested LocatedValues.
// This recursively unwraps in case values are wrapped multiple times.
//
// Example:
//
//	lv := LocatedValue{Value: SymbolValue("x"), Location: loc}
//	sym := lv.Unwrap()  // Returns SymbolValue("x")
//
// Note: This method is on LocatedValue itself. For unwrapping arbitrary
// values, use the unwrapLocated() function in eval/util.go instead.
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

// Type returns the type of the underlying value.
// LocatedValue is transparent for type checking - it delegates to the wrapped value.
func (lv LocatedValue) Type() Type {
	return lv.Unwrap().Type()
}

// String returns the string representation of the underlying value.
// LocatedValue is transparent for printing - it delegates to the wrapped value.
func (lv LocatedValue) String() string {
	return lv.Unwrap().String()
}

// GetLocation returns the source location of this value.
// This is used by the evaluator when constructing error messages.
func (lv LocatedValue) GetLocation() *SourceLocation {
	return lv.Location
}

// WithLocation wraps a value with source location information.
// This is typically called by the parser when creating AST nodes.
//
// Example:
//
//	sym := WithLocation(SymbolValue("x"), "test.m28", 15, 22)
//	// Later when there's an error involving this symbol:
//	// error: undefined variable 'x' at test.m28:15:22
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

// GetValueLocation extracts source location from a value if it has one.
// Returns nil if the value is not wrapped in LocatedValue.
//
// This is useful for error reporting when you want to include source location
// but the value might or might not be wrapped.
//
// Example:
//
//	if loc := GetValueLocation(value); loc != nil {
//	    return fmt.Errorf("type error at %s: expected int, got %v", loc, value.Type())
//	}
func GetValueLocation(value Value) *SourceLocation {
	if lv, ok := value.(LocatedValue); ok {
		return lv.Location
	}
	return nil
}
