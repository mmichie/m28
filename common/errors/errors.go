// Package errors provides standardized error types for the M28 language.
// These error types follow Python's error hierarchy while maintaining Go idioms.
package errors

import (
	"fmt"
	"strings"

	"github.com/mmichie/m28/core"
)

// ErrorType represents the category of error, similar to Python's exception types.
type ErrorType string

// Standard error types matching Python's exception hierarchy
const (
	TypeError         ErrorType = "TypeError"
	ValueError        ErrorType = "ValueError"
	ArgumentError     ErrorType = "ArgumentError"
	AttributeError    ErrorType = "AttributeError"
	KeyError          ErrorType = "KeyError"
	IndexError        ErrorType = "IndexError"
	RuntimeError      ErrorType = "RuntimeError"
	NameError         ErrorType = "NameError"
	SyntaxError       ErrorType = "SyntaxError"
	ZeroDivisionError ErrorType = "ZeroDivisionError"
)

// M28Error is the base error type for all M28 errors.
// It provides structured error information while implementing the standard error interface.
type M28Error struct {
	Type       ErrorType
	Function   string                 // The function where the error occurred
	Message    string                 // The error message
	Details    map[string]interface{} // Additional context
	Location   *core.SourceLocation   // Source location where error occurred
	SyntaxKind int                    // Which frontend syntax (ast.SyntaxKind)
}

// Error implements the error interface
func (e *M28Error) Error() string {
	var b strings.Builder

	// Format: "TypeError: function_name: message"
	b.WriteString(string(e.Type))
	b.WriteString(": ")

	if e.Function != "" {
		b.WriteString(e.Function)
		b.WriteString(": ")
	}

	b.WriteString(e.Message)

	// Add location if available
	if e.Location != nil {
		b.WriteString(" at ")
		b.WriteString(e.Location.String())
	}

	return b.String()
}

// Is allows error comparison using errors.Is
func (e *M28Error) Is(target error) bool {
	if target == nil {
		return false
	}

	t, ok := target.(*M28Error)
	if !ok {
		return false
	}

	// Compare by error type
	return e.Type == t.Type
}

// WithDetail adds additional context to the error
func (e *M28Error) WithDetail(key string, value interface{}) *M28Error {
	if e.Details == nil {
		e.Details = make(map[string]interface{})
	}
	e.Details[key] = value
	return e
}

// WithLocation adds source location to the error
func (e *M28Error) WithLocation(loc *core.SourceLocation) *M28Error {
	e.Location = loc
	return e
}

// WithSyntaxKind adds syntax kind to the error
func (e *M28Error) WithSyntaxKind(kind int) *M28Error {
	e.SyntaxKind = kind
	return e
}

// Common error constructors for consistency

// NewTypeError creates a new TypeError
func NewTypeError(function string, expected, got string) *M28Error {
	return &M28Error{
		Type:     TypeError,
		Function: function,
		Message:  fmt.Sprintf("expected %s, got %s", expected, got),
	}
}

// NewTypeErrorf creates a new TypeError with formatted message
func NewTypeErrorf(function string, format string, args ...interface{}) *M28Error {
	return &M28Error{
		Type:     TypeError,
		Function: function,
		Message:  fmt.Sprintf(format, args...),
	}
}

// NewValueError creates a new ValueError
func NewValueError(function string, message string) *M28Error {
	return &M28Error{
		Type:     ValueError,
		Function: function,
		Message:  message,
	}
}

// NewValueErrorf creates a new ValueError with formatted message
func NewValueErrorf(function string, format string, args ...interface{}) *M28Error {
	return &M28Error{
		Type:     ValueError,
		Function: function,
		Message:  fmt.Sprintf(format, args...),
	}
}

// NewArgumentError creates a new ArgumentError for incorrect argument counts
func NewArgumentError(function string, expected, got int) *M28Error {
	var message string
	if expected == 1 {
		message = fmt.Sprintf("takes exactly 1 argument (%d given)", got)
	} else {
		message = fmt.Sprintf("takes exactly %d arguments (%d given)", expected, got)
	}

	return &M28Error{
		Type:     ArgumentError,
		Function: function,
		Message:  message,
		Details: map[string]interface{}{
			"expected": expected,
			"got":      got,
		},
	}
}

// NewArgumentRangeError creates an error for argument count within a range
func NewArgumentRangeError(function string, min, max, got int) *M28Error {
	var message string
	if min == max {
		return NewArgumentError(function, min, got)
	}

	message = fmt.Sprintf("takes %d to %d arguments (%d given)", min, max, got)

	return &M28Error{
		Type:     ArgumentError,
		Function: function,
		Message:  message,
		Details: map[string]interface{}{
			"min": min,
			"max": max,
			"got": got,
		},
	}
}

// NewAttributeError creates a new AttributeError
func NewAttributeError(typeName, attribute string) *M28Error {
	return &M28Error{
		Type:    AttributeError,
		Message: fmt.Sprintf("'%s' object has no attribute '%s'", typeName, attribute),
		Details: map[string]interface{}{
			"type":      typeName,
			"attribute": attribute,
		},
	}
}

// NewKeyError creates a new KeyError
func NewKeyError(key string) *M28Error {
	return &M28Error{
		Type:    KeyError,
		Message: fmt.Sprintf("key not found: %s", key),
		Details: map[string]interface{}{
			"key": key,
		},
	}
}

// NewIndexError creates a new IndexError
func NewIndexError(index int, length int) *M28Error {
	return &M28Error{
		Type:    IndexError,
		Message: fmt.Sprintf("index %d out of range (length %d)", index, length),
		Details: map[string]interface{}{
			"index":  index,
			"length": length,
		},
	}
}

// NewRuntimeError creates a new RuntimeError
func NewRuntimeError(function, message string) *M28Error {
	return &M28Error{
		Type:     RuntimeError,
		Function: function,
		Message:  message,
	}
}

// NewNameError creates a new NameError
func NewNameError(name string) *M28Error {
	return &M28Error{
		Type:    NameError,
		Message: fmt.Sprintf("name '%s' is not defined", name),
		Details: map[string]interface{}{
			"name": name,
		},
	}
}

// NewZeroDivisionError creates a new ZeroDivisionError
func NewZeroDivisionError(function string) *M28Error {
	return &M28Error{
		Type:     ZeroDivisionError,
		Function: function,
		Message:  "division by zero",
	}
}

// Wrap wraps an existing error with M28 error context
func Wrap(err error, errorType ErrorType, function string) *M28Error {
	if err == nil {
		return nil
	}

	// If it's already an M28Error, preserve the original type
	if m28err, ok := err.(*M28Error); ok {
		m28err.Function = function
		return m28err
	}

	return &M28Error{
		Type:     errorType,
		Function: function,
		Message:  err.Error(),
	}
}
