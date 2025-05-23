package core

import (
	"fmt"
	"strings"
)

// EvalError represents an error that occurred during evaluation
type EvalError struct {
	Message    string
	StackTrace []TraceEntry
	Wrapped    error
}

// Error implements the error interface
func (e *EvalError) Error() string {
	var sb strings.Builder

	// Main error message
	sb.WriteString(e.Message)

	// Add wrapped error if present
	if e.Wrapped != nil {
		sb.WriteString(": ")
		sb.WriteString(e.Wrapped.Error())
	}

	// Add stack trace if present
	if len(e.StackTrace) > 0 {
		sb.WriteString("\n\nStack trace (most recent call first):\n")
		for i, entry := range e.StackTrace {
			sb.WriteString(fmt.Sprintf("  %d. %s", i+1, entry.String()))
			if entry.File != "" {
				sb.WriteString(fmt.Sprintf(" at %s:%d:%d", entry.File, entry.Line, entry.Column))
			}
			sb.WriteString("\n")
		}
	}

	return sb.String()
}

// String returns a string representation of a trace entry
func (t TraceEntry) String() string {
	if t.Function != "" {
		return t.Function
	}
	return "<anonymous>"
}

// NewEvalError creates a new evaluation error
func NewEvalError(message string, ctx *Context) *EvalError {
	return &EvalError{
		Message:    message,
		StackTrace: copyStackTrace(ctx.CallStack),
	}
}

// WrapEvalError wraps an existing error with evaluation context
func WrapEvalError(err error, message string, ctx *Context) *EvalError {
	if evalErr, ok := err.(*EvalError); ok {
		// Don't double-wrap eval errors
		return evalErr
	}

	return &EvalError{
		Message:    message,
		StackTrace: copyStackTrace(ctx.CallStack),
		Wrapped:    err,
	}
}

// copyStackTrace makes a copy of the stack trace
func copyStackTrace(trace []TraceEntry) []TraceEntry {
	if len(trace) == 0 {
		return nil
	}

	result := make([]TraceEntry, len(trace))
	copy(result, trace)
	return result
}

// TypeError represents a type mismatch error
type TypeError struct {
	Expected string
	Got      string
	Context  string
}

func (e *TypeError) Error() string {
	if e.Context != "" {
		return fmt.Sprintf("%s: expected %s, got %s", e.Context, e.Expected, e.Got)
	}
	return fmt.Sprintf("expected %s, got %s", e.Expected, e.Got)
}

// NewTypeError creates a new type error
func NewTypeError(expected string, got Value, context string) *TypeError {
	gotType := string(got.Type())

	// Use TypeDescriptor for better type names
	if desc := GetTypeDescriptorForValue(got); desc != nil {
		gotType = desc.PythonName
	}

	return &TypeError{
		Expected: expected,
		Got:      gotType,
		Context:  context,
	}
}

// ArgumentError represents an error with function arguments
type ArgumentError struct {
	Function string
	Expected int
	Got      int
	Message  string
}

func (e *ArgumentError) Error() string {
	if e.Message != "" {
		return e.Message
	}

	if e.Expected >= 0 {
		return fmt.Sprintf("%s() takes %d arguments, got %d", e.Function, e.Expected, e.Got)
	}

	return fmt.Sprintf("invalid arguments to %s()", e.Function)
}

// AttributeError represents an attribute access error
type AttributeError struct {
	Object    Value
	Attribute string
}

func (e *AttributeError) Error() string {
	typeName := "object"

	if desc := GetTypeDescriptorForValue(e.Object); desc != nil {
		typeName = desc.PythonName
	}

	return fmt.Sprintf("'%s' object has no attribute '%s'", typeName, e.Attribute)
}

// NameError represents an undefined variable error
type NameError struct {
	Name string
}

func (e *NameError) Error() string {
	return fmt.Sprintf("name '%s' is not defined", e.Name)
}

// ZeroDivisionError represents division by zero
type ZeroDivisionError struct{}

func (e *ZeroDivisionError) Error() string {
	return "division by zero"
}

// IndexError represents an index out of bounds error
type IndexError struct {
	Index  int
	Length int
}

func (e *IndexError) Error() string {
	return fmt.Sprintf("list index out of range (index %d, length %d)", e.Index, e.Length)
}

// KeyError represents a missing dictionary key
type KeyError struct {
	Key Value
}

func (e *KeyError) Error() string {
	return fmt.Sprintf("key not found: %s", PrintValue(e.Key))
}
