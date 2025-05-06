package core

import (
	"fmt"
	"strings"
)

// Location represents a source code location for error reporting
type Location struct {
	Filename string
	Line     int
	Column   int
}

// String returns a string representation of the location
func (l Location) String() string {
	if l.Filename == "" {
		return "<unknown location>"
	}
	return fmt.Sprintf("%s:%d:%d", l.Filename, l.Line, l.Column)
}

// TraceEntry represents a single entry in a traceback
type TraceEntry struct {
	Location  Location
	Function  string
	Statement string
}

// String returns a string representation of the trace entry
func (t TraceEntry) String() string {
	if t.Location.Filename != "" && t.Location.Line > 0 {
		return fmt.Sprintf("File \"%s\", line %d, in %s\n  %s",
			t.Location.Filename,
			t.Location.Line,
			t.Function,
			t.Statement)
	}
	return fmt.Sprintf("in %s: %s", t.Function, t.Statement)
}

// Traceback represents a stack trace for an exception
type Traceback []TraceEntry

// String returns a string representation of the traceback
func (t Traceback) String() string {
	var sb strings.Builder
	sb.WriteString("Traceback (most recent call last):")

	for i, entry := range t {
		sb.WriteString("\n  ")
		sb.WriteString(entry.String())
		if i < len(t)-1 {
			sb.WriteString("\n")
		}
	}

	return sb.String()
}

// Exception represents a Python-style exception
type Exception struct {
	Type      string     // Exception type name
	Message   string     // Error message
	Traceback Traceback  // Stack trace
	Cause     *Exception // Original exception for wrapped exceptions
	Parent    *Exception // Parent class for exception hierarchy
}

// Error makes Exception implement the error interface
func (e *Exception) Error() string {
	return fmt.Sprintf("%s: %s", e.Type, e.Message)
}

// String returns a detailed string representation of the exception
func (e *Exception) String() string {
	var result strings.Builder

	// Add traceback if available
	if e.Traceback != nil && len(e.Traceback) > 0 {
		result.WriteString("Traceback (most recent call last):\n")

		// Format each traceback entry
		for i, entry := range e.Traceback {
			var entryStr string
			if entry.Location.Filename != "" && entry.Location.Line > 0 {
				entryStr = fmt.Sprintf("  File \"%s\", line %d, in %s\n    %s",
					entry.Location.Filename,
					entry.Location.Line,
					entry.Function,
					entry.Statement)
			} else {
				entryStr = fmt.Sprintf("  in %s: %s", entry.Function, entry.Statement)
			}
			result.WriteString(entryStr)

			// Add a newline for each entry except the last one
			if i < len(e.Traceback)-1 {
				result.WriteString("\n")
			}
		}

		// Add a line break before the exception message
		result.WriteString("\n")
	}

	// Add exception type and message
	result.WriteString(fmt.Sprintf("%s: %s", e.Type, e.Message))

	// Add cause if available
	if e.Cause != nil {
		result.WriteString("\n\nThe above exception was the direct cause of the following exception:\n")
		result.WriteString(e.Cause.String())
	}

	return result.String()
}

// AddTraceEntry adds a trace entry to the exception's traceback
func (e *Exception) AddTraceEntry(entry TraceEntry) {
	e.Traceback = append(e.Traceback, entry)
}

// IsSubclassOf checks if this exception is a subclass of the given type
func (e *Exception) IsSubclassOf(typeName string) bool {
	if e.Type == typeName {
		return true
	}

	if e.Parent != nil {
		return e.Parent.IsSubclassOf(typeName)
	}

	return false
}

// NewException creates a new exception of the given type
func NewException(typeName, message string) *Exception {
	// Get the parent exception if it's a standard type
	var parent *Exception
	if typeName != "Exception" {
		parent = StandardExceptions["Exception"]
	}

	// Create the exception
	exception := &Exception{
		Type:    typeName,
		Message: message,
		Parent:  parent,
	}

	return exception
}

// StandardExceptions defines the standard exception types
var StandardExceptions = map[string]*Exception{}

// Initialize standard exceptions
func init() {
	// Base exception
	baseException := &Exception{
		Type:    "Exception",
		Message: "",
	}
	StandardExceptions["Exception"] = baseException

	// Define exception hierarchy
	defineException("ValueError", "Exception")
	defineException("TypeError", "Exception")
	defineException("IndexError", "Exception")
	defineException("KeyError", "Exception")
	defineException("NameError", "Exception")
	defineException("ZeroDivisionError", "Exception")
	defineException("AssertionError", "Exception")
	defineException("RuntimeError", "Exception")
	defineException("IOError", "Exception")
	defineException("FileNotFoundError", "IOError")
	defineException("SyntaxError", "Exception")
	defineException("AttributeError", "Exception")
	defineException("ImportError", "Exception")
	defineException("StopIteration", "Exception")
}

// defineException creates a new exception type with the given parent
func defineException(typeName, parentName string) {
	parent, exists := StandardExceptions[parentName]
	if !exists {
		// If parent doesn't exist, use base Exception
		parent = StandardExceptions["Exception"]
	}

	exception := &Exception{
		Type:    typeName,
		Message: "",
		Parent:  parent,
	}

	StandardExceptions[typeName] = exception
}

// DefineCustomException creates a new custom exception type
func DefineCustomException(typeName, parentName string) *Exception {
	parent, exists := StandardExceptions[parentName]
	if !exists {
		// If parent doesn't exist, use base Exception
		parent = StandardExceptions["Exception"]
	}

	exception := &Exception{
		Type:    typeName,
		Message: "",
		Parent:  parent,
	}

	StandardExceptions[typeName] = exception
	return exception
}
