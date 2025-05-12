package core

import (
	"fmt"
	"os"
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

	// Color codes
	cyan := getColorCode("\033[1;36m")
	blue := getColorCode("\033[1;34m")
	white := getColorCode("\033[37m")
	red := getColorCode("\033[1;31m")
	yellow := getColorCode("\033[1;33m")
	bold := getColorCode("\033[1m")
	reset := getColorCode("\033[0m")

	// Add traceback if available
	if e.Traceback != nil && len(e.Traceback) > 0 {
		result.WriteString(fmt.Sprintf("%sTraceback (most recent call last):%s\n", cyan, reset))

		// Format each traceback entry
		for i, entry := range e.Traceback {
			if entry.Location.Filename != "" && entry.Location.Line > 0 {
				// Format with file, line, and function information with highlighting
				result.WriteString(fmt.Sprintf("  %sFile \"%s\", line %d, in %s%s\n",
					blue,
					entry.Location.Filename,
					entry.Location.Line,
					entry.Function,
					reset))

				// Indent the code statement for readability
				if entry.Statement != "" {
					result.WriteString(fmt.Sprintf("    %s%s%s", white, entry.Statement, reset))
				} else {
					result.WriteString(fmt.Sprintf("    %s<unknown code>%s", white, reset))
				}
			} else {
				// Basic format for entries without location info
				result.WriteString(fmt.Sprintf("  %sin %s:%s %s%s%s",
					blue, entry.Function, reset, white, entry.Statement, reset))
			}

			// Add a newline between entries
			if i < len(e.Traceback)-1 {
				result.WriteString("\n")
			}
		}

		// Add a line break before the exception message
		result.WriteString("\n\n")
	}

	// Add exception type and message with highlighting
	result.WriteString(fmt.Sprintf("%s%s:%s %s%s%s", red, e.Type, reset, bold, e.Message, reset))

	// Add cause if available
	if e.Cause != nil {
		result.WriteString(fmt.Sprintf("\n\n%sThe above exception was the direct cause of the following exception:%s\n",
			yellow, reset))
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

// SourceCodeCache maps filenames to source code for better error reporting
var SourceCodeCache = make(map[string][]string)

// RegisterSourceCode stores source code for a file in the cache
func RegisterSourceCode(filename string, sourceCode string) {
	lines := strings.Split(sourceCode, "\n")
	SourceCodeCache[filename] = lines
}

// GetSourceCode attempts to get more context from the source code cache
func GetSourceCode(location Location) (string, bool) {
	if location.Filename == "" || location.Line <= 0 {
		return "", false
	}

	lines, ok := SourceCodeCache[location.Filename]
	if !ok || location.Line > len(lines) {
		return "", false
	}

	// Get the line of code at the error location
	sourceLine := lines[location.Line-1]
	return strings.TrimSpace(sourceLine), true
}

// ColoredErrors determines whether to use colored error output
var ColoredErrors = true

// IsTerminal attempts to determine if the output is a terminal
// where color would be useful
func IsTerminal() bool {
	fileInfo, err := os.Stdout.Stat()
	if err != nil {
		return false
	}
	return (fileInfo.Mode() & os.ModeCharDevice) != 0
}

// DisableColors turns off colored error output
func DisableColors() {
	ColoredErrors = false
}

// EnableColors turns on colored error output
func EnableColors() {
	ColoredErrors = true
}

// getColorCode returns either the ANSI color code or empty string based on settings
func getColorCode(code string) string {
	if ColoredErrors && IsTerminal() {
		return code
	}
	return ""
}

// StandardExceptions defines the standard exception types
var StandardExceptions = map[string]*Exception{}

// Initialize standard exceptions
func init() {
	// Auto-detect whether to use colors by default
	ColoredErrors = IsTerminal()
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
