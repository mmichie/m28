package core

import (
	"fmt"
	"os"
	"regexp"
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
		// Format with color codes if enabled
		blue := getColorCode("\033[1;34m")
		white := getColorCode("\033[37m")
		reset := getColorCode("\033[0m")

		return fmt.Sprintf("%sFile \"%s\", line %d, in %s%s%s",
			blue,
			t.Location.Filename,
			t.Location.Line,
			t.Function,
			reset,
			white)
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

		// Add source context if available
		if entry.Location.Filename != "" && entry.Location.Line > 0 {
			if sourceContext, ok := GetSourceContext(entry.Location, 1); ok {
				sb.WriteString("\n")
				contextLines := strings.Split(sourceContext, "\n")
				for _, line := range contextLines {
					if line != "" {
						sb.WriteString("    " + line + "\n")
					}
				}
			}
		}

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

				// Try to get full source context with line numbers and pointer
				if sourceContext, ok := GetSourceContext(entry.Location, 2); ok {
					// Split the context into lines and add proper indentation
					contextLines := strings.Split(sourceContext, "\n")
					for _, line := range contextLines {
						if line != "" {
							result.WriteString(fmt.Sprintf("  %s%s%s\n", white, line, reset))
						}
					}
				} else {
					// Fall back to the simple statement if context not available
					if entry.Statement != "" {
						result.WriteString(fmt.Sprintf("    %s%s%s\n", white, entry.Statement, reset))
					} else {
						result.WriteString(fmt.Sprintf("    %s<unknown code>%s\n", white, reset))
					}
				}
			} else {
				// Basic format for entries without location info
				result.WriteString(fmt.Sprintf("  %sin %s:%s %s%s%s\n",
					blue, entry.Function, reset, white, entry.Statement, reset))
			}

			// Add a newline between entries if not the last one
			if i < len(e.Traceback)-1 {
				result.WriteString("\n")
			}
		}

		// Add a line break before the exception message
		result.WriteString("\n")
	}

	// Add exception type and message with highlighting
	result.WriteString(fmt.Sprintf("%s%s:%s %s%s%s", red, e.Type, reset, bold, e.Message, reset))

	// Add suggestions for common errors
	if suggestion := GetErrorSuggestion(e); suggestion != "" {
		result.WriteString(fmt.Sprintf("\n\n%sSuggestion: %s%s", yellow, suggestion, reset))
	}

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

// GetSourceContext returns a snippet of code around the error location
// with line numbers and an indicator pointing to the exact position
func GetSourceContext(location Location, contextLines int) (string, bool) {
	if location.Filename == "" || location.Line <= 0 {
		return "", false
	}

	lines, ok := SourceCodeCache[location.Filename]
	if !ok || location.Line > len(lines) {
		return "", false
	}

	// Determine the range of lines to show
	startLine := location.Line - contextLines
	if startLine < 1 {
		startLine = 1
	}

	endLine := location.Line + contextLines
	if endLine > len(lines) {
		endLine = len(lines)
	}

	// Build the context with line numbers
	var sb strings.Builder
	lineNoWidth := len(fmt.Sprintf("%d", endLine)) // Width needed for line numbers

	for i := startLine; i <= endLine; i++ {
		linePrefix := fmt.Sprintf("%*d | ", lineNoWidth, i)

		// Highlight the error line
		if i == location.Line {
			// Add the line with an indicator showing the column position
			sb.WriteString(fmt.Sprintf("%s%s\n", linePrefix, lines[i-1]))

			// Add the pointer to the error position
			pointerPos := location.Column - 1
			if pointerPos < 0 {
				pointerPos = 0
			}

			// Create the pointer line with spaces and a caret (^)
			pointerLine := strings.Repeat(" ", lineNoWidth+3) // Spaces for the line number and '| '
			pointerLine += strings.Repeat(" ", pointerPos) + "^"
			sb.WriteString(pointerLine + "\n")
		} else {
			sb.WriteString(fmt.Sprintf("%s%s\n", linePrefix, lines[i-1]))
		}
	}

	return sb.String(), true
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

// GetErrorSuggestion provides helpful suggestions for common errors
func GetErrorSuggestion(e *Exception) string {
	// We'll use the message and exception type to generate suggestions
	message := strings.ToLower(e.Message)

	// Provide type-specific suggestions
	switch e.Type {
	case "NameError":
		if strings.Contains(message, "undefined") || strings.Contains(message, "not defined") {
			// Extract the variable name from the error message
			varName := extractVariableName(message)
			if varName != "" {
				return fmt.Sprintf("The variable '%s' is used before being defined. Check for typos or ensure it's initialized before use.", varName)
			}
			return "Check for typos in variable names or ensure variables are defined before use."
		}

	case "SyntaxError":
		if strings.Contains(message, "unexpected token") || strings.Contains(message, "unexpected end") {
			return "Check for missing or mismatched parentheses, brackets, or quotes."
		}
		if strings.Contains(message, "expected") {
			return "The syntax is incorrect. Review the code structure and ensure it follows the language grammar."
		}

	case "TypeError":
		if strings.Contains(message, "not callable") {
			return "You're trying to call something that is not a function or method."
		}
		if strings.Contains(message, "wrong number of arguments") || strings.Contains(message, "takes") {
			return "Check the function signature and ensure you're passing the correct number of arguments."
		}
		if strings.Contains(message, "cannot be applied to") {
			return "The operation can't be performed on these types of values. Check the types of your operands."
		}

	case "KeyError":
		// Extract the key from the error message
		key := extractKey(message)
		if key != "" {
			return fmt.Sprintf("The key '%s' doesn't exist in the dictionary. Use .get() or check the keys before accessing.", key)
		}
		return "The key doesn't exist in the dictionary. Use .get() or check the keys before accessing."

	case "IndexError":
		if strings.Contains(message, "out of range") || strings.Contains(message, "index") {
			return "The list index is out of range. Check the length of your list and ensure indices are valid."
		}

	case "ValueError":
		if strings.Contains(message, "convert") || strings.Contains(message, "parse") {
			return "The value cannot be converted to the expected type. Check the input format."
		}

	case "ImportError":
		if strings.Contains(message, "no module") || strings.Contains(message, "cannot import") {
			// Extract the module name
			moduleName := extractModuleName(message)
			if moduleName != "" {
				return fmt.Sprintf("Module '%s' couldn't be found. Check the module name and path.", moduleName)
			}
			return "The module couldn't be found. Check the module name and path."
		}

	case "AttributeError":
		// Extract the attribute and object
		attr, obj := extractAttributeAndObject(message)
		if attr != "" && obj != "" {
			return fmt.Sprintf("The object '%s' has no attribute '%s'. Check the object type and available attributes.", obj, attr)
		}
		return "The object doesn't have this attribute. Check the object type and available attributes."

	case "ZeroDivisionError":
		return "Division by zero is not allowed. Add checks to prevent dividing by zero."
	}

	// Generic suggestions based on error message patterns
	if strings.Contains(message, "undefined") || strings.Contains(message, "not defined") {
		return "Check for typos or ensure identifiers are defined before use."
	}

	if strings.Contains(message, "expected") && strings.Contains(message, "got") {
		return "The code expected a different type or format. Check the types of your values."
	}

	// No specific suggestion available
	return ""
}

// Helper functions for error suggestions

// extractVariableName tries to extract variable name from NameError messages
func extractVariableName(message string) string {
	// Common patterns in name error messages
	patterns := []string{
		"variable '([^']+)' is not defined",
		"'([^']+)' is not defined",
		"undefined variable '([^']+)'",
		"undefined: ([^ ]+)",
	}

	for _, pattern := range patterns {
		re := regexp.MustCompile(pattern)
		matches := re.FindStringSubmatch(message)
		if len(matches) > 1 {
			return matches[1]
		}
	}

	return ""
}

// extractKey tries to extract a key from KeyError messages
func extractKey(message string) string {
	// Common patterns in key error messages
	patterns := []string{
		"key '([^']+)' not found",
		"'([^']+)' is not a key",
	}

	for _, pattern := range patterns {
		re := regexp.MustCompile(pattern)
		matches := re.FindStringSubmatch(message)
		if len(matches) > 1 {
			return matches[1]
		}
	}

	return ""
}

// extractModuleName tries to extract module name from ImportError messages
func extractModuleName(message string) string {
	// Common patterns in import error messages
	patterns := []string{
		"no module named '([^']+)'",
		"cannot import module '([^']+)'",
	}

	for _, pattern := range patterns {
		re := regexp.MustCompile(pattern)
		matches := re.FindStringSubmatch(message)
		if len(matches) > 1 {
			return matches[1]
		}
	}

	return ""
}

// extractAttributeAndObject tries to extract attribute and object from AttributeError messages
func extractAttributeAndObject(message string) (string, string) {
	// Common patterns in attribute error messages
	patterns := []string{
		"'([^']+)' object has no attribute '([^']+)'",
		"object '([^']+)' has no attribute '([^']+)'",
	}

	for _, pattern := range patterns {
		re := regexp.MustCompile(pattern)
		matches := re.FindStringSubmatch(message)
		if len(matches) > 2 {
			return matches[2], matches[1]
		}
	}

	return "", ""
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
