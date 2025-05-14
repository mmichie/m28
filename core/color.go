package core

import (
	"fmt"
	"os"
	"strings"
	"unicode"
)

// Color constants for terminal output
const (
	ColorReset  = "\033[0m"
	ColorRed    = "\033[1;31m"
	ColorGreen  = "\033[1;32m"
	ColorYellow = "\033[1;33m"
	ColorBlue   = "\033[1;34m"
	ColorPurple = "\033[1;35m"
	ColorCyan   = "\033[1;36m"
	ColorWhite  = "\033[37m"
	ColorGray   = "\033[2;37m"
	ColorBold   = "\033[1m"

	// Bright colors
	ColorBrightRed    = "\033[91m"
	ColorBrightGreen  = "\033[92m"
	ColorBrightYellow = "\033[93m"
	ColorBrightBlue   = "\033[94m"
	ColorBrightPurple = "\033[95m"
	ColorBrightCyan   = "\033[96m"
	ColorBrightWhite  = "\033[97m"
)

// ColoredPrompt is the default prompt with color
// This will be updated dynamically in the REPL with the current execution count
var ColoredPrompt = fmt.Sprintf("%sm28>%s ", ColorBlue, ColorReset)

// ColorEnabled determines whether to use colored output
var ColorEnabled = true

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

// DisableColors turns off colored output
func DisableColors() {
	ColorEnabled = false
	ColoredErrors = false
}

// EnableColors turns on colored output
func EnableColors() {
	ColorEnabled = true
	ColoredErrors = true
}

// GetColorCode returns either the ANSI color code or empty string based on settings
func GetColorCode(code string) string {
	if ColorEnabled && IsTerminal() {
		return code
	}
	return ""
}

// getColorCode is an alias for GetColorCode to maintain compatibility with existing code
func getColorCode(code string) string {
	return GetColorCode(code)
}

// ColorizeValue returns a colorized string representation of a value
func ColorizeValue(val LispValue) string {
	switch v := val.(type) {
	case float64:
		return fmt.Sprintf("%s%g%s", GetColorCode(ColorCyan), v, GetColorCode(ColorReset))
	case string:
		return fmt.Sprintf("%s%q%s", GetColorCode(ColorGreen), v, GetColorCode(ColorReset))
	case PythonicBool:
		if v {
			return fmt.Sprintf("%sTrue%s", GetColorCode(ColorYellow), GetColorCode(ColorReset))
		}
		return fmt.Sprintf("%sFalse%s", GetColorCode(ColorYellow), GetColorCode(ColorReset))
	case PythonicNone:
		return fmt.Sprintf("%sNone%s", GetColorCode(ColorYellow), GetColorCode(ColorReset))
	case *Lambda, BuiltinFunc:
		return fmt.Sprintf("%s#<function>%s", GetColorCode(ColorBlue), GetColorCode(ColorReset))
	default:
		return PrintValue(val)
	}
}

// Initialize color settings
func init() {
	// Auto-detect whether to use colors by default
	ColorEnabled = IsTerminal()
	ColoredErrors = ColorEnabled

	// Initialize the colored prompt (will be updated by REPL)
	ColoredPrompt = ColorBrightPurple + "m28> " + ColorReset
}

// FormatIPythonPrompt returns an IPython-style prompt with execution count
func FormatIPythonPrompt(executionCount int) string {
	countStr := fmt.Sprintf("%d", executionCount)
	return fmt.Sprintf("%sIn [%s%s%s]:%s ",
		GetColorCode(ColorGreen),
		GetColorCode(ColorBrightCyan),
		countStr,
		GetColorCode(ColorGreen),
		GetColorCode(ColorReset))
}

// FormatIPythonContinuationPrompt returns a prompt for continued lines
func FormatIPythonContinuationPrompt(spaces int) string {
	padding := strings.Repeat(" ", spaces)
	return fmt.Sprintf("%s...:%s ",
		GetColorCode(ColorGreen),
		GetColorCode(ColorReset)) + padding
}

// SyntaxHighlight applies syntax highlighting to Lisp code
func SyntaxHighlight(code string) string {
	if !ColorEnabled {
		return code
	}

	// Common keywords
	keywords := map[string]bool{
		"def": true, "fn": true, "if": true, "else": true, "elif": true,
		"for": true, "while": true, "try": true, "except": true, "finally": true,
		"class": true, "import": true, "from": true, "as": true,
		"return": true, "break": true, "continue": true, "pass": true,
		"and": true, "or": true, "not": true, "in": true, "is": true,
		"None": true, "True": true, "False": true, "lambda": true,
		"with": true, "yield": true, "assert": true, "raise": true,
		"del": true, "global": true, "local": true, "nonlocal": true,
	}

	// Simple recursive descent parser
	var highlight func(string, int) (string, int)
	highlight = func(s string, i int) (string, int) {
		if i >= len(s) {
			return "", i
		}

		// Skip whitespace
		if unicode.IsSpace(rune(s[i])) {
			return string(s[i]), i + 1
		}

		// Comment
		if s[i] == ';' {
			j := i
			for j < len(s) && s[j] != '\n' {
				j++
			}
			return GetColorCode(ColorGreen) + s[i:j] + GetColorCode(ColorReset), j
		}

		// String
		if s[i] == '"' {
			j := i + 1
			for j < len(s) && s[j] != '"' {
				if s[j] == '\\' && j+1 < len(s) {
					j += 2 // Skip escaped character
				} else {
					j++
				}
			}
			if j < len(s) {
				j++ // Include closing quote
			}
			return GetColorCode(ColorGreen) + s[i:j] + GetColorCode(ColorReset), j
		}

		// Number
		if unicode.IsDigit(rune(s[i])) || (s[i] == '-' && i+1 < len(s) && unicode.IsDigit(rune(s[i+1]))) {
			j := i
			if s[j] == '-' {
				j++
			}
			for j < len(s) && (unicode.IsDigit(rune(s[j])) || s[j] == '.') {
				j++
			}
			return GetColorCode(ColorBlue) + s[i:j] + GetColorCode(ColorReset), j
		}

		// Symbol or keyword
		if unicode.IsLetter(rune(s[i])) || s[i] == '_' {
			j := i
			for j < len(s) && (unicode.IsLetter(rune(s[j])) || unicode.IsDigit(rune(s[j])) || s[j] == '_' || s[j] == '-') {
				j++
			}
			word := s[i:j]
			if keywords[word] {
				return GetColorCode(ColorPurple) + word + GetColorCode(ColorReset), j
			}
			return GetColorCode(ColorCyan) + word + GetColorCode(ColorReset), j
		}

		// Delimiters
		if s[i] == '(' || s[i] == ')' || s[i] == '[' || s[i] == ']' || s[i] == '{' || s[i] == '}' {
			return GetColorCode(ColorYellow) + string(s[i]) + GetColorCode(ColorReset), i + 1
		}

		// Default
		return string(s[i]), i + 1
	}

	var result strings.Builder
	i := 0
	for i < len(code) {
		var fragment string
		fragment, i = highlight(code, i)
		result.WriteString(fragment)
	}

	return result.String()
}

// FormatIPythonOutput returns an IPython-style output prefix with execution count
func FormatIPythonOutput(executionCount int) string {
	countStr := fmt.Sprintf("%d", executionCount)
	return fmt.Sprintf("%sOut[%s%s%s]:%s ",
		GetColorCode(ColorRed),
		GetColorCode(ColorBrightCyan),
		countStr,
		GetColorCode(ColorRed),
		GetColorCode(ColorReset))
}
