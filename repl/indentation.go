package repl

import (
	"strings"
)

// IndentationTracker tracks and manages indentation for multi-line input
type IndentationTracker struct {
	indentLevel int
	indentSize  int // Number of spaces per indent level
}

// NewIndentationTracker creates a new indentation tracker
func NewIndentationTracker() *IndentationTracker {
	return &IndentationTracker{
		indentLevel: 0,
		indentSize:  4, // Default to 4 spaces
	}
}

// AnalyzeLine analyzes a line and returns the appropriate indentation change
func (it *IndentationTracker) AnalyzeLine(line string) (indentChange int) {
	trimmed := strings.TrimSpace(line)
	if trimmed == "" {
		return 0
	}

	// Keywords that increase indentation
	indentKeywords := []string{
		"def ", "lambda ", "class ",
		"if ", "elif ", "else:",
		"for ", "while ",
		"try:", "except ", "finally:",
		"with ",
	}

	// Keywords that decrease indentation (on the same line)
	dedentKeywords := []string{
		"return", "break", "continue", "pass", "raise",
	}

	// Check for indent keywords
	for _, keyword := range indentKeywords {
		if strings.HasPrefix(trimmed, "("+keyword) || strings.HasPrefix(trimmed, keyword) {
			// Special case for else/elif/except/finally - they dedent first, then indent
			if strings.HasPrefix(trimmed, "elif ") || strings.HasPrefix(trimmed, "else:") ||
				strings.HasPrefix(trimmed, "except ") || strings.HasPrefix(trimmed, "finally:") {
				return 0 // No net change - dedent then indent
			}
			return 1
		}
	}

	// Check for dedent keywords
	for _, keyword := range dedentKeywords {
		if strings.HasPrefix(trimmed, "("+keyword) || strings.HasPrefix(trimmed, keyword) ||
			trimmed == keyword {
			return -1
		}
	}

	// Check if line ends with a colon (likely to start a block)
	if strings.HasSuffix(trimmed, ":") {
		return 1
	}

	return 0
}

// GetIndentation returns the current indentation string
func (it *IndentationTracker) GetIndentation() string {
	return strings.Repeat(" ", it.indentLevel*it.indentSize)
}

// UpdateLevel updates the indentation level based on the line
func (it *IndentationTracker) UpdateLevel(line string) {
	change := it.AnalyzeLine(line)
	it.indentLevel += change
	if it.indentLevel < 0 {
		it.indentLevel = 0
	}
}

// Reset resets the indentation level
func (it *IndentationTracker) Reset() {
	it.indentLevel = 0
}

// DetectDedent checks if a line represents a dedent
func (it *IndentationTracker) DetectDedent(line string) bool {
	// Count leading spaces
	leadingSpaces := 0
	for _, ch := range line {
		if ch == ' ' {
			leadingSpaces++
		} else {
			break
		}
	}

	currentIndent := leadingSpaces / it.indentSize
	return currentIndent < it.indentLevel
}

// SetLevelFromLine sets the indentation level based on leading spaces
func (it *IndentationTracker) SetLevelFromLine(line string) {
	leadingSpaces := 0
	for _, ch := range line {
		if ch == ' ' {
			leadingSpaces++
		} else {
			break
		}
	}

	it.indentLevel = leadingSpaces / it.indentSize
}
