package repl

import (
	"fmt"
	"strings"
)

// isIncomplete checks if an expression is incomplete and needs more input
func isIncomplete(input string) bool {
	// Count parentheses
	parenCount := 0
	bracketCount := 0
	braceCount := 0
	inString := false
	escape := false

	for i, ch := range input {
		if escape {
			escape = false
			continue
		}

		if ch == '\\' {
			escape = true
			continue
		}

		if ch == '"' && (i == 0 || input[i-1] != '\\') {
			inString = !inString
			continue
		}

		if inString {
			continue
		}

		switch ch {
		case '(':
			parenCount++
		case ')':
			parenCount--
		case '[':
			bracketCount++
		case ']':
			bracketCount--
		case '{':
			braceCount++
		case '}':
			braceCount--
		}
	}

	// Expression is incomplete if any brackets are unmatched
	return parenCount > 0 || bracketCount > 0 || braceCount > 0 || inString
}

// isPythonIncomplete checks if Python code is incomplete and needs more input
func isPythonIncomplete(input string) bool {
	// First check for unmatched brackets (reuse existing logic)
	if isIncomplete(input) {
		return true
	}

	lines := strings.Split(input, "\n")
	if len(lines) == 0 {
		return false
	}

	// Check if the last non-empty line ends with ':'
	// This indicates a block start (if, for, while, def, class, try, except, etc.)
	for i := len(lines) - 1; i >= 0; i-- {
		trimmed := strings.TrimSpace(lines[i])
		if trimmed != "" {
			// Check for colon at the end (ignoring comments)
			if idx := strings.Index(trimmed, "#"); idx >= 0 {
				trimmed = trimmed[:idx]
				trimmed = strings.TrimSpace(trimmed)
			}
			if strings.HasSuffix(trimmed, ":") {
				return true
			}
			break
		}
	}

	// Check if we're inside an indented block
	// If the last non-empty line is indented, we might need more input
	if len(lines) > 1 {
		lastNonEmpty := ""
		for i := len(lines) - 1; i >= 0; i-- {
			if strings.TrimSpace(lines[i]) != "" {
				lastNonEmpty = lines[i]
				break
			}
		}

		// If the last line is empty and the previous non-empty line was indented,
		// we're done with the block
		if lastNonEmpty != "" && len(lines[len(lines)-1]) == 0 {
			return false
		}
	}

	// Check for line continuation with backslash
	lastLine := lines[len(lines)-1]
	trimmed := strings.TrimRight(lastLine, " \t")
	if strings.HasSuffix(trimmed, "\\") {
		return true
	}

	return false
}

// readMultilineInput reads multi-line input until the expression is complete
func (r *REPL) readMultilineInput(firstLine string) (string, error) {
	var lines []string
	lines = append(lines, firstLine)

	// Initialize indentation tracker for this multi-line input
	r.indentTracker.Reset()
	r.indentTracker.UpdateLevel(firstLine)

	// Use appropriate incomplete checker based on mode
	incompleteChecker := isIncomplete
	if r.pythonMode {
		incompleteChecker = isPythonIncomplete
	}

	for incompleteChecker(strings.Join(lines, "\n")) {
		// Print continuation prompt
		fmt.Fprint(r.writer, r.executionState.FormatContinuationPrompt())

		// Add smart indentation
		indentation := r.indentTracker.GetIndentation()
		fmt.Fprint(r.writer, indentation)

		line, err := r.reader.ReadString('\n')
		if err != nil {
			return "", err
		}

		// Allow user to cancel multi-line input with empty line at base indentation
		trimmed := strings.TrimSpace(line)
		if trimmed == "" && r.indentTracker.indentLevel == 0 {
			return "", fmt.Errorf("multi-line input cancelled")
		}

		// If user provided their own indentation, respect it
		if len(line) > 0 && line[0] == ' ' {
			r.indentTracker.SetLevelFromLine(line)
			line = strings.TrimRight(line, "\n")
		} else if trimmed != "" {
			// Add our indentation to non-empty lines
			line = indentation + strings.TrimRight(line, "\n")
		} else {
			line = strings.TrimRight(line, "\n")
		}

		lines = append(lines, line)

		// Update indentation level for next line
		if trimmed != "" {
			r.indentTracker.UpdateLevel(trimmed)
		}
	}

	r.indentTracker.Reset()
	return strings.Join(lines, "\n"), nil
}
