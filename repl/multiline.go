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

// readMultilineInput reads multi-line input until the expression is complete
func (r *REPL) readMultilineInput(firstLine string) (string, error) {
	var lines []string
	lines = append(lines, firstLine)
	
	for isIncomplete(strings.Join(lines, "\n")) {
		// Print continuation prompt
		fmt.Fprint(r.writer, "... ")
		
		line, err := r.reader.ReadString('\n')
		if err != nil {
			return "", err
		}
		
		// Allow user to cancel multi-line input with empty line
		if strings.TrimSpace(line) == "" {
			return "", fmt.Errorf("multi-line input cancelled")
		}
		
		lines = append(lines, strings.TrimRight(line, "\n"))
	}
	
	return strings.Join(lines, "\n"), nil
}