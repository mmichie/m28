package repl

import (
	"bufio"
	"fmt"
	"os"
	"strings"

	"github.com/mmichie/m28/core"
)

func (r *REPL) ExecuteFile(filename string) error {
	file, err := os.Open(filename)
	if err != nil {
		return fmt.Errorf("error opening file: %v", err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var input strings.Builder
	lineNumber := 0

	for scanner.Scan() {
		lineNumber++
		line := strings.TrimSpace(scanner.Text())

		if line == "" || strings.HasPrefix(line, ";") {
			// Skip empty lines and comments
			continue
		}

		input.WriteString(line)
		input.WriteString("\n")

		if strings.Count(input.String(), "(") == strings.Count(input.String(), ")") {
			expr := strings.TrimSpace(input.String())
			if expr != "" {
				result, err := r.EvaluateString(expr)
				if err != nil {
					return fmt.Errorf("error evaluating expression at line %d: %v", lineNumber, err)
				}
				if result != nil {
					fmt.Printf("Result: %v\n", core.PrintValue(result))
				}
			}
			input.Reset()
		}
	}

	if err := scanner.Err(); err != nil {
		return fmt.Errorf("error reading file: %v", err)
	}

	return nil
}
