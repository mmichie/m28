package repl

import (
	"fmt"
	"os"
)

func (r *REPL) ExecuteFile(filename string) error {
	file, err := os.Open(filename)
	if err != nil {
		return fmt.Errorf("error opening file: %v", err)
	}
	defer file.Close()

	// Read the entire file content
	fileContent, err := os.ReadFile(filename)
	if err != nil {
		return fmt.Errorf("error reading file: %v", err)
	}

	// Process the entire file as a single unit to ensure proper parsing
	content := string(fileContent)

	// Let the parser handle the entire file to properly manage nested expressions
	_, err = r.EvaluateString(content)
	if err != nil {
		return fmt.Errorf("error executing file: %v", err)
	}

	return nil
}
