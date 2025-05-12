package repl

import (
	"fmt"
	"os"

	"github.com/mmichie/m28/core"
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

	// Register source code in cache for better error reporting
	core.RegisterSourceCode(filename, content)

	// Set the current filename in the parser for better error reporting
	r.parser.SetFilename(filename)

	// Parse the content into expressions
	expr, err := r.parser.Parse(content)
	if err != nil {
		return fmt.Errorf("parse error: %v", err)
	}

	// Check if we got multiple expressions as a list
	if exprList, ok := expr.(core.LispList); ok {
		// Execute each expression in the list
		var lastErr error

		for _, subExpr := range exprList {
			_, lastErr = r.evaluator.Eval(subExpr, r.env)
			if lastErr != nil {
				// Don't wrap the error if it's already an Exception
				if ex, ok := lastErr.(*core.Exception); ok {
					return ex
				}
				return fmt.Errorf("error executing file: %v", lastErr)
			}
		}

		// Return success - ignore the last result
		return nil
	}

	// Single expression case
	_, err = r.evaluator.Eval(expr, r.env)

	// Don't wrap the error again if it's already an Exception
	if err != nil {
		if ex, ok := err.(*core.Exception); ok {
			// Return the exception directly to preserve traceback information
			return ex
		}

		// Only wrap other error types
		return fmt.Errorf("error executing file: %v", err)
	}

	return nil
}
