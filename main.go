package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/repl"
)

func main() {
	r := repl.NewREPL()

	// Store evaluator in global environment for object method calls
	r.StoreEvaluator()

	if len(os.Args) > 1 {
		filename := os.Args[1]
		ext := filepath.Ext(filename)

		if ext == ".lisp" || ext == ".m28" {
			// Execute the file
			err := r.ExecuteFile(filename)
			if err != nil {
				// If it's an exception, print it properly - it may have traceback info
				if ex, ok := err.(*core.Exception); ok {
					fmt.Println(ex)
				} else {
					fmt.Println("Error executing file:", err)
				}
				os.Exit(1)
			}
		} else {
			// Evaluate the arguments as a single expression
			input := strings.Join(os.Args[1:], " ")
			result, err := r.EvaluateString(input)
			if err != nil {
				fmt.Println("Error:", err)
				os.Exit(1)
			}
			fmt.Println(result)
		}
	} else {
		// If no arguments are provided, start the REPL
		r.Run()
	}
}
