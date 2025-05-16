package main

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/initialize"
	"github.com/mmichie/m28/repl"
)

func main() {
	// Parse command line flags
	flags := repl.ParseFlags()

	// Initialize the REPL with command flags
	r := repl.NewREPL(flags)

	// Store evaluator in global environment for object method calls
	r.StoreEvaluator()

	// Initialize concurrency features
	initialize.InitializeConcurrency()

	// A flag to determine if we should enter interactive mode
	enterRepl := flags.Interactive

	// Handle the evaluation flag (-e/--eval)
	if flags.EvalCode != "" {
		result, err := r.EvaluateString(flags.EvalCode)
		if err != nil {
			printError(err)
			os.Exit(1)
		}
		fmt.Println(core.PrintValue(result))

		// Exit after evaluation unless interactive mode is requested
		if !enterRepl {
			return
		}
	}

	// Handle the command flag (-c/--command)
	if flags.Command != "" {
		_, err := r.EvaluateString(flags.Command)
		if err != nil {
			printError(err)
			os.Exit(1)
		}

		// Exit after command execution unless interactive mode is requested
		if !enterRepl {
			return
		}
	}

	// Handle file arguments
	for _, filename := range flags.Filenames {
		ext := filepath.Ext(filename)

		if ext == ".lisp" || ext == ".m28" {
			// Execute the file
			err := r.ExecuteFile(filename)
			if err != nil {
				printError(err)
				os.Exit(1)
			}
		} else {
			// Treat as code to evaluate
			result, err := r.EvaluateString(filename)
			if err != nil {
				printError(err)
				os.Exit(1)
			}
			fmt.Println(core.PrintValue(result))
		}
	}

	// Start the REPL if no arguments were provided or interactive mode was requested
	if len(flags.Filenames) == 0 && flags.EvalCode == "" && flags.Command == "" || enterRepl {
		r.Run()
	}
}

// printError formats and prints errors with special handling for exceptions
func printError(err error) {
	if ex, ok := err.(*core.Exception); ok {
		// Print exception with traceback if available
		fmt.Println(ex)
	} else {
		fmt.Println("Error:", err)
	}
}
