package main

import (
	"fmt"
	"log"

	"github.com/mmichie/m28/embed"
)

func main() {
	// Create M28 engine
	engine := embed.NewM28Engine()

	// Example expressions that work
	examples := []string{
		"(+ 1 2 3)",
		"(* 5 6)",
		"(str \"Hello\" \" \" \"World\")",
		"[1, 2, 3, 4, 5]",
		"(map (lambda (x) (* x 2)) [1, 2, 3])",
		"(len \"Hello\")",
		"(if (> 10 5) \"yes\" \"no\")",
	}

	fmt.Println("M28 Embedding - Working Examples")
	fmt.Println("================================")

	for _, expr := range examples {
		result, err := engine.Evaluate(expr)
		if err != nil {
			log.Printf("Error evaluating %s: %v\n", expr, err)
			continue
		}
		fmt.Printf("%s => %v\n", expr, result)
	}

	// Show how to use shell commands
	fmt.Println("\nShell Integration:")
	result, err := engine.Evaluate(`(shell "echo Hello from shell")`)
	if err == nil {
		fmt.Printf("Shell output: %v\n", result)
	}

	// Show environment variables
	result, err = engine.Evaluate(`(getenv "USER")`)
	if err == nil {
		fmt.Printf("Current user: %v\n", result)
	}
}
