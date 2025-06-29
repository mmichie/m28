package main

import (
	"fmt"
	"log"

	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/embed"
)

func main() {
	// Create a new M28 engine
	engine := embed.NewM28Engine()

	// Example 1: Simple arithmetic
	result, err := engine.Evaluate("(+ 1 2 3 4)")
	if err != nil {
		log.Fatal(err)
	}
	fmt.Printf("1 + 2 + 3 + 4 = %v\n", result)

	// Example 2: Define a variable
	engine.DefineValue("pi", core.NumberValue(3.14159))

	// Example 3: Use the variable in calculations
	result, err = engine.Evaluate("(* pi 2)")
	if err != nil {
		log.Fatal(err)
	}
	fmt.Printf("pi * 2 = %v\n", result)

	// Example 4: Define a custom Go function
	engine.DefineFunction("greet", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("greet expects 1 argument, got %d", len(args))
		}

		name, ok := args[0].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("greet expects string argument")
		}

		greeting := fmt.Sprintf("Hello, %s! Welcome to M28.", string(name))
		return core.StringValue(greeting), nil
	})

	// Example 5: Call the custom function
	result, err = engine.Evaluate(`(greet "Developer")`)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Printf("Greeting: %v\n", result)

	// Example 6: Use built-in functions
	result, err = engine.Evaluate(`(map (lambda (x) (* x x)) [1, 2, 3, 4, 5])`)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Printf("Squares: %v\n", result)

	// Example 7: Execute shell commands (if needed)
	result, err = engine.Evaluate(`(shell "echo 'Hello from shell'")`)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Printf("Shell output: %v\n", result)

	// Example 8: Work with data structures
	// Create a dictionary using the dict function
	result, err = engine.Evaluate(`(dict "name" "Alice" "age" 30 "skills" ["Go", "Python", "M28"])`)
	if err != nil {
		log.Fatal("Dict creation failed:", err)
	}
	fmt.Printf("Dict: %v\n", result)

	// Store dict and access it
	engine.DefineValue("person", result)
	result, err = engine.Evaluate(`(get person "skills")`)
	if err != nil {
		log.Fatal("Get failed:", err)
	}
	fmt.Printf("Skills: %v\n", result)
}
