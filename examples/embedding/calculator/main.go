package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"

	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/embed"
)

// A simple calculator application using embedded M28
func main() {
	engine := embed.NewM28Engine()

	// Add some mathematical constants
	engine.DefineValue("pi", core.NumberValue(3.14159265359))
	engine.DefineValue("e", core.NumberValue(2.71828182846))
	engine.DefineValue("phi", core.NumberValue(1.61803398875)) // Golden ratio

	// Add some useful functions
	engine.DefineFunction("deg2rad", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("deg2rad expects 1 argument")
		}
		deg, ok := args[0].(core.NumberValue)
		if !ok {
			return nil, fmt.Errorf("deg2rad expects number argument")
		}
		rad := float64(deg) * 3.14159265359 / 180.0
		return core.NumberValue(rad), nil
	})

	fmt.Println("M28 Calculator")
	fmt.Println("Type expressions like: (+ 1 2), (* pi 2), (sqrt 16)")
	fmt.Println("Constants available: pi, e, phi")
	fmt.Println("Type 'quit' to exit")
	fmt.Println()

	scanner := bufio.NewScanner(os.Stdin)
	for {
		fmt.Print("calc> ")
		if !scanner.Scan() {
			if err := scanner.Err(); err != nil {
				fmt.Printf("Scanner error: %v\n", err)
			}
			break
		}

		input := strings.TrimSpace(scanner.Text())
		if input == "quit" || input == "exit" {
			break
		}

		if input == "" {
			continue
		}

		result, err := engine.Evaluate(input)
		if err != nil {
			fmt.Printf("Error: %v\n", err)
		} else {
			fmt.Printf("= %v\n", result)
		}
	}

	fmt.Println("Goodbye!")
}
