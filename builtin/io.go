package builtin

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// RegisterIOFunctions registers I/O related functions
func RegisterIOFunctions(ctx *core.Context) {
	// File operations
	ctx.Define("open", core.NewBuiltinFunction(openFunc))

	// Print function (already exists but let's ensure it handles multiple args)
	ctx.Define("print", core.NewBuiltinFunction(printFunc))

	// Input function
	ctx.Define("input", core.NewBuiltinFunction(inputFunc))
}

// openFunc implements the open() builtin
func openFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 || len(args) > 3 {
		return nil, fmt.Errorf("open() takes 1 to 3 arguments")
	}

	// Get filename
	filename, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("open() filename must be a string")
	}

	// Get mode (default to "r")
	mode := "r"
	if len(args) > 1 {
		if m, ok := args[1].(core.StringValue); ok {
			mode = string(m)
		} else {
			return nil, fmt.Errorf("open() mode must be a string")
		}
	}

	// TODO: Handle encoding parameter (args[2]) if provided

	// Create and return file object
	file, err := core.NewFile(string(filename), mode)
	if err != nil {
		return nil, err
	}

	return file, nil
}

// printFunc implements the print() builtin
func printFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	// Print all arguments separated by spaces
	for i, arg := range args {
		if i > 0 {
			fmt.Print(" ")
		}
		fmt.Print(core.PrintValueWithoutQuotes(arg))
	}
	fmt.Println()

	return core.Nil, nil
}

// inputFunc implements the input() builtin
func inputFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	// Print prompt if provided
	if len(args) > 0 {
		fmt.Print(core.PrintValue(args[0]))
	}

	// Read line from stdin
	var line string
	_, err := fmt.Scanln(&line)
	if err != nil {
		// Handle empty input (just pressing enter)
		if err.Error() == "unexpected newline" {
			return core.StringValue(""), nil
		}
		return nil, fmt.Errorf("error reading input: %v", err)
	}

	return core.StringValue(line), nil
}
