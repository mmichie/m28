package builtin

import (
	"bufio"
	"fmt"
	"os"
	"strings"

	"github.com/mmichie/m28/core"
)

// RegisterIO registers I/O functions
func RegisterIO(ctx *core.Context) {
	// print - print objects to stdout
	ctx.Define("print", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		strs := make([]string, len(args))
		for i, arg := range args {
			if arg == core.Nil {
				strs[i] = "None"
			} else {
				strs[i] = arg.String()
			}
		}
		fmt.Println(strings.Join(strs, " "))
		return core.Nil, nil
	}))

	// input - read line from stdin
	ctx.Define("input", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) > 1 {
			return nil, fmt.Errorf("input() takes at most 1 argument (%d given)", len(args))
		}

		// Print prompt if provided
		if len(args) == 1 {
			prompt := args[0].String()
			fmt.Print(prompt)
		}

		// Read line from stdin
		reader := bufio.NewReader(os.Stdin)
		line, err := reader.ReadString('\n')
		if err != nil {
			return nil, err
		}

		// Remove trailing newline
		line = strings.TrimSuffix(line, "\n")
		line = strings.TrimSuffix(line, "\r") // For Windows

		return core.StringValue(line), nil
	}))

	// open - open file
	ctx.Define("open", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 || len(args) > 3 {
			return nil, fmt.Errorf("open() takes from 1 to 3 arguments (%d given)", len(args))
		}

		// Get filename
		filename, ok := args[0].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("open() argument 1 must be string, not %s", args[0].Type())
		}

		// Get mode (default to "r")
		mode := "r"
		if len(args) >= 2 {
			if m, ok := args[1].(core.StringValue); ok {
				mode = string(m)
			} else {
				return nil, fmt.Errorf("open() argument 2 must be string, not %s", args[1].Type())
			}
		}

		// Encoding parameter would be args[2], but we'll ignore it for now

		// Create and return file object
		return core.NewFile(string(filename), mode)
	}))
}
