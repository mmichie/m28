package builtin

import (
	"bufio"
	"fmt"
	"os"
	"strings"

	"github.com/mmichie/m28/common/errors"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// RegisterIO registers I/O functions using the builder framework
func RegisterIO(ctx *core.Context) {
	// print - print objects to stdout
	// Supports sep= and end= keyword arguments like Python's print()
	printFunc := &core.BuiltinFunctionWithKwargs{
		BaseObject: *core.NewBaseObject(core.FunctionType),
		Name:       "print",
		Fn: func(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
			// Default values for sep and end (Python defaults)
			sep := " "
			end := "\n"

			// Check for sep= kwarg
			if sepVal, ok := kwargs["sep"]; ok {
				if sepVal == core.None {
					sep = " " // None means use default
				} else if s, ok := sepVal.(core.StringValue); ok {
					sep = string(s)
				} else {
					return nil, fmt.Errorf("sep must be None or a string, not %s", sepVal.Type())
				}
			}

			// Check for end= kwarg
			if endVal, ok := kwargs["end"]; ok {
				if endVal == core.None {
					end = "\n" // None means use default
				} else if s, ok := endVal.(core.StringValue); ok {
					end = string(s)
				} else {
					return nil, fmt.Errorf("end must be None or a string, not %s", endVal.Type())
				}
			}

			// Check for unexpected kwargs
			for key := range kwargs {
				if key != "sep" && key != "end" && key != "file" && key != "flush" {
					return nil, fmt.Errorf("print() got an unexpected keyword argument '%s'", key)
				}
			}

			// Print can take any number of arguments
			parts := make([]string, len(args))
			for i, arg := range args {
				parts[i] = core.PrintValueWithoutQuotes(arg)
			}
			fmt.Print(strings.Join(parts, sep) + end)
			return core.NilValue{}, nil
		},
	}
	ctx.Define("print", printFunc)

	// input - read line from stdin with optional prompt
	// BEFORE: 23 lines
	// AFTER: 15 lines with validation
	ctx.Define("input", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("input", args)

		if err := v.Range(0, 1); err != nil {
			return nil, err
		}

		// Print prompt if provided
		if v.Count() == 1 {
			prompt, err := v.GetString(0)
			if err != nil {
				return nil, err
			}
			fmt.Print(prompt)
		}

		// Read line from stdin
		reader := bufio.NewReader(os.Stdin)
		line, err := reader.ReadString('\n')
		if err != nil {
			return nil, errors.NewRuntimeError("input", err.Error())
		}

		// Remove trailing newline
		line = strings.TrimSuffix(line, "\n")
		line = strings.TrimSuffix(line, "\r") // For Windows

		return core.StringValue(line), nil
	}))

	// open - open file with mode and encoding
	// Uses BuiltinFunctionWithKwargs to support encoding= keyword argument
	openFunc := &core.BuiltinFunctionWithKwargs{
		BaseObject: *core.NewBaseObject(core.FunctionType),
		Name:       "open",
		Fn:         OpenBuilderWithKwargs(),
	}
	ctx.Define("open", openFunc)
}

// OpenBuilderWithKwargs creates the open function with keyword argument support
func OpenBuilderWithKwargs() func([]core.Value, map[string]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("open", args)

		if err := v.Range(1, 3); err != nil {
			return nil, err
		}

		// Get filename
		filename, err := v.GetString(0)
		if err != nil {
			return nil, err
		}

		// Get mode (default "r")
		mode := "r"
		if v.Count() >= 2 {
			m, err := v.GetString(1)
			if err != nil {
				return nil, err
			}
			mode = m
		}

		// Get encoding from kwargs (default "utf-8") - accepted but ignored for now
		// encoding := "utf-8"
		// if kwargs != nil {
		// 	if encVal, ok := kwargs["encoding"]; ok {
		// 		if encStr, ok := encVal.(core.StringValue); ok {
		// 			encoding = string(encStr)
		// 		}
		// 	}
		// }

		// Create file object
		// Note: core.NewFile only takes filename and mode
		// The encoding parameter is accepted but currently not used
		file, err := core.NewFile(filename, mode)
		if err != nil {
			return nil, errors.NewRuntimeError("open", err.Error())
		}

		return file, nil
	}
}

// Migration Statistics:
// Functions migrated: 3 I/O functions
// Original lines: ~59 lines
// Migrated lines: ~45 lines
// Reduction: ~24% with better validation
