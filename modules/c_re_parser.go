package modules

import (
	"github.com/mmichie/m28/core"
)

// Init_REParserModule initializes the re._parser stub module
// This provides minimal regex parser support for the re module
func Init_REParserModule() *core.DictValue {
	module := core.NewDict()

	// parse - parse a regex pattern string or bytes into a parsed pattern object
	// parse(str|bytes, flags=0, state=None)
	module.Set("parse", core.NewNamedBuiltinFunction("parse", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		core.DebugLog("[RE-PARSER] parse() called with %d args\n", len(args))
		if len(args) < 1 {
			return nil, core.NewTypeError("parse", nil, "parse() takes at least 1 argument")
		}

		// Get pattern - can be string or bytes
		var pattern core.Value
		switch p := args[0].(type) {
		case core.StringValue:
			pattern = p
			core.DebugLog("[RE-PARSER] pattern (string): %s\n", string(p))
		case core.BytesValue:
			pattern = p
			core.DebugLog("[RE-PARSER] pattern (bytes): %v\n", []byte(p))
		default:
			return nil, core.NewTypeError("parse", args[0], "pattern must be a string or bytes")
		}

		// Get flags (default 0)
		flags := core.NumberValue(0)
		if len(args) >= 2 {
			if f, ok := args[1].(core.NumberValue); ok {
				flags = f
			}
		}
		core.DebugLog("[RE-PARSER] flags: %v\n", flags)

		// Create a State object (minimal stub)
		core.DebugLog("[RE-PARSER] Creating state dict\n")
		state := core.NewDict()
		state.Set("flags", flags)
		state.Set("str", pattern)
		state.Set("groups", core.NumberValue(1)) // At least 1 group (the whole match)
		state.Set("groupdict", core.NewDict())
		state.Set("grouprefpos", core.NewDict())
		state.Set("groupwidths", core.NewDict())

		// Create a SubPattern object (minimal stub)
		// SubPattern needs: .state, .data, .width
		core.DebugLog("[RE-PARSER] Creating subPattern dict\n")
		subPattern := core.NewDict()
		subPattern.Set("state", state)
		subPattern.Set("data", core.NewList()) // Empty list of pattern operations
		subPattern.Set("width", core.TupleValue{core.NumberValue(0), core.NumberValue(65535)})

		// Add getwidth method
		core.DebugLog("[RE-PARSER] Adding getwidth method\n")
		subPattern.Set("getwidth", core.NewNamedBuiltinFunction("getwidth", func(getwidthArgs []core.Value, getwidthCtx *core.Context) (core.Value, error) {
			core.DebugLog("[RE-PARSER] getwidth() called\n")
			// Return (min_width, max_width)
			return core.TupleValue{core.NumberValue(0), core.NumberValue(65535)}, nil
		}))

		core.DebugLog("[RE-PARSER] parse() returning subPattern\n")
		return subPattern, nil
	}))

	// parse_template - parse a replacement template string
	// parse_template(source, pattern)
	module.Set("parse_template", core.NewNamedBuiltinFunction("parse_template", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 {
			return nil, core.NewTypeError("parse_template", nil, "parse_template() takes 2 arguments")
		}

		// Return empty list for now (no replacement operations)
		return core.NewList(), nil
	}))

	// State class - represents parser state
	stateClass := core.NewClassWithParents("State", []*core.Class{})
	module.Set("State", stateClass)

	// SubPattern class - represents a parsed subpattern
	subPatternClass := core.NewClassWithParents("SubPattern", []*core.Class{})
	module.Set("SubPattern", subPatternClass)

	// Tokenizer class - tokenizes regex patterns
	tokenizerClass := core.NewClassWithParents("Tokenizer", []*core.Class{})
	module.Set("Tokenizer", tokenizerClass)

	// error - regex parsing error exception
	errorClass := core.NewClassWithParents("error", []*core.Class{})
	module.Set("error", errorClass)

	return module
}
