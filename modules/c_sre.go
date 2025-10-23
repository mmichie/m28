package modules

import (
	"strings"
	"unicode"

	"github.com/mmichie/m28/core"
)

// Init_SREModule initializes the _sre C extension stub module
// This provides minimal regex engine support for Python stdlib modules
func Init_SREModule() *core.DictValue {
	module := core.NewDict()

	// MAGIC - version identifier for the SRE module
	// Must match the MAGIC in re._constants (Python 3.12 = 20221023)
	module.Set("MAGIC", core.NumberValue(20221023))

	// CODESIZE - size of regex opcode in bytes (typically 2 or 4)
	module.Set("CODESIZE", core.NumberValue(4))

	// MAXREPEAT - maximum repeat count
	module.Set("MAXREPEAT", core.NumberValue(4294967295)) // 2^32 - 1

	// MAXGROUPS - maximum number of groups
	module.Set("MAXGROUPS", core.NumberValue(2147483647)) // 2^31 - 1

	// unicode_iscased - check if a Unicode character has case
	module.Set("unicode_iscased", core.NewNamedBuiltinFunction("unicode_iscased", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, core.NewTypeError("unicode_iscased", nil, "unicode_iscased() takes exactly 1 argument")
		}

		// Get the character code
		var ch rune
		switch v := args[0].(type) {
		case core.NumberValue:
			ch = rune(v)
		case core.StringValue:
			if len(v) > 0 {
				ch = rune(v[0])
			} else {
				return core.BoolValue(false), nil
			}
		default:
			return nil, core.NewTypeError("unicode_iscased", args[0], "argument must be int or str")
		}

		// A character is cased if it has both uppercase and lowercase forms
		// or if it is already uppercase or lowercase
		isCased := unicode.IsUpper(ch) || unicode.IsLower(ch) || unicode.ToUpper(ch) != ch || unicode.ToLower(ch) != ch
		return core.BoolValue(isCased), nil
	}))

	// unicode_tolower - convert Unicode character to lowercase
	module.Set("unicode_tolower", core.NewNamedBuiltinFunction("unicode_tolower", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, core.NewTypeError("unicode_tolower", nil, "unicode_tolower() takes exactly 1 argument")
		}

		// Get the character code
		var ch rune
		switch v := args[0].(type) {
		case core.NumberValue:
			ch = rune(v)
		case core.StringValue:
			if len(v) > 0 {
				ch = rune(v[0])
			} else {
				return core.NumberValue(0), nil
			}
		default:
			return nil, core.NewTypeError("unicode_tolower", args[0], "argument must be int or str")
		}

		lower := unicode.ToLower(ch)
		return core.NumberValue(lower), nil
	}))

	// ascii_iscased - check if an ASCII character has case
	module.Set("ascii_iscased", core.NewNamedBuiltinFunction("ascii_iscased", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, core.NewTypeError("ascii_iscased", nil, "ascii_iscased() takes exactly 1 argument")
		}

		// Get the character code
		var ch rune
		switch v := args[0].(type) {
		case core.NumberValue:
			ch = rune(v)
		case core.StringValue:
			if len(v) > 0 {
				ch = rune(v[0])
			} else {
				return core.BoolValue(false), nil
			}
		default:
			return nil, core.NewTypeError("ascii_iscased", args[0], "argument must be int or str")
		}

		// ASCII cased characters are A-Z and a-z
		isCased := (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z')
		return core.BoolValue(isCased), nil
	}))

	// ascii_tolower - convert ASCII character to lowercase
	module.Set("ascii_tolower", core.NewNamedBuiltinFunction("ascii_tolower", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, core.NewTypeError("ascii_tolower", nil, "ascii_tolower() takes exactly 1 argument")
		}

		// Get the character code
		var ch rune
		switch v := args[0].(type) {
		case core.NumberValue:
			ch = rune(v)
		case core.StringValue:
			if len(v) > 0 {
				ch = rune(v[0])
			} else {
				return core.NumberValue(0), nil
			}
		default:
			return nil, core.NewTypeError("ascii_tolower", args[0], "argument must be int or str")
		}

		// ASCII lowercase
		if ch >= 'A' && ch <= 'Z' {
			ch = ch + 32
		}
		return core.NumberValue(ch), nil
	}))

	// compile - compile a regex pattern (stub)
	// Returns a minimal pattern object
	module.Set("compile", core.NewNamedBuiltinFunction("compile", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, core.NewTypeError("compile", nil, "compile() takes at least 1 argument")
		}

		// Create a minimal pattern object (dict with basic attributes)
		pattern := core.NewDict()

		// Store the pattern string
		if patternStr, ok := args[0].(core.StringValue); ok {
			pattern.Set("pattern", patternStr)
		}

		// Add flags (default 0)
		flags := core.NumberValue(0)
		if len(args) >= 2 {
			if f, ok := args[1].(core.NumberValue); ok {
				flags = f
			}
		}
		pattern.Set("flags", flags)

		// Add stub methods
		pattern.Set("match", core.NewNamedBuiltinFunction("match", func(matchArgs []core.Value, matchCtx *core.Context) (core.Value, error) {
			// Return None (no match) for now
			return core.None, nil
		}))

		pattern.Set("search", core.NewNamedBuiltinFunction("search", func(searchArgs []core.Value, searchCtx *core.Context) (core.Value, error) {
			// Return None (no match) for now
			return core.None, nil
		}))

		pattern.Set("findall", core.NewNamedBuiltinFunction("findall", func(findallArgs []core.Value, findallCtx *core.Context) (core.Value, error) {
			// Return empty list for now
			return core.NewList(), nil
		}))

		pattern.Set("sub", core.NewNamedBuiltinFunction("sub", func(subArgs []core.Value, subCtx *core.Context) (core.Value, error) {
			// Just return the original string for now
			if len(subArgs) >= 2 {
				if str, ok := subArgs[1].(core.StringValue); ok {
					return str, nil
				}
			}
			return core.StringValue(""), nil
		}))

		return pattern, nil
	}))

	// getlower - get lowercase version of character (with locale)
	module.Set("getlower", core.NewNamedBuiltinFunction("getlower", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 {
			return nil, core.NewTypeError("getlower", nil, "getlower() takes at least 2 arguments")
		}

		// Get the character code
		var ch rune
		switch v := args[0].(type) {
		case core.NumberValue:
			ch = rune(v)
		case core.StringValue:
			if len(v) > 0 {
				ch = rune(v[0])
			} else {
				return core.NumberValue(0), nil
			}
		default:
			return nil, core.NewTypeError("getlower", args[0], "first argument must be int or str")
		}

		// Simple lowercase conversion
		lower := unicode.ToLower(ch)
		return core.NumberValue(lower), nil
	}))

	// getcodesize - get the size of compiled regex code
	module.Set("getcodesize", core.NewNamedBuiltinFunction("getcodesize", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.NumberValue(4), nil
	}))

	// getlower_locale - lowercase with locale (stub - just uses regular lowercase)
	module.Set("getlower_locale", core.NewNamedBuiltinFunction("getlower_locale", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, core.NewTypeError("getlower_locale", nil, "getlower_locale() takes at least 1 argument")
		}

		var ch rune
		switch v := args[0].(type) {
		case core.NumberValue:
			ch = rune(v)
		case core.StringValue:
			if len(v) > 0 {
				ch = rune(v[0])
			} else {
				return core.NumberValue(0), nil
			}
		default:
			return nil, core.NewTypeError("getlower_locale", args[0], "argument must be int or str")
		}

		lower := strings.ToLower(string(ch))
		if len(lower) > 0 {
			return core.NumberValue(rune(lower[0])), nil
		}
		return core.NumberValue(ch), nil
	}))

	return module
}
