package modules

import (
	"github.com/mmichie/m28/core"
)

// InitUnicodedataModule creates a minimal stub for the unicodedata C extension module
// This is needed by test.support.os_helper for filename normalization on macOS
// We provide minimal stubs to allow test imports to work
func InitUnicodedataModule() *core.DictValue {
	module := core.NewDict()

	// normalize(form, string) -> string
	// Unicode normalization - for now, just return the string unchanged
	// This is sufficient for test.support.os_helper which uses it for test filenames
	module.Set("normalize", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 {
			return nil, core.NewTypeError("normalize", nil, "normalize() takes exactly 2 arguments")
		}
		// args[0] is the normalization form ('NFC', 'NFD', 'NFKC', 'NFKD')
		// args[1] is the string to normalize
		// For now, just return the string unchanged
		return args[1], nil
	}))

	// name(character[, default]) -> string
	// Return the name assigned to the character, or default if not found
	module.Set("name", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, core.NewTypeError("name", nil, "name() takes at least 1 argument")
		}
		// For minimal stub, return empty string or default
		if len(args) > 1 {
			return args[1], nil
		}
		return core.StringValue(""), nil
	}))

	// lookup(name) -> character
	// Look up character by name
	module.Set("lookup", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, core.NewTypeError("lookup", nil, "lookup() takes exactly 1 argument")
		}
		// Minimal stub - would need actual Unicode database
		return core.StringValue(""), nil
	}))

	// category(character) -> string
	// Return general category assigned to the character
	module.Set("category", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, core.NewTypeError("category", nil, "category() takes exactly 1 argument")
		}
		// Return a generic category
		return core.StringValue("Lo"), nil // Letter, other
	}))

	return module
}
