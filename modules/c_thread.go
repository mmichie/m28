package modules

import (
	"github.com/mmichie/m28/core"
)

// InitThreadModule initializes the _thread C extension stub module
// This provides minimal threading support for Python stdlib modules
func InitThreadModule() *core.DictValue {
	module := core.NewDict()

	// get_ident() - returns the thread identifier of the current thread
	// In M28 (single-threaded), always return a constant value
	module.Set("get_ident", core.NewNamedBuiltinFunction("get_ident", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 0 {
			return nil, core.NewTypeError("get_ident", nil, "get_ident() takes no arguments")
		}
		// Return a constant thread ID since M28 is single-threaded
		return core.NumberValue(1), nil
	}))

	return module
}
