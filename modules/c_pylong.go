package modules

import (
	"github.com/mmichie/m28/core"
)

// Init_PylongModule creates the _pylong module stub
// This provides Python implementations of algorithms for use by longobject.c.
// The CPython _pylong module provides asymptotically faster algorithms for huge integers,
// but test_int.py handles ImportError gracefully and checks if _pylong is None.
func Init_PylongModule() *core.DictValue {
	module := core.NewDict()

	// int_to_decimal - convert int to Decimal (would need decimal module)
	// We stub this but it won't be called if test_int checks for None
	module.Set("int_to_decimal", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// This would convert a large int to decimal.Decimal
		// For now, just return None to indicate not implemented
		return core.None, nil
	}))

	// int_to_decimal_string - convert int to decimal string
	module.Set("int_to_decimal_string", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// This would convert a large int to a decimal string representation
		// For now, use str() as fallback
		if len(args) != 1 {
			return core.None, nil
		}
		return core.StringValue(args[0].String()), nil
	}))

	// _str_to_int_inner - asymptotically fast string to int conversion
	module.Set("_str_to_int_inner", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// This would do fast string to int conversion for huge numbers
		// For now, just return None to indicate not implemented
		return core.None, nil
	}))

	return module
}
