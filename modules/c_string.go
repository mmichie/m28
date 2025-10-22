package modules

import (
	"github.com/mmichie/m28/core"
)

// Init_StringModule creates the _string C extension module stub
// This provides C-accelerated string functions for Python's string.py
func Init_StringModule() *core.DictValue {
	stringModule := core.NewDict()

	// formatter_parser - used by string.Formatter
	// Returns an iterator of (literal_text, field_name, format_spec, conversion) tuples
	stringModule.Set("formatter_parser", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, core.NewTypeError("str", nil, "formatter_parser() argument")
		}
		// Stub: return empty list (no format fields)
		return core.NewList(), nil
	}))

	// formatter_field_name_split - used by string.Formatter
	// Splits a field name into (first, rest) where first is the attribute name
	stringModule.Set("formatter_field_name_split", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, core.NewTypeError("str", nil, "formatter_field_name_split() argument")
		}
		fieldName, ok := args[0].(core.StringValue)
		if !ok {
			return nil, core.NewTypeError("str", args[0], "formatter_field_name_split() argument")
		}
		// Stub: return (field_name, empty_iterator)
		result := core.TupleValue{fieldName, core.NewList()}
		return result, nil
	}))

	return stringModule
}
