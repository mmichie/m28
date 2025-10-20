package modules

import (
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// InitWarningsModule creates and returns a minimal warnings module stub
func InitWarningsModule() *core.DictValue {
	warningsModule := core.NewDict()

	// warn - issue a warning
	warningsModule.Set("warn", core.NewNamedBuiltinFunction("warn", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("warn", args)
		if err := v.Range(1, 4); err != nil {
			return nil, err
		}
		// For now, just ignore warnings
		return core.None, nil
	}))

	// warn_explicit - issue a warning with explicit file and line
	warningsModule.Set("warn_explicit", core.NewNamedBuiltinFunction("warn_explicit", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// For now, just ignore warnings
		return core.None, nil
	}))

	// showwarning - show a warning
	warningsModule.Set("showwarning", core.NewNamedBuiltinFunction("showwarning", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// For now, just ignore warnings
		return core.None, nil
	}))

	// formatwarning - format a warning
	warningsModule.Set("formatwarning", core.NewNamedBuiltinFunction("formatwarning", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.StringValue(""), nil
	}))

	// filterwarnings - add a filter entry to the warnings filter
	warningsModule.Set("filterwarnings", core.NewNamedBuiltinFunction("filterwarnings", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// For now, just ignore
		return core.None, nil
	}))

	// simplefilter - insert a simple filter entry
	warningsModule.Set("simplefilter", core.NewNamedBuiltinFunction("simplefilter", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// For now, just ignore
		return core.None, nil
	}))

	// resetwarnings - clear warnings filters
	warningsModule.Set("resetwarnings", core.NewNamedBuiltinFunction("resetwarnings", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	// Warning - base class for all warning categories
	warningClass := core.NewClass("Warning", nil)
	warningsModule.Set("Warning", warningClass)

	// UserWarning
	userWarningClass := core.NewClass("UserWarning", warningClass)
	warningsModule.Set("UserWarning", userWarningClass)

	// DeprecationWarning
	deprecationWarningClass := core.NewClass("DeprecationWarning", warningClass)
	warningsModule.Set("DeprecationWarning", deprecationWarningClass)

	// PendingDeprecationWarning
	pendingDeprecationWarningClass := core.NewClass("PendingDeprecationWarning", warningClass)
	warningsModule.Set("PendingDeprecationWarning", pendingDeprecationWarningClass)

	// SyntaxWarning
	syntaxWarningClass := core.NewClass("SyntaxWarning", warningClass)
	warningsModule.Set("SyntaxWarning", syntaxWarningClass)

	// RuntimeWarning
	runtimeWarningClass := core.NewClass("RuntimeWarning", warningClass)
	warningsModule.Set("RuntimeWarning", runtimeWarningClass)

	// FutureWarning
	futureWarningClass := core.NewClass("FutureWarning", warningClass)
	warningsModule.Set("FutureWarning", futureWarningClass)

	// ImportWarning
	importWarningClass := core.NewClass("ImportWarning", warningClass)
	warningsModule.Set("ImportWarning", importWarningClass)

	// UnicodeWarning
	unicodeWarningClass := core.NewClass("UnicodeWarning", warningClass)
	warningsModule.Set("UnicodeWarning", unicodeWarningClass)

	// BytesWarning
	bytesWarningClass := core.NewClass("BytesWarning", warningClass)
	warningsModule.Set("BytesWarning", bytesWarningClass)

	// ResourceWarning
	resourceWarningClass := core.NewClass("ResourceWarning", warningClass)
	warningsModule.Set("ResourceWarning", resourceWarningClass)

	// filters list
	warningsModule.Set("filters", core.ListValue{})

	// _defaultaction
	warningsModule.Set("_defaultaction", core.StringValue("default"))

	// _onceregistry
	warningsModule.Set("_onceregistry", core.NewDict())

	return warningsModule
}
