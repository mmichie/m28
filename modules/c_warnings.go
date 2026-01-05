package modules

import (
	"fmt"
	"os"

	"github.com/mmichie/m28/core"
)

// Init_WarningsModule creates and returns the _warnings module stub
// This is a minimal stub that provides the C extension interface for warnings.py
func Init_WarningsModule() *core.DictValue {
	warningsModule := core.NewDict()

	// warn - emit a warning message
	warningsModule.Set("warn", &core.BuiltinFunctionWithKwargs{
		BaseObject: *core.NewBaseObject(core.FunctionType),
		Name:       "warn",
		Fn: func(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) < 1 {
				return nil, fmt.Errorf("warn() missing required argument: 'message'")
			}
			message := core.PrintValue(args[0])
			category := "UserWarning"
			if len(args) >= 2 {
				if str, ok := args[1].(core.StringValue); ok {
					category = string(str)
				} else if cls, ok := args[1].(*core.Class); ok {
					category = cls.Name
				}
			}
			fmt.Fprintf(os.Stderr, "<string>:1: %s: %s\n", category, message)
			return core.None, nil
		},
	})

	// warn_explicit - emit a warning with explicit location
	warningsModule.Set("warn_explicit", &core.BuiltinFunctionWithKwargs{
		BaseObject: *core.NewBaseObject(core.FunctionType),
		Name:       "warn_explicit",
		Fn: func(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) < 4 {
				return nil, fmt.Errorf("warn_explicit() requires at least 4 arguments")
			}
			message := core.PrintValue(args[0])
			category := "Warning"
			if str, ok := args[1].(core.StringValue); ok {
				category = string(str)
			} else if cls, ok := args[1].(*core.Class); ok {
				category = cls.Name
			}
			filename := core.PrintValue(args[2])
			lineno := 0
			if num, ok := args[3].(core.NumberValue); ok {
				lineno = int(num)
			}
			fmt.Fprintf(os.Stderr, "%s:%d: %s: %s\n", filename, lineno, category, message)
			return core.None, nil
		},
	})

	// filters - list of warning filters
	warningsModule.Set("filters", core.NewList())

	// _defaultaction - default warning action
	warningsModule.Set("_defaultaction", core.StringValue("default"))

	// _onceregistry - registry for "once" warnings
	warningsModule.Set("_onceregistry", core.NewDict())

	// _filters_mutated - called when filters list is modified
	warningsModule.Set("_filters_mutated", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// No-op for now - would normally invalidate warning caches
		return core.None, nil
	}))

	// _showwarnmsg_impl - implementation for showing warnings
	// This gets replaced by catch_warnings(record=True) with list.append
	showwarnmsgImpl := core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Default: just ignore warnings
		return core.None, nil
	})
	warningsModule.Set("_showwarnmsg_impl", showwarnmsgImpl)

	// showwarning - show a warning
	showwarningFunc := &core.BuiltinFunctionWithKwargs{
		BaseObject: *core.NewBaseObject(core.FunctionType),
		Name:       "showwarning",
		Fn: func(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
			// Default implementation - calls _showwarnmsg_impl
			// The Python warnings module may replace this
			return core.None, nil
		},
	}
	warningsModule.Set("showwarning", showwarningFunc)

	// _showwarning_orig - original showwarning implementation
	// Used by catch_warnings to reset
	warningsModule.Set("_showwarning_orig", showwarningFunc)

	return warningsModule
}
