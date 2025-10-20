// Package modules provides the traceback module for M28
package modules

import (
	"github.com/mmichie/m28/core"
)

// InitTracebackModule registers the traceback module
// This is a minimal stub to unblock unittest imports
func InitTracebackModule() *core.DictValue {
	tracebackModule := core.NewDict()

	// TracebackException class (stub)
	tracebackModule.SetWithKey("TracebackException", core.StringValue("TracebackException"),
		core.NewBuiltinFunction(newTracebackException))

	// Add other common functions as stubs
	tracebackModule.SetWithKey("format_exc", core.StringValue("format_exc"),
		core.NewBuiltinFunction(formatExc))
	tracebackModule.SetWithKey("print_exc", core.StringValue("print_exc"),
		core.NewBuiltinFunction(printExc))

	return tracebackModule
}

func newTracebackException(args []core.Value, ctx *core.Context) (core.Value, error) {
	// Stub: just return a dict representing a TracebackException
	// In reality, this would capture exception info and format it
	obj := core.NewDict()
	obj.SetWithKey("__class__", core.StringValue("__class__"), core.StringValue("TracebackException"))

	// Add required methods
	obj.SetWithKey("format", core.StringValue("format"),
		core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			// Return empty list of formatted lines
			return core.NewList(), nil
		}))

	return obj, nil
}

func formatExc(args []core.Value, ctx *core.Context) (core.Value, error) {
	// Return empty string for now
	return core.StringValue(""), nil
}

func printExc(args []core.Value, ctx *core.Context) (core.Value, error) {
	// No-op for now
	return core.NilValue{}, nil
}
