package modules

import (
	"github.com/mmichie/m28/core"
)

// Init_ContextvarsModule creates the _contextvars module stub
// This is a C extension module that provides context variables for async code
func Init_ContextvarsModule() *core.DictValue {
	contextvarsModule := core.NewDict()

	// ContextVar - class for context variables
	contextVarClass := core.NewClass("ContextVar", nil)
	contextvarsModule.SetWithKey("ContextVar", core.StringValue("ContextVar"), contextVarClass)

	// Context - class representing an execution context
	contextClass := core.NewClass("Context", nil)
	contextvarsModule.SetWithKey("Context", core.StringValue("Context"), contextClass)

	// Token - class for context variable tokens
	tokenClass := core.NewClass("Token", nil)
	contextvarsModule.SetWithKey("Token", core.StringValue("Token"), tokenClass)

	// copy_context() - returns a copy of the current context
	contextvarsModule.SetWithKey("copy_context", core.StringValue("copy_context"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Return a stub Context instance
		contextInstance, err := contextClass.Call([]core.Value{}, ctx)
		if err != nil {
			return nil, err
		}
		return contextInstance, nil
	}))

	return contextvarsModule
}
