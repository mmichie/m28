package modules

import (
	"github.com/mmichie/m28/core"
)

// Init_ContextvarsModule creates the _contextvars module stub
// This is a C extension module that provides context variables for async code
func Init_ContextvarsModule() *core.DictValue {
	contextvarsModule := core.NewDict()

	// ContextVar - class for context variables
	//   ContextVar(name, *, default=MISSING)
	// Stores the name as __name__ and the default (or sentinel) for later get().
	contextVarClass := core.NewClass("ContextVar", nil)
	contextVarMissing := core.NewDict() // sentinel for "no default"
	contextVarClass.SetMethod("__init__", &core.BuiltinFunctionWithKwargs{
		BaseObject: *core.NewBaseObject(core.FunctionType),
		Name:       "ContextVar.__init__",
		Fn: func(args []core.Value, kwargs *core.Kwargs, ctx *core.Context) (core.Value, error) {
			if len(args) < 2 {
				return nil, &core.TypeError{Message: "ContextVar() takes a name argument"}
			}
			self, ok := args[0].(*core.Instance)
			if !ok {
				return core.None, nil
			}
			self.Attributes["__name__"] = args[1]
			self.Attributes["_value"] = core.Value(contextVarMissing)
			if def, ok := kwargs.Get("default"); ok {
				self.Attributes["_default"] = def
			} else {
				self.Attributes["_default"] = core.Value(contextVarMissing)
			}
			return core.None, nil
		},
	})
	contextVarClass.SetMethod("get", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, &core.TypeError{Message: "get() missing self"}
		}
		self, ok := args[0].(*core.Instance)
		if !ok {
			return core.None, nil
		}
		// If a current value has been set, return it
		if v, ok := self.Attributes["_value"]; ok && v != core.Value(contextVarMissing) {
			return v, nil
		}
		// Else fall back to explicit get(default) argument, then __init__ default
		if len(args) >= 2 {
			return args[1], nil
		}
		if d, ok := self.Attributes["_default"]; ok && d != core.Value(contextVarMissing) {
			return d, nil
		}
		// No value and no default: CPython raises LookupError (which callers like
		// decimal.getcontext() catch), not AttributeError.
		return nil, &core.LookupError{Message: "ContextVar has no value"}
	}))
	contextVarClass.SetMethod("set", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 {
			return nil, &core.TypeError{Message: "set() requires a value"}
		}
		self, ok := args[0].(*core.Instance)
		if !ok {
			return core.None, nil
		}
		oldValue := self.Attributes["_value"]
		self.Attributes["_value"] = args[1]
		return oldValue, nil
	}))
	contextVarClass.SetMethod("reset", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 {
			return core.None, nil
		}
		self, ok := args[0].(*core.Instance)
		if !ok {
			return core.None, nil
		}
		self.Attributes["_value"] = args[1]
		return core.None, nil
	}))
	contextvarsModule.SetStr("ContextVar", contextVarClass)

	// Context - class representing an execution context
	contextClass := core.NewClass("Context", nil)
	contextvarsModule.SetStr("Context", contextClass)

	// Token - class for context variable tokens
	tokenClass := core.NewClass("Token", nil)
	contextvarsModule.SetStr("Token", tokenClass)

	// copy_context() - returns a copy of the current context
	contextvarsModule.SetStr("copy_context", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Return a stub Context instance
		contextInstance, err := contextClass.Call([]core.Value{}, ctx)
		if err != nil {
			return nil, err
		}
		return contextInstance, nil
	}))

	return contextvarsModule
}
