package modules

import (
	"github.com/mmichie/m28/core"
)

// Init_TypingModule creates the _typing module stub
// This is a C extension module that provides core type system primitives
func Init_TypingModule() *core.DictValue {
	typingModule := core.NewDict()

	// _idfunc - identity function used internally by typing module
	typingModule.SetWithKey("_idfunc", core.StringValue("_idfunc"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, core.NewTypeError("1 argument", core.TupleValue(args), "_idfunc() takes exactly 1 argument")
		}
		return args[0], nil
	}))

	// TypeVar - class for type variables
	typeVarClass := core.NewClass("TypeVar", nil)
	typingModule.SetWithKey("TypeVar", core.StringValue("TypeVar"), typeVarClass)

	// ParamSpec - class for parameter specifications
	paramSpecClass := core.NewClass("ParamSpec", nil)
	typingModule.SetWithKey("ParamSpec", core.StringValue("ParamSpec"), paramSpecClass)

	// TypeVarTuple - class for type variable tuples
	typeVarTupleClass := core.NewClass("TypeVarTuple", nil)
	typingModule.SetWithKey("TypeVarTuple", core.StringValue("TypeVarTuple"), typeVarTupleClass)

	// ParamSpecArgs - class for ParamSpec args
	paramSpecArgsClass := core.NewClass("ParamSpecArgs", nil)
	typingModule.SetWithKey("ParamSpecArgs", core.StringValue("ParamSpecArgs"), paramSpecArgsClass)

	// ParamSpecKwargs - class for ParamSpec kwargs
	paramSpecKwargsClass := core.NewClass("ParamSpecKwargs", nil)
	typingModule.SetWithKey("ParamSpecKwargs", core.StringValue("ParamSpecKwargs"), paramSpecKwargsClass)

	// TypeAliasType - class for type aliases
	typeAliasTypeClass := core.NewClass("TypeAliasType", nil)
	typingModule.SetWithKey("TypeAliasType", core.StringValue("TypeAliasType"), typeAliasTypeClass)

	// Generic - base class for generic types
	genericClass := core.NewClass("Generic", nil)
	typingModule.SetWithKey("Generic", core.StringValue("Generic"), genericClass)

	return typingModule
}
