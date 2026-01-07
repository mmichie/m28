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
	// TypeVar(name, *constraints, bound=None, covariant=False, contravariant=False)
	typeVarClass := core.NewClass("TypeVar", nil)
	typeVarClass.SetMethod("__init__", &core.BuiltinFunctionWithKwargs{
		BaseObject: *core.NewBaseObject(core.FunctionType),
		Name:       "TypeVar.__init__",
		Fn: func(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) < 2 {
				return core.None, nil
			}
			self, ok := args[0].(*core.Instance)
			if !ok {
				return core.None, nil
			}
			// First arg after self is the name
			self.Attributes["__name__"] = args[1]
			// Store constraints (remaining positional args)
			if len(args) > 2 {
				constraints := make([]core.Value, len(args)-2)
				copy(constraints, args[2:])
				self.Attributes["__constraints__"] = core.TupleValue(constraints)
			} else {
				self.Attributes["__constraints__"] = core.TupleValue{}
			}
			// Handle kwargs: bound, covariant, contravariant
			self.Attributes["__bound__"] = core.None
			if bound, ok := kwargs["bound"]; ok {
				self.Attributes["__bound__"] = bound
			}
			self.Attributes["__covariant__"] = core.BoolValue(false)
			if cov, ok := kwargs["covariant"]; ok {
				self.Attributes["__covariant__"] = cov
			}
			self.Attributes["__contravariant__"] = core.BoolValue(false)
			if contra, ok := kwargs["contravariant"]; ok {
				self.Attributes["__contravariant__"] = contra
			}
			return core.None, nil
		},
	})
	typeVarClass.SetMethod("__repr__", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return core.StringValue("TypeVar"), nil
		}
		if self, ok := args[0].(*core.Instance); ok {
			if name, found := self.Attributes["__name__"]; found {
				return core.StringValue("~" + name.String()), nil
			}
		}
		return core.StringValue("TypeVar"), nil
	}))
	typingModule.SetWithKey("TypeVar", core.StringValue("TypeVar"), typeVarClass)

	// ParamSpec - class for parameter specifications
	paramSpecClass := core.NewClass("ParamSpec", nil)
	paramSpecClass.SetMethod("__init__", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 {
			return core.None, nil
		}
		self, ok := args[0].(*core.Instance)
		if !ok {
			return core.None, nil
		}
		self.Attributes["__name__"] = args[1]
		return core.None, nil
	}))
	typingModule.SetWithKey("ParamSpec", core.StringValue("ParamSpec"), paramSpecClass)

	// TypeVarTuple - class for type variable tuples
	typeVarTupleClass := core.NewClass("TypeVarTuple", nil)
	typeVarTupleClass.SetMethod("__init__", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 {
			return core.None, nil
		}
		self, ok := args[0].(*core.Instance)
		if !ok {
			return core.None, nil
		}
		self.Attributes["__name__"] = args[1]
		return core.None, nil
	}))
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
