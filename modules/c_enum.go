package modules

import (
	"github.com/mmichie/m28/core"
)

// simpleEnumWithKwargs wraps a BuiltinFunction to accept keyword arguments
type simpleEnumWithKwargs struct {
	*core.BuiltinFunction
}

// CallWithKeywords implements keyword argument support for _simple_enum
func (s *simpleEnumWithKwargs) CallWithKeywords(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
	// _simple_enum accepts keyword arguments but we ignore them in the stub
	// Just call the underlying function with positional args only
	return s.BuiltinFunction.Call(args, ctx)
}

// InitEnumModule initializes a stub enum module
// This provides minimal enum support for Python stdlib modules like re
func InitEnumModule() *core.DictValue {
	module := core.NewDict()

	// Create a simple Enum base class
	enumClass := core.NewClassWithParents("Enum", []*core.Class{})
	module.Set("Enum", enumClass)

	// Create IntFlag class (subclass of Enum)
	intFlagClass := core.NewClassWithParents("IntFlag", []*core.Class{enumClass})
	module.Set("IntFlag", intFlagClass)

	// Create Flag class (subclass of Enum)
	flagClass := core.NewClassWithParents("Flag", []*core.Class{enumClass})
	module.Set("Flag", flagClass)

	// Create StrEnum class (subclass of Enum)
	strEnumClass := core.NewClassWithParents("StrEnum", []*core.Class{enumClass})
	module.Set("StrEnum", strEnumClass)

	// FlagBoundary enum values
	// For now, these are just string constants
	module.Set("STRICT", core.StringValue("STRICT"))
	module.Set("CONFORM", core.StringValue("CONFORM"))
	module.Set("EJECT", core.StringValue("EJECT"))
	module.Set("KEEP", core.StringValue("KEEP"))

	// Create FlagBoundary class
	flagBoundaryClass := core.NewClassWithParents("FlagBoundary", []*core.Class{strEnumClass})
	module.Set("FlagBoundary", flagBoundaryClass)

	// global_enum decorator - for now, just returns the class unchanged
	module.Set("global_enum", core.NewNamedBuiltinFunction("global_enum", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, core.NewTypeError("global_enum", nil, "global_enum() takes at least 1 argument")
		}
		// Just return the class unchanged
		return args[0], nil
	}))

	// global_flag_repr decorator
	module.Set("global_flag_repr", core.NewNamedBuiltinFunction("global_flag_repr", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, core.NewTypeError("global_flag_repr", nil, "global_flag_repr() takes at least 1 argument")
		}
		return args[0], nil
	}))

	// global_enum_repr decorator
	module.Set("global_enum_repr", core.NewNamedBuiltinFunction("global_enum_repr", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, core.NewTypeError("global_enum_repr", nil, "global_enum_repr() takes at least 1 argument")
		}
		return args[0], nil
	}))

	// global_str decorator
	module.Set("global_str", core.NewNamedBuiltinFunction("global_str", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, core.NewTypeError("global_str", nil, "global_str() takes at least 1 argument")
		}
		return args[0], nil
	}))

	// _simple_enum decorator factory
	// Returns a decorator that returns the class unchanged
	// _simple_enum(etype=Enum, *, boundary=None, use_args=None)
	simpleEnumFunc := core.NewNamedBuiltinFunction("_simple_enum", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Returns a decorator function
		decorator := core.NewNamedBuiltinFunction("_simple_enum_decorator", func(decoratorArgs []core.Value, decoratorCtx *core.Context) (core.Value, error) {
			if len(decoratorArgs) < 1 {
				return nil, core.NewTypeError("_simple_enum", nil, "_simple_enum decorator takes at least 1 argument")
			}
			// Just return the class unchanged
			return decoratorArgs[0], nil
		})
		return decorator, nil
	})
	// Create a wrapper that implements CallWithKeywords
	simpleEnumWrapper := &simpleEnumWithKwargs{
		BuiltinFunction: simpleEnumFunc,
	}
	module.Set("_simple_enum", simpleEnumWrapper)

	// auto() function - returns incrementing integers
	autoCounter := 0
	module.Set("auto", core.NewNamedBuiltinFunction("auto", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		autoCounter++
		return core.NumberValue(autoCounter), nil
	}))

	// nonmember decorator - marks a member as not part of the enum
	module.Set("nonmember", core.NewNamedBuiltinFunction("nonmember", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, core.NewTypeError("nonmember", nil, "nonmember() takes exactly 1 argument")
		}
		return args[0], nil
	}))

	// member decorator - marks a member as part of the enum
	module.Set("member", core.NewNamedBuiltinFunction("member", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, core.NewTypeError("member", nil, "member() takes exactly 1 argument")
		}
		return args[0], nil
	}))

	// property decorator for enum members
	module.Set("property", core.NewNamedBuiltinFunction("property", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, core.NewTypeError("property", nil, "property() takes at least 1 argument")
		}
		return args[0], nil
	}))

	// EnumType metaclass - for now, just return a stub
	enumTypeClass := core.NewClassWithParents("EnumType", []*core.Class{})
	module.Set("EnumType", enumTypeClass)

	// EnumMeta metaclass (alias for EnumType)
	module.Set("EnumMeta", enumTypeClass)

	// ReprEnum class
	reprEnumClass := core.NewClassWithParents("ReprEnum", []*core.Class{enumClass})
	module.Set("ReprEnum", reprEnumClass)

	// IntEnum class
	intEnumClass := core.NewClassWithParents("IntEnum", []*core.Class{enumClass})

	// Add _convert_() classmethod to IntEnum
	// This is used by signal.py to convert module-level constants into enum members
	// For our minimal stub, we just return None to indicate success
	intEnumClass.SetMethod("_convert_", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// signal.py calls: _IntEnum._convert_('Signals', __name__, lambda_filter)
		// We don't need to actually do anything - just return None to indicate success
		return core.None, nil
	}))

	module.Set("IntEnum", intEnumClass)

	// unique decorator - ensures all enum values are unique
	module.Set("unique", core.NewNamedBuiltinFunction("unique", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, core.NewTypeError("unique", nil, "unique() takes exactly 1 argument")
		}
		return args[0], nil
	}))

	// verify decorator - verifies enum constraints
	module.Set("verify", core.NewNamedBuiltinFunction("verify", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, core.NewTypeError("verify", nil, "verify() takes at least 1 argument")
		}
		// Returns a decorator
		decorator := core.NewNamedBuiltinFunction("verify_decorator", func(decoratorArgs []core.Value, decoratorCtx *core.Context) (core.Value, error) {
			if len(decoratorArgs) < 1 {
				return nil, core.NewTypeError("verify", nil, "verify decorator takes at least 1 argument")
			}
			return decoratorArgs[0], nil
		})
		return decorator, nil
	}))

	// CONTINUOUS, NAMED_FLAGS, UNIQUE - verification flags
	module.Set("CONTINUOUS", core.StringValue("CONTINUOUS"))
	module.Set("NAMED_FLAGS", core.StringValue("NAMED_FLAGS"))
	module.Set("UNIQUE", core.StringValue("UNIQUE"))

	return module
}
