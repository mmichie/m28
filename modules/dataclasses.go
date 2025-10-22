package modules

import (
	"github.com/mmichie/m28/builtin"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// InitDataclassesModule creates and returns a minimal dataclasses module stub
func InitDataclassesModule() *core.DictValue {
	dataclassesModule := core.NewDict()

	// is_dataclass - check if an object is a dataclass
	dataclassesModule.Set("is_dataclass", core.NewNamedBuiltinFunction("is_dataclass", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("is_dataclass", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		// For now, always return False
		// TODO: Track which classes are dataclasses
		return core.False, nil
	}))

	// fields - return field definitions for a dataclass
	dataclassesModule.Set("fields", core.NewNamedBuiltinFunction("fields", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("fields", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		// Return empty tuple since we don't track dataclass fields yet
		return core.TupleValue{}, nil
	}))

	// dataclass - decorator to create a dataclass
	dataclassesModule.Set("dataclass", builtin.NewKwargsBuiltinFunction("dataclass", func(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
		// Can be called as @dataclass or @dataclass(init=True, repr=True, ...)
		// For now, just return a passthrough decorator that ignores all kwargs
		if len(args) == 0 {
			// Called with keyword arguments only: @dataclass(init=True, ...)
			// Return a function that takes a class and returns it
			return core.NewBuiltinFunction(func(classArgs []core.Value, classCtx *core.Context) (core.Value, error) {
				if len(classArgs) != 1 {
					return nil, core.NewTypeError("class", nil, "dataclass decorator argument")
				}
				// Just return the class unchanged
				return classArgs[0], nil
			}), nil
		}

		// Called without keyword arguments, or with a class as first arg: @dataclass
		// args[0] is the class, return it unchanged
		return args[0], nil
	}))

	// field - define a field with metadata
	dataclassesModule.Set("field", builtin.NewKwargsBuiltinFunction("field", func(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
		// Return a simple dict representing a field
		// This is called like: field(default=None, init=True, repr=True)
		// For now, ignore all kwargs and just return an empty dict
		return core.NewDict(), nil
	}))

	// FrozenInstanceError - exception for frozen dataclass modification
	frozenErrorClass := core.NewClass("FrozenInstanceError", nil)
	dataclassesModule.Set("FrozenInstanceError", frozenErrorClass)

	// MISSING sentinel value
	dataclassesModule.Set("MISSING", core.NewClass("_MISSING_TYPE", nil))

	return dataclassesModule
}
