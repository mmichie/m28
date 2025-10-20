package modules

import (
	"github.com/mmichie/m28/core"
)

// InitTypesModule creates and returns the types module
func InitTypesModule() *core.DictValue {
	typesModule := core.NewDict()

	// DynamicClassAttribute - similar to property but with special class/instance behavior
	// For now, create a simple class that will be used as a base for enum.property
	typesModule.Set("DynamicClassAttribute", core.NewClass("DynamicClassAttribute", nil))

	// MappingProxyType - read-only proxy for mappings
	// For now, return a simple class
	typesModule.Set("MappingProxyType", core.NewClass("MappingProxyType", nil))

	// GenericAlias - used for generic type hints like list[int]
	// For now, return a simple class
	typesModule.Set("GenericAlias", core.NewClass("GenericAlias", nil))

	// SimpleNamespace - a simple object that can hold arbitrary attributes
	simpleNamespaceClass := core.NewClass("SimpleNamespace", nil)
	// Add __init__ method
	simpleNamespaceClass.SetMethod("__init__", core.NewNamedBuiltinFunction("__init__", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) == 0 {
			return nil, core.NewTypeError("object", nil, "__init__ missing required argument: self")
		}
		self := args[0]
		// SimpleNamespace can accept keyword arguments, but we'll accept them as a dict for now
		// In Python: SimpleNamespace(x=1, y=2) creates an object with x and y attributes
		return self, nil
	}))
	// Add __repr__ method
	simpleNamespaceClass.SetMethod("__repr__", core.NewNamedBuiltinFunction("__repr__", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) == 0 {
			return nil, core.NewTypeError("object", nil, "__repr__ missing required argument: self")
		}
		// For now, just return a simple representation
		return core.StringValue("namespace()"), nil
	}))
	typesModule.Set("SimpleNamespace", simpleNamespaceClass)

	// MethodType - type of bound methods
	// For now, create a simple callable class
	typesModule.Set("MethodType", core.NewNamedBuiltinFunction("MethodType", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// MethodType(func, instance) creates a bound method
		// For now, just return the function
		if len(args) >= 1 {
			return args[0], nil
		}
		return core.None, nil
	}))

	return typesModule
}
