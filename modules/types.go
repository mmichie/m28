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

	// FunctionType - type of user-defined functions
	// This is the type returned by type(lambda: None)
	typesModule.Set("FunctionType", core.NewClass("FunctionType", nil))

	// LambdaType - alias for FunctionType
	typesModule.Set("LambdaType", core.NewClass("FunctionType", nil))

	// CodeType - type of code objects
	typesModule.Set("CodeType", core.NewClass("CodeType", nil))

	// BuiltinFunctionType - type of built-in functions
	typesModule.Set("BuiltinFunctionType", core.NewClass("BuiltinFunctionType", nil))

	// BuiltinMethodType - alias for BuiltinFunctionType
	typesModule.Set("BuiltinMethodType", core.NewClass("BuiltinFunctionType", nil))

	// ModuleType - type of modules
	typesModule.Set("ModuleType", core.NewClass("ModuleType", nil))

	// GeneratorType - type of generator objects
	typesModule.Set("GeneratorType", core.NewClass("GeneratorType", nil))

	// CoroutineType - type of coroutine objects
	typesModule.Set("CoroutineType", core.NewClass("CoroutineType", nil))

	// AsyncGeneratorType - type of async generator objects
	typesModule.Set("AsyncGeneratorType", core.NewClass("AsyncGeneratorType", nil))

	// FrameType - type of frame objects
	typesModule.Set("FrameType", core.NewClass("FrameType", nil))

	// TracebackType - type of traceback objects
	typesModule.Set("TracebackType", core.NewClass("TracebackType", nil))

	// NoneType - type of None
	typesModule.Set("NoneType", core.NewClass("NoneType", nil))

	// NotImplementedType - type of NotImplemented
	typesModule.Set("NotImplementedType", core.NewClass("NotImplementedType", nil))

	// EllipsisType - type of Ellipsis (...)
	typesModule.Set("EllipsisType", core.NewClass("EllipsisType", nil))

	return typesModule
}
