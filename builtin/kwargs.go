package builtin

import (
	"fmt"
	"github.com/mmichie/m28/core"
)

// KwargsBuiltinFunction is a builtin function that supports keyword arguments
type KwargsBuiltinFunction struct {
	core.BaseObject
	fn       func(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error)
	name     string
	registry *core.MethodRegistry
}

// NewKwargsBuiltinFunction creates a new builtin function that accepts keyword arguments
func NewKwargsBuiltinFunction(name string, fn func(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error)) *KwargsBuiltinFunction {
	f := &KwargsBuiltinFunction{
		BaseObject: *core.NewBaseObject(core.FunctionType),
		fn:         fn,
		name:       name,
	}

	// Initialize the method registry
	f.registry = f.createRegistry()

	return f
}

// Call implements Callable.Call - but this won't be called directly due to how eval works
func (f *KwargsBuiltinFunction) Call(args []core.Value, ctx *core.Context) (core.Value, error) {
	// This is called when no keyword arguments are provided
	return f.fn(args, make(map[string]core.Value), ctx)
}

// CallWithKeywords is the method that eval should use for keyword argument support
func (f *KwargsBuiltinFunction) CallWithKeywords(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
	return f.fn(args, kwargs, ctx)
}

// Type returns the function type
func (f *KwargsBuiltinFunction) Type() core.Type {
	return core.FunctionType
}

// String returns string representation
func (f *KwargsBuiltinFunction) String() string {
	if f.name != "" {
		return fmt.Sprintf("<builtin '%s'>", f.name)
	}
	return "<builtin>"
}

// createRegistry sets up all properties for KwargsBuiltinFunction
func (f *KwargsBuiltinFunction) createRegistry() *core.MethodRegistry {
	registry := core.NewMethodRegistry()

	// Register properties
	registry.RegisterProperties(
		core.MakeProperty("__name__", "Function name", func(receiver core.Value) (core.Value, error) {
			fn := receiver.(*KwargsBuiltinFunction)
			return core.StringValue(fn.name), nil
		}),
	)

	return registry
}

// GetRegistry implements AttributeProvider
func (f *KwargsBuiltinFunction) GetRegistry() *core.MethodRegistry {
	return f.registry
}

// GetBaseObject implements AttributeProvider
func (f *KwargsBuiltinFunction) GetBaseObject() *core.BaseObject {
	return &f.BaseObject
}

// GetAttr implements the new simplified GetAttr pattern
func (f *KwargsBuiltinFunction) GetAttr(name string) (core.Value, bool) {
	return core.GetAttrWithRegistry(f, name)
}
