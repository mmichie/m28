package builtin

import (
	"fmt"
	"github.com/mmichie/m28/core"
)

// KwargsBuiltinFunction is a builtin function that supports keyword arguments
type KwargsBuiltinFunction struct {
	core.BaseObject
	fn   func(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error)
	name string
}

// NewKwargsBuiltinFunction creates a new builtin function that accepts keyword arguments
func NewKwargsBuiltinFunction(name string, fn func(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error)) *KwargsBuiltinFunction {
	return &KwargsBuiltinFunction{
		BaseObject: *core.NewBaseObject(core.FunctionType),
		fn:         fn,
		name:       name,
	}
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

// GetAttr gets attributes from the function
func (f *KwargsBuiltinFunction) GetAttr(name string) (core.Value, bool) {
	switch name {
	case "__name__":
		return core.StringValue(f.name), true
	default:
		return f.BaseObject.GetAttr(name)
	}
}
