package builtin

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// RegisterAttributes registers attribute-related functions
func RegisterAttributes(ctx *core.Context) {
	// dir - list attributes of an object
	ctx.Define("dir", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("dir expects 1 argument, got %d", len(args))
		}

		desc := core.GetTypeDescriptorForValue(args[0])
		if desc == nil {
			// Return empty list for types without descriptors
			return core.EmptyList, nil
		}

		// Get all attribute names
		names := desc.GetAttributeNames()
		result := make(core.ListValue, len(names))
		for i, name := range names {
			result[i] = core.StringValue(name)
		}

		return result, nil
	}))

	// hasattr, getattr, setattr are now registered in essential_builtins.go
	// which has more complete implementations that handle type descriptors

	// delattr - delete attribute from object
	ctx.Define("delattr", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, fmt.Errorf("delattr expects 2 arguments, got %d", len(args))
		}

		_, ok := args[1].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("delattr attribute name must be a string")
		}

		// For now, we don't support attribute deletion
		desc := core.GetTypeDescriptorForValue(args[0])
		typeName := "object"
		if desc != nil {
			typeName = desc.PythonName
		}
		return nil, fmt.Errorf("'%s' object does not support attribute deletion", typeName)
	}))
}
