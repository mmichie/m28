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

	// hasattr - check if object has attribute
	ctx.Define("hasattr", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, fmt.Errorf("hasattr expects 2 arguments, got %d", len(args))
		}

		attrName, ok := args[1].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("hasattr attribute name must be a string")
		}

		// Try to get the attribute
		if obj, ok := args[0].(interface {
			GetAttr(string) (core.Value, bool)
		}); ok {
			_, found := obj.GetAttr(string(attrName))
			return core.BoolValue(found), nil
		}

		return core.False, nil
	}))

	// getattr - get attribute from object
	ctx.Define("getattr", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 || len(args) > 3 {
			return nil, fmt.Errorf("getattr expects 2 or 3 arguments, got %d", len(args))
		}

		attrName, ok := args[1].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("getattr attribute name must be a string")
		}

		// Try to get the attribute
		if obj, ok := args[0].(interface {
			GetAttr(string) (core.Value, bool)
		}); ok {
			val, found := obj.GetAttr(string(attrName))
			if found {
				return val, nil
			}
		}

		// Return default if provided
		if len(args) == 3 {
			return args[2], nil
		}

		// Otherwise, attribute error
		desc := core.GetTypeDescriptorForValue(args[0])
		typeName := "object"
		if desc != nil {
			typeName = desc.PythonName
		}
		return nil, fmt.Errorf("'%s' object has no attribute '%s'", typeName, string(attrName))
	}))

	// setattr - set attribute on object
	ctx.Define("setattr", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 3 {
			return nil, fmt.Errorf("setattr expects 3 arguments, got %d", len(args))
		}

		attrName, ok := args[1].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("setattr attribute name must be a string")
		}

		// Try to set the attribute
		if obj, ok := args[0].(interface {
			SetAttr(string, core.Value) error
		}); ok {
			err := obj.SetAttr(string(attrName), args[2])
			if err != nil {
				return nil, err
			}
			return core.Nil, nil
		}

		// Object doesn't support attribute setting
		desc := core.GetTypeDescriptorForValue(args[0])
		typeName := "object"
		if desc != nil {
			typeName = desc.PythonName
		}
		return nil, fmt.Errorf("'%s' object attribute '%s' is read-only", typeName, string(attrName))
	}))

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
