package builtin

import (
	"fmt"
	"github.com/mmichie/m28/core"
)

// RegisterEssentialBuiltins registers additional essential built-in functions
func RegisterEssentialBuiltins(ctx *core.Context) {
	// raise is now a special form in evaluator.go to support more flexible syntax
	// Commented out to avoid conflict with the special form
	// ctx.Define("raise", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
	// 	...
	// }))

	// error() - alias for raise() for backward compatibility
	ctx.Define("error", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) == 0 {
			return nil, fmt.Errorf("RuntimeError")
		}
		if len(args) > 1 {
			return nil, fmt.Errorf("error() takes at most 1 argument (%d given)", len(args))
		}

		// Get the error message
		var message string
		switch v := args[0].(type) {
		case core.StringValue:
			message = string(v)
		default:
			message = v.String()
		}

		// Return a generic error that will be caught by try/except
		return nil, fmt.Errorf(message)
	}))

	// all() - return True if all elements are true (or if iterable is empty)
	ctx.Define("all", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("all() takes exactly one argument (%d given)", len(args))
		}

		// First check if it implements Iterable interface
		if iterable, ok := args[0].(core.Iterable); ok {
			iter := iterable.Iterator()
			for {
				item, hasNext := iter.Next()
				if !hasNext {
					break
				}
				if !core.IsTruthy(item) {
					return core.False, nil
				}
			}
			return core.True, nil
		}

		// Handle specific types that might not pass the interface check
		switch v := args[0].(type) {
		case *core.ListValue:
			for _, item := range v.Items() {
				if !core.IsTruthy(item) {
					return core.False, nil
				}
			}
			return core.True, nil
		case core.TupleValue:
			for _, item := range v {
				if !core.IsTruthy(item) {
					return core.False, nil
				}
			}
			return core.True, nil
		case *core.SetValue:
			// Use the Iterator method we added
			iter := v.Iterator()
			for {
				item, hasNext := iter.Next()
				if !hasNext {
					break
				}
				if !core.IsTruthy(item) {
					return core.False, nil
				}
			}
			return core.True, nil
		default:
			return nil, fmt.Errorf("all() argument must be an iterable, got type %T", args[0])
		}
	}))

	// any() - return True if any element is true (False if iterable is empty)
	ctx.Define("any", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("any() takes exactly one argument (%d given)", len(args))
		}

		// First check if it implements Iterable interface
		if iterable, ok := args[0].(core.Iterable); ok {
			iter := iterable.Iterator()
			for {
				item, hasNext := iter.Next()
				if !hasNext {
					break
				}
				if core.IsTruthy(item) {
					return core.True, nil
				}
			}
			return core.False, nil
		}

		// Handle specific types that might not pass the interface check
		switch v := args[0].(type) {
		case *core.ListValue:
			for _, item := range v.Items() {
				if core.IsTruthy(item) {
					return core.True, nil
				}
			}
			return core.False, nil
		case core.TupleValue:
			for _, item := range v {
				if core.IsTruthy(item) {
					return core.True, nil
				}
			}
			return core.False, nil
		case *core.SetValue:
			// Use the Iterator method we added
			iter := v.Iterator()
			for {
				item, hasNext := iter.Next()
				if !hasNext {
					break
				}
				if core.IsTruthy(item) {
					return core.True, nil
				}
			}
			return core.False, nil
		default:
			return nil, fmt.Errorf("any() argument must be an iterable, got type %T", args[0])
		}
	}))

	// round() - now registered in numeric.go

	// divmod() - now registered in numeric.go

	// hasattr() - check if object has attribute
	ctx.Define("hasattr", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, fmt.Errorf("hasattr() takes exactly 2 arguments (%d given)", len(args))
		}

		obj := args[0]
		nameVal, ok := args[1].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("hasattr() attribute name must be a string")
		}
		name := string(nameVal)

		// Check for the attribute
		if objWithAttrs, ok := obj.(interface {
			GetAttr(string) (core.Value, bool)
		}); ok {
			_, found := objWithAttrs.GetAttr(name)
			return core.BoolValue(found), nil
		}

		// Check type descriptor methods
		desc := core.GetTypeDescriptorForValue(obj)
		if desc != nil {
			if _, ok := desc.Methods[name]; ok {
				return core.True, nil
			}
			if _, ok := desc.Properties[name]; ok {
				return core.True, nil
			}
		}

		return core.False, nil
	}))

	// getattr() - get attribute from object
	ctx.Define("getattr", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 || len(args) > 3 {
			return nil, fmt.Errorf("getattr() takes 2 or 3 arguments (%d given)", len(args))
		}

		obj := args[0]
		nameVal, ok := args[1].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("getattr() attribute name must be a string")
		}
		name := string(nameVal)

		// Try __getattribute__ first (called for every attribute access)
		if o, ok := obj.(core.Object); ok {
			if method, exists := o.GetAttr("__getattribute__"); exists {
				if callable, ok := method.(core.Callable); ok {
					result, err := callable.Call([]core.Value{core.StringValue(name)}, ctx)
					if err == nil {
						return result, nil
					}
					// If __getattribute__ raises AttributeError, continue to normal lookup
					// For other errors, return them
					if !isAttributeError(err) {
						return nil, err
					}
				}
			}
		}

		// Try normal attribute lookup
		if objWithAttrs, ok := obj.(interface {
			GetAttr(string) (core.Value, bool)
		}); ok {
			if val, found := objWithAttrs.GetAttr(name); found {
				return val, nil
			}
		}

		// Check type descriptor methods
		desc := core.GetTypeDescriptorForValue(obj)
		if desc != nil {
			if method, ok := desc.Methods[name]; ok {
				// Create bound method
				return &core.BoundMethod{
					Receiver: obj,
					Method:   method,
					TypeDesc: desc,
				}, nil
			}
			if prop, ok := desc.Properties[name]; ok && prop.Getter != nil {
				return prop.Getter(obj)
			}
		}

		// Try __getattr__ as fallback when attribute not found
		if o, ok := obj.(core.Object); ok {
			if method, exists := o.GetAttr("__getattr__"); exists {
				if callable, ok := method.(core.Callable); ok {
					result, err := callable.Call([]core.Value{core.StringValue(name)}, ctx)
					if err == nil {
						return result, nil
					}
					// If __getattr__ also fails, continue to default/error
				}
			}
		}

		// Return default if provided
		if len(args) == 3 {
			return args[2], nil
		}

		return nil, &core.AttributeError{
			ObjType:  string(obj.Type()),
			AttrName: name,
		}
	}))

	// setattr() - set attribute on object
	ctx.Define("setattr", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 3 {
			return nil, fmt.Errorf("setattr() takes exactly 3 arguments (%d given)", len(args))
		}

		obj := args[0]
		nameVal, ok := args[1].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("setattr() attribute name must be a string")
		}
		name := string(nameVal)
		value := args[2]

		// Try __setattr__ dunder method first
		if o, ok := obj.(core.Object); ok {
			if method, exists := o.GetAttr("__setattr__"); exists {
				if callable, ok := method.(core.Callable); ok {
					_, err := callable.Call([]core.Value{core.StringValue(name), value}, ctx)
					if err != nil {
						return nil, err
					}
					return core.None, nil
				}
			}
		}

		// Try normal attribute setting
		if objWithAttrs, ok := obj.(interface {
			SetAttr(string, core.Value) error
		}); ok {
			if err := objWithAttrs.SetAttr(name, value); err != nil {
				return nil, err
			}
			return core.None, nil
		}

		// Check if it's a read-only property
		desc := core.GetTypeDescriptorForValue(obj)
		if desc != nil {
			if prop, ok := desc.Properties[name]; ok && prop.ReadOnly {
				return nil, fmt.Errorf("can't set attribute '%s' of '%s' objects", name, obj.Type())
			}
		}

		return nil, fmt.Errorf("'%s' object has no attribute '%s'", obj.Type(), name)
	}))

	// callable() - check if object is callable
	// callable() - now registered in functional.go

	// __import__(name, globals=None, locals=None, fromlist=(), level=0)
	// This is the function that the import statement calls
	ctx.Define("__import__", core.NewBuiltinFunction(func(args []core.Value, innerCtx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("__import__() requires at least 1 argument (0 given)")
		}

		// Get module name
		nameVal, ok := args[0].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("__import__() argument 1 must be str, not %T", args[0])
		}
		moduleName := string(nameVal)

		// Special case: __main__ module
		// This represents the currently executing script
		if moduleName == "__main__" {
			// Return a mock __main__ module with basic attributes
			mainModule := core.NewDict()
			mainModule.Set("__name__", core.StringValue("__main__"))
			mainModule.Set("__file__", core.StringValue("<stdin>"))
			mainModule.Set("__package__", core.None)
			return mainModule, nil
		}

		// Get fromlist if provided (args[3])
		var fromlist []string
		if len(args) > 3 {
			switch fl := args[3].(type) {
			case *core.ListValue:
				for _, item := range fl.Items() {
					if strVal, ok := item.(core.StringValue); ok {
						fromlist = append(fromlist, string(strVal))
					}
				}
			case core.TupleValue:
				for _, item := range fl {
					if strVal, ok := item.(core.StringValue); ok {
						fromlist = append(fromlist, string(strVal))
					}
				}
			}
		}

		// Use the module loader to load the module
		// Get the global context (use ctx not innerCtx for module loading)
		globalCtx := ctx
		if globalCtx.Global != nil {
			globalCtx = globalCtx.Global
		}

		// Try to load the module
		loader := core.GetModuleLoader()
		module, err := loader.LoadModule(moduleName, globalCtx)
		if err != nil {
			return nil, fmt.Errorf("ImportError: %v", err)
		}

		// If fromlist is empty or module is a package, return the top-level module
		// Otherwise return the final module
		if len(fromlist) == 0 {
			// Return the top-level module (e.g., for "import os.path", return os)
			topLevel := moduleName
			for i, ch := range moduleName {
				if ch == '.' {
					topLevel = moduleName[:i]
					break
				}
			}
			if topLevel != moduleName {
				// Load and return the top-level
				topModule, err := loader.LoadModule(topLevel, globalCtx)
				if err != nil {
					return nil, fmt.Errorf("ImportError: %v", err)
				}
				return topModule, nil
			}
		}

		return module, nil
	}))
}

// isAttributeError checks if an error is an AttributeError
func isAttributeError(err error) bool {
	if err == nil {
		return false
	}
	msg := err.Error()
	return contains(msg, "attribute") || contains(msg, "AttributeError")
}

// contains checks if string s contains substr (case-insensitive helper)
func contains(s, substr string) bool {
	return len(s) >= len(substr) && (s == substr || findSubstring(s, substr))
}

func findSubstring(s, substr string) bool {
	for i := 0; i <= len(s)-len(substr); i++ {
		if s[i:i+len(substr)] == substr {
			return true
		}
	}
	return false
}
