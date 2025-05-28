package eval

import (
	"fmt"
	"strconv"
	
	"github.com/mmichie/m28/core"
)

// DotForm implements the dot notation special form
// (. obj property) -> property access
// (. obj method arg1 arg2...) -> method call
func DotForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("dot notation requires at least 2 arguments, got %d", len(args))
	}
	
	
	// Evaluate the object
	obj, err := Eval(args[0], ctx)
	if err != nil {
		return nil, fmt.Errorf("error evaluating object: %v", err)
	}
	
	// Get the property name
	propName, ok := args[1].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("property name must be a string, got %T", args[1])
	}
	
	// Check if it's a numeric index
	if idx, err := strconv.Atoi(string(propName)); err == nil {
		// Numeric index access
		return getByIndex(obj, idx)
	}
	
	// Check if object supports GetAttr
	if objWithAttrs, ok := obj.(core.Object); ok {
		// Get the property/method
		value, found := objWithAttrs.GetAttr(string(propName))
		if !found {
			return nil, fmt.Errorf("%s has no attribute '%s'", obj.Type(), string(propName))
		}
		
		// If there are more arguments (even if just __call__ marker), it's a method call
		if len(args) > 2 {
			// Check if it's just the __call__ marker (method with no args)
			hasCallMarker := false
			if len(args) == 3 {
				if sym, ok := args[2].(core.SymbolValue); ok && string(sym) == "__call__" {
					hasCallMarker = true
				}
			}
			
			// Method call
			method, ok := value.(interface {
				Call([]core.Value, *core.Context) (core.Value, error)
			})
			if !ok {
				// Check if it's a bound method
				if bm, ok := value.(*core.BoundMethod); ok {
					// Evaluate the arguments (skip __call__ marker if present)
					var evalArgs []core.Value
					if hasCallMarker {
						evalArgs = []core.Value{} // No args
					} else {
						evalArgs = make([]core.Value, len(args)-2)
						for i, arg := range args[2:] {
							evalArgs[i], err = Eval(arg, ctx)
							if err != nil {
								return nil, fmt.Errorf("error evaluating argument %d: %v", i+1, err)
							}
						}
					}
					return bm.Call(evalArgs, ctx)
				}
				return nil, fmt.Errorf("'%s' is not callable", string(propName))
			}
			
			// Evaluate the arguments (skip __call__ marker if present)
			var evalArgs []core.Value
			if hasCallMarker {
				evalArgs = []core.Value{} // No args
			} else {
				evalArgs = make([]core.Value, len(args)-2)
				for i, arg := range args[2:] {
					evalArgs[i], err = Eval(arg, ctx)
					if err != nil {
						return nil, fmt.Errorf("error evaluating argument %d: %v", i+1, err)
					}
				}
			}
			
			return method.Call(evalArgs, ctx)
		}
		
		// Just property access
		return value, nil
	}
	
	// Special handling for basic types that don't implement Object
	// Check if it's a method call (has args or __call__ marker)
	isMethodCall := len(args) > 2
	methodArgs := args[2:]
	
	// Check for __call__ marker
	if isMethodCall && len(args) == 3 {
		if sym, ok := args[2].(core.SymbolValue); ok && string(sym) == "__call__" {
			// It's a method call with no args
			methodArgs = core.ListValue{}
		}
	}
	
	switch v := obj.(type) {
	case core.ListValue:
		return getListAttr(v, string(propName), isMethodCall, methodArgs, ctx)
	case core.StringValue:
		return getStringAttr(v, string(propName), isMethodCall, methodArgs, ctx)
	case *core.DictValue:
		return getDictAttr(v, string(propName), isMethodCall, methodArgs, ctx)
	default:
		return nil, fmt.Errorf("%s does not support attribute access", obj.Type())
	}
}

// getByIndex gets a value by numeric index
func getByIndex(obj core.Value, idx int) (core.Value, error) {
	switch v := obj.(type) {
	case core.ListValue:
		if idx < 0 {
			idx = len(v) + idx
		}
		if idx < 0 || idx >= len(v) {
			return nil, &core.IndexError{Index: idx, Length: len(v)}
		}
		return v[idx], nil
		
	case core.StringValue:
		s := string(v)
		if idx < 0 {
			idx = len(s) + idx
		}
		if idx < 0 || idx >= len(s) {
			return nil, &core.IndexError{Index: idx, Length: len(s)}
		}
		return core.StringValue(s[idx:idx+1]), nil
		
	case core.TupleValue:
		if idx < 0 {
			idx = len(v) + idx
		}
		if idx < 0 || idx >= len(v) {
			return nil, &core.IndexError{Index: idx, Length: len(v)}
		}
		return v[idx], nil
		
	default:
		return nil, fmt.Errorf("%s does not support index access", obj.Type())
	}
}

// getListAttr handles attribute access for lists
func getListAttr(lst core.ListValue, attr string, isCall bool, args core.ListValue, ctx *core.Context) (core.Value, error) {
	// Check for list methods
	switch attr {
	case "append":
		if !isCall {
			return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("append() takes exactly one argument")
				}
				return append(lst, args[0]), nil
			}), nil
		}
		// Direct call
		if len(args) != 1 {
			return nil, fmt.Errorf("append() takes exactly one argument")
		}
		val, err := Eval(args[0], ctx)
		if err != nil {
			return nil, err
		}
		return append(lst, val), nil
		
	case "length", "len":
		return core.NumberValue(len(lst)), nil
		
	default:
		return nil, fmt.Errorf("list has no attribute '%s'", attr)
	}
}

// getStringAttr handles attribute access for strings
func getStringAttr(str core.StringValue, attr string, isCall bool, args core.ListValue, ctx *core.Context) (core.Value, error) {
	// Get the type descriptor for string
	td := core.GetTypeDescriptor("string")
	if td != nil {
		if method, ok := td.Methods[attr]; ok {
			if !isCall {
				// Return bound method
				return &core.BoundMethod{
					Receiver: str,
					Method:   method,
					TypeDesc: td,
				}, nil
			}
			// Direct call
			if method.Handler != nil {
				evalArgs := make([]core.Value, len(args))
				for i, arg := range args {
					var err error
					evalArgs[i], err = Eval(arg, ctx)
					if err != nil {
						return nil, err
					}
				}
				return method.Handler(str, evalArgs, ctx)
			}
		}
	}
	
	// Fallback for basic attributes
	switch attr {
	case "length", "len":
		return core.NumberValue(len(string(str))), nil
	default:
		return nil, fmt.Errorf("string has no attribute '%s'", attr)
	}
}

// getDictAttr handles attribute access for dicts
func getDictAttr(dict *core.DictValue, attr string, isCall bool, args core.ListValue, ctx *core.Context) (core.Value, error) {
	// First, check if it's a dictionary key access
	if val, exists := dict.Get(attr); exists {
		return val, nil
	}
	
	// Otherwise, check for dict methods
	td := core.GetTypeDescriptor("dict")
	if td != nil {
		if method, ok := td.Methods[attr]; ok {
			if !isCall {
				// Return bound method
				return &core.BoundMethod{
					Receiver: dict,
					Method:   method,
					TypeDesc: td,
				}, nil
			}
			// Direct call
			if method.Handler != nil {
				evalArgs := make([]core.Value, len(args))
				for i, arg := range args {
					var err error
					evalArgs[i], err = Eval(arg, ctx)
					if err != nil {
						return nil, err
					}
				}
				return method.Handler(dict, evalArgs, ctx)
			}
		}
	}
	
	return nil, fmt.Errorf("dict has no attribute '%s'", attr)
}

// RegisterDotNotation registers the dot notation special form
func RegisterDotNotation() {
	RegisterSpecialForm(".", DotForm)
}