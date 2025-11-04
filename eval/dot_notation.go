package eval

import (
	"fmt"
	"strconv"

	"github.com/mmichie/m28/core"
)

// DotForm implements the dot notation special form
// (. obj property) -> property access
// (. obj method arg1 arg2...) -> method call
func DotForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() < 2 {
		return nil, fmt.Errorf("dot notation requires at least 2 arguments, got %d", args.Len())
	}

	// Evaluate the object
	obj, err := Eval(args.Items()[0], ctx)
	if err != nil {
		return nil, fmt.Errorf("error evaluating object: %v", err)
	}

	// Get the property name
	propName, ok := args.Items()[1].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("property name must be a string, got %T", args.Items()[1])
	}

	// Check if it's a numeric index
	if idx, err := strconv.Atoi(string(propName)); err == nil {
		// Numeric index access
		return getByIndex(obj, idx)
	}

	// Check if object supports GetAttr (either full Object interface or just GetAttr)
	var value core.Value
	var found bool

	if objWithAttrs, ok := obj.(core.Object); ok {
		// Full Object interface
		value, found = objWithAttrs.GetAttr(string(propName))
	} else if objWithGetAttr, ok := obj.(interface {
		GetAttr(string) (core.Value, bool)
	}); ok {
		// Just GetAttr method
		value, found = objWithGetAttr.GetAttr(string(propName))
	}

	if found {
		// Handle property descriptor protocol
		if prop, ok := value.(*core.PropertyValue); ok {
			// It's a property - call the getter
			if prop.Getter == nil {
				return nil, fmt.Errorf("unreadable attribute '%s'", string(propName))
			}

			// Call the getter with the object as self
			if getter, ok := prop.Getter.(interface {
				Call([]core.Value, *core.Context) (core.Value, error)
			}); ok {
				return getter.Call([]core.Value{obj}, ctx)
			}
			return nil, fmt.Errorf("property getter is not callable")
		}

		// Handle generic descriptor protocol (e.g., TupleGetter)
		// Check if value has __get__ method
		// BUT: Don't invoke __get__ if:
		// 1. The value is a Class - classes themselves are not descriptors when accessed from modules
		// 2. The value is a function in an instance's __dict__ - these should not be bound as methods
		//    (Python only binds functions found in the class, not in instance __dict__)
		if _, isClass := value.(*core.Class); !isClass {
			// Check if this is a function stored in instance __dict__
			// Functions in instance __dict__ should NOT be bound as methods
			skipDescriptor := false
			if inst, isInst := obj.(*core.Instance); isInst {
				// Check if the attribute is in the instance's __dict__
				if _, inInstanceDict := inst.Attributes[string(propName)]; inInstanceDict {
					// It's from instance __dict__ - check if it's a function
					if _, isFunc := value.(*UserFunction); isFunc {
						skipDescriptor = true
					} else if _, isGenFunc := value.(*core.GeneratorFunction); isGenFunc {
						skipDescriptor = true
					}
				}
			}

			if !skipDescriptor {
				if valueWithGetAttr, ok := value.(interface {
					GetAttr(string) (core.Value, bool)
				}); ok {
					if getMethod, hasGet := valueWithGetAttr.GetAttr("__get__"); hasGet {
						// It's a descriptor - call __get__(self, instance, owner)
						if getter, ok := getMethod.(interface {
							Call([]core.Value, *core.Context) (core.Value, error)
						}); ok {
							// Get the owner class if possible
							var owner core.Value = core.None
							if objWithClass, ok := obj.(interface {
								GetAttr(string) (core.Value, bool)
							}); ok {
								if classVal, ok := objWithClass.GetAttr("__class__"); ok {
									owner = classVal
								}
							}
							// Call __get__(instance, owner)
							// Note: getter is already a bound method (bound to the descriptor instance),
							// so we don't pass the descriptor as the first arg
							descriptorResult, err := getter.Call([]core.Value{obj, owner}, ctx)
							if err != nil {
								return nil, err
							}

							// IMPORTANT: If there are more arguments, this is a method call
							// The descriptor returned a bound method, now we need to call it
							if args.Len() > 2 {
								// Check if it's just the __call__ marker (method with no args)
								hasCallMarker := false
								if args.Len() == 3 {
									if sym, ok := args.Items()[2].(core.SymbolValue); ok && string(sym) == "__call__" {
										hasCallMarker = true
									}
								}

								// Call the bound method
								if callable, ok := descriptorResult.(interface {
									Call([]core.Value, *core.Context) (core.Value, error)
								}); ok {
									// Evaluate the arguments (skip __call__ marker if present)
									var evalArgs []core.Value
									if hasCallMarker {
										evalArgs = []core.Value{} // No args
									} else {
										evalArgs = make([]core.Value, args.Len()-2)
										for i, arg := range args.Items()[2:] {
											evalArgs[i], err = Eval(arg, ctx)
											if err != nil {
												// 												fmt.Printf("[DEBUG DotForm Descriptor] Error evaluating argument %d for method %s\n", i+1, propName)
												// 												fmt.Printf("[DEBUG DotForm Descriptor]   obj = %T\n", obj)
												// 												fmt.Printf("[DEBUG DotForm Descriptor]   arg = %v\n", core.PrintValue(arg))
												return nil, fmt.Errorf("error evaluating argument %d: %v", i+1, err)
											}
										}
									}
									return callable.Call(evalArgs, ctx)
								}
								return nil, fmt.Errorf("'%s' is not callable", string(propName))
							}

							return descriptorResult, nil
						}
					}
				}
			}
		}

		// Handle staticmethod - unwrap to the underlying function
		if sm, ok := value.(*core.StaticMethodValue); ok {
			value = sm.Function
		}

		// Handle classmethod - bind to the class
		if cm, ok := value.(*core.ClassMethodValue); ok {
			// Special case for __new__: return the underlying function without binding
			// This matches CPython behavior where Class.__new__ is an unbound method
			if string(propName) == "__new__" {
				// Check if we're accessing from a class (not an instance)
				if _, isClass := obj.(*core.Class); isClass {
					value = cm.Function
				} else if _, isTupleType := obj.(interface{ GetClass() *core.Class }); isTupleType {
					// Also handle TupleType, DictType, etc.
					value = cm.Function
				} else {
					// For instances, still create bound classmethod
					cls := obj
					if objWithClass, ok := obj.(interface {
						GetAttr(string) (core.Value, bool)
					}); ok {
						if classVal, found := objWithClass.GetAttr("__class__"); found {
							cls = classVal
						}
					}
					value = &core.BoundClassMethod{
						Class:    cls,
						Function: cm.Function,
					}
				}
			} else {
				// For other classmethods, create bound classmethod normally
				cls := obj
				if objWithClass, ok := obj.(interface {
					GetAttr(string) (core.Value, bool)
				}); ok {
					if classVal, found := objWithClass.GetAttr("__class__"); found {
						cls = classVal
					}
				}
				value = &core.BoundClassMethod{
					Class:    cls,
					Function: cm.Function,
				}
			}
		}

		// If the object is a Super, wrap the method to inject __class__ when called
		if superObj, ok := obj.(*core.Super); ok {
			if _, ok := value.(interface {
				Call([]core.Value, *core.Context) (core.Value, error)
			}); ok {
				value = &core.BoundSuperMethod{
					Method: value,
					Class:  superObj.Class,
				}
			}
		}

		// If there are more arguments (even if just __call__ marker), it's a method call
		if args.Len() > 2 {
			// Check if it's just the __call__ marker (method with no args)
			hasCallMarker := false
			if args.Len() == 3 {
				if sym, ok := args.Items()[2].(core.SymbolValue); ok && string(sym) == "__call__" {
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
						evalArgs = make([]core.Value, args.Len()-2)
						for i, arg := range args.Items()[2:] {
							evalArgs[i], err = Eval(arg, ctx)
							if err != nil {
								// 								fmt.Printf("[DEBUG DotForm BoundMethod] Error evaluating argument %d for method %s\n", i+1, propName)
								// 								fmt.Printf("[DEBUG DotForm BoundMethod]   obj = %T\n", obj)
								// 								fmt.Printf("[DEBUG DotForm BoundMethod]   arg = %v\n", core.PrintValue(arg))
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
				evalArgs = make([]core.Value, args.Len()-2)
				for i, arg := range args.Items()[2:] {
					evalArgs[i], err = Eval(arg, ctx)
					if err != nil {
						// 						fmt.Printf("[DEBUG DotForm] Error evaluating argument %d for method %s\n", i+1, propName)
						// 						fmt.Printf("[DEBUG DotForm]   obj = %T\n", obj)
						// 						fmt.Printf("[DEBUG DotForm]   arg = %v\n", core.PrintValue(arg))
						return nil, fmt.Errorf("error evaluating argument %d: %v", i+1, err)
					}
				}
			}

			// If calling a method on a class or super object, inject __class__ into the context
			// so that super() knows which class's method is being executed
			callCtx := ctx
			if class, ok := obj.(*core.Class); ok {
				// Get the class where this method is defined
				if _, definingClass, found := class.GetMethodWithClass(string(propName)); found && definingClass != nil {
					// Create a new context with __class__ set to the defining class
					callCtx = core.NewContext(ctx)
					callCtx.Define("__class__", definingClass)
				}
			} else if superObj, ok := obj.(*core.Super); ok {
				// When calling a method through super(), set __class__ to the super's class
				// (which is the parent class where the method is defined)
				callCtx = core.NewContext(ctx)
				callCtx.Define("__class__", superObj.Class)
			}

			return method.Call(evalArgs, callCtx)
		}

		// Just property access
		return value, nil
	}

	if !found {
		return nil, &core.AttributeError{
			ObjType:  string(obj.Type()),
			AttrName: string(propName),
		}
	}

	// Special handling for basic types that don't implement Object
	// Check if it's a method call (has args or __call__ marker)
	isMethodCall := args.Len() > 2
	methodArgs := args.Items()[2:]

	// Check for __call__ marker
	if isMethodCall && args.Len() == 3 {
		if sym, ok := args.Items()[2].(core.SymbolValue); ok && string(sym) == "__call__" {
			// It's a method call with no args
			methodArgs = []core.Value{}
		}
	}

	switch v := obj.(type) {
	case *core.ListValue:
		return getListAttr(v, string(propName), isMethodCall, core.NewList(methodArgs...), ctx)
	case core.StringValue:
		return getStringAttr(v, string(propName), isMethodCall, core.NewList(methodArgs...), ctx)
	case *core.DictValue:
		return getDictAttr(v, string(propName), isMethodCall, core.NewList(methodArgs...), ctx)
	default:
		return nil, fmt.Errorf("%s does not support attribute access", obj.Type())
	}
}

// getByIndex gets a value by numeric index
func getByIndex(obj core.Value, idx int) (core.Value, error) {
	switch v := obj.(type) {
	case *core.ListValue:
		if idx < 0 {
			idx = v.Len() + idx
		}
		if idx < 0 || idx >= v.Len() {
			return nil, &core.IndexError{Index: idx, Length: v.Len()}
		}
		return v.Items()[idx], nil

	case core.StringValue:
		s := string(v)
		if idx < 0 {
			idx = len(s) + idx
		}
		if idx < 0 || idx >= len(s) {
			return nil, &core.IndexError{Index: idx, Length: len(s)}
		}
		return core.StringValue(s[idx : idx+1]), nil

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
func getListAttr(lst *core.ListValue, attr string, isCall bool, args *core.ListValue, ctx *core.Context) (core.Value, error) {
	// Check for list methods
	switch attr {
	case "append":
		if !isCall {
			return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("append() takes exactly one argument")
				}
				lst.Append(args[0])
				return core.Nil, nil
			}), nil
		}
		// Direct call
		if args.Len() != 1 {
			return nil, fmt.Errorf("append() takes exactly one argument")
		}
		val, err := Eval(args.Items()[0], ctx)
		if err != nil {
			return nil, err
		}
		lst.Append(val)
		return core.Nil, nil

	case "length", "len":
		return core.NumberValue(lst.Len()), nil

	default:
		return nil, &core.AttributeError{
			ObjType:  "list",
			AttrName: attr,
		}
	}
}

// getStringAttr handles attribute access for strings
func getStringAttr(str core.StringValue, attr string, isCall bool, args *core.ListValue, ctx *core.Context) (core.Value, error) {
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
				evalArgs := make([]core.Value, args.Len())
				for i, arg := range args.Items() {
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
		return nil, &core.AttributeError{
			ObjType:  "string",
			AttrName: attr,
		}
	}
}

// getDictAttr handles attribute access for dicts
func getDictAttr(dict *core.DictValue, attr string, isCall bool, args *core.ListValue, ctx *core.Context) (core.Value, error) {
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
				evalArgs := make([]core.Value, args.Len())
				for i, arg := range args.Items() {
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

	return nil, &core.AttributeError{
		ObjType:  "dict",
		AttrName: attr,
	}
}

// RegisterDotNotation registers the dot notation special form
func RegisterDotNotation() {
	RegisterSpecialForm(".", DotForm)
}
