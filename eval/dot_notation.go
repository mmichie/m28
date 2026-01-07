package eval

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/mmichie/m28/common/suggestions"
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

	// Get the property name - can be a string or symbol
	propVal := unwrapLocated(args.Items()[1])
	var propName core.StringValue
	switch v := propVal.(type) {
	case core.StringValue:
		propName = v
	case core.SymbolValue:
		// Convert symbol to string for property access
		propName = core.StringValue(v)
	default:
		return nil, fmt.Errorf("property name must be a string or symbol, got %T", propVal)
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
									unwrapped := unwrapLocated(args.Items()[2])
									if str, ok := unwrapped.(core.StringValue); ok && string(str) == "__call__" {
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
		// BUT skip wrapping if Super.GetAttr already returned a properly wrapped method
		if superObj, ok := obj.(*core.Super); ok {
			// Don't double-wrap - Super.GetAttr.bindMethod already returns
			// BoundSuperMethod for instance methods and BoundClassMethod for class methods
			if _, isBoundSuperMethod := value.(*core.BoundSuperMethod); !isBoundSuperMethod {
				if _, isBoundClassMethod := value.(*core.BoundClassMethod); !isBoundClassMethod {
					if _, ok := value.(interface {
						Call([]core.Value, *core.Context) (core.Value, error)
					}); ok {
						value = &core.BoundSuperMethod{
							Method: value,
							Class:  superObj.Class,
						}
					}
				}
			}
		}

		// If there are more arguments (even if just __call__ marker), it's a method call
		if args.Len() > 2 {
			// Check if it's just the __call__ marker (method with no args)
			hasCallMarker := false
			if args.Len() == 3 {
				unwrapped := unwrapLocated(args.Items()[2])
				if str, ok := unwrapped.(core.StringValue); ok && string(str) == "__call__" {
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

	// Special handling for basic types that don't implement Object (or implement it partially)
	// These type-specific handlers are tried when GetAttr didn't find the attribute
	if !found {
		// Check if it's a method call (has args or __call__ marker)
		isMethodCall := args.Len() > 2
		methodArgs := args.Items()[2:]

		// Check for __call__ marker
		if isMethodCall && args.Len() == 3 {
			unwrapped := unwrapLocated(args.Items()[2])
			if str, ok := unwrapped.(core.StringValue); ok && string(str) == "__call__" {
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
		case *core.SetValue:
			return getSetAttr(v, string(propName), isMethodCall, core.NewList(methodArgs...), ctx)
		case core.TupleValue:
			return getTupleAttr(v, string(propName), isMethodCall, core.NewList(methodArgs...), ctx)
		default:
			// Generate suggestion based on available attributes
			availableAttrs := getAvailableAttributes(obj)
			suggestion := generateAttributeSuggestion(string(propName), availableAttrs)
			return nil, &core.AttributeError{
				ObjType:    string(obj.Type()),
				AttrName:   string(propName),
				Suggestion: suggestion,
			}
		}
	}

	// This should not be reached - found was true and should have been handled above
	return nil, fmt.Errorf("unexpected state: attribute '%s' found but not handled", string(propName))
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
	// Get the type descriptor for list
	td := core.GetTypeDescriptor("list")

	// Handle special M28 type protocol attributes that auto-call
	// These are accessed as properties but behave as method calls
	switch attr {
	case "length", "len":
		return core.NumberValue(lst.Len()), nil

	case "pop", "reverse", "copy":
		// These methods auto-call when accessed
		// (. list pop) calls pop() with no args
		// (. list pop 0) calls pop(0)
		// (. list reverse) calls reverse()
		// (. list copy) calls copy()
		// NOTE: sort is excluded to support kwargs (key=, reverse=)
		if td != nil {
			if method, ok := td.Methods[attr]; ok && method.Handler != nil {
				evalArgs := make([]core.Value, args.Len())
				for i, arg := range args.Items() {
					var err error
					evalArgs[i], err = Eval(arg, ctx)
					if err != nil {
						return nil, err
					}
				}
				return method.Handler(lst, evalArgs, ctx)
			}
		}
		return nil, &core.AttributeError{ObjType: "list", AttrName: attr}
	}

	// Check type descriptor for other methods
	if td != nil {
		if method, ok := td.Methods[attr]; ok {
			if !isCall {
				// Return bound method
				return &core.BoundMethod{
					Receiver: lst,
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
				return method.Handler(lst, evalArgs, ctx)
			}
		}
	}

	return nil, &core.AttributeError{
		ObjType:  "list",
		AttrName: attr,
	}
}

// getStringAttr handles attribute access for strings
func getStringAttr(str core.StringValue, attr string, isCall bool, args *core.ListValue, ctx *core.Context) (core.Value, error) {
	// Get the type descriptor for string
	td := core.GetTypeDescriptor("string")

	// Handle special M28 type protocol attributes that auto-call
	// These are accessed as properties but behave as method calls
	switch attr {
	case "length", "len":
		return core.NumberValue(len(string(str))), nil

	case "upper":
		return core.StringValue(strings.ToUpper(string(str))), nil

	case "lower":
		return core.StringValue(strings.ToLower(string(str))), nil

	case "strip":
		return core.StringValue(strings.TrimSpace(string(str))), nil

	case "lstrip":
		return core.StringValue(strings.TrimLeft(string(str), " \t\n\r")), nil

	case "rstrip":
		return core.StringValue(strings.TrimRight(string(str), " \t\n\r")), nil

	case "capitalize":
		s := string(str)
		if len(s) == 0 {
			return str, nil
		}
		return core.StringValue(strings.ToUpper(s[:1]) + strings.ToLower(s[1:])), nil

	case "title":
		return core.StringValue(strings.Title(string(str))), nil

	case "isdigit":
		s := string(str)
		if len(s) == 0 {
			return core.BoolValue(false), nil
		}
		for _, r := range s {
			if r < '0' || r > '9' {
				return core.BoolValue(false), nil
			}
		}
		return core.BoolValue(true), nil

	case "isalpha":
		s := string(str)
		if len(s) == 0 {
			return core.BoolValue(false), nil
		}
		for _, r := range s {
			if !((r >= 'a' && r <= 'z') || (r >= 'A' && r <= 'Z')) {
				return core.BoolValue(false), nil
			}
		}
		return core.BoolValue(true), nil

	case "isspace":
		s := string(str)
		if len(s) == 0 {
			return core.BoolValue(false), nil
		}
		for _, r := range s {
			if r != ' ' && r != '\t' && r != '\n' && r != '\r' {
				return core.BoolValue(false), nil
			}
		}
		return core.BoolValue(true), nil

	case "contains":
		// Need an argument for contains
		if td != nil {
			if method, ok := td.Methods["contains"]; ok && method.Handler != nil {
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
		// Fallback: implement directly
		if args.Len() < 1 {
			return nil, fmt.Errorf("contains() requires an argument")
		}
		subVal, err := Eval(args.Items()[0], ctx)
		if err != nil {
			return nil, err
		}
		subStr, ok := subVal.(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("contains() argument must be a string")
		}
		return core.BoolValue(strings.Contains(string(str), string(subStr))), nil
	}

	// Check type descriptor for other methods
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

	return nil, &core.AttributeError{
		ObjType:  "string",
		AttrName: attr,
	}
}

// getDictAttr handles attribute access for dicts
func getDictAttr(dict *core.DictValue, attr string, isCall bool, args *core.ListValue, ctx *core.Context) (core.Value, error) {
	// Get the type descriptor for dict
	td := core.GetTypeDescriptor("dict")

	// Handle special M28 type protocol attributes that auto-call
	switch attr {
	case "length", "len":
		return core.NumberValue(dict.Size()), nil

	case "contains":
		// Need an argument for contains
		if args.Len() < 1 {
			return nil, fmt.Errorf("contains() requires an argument")
		}
		keyVal, err := Eval(args.Items()[0], ctx)
		if err != nil {
			return nil, err
		}
		_, exists := dict.GetValue(keyVal)
		return core.BoolValue(exists), nil

	case "keys":
		// Return list of keys
		origKeys := dict.OriginalKeys()
		result := core.NewList()
		for _, key := range origKeys {
			result.Append(key)
		}
		return result, nil

	case "values":
		// Return list of values
		result := core.NewList()
		for _, key := range dict.OriginalKeys() {
			if val, exists := dict.GetValue(key); exists {
				result.Append(val)
			}
		}
		return result, nil

	case "items":
		// Return list of [key, value] tuples
		result := core.NewList()
		for _, key := range dict.OriginalKeys() {
			if val, exists := dict.GetValue(key); exists {
				result.Append(core.TupleValue{key, val})
			}
		}
		return result, nil

	case "set":
		// Set a key-value pair
		if args.Len() < 2 {
			return nil, fmt.Errorf("set() requires key and value arguments")
		}
		keyVal, err := Eval(args.Items()[0], ctx)
		if err != nil {
			return nil, err
		}
		valVal, err := Eval(args.Items()[1], ctx)
		if err != nil {
			return nil, err
		}
		dict.SetValue(keyVal, valVal)
		return core.Nil, nil

	case "delete":
		// Delete a key
		if args.Len() < 1 {
			return nil, fmt.Errorf("delete() requires a key argument")
		}
		keyVal, err := Eval(args.Items()[0], ctx)
		if err != nil {
			return nil, err
		}
		dict.DeleteValue(keyVal)
		return core.Nil, nil

	case "clear":
		// Clear all entries by deleting each key
		// Make a copy of keys to avoid modifying while iterating
		keys := make([]string, len(dict.Keys()))
		copy(keys, dict.Keys())
		for _, key := range keys {
			dict.Delete(key)
		}
		return core.Nil, nil

	case "update":
		// Update with another dict
		if args.Len() < 1 {
			return nil, fmt.Errorf("update() requires an argument")
		}
		otherArg, err := Eval(args.Items()[0], ctx)
		if err != nil {
			return nil, err
		}
		other, ok := otherArg.(*core.DictValue)
		if !ok {
			return nil, fmt.Errorf("update() argument must be a dict")
		}
		// Copy all entries from other to dict
		for _, key := range other.OriginalKeys() {
			if val, exists := other.GetValue(key); exists {
				dict.SetValue(key, val)
			}
		}
		return core.Nil, nil
	}

	// First, check if it's a dictionary key access
	if val, exists := dict.Get(attr); exists {
		return val, nil
	}

	// Otherwise, check for dict methods
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

	// Generate suggestion based on available attributes
	availableAttrs := getAvailableAttributes(dict)
	suggestion := generateAttributeSuggestion(attr, availableAttrs)
	return nil, &core.AttributeError{
		ObjType:    "dict",
		AttrName:   attr,
		Suggestion: suggestion,
	}
}

// getSetAttr handles attribute access for sets
func getSetAttr(set *core.SetValue, attr string, isCall bool, args *core.ListValue, ctx *core.Context) (core.Value, error) {
	// Get the type descriptor for set
	td := core.GetTypeDescriptor("set")

	// Handle special M28 type protocol attributes that auto-call
	switch attr {
	case "length", "len":
		return core.NumberValue(set.Size()), nil

	case "contains":
		if args.Len() < 1 {
			return nil, fmt.Errorf("contains() requires an argument")
		}
		valArg, err := Eval(args.Items()[0], ctx)
		if err != nil {
			return nil, err
		}
		return core.BoolValue(set.Contains(valArg)), nil

	case "add":
		if args.Len() < 1 {
			return nil, fmt.Errorf("add() requires an argument")
		}
		valArg, err := Eval(args.Items()[0], ctx)
		if err != nil {
			return nil, err
		}
		set.Add(valArg)
		return core.Nil, nil

	case "remove":
		if args.Len() < 1 {
			return nil, fmt.Errorf("remove() requires an argument")
		}
		valArg, err := Eval(args.Items()[0], ctx)
		if err != nil {
			return nil, err
		}
		set.Remove(valArg)
		return core.Nil, nil

	case "union":
		if args.Len() < 1 {
			return nil, fmt.Errorf("union() requires an argument")
		}
		otherArg, err := Eval(args.Items()[0], ctx)
		if err != nil {
			return nil, err
		}
		other, ok := otherArg.(*core.SetValue)
		if !ok {
			return nil, fmt.Errorf("union() argument must be a set")
		}
		result := core.NewSet()
		for _, item := range set.Items() {
			result.Add(item)
		}
		for _, item := range other.Items() {
			result.Add(item)
		}
		return result, nil

	case "intersection":
		if args.Len() < 1 {
			return nil, fmt.Errorf("intersection() requires an argument")
		}
		otherArg, err := Eval(args.Items()[0], ctx)
		if err != nil {
			return nil, err
		}
		other, ok := otherArg.(*core.SetValue)
		if !ok {
			return nil, fmt.Errorf("intersection() argument must be a set")
		}
		result := core.NewSet()
		for _, item := range set.Items() {
			if other.Contains(item) {
				result.Add(item)
			}
		}
		return result, nil

	case "difference":
		if args.Len() < 1 {
			return nil, fmt.Errorf("difference() requires an argument")
		}
		otherArg, err := Eval(args.Items()[0], ctx)
		if err != nil {
			return nil, err
		}
		result := core.NewSet()
		for _, item := range set.Items() {
			result.Add(item)
		}
		// Check if it's a set (fast path)
		if other, ok := otherArg.(*core.SetValue); ok {
			for _, item := range other.Items() {
				result.Remove(item)
			}
			return result, nil
		}
		// Check if it's an iterable
		if iterable, ok := otherArg.(core.Iterable); ok {
			iter := iterable.Iterator()
			for {
				val, hasNext := iter.Next()
				if !hasNext {
					break
				}
				result.Remove(val)
			}
			return result, nil
		}
		return nil, fmt.Errorf("difference() argument must be an iterable, not %s", otherArg.Type())
	}

	// Check type descriptor for other methods
	if td != nil {
		if method, ok := td.Methods[attr]; ok {
			if !isCall {
				return &core.BoundMethod{
					Receiver: set,
					Method:   method,
					TypeDesc: td,
				}, nil
			}
			if method.Handler != nil {
				evalArgs := make([]core.Value, args.Len())
				for i, arg := range args.Items() {
					var err error
					evalArgs[i], err = Eval(arg, ctx)
					if err != nil {
						return nil, err
					}
				}
				return method.Handler(set, evalArgs, ctx)
			}
		}
	}

	return nil, &core.AttributeError{
		ObjType:  "set",
		AttrName: attr,
	}
}

// getTupleAttr handles attribute access for tuples
func getTupleAttr(tuple core.TupleValue, attr string, isCall bool, args *core.ListValue, ctx *core.Context) (core.Value, error) {
	// Get the type descriptor for tuple
	td := core.GetTypeDescriptor("tuple")

	// Handle special M28 type protocol attributes that auto-call
	switch attr {
	case "length", "len":
		return core.NumberValue(len(tuple)), nil

	case "get":
		if args.Len() < 1 {
			return nil, fmt.Errorf("get() requires an index argument")
		}
		idxArg, err := Eval(args.Items()[0], ctx)
		if err != nil {
			return nil, err
		}
		idx, ok := idxArg.(core.NumberValue)
		if !ok {
			return nil, fmt.Errorf("tuple indices must be integers")
		}
		index := int(idx)
		if index < 0 {
			index = len(tuple) + index
		}
		if index < 0 || index >= len(tuple) {
			return nil, &core.IndexError{Index: int(idx), Length: len(tuple)}
		}
		return tuple[index], nil

	case "contains":
		if args.Len() < 1 {
			return nil, fmt.Errorf("contains() requires an argument")
		}
		valArg, err := Eval(args.Items()[0], ctx)
		if err != nil {
			return nil, err
		}
		for _, item := range tuple {
			if core.EqualValues(item, valArg) {
				return core.BoolValue(true), nil
			}
		}
		return core.BoolValue(false), nil

	case "tolist":
		result := core.NewList()
		for _, item := range tuple {
			result.Append(item)
		}
		return result, nil
	}

	// Check type descriptor for other methods
	if td != nil {
		if method, ok := td.Methods[attr]; ok {
			if !isCall {
				return &core.BoundMethod{
					Receiver: tuple,
					Method:   method,
					TypeDesc: td,
				}, nil
			}
			if method.Handler != nil {
				evalArgs := make([]core.Value, args.Len())
				for i, arg := range args.Items() {
					var err error
					evalArgs[i], err = Eval(arg, ctx)
					if err != nil {
						return nil, err
					}
				}
				return method.Handler(tuple, evalArgs, ctx)
			}
		}
	}

	return nil, &core.AttributeError{
		ObjType:  "tuple",
		AttrName: attr,
	}
}

// getAvailableAttributes returns all available attributes for an object
func getAvailableAttributes(obj core.Value) []string {
	attrs := make(map[string]bool)

	// Try to get attributes from object protocol
	if objWithAttrs, ok := obj.(interface {
		GetAttr(string) (core.Value, bool)
	}); ok {
		// For classes, get all methods and attributes
		if class, ok := obj.(*core.Class); ok {
			// Get instance methods
			for name := range class.Methods {
				attrs[name] = true
			}
			// Get class attributes
			for name := range class.Attributes {
				attrs[name] = true
			}
		}
		// Try common Python attributes
		commonAttrs := []string{
			"__str__", "__repr__", "__dict__", "__class__", "__name__",
			"__module__", "__doc__", "__init__", "__call__", "__len__",
			"__getitem__", "__setitem__", "__contains__", "__iter__",
			"append", "extend", "pop", "remove", "clear", "count", "index",
			"keys", "values", "items", "get", "update", "split", "join",
			"strip", "replace", "format", "upper", "lower",
		}
		for _, attr := range commonAttrs {
			if _, found := objWithAttrs.GetAttr(attr); found {
				attrs[attr] = true
			}
		}
	}

	// For type descriptors, get type-specific methods
	if td := core.GetTypeDescriptorForValue(obj); td != nil {
		for name := range td.Methods {
			attrs[name] = true
		}
	}

	// Convert to slice
	result := make([]string, 0, len(attrs))
	for attr := range attrs {
		result = append(result, attr)
	}

	return result
}

// generateAttributeSuggestion generates a "did you mean" suggestion for an attribute
func generateAttributeSuggestion(target string, availableAttrs []string) string {
	// Find similar attributes within edit distance 2, suggest up to 3
	similar := suggestions.FindSimilarNames(target, availableAttrs, 2, 3)
	return suggestions.FormatSuggestion(similar)
}

// RegisterDotNotation registers the dot notation special form
func RegisterDotNotation() {
	RegisterSpecialForm(".", DotForm)
}
