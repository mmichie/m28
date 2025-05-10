package eval

import (
	"fmt"
	"strings"

	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/special_forms"
)

type Evaluator struct {
	specialForms map[core.LispSymbol]special_forms.SpecialFormFunc
	callStack    []core.TraceEntry // Track the current call stack
	currentFunc  string            // Track the current function name
}

func NewEvaluator() core.Evaluator {
	return &Evaluator{
		specialForms: special_forms.GetSpecialForms(),
		callStack:    make([]core.TraceEntry, 0),
		currentFunc:  "<toplevel>",
	}
}

// pushCallFrame adds a new call frame to the call stack
func (e *Evaluator) pushCallFrame(funcName string, location core.Location, statement string) {
	entry := core.TraceEntry{
		Function:  funcName,
		Location:  location,
		Statement: statement,
	}
	e.callStack = append(e.callStack, entry)
	e.currentFunc = funcName
}

// popCallFrame removes the last call frame from the call stack
func (e *Evaluator) popCallFrame() {
	if len(e.callStack) > 0 {
		e.callStack = e.callStack[:len(e.callStack)-1]
		if len(e.callStack) > 0 {
			e.currentFunc = e.callStack[len(e.callStack)-1].Function
		} else {
			e.currentFunc = "<toplevel>"
		}
	}
}

// getCurrentTraceback returns the current traceback
func (e *Evaluator) getCurrentTraceback() core.Traceback {
	// Create a copy of the call stack
	traceback := make(core.Traceback, len(e.callStack))
	copy(traceback, e.callStack)
	return traceback
}

// getLocationFromExpr extracts location information from an expression
func (e *Evaluator) getLocationFromExpr(expr core.LispValue) core.Location {
	if located, ok := expr.(core.LocatedValue); ok {
		return located.Location
	}
	// Default location if no source information is available
	return core.Location{
		Filename: "<unknown>",
		Line:     0,
		Column:   0,
	}
}

// unwrapLocatedValue extracts the value from a LocatedValue
func (e *Evaluator) unwrapLocatedValue(expr core.LispValue) core.LispValue {
	if located, ok := expr.(core.LocatedValue); ok {
		return located.Value
	}
	return expr
}

func (e *Evaluator) Eval(expr core.LispValue, env core.Environment) (core.LispValue, error) {
	// Extract location information if available
	location := e.getLocationFromExpr(expr)

	// Track current expression evaluation in call stack for top-level expressions
	// Use a generic name for expressions, we'll update it if it's a function call
	exprStr := "expression"
	if list, ok := expr.(core.LispList); ok && len(list) > 0 {
		// Unwrap first element if it's a LocatedValue to check for symbol
		firstElem := e.unwrapLocatedValue(list[0])
		if sym, ok := firstElem.(core.LispSymbol); ok {
			exprStr = string(sym)
		}
	} else if locatedList, ok := expr.(core.LocatedValue); ok {
		if list, ok := locatedList.Value.(core.LispList); ok && len(list) > 0 {
			firstElem := e.unwrapLocatedValue(list[0])
			if sym, ok := firstElem.(core.LispSymbol); ok {
				exprStr = string(sym)
			}
		}
	}
	e.pushCallFrame("<eval>", location, exprStr)
	defer e.popCallFrame()

	// Unwrap the LocatedValue if present
	unwrappedExpr := e.unwrapLocatedValue(expr)

	// We need to switch on the unwrapped expression
	switch v := unwrappedExpr.(type) {
	case core.LispSymbol:
		// Check for dot notation: module.attribute, dict.method, or object.attribute
		if strings.Contains(string(v), ".") {
			fmt.Printf("DEBUG: Dot notation detected in symbol: %s\n", string(v))
			parts := strings.Split(string(v), ".")
			fmt.Printf("DEBUG: Dot notation parts: %v\n", parts)

			// Special case for dict methods and set methods - look up directly in the environment
			if parts[0] == "dict" || parts[0] == "set" {
				// Functions like dict.keys, dict.update, set.intersection, etc.
				// should be looked up directly in the environment
				fmt.Printf("DEBUG: Special case for dict/set methods: %s\n", string(v))
				val, ok := env.Get(v)
				if ok {
					fmt.Printf("DEBUG: Found method directly: %T\n", val)
					return val, nil
				}
				fmt.Printf("DEBUG: Method not found directly, falling through\n")
				// Fall through to the module lookup if not found
			}

			// Handle multiple levels of attributes (e.g., module.submodule.attribute)
			current := parts[0]
			fmt.Printf("DEBUG: Looking up base object: %s\n", current)

			// Start with the first part
			currentObj, ok := env.Get(core.LispSymbol(current))
			if !ok {
				// Try checking the environment directly
				fmt.Printf("DEBUG: Base object not found via Get: %s\n", current)
				fmt.Printf("DEBUG: Checking environment directly for object\n")

				// Try looking for a dot handler for this object
				dotHandlerName := core.LispSymbol(current + ".__dot__")
				fmt.Printf("DEBUG: Looking for dot handler: %s\n", dotHandlerName)
				if handler, ok := env.Get(dotHandlerName); ok {
					// We found a dot handler, so use it to access the property
					fmt.Printf("DEBUG: Found dot handler for %s\n", current)

					// This could be a more elegant solution with a ModuleRef interface,
					// but for now we'll just handle it specially
					if builtinFn, ok := handler.(core.BuiltinFunc); ok {
						// We'll continue processing in the dot notation loop
						fmt.Printf("DEBUG: Using dot handler to process dot notation\n")

						// Get the property name
						if len(parts) < 2 {
							err := fmt.Errorf(core.ErrDotMissingArgs)
							return nil, e.enrichErrorWithTraceback(err)
						}

						propName := parts[1]
						fmt.Printf("DEBUG: Accessing property: %s\n", propName)

						// Call the handler with the property name
						result, err := builtinFn([]core.LispValue{propName}, env)
						if err != nil {
							fmt.Printf("DEBUG: Dot handler error: %v\n", err)
							return nil, e.enrichErrorWithTraceback(err)
						}

						fmt.Printf("DEBUG: Dot handler result: %T\n", result)

						// If only one level, return directly
						if len(parts) == 2 {
							return result, nil
						}

						// Otherwise start at the next level with the result
						currentObj = result
						current = fmt.Sprintf("%s.%s", current, propName)

						// Continue from position 2
						for i := 2; i < len(parts); i++ {
							attrName := parts[i]
							fmt.Printf("DEBUG: Processing additional level: %s\n", attrName)

							// Check if the object implements DotAccessible interface
							if dotAccessible, ok := currentObj.(core.DotAccessible); ok {
								// Try to access the property
								if dotAccessible.HasProperty(attrName) {
									var found bool
									currentObj, found = dotAccessible.GetProperty(attrName)
									if !found {
										err := core.ErrDotNoPropertyf(attrName)
										return nil, e.enrichErrorWithTraceback(err)
									}
									current = fmt.Sprintf("%s.%s", current, attrName)
								} else if dotAccessible.HasMethod(attrName) {
									// Create a method reference
									methodRef := core.BuiltinFunc(func(args []core.LispValue, env core.Environment) (core.LispValue, error) {
										return dotAccessible.CallMethod(attrName, args)
									})
									currentObj = methodRef
									current = fmt.Sprintf("%s.%s", current, attrName)
								} else {
									err := core.ErrDotNoPropertyf(attrName)
									return nil, e.enrichErrorWithTraceback(err)
								}
							} else if dict, ok := currentObj.(*core.PythonicDict); ok {
								// Legacy path for dictionaries
								currentObj, ok = dict.Get(attrName)
								if !ok {
									err := fmt.Errorf("attribute %s not found in %s", attrName, current)
									return nil, e.enrichErrorWithTraceback(err)
								}
								current = fmt.Sprintf("%s.%s", current, attrName)
							} else {
								// Try to use the object's attributes
								err := fmt.Errorf("cannot access attribute %s on non-dictionary %T", attrName, currentObj)
								return nil, e.enrichErrorWithTraceback(err)
							}
						}

						return currentObj, nil
					}
				}

				// Print environment debug info
				fmt.Printf("DEBUG: Environment: %s\n", env)

				err := fmt.Errorf("undefined object: %s", current)
				return nil, e.enrichErrorWithTraceback(err)
			}
			fmt.Printf("DEBUG: Found base object: %s, type: %T\n", current, currentObj)

			// Traverse the chain of attributes
			for i := 1; i < len(parts); i++ {
				attrName := core.LispSymbol(parts[i])
				fmt.Printf("DEBUG: Looking up attribute: %s in %s\n", attrName, current)

				// First check if the object implements DotAccessible interface
				if dotAccessible, ok := currentObj.(core.DotAccessible); ok {
					fmt.Printf("DEBUG: Object implements DotAccessible interface\n")
					attrNameStr := string(attrName)

					// Check if it's a property access
					if dotAccessible.HasProperty(attrNameStr) {
						fmt.Printf("DEBUG: Found property %s via DotAccessible interface\n", attrNameStr)
						var found bool
						currentObj, found = dotAccessible.GetProperty(attrNameStr)
						if !found {
							err := fmt.Errorf("property %s not found in %s", attrName, current)
							return nil, e.enrichErrorWithTraceback(err)
						}
						current = current + "." + parts[i]
					} else if dotAccessible.HasMethod(attrNameStr) {
						// It's a method, but we're not calling it yet, just getting a reference
						fmt.Printf("DEBUG: Found method %s via DotAccessible interface\n", attrNameStr)
						// Create a method reference that can be called later
						methodRef := core.BuiltinFunc(func(args []core.LispValue, env core.Environment) (core.LispValue, error) {
							return dotAccessible.CallMethod(attrNameStr, args)
						})
						currentObj = methodRef
						current = current + "." + parts[i]
					} else {
						// Try special handling for modules if it's a PythonicDict
						if dict, ok := currentObj.(*core.PythonicDict); ok {
							if _, hasName := dict.Get("__name__"); hasName {
								fmt.Printf("DEBUG: This appears to be a module (has __name__)\n")
								// Extra debug for module contents
								items, _ := dict.CallMethod("items", []core.LispValue{})
								if itemsList, ok := items.(core.LispList); ok {
									fmt.Printf("DEBUG: Module contents (%d items):\n", len(itemsList))
									for _, item := range itemsList {
										if pair, ok := item.(core.LispList); ok && len(pair) == 2 {
											fmt.Printf("  - %v: %T\n", pair[0], pair[1])
										}
									}
								}
							}
						}
						err := fmt.Errorf("attribute %s not found in %s", attrName, current)
						return nil, e.enrichErrorWithTraceback(err)
					}
				} else if dict, ok := currentObj.(*core.PythonicDict); ok {
					// Legacy path for dictionaries that might not implement DotAccessible
					fmt.Printf("DEBUG: Object is a PythonicDict (legacy path)\n")
					// Get the attribute
					var found bool
					currentObj, found = dict.Get(attrName)
					if !found {
						fmt.Printf("DEBUG: Attribute %s not found in dict %s\n", attrName, current)
						// Try special handling for modules
						if _, hasName := dict.Get("__name__"); hasName {
							fmt.Printf("DEBUG: This appears to be a module (has __name__)\n")
							// Extra debug for module contents
							items, _ := dict.CallMethod("items", []core.LispValue{})
							if itemsList, ok := items.(core.LispList); ok {
								fmt.Printf("DEBUG: Module contents (%d items):\n", len(itemsList))
								for _, item := range itemsList {
									if pair, ok := item.(core.LispList); ok && len(pair) == 2 {
										fmt.Printf("  - %v: %T\n", pair[0], pair[1])
									}
								}
							}
						}
						err := fmt.Errorf("attribute %s not found in %s", attrName, current)
						return nil, e.enrichErrorWithTraceback(err)
					}
					fmt.Printf("DEBUG: Found attribute %s in dict, type: %T\n", attrName, currentObj)
					current = current + "." + parts[i]
				} else {
					// Try to access the attribute using the dot special form
					fmt.Printf("DEBUG: Not a dict, trying dot special form for %s.%s\n", current, attrName)
					dotArgs := []core.LispValue{currentObj, string(attrName)}
					// Check if special_forms package is available
					if dotFn, ok := e.specialForms[core.LispSymbol("dot")]; ok {
						fmt.Printf("DEBUG: Found dot special form, calling it\n")
						// Use the dot special form
						result, err := dotFn(e, dotArgs, env)
						if err != nil {
							fmt.Printf("DEBUG: Dot special form error: %v\n", err)
							return nil, e.enrichErrorWithTraceback(err)
						}
						fmt.Printf("DEBUG: Dot special form succeeded, result type: %T\n", result)
						currentObj = result
						current = current + "." + parts[i]
					} else {
						fmt.Printf("DEBUG: Dot special form not available\n")
						err := fmt.Errorf("%s is not an object with attributes (does not implement DotAccessible)", current)
						return nil, e.enrichErrorWithTraceback(err)
					}
				}
			}

			return currentObj, nil
		}

		// Regular symbol lookup
		value, ok := env.Get(v)
		if !ok {
			// Check if this is likely a special form that wasn't properly registered
			if core.IsBuiltinSpecialForm(v) {
				err := fmt.Errorf("special form '%s' not properly registered in environment - ensure special_forms.RegisterSpecialForms() is called during initialization", v)
				return nil, e.enrichErrorWithTraceback(err)
			}

			// Check for typos in common symbols
			suggestions := suggestSymbol(v, env)
			if len(suggestions) > 0 {
				err := fmt.Errorf("undefined symbol: %s (did you mean: %s?)", v, strings.Join(suggestions, ", "))
				return nil, e.enrichErrorWithTraceback(err)
			}

			err := fmt.Errorf("undefined symbol: %s", v)
			return nil, e.enrichErrorWithTraceback(err)
		}
		return value, nil
	case float64, int, string, core.PythonicBool, core.PythonicNone:
		return v, nil
	case *core.PythonicDict:
		// Evaluate dictionary literals
		result, err := e.evalDict(v, env)
		if err != nil {
			return nil, e.enrichErrorWithTraceback(err)
		}
		return result, nil
	case *core.PythonicSet:
		// Evaluate set literals
		result, err := e.evalSet(v, env)
		if err != nil {
			return nil, e.enrichErrorWithTraceback(err)
		}
		return result, nil
	case core.LispListLiteral:
		// Evaluate list literal elements
		result := make(core.LispList, len(v))
		for i, elem := range v {
			evalElem, err := e.Eval(elem, env)
			if err != nil {
				return nil, e.enrichErrorWithTraceback(err)
			}
			result[i] = evalElem
		}
		return result, nil
	case core.LispTuple:
		// Evaluate tuple elements (similar to list literals)
		result := make(core.LispTuple, len(v))
		for i, elem := range v {
			evalElem, err := e.Eval(elem, env)
			if err != nil {
				return nil, e.enrichErrorWithTraceback(err)
			}
			result[i] = evalElem
		}
		return result, nil
	case core.LispComprehension:
		result, err := e.evalComprehension(v, env)
		if err != nil {
			return nil, e.enrichErrorWithTraceback(err)
		}
		return result, nil
	case core.LispList:
		if len(v) == 0 {
			return core.LispList{}, nil
		}

		// Ensure we unwrap the first element for special form and function lookup
		first := e.unwrapLocatedValue(v[0])
		rest := v[1:]

		switch f := first.(type) {
		case core.LispSymbol:
			// Check for dot notation in function call: module.function(args) or object.method(args)
			if strings.Contains(string(f), ".") {
				// Handle multiple levels of attributes (e.g., module.submodule.function)
				parts := strings.Split(string(f), ".")
				current := parts[0]

				// Start with the first part
				currentObj, ok := env.Get(core.LispSymbol(current))
				if !ok {
					err := fmt.Errorf("undefined object: %s", current)
					return nil, e.enrichErrorWithTraceback(err)
				}

				// Traverse all but the last part (which is the function name)
				for i := 1; i < len(parts)-1; i++ {
					attrName := core.LispSymbol(parts[i])

					// Check if the object implements DotAccessible interface
					if dotAccessible, ok := currentObj.(core.DotAccessible); ok {
						attrNameStr := string(attrName)

						// Check if it's a property access
						if dotAccessible.HasProperty(attrNameStr) {
							fmt.Printf("DEBUG: Found property %s via DotAccessible interface\n", attrNameStr)
							var found bool
							currentObj, found = dotAccessible.GetProperty(attrNameStr)
							if !found {
								err := core.ErrDotNoPropertyf(string(attrName))
								return nil, e.enrichErrorWithTraceback(err)
							}
							current = current + "." + parts[i]
						} else if dotAccessible.HasMethod(attrNameStr) {
							// It's a method, but we're not calling it yet, just getting a reference
							fmt.Printf("DEBUG: Found method %s via DotAccessible interface\n", attrNameStr)
							// Create a method reference that can be called later
							methodRef := core.BuiltinFunc(func(args []core.LispValue, env core.Environment) (core.LispValue, error) {
								return dotAccessible.CallMethod(attrNameStr, args)
							})
							currentObj = methodRef
							current = current + "." + parts[i]
						} else {
							err := fmt.Errorf("attribute %s not found in %s", attrName, current)
							return nil, e.enrichErrorWithTraceback(err)
						}
					} else if dict, ok := currentObj.(*core.PythonicDict); ok {
						// Legacy path for dictionaries
						// Get the attribute
						var found bool
						currentObj, found = dict.Get(attrName)
						if !found {
							err := fmt.Errorf("attribute %s not found in %s", attrName, current)
							return nil, e.enrichErrorWithTraceback(err)
						}
						current = current + "." + parts[i]
					} else {
						err := fmt.Errorf("%s is not an object with attributes (does not implement DotAccessible)", current)
						return nil, e.enrichErrorWithTraceback(err)
					}
				}

				// Get the function name (last part)
				funcName := core.LispSymbol(parts[len(parts)-1])

				// First check if the object implements DotAccessible interface
				if dotAccessible, ok := currentObj.(core.DotAccessible); ok {
					funcNameStr := string(funcName)

					// Check if it's a method call
					if dotAccessible.HasMethod(funcNameStr) {
						fmt.Printf("DEBUG: Found method %s via DotAccessible interface\n", funcNameStr)

						// Evaluate the arguments
						evalArgs, err := e.evalArgs(rest, env)
						if err != nil {
							return nil, e.enrichErrorWithTraceback(err)
						}

						// Call the method directly on the object
						return dotAccessible.CallMethod(funcNameStr, evalArgs)
					} else if dotAccessible.HasProperty(funcNameStr) {
						// It's a property access followed by a function call
						fmt.Printf("DEBUG: Found property %s via DotAccessible interface\n", funcNameStr)

						fnVal, found := dotAccessible.GetProperty(funcNameStr)
						if !found {
							err := core.ErrDotNoPropertyf(string(funcName))
							return nil, e.enrichErrorWithTraceback(err)
						}

						// Evaluate the arguments
						evalArgs, err := e.evalArgs(rest, env)
						if err != nil {
							return nil, e.enrichErrorWithTraceback(err)
						}

						// Apply the function
						return e.Apply(fnVal, evalArgs, env)
					} else {
						err := core.ErrDotNoMethodf(string(funcName))
						return nil, e.enrichErrorWithTraceback(err)
					}
				} else if dict, ok := currentObj.(*core.PythonicDict); ok {
					// Legacy path for dictionaries that might not implement DotAccessible
					// Get the function
					fnVal, found := dict.Get(funcName)
					if !found {
						err := core.ErrDotNoPropertyf(string(funcName))
						return nil, e.enrichErrorWithTraceback(err)
					}

					// Evaluate the arguments
					args, err := e.evalArgs(rest, env)
					if err != nil {
						return nil, e.enrichErrorWithTraceback(err)
					}

					// Apply the function
					return e.Apply(fnVal, args, env)
				} else {
					err := core.ErrDotMissingInterfacef(currentObj)
					return nil, e.enrichErrorWithTraceback(err)
				}
			} else if f == core.LispSymbol("=") {
				// Special handling for assignment
				if len(rest) != 2 {
					err := fmt.Errorf("= requires exactly two arguments")
					return nil, e.enrichErrorWithTraceback(err)
				}

				// Unwrap the symbol in assignment to ensure it's not a LocatedValue
				unwrappedSymbol := e.unwrapLocatedValue(rest[0])
				symbol, ok := unwrappedSymbol.(core.LispSymbol)
				if !ok {
					err := fmt.Errorf("first argument to = must be a symbol")
					return nil, e.enrichErrorWithTraceback(err)
				}
				value, err := e.Eval(rest[1], env)
				if err != nil {
					return nil, e.enrichErrorWithTraceback(err)
				}
				env.Define(symbol, value)
				return value, nil
			}

			// Check for special form directly in our map
			if specialForm, ok := e.specialForms[f]; ok {
				// Unwrap all arguments for special forms
				unwrappedArgs := make([]core.LispValue, len(rest))
				for i, arg := range rest {
					unwrappedArgs[i] = e.unwrapLocatedValue(arg)
				}
				result, err := specialForm(e, unwrappedArgs, env)
				if err != nil {
					return nil, e.enrichErrorWithTraceback(err)
				}
				return result, nil
			}

			// Check for special form marker in the environment
			if value, ok := env.Get(f); ok {
				if marker, ok := value.(core.SpecialFormMarker); ok {
					if specialForm, ok := e.specialForms[marker.Name]; ok {
						// Unwrap all arguments for special forms
						unwrappedArgs := make([]core.LispValue, len(rest))
						for i, arg := range rest {
							unwrappedArgs[i] = e.unwrapLocatedValue(arg)
						}
						result, err := specialForm(e, unwrappedArgs, env)
						if err != nil {
							return nil, e.enrichErrorWithTraceback(err)
						}
						return result, nil
					}
				}
			}

			// Not a special form, evaluate as a regular function call
			fn, err := e.Eval(v[0], env) // Use the original value with location info
			if err != nil {
				return nil, e.enrichErrorWithTraceback(err)
			}
			args, err := e.evalArgs(rest, env)
			if err != nil {
				return nil, e.enrichErrorWithTraceback(err)
			}
			return e.Apply(fn, args, env)
		default:
			fn, err := e.Eval(v[0], env) // Use the original value with location info
			if err != nil {
				return nil, e.enrichErrorWithTraceback(err)
			}
			args, err := e.evalArgs(rest, env)
			if err != nil {
				return nil, e.enrichErrorWithTraceback(err)
			}
			return e.Apply(fn, args, env)
		}
	default:
		err := fmt.Errorf("unknown expression type: %T", expr)
		return nil, e.enrichErrorWithTraceback(err)
	}
}

func (e *Evaluator) evalArgs(args []core.LispValue, env core.Environment) ([]core.LispValue, error) {
	evaluated := make([]core.LispValue, len(args))
	for i, arg := range args {
		// First, unwrap LocatedValue if present to check for keyword arguments
		unwrappedArg := e.unwrapLocatedValue(arg)

		// Check if arg is a symbol that represents a keyword argument
		if symbol, ok := unwrappedArg.(core.LispSymbol); ok {
			symbolStr := string(symbol)
			if len(symbolStr) > 0 && strings.Contains(symbolStr, "=") {
				// This is a keyword argument in the form name=value
				evaluated[i] = symbolStr // Pass it as a string for the lambda to process
				continue
			} else if len(symbol) > 1 && symbol[0] == ':' {
				// This is a keyword argument, don't evaluate it
				evaluated[i] = unwrappedArg
				continue
			}
		}

		// Otherwise evaluate normally, preserving location information where appropriate
		value, err := e.Eval(arg, env)
		if err != nil {
			return nil, e.enrichErrorWithTraceback(err)
		}

		// Check if the evaluated value contains a keyword format
		unwrappedValue := e.unwrapLocatedValue(value)
		if str, ok := unwrappedValue.(string); ok && strings.Contains(str, "=") {
			// Preserve the string format for keyword detection in ApplyLambda
			evaluated[i] = str
		} else {
			evaluated[i] = value
		}
	}
	return evaluated, nil
}

// enrichErrorWithTraceback adds traceback information to errors
func (e *Evaluator) enrichErrorWithTraceback(err error) error {
	if err == nil {
		return nil
	}

	// Skip traceback for special control flow signals
	switch err.(type) {
	case special_forms.ReturnSignal, special_forms.YieldSignal:
		return err // Control flow signals should pass through unchanged
	}

	// If it's already an exception with a traceback, return it as is
	if ex, ok := err.(*core.Exception); ok && ex.Traceback != nil && len(ex.Traceback) > 0 {
		return ex
	}

	// Create a new exception or enrich an existing one
	var ex *core.Exception
	if exErr, ok := err.(*core.Exception); ok {
		ex = exErr
	} else {
		// Convert regular error to Exception
		ex = core.NewException("RuntimeError", err.Error())
	}

	// Add current traceback if not present
	if ex.Traceback == nil || len(ex.Traceback) == 0 {
		ex.Traceback = e.getCurrentTraceback()
	}

	return ex
}

func (e *Evaluator) Apply(fn core.LispValue, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// Unwrap the function if it's a LocatedValue
	unwrappedFn := e.unwrapLocatedValue(fn)

	// Extract location information if available
	location := e.getLocationFromExpr(fn)

	// Convert args to string for traceback
	argsStr := "("
	for i, arg := range args {
		if i > 0 {
			argsStr += ", "
		}
		// Limit the string representation for brevity
		argStr := core.PrintValue(e.unwrapLocatedValue(arg))
		if len(argStr) > 20 {
			argStr = argStr[:17] + "..."
		}
		argsStr += argStr
	}
	argsStr += ")"

	// Function name for call stack
	var funcName string
	switch f := unwrappedFn.(type) {
	case core.BuiltinFunc:
		funcName = "<builtin>"
	case *core.Lambda:
		funcName = "<lambda>"
	case core.LispSymbol:
		funcName = string(f)
	default:
		funcName = "<anonymous>"
	}

	// Create a statement representation for the traceback
	statement := funcName + argsStr

	// Push a new call frame
	e.pushCallFrame(funcName, location, statement)

	// Defer popping the call frame to ensure it happens even on panic
	defer e.popCallFrame()

	var result core.LispValue
	var err error

	// Unwrap arguments for built-in functions that might not handle LocatedValue
	unwrappedArgs := make([]core.LispValue, len(args))
	for i, arg := range args {
		unwrappedArgs[i] = e.unwrapLocatedValue(arg)
	}

	// Apply the function based on its type
	switch f := unwrappedFn.(type) {
	case core.BuiltinFunc:
		// For built-in functions, always provide fully unwrapped arguments
		result, err = f(unwrappedArgs, env)
	case *core.Lambda:
		// Check for tail call optimization opportunity
		if isTailCall := e.checkTailCallOptimization(); isTailCall {
			// Create initial tail call
			initialTailCall := &TailCall{
				Function: f,
				Args:     args,
				Env:      env,
			}
			// Use trampoline to handle recursive calls without growing stack
			result, err = e.trampoline(initialTailCall)
		} else {
			// Use the lambda directly - the instance ID mechanism ensures proper state isolation
			// Keep original args for lambdas as they might need the location information
			result, err = special_forms.ApplyLambda(e, f, args, env)
		}
	case core.LispList:
		if len(f) > 0 {
			// Unwrap the first element to check if it's a lambda
			firstElem := e.unwrapLocatedValue(f[0])
			if firstElem == core.LispSymbol("lambda") {
				// For inline lambda definitions, unwrap all elements
				unwrappedLambdaBody := make([]core.LispValue, len(f[1:]))
				for i, item := range f[1:] {
					unwrappedLambdaBody[i] = e.unwrapLocatedValue(item)
				}

				lambda, lambdaErr := special_forms.EvalLambdaPython(e, unwrappedLambdaBody, env)
				if lambdaErr != nil {
					err = lambdaErr
					break
				}

				// Check for tail call optimization opportunity
				if isTailCall := e.checkTailCallOptimization(); isTailCall {
					// Create initial tail call
					initialTailCall := &TailCall{
						Function: lambda,
						Args:     args,
						Env:      env,
					}
					// Use trampoline to handle recursive calls without growing stack
					result, err = e.trampoline(initialTailCall)
				} else {
					result, err = special_forms.ApplyLambda(e, lambda.(*core.Lambda), args, env)
				}
			} else {
				err = fmt.Errorf("not a function: %v", fn)
			}
		} else {
			err = fmt.Errorf("not a function: %v", fn)
		}
	default:
		err = fmt.Errorf("not a function: %v", fn)
	}

	// Enrich error with traceback if needed
	if err != nil {
		return nil, e.enrichErrorWithTraceback(err)
	}

	return result, nil
}

// checkTailCallOptimization determines if we should use tail call optimization
// For simplicity, we'll always return true to enable TCO for all recursive calls
func (e *Evaluator) checkTailCallOptimization() bool {
	// We can make this more sophisticated by checking the current stack depth
	// or looking at the specific recursion pattern, but for now we'll enable it globally
	return true
}

// levenshteinDistance calculates the edit distance between two strings
func levenshteinDistance(s1, s2 string) int {
	s1 = strings.ToLower(s1)
	s2 = strings.ToLower(s2)

	if len(s1) == 0 {
		return len(s2)
	}
	if len(s2) == 0 {
		return len(s1)
	}

	// Create a matrix with dimensions (len(s1)+1) x (len(s2)+1)
	matrix := make([][]int, len(s1)+1)
	for i := range matrix {
		matrix[i] = make([]int, len(s2)+1)
		matrix[i][0] = i
	}
	for j := range matrix[0] {
		matrix[0][j] = j
	}

	// Fill in the matrix
	for i := 1; i <= len(s1); i++ {
		for j := 1; j <= len(s2); j++ {
			cost := 1
			if s1[i-1] == s2[j-1] {
				cost = 0
			}
			matrix[i][j] = min(
				matrix[i-1][j]+1,      // deletion
				matrix[i][j-1]+1,      // insertion
				matrix[i-1][j-1]+cost, // substitution
			)
		}
	}

	return matrix[len(s1)][len(s2)]
}

// min returns the minimum of three integers
func min(a, b, c int) int {
	if a < b {
		if a < c {
			return a
		}
		return c
	}
	if b < c {
		return b
	}
	return c
}

// suggestSymbol provides suggestions for similar symbols in the environment
func suggestSymbol(symbol core.LispSymbol, env core.Environment) []string {
	// Maximum edit distance to consider a suggestion
	const maxDistance = 2

	// Common special forms and builtins to check against
	commonSymbols := []string{
		"def", "if", "else", "for", "while", "lambda", "class", "import",
		"raise", "try", "with", "return", "yield", "None", "True", "False",
		"print", "range", "len", "dict", "list", "set", "tuple",
	}

	symbolStr := string(symbol)
	var suggestions []string

	// Check environment for similar symbols
	// Try to access ForEachSymbol if available on the environment
	if envWithSymbols, ok := env.(interface {
		ForEachSymbol(func(core.LispSymbol, core.LispValue))
	}); ok {
		envWithSymbols.ForEachSymbol(func(sym core.LispSymbol, _ core.LispValue) {
			symStr := string(sym)
			if distance := levenshteinDistance(symbolStr, symStr); distance <= maxDistance {
				suggestions = append(suggestions, symStr)
			}
		})
	}

	// Also check common symbols not in environment
	for _, common := range commonSymbols {
		if distance := levenshteinDistance(symbolStr, common); distance <= maxDistance {
			// Check if already in suggestions
			found := false
			for _, s := range suggestions {
				if s == common {
					found = true
					break
				}
			}
			if !found {
				suggestions = append(suggestions, common)
			}
		}
	}

	// Limit to top 3 suggestions
	if len(suggestions) > 3 {
		suggestions = suggestions[:3]
	}

	return suggestions
}

func evalSymbol(symbol core.LispSymbol, env core.Environment) (core.LispValue, error) {
	switch symbol {
	case "None":
		return core.PythonicNone{}, nil
	case "True":
		return core.PythonicBool(true), nil
	case "False":
		return core.PythonicBool(false), nil
	default:
		value, ok := env.Get(symbol)
		if !ok {
			// Check for potential suggestions
			suggestions := suggestSymbol(symbol, env)
			if len(suggestions) > 0 {
				return nil, fmt.Errorf("undefined symbol: %s (did you mean: %s?)", symbol, strings.Join(suggestions, ", "))
			}
			return nil, fmt.Errorf("undefined symbol: %s", symbol)
		}
		return value, nil
	}
}

func (e *Evaluator) evalList(list core.LispList, env core.Environment) (core.LispValue, error) {
	if len(list) == 0 {
		return core.LispList{}, nil
	}

	first := list[0]
	rest := list[1:]

	switch v := first.(type) {
	case core.LispSymbol:
		if specialForm, ok := e.specialForms[v]; ok {
			result, err := specialForm(e, rest, env)
			if err != nil {
				return nil, e.enrichErrorWithTraceback(err)
			}
			return result, nil
		}

		// Check for special form marker in the environment
		if value, ok := env.Get(v); ok {
			if marker, ok := value.(core.SpecialFormMarker); ok {
				if specialForm, ok := e.specialForms[marker.Name]; ok {
					result, err := specialForm(e, rest, env)
					if err != nil {
						return nil, e.enrichErrorWithTraceback(err)
					}
					return result, nil
				}
			}
		}
		fn, err := e.Eval(v, env)
		if err != nil {
			err = fmt.Errorf("error evaluating symbol %s: %v", v, err)
			return nil, e.enrichErrorWithTraceback(err)
		}
		args, err := e.evalArgs(rest, env)
		if err != nil {
			err = fmt.Errorf("error evaluating arguments: %v", err)
			return nil, e.enrichErrorWithTraceback(err)
		}
		return e.Apply(fn, args, env)
	default:
		fn, err := e.Eval(first, env)
		if err != nil {
			err = fmt.Errorf("error evaluating function: %v", err)
			return nil, e.enrichErrorWithTraceback(err)
		}
		args, err := e.evalArgs(rest, env)
		if err != nil {
			err = fmt.Errorf("error evaluating arguments: %v", err)
			return nil, e.enrichErrorWithTraceback(err)
		}
		return e.Apply(fn, args, env)
	}
}

func (e *Evaluator) evalDict(dict *core.PythonicDict, env core.Environment) (core.LispValue, error) {
	result := core.NewPythonicDict()

	// Iterate over the original dict and evaluate each key-value pair
	keyFunc := func(key, value core.LispValue) error {
		evaluatedKey, err := e.Eval(key, env)
		if err != nil {
			return e.enrichErrorWithTraceback(err)
		}
		evaluatedValue, err := e.Eval(value, env)
		if err != nil {
			return e.enrichErrorWithTraceback(err)
		}
		result.Set(evaluatedKey, evaluatedValue)
		return nil
	}

	err := dict.Iterate(keyFunc)
	if err != nil {
		return nil, e.enrichErrorWithTraceback(err)
	}

	return result, nil
}

func (e *Evaluator) evalSet(set *core.PythonicSet, env core.Environment) (core.LispValue, error) {
	newSet := core.NewPythonicSet()
	for v := range set.Data() {
		evalValue, err := e.Eval(v, env)
		if err != nil {
			return nil, e.enrichErrorWithTraceback(err)
		}
		newSet.Add(evalValue)
	}
	return newSet, nil
}

func (e *Evaluator) applyLambda(lambda *core.Lambda, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	result, err := special_forms.ApplyLambda(e, lambda, args, env)
	if err != nil {
		return nil, e.enrichErrorWithTraceback(err)
	}
	return result, nil
}

// evalComprehension evaluates a list comprehension: [expr for var in iterable if condition]
func (e *Evaluator) evalComprehension(comp core.LispComprehension, env core.Environment) (core.LispValue, error) {
	// Evaluate the iterable expression
	iterableVal, err := e.Eval(comp.Iterable, env)
	if err != nil {
		err = fmt.Errorf("error evaluating iterable in list comprehension: %v", err)
		return nil, e.enrichErrorWithTraceback(err)
	}

	// Convert the iterable to a list
	var iterList core.LispList
	switch v := iterableVal.(type) {
	case core.LispList:
		iterList = v
	case core.LispListLiteral:
		iterList = core.LispList(v)
	case string:
		// Handle strings by converting them to a list of characters
		iterList = make(core.LispList, len(v))
		for i, ch := range v {
			iterList[i] = string(ch)
		}
	default:
		err = fmt.Errorf("iterable in list comprehension must be a list or string, got %T", iterableVal)
		return nil, e.enrichErrorWithTraceback(err)
	}

	// Create a result list
	var result core.LispList

	// Create a new environment for the loop
	loopEnv := env.NewEnvironment(env)

	// Iterate over the iterable
	for _, item := range iterList {
		// Bind the variable
		loopEnv.Define(comp.Variable, item)

		// Check the condition if there is one
		if comp.Condition != nil {
			condVal, err := e.Eval(comp.Condition, loopEnv)
			if err != nil {
				err = fmt.Errorf("error evaluating condition in list comprehension: %v", err)
				return nil, e.enrichErrorWithTraceback(err)
			}
			// Skip this iteration if the condition is false
			if !core.IsTruthy(condVal) {
				continue
			}
		}

		// Evaluate the expression
		exprVal, err := e.Eval(comp.Expression, loopEnv)
		if err != nil {
			err = fmt.Errorf("error evaluating expression in list comprehension: %v", err)
			return nil, e.enrichErrorWithTraceback(err)
		}

		// Add the result to our list
		result = append(result, exprVal)
	}

	return result, nil
}
