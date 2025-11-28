// Package eval provides function call evaluation
package eval

import (
	"fmt"
	"os"
	"strings"

	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/core"
)

func evalFunctionCall(expr *core.ListValue, ctx *core.Context) (core.Value, error) {
	// Check if the function is referenced by a symbol (for better error messages)
	var symbolName string
	if sym, ok := expr.Items()[0].(core.SymbolValue); ok {
		symbolName = string(sym)
	}
	core.DebugLog("[EVAL] evalFunctionCall: %s with %d args\n", symbolName, expr.Len()-1)

	// Evaluate the function
	core.DebugLog("[EVAL] Evaluating function expression\n")
	fn, err := Eval(expr.Items()[0], ctx)
	if err != nil {
		return nil, err
	}
	core.DebugLog("[EVAL] Function evaluated to: %T\n", fn)

	// Evaluate the arguments
	args := make([]core.Value, 0, expr.Len()-1)
	for i, arg := range expr.Items()[1:] {
		core.DebugLog("[EVAL] Evaluating arg %d\n", i)
		evalArg, err := Eval(arg, ctx)
		if err != nil {
			return nil, err
		}
		core.DebugLog("[EVAL] Arg %d evaluated to: %T\n", i, evalArg)
		args = append(args, evalArg)
	}
	core.DebugLog("[EVAL] All args evaluated\n")

	// Call the function
	callable, ok := fn.(core.Callable)
	if !ok {
		// Check if object has __call__ method
		if result, found, err := types.CallCall(fn, args, ctx); found {
			return result, err
		}

		errMsg := "function call"
		if symbolName != "" {
			errMsg = fmt.Sprintf("calling '%s'", symbolName)
		}
		return nil, core.NewTypeError("callable", fn, errMsg)
	}

	// Add function name to call stack if available
	var funcName string

	// Prefer the symbol name if we have it (for better error messages)
	if symbolName != "" {
		funcName = symbolName
	} else {
		// Fall back to introspecting the function object
		switch f := fn.(type) {
		case core.SymbolValue:
			funcName = string(f)
		case *core.BuiltinFunction:
			if nameVal, ok := f.GetAttr("__name__"); ok {
				if nameStr, ok := nameVal.(core.StringValue); ok {
					funcName = string(nameStr)
				} else {
					funcName = "<builtin>"
				}
			} else {
				funcName = "<builtin>"
			}
		case *core.BoundMethod:
			funcName = fmt.Sprintf("%s.%s", f.TypeDesc.PythonName, f.Method.Name)
		default:
			funcName = "<anonymous>"
		}
	}

	// Get source location from context for better error messages
	file := ""
	line := 0
	col := 0
	if loc := ctx.CurrentLocation(); loc != nil {
		file = loc.File
		line = loc.Line
		col = loc.Column
	}
	ctx.PushStack(funcName, file, line, col)
	defer ctx.PopStack()

	// Trace function call BEFORE execution
	core.TraceEnterFunction(funcName, args)

	result, err := callable.Call(args, ctx)

	// Trace function exit AFTER execution
	if err != nil {
		core.TraceExitFunction(funcName, nil, err)
	} else {
		core.TraceExitFunction(funcName, result, nil)
	}

	if err != nil {
		// Preserve PythonError for proper try/except matching
		if _, ok := err.(*core.PythonError); ok {
			return nil, err
		}

		// Preserve Exception for proper try/except matching
		if _, ok := err.(*Exception); ok {
			return nil, err
		}

		// Check if this is already a well-formatted error (e.g., from assert)
		// Don't wrap errors that are already EvalError or have specific messages
		if _, isEvalError := err.(*core.EvalError); isEvalError {
			return nil, err
		}

		// Special handling for assert - preserve its custom error messages
		if funcName == "assert" {
			return nil, core.WrapEvalError(err, err.Error(), ctx)
		}

		// Include the underlying error message for better debugging
		return nil, core.WrapEvalError(err, fmt.Sprintf("error in %s: %v", funcName, err), ctx)
	}

	return result, nil
}

type ReturnValue struct {
	Value core.Value
}

// Type implements Value.Type
func (r *ReturnValue) Type() core.Type {
	return "return"
}

// String implements Value.String
func (r *ReturnValue) String() string {
	return fmt.Sprintf("<return %v>", r.Value)
}

// UserFunction represents a user-defined function
type UserFunction struct {
	core.BaseObject
	params    []core.SymbolValue // Legacy: simple parameter list
	signature *FunctionSignature // New: full signature with defaults
	body      core.Value
	env       *core.Context
	name      string // Optional function name
	isLambda  bool   // True for lambda expressions (implicitly return body value)
}

// Call implements Callable.Call
func (f *UserFunction) Call(args []core.Value, ctx *core.Context) (core.Value, error) {
	// Trace function entry
	core.TraceEnterFunction(f.name, args)

	// Debug logging for function calls
	core.DebugLog("[CALL] UserFunction.Call: %s with %d args\n", f.name, len(args))

	// Special debug for __init__
	if f.name == "__init__" {
		// Try to figure out which class this is for
		var className string = "unknown"
		var funcClassName string = "unknown"
		if len(args) > 0 {
			if inst, ok := args[0].(*core.Instance); ok {
				className = inst.Class.Name
			}
		}
		// Also get the function's defining class from __class__ if available
		if f.env != nil {
			if classVal, err := f.env.Lookup("__class__"); err == nil {
				if cls, ok := classVal.(*core.Class); ok {
					funcClassName = cls.Name
				}
			}
		}
		if className == "PurePath" || className == "PurePosixPath" || className == "PosixPath" || className == "Path" || funcClassName == "PurePath" || funcClassName == "Path" || funcClassName == "PosixPath" {
			// 			fmt.Printf("[DEBUG UserFunction.Call] Calling __init__ for instance.Class=%s, func.__class__=%s, with %d args\n", className, funcClassName, len(args))
			// 			fmt.Printf("[DEBUG UserFunction.Call] Body type: %T\n", f.body)
			// 			fmt.Printf("[DEBUG UserFunction.Call] Body value: %v\n", f.body)
		}
	}

	// Create a new environment with the function's environment as parent
	funcEnv := core.NewContext(f.env)
	core.DebugLog("[CALL] Created funcEnv for %s\n", f.name)

	// If __class__ is defined in the call context, copy it to the function environment
	// This allows super() to know which class's method is being executed
	if ctx != nil {
		core.DebugLog("[CALL] Checking for __class__ in context\n")
		if classVal, err := ctx.Lookup("__class__"); err == nil {
			core.DebugLog("[CALL] Found __class__, defining in funcEnv\n")
			funcEnv.Define("__class__", classVal)
			// Debug for pathlib classes
			if f.name == "__init__" && classVal != nil {
				if cls, ok := classVal.(*core.Class); ok {
					if cls.Name == "PosixPath" || cls.Name == "Path" || cls.Name == "PurePath" {
						// 						fmt.Printf("[DEBUG] Setting __class__ to %s in %s.__init__\n", cls.Name, f.name)
					}
				}
			}
		} else {
			core.DebugLog("[CALL] No __class__ found (error: %v)\n", err)
			// Debug for pathlib classes
			if f.name == "__init__" && len(args) > 0 {
				if inst, ok := args[0].(*core.Instance); ok {
					if inst.Class.Name == "PosixPath" || inst.Class.Name == "Path" || inst.Class.Name == "PurePath" {
						// 						fmt.Printf("[DEBUG] NO __class__ found for %s instance in __init__\n", inst.Class.Name)
					}
				}
			}
		}
	}

	// Use new signature-based binding if available
	if f.signature != nil {
		// For now, we don't support keyword arguments in calls, just positional
		err := f.signature.BindArguments(args, nil, f.env, funcEnv)
		if err != nil {
			return nil, err
		}
	} else {
		// Legacy: simple parameter binding
		if len(args) != len(f.params) {
			return nil, fmt.Errorf("expected %d arguments, got %d", len(f.params), len(args))
		}

		for i, param := range f.params {
			funcEnv.Define(string(param), args[i])
		}
	}

	// Evaluate the body in the new environment
	if f.name == "__init__" {
		var className string = "unknown"
		if len(args) > 0 {
			if inst, ok := args[0].(*core.Instance); ok {
				className = inst.Class.Name
			}
		}
		if className == "PurePath" || className == "PurePosixPath" || className == "PosixPath" || className == "Path" {
			// 			fmt.Printf("[DEBUG UserFunction.Call] About to eval %s.__init__ body\n", className)
		}
	}
	result, err := Eval(f.body, funcEnv)
	if f.name == "__init__" {
		var className string = "unknown"
		if len(args) > 0 {
			if inst, ok := args[0].(*core.Instance); ok {
				className = inst.Class.Name
			}
		}
		if className == "PurePath" || className == "PurePosixPath" || className == "PosixPath" || className == "Path" {
			// 			fmt.Printf("[DEBUG UserFunction.Call] %s.__init__ body eval returned, err=%v\n", className, err)
		}
	}
	if err != nil {
		return nil, err
	}

	// Handle return values
	if ret, ok := result.(*ReturnValue); ok {
		core.TraceExitFunction(f.name, ret.Value, nil)
		return ret.Value, nil
	}

	// Check if we got a yield value (shouldn't happen in normal functions)
	if _, ok := result.(*core.YieldValue); ok {
		return nil, fmt.Errorf("yield outside of generator function")
	}

	// Lambda expressions implicitly return the value of their body expression
	if f.isLambda {
		core.TraceExitFunction(f.name, result, nil)
		return result, nil
	}

	// In Python, def functions without an explicit return statement return None
	// (not the value of the last expression like in Lisp)
	core.TraceExitFunction(f.name, core.None, nil)
	return core.None, nil
}

// CallWithKwargs calls the function with positional and keyword arguments
func (f *UserFunction) CallWithKwargs(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
	// Trace function entry
	core.TraceEnterFunction(f.name, args)

	// Create a new environment with the function's environment as parent
	funcEnv := core.NewContext(f.env)

	// If __class__ is defined in the call context, copy it to the function environment
	// This allows super() to know which class's method is being executed
	debugSuper := os.Getenv("M28_DEBUG_SUPER") != ""
	if classVal, err := ctx.Lookup("__class__"); err == nil {
		if debugSuper {
			className := "unknown"
			if cls, ok := classVal.(*core.Class); ok {
				className = cls.Name
			}
			fmt.Fprintf(os.Stderr, "[DEBUG UserFunction.CallWithKwargs] Function %s: copying __class__=%s from ctx to funcEnv\n", f.name, className)
		}
		funcEnv.Define("__class__", classVal)
	} else if debugSuper {
		fmt.Fprintf(os.Stderr, "[DEBUG UserFunction.CallWithKwargs] Function %s: NO __class__ in ctx (error: %v)\n", f.name, err)
	}

	// Use new signature-based binding if available
	if f.signature != nil {
		// Signature supports keyword arguments
		err := f.signature.BindArguments(args, kwargs, f.env, funcEnv)
		if err != nil {
			core.TraceExitFunction(f.name, nil, err)
			return nil, err
		}
	} else {
		// Legacy: simple parameter binding - no kwargs support
		if len(kwargs) > 0 {
			// Debug: identify which function is being called
			funcName := "unknown"
			if f.name != "" {
				funcName = f.name
			}
			// 			fmt.Printf("[DEBUG] Function %s called with kwargs but has no signature\n", funcName)
			// 			fmt.Printf("[DEBUG] kwargs: %v\n", kwargs)
			err := fmt.Errorf("function %s does not support keyword arguments", funcName)
			core.TraceExitFunction(f.name, nil, err)
			return nil, err
		}
		if len(args) != len(f.params) {
			err := fmt.Errorf("expected %d arguments, got %d", len(f.params), len(args))
			core.TraceExitFunction(f.name, nil, err)
			return nil, err
		}

		for i, param := range f.params {
			funcEnv.Define(string(param), args[i])
		}
	}

	// Evaluate the body in the new environment
	result, err := Eval(f.body, funcEnv)
	if err != nil {
		core.TraceExitFunction(f.name, nil, err)
		return nil, err
	}

	// Handle return values
	if ret, ok := result.(*ReturnValue); ok {
		core.TraceExitFunction(f.name, ret.Value, nil)
		return ret.Value, nil
	}

	// Lambda expressions implicitly return the value of their body expression
	if f.isLambda {
		core.TraceExitFunction(f.name, result, nil)
		return result, nil
	}

	// In Python, def functions without an explicit return statement return None
	// (not the value of the last expression like in Lisp)
	core.TraceExitFunction(f.name, core.None, nil)
	return core.None, nil
}

// CallWithKeywords is an alias for CallWithKwargs for interface compatibility
func (f *UserFunction) CallWithKeywords(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
	return f.CallWithKwargs(args, kwargs, ctx)
}

// String implements Value.String
func (f *UserFunction) String() string {
	params := make([]string, len(f.params))
	for i, param := range f.params {
		params[i] = string(param)
	}
	if f.name != "" {
		return fmt.Sprintf("<function %s(%s)>", f.name, strings.Join(params, " "))
	}
	return fmt.Sprintf("<function (%s)>", strings.Join(params, " "))
}

// GetName returns the function name for repr() purposes
func (f *UserFunction) GetName() string {
	return f.name
}

// GetAttr implements attribute access for user functions
func (f *UserFunction) GetAttr(name string) (core.Value, bool) {
	switch name {
	case "__name__":
		if f.name != "" {
			return core.StringValue(f.name), true
		}
		return core.StringValue("<anonymous>"), true
	case "__qualname__":
		// Check if __qualname__ was explicitly set
		if val, ok := f.BaseObject.GetAttr("__qualname__"); ok {
			return val, true
		}
		// Default to same as __name__
		if f.name != "" {
			return core.StringValue(f.name), true
		}
		return core.StringValue("<anonymous>"), true
	case "__module__":
		// Check if __module__ was explicitly set
		if val, ok := f.BaseObject.GetAttr("__module__"); ok {
			return val, true
		}
		// Default to "__main__" for user-defined functions
		return core.StringValue("__main__"), true
	case "__doc__":
		// Check if __doc__ was explicitly set
		if val, ok := f.BaseObject.GetAttr("__doc__"); ok {
			return val, true
		}
		// Default to None for no docstring
		return core.None, true
	case "__code__":
		// Return a code object (CodeType) with proper signature metadata
		// This is needed for inspect.signature() to work properly
		codeObj := core.NewCodeObject(f)

		// Extract parameter information from signature or legacy params
		var argCount int
		var varnames []core.Value
		var kwonlyargcount int
		var defaults []core.Value

		if f.signature != nil {
			// New-style function with signature
			argCount = len(f.signature.RequiredParams) + len(f.signature.OptionalParams)
			kwonlyargcount = 0 // M28 doesn't support keyword-only params yet

			// Build varnames list
			for _, param := range f.signature.RequiredParams {
				varnames = append(varnames, core.StringValue(string(param.Name)))
			}
			for _, param := range f.signature.OptionalParams {
				varnames = append(varnames, core.StringValue(string(param.Name)))
			}
			if f.signature.RestParam != nil {
				varnames = append(varnames, core.StringValue(string(*f.signature.RestParam)))
			}
			if f.signature.KeywordParam != nil {
				varnames = append(varnames, core.StringValue(string(*f.signature.KeywordParam)))
			}

			// Extract defaults
			for _, param := range f.signature.OptionalParams {
				if param.DefaultValue != nil {
					defaults = append(defaults, param.DefaultValue)
				}
			}
		} else {
			// Legacy function with simple params
			argCount = len(f.params)
			kwonlyargcount = 0
			for _, param := range f.params {
				varnames = append(varnames, core.StringValue(string(param)))
			}
		}

		// Set code object attributes that inspect.py expects
		codeObj.SetAttr("co_argcount", core.NumberValue(float64(argCount)))
		codeObj.SetAttr("co_posonlyargcount", core.NumberValue(0)) // M28 doesn't support positional-only params yet
		codeObj.SetAttr("co_kwonlyargcount", core.NumberValue(float64(kwonlyargcount)))
		codeObj.SetAttr("co_nlocals", core.NumberValue(float64(len(varnames))))
		codeObj.SetAttr("co_stacksize", core.NumberValue(0))
		// co_flags: 1=OPTIMIZED, 2=NEWLOCALS, 4=VARARGS, 8=VARKEYWORDS
		flags := 3 // OPTIMIZED | NEWLOCALS
		if f.signature != nil {
			if f.signature.RestParam != nil {
				flags |= 4 // VARARGS
			}
			if f.signature.KeywordParam != nil {
				flags |= 8 // VARKEYWORDS
			}
		}
		codeObj.SetAttr("co_flags", core.NumberValue(float64(flags)))
		codeObj.SetAttr("co_code", core.StringValue(""))
		codeObj.SetAttr("co_consts", core.TupleValue{})
		codeObj.SetAttr("co_names", core.TupleValue{})
		codeObj.SetAttr("co_varnames", core.TupleValue(varnames))
		codeObj.SetAttr("co_freevars", core.TupleValue{})
		codeObj.SetAttr("co_cellvars", core.TupleValue{})
		codeObj.SetAttr("co_filename", core.StringValue("<lambda>"))
		codeObj.SetAttr("co_name", core.StringValue(f.name))
		codeObj.SetAttr("co_firstlineno", core.NumberValue(1))
		codeObj.SetAttr("co_lnotab", core.StringValue(""))

		return codeObj, true
	case "__globals__":
		// Return the function's global namespace
		// This is the environment where the function was defined
		var globalsDict *core.DictValue

		// Get the global context for this function
		globalCtx := f.env
		if globalCtx != nil && globalCtx.Global != nil {
			globalCtx = globalCtx.Global
		}

		// If the global context has a ModuleDict, use it
		if globalCtx != nil && globalCtx.ModuleDict != nil {
			globalsDict = globalCtx.ModuleDict
		} else if globalCtx != nil {
			// Otherwise, create a dict from the global Vars
			globalsDict = core.NewDict()
			for name, value := range globalCtx.Vars {
				keyVal := core.StringValue(name)
				globalsDict.SetWithKey(core.ValueToKey(keyVal), keyVal, value)
			}
		} else {
			// No global context, return empty dict
			globalsDict = core.NewDict()
		}

		return globalsDict, true
	case "__dict__":
		// Return function's namespace/attributes as a dict
		funcDict := core.NewDict()
		// Could populate with actual function attributes if needed
		return funcDict, true
	case "__closure__":
		// Return tuple of closure cells (or None for no closure)
		// For now, return a tuple with a dummy cell to avoid subscript errors
		dummyCell := core.NewDict()
		dummyCell.Set("cell_contents", core.None)
		return core.TupleValue{dummyCell}, true
	case "__annotations__":
		// Check if __annotations__ was explicitly set
		if val, ok := f.BaseObject.GetAttr("__annotations__"); ok {
			return val, true
		}
		// Return empty dict for annotations by default
		return core.NewDict(), true
	case "__defaults__":
		// Return default argument values as a tuple
		if f.signature != nil {
			var defaults []core.Value
			for _, param := range f.signature.OptionalParams {
				if param.DefaultValue != nil {
					defaults = append(defaults, param.DefaultValue)
				}
			}
			if len(defaults) > 0 {
				return core.TupleValue(defaults), true
			}
		}
		return core.None, true
	case "__kwdefaults__":
		// Return keyword-only default argument values as a dict
		// M28 doesn't support keyword-only params yet, so always return None
		return core.None, true
	case "__type_params__":
		// Check if __type_params__ was explicitly set
		if val, ok := f.BaseObject.GetAttr("__type_params__"); ok {
			return val, true
		}
		// Return empty tuple for type parameters by default
		return core.TupleValue{}, true
	case "__hash__":
		// Return a hash function for the function
		// Python functions are hashable by identity (pointer address)
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			// Use the function's memory address as hash
			// This makes functions hashable so they can be dict keys
			hashStr := fmt.Sprintf("%p", f)
			// Convert hex string to number (simple hash)
			var hashNum int64
			fmt.Sscanf(hashStr, "%x", &hashNum)
			return core.NumberValue(float64(hashNum)), nil
		}), true
	case "__get__":
		// Return a descriptor function that binds this function to an instance
		// When called as func.__get__(instance, owner), it returns a bound method
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			// Descriptor protocol: __get__(self, instance, owner)
			// args[0] is instance (or None if accessed on class)
			// args[1] is owner class (optional)

			if len(args) < 1 {
				return nil, fmt.Errorf("__get__() requires at least 1 argument (instance), got %d", len(args))
			}

			instance := args[0]

			// If instance is None, return the function itself (class-level access)
			if instance == core.None || instance == core.Nil {
				return f, nil
			}

			// Create a bound method
			// Check if this is an Instance
			if inst, ok := instance.(*core.Instance); ok {
				return &core.BoundInstanceMethod{
					Instance:      inst,
					Method:        f,
					DefiningClass: inst.Class,
				}, nil
			}

			// For non-Instance objects, return the function unbound
			// (this handles edge cases where __get__ is called on non-instances)
			return f, nil
		}), true
	default:
		return f.BaseObject.GetAttr(name)
	}
}

// SetAttr implements attribute setting for user functions
func (f *UserFunction) SetAttr(name string, value core.Value) error {
	// Allow setting special attributes
	switch name {
	case "__name__":
		// Update the function name
		if str, ok := value.(core.StringValue); ok {
			f.name = string(str)
		}
		return f.BaseObject.SetAttr(name, value)
	case "__qualname__", "__module__", "__doc__", "__annotations__", "__type_params__":
		// Store in BaseObject for later retrieval
		return f.BaseObject.SetAttr(name, value)
	default:
		// Allow setting arbitrary attributes
		return f.BaseObject.SetAttr(name, value)
	}
}

// lambdaForm implements the lambda special form
func lambdaForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() < 2 {
		return nil, fmt.Errorf("lambda requires at least 2 arguments: parameters and body")
	}

	// Get the parameter list
	firstArg := unwrapLocated(args.Items()[0])
	paramList, ok := firstArg.(*core.ListValue)
	if !ok {
		return nil, fmt.Errorf("lambda: parameters must be a list, got %T: %#v", firstArg, firstArg)
	}

	// Try to parse as new-style parameter list with defaults
	signature, err := ParseParameterList(paramList.Items())
	if err != nil {
		// Fall back to legacy simple parameter parsing
		params := make([]core.SymbolValue, 0, paramList.Len())
		for _, p := range paramList.Items() {
			pVal := unwrapLocated(p)
			sym, ok := pVal.(core.SymbolValue)
			if !ok {
				return nil, fmt.Errorf("lambda: parameters must be symbols")
			}
			params = append(params, sym)
		}

		// The body is the rest of the expressions wrapped in a do form
		var body core.Value
		if args.Len() == 2 {
			body = args.Items()[1]
		} else {
			// Multiple expressions - wrap in do
			body = core.NewList(append([]core.Value{core.SymbolValue("do")}, args.Items()[1:]...)...)
		}

		// Create the function with legacy params
		fn := &UserFunction{
			BaseObject: *core.NewBaseObject(core.FunctionType),
			params:     params,
			body:       body,
			env:        ctx, // Capture current environment
			isLambda:   true,
		}

		// Check if this is a generator function
		return makeGeneratorFunction(fn), nil
	}

	// New-style function with signature
	// The body is the rest of the expressions wrapped in a do form
	var body core.Value
	if args.Len() == 2 {
		body = args.Items()[1]
	} else {
		// Multiple expressions - wrap in do
		body = core.NewList(append([]core.Value{core.SymbolValue("do")}, args.Items()[1:]...)...)
	}

	// Build legacy params list for backward compatibility
	var params []core.SymbolValue
	for _, p := range signature.RequiredParams {
		params = append(params, p.Name)
	}
	for _, p := range signature.OptionalParams {
		params = append(params, p.Name)
	}

	// Evaluate default values at definition time (matches Python semantics)
	if err := evaluateDefaultValues(signature, ctx); err != nil {
		return nil, err
	}

	// Create the function with signature
	fn := &UserFunction{
		BaseObject: *core.NewBaseObject(core.FunctionType),
		params:     params,
		signature:  signature,
		body:       body,
		env:        ctx, // Capture current environment
		isLambda:   true,
	}

	// Check if this is a generator function
	return makeGeneratorFunction(fn), nil
}
