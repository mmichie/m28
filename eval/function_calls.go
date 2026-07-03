// Package eval provides function call evaluation
package eval

import (
	"fmt"
	"strings"

	"github.com/mmichie/m28/core"
)

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
	name      string          // Optional function name
	isLambda  bool            // True for lambda expressions (implicitly return body value)
	docstring string          // Function docstring (first string literal in body)
	dict      *core.DictValue // Live __dict__ for arbitrary user-set attributes

	// __defaults__ / __kwdefaults__ overrides. nil means "not overridden" (use
	// the signature as defined). When set (to a tuple/dict, or to None via
	// assignment or del), the call path uses a signature rebuilt from
	// origSignature; the getters return the stored override directly. This keeps
	// the rebuild on the cold attribute-set path, leaving normal calls untouched.
	defaultsOverride   core.Value         // nil unset; None=no positional defaults; TupleValue=defaults
	kwdefaultsOverride core.Value         // nil unset; None=no kw-only defaults; *DictValue=kw defaults
	origSignature      *FunctionSignature // signature as originally defined (captured on first override)

	// Resolution-layer state, computed lazily on first call (ensureResolved).
	// When slotBody != nil the function runs on a flat slot frame (callSlots)
	// instead of map-based scope; otherwise it uses the ordinary path. Computed
	// once per function object; resolveTried also caches the negative result.
	resolveTried bool
	slotBody     core.Value // rewritten body with slotRefs; nil => not slot-eligible
	numSlots     int        // size of the slot frame
	numParams    int        // positional params, bound into slots 0..numParams-1
}

// ensureResolved runs the slot-frame analysis once. Afterwards f.slotBody is
// non-nil iff the function can execute on a slot frame: it needs a simple
// signature (only required positional params — no defaults, *args, **kwargs, or
// keyword-only params, so positional args map 1:1 onto the leading slots) and a
// body the resolution pass fully understands. Anything else leaves slotBody nil
// and the function keeps using map-based scope.
func (f *UserFunction) ensureResolved() {
	if f.resolveTried {
		return
	}
	f.resolveTried = true

	paramNames, ok := f.simpleParamNames()
	if !ok {
		return
	}
	res, ok := analyzeLocals(paramNames, f.body, isSpecialFormName)
	if !ok {
		return
	}
	rewritten, ok := resolveBody(f.body, res.slots, isSpecialFormName)
	if !ok {
		return
	}
	// Resolution layer 3: compile the slot-rewritten body into IR nodes that
	// evaluate themselves directly. compileIR never fails — anything it does not
	// model stays as the rewritten value and runs the layer-2 way through Eval.
	compiled := compileIR(rewritten)
	// Publish numParams/numSlots before slotBody so a reader that sees a
	// non-nil slotBody also sees consistent frame sizes.
	f.numParams = len(paramNames)
	f.numSlots = len(res.slots)
	f.slotBody = compiled
}

// simpleParamNames returns the positional parameter names in declaration order,
// or ok=false when the signature is anything callSlots cannot bind by position
// alone (defaults, *args, **kwargs, or keyword-only params). The legacy
// params-only path is always simple.
func (f *UserFunction) simpleParamNames() ([]string, bool) {
	if f.signature != nil {
		sig := f.signature
		if sig.RestParam != nil || sig.KeywordParam != nil || len(sig.OptionalParams) > 0 {
			return nil, false
		}
		names := make([]string, 0, len(sig.RequiredParams))
		for _, p := range sig.RequiredParams {
			if p.KeywordOnly {
				return nil, false
			}
			names = append(names, string(p.Name))
		}
		return names, true
	}
	names := make([]string, len(f.params))
	for i, p := range f.params {
		names[i] = string(p)
	}
	return names, true
}

// callSlots executes a slot-eligible function: it binds positional args to the
// leading slots and evaluates the pre-resolved body against the slot frame,
// bypassing map-based scope. The caller guarantees len(args) == f.numParams and
// no keyword arguments, so binding is a positional copy.
func (f *UserFunction) callSlots(args []core.Value, ctx *core.Context) (core.Value, error) {
	core.TraceEnterFunction(f.name, args)

	// Pooled frame context (M28-xi1): recycled only on the clean-return paths
	// below; error returns leave the context to the GC (see the escape audit
	// at core.frameCtxPool).
	funcEnv := core.AcquireFrameContext(f.env, f.numSlots)
	// Track call depth to raise RecursionError before the Go stack overflows.
	callerDepth := 0
	if ctx != nil {
		callerDepth = ctx.Depth
	}
	funcEnv.Depth = callerDepth + 1
	if funcEnv.Depth > core.GetRecursionLimit() {
		return nil, &core.RecursionError{Message: "maximum recursion depth exceeded"}
	}

	// Slots 0..numParams-1 hold the parameters in order; later slots start
	// unbound until first assignment (read-before-assign => UnboundLocalError).
	// Binding goes through SlotFrame.Set — the tag choke point.
	for i, a := range args {
		funcEnv.Locals.Set(i, a)
	}

	result, err := Eval(f.slotBody, funcEnv)
	if err != nil {
		core.TraceExitFunction(f.name, nil, err)
		return nil, err
	}

	if ret, ok := result.(*ReturnValue); ok {
		core.TraceExitFunction(f.name, ret.Value, nil)
		core.ReleaseFrameContext(funcEnv)
		return ret.Value, nil
	}
	if _, ok := result.(*core.YieldValue); ok {
		return nil, fmt.Errorf("yield outside of generator function")
	}
	// Lambda bodies implicitly return their value; def bodies return None.
	if f.isLambda {
		core.TraceExitFunction(f.name, result, nil)
		core.ReleaseFrameContext(funcEnv)
		return result, nil
	}
	core.TraceExitFunction(f.name, core.None, nil)
	core.ReleaseFrameContext(funcEnv)
	return core.None, nil
}

// applyDefaultOverrides rebuilds f.signature from f.origSignature to reflect the
// current __defaults__ / __kwdefaults__ overrides. __defaults__ supplies default
// values for the trailing positional parameters (CPython maps the tuple to the
// last len(tuple) of them); __kwdefaults__ supplies defaults for keyword-only
// parameters by name. A parameter that loses its default becomes required.
func (f *UserFunction) applyDefaultOverrides() {
	if f.origSignature == nil {
		return
	}
	orig := f.origSignature

	// Split original params into positional and keyword-only, preserving order.
	var pos, kw []ParameterInfo
	collect := func(params []ParameterInfo) {
		for _, p := range params {
			if p.KeywordOnly {
				kw = append(kw, p)
			} else {
				pos = append(pos, p)
			}
		}
	}
	collect(orig.RequiredParams)
	collect(orig.OptionalParams)

	// __defaults__: map the trailing positional params to the override tuple.
	if f.defaultsOverride != nil {
		for i := range pos {
			pos[i].HasDefault = false
			pos[i].DefaultValue = nil
		}
		if tup, ok := f.defaultsOverride.(core.TupleValue); ok {
			start := len(pos) - len(tup)
			if start < 0 {
				start = 0
			}
			for i := start; i < len(pos); i++ {
				pos[i].HasDefault = true
				pos[i].DefaultValue = tup[i-start]
			}
		}
	}

	// __kwdefaults__: keyword-only params present in the dict get that default;
	// the rest become required.
	if f.kwdefaultsOverride != nil {
		dict, _ := f.kwdefaultsOverride.(*core.DictValue)
		for i := range kw {
			kw[i].HasDefault = false
			kw[i].DefaultValue = nil
			if dict != nil {
				if v, found := dict.GetValue(core.StringValue(string(kw[i].Name))); found {
					kw[i].HasDefault = true
					kw[i].DefaultValue = v
				}
			}
		}
	}

	// Rebuild Required (no default) then Optional (with default), positional
	// before keyword-only within each so BindArguments binds positionals in order.
	var req, opt []ParameterInfo
	for _, group := range [][]ParameterInfo{pos, kw} {
		for _, p := range group {
			if p.HasDefault {
				opt = append(opt, p)
			} else {
				req = append(req, p)
			}
		}
	}

	f.signature = &FunctionSignature{
		RequiredParams: req,
		OptionalParams: opt,
		RestParam:      orig.RestParam,
		KeywordParam:   orig.KeywordParam,
	}
}

// ensureDict lazily creates the function's live __dict__ used to store
// arbitrary user-set attributes (e.g. func.x = 1 or func.__dict__['x'] = 1).
// Returning the same object every time makes func.__dict__ a live mapping,
// matching CPython semantics.
func (f *UserFunction) ensureDict() *core.DictValue {
	if f.dict == nil {
		f.dict = core.NewDict()
	}
	return f.dict
}

// Call implements Callable.Call
func (f *UserFunction) Call(args []core.Value, ctx *core.Context) (core.Value, error) {
	// Resolution layer: a simple-signature function with a slot-compiled body
	// runs on a flat slot frame, given the right positional arity. callSlots
	// owns its own tracing, so this returns before the map-based path below.
	f.ensureResolved()
	if f.slotBody != nil && len(args) == f.numParams {
		return f.callSlots(args, ctx)
	}

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
	// The captured f.env already contains the correct __class__ for super() to use.
	// We do NOT copy __class__ from the call context because that would break
	// super() in nested classes - the defining class's __class__ is correct, not the caller's.
	funcEnv := core.NewContext(f.env)
	funcEnv.IsFunctionScope = true
	// Track call depth to raise RecursionError before the Go stack overflows.
	// ctx can be nil on some internal call paths (e.g. repr/hash), so guard it.
	callerDepth := 0
	if ctx != nil {
		callerDepth = ctx.Depth
	}
	funcEnv.Depth = callerDepth + 1
	if funcEnv.Depth > core.GetRecursionLimit() {
		return nil, &core.RecursionError{Message: "maximum recursion depth exceeded"}
	}
	core.DebugLog("[CALL] Created funcEnv for %s\n", f.name)

	// Use new signature-based binding if available
	if f.signature != nil {
		// For now, we don't support keyword arguments in calls, just positional
		err := f.signature.BindArguments(f.name, args, nil, f.env, funcEnv)
		if err != nil {
			return nil, err
		}
	} else {
		// Legacy: simple parameter binding
		if len(args) != len(f.params) {
			return nil, &core.TypeError{Message: fmt.Sprintf("expected %d arguments, got %d", len(f.params), len(args))}
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
	// Resolution layer: when called purely positionally with the right arity, a
	// slot-eligible function uses the slot frame. Any keyword arguments fall
	// through to the map-based path, which binds them correctly.
	f.ensureResolved()
	if f.slotBody != nil && len(kwargs) == 0 && len(args) == f.numParams {
		return f.callSlots(args, ctx)
	}

	// Trace function entry
	core.TraceEnterFunction(f.name, args)

	// Create a new environment with the function's environment as parent
	// The captured f.env already contains the correct __class__ for super() to use.
	// We do NOT copy __class__ from the call context because that would break
	// super() in nested classes - the defining class's __class__ is correct, not the caller's.
	funcEnv := core.NewContext(f.env)
	funcEnv.IsFunctionScope = true
	// Track call depth to raise RecursionError before the Go stack overflows.
	// ctx can be nil on some internal call paths (e.g. repr/hash), so guard it.
	callerDepth := 0
	if ctx != nil {
		callerDepth = ctx.Depth
	}
	funcEnv.Depth = callerDepth + 1
	if funcEnv.Depth > core.GetRecursionLimit() {
		return nil, &core.RecursionError{Message: "maximum recursion depth exceeded"}
	}

	// Use new signature-based binding if available
	if f.signature != nil {
		// Signature supports keyword arguments
		err := f.signature.BindArguments(f.name, args, kwargs, f.env, funcEnv)
		if err != nil {
			core.TraceExitFunction(f.name, nil, err)
			return nil, err
		}
	} else {
		// Legacy: simple parameter binding - no kwargs support
		if len(kwargs) > 0 {
			err := fmt.Errorf("function %s does not support keyword arguments", f.name)
			core.TraceExitFunction(f.name, nil, err)
			return nil, err
		}
		if len(args) != len(f.params) {
			err := &core.TypeError{Message: fmt.Sprintf("expected %d arguments, got %d", len(f.params), len(args))}
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
		// Return the captured docstring if available
		if f.docstring != "" {
			return core.StringValue(f.docstring), true
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
			// New-style function with signature. Keyword-only params (those after
			// the bare * separator) are tracked via ParameterInfo.KeywordOnly and
			// counted separately, so co_argcount counts only positional params and
			// co_kwonlyargcount counts the keyword-only ones (matching CPython).
			for _, param := range f.signature.RequiredParams {
				if param.KeywordOnly {
					kwonlyargcount++
				} else {
					argCount++
				}
			}
			for _, param := range f.signature.OptionalParams {
				if param.KeywordOnly {
					kwonlyargcount++
				} else {
					argCount++
				}
			}

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
		// Return the function's live namespace. This is the SAME object every
		// time, so func.__dict__['x'] = 1 and func.__dict__.update(...) mutate
		// the function's attributes, matching CPython semantics.
		return f.ensureDict(), true
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
		// Lazily create and store a stable empty dict so that repeated access
		// returns the same object (func.__annotations__ is func.__annotations__).
		annots := core.NewDict()
		f.BaseObject.SetAttr("__annotations__", annots)
		return annots, true
	case "__defaults__":
		// An explicit override (set or deleted) takes precedence.
		if f.defaultsOverride != nil {
			return f.defaultsOverride, true
		}
		// Tuple of default values for positional parameters, or None when there
		// are none. Keyword-only defaults live in __kwdefaults__, so they are
		// excluded here (matching CPython).
		if f.signature != nil {
			var defaults []core.Value
			for _, param := range f.signature.OptionalParams {
				if param.KeywordOnly {
					continue
				}
				if param.DefaultValue != nil {
					// Unwrap the source-location wrapper so a literal default is
					// returned as the real value (CPython evaluates defaults at
					// def time); otherwise __defaults__ holds a LocatedValue that
					// fails == and arithmetic against the bare value.
					defaults = append(defaults, unwrapLocated(param.DefaultValue))
				}
			}
			if len(defaults) > 0 {
				return core.TupleValue(defaults), true
			}
		}
		return core.None, true
	case "__kwdefaults__":
		// An explicit override (set or deleted) takes precedence.
		if f.kwdefaultsOverride != nil {
			return f.kwdefaultsOverride, true
		}
		// Dict of default values for keyword-only parameters, or None when no
		// keyword-only parameter has a default (CPython semantics).
		if f.signature != nil {
			kwdefaults := core.NewDict()
			found := false
			for _, param := range f.signature.OptionalParams {
				if param.KeywordOnly && param.DefaultValue != nil {
					if err := kwdefaults.SetValue(core.StringValue(string(param.Name)), unwrapLocated(param.DefaultValue)); err == nil {
						found = true
					}
				}
			}
			if found {
				return kwdefaults, true
			}
		}
		return core.None, true
	case "__type_params__":
		// Check if __type_params__ was explicitly set
		if val, ok := f.BaseObject.GetAttr("__type_params__"); ok {
			return val, true
		}
		// Return empty tuple for type parameters by default
		return core.TupleValue{}, true
	case "__eq__":
		// Python functions compare equal by identity (same object = equal)
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("__eq__ requires 1 argument")
			}
			// Identity comparison - same pointer means equal
			if args[0] == f {
				return core.True, nil
			}
			// Also check if it's wrapped in another type but same pointer
			if other, ok := args[0].(*UserFunction); ok {
				if other == f {
					return core.True, nil
				}
			}
			return core.False, nil
		}), true
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
	case "__call__":
		// Functions are callable via __call__
		// func.__call__(args) is equivalent to func(args)
		// This is important for decorators that use func.__call__
		return f, true
	default:
		// Arbitrary user-set attributes live in the function's __dict__.
		if f.dict != nil {
			if v, ok := f.dict.GetValue(core.StringValue(name)); ok {
				return v, true
			}
		}
		return f.BaseObject.GetAttr(name)
	}
}

// SetAttr implements attribute setting for user functions
func (f *UserFunction) SetAttr(name string, value core.Value) error {
	// Allow setting special attributes
	switch name {
	case "__name__":
		// __name__ must be a string (CPython raises TypeError otherwise).
		str, ok := value.(core.StringValue)
		if !ok {
			return &core.TypeError{Message: fmt.Sprintf("__name__ must be set to a string object, not '%s'", value.Type())}
		}
		f.name = string(str)
		return f.BaseObject.SetAttr(name, value)
	case "__qualname__":
		// __qualname__ must be a string and is independent of __name__.
		if _, ok := value.(core.StringValue); !ok {
			return &core.TypeError{Message: fmt.Sprintf("__qualname__ must be set to a string object, not '%s'", value.Type())}
		}
		return f.BaseObject.SetAttr(name, value)
	case "__module__", "__doc__", "__annotations__", "__type_params__":
		// Store in BaseObject for later retrieval
		return f.BaseObject.SetAttr(name, value)
	case "__defaults__":
		// Replacing __defaults__ changes the defaults of the trailing positional
		// parameters (and which parameters are required) for subsequent calls.
		switch value.(type) {
		case core.TupleValue, core.NilValue:
		default:
			return &core.TypeError{Message: fmt.Sprintf("__defaults__ must be set to a tuple object, not '%s'", value.Type())}
		}
		if f.origSignature == nil {
			f.origSignature = f.signature
		}
		f.defaultsOverride = value
		f.applyDefaultOverrides()
		return nil
	case "__kwdefaults__":
		// Replacing __kwdefaults__ changes the defaults of keyword-only params.
		switch value.(type) {
		case *core.DictValue, core.NilValue:
		default:
			return &core.TypeError{Message: fmt.Sprintf("__kwdefaults__ must be set to a dict object, not '%s'", value.Type())}
		}
		if f.origSignature == nil {
			f.origSignature = f.signature
		}
		f.kwdefaultsOverride = value
		f.applyDefaultOverrides()
		return nil
	case "__dict__":
		// Replacing __dict__ wholesale: adopt the given dict as the live store.
		if d, ok := value.(*core.DictValue); ok {
			f.dict = d
			return nil
		}
		return fmt.Errorf("__dict__ must be set to a dictionary, not '%s'", value.Type())
	default:
		// Arbitrary user-set attributes live in the function's live __dict__.
		return f.ensureDict().SetValue(core.StringValue(name), value)
	}
}

// DelAttr deletes a function attribute (e.g. `del func.x`). Arbitrary user-set
// attributes live in the function's __dict__; a few structural attributes
// cannot be deleted, matching CPython.
func (f *UserFunction) DelAttr(name string) error {
	switch name {
	case "__name__", "__qualname__", "__dict__":
		return &core.AttributeError{ObjType: "function", Message: fmt.Sprintf("__%s__ may not be deleted", name)}
	case "__defaults__":
		// del func.__defaults__ removes all positional defaults (CPython sets it
		// to None), making those parameters required again.
		if f.origSignature == nil {
			f.origSignature = f.signature
		}
		f.defaultsOverride = core.None
		f.applyDefaultOverrides()
		return nil
	case "__kwdefaults__":
		if f.origSignature == nil {
			f.origSignature = f.signature
		}
		f.kwdefaultsOverride = core.None
		f.applyDefaultOverrides()
		return nil
	}
	if f.dict != nil && f.dict.DeleteValue(core.StringValue(name)) {
		return nil
	}
	// Fall back to special attributes stored on the BaseObject (e.g. __doc__).
	if err := f.BaseObject.DelAttr(name); err == nil {
		return nil
	}
	return &core.AttributeError{ObjType: "function", Message: fmt.Sprintf("'function' object has no attribute '%s'", name)}
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

	// Unwrap LocatedValue wrappers so default-valued params like `(i 5)` are
	// seen as plain lists (matches how def passes its params). Without this,
	// ParseParameterList rejects `lambda i=5: i` and we fall back to the
	// symbols-only path, which errors on the default.
	rawItems := paramList.Items()
	paramItems := make([]core.Value, len(rawItems))
	for idx, it := range rawItems {
		paramItems[idx] = unwrapLocated(it)
	}

	// Try to parse as new-style parameter list with defaults
	signature, err := ParseParameterList(paramItems)
	if err != nil {
		// Fall back to legacy simple parameter parsing
		params := make([]core.SymbolValue, 0, len(paramItems))
		for _, p := range paramItems {
			sym, ok := p.(core.SymbolValue)
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
		setFunctionModule(fn, ctx)

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
	setFunctionModule(fn, ctx)

	// Check if this is a generator function
	return makeGeneratorFunction(fn), nil
}
