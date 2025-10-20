// Package eval provides the evaluation system for M28 expressions.
package eval

import (
	"fmt"
	"strings"

	"github.com/mmichie/m28/core"
)

func init() {
	// Set the EvalHook so that builtin eval() can work
	core.EvalHook = Eval
}

// Eval evaluates an expression in a context
func Eval(expr core.Value, ctx *core.Context) (core.Value, error) {
	switch v := expr.(type) {
	case core.NumberValue, core.StringValue, core.BoolValue, core.NilValue:
		// Self-evaluating primitives
		return v, nil

	case core.SymbolValue:
		// Variable lookup
		val, err := ctx.Lookup(string(v))
		if err != nil {
			return nil, core.WrapEvalError(err, "name error", ctx)
		}
		return val, nil

	case *core.ListValue:
		// Empty list evaluates to itself
		if v.Len() == 0 {
			return core.EmptyList, nil
		}

		// Check if it's a decorator form (@decorator ...)
		if isDecoratorForm(v) {
			return evalDecoratorForm(v, ctx)
		}

		// Check if it's a special form first (if, def, etc.)
		if sym, ok := v.Items()[0].(core.SymbolValue); ok {
			if handler, ok := specialForms[string(sym)]; ok {
				return handler(core.NewList(v.Items()[1:]...), ctx)
			}
		}

		// Check if it's a macro call (function with __macro__ attribute)
		if sym, ok := v.Items()[0].(core.SymbolValue); ok {
			if isMacroCall(sym, ctx) {
				return evalMacroCall(v, ctx)
			}
		}

		// Otherwise it's a function call
		return evalFunctionCallWithKeywords(v, ctx)

	default:
		// Other values evaluate to themselves
		return expr, nil
	}
}

// evalFunctionCall evaluates a function call expression
func evalFunctionCall(expr *core.ListValue, ctx *core.Context) (core.Value, error) {
	// Check if the function is referenced by a symbol (for better error messages)
	var symbolName string
	if sym, ok := expr.Items()[0].(core.SymbolValue); ok {
		symbolName = string(sym)
	}

	// Evaluate the function
	fn, err := Eval(expr.Items()[0], ctx)
	if err != nil {
		return nil, err
	}

	// Evaluate the arguments
	args := make([]core.Value, 0, expr.Len()-1)
	for _, arg := range expr.Items()[1:] {
		evalArg, err := Eval(arg, ctx)
		if err != nil {
			return nil, err
		}
		args = append(args, evalArg)
	}

	// Call the function
	callable, ok := fn.(core.Callable)
	if !ok {
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

	ctx.PushStack(funcName, "", 0, 0)
	defer ctx.PopStack()

	result, err := callable.Call(args, ctx)
	if err != nil {
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

// SpecialFormHandler handles special forms like if, def, etc.
type SpecialFormHandler func(args *core.ListValue, ctx *core.Context) (core.Value, error)

// specialForms maps special form names to their handlers
var specialForms map[string]SpecialFormHandler

// specialFormsRegistry tracks where special forms are registered for duplicate detection
var specialFormsRegistry = core.NewRegistry("special form")

func init() {
	specialForms = map[string]SpecialFormHandler{
		// Control flow
		"if":     ifForm,
		"do":     doForm,
		"return": returnForm,

		// Definitions
		// Note: "def" is registered by special_forms/register.go
		"=":      assignForm,
		"quote":  quoteForm,
		"lambda": lambdaForm,
		"fn":     lambdaForm, // Alias for lambda

		// Module system (will be overridden by enhanced version)
		// "import": importForm,

		// Exception handling
		"try":   tryForm,
		"raise": raiseForm,

		// Comprehensions
		"list-comp": ListCompForm,
		"dict-comp": DictCompForm,
		"set-comp":  SetCompForm,
		"gen-expr":  GenExprForm,
		"gen-comp":  GenExprForm, // Alias for gen-expr

		// List literal (evaluates contents)
		"list-literal": listLiteralForm,

		// Tuple literal (evaluates contents)
		"tuple-literal": tupleLiteralForm,

		// Other special forms will be added through RegisterSpecialForm
	}

	// Register the initial forms in the registry to track them
	for name, handler := range specialForms {
		specialFormsRegistry.Register(name, handler)
	}

	// Register enhanced module forms
	RegisterModuleForms()

	// Register class forms
	RegisterClassForms()

	// Register generator forms
	RegisterGeneratorForms()

	// Register context manager forms
	RegisterContextForms()

	// Register async/concurrent forms
	RegisterAsyncForms()

	// Register dot notation
	RegisterDotNotation()

	// Register indexing
	RegisterIndexing()

	// Register augmented assignment
	RegisterAugmentedAssignment()

	// Register s-string forms
	RegisterSStringForms()

	// Register quasiquote forms
	RegisterQuasiquoteForms()
}

// StrictDuplicateChecking controls whether duplicate registrations cause a panic
var StrictDuplicateChecking = false

// RegisterSpecialForm registers a special form with duplicate detection
func RegisterSpecialForm(name string, handler SpecialFormHandler) {
	if err := specialFormsRegistry.Register(name, handler); err != nil {
		if StrictDuplicateChecking {
			panic(err)
		}
		// In non-strict mode, allow overwrites but log them
		// This preserves existing behavior where special_forms/register.go
		// intentionally overrides some handlers with delegating versions
		// fmt.Printf("Warning: %v\n", err)
	}
	specialForms[name] = handler
}

// ifForm delegates to IfForm in util.go
func ifForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	// Delegate to util.go implementation which supports elif
	return IfForm(args, ctx)
}

// assignForm implements the = special form for assignment
func assignForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() != 2 {
		return nil, fmt.Errorf("= requires 2 arguments, got %d", args.Len())
	}

	// Get the target
	target := args.Items()[0]

	// Check if target is a list
	if targetList, ok := target.(*core.ListValue); ok {
		// Check if it's a special form (indexed or property assignment)
		if targetList.Len() > 0 {
			if sym, ok := targetList.Items()[0].(core.SymbolValue); ok {
				switch string(sym) {
				case "get-item":
					// Indexed assignment: lst[i] = value
					if targetList.Len() != 3 {
						return nil, fmt.Errorf("invalid index expression")
					}
					// Evaluate the value
					value, err := Eval(args.Items()[1], ctx)
					if err != nil {
						return nil, err
					}
					// Use SetItemForm to handle the assignment
					return SetItemForm(core.NewList(targetList.Items()[1], targetList.Items()[2], value), ctx)

				case ".":
					// Property assignment: obj.prop = value
					// This is handled by dot notation already
					break
				}
			}
		}

		// Tuple unpacking: (= (x, y) (10, 20))
		// Evaluate the value
		value, err := Eval(args.Items()[1], ctx)
		if err != nil {
			return nil, err
		}

		// The value must be a tuple or list
		var values []core.Value
		switch v := value.(type) {
		case core.TupleValue:
			values = []core.Value(v)
		case *core.ListValue:
			values = v.Items()
		default:
			return nil, fmt.Errorf("cannot unpack non-sequence %v", value.Type())
		}

		// Check that the number of targets matches the number of values
		if targetList.Len() != len(values) {
			return nil, fmt.Errorf("too many values to unpack (expected %d, got %d)", targetList.Len(), len(values))
		}

		// Assign each value to its corresponding target
		for i, t := range targetList.Items() {
			sym, ok := t.(core.SymbolValue)
			if !ok {
				return nil, fmt.Errorf("assignment target must be a symbol, got %v", t.Type())
			}
			ctx.Define(string(sym), values[i])
		}

		return value, nil
	}

	// Single variable assignment
	sym, ok := target.(core.SymbolValue)
	if !ok {
		return nil, fmt.Errorf("assignment target must be a symbol, got %v", target.Type())
	}

	// Evaluate the value
	value, err := Eval(args.Items()[1], ctx)
	if err != nil {
		return nil, err
	}

	// Variable assignment - always define/update in current scope
	symName := string(sym)
	ctx.Define(symName, value)
	return value, nil
}

// quoteForm implements the quote special form
func quoteForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() != 1 {
		return nil, fmt.Errorf("quote requires 1 argument")
	}

	// Return the argument unevaluated
	return args.Items()[0], nil
}

// doForm implements the do special form for grouping expressions
func doForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() == 0 {
		return core.Nil, nil
	}

	// Evaluate all expressions in sequence
	var result core.Value = core.Nil
	var err error

	for _, expr := range args.Items() {
		result, err = Eval(expr, ctx)
		if err != nil {
			return nil, err
		}

		// Check for return value
		if ret, ok := result.(*ReturnValue); ok {
			return ret.Value, nil
		}
	}

	// Return the value of the last expression
	return result, nil
}

// returnForm implements the return special form
func returnForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() > 1 {
		return nil, fmt.Errorf("return takes at most 1 argument")
	}

	var value core.Value = core.Nil
	var err error

	if args.Len() == 1 {
		value, err = Eval(args.Items()[0], ctx)
		if err != nil {
			return nil, err
		}
	}

	return &ReturnValue{Value: value}, nil
}

// importForm implements the import special form
func importForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() != 1 {
		return nil, fmt.Errorf("import requires 1 argument")
	}

	// Get the module name
	var moduleName string
	switch name := args.Items()[0].(type) {
	case core.StringValue:
		moduleName = string(name)
	case core.SymbolValue:
		moduleName = string(name)
	default:
		return nil, fmt.Errorf("import: argument must be a string or symbol")
	}

	// Get the module loader
	loader := core.GetModuleLoader()
	if loader == nil {
		return nil, fmt.Errorf("no module loader registered")
	}

	// Load the module
	module, err := loader.LoadModule(moduleName, ctx)
	if err != nil {
		return nil, fmt.Errorf("failed to import module %s: %v", moduleName, err)
	}

	// Store module in the context
	ctx.Define(moduleName, module)

	return module, nil
}

// ReturnValue represents a return value from a function
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
}

// Call implements Callable.Call
func (f *UserFunction) Call(args []core.Value, ctx *core.Context) (core.Value, error) {
	// Create a new environment with the function's environment as parent
	funcEnv := core.NewContext(f.env)

	// If __class__ is defined in the call context, copy it to the function environment
	// This allows super() to know which class's method is being executed
	if ctx != nil {
		if classVal, err := ctx.Lookup("__class__"); err == nil {
			funcEnv.Define("__class__", classVal)
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
	result, err := Eval(f.body, funcEnv)
	if err != nil {
		return nil, err
	}

	// Handle return values
	if ret, ok := result.(*ReturnValue); ok {
		return ret.Value, nil
	}

	// Check if we got a yield value (shouldn't happen in normal functions)
	if _, ok := result.(*core.YieldValue); ok {
		return nil, fmt.Errorf("yield outside of generator function")
	}

	return result, nil
}

// CallWithKwargs calls the function with positional and keyword arguments
func (f *UserFunction) CallWithKwargs(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
	// Create a new environment with the function's environment as parent
	funcEnv := core.NewContext(f.env)

	// If __class__ is defined in the call context, copy it to the function environment
	// This allows super() to know which class's method is being executed
	if classVal, err := ctx.Lookup("__class__"); err == nil {
		funcEnv.Define("__class__", classVal)
	}

	// Use new signature-based binding if available
	if f.signature != nil {
		// Signature supports keyword arguments
		err := f.signature.BindArguments(args, kwargs, f.env, funcEnv)
		if err != nil {
			return nil, err
		}
	} else {
		// Legacy: simple parameter binding - no kwargs support
		if len(kwargs) > 0 {
			return nil, fmt.Errorf("function does not support keyword arguments")
		}
		if len(args) != len(f.params) {
			return nil, fmt.Errorf("expected %d arguments, got %d", len(f.params), len(args))
		}

		for i, param := range f.params {
			funcEnv.Define(string(param), args[i])
		}
	}

	// Evaluate the body in the new environment
	result, err := Eval(f.body, funcEnv)
	if err != nil {
		return nil, err
	}

	// Handle return values
	if ret, ok := result.(*ReturnValue); ok {
		return ret.Value, nil
	}

	return result, nil
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
		// Return a code object (CodeType)
		// Python's types.py uses __code__ to get the CodeType
		return core.NewCodeObject(f), true
	case "__globals__":
		// Return the function's global namespace
		// This is the environment where the function was defined
		globalsDict := core.NewDict()
		// In a real implementation, we would populate this with the actual globals
		// For now, return an empty dict to satisfy type() call in types.py
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
	case "__type_params__":
		// Check if __type_params__ was explicitly set
		if val, ok := f.BaseObject.GetAttr("__type_params__"); ok {
			return val, true
		}
		// Return empty tuple for type parameters by default
		return core.TupleValue{}, true
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
	paramList, ok := args.Items()[0].(*core.ListValue)
	if !ok {
		return nil, fmt.Errorf("lambda: parameters must be a list, got %T: %#v", args.Items()[0], args.Items()[0])
	}

	// Try to parse as new-style parameter list with defaults
	signature, err := ParseParameterList(paramList.Items())
	if err != nil {
		// Fall back to legacy simple parameter parsing
		params := make([]core.SymbolValue, 0, paramList.Len())
		for _, p := range paramList.Items() {
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

	// Create the function with signature
	fn := &UserFunction{
		BaseObject: *core.NewBaseObject(core.FunctionType),
		params:     params,
		signature:  signature,
		body:       body,
		env:        ctx, // Capture current environment
	}

	// Check if this is a generator function
	return makeGeneratorFunction(fn), nil
}

// Exception represents a raised exception
type Exception struct {
	Type    string
	Message string
	Value   core.Value
}

func (e *Exception) Error() string {
	if e.Message != "" {
		return fmt.Sprintf("%s: %s", e.Type, e.Message)
	}
	return e.Type
}

// isExceptionType checks if a string is a known exception type
func isExceptionType(name string) bool {
	knownTypes := map[string]bool{
		"Exception":         true,
		"Error":             true,
		"NameError":         true,
		"TypeError":         true,
		"ValueError":        true,
		"ZeroDivisionError": true,
		"KeyError":          true,
		"IndexError":        true,
		"AttributeError":    true,
		"RuntimeError":      true,
		"FileNotFoundError": true,
		"PermissionError":   true,
	}
	return knownTypes[name]
}

// tryForm implements the try/except/finally special form
// Forms:
//
//	(try body)
//	(try body (except handler)) ; catch all
//	(try body (except var handler)) ; catch all with variable
//	(try body (except Type handler)) ; catch specific type
//	(try body (except Type var handler)) ; catch specific type with variable
//	(try body ... (finally cleanup))
func tryForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() == 0 {
		return nil, fmt.Errorf("try requires at least a body")
	}

	var tryBody []core.Value
	var exceptClauses []*core.ListValue
	var finallyClause *core.ListValue

	// Parse the try form
	for i, arg := range args.Items() {
		if list, ok := arg.(*core.ListValue); ok && list.Len() > 0 {
			if sym, ok := list.Items()[0].(core.SymbolValue); ok {
				switch string(sym) {
				case "except":
					if i == 0 {
						return nil, fmt.Errorf("try must have a body before except")
					}
					exceptClauses = append(exceptClauses, list)
					continue
				case "finally":
					if i == 0 {
						return nil, fmt.Errorf("try must have a body before finally")
					}
					finallyClause = list
					continue
				}
			}
		}

		// If we haven't seen except or finally yet, it's part of the try body
		if len(exceptClauses) == 0 && finallyClause == nil {
			tryBody = append(tryBody, arg)
		}
	}

	// Helper to run finally clause
	runFinally := func() error {
		if finallyClause != nil && finallyClause.Len() > 1 {
			for _, expr := range finallyClause.Items()[1:] {
				_, err := Eval(expr, ctx)
				if err != nil {
					return err
				}
			}
		}
		return nil
	}

	// Execute try body
	var result core.Value = core.Nil
	var tryErr error

	for _, expr := range tryBody {
		result, tryErr = Eval(expr, ctx)
		if tryErr != nil {
			break
		}
	}

	// If no error, run finally and return
	if tryErr == nil {
		if err := runFinally(); err != nil {
			return nil, err
		}
		return result, nil
	}

	// Handle the exception
	for _, exceptClause := range exceptClauses {
		if exceptClause.Len() < 2 {
			continue
		}

		// Parse except clause
		// Forms:
		// (except handler...) - catch all
		// (except var handler...) - catch all with variable binding (legacy)
		// (except Type handler...) - catch specific type
		// (except Type var handler...) - catch specific type with variable (legacy)
		// (except Type as var handler...) - catch specific type with variable (Python style)
		// (except as var handler...) - catch all with variable (Python style)
		var excType string
		var excVar string
		var handlerStart int = 1

		if exceptClause.Len() > 1 {
			// Check if second element is a symbol
			if sym, ok := exceptClause.Items()[1].(core.SymbolValue); ok {
				symStr := string(sym)

				// Check for "as" syntax for catch-all
				if symStr == "as" && exceptClause.Len() > 2 {
					// (except as var handler...)
					if varSym, ok := exceptClause.Items()[2].(core.SymbolValue); ok {
						excVar = string(varSym)
						handlerStart = 3
					}
				} else if isExceptionType(symStr) || (len(symStr) > 0 && symStr[0] >= 'A' && symStr[0] <= 'Z') {
					// It's an exception type
					excType = symStr
					handlerStart = 2

					// Check for "as" syntax
					if exceptClause.Len() > 3 && handlerStart == 2 {
						if asSym, ok := exceptClause.Items()[2].(core.SymbolValue); ok && string(asSym) == "as" {
							// (except Type as var handler...)
							if varSym, ok := exceptClause.Items()[3].(core.SymbolValue); ok {
								excVar = string(varSym)
								handlerStart = 4
							}
						}
					}

					// Legacy: Check if next element is a variable name (lowercase)
					if excVar == "" && exceptClause.Len() > 2 {
						if varSym, ok := exceptClause.Items()[2].(core.SymbolValue); ok {
							varStr := string(varSym)
							if len(varStr) > 0 && varStr[0] >= 'a' && varStr[0] <= 'z' {
								excVar = varStr
								handlerStart = 3
							}
						}
					}
				} else {
					// Legacy: It's a variable name for catch-all
					excVar = symStr
					handlerStart = 2
				}
			}
		}

		// Check if this except matches
		matches := false
		if excType == "" {
			// Catch-all
			matches = true
		} else {
			// Check exception type
			if exc, ok := tryErr.(*Exception); ok {
				matches = exc.Type == excType
			} else {
				// Check if it's wrapped in EvalError
				var baseErr error = tryErr
				if evalErr, ok := tryErr.(*core.EvalError); ok && evalErr.Wrapped != nil {
					baseErr = evalErr.Wrapped
				}

				// Match built-in error types
				switch baseErr.(type) {
				case *core.NameError:
					matches = excType == "NameError" || excType == "Exception"
				case *core.TypeError:
					matches = excType == "TypeError" || excType == "Exception"
				case *core.ZeroDivisionError:
					matches = excType == "ZeroDivisionError" || excType == "Exception"
				case *core.KeyError:
					matches = excType == "KeyError" || excType == "Exception"
				case *core.IndexError:
					matches = excType == "IndexError" || excType == "Exception"
				case *core.ImportError:
					matches = excType == "ImportError" || excType == "Exception"
				default:
					// Check if it's our custom Exception type
					if exc, ok := baseErr.(*Exception); ok {
						matches = exc.Type == excType || excType == "Exception"
					} else {
						matches = excType == "Exception" || excType == "Error"
					}
				}
			}
		}

		if matches {
			// Use parent context for handler to preserve variable assignments
			// unless we need to bind an exception variable
			handlerCtx := ctx

			// Bind exception variable if specified
			if excVar != "" {
				// Create new context only when binding exception variable
				handlerCtx = core.NewContext(ctx)
				// Create exception value
				var excValue core.Value

				// Check what kind of error we have
				if exc, ok := tryErr.(*Exception); ok {
					if exc.Value != nil {
						excValue = exc.Value
					} else {
						excValue = core.StringValue(exc.Error())
					}
				} else if evalErr, ok := tryErr.(*core.EvalError); ok {
					// Extract the message without the stack trace
					if evalErr.Wrapped != nil {
						excValue = core.StringValue(evalErr.Wrapped.Error())
					} else {
						excValue = core.StringValue(evalErr.Message)
					}
				} else {
					excValue = core.StringValue(tryErr.Error())
				}

				handlerCtx.Define(excVar, excValue)
			}

			// Execute handler
			handlerErr := error(nil)
			for i := handlerStart; i < exceptClause.Len(); i++ {
				result, handlerErr = Eval(exceptClause.Items()[i], handlerCtx)
				if handlerErr != nil {
					// Exception occurred in handler - this becomes the new error
					tryErr = handlerErr
					break
				}
			}

			// If handler completed without error, the exception is handled
			if handlerErr == nil {
				tryErr = nil // Clear the original exception
			}
			break
		}
	}

	// Run finally clause
	if err := runFinally(); err != nil {
		return nil, err
	}

	// If exception wasn't handled or a new exception occurred, raise it
	if tryErr != nil {
		return nil, tryErr
	}

	return result, nil
}

// raiseForm implements the raise special form
// Forms:
//
//	(raise "message")
//	(raise ExceptionType)
//	(raise ExceptionType "message")
//	(raise (ExceptionType args...))
func raiseForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() == 0 {
		return nil, fmt.Errorf("raise requires at least 1 argument")
	}

	// Single argument
	if args.Len() == 1 {
		// Check if it's a list (exception constructor call)
		if list, ok := args.Items()[0].(*core.ListValue); ok && list.Len() > 0 {
			// Evaluate the exception constructor
			excObj, err := Eval(list, ctx)
			if err != nil {
				return nil, err
			}

			// Extract exception information from the object
			if sym, ok := list.Items()[0].(core.SymbolValue); ok {
				excType := string(sym)

				// Try to get message from the object
				var message string
				// Check if it's an exception instance
				if inst, ok := excObj.(*core.Instance); ok {
					// Try args attribute first (Python standard)
					if argsAttr, hasArgs := inst.GetAttr("args"); hasArgs {
						if argsTuple, ok := argsAttr.(core.TupleValue); ok && len(argsTuple) > 0 {
							if msgStr, ok := argsTuple[0].(core.StringValue); ok {
								message = string(msgStr)
							}
						}
					}
					// Fallback to message attribute
					if message == "" {
						if msgVal, found := inst.GetAttr("message"); found {
							message = core.PrintValueWithoutQuotes(msgVal)
						}
					}
				} else if obj, ok := excObj.(*core.DictValue); ok {
					// Fallback for dict-based exceptions
					if msgVal, exists := obj.Get("message"); exists {
						message = core.PrintValueWithoutQuotes(msgVal)
					}
				}

				return nil, &Exception{
					Type:    excType,
					Message: message,
					Value:   excObj, // Store the actual exception object
				}
			}
		}

		// Evaluate the argument
		val, err := Eval(args.Items()[0], ctx)
		if err != nil {
			return nil, err
		}

		// Check if it's an exception class
		if class, ok := val.(*core.Class); ok {
			// Instantiate the exception class
			instance, err := class.Call([]core.Value{}, ctx)
			if err != nil {
				return nil, err
			}

			// Add __traceback__ attribute to the instance
			if inst, ok := instance.(*core.Instance); ok {
				inst.SetAttr("__traceback__", core.NewTraceback())
			}

			// Return exception with class name as type
			return nil, &Exception{
				Type:    class.Name,
				Message: "",
				Value:   instance, // Store the instance
			}
		}

		// Check if it's an exception instance
		if inst, ok := val.(*core.Instance); ok {
			// Get message from instance's args or message attribute
			var message string
			// Try args attribute first (Python standard)
			if argsAttr, hasArgs := inst.GetAttr("args"); hasArgs {
				if argsTuple, ok := argsAttr.(core.TupleValue); ok && len(argsTuple) > 0 {
					if msgStr, ok := argsTuple[0].(core.StringValue); ok {
						message = string(msgStr)
					}
				}
			}
			// Fallback to message attribute
			if message == "" {
				if msgVal, found := inst.GetAttr("message"); found {
					message = core.PrintValueWithoutQuotes(msgVal)
				}
			}

			return nil, &Exception{
				Type:    inst.Class.Name,
				Message: message,
				Value:   inst,
			}
		}

		// Single string argument - generic exception with message
		if msg, ok := val.(core.StringValue); ok {
			return nil, &Exception{
				Type:    "Exception",
				Message: string(msg),
			}
		}

		// If it's not a string, convert to string
		return nil, &Exception{
			Type:    "Exception",
			Message: core.PrintValueWithoutQuotes(val),
		}
	}

	// Two arguments - type and message
	if args.Len() == 2 {
		var excType string
		var excMsg string

		// Get exception type
		switch t := args.Items()[0].(type) {
		case core.SymbolValue:
			excType = string(t)
		case core.StringValue:
			excType = string(t)
		default:
			return nil, fmt.Errorf("raise: exception type must be a symbol or string")
		}

		// Evaluate and get message
		msgVal, err := Eval(args.Items()[1], ctx)
		if err != nil {
			return nil, err
		}

		if msg, ok := msgVal.(core.StringValue); ok {
			excMsg = string(msg)
		} else {
			excMsg = core.PrintValueWithoutQuotes(msgVal)
		}

		return nil, &Exception{
			Type:    excType,
			Message: excMsg,
		}
	}

	return nil, fmt.Errorf("raise: too many arguments")
}

// convertIterableToSlice converts an iterable value to a slice of values
// This supports lists, tuples, strings, and any type implementing the Iterable interface
func convertIterableToSlice(iterable core.Value) ([]core.Value, error) {
	var items []core.Value
	switch v := iterable.(type) {
	case *core.ListValue:
		items = v.Items()
	case core.TupleValue:
		items = v
	case core.StringValue:
		// Convert string to list of characters
		for _, ch := range string(v) {
			items = append(items, core.StringValue(string(ch)))
		}
	default:
		// Try using Iterator interface
		if iterableObj, ok := v.(core.Iterable); ok {
			iter := iterableObj.Iterator()
			for {
				val, hasNext := iter.Next()
				if !hasNext {
					break
				}
				items = append(items, val)
			}
		} else {
			return nil, fmt.Errorf("value must be iterable, got %s", v.Type())
		}
	}
	return items, nil
}

// unpackTuplePattern unpacks a value according to a pattern like "(x, y)" or "(x, (y, z))"
// and binds the variables in the given context
func unpackTuplePattern(pattern string, value core.Value, ctx *core.Context) error {
	// Remove outer parentheses
	pattern = strings.TrimSpace(pattern)
	if !strings.HasPrefix(pattern, "(") || !strings.HasSuffix(pattern, ")") {
		return fmt.Errorf("invalid tuple pattern: %s", pattern)
	}
	pattern = pattern[1 : len(pattern)-1]

	// Parse variable names from the pattern
	// Simple approach: split by comma at the top level
	varNames := parseTuplePatternVars(pattern)

	// Convert value to a slice
	var values []core.Value
	switch v := value.(type) {
	case core.TupleValue:
		values = []core.Value(v)
	case *core.ListValue:
		values = v.Items()
	default:
		return fmt.Errorf("cannot unpack non-sequence type %s", value.Type())
	}

	// Check length matches
	if len(values) != len(varNames) {
		return fmt.Errorf("cannot unpack %d values into %d variables", len(values), len(varNames))
	}

	// Bind each variable
	for i, varName := range varNames {
		varName = strings.TrimSpace(varName)
		// Check if this is a nested pattern
		if strings.HasPrefix(varName, "(") && strings.HasSuffix(varName, ")") {
			// Recursive unpacking
			if err := unpackTuplePattern(varName, values[i], ctx); err != nil {
				return err
			}
		} else {
			// Simple variable binding
			ctx.Define(varName, values[i])
		}
	}

	return nil
}

// parseTuplePatternVars splits a tuple pattern into variable names
// Example: "x, y" -> ["x", "y"]
// Example: "x, (y, z)" -> ["x", "(y, z)"]
func parseTuplePatternVars(pattern string) []string {
	var result []string
	var current strings.Builder
	depth := 0

	for _, ch := range pattern {
		switch ch {
		case '(':
			depth++
			current.WriteRune(ch)
		case ')':
			depth--
			current.WriteRune(ch)
		case ',':
			if depth == 0 {
				// Top-level comma, split here
				result = append(result, current.String())
				current.Reset()
			} else {
				current.WriteRune(ch)
			}
		default:
			current.WriteRune(ch)
		}
	}

	// Add the last part
	if current.Len() > 0 {
		result = append(result, current.String())
	}

	return result
}

// comprehensionLoop executes a comprehension loop over items with optional filtering
// The callback is called for each item that passes the condition (if provided)
func comprehensionLoop(
	items []core.Value,
	varName string,
	condition core.Value, // nil if no condition
	ctx *core.Context,
	callback func(loopCtx *core.Context) error,
) error {
	loopCtx := core.NewContext(ctx)
	for _, item := range items {
		// Check if varName is a tuple pattern like "(x, y)" or "(x, (y, z))"
		if strings.HasPrefix(varName, "(") && strings.HasSuffix(varName, ")") {
			// Parse the tuple pattern and unpack the item
			if err := unpackTuplePattern(varName, item, loopCtx); err != nil {
				return fmt.Errorf("error unpacking loop variable: %v", err)
			}
		} else {
			// Simple variable binding
			loopCtx.Define(varName, item)
		}

		// Check condition if present
		if condition != nil {
			condResult, err := Eval(condition, loopCtx)
			if err != nil {
				return fmt.Errorf("error evaluating condition: %v", err)
			}

			// Skip if condition is falsy
			if !core.IsTruthy(condResult) {
				continue
			}
		}

		// Execute the callback for this item
		if err := callback(loopCtx); err != nil {
			return err
		}
	}
	return nil
}

// listCompForm implements list comprehensions
// Forms:
//
//	(list-comp expr var iterable)
//	(list-comp expr var iterable condition)
//	(list-comp expr ((var1 iter1 [cond1]) (var2 iter2 [cond2]) ...))  // nested
func ListCompForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() < 2 {
		return nil, fmt.Errorf("list-comp requires at least 2 arguments")
	}

	// Check if this is multi-clause format (args.Items()[1] is a list)
	if clausesList, ok := args.Items()[1].(*core.ListValue); ok {
		// Multi-clause nested comprehension (2 args: expr, clauses)
		if args.Len() != 2 {
			return nil, fmt.Errorf("multi-clause list-comp requires exactly 2 arguments")
		}
		return listCompMultiClause(args.Items()[0], clausesList, ctx)
	}

	// Single-clause format (backward compatible)
	// Requires 3-4 args: expr, var, iterable, [condition]
	if args.Len() < 3 || args.Len() > 4 {
		return nil, fmt.Errorf("single-clause list-comp requires 3 or 4 arguments")
	}

	// Get the variable name
	varSym, ok := args.Items()[1].(core.SymbolValue)
	if !ok {
		return nil, fmt.Errorf("list comprehension variable must be a symbol")
	}
	varName := string(varSym)

	// Evaluate the iterable
	iterable, err := Eval(args.Items()[2], ctx)
	if err != nil {
		return nil, fmt.Errorf("error evaluating iterable: %v", err)
	}

	// Convert iterable to a sequence we can iterate over
	items, err := convertIterableToSlice(iterable)
	if err != nil {
		return nil, fmt.Errorf("list comprehension: %v", err)
	}

	// Create result list
	result := make([]core.Value, 0)

	// Get optional condition
	var condition core.Value
	if args.Len() == 4 {
		condition = args.Items()[3]
	}

	// Execute the comprehension loop
	err = comprehensionLoop(items, varName, condition, ctx, func(loopCtx *core.Context) error {
		// Evaluate the expression
		exprResult, err := Eval(args.Items()[0], loopCtx)
		if err != nil {
			return fmt.Errorf("error evaluating expression: %v", err)
		}

		// Add to result
		result = append(result, exprResult)
		return nil
	})

	if err != nil {
		return nil, err
	}

	return core.NewList(result...), nil
}

// listCompMultiClause handles nested list comprehensions
// Format: (list-comp expr ((var1 iter1 [cond1]) (var2 iter2 [cond2]) ...))
func listCompMultiClause(expr core.Value, clausesList *core.ListValue, ctx *core.Context) (core.Value, error) {
	result := make([]core.Value, 0)

	// Parse clauses
	type clause struct {
		varName   string
		iterable  core.Value
		condition core.Value
	}

	clauses := make([]clause, 0, clausesList.Len())
	for i, clauseVal := range clausesList.Items() {
		clauseList, ok := clauseVal.(*core.ListValue)
		if !ok {
			return nil, fmt.Errorf("clause %d must be a list", i)
		}

		if clauseList.Len() < 2 || clauseList.Len() > 3 {
			return nil, fmt.Errorf("clause %d must have 2 or 3 elements (var, iter, [condition])", i)
		}

		varSym, ok := clauseList.Items()[0].(core.SymbolValue)
		if !ok {
			return nil, fmt.Errorf("clause %d variable must be a symbol", i)
		}

		var condition core.Value
		if clauseList.Len() == 3 {
			condition = clauseList.Items()[2]
		}

		clauses = append(clauses, clause{
			varName:   string(varSym),
			iterable:  clauseList.Items()[1], // Not evaluated yet
			condition: condition,
		})
	}

	// Recursively evaluate nested loops
	var evalClauses func(clauseIdx int, loopCtx *core.Context) error
	evalClauses = func(clauseIdx int, loopCtx *core.Context) error {
		if clauseIdx >= len(clauses) {
			// All loops processed, evaluate the expression
			exprResult, err := Eval(expr, loopCtx)
			if err != nil {
				return fmt.Errorf("error evaluating expression: %v", err)
			}
			result = append(result, exprResult)
			return nil
		}

		// Evaluate the iterable for this clause
		c := clauses[clauseIdx]
		iterable, err := Eval(c.iterable, loopCtx)
		if err != nil {
			return fmt.Errorf("error evaluating iterable in clause %d: %v", clauseIdx, err)
		}

		// Convert to slice
		items, err := convertIterableToSlice(iterable)
		if err != nil {
			return fmt.Errorf("clause %d: %v", clauseIdx, err)
		}

		// Loop over items
		return comprehensionLoop(items, c.varName, c.condition, loopCtx, func(innerCtx *core.Context) error {
			// Recurse to next clause
			return evalClauses(clauseIdx+1, innerCtx)
		})
	}

	// Start evaluation from first clause
	if err := evalClauses(0, ctx); err != nil {
		return nil, err
	}

	return core.NewList(result...), nil
}

// DictCompForm implements the dict-comp special form
// Syntax:
//
//	(dict-comp key-expr value-expr var iterable)
//	(dict-comp key-expr value-expr var iterable condition)
//	(dict-comp key-expr value-expr ((var1 iter1 [cond1]) (var2 iter2 [cond2]) ...))  // nested
func DictCompForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() < 3 {
		return nil, fmt.Errorf("dict-comp requires at least 3 arguments")
	}

	// Check if this is multi-clause format (args.Items()[2] is a list)
	if clausesList, ok := args.Items()[2].(*core.ListValue); ok {
		// Multi-clause nested comprehension (3 args: key-expr, value-expr, clauses)
		if args.Len() != 3 {
			return nil, fmt.Errorf("multi-clause dict-comp requires exactly 3 arguments")
		}
		return dictCompMultiClause(args.Items()[0], args.Items()[1], clausesList, ctx)
	}

	// Single-clause format (backward compatible)
	// Requires 4-5 args: key-expr, value-expr, var, iterable, [condition]
	if args.Len() < 4 || args.Len() > 5 {
		return nil, fmt.Errorf("single-clause dict-comp requires 4 or 5 arguments")
	}

	// Get the variable name
	varSym, ok := args.Items()[2].(core.SymbolValue)
	if !ok {
		return nil, fmt.Errorf("dict comprehension variable must be a symbol")
	}
	varName := string(varSym)

	// Evaluate the iterable
	iterable, err := Eval(args.Items()[3], ctx)
	if err != nil {
		return nil, fmt.Errorf("error evaluating iterable: %v", err)
	}

	// Convert iterable to a sequence we can iterate over
	items, err := convertIterableToSlice(iterable)
	if err != nil {
		return nil, fmt.Errorf("dict comprehension: %v", err)
	}

	// Create result dict
	result := core.NewDict()

	// Get optional condition
	var condition core.Value
	if args.Len() == 5 {
		condition = args.Items()[4]
	}

	// Execute the comprehension loop
	err = comprehensionLoop(items, varName, condition, ctx, func(loopCtx *core.Context) error {
		// Evaluate the key expression
		keyResult, err := Eval(args.Items()[0], loopCtx)
		if err != nil {
			return fmt.Errorf("error evaluating key expression: %v", err)
		}

		// Evaluate the value expression
		valueResult, err := Eval(args.Items()[1], loopCtx)
		if err != nil {
			return fmt.Errorf("error evaluating value expression: %v", err)
		}

		// Add to result dict
		keyStr := core.ValueToKey(keyResult)
		result.SetWithKey(keyStr, keyResult, valueResult)
		return nil
	})

	if err != nil {
		return nil, err
	}

	return result, nil
}

// dictCompMultiClause handles nested dict comprehensions
// Format: (dict-comp key-expr value-expr ((var1 iter1 [cond1]) (var2 iter2 [cond2]) ...))
func dictCompMultiClause(keyExpr, valueExpr core.Value, clausesList *core.ListValue, ctx *core.Context) (core.Value, error) {
	result := core.NewDict()

	// Parse clauses
	type clause struct {
		varName   string
		iterable  core.Value
		condition core.Value
	}

	clauses := make([]clause, 0, clausesList.Len())
	for i, clauseVal := range clausesList.Items() {
		clauseList, ok := clauseVal.(*core.ListValue)
		if !ok {
			return nil, fmt.Errorf("clause %d must be a list", i)
		}

		if clauseList.Len() < 2 || clauseList.Len() > 3 {
			return nil, fmt.Errorf("clause %d must have 2 or 3 elements (var, iter, [condition])", i)
		}

		varSym, ok := clauseList.Items()[0].(core.SymbolValue)
		if !ok {
			return nil, fmt.Errorf("clause %d variable must be a symbol", i)
		}

		var condition core.Value
		if clauseList.Len() == 3 {
			condition = clauseList.Items()[2]
		}

		clauses = append(clauses, clause{
			varName:   string(varSym),
			iterable:  clauseList.Items()[1],
			condition: condition,
		})
	}

	// Recursively evaluate nested loops
	var evalClauses func(clauseIdx int, loopCtx *core.Context) error
	evalClauses = func(clauseIdx int, loopCtx *core.Context) error {
		if clauseIdx >= len(clauses) {
			// All loops processed, evaluate key and value
			keyResult, err := Eval(keyExpr, loopCtx)
			if err != nil {
				return fmt.Errorf("error evaluating key expression: %v", err)
			}

			valueResult, err := Eval(valueExpr, loopCtx)
			if err != nil {
				return fmt.Errorf("error evaluating value expression: %v", err)
			}

			keyStr := core.ValueToKey(keyResult)
			result.SetWithKey(keyStr, keyResult, valueResult)
			return nil
		}

		// Evaluate the iterable for this clause
		c := clauses[clauseIdx]
		iterable, err := Eval(c.iterable, loopCtx)
		if err != nil {
			return fmt.Errorf("error evaluating iterable in clause %d: %v", clauseIdx, err)
		}

		// Convert to slice
		items, err := convertIterableToSlice(iterable)
		if err != nil {
			return fmt.Errorf("clause %d: %v", clauseIdx, err)
		}

		// Loop over items
		return comprehensionLoop(items, c.varName, c.condition, loopCtx, func(innerCtx *core.Context) error {
			return evalClauses(clauseIdx+1, innerCtx)
		})
	}

	if err := evalClauses(0, ctx); err != nil {
		return nil, err
	}

	return result, nil
}

// SetCompForm implements the set-comp special form
// Syntax:
//
//	(set-comp expr var iterable)
//	(set-comp expr var iterable condition)
//	(set-comp expr ((var1 iter1 [cond1]) (var2 iter2 [cond2]) ...))  // nested
func SetCompForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() < 2 {
		return nil, fmt.Errorf("set-comp requires at least 2 arguments")
	}

	// Check if this is multi-clause format (args.Items()[1] is a list)
	if clausesList, ok := args.Items()[1].(*core.ListValue); ok {
		// Multi-clause nested comprehension (2 args: expr, clauses)
		if args.Len() != 2 {
			return nil, fmt.Errorf("multi-clause set-comp requires exactly 2 arguments")
		}
		return setCompMultiClause(args.Items()[0], clausesList, ctx)
	}

	// Single-clause format (backward compatible)
	// Requires 3-4 args: expr, var, iterable, [condition]
	if args.Len() < 3 || args.Len() > 4 {
		return nil, fmt.Errorf("single-clause set-comp requires 3 or 4 arguments")
	}

	// Get the variable name
	varSym, ok := args.Items()[1].(core.SymbolValue)
	if !ok {
		return nil, fmt.Errorf("set comprehension variable must be a symbol")
	}
	varName := string(varSym)

	// Evaluate the iterable
	iterable, err := Eval(args.Items()[2], ctx)
	if err != nil {
		return nil, fmt.Errorf("error evaluating iterable: %v", err)
	}

	// Convert iterable to a sequence we can iterate over
	items, err := convertIterableToSlice(iterable)
	if err != nil {
		return nil, fmt.Errorf("set comprehension: %v", err)
	}

	// Create result set
	result := core.NewSet()

	// Get optional condition
	var condition core.Value
	if args.Len() == 4 {
		condition = args.Items()[3]
	}

	// Execute the comprehension loop
	err = comprehensionLoop(items, varName, condition, ctx, func(loopCtx *core.Context) error {
		// Evaluate the expression
		exprResult, err := Eval(args.Items()[0], loopCtx)
		if err != nil {
			return fmt.Errorf("error evaluating expression: %v", err)
		}

		// Add to result set
		result.Add(exprResult)
		return nil
	})

	if err != nil {
		return nil, err
	}

	return result, nil
}

// setCompMultiClause handles nested set comprehensions
// Format: (set-comp expr ((var1 iter1 [cond1]) (var2 iter2 [cond2]) ...))
func setCompMultiClause(expr core.Value, clausesList *core.ListValue, ctx *core.Context) (core.Value, error) {
	result := core.NewSet()

	// Parse clauses
	type clause struct {
		varName   string
		iterable  core.Value
		condition core.Value
	}

	clauses := make([]clause, 0, clausesList.Len())
	for i, clauseVal := range clausesList.Items() {
		clauseList, ok := clauseVal.(*core.ListValue)
		if !ok {
			return nil, fmt.Errorf("clause %d must be a list", i)
		}

		if clauseList.Len() < 2 || clauseList.Len() > 3 {
			return nil, fmt.Errorf("clause %d must have 2 or 3 elements (var, iter, [condition])", i)
		}

		varSym, ok := clauseList.Items()[0].(core.SymbolValue)
		if !ok {
			return nil, fmt.Errorf("clause %d variable must be a symbol", i)
		}

		var condition core.Value
		if clauseList.Len() == 3 {
			condition = clauseList.Items()[2]
		}

		clauses = append(clauses, clause{
			varName:   string(varSym),
			iterable:  clauseList.Items()[1],
			condition: condition,
		})
	}

	// Recursively evaluate nested loops
	var evalClauses func(clauseIdx int, loopCtx *core.Context) error
	evalClauses = func(clauseIdx int, loopCtx *core.Context) error {
		if clauseIdx >= len(clauses) {
			// All loops processed, evaluate the expression
			exprResult, err := Eval(expr, loopCtx)
			if err != nil {
				return fmt.Errorf("error evaluating expression: %v", err)
			}
			result.Add(exprResult)
			return nil
		}

		// Evaluate the iterable for this clause
		c := clauses[clauseIdx]
		iterable, err := Eval(c.iterable, loopCtx)
		if err != nil {
			return fmt.Errorf("error evaluating iterable in clause %d: %v", clauseIdx, err)
		}

		// Convert to slice
		items, err := convertIterableToSlice(iterable)
		if err != nil {
			return fmt.Errorf("clause %d: %v", clauseIdx, err)
		}

		// Loop over items
		return comprehensionLoop(items, c.varName, c.condition, loopCtx, func(innerCtx *core.Context) error {
			return evalClauses(clauseIdx+1, innerCtx)
		})
	}

	if err := evalClauses(0, ctx); err != nil {
		return nil, err
	}

	return result, nil
}

// GenExprForm implements generator expressions
// Forms:
//
//	Old format: (gen-expr expr var iterable [condition])
//	Lambda format: (gen-comp (lambda (var) expr) iterable [(lambda (var) condition)])
//
// Returns a Generator object that lazily evaluates the expression
func GenExprForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() < 2 || args.Len() > 4 {
		return nil, fmt.Errorf("gen-expr/gen-comp requires 2-4 arguments")
	}

	// Check if this is lambda format: first arg is a lambda
	if lambdaList, ok := args.Items()[0].(*core.ListValue); ok && lambdaList.Len() > 0 {
		if lambdaSym, ok := lambdaList.Items()[0].(core.SymbolValue); ok && string(lambdaSym) == "lambda" {
			// Lambda format: (gen-comp (lambda (var) expr) iterable [(lambda (var) condition)])
			if args.Len() < 2 || args.Len() > 3 {
				return nil, fmt.Errorf("gen-comp lambda format requires 2 or 3 arguments")
			}

			// Extract variable name from lambda
			if lambdaList.Len() < 3 {
				return nil, fmt.Errorf("invalid lambda in gen-comp")
			}
			params, ok := lambdaList.Items()[1].(*core.ListValue)
			if !ok || params.Len() != 1 {
				return nil, fmt.Errorf("gen-comp lambda must have exactly one parameter")
			}
			varSym, ok := params.Items()[0].(core.SymbolValue)
			if !ok {
				return nil, fmt.Errorf("gen-comp lambda parameter must be a symbol")
			}
			varName := string(varSym)

			// Expression is the lambda body
			expr := lambdaList.Items()[2]

			// Evaluate the iterable
			iterable, err := Eval(args.Items()[1], ctx)
			if err != nil {
				return nil, fmt.Errorf("error evaluating iterable: %v", err)
			}

			// Optional condition (also a lambda)
			var condition core.Value
			if args.Len() == 3 {
				if condLambda, ok := args.Items()[2].(*core.ListValue); ok && condLambda.Len() >= 3 {
					condition = condLambda.Items()[2] // lambda body
				}
			}

			// Create generator
			gen, err := core.NewGeneratorExpression("genexpr", expr, varName, iterable, condition, ctx, Eval)
			if err != nil {
				return nil, err
			}
			return gen, nil
		}
	}

	// Old format: (gen-expr expr var iterable [condition])
	if args.Len() < 3 || args.Len() > 4 {
		return nil, fmt.Errorf("gen-expr old format requires 3 or 4 arguments")
	}

	// Get the variable name
	varSym, ok := args.Items()[1].(core.SymbolValue)
	if !ok {
		return nil, fmt.Errorf("generator expression variable must be a symbol")
	}
	varName := string(varSym)

	// Evaluate the iterable
	iterable, err := Eval(args.Items()[2], ctx)
	if err != nil {
		return nil, fmt.Errorf("error evaluating iterable: %v", err)
	}

	// Store the expression, condition (if present), and context
	expr := args.Items()[0]
	var condition core.Value
	if args.Len() == 4 {
		condition = args.Items()[3]
	}

	// Create a Generator object with the expression, variable, iterable, and condition
	gen, err := core.NewGeneratorExpression("genexpr", expr, varName, iterable, condition, ctx, Eval)
	if err != nil {
		return nil, err
	}
	return gen, nil
}

// listLiteralForm implements the list-literal special form
// It evaluates all arguments and returns them as a list
func listLiteralForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	result := make([]core.Value, 0, args.Len())

	// Evaluate each element
	for _, arg := range args.Items() {
		val, err := Eval(arg, ctx)
		if err != nil {
			return nil, err
		}
		result = append(result, val)
	}

	return core.NewList(result...), nil
}

// tupleLiteralForm implements the tuple-literal special form
// Usage: (tuple-literal elem1 elem2 ...)
func tupleLiteralForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	result := make(core.TupleValue, 0, args.Len())

	// Evaluate each element
	for _, arg := range args.Items() {
		val, err := Eval(arg, ctx)
		if err != nil {
			return nil, err
		}
		result = append(result, val)
	}

	return result, nil
}
