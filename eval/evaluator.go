// Package eval provides the evaluation system for M28 expressions.
package eval

import (
	"fmt"
	"strings"

	"github.com/mmichie/m28/core"
)

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

	case core.ListValue:
		// Empty list evaluates to itself
		if len(v) == 0 {
			return core.EmptyList, nil
		}

		// Check if it's a special form first (if, def, etc.)
		if sym, ok := v[0].(core.SymbolValue); ok {
			if handler, ok := specialForms[string(sym)]; ok {
				return handler(v[1:], ctx)
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
func evalFunctionCall(expr core.ListValue, ctx *core.Context) (core.Value, error) {
	// Evaluate the function
	fn, err := Eval(expr[0], ctx)
	if err != nil {
		return nil, err
	}

	// Evaluate the arguments
	args := make([]core.Value, 0, len(expr)-1)
	for _, arg := range expr[1:] {
		evalArg, err := Eval(arg, ctx)
		if err != nil {
			return nil, err
		}
		args = append(args, evalArg)
	}

	// Call the function
	callable, ok := fn.(core.Callable)
	if !ok {
		return nil, core.NewTypeError("callable", fn, "function call")
	}

	// Add function name to call stack if available
	var funcName string
	switch f := fn.(type) {
	case core.SymbolValue:
		funcName = string(f)
	case *core.BuiltinFunction:
		funcName = "<builtin>"
	case *core.BoundMethod:
		funcName = fmt.Sprintf("%s.%s", f.TypeDesc.PythonName, f.Method.Name)
	default:
		funcName = "<anonymous>"
	}

	ctx.PushStack(funcName, "", 0, 0)
	defer ctx.PopStack()

	result, err := callable.Call(args, ctx)
	if err != nil {
		return nil, core.WrapEvalError(err, fmt.Sprintf("error in %s", funcName), ctx)
	}

	return result, nil
}

// SpecialFormHandler handles special forms like if, def, etc.
type SpecialFormHandler func(args core.ListValue, ctx *core.Context) (core.Value, error)

// specialForms maps special form names to their handlers
var specialForms map[string]SpecialFormHandler

func init() {
	specialForms = map[string]SpecialFormHandler{
		// Control flow
		"if":     ifForm,
		"do":     doForm,
		"return": returnForm,

		// Definitions
		"def":    defForm,
		"=":      assignForm,
		"quote":  quoteForm,
		"lambda": lambdaForm,
		"fn":     lambdaForm, // Alias for lambda

		// Module system (will be overridden by enhanced version)
		// "import": importForm,

		// Exception handling
		"try":   tryForm,
		"raise": raiseForm,

		// List comprehension
		"list-comp": ListCompForm,

		// List literal (evaluates contents)
		"list-literal": listLiteralForm,

		// Other special forms will be added through RegisterSpecialForm
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
}

// RegisterSpecialForm registers a special form
func RegisterSpecialForm(name string, handler SpecialFormHandler) {
	specialForms[name] = handler
}

// ifForm delegates to IfForm in util.go
func ifForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	// Delegate to util.go implementation which supports elif
	return IfForm(args, ctx)
}

// defForm implements the def special form to define functions and variables
func defForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("def requires at least 2 arguments")
	}

	// Check for alternative function definition form: (def (name args...) body...)
	if fnDef, ok := args[0].(core.ListValue); ok && len(fnDef) > 0 {
		// Get function name from the first element of the list
		name, ok := fnDef[0].(core.SymbolValue)
		if !ok {
			return nil, fmt.Errorf("def: function name must be a symbol")
		}

		// Get parameter list (the rest of the elements after the name)
		params := make([]core.SymbolValue, 0, len(fnDef)-1)
		for _, param := range fnDef[1:] {
			if sym, ok := param.(core.SymbolValue); ok {
				params = append(params, sym)
			} else {
				return nil, fmt.Errorf("parameters must be symbols")
			}
		}

		// Create function body (implicit do)
		body := args[1:]
		var functionBody core.Value

		if len(body) == 1 {
			// Single expression body
			functionBody = body[0]
		} else {
			// Multi-expression body, wrap in do
			functionBody = core.ListValue(append([]core.Value{core.SymbolValue("do")}, body...))
		}

		function := &UserFunction{
			BaseObject: *core.NewBaseObject(core.FunctionType),
			params:     params,
			body:       functionBody,
			env:        ctx,
			name:       string(name),
		}

		ctx.Define(string(name), function)
		return function, nil
	}

	// Standard form: (def name ...)
	name, ok := args[0].(core.SymbolValue)
	if !ok {
		return nil, fmt.Errorf("def: first argument must be a symbol")
	}

	// Check different definition forms
	if len(args) >= 3 {
		// Check for function definition: (def name (params) body...)
		if paramList, ok := args[1].(core.ListValue); ok {
			// It's a function definition with parameter list

			// Parse parameter list
			params := make([]core.SymbolValue, 0, len(paramList))
			for _, param := range paramList {
				if sym, ok := param.(core.SymbolValue); ok {
					params = append(params, sym)
				} else {
					return nil, fmt.Errorf("parameters must be symbols")
				}
			}

			// Create function body (implicit do)
			body := args[2:]
			var functionBody core.Value

			if len(body) == 1 {
				// Single expression body
				functionBody = body[0]
			} else {
				// Multi-expression body, wrap in do
				functionBody = core.ListValue(append([]core.Value{core.SymbolValue("do")}, body...))
			}

			function := &UserFunction{
				BaseObject: *core.NewBaseObject(core.FunctionType),
				params:     params,
				body:       functionBody,
				env:        ctx,
				name:       string(name),
			}

			ctx.Define(string(name), function)
			return function, nil
		}
	}

	// Variable definition: (def name value)
	value, err := Eval(args[1], ctx)
	if err != nil {
		return nil, err
	}

	ctx.Define(string(name), value)
	return value, nil
}

// assignForm implements the = special form for assignment
func assignForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("= requires 2 arguments, got %d", len(args))
	}

	// Get the target
	target := args[0]

	// Check if target is a list (tuple unpacking)
	if targetList, ok := target.(core.ListValue); ok {
		// Tuple unpacking: (= (x, y) (10, 20))
		// Evaluate the value
		value, err := Eval(args[1], ctx)
		if err != nil {
			return nil, err
		}

		// The value must be a tuple or list
		var values []core.Value
		switch v := value.(type) {
		case core.TupleValue:
			values = []core.Value(v)
		case core.ListValue:
			values = []core.Value(v)
		default:
			return nil, fmt.Errorf("cannot unpack non-sequence %v", value.Type())
		}

		// Check that the number of targets matches the number of values
		if len(targetList) != len(values) {
			return nil, fmt.Errorf("too many values to unpack (expected %d, got %d)", len(targetList), len(values))
		}

		// Assign each value to its corresponding target
		for i, t := range targetList {
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
	value, err := Eval(args[1], ctx)
	if err != nil {
		return nil, err
	}

	// Variable assignment - always define/update in current scope
	symName := string(sym)
	ctx.Define(symName, value)
	return value, nil
}

// quoteForm implements the quote special form
func quoteForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("quote requires 1 argument")
	}

	// Return the argument unevaluated
	return args[0], nil
}

// doForm implements the do special form for grouping expressions
func doForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) == 0 {
		return core.Nil, nil
	}

	// Evaluate all expressions in sequence
	var result core.Value = core.Nil
	var err error

	for _, expr := range args {
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
func returnForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) > 1 {
		return nil, fmt.Errorf("return takes at most 1 argument")
	}

	var value core.Value = core.Nil
	var err error

	if len(args) == 1 {
		value, err = Eval(args[0], ctx)
		if err != nil {
			return nil, err
		}
	}

	return &ReturnValue{Value: value}, nil
}

// importForm implements the import special form
func importForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("import requires 1 argument")
	}

	// Get the module name
	var moduleName string
	switch name := args[0].(type) {
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

	return result, nil
}

// CallWithKwargs calls the function with positional and keyword arguments
func (f *UserFunction) CallWithKwargs(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
	// Create a new environment with the function's environment as parent
	funcEnv := core.NewContext(f.env)

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
	default:
		return f.BaseObject.GetAttr(name)
	}
}

// lambdaForm implements the lambda special form
func lambdaForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("lambda requires at least 2 arguments: parameters and body")
	}

	// Get the parameter list
	paramList, ok := args[0].(core.ListValue)
	if !ok {
		return nil, fmt.Errorf("lambda: parameters must be a list")
	}

	// Try to parse as new-style parameter list with defaults
	signature, err := ParseParameterList(paramList)
	if err != nil {
		// Fall back to legacy simple parameter parsing
		params := make([]core.SymbolValue, 0, len(paramList))
		for _, p := range paramList {
			sym, ok := p.(core.SymbolValue)
			if !ok {
				return nil, fmt.Errorf("lambda: parameters must be symbols")
			}
			params = append(params, sym)
		}

		// The body is the rest of the expressions wrapped in a do form
		var body core.Value
		if len(args) == 2 {
			body = args[1]
		} else {
			// Multiple expressions - wrap in do
			body = core.ListValue(append([]core.Value{core.SymbolValue("do")}, args[1:]...))
		}

		// Create the function with legacy params
		fn := &UserFunction{
			BaseObject: *core.NewBaseObject(core.FunctionType),
			params:     params,
			body:       body,
			env:        ctx, // Capture current environment
		}

		return fn, nil
	}

	// New-style function with signature
	// The body is the rest of the expressions wrapped in a do form
	var body core.Value
	if len(args) == 2 {
		body = args[1]
	} else {
		// Multiple expressions - wrap in do
		body = core.ListValue(append([]core.Value{core.SymbolValue("do")}, args[1:]...))
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

	return fn, nil
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
func tryForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) == 0 {
		return nil, fmt.Errorf("try requires at least a body")
	}

	var tryBody []core.Value
	var exceptClauses []core.ListValue
	var finallyClause core.ListValue

	// Parse the try form
	for i, arg := range args {
		if list, ok := arg.(core.ListValue); ok && len(list) > 0 {
			if sym, ok := list[0].(core.SymbolValue); ok {
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
		if len(exceptClauses) == 0 && len(finallyClause) == 0 {
			tryBody = append(tryBody, arg)
		}
	}

	// Helper to run finally clause
	runFinally := func() error {
		if len(finallyClause) > 1 {
			for _, expr := range finallyClause[1:] {
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
	handled := false

	for _, exceptClause := range exceptClauses {
		if len(exceptClause) < 2 {
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

		if len(exceptClause) > 1 {
			// Check if second element is a symbol
			if sym, ok := exceptClause[1].(core.SymbolValue); ok {
				symStr := string(sym)

				// Check for "as" syntax for catch-all
				if symStr == "as" && len(exceptClause) > 2 {
					// (except as var handler...)
					if varSym, ok := exceptClause[2].(core.SymbolValue); ok {
						excVar = string(varSym)
						handlerStart = 3
					}
				} else if isExceptionType(symStr) || (len(symStr) > 0 && symStr[0] >= 'A' && symStr[0] <= 'Z') {
					// It's an exception type
					excType = symStr
					handlerStart = 2

					// Check for "as" syntax
					if len(exceptClause) > 3 && handlerStart == 2 {
						if asSym, ok := exceptClause[2].(core.SymbolValue); ok && string(asSym) == "as" {
							// (except Type as var handler...)
							if varSym, ok := exceptClause[3].(core.SymbolValue); ok {
								excVar = string(varSym)
								handlerStart = 4
							}
						}
					}

					// Legacy: Check if next element is a variable name (lowercase)
					if excVar == "" && len(exceptClause) > 2 {
						if varSym, ok := exceptClause[2].(core.SymbolValue); ok {
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
					matches = excType == "NameError"
				case *core.TypeError:
					matches = excType == "TypeError"
				case *core.ZeroDivisionError:
					matches = excType == "ZeroDivisionError"
				case *core.KeyError:
					matches = excType == "KeyError"
				case *core.IndexError:
					matches = excType == "IndexError"
				default:
					// Check if it's our custom Exception type
					if exc, ok := baseErr.(*Exception); ok {
						matches = exc.Type == excType
					} else {
						matches = excType == "Exception" || excType == "Error"
					}
				}
			}
		}

		if matches {
			// Create new context for handler
			handlerCtx := core.NewContext(ctx)

			// Bind exception variable if specified
			if excVar != "" {
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
			for i := handlerStart; i < len(exceptClause); i++ {
				result, tryErr = Eval(exceptClause[i], handlerCtx)
				if tryErr != nil {
					break
				}
			}

			handled = true
			break
		}
	}

	// Run finally clause
	if err := runFinally(); err != nil {
		return nil, err
	}

	// If exception wasn't handled, re-raise it
	if !handled && tryErr != nil {
		return nil, tryErr
	}

	return result, tryErr
}

// raiseForm implements the raise special form
// Forms:
//
//	(raise "message")
//	(raise ExceptionType)
//	(raise ExceptionType "message")
//	(raise (ExceptionType args...))
func raiseForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) == 0 {
		return nil, fmt.Errorf("raise requires at least 1 argument")
	}

	// Single argument
	if len(args) == 1 {
		// Check if it's a list (exception constructor call)
		if list, ok := args[0].(core.ListValue); ok && len(list) > 0 {
			// Evaluate the exception constructor
			excObj, err := Eval(list, ctx)
			if err != nil {
				return nil, err
			}

			// Extract exception information from the object
			// For now, we'll create a basic exception with the type name
			if sym, ok := list[0].(core.SymbolValue); ok {
				excType := string(sym)

				// Try to get message from the object
				var message string
				if obj, ok := excObj.(*core.DictValue); ok {
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

		// Single string argument - generic exception with message
		if msg, ok := args[0].(core.StringValue); ok {
			return nil, &Exception{
				Type:    "Exception",
				Message: string(msg),
			}
		}

		// Single symbol - exception type with no message
		if typ, ok := args[0].(core.SymbolValue); ok {
			return nil, &Exception{
				Type: string(typ),
			}
		}
	}

	// Two arguments - type and message
	if len(args) == 2 {
		var excType string
		var excMsg string

		// Get exception type
		switch t := args[0].(type) {
		case core.SymbolValue:
			excType = string(t)
		case core.StringValue:
			excType = string(t)
		default:
			return nil, fmt.Errorf("raise: exception type must be a symbol or string")
		}

		// Get message
		if msg, ok := args[1].(core.StringValue); ok {
			excMsg = string(msg)
		} else {
			excMsg = core.PrintValueWithoutQuotes(args[1])
		}

		return nil, &Exception{
			Type:    excType,
			Message: excMsg,
		}
	}

	return nil, fmt.Errorf("raise: too many arguments")
}

// listCompForm implements list comprehensions
// Forms:
//
//	(list-comp expr var iterable)
//	(list-comp expr var iterable condition)
func ListCompForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) < 3 || len(args) > 4 {
		return nil, fmt.Errorf("list-comp requires 3 or 4 arguments")
	}

	// Get the variable name
	varSym, ok := args[1].(core.SymbolValue)
	if !ok {
		return nil, fmt.Errorf("list comprehension variable must be a symbol")
	}
	varName := string(varSym)

	// Evaluate the iterable
	iterable, err := Eval(args[2], ctx)
	if err != nil {
		return nil, fmt.Errorf("error evaluating iterable: %v", err)
	}

	// Convert iterable to a sequence we can iterate over
	var items []core.Value
	switch v := iterable.(type) {
	case core.ListValue:
		items = v
	case core.TupleValue:
		items = v
	case core.StringValue:
		// Convert string to list of characters
		for _, ch := range string(v) {
			items = append(items, core.StringValue(string(ch)))
		}
	default:
		// Try to call iter() on it if it has that method
		// For now, just error
		return nil, fmt.Errorf("list comprehension iterable must be a sequence, got %s", v.Type())
	}

	// Create result list
	result := make(core.ListValue, 0)

	// Create a new context for the loop variable
	loopCtx := core.NewContext(ctx)

	// Iterate over items
	for _, item := range items {
		// Bind the loop variable
		loopCtx.Define(varName, item)

		// Check condition if present
		if len(args) == 4 {
			condResult, err := Eval(args[3], loopCtx)
			if err != nil {
				return nil, fmt.Errorf("error evaluating condition: %v", err)
			}

			// Skip if condition is falsy
			if !core.IsTruthy(condResult) {
				continue
			}
		}

		// Evaluate the expression
		exprResult, err := Eval(args[0], loopCtx)
		if err != nil {
			return nil, fmt.Errorf("error evaluating expression: %v", err)
		}

		// Add to result
		result = append(result, exprResult)
	}

	return result, nil
}

// listLiteralForm implements the list-literal special form
// It evaluates all arguments and returns them as a list
func listLiteralForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	result := make(core.ListValue, 0, len(args))

	// Evaluate each element
	for _, arg := range args {
		val, err := Eval(arg, ctx)
		if err != nil {
			return nil, err
		}
		result = append(result, val)
	}

	return result, nil
}
