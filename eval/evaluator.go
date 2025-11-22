// Package eval provides the evaluation system for M28 expressions.
package eval

import (
	"fmt"
	"log"
	"os"
	"strings"

	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/core/protocols"
)

func init() {
	// Set the EvalHook so that builtin eval() can work
	core.EvalHook = Eval
}

// Eval evaluates an expression in a context
func Eval(expr core.Value, ctx *core.Context) (core.Value, error) {
	// Increment evaluation counter and log progress periodically
	if ctx != nil {
		ctx.EvalCount++
		// Disable debug logging for normal operation
		logThis := false
		if logThis {
			// Show progress every 100 evaluations (lowered for debugging slow modules)
			exprStr := core.PrintValue(expr)
			if len(exprStr) > 150 {
				exprStr = exprStr[:150] + "..."
			}

			// Show module/function context for debugging
			contextInfo := fmt.Sprintf(" [ctx:%p]", ctx)

			// Check ctx.Vars directly to avoid ValueToKey recursion issues
			if nameVal, ok := ctx.Vars["__name__"]; ok {
				if nameStr, ok := nameVal.(core.StringValue); ok {
					contextInfo += fmt.Sprintf(" [module:%s]", string(nameStr))
				}
			} else if ctx.CurrentFunction != "" {
				contextInfo += fmt.Sprintf(" [func:%s]", ctx.CurrentFunction)
			} else if ctx.Global != nil && ctx.Global != ctx {
				// Check parent module
				if nameVal, ok := ctx.Global.Vars["__name__"]; ok {
					if nameStr, ok := nameVal.(core.StringValue); ok {
						contextInfo += fmt.Sprintf(" [parent_module:%s]", string(nameStr))
					}
				}
			}

			log.Printf("[EVAL-PROGRESS] %d evals%s expr=%s", ctx.EvalCount, contextInfo, exprStr)
		}
	}

	switch v := expr.(type) {
	case core.NumberValue, core.StringValue, core.BoolValue, core.NilValue:
		// Self-evaluating primitives
		return v, nil

	case core.SymbolValue:
		// Variable lookup (operators are handled via fast-path in ctx.Lookup)
		val, err := ctx.Lookup(string(v))
		if err != nil {
			return nil, core.WrapEvalError(err, "name error", ctx)
		}
		return val, nil

	case *core.ListValue:
		// Empty list evaluates to a fresh empty list (avoid shared mutable state)
		if v.Len() == 0 {
			return core.NewList(), nil
		}

		core.DebugLog("[EVAL-LIST] Evaluating list with %d elements, first: %T\n", v.Len(), v.Items()[0])

		// Check if it's a decorator form (@decorator ...)
		if isDecoratorForm(v) {
			core.DebugLog("[EVAL-LIST] Is decorator form\n")
			return evalDecoratorForm(v, ctx)
		}

		// Check if it's a special form first (if, def, etc.)
		if sym, ok := v.Items()[0].(core.SymbolValue); ok {
			core.DebugLog("[EVAL-LIST] First element is symbol: %s\n", string(sym))

			if handler, ok := specialForms[string(sym)]; ok {
				core.DebugLog("[EVAL-LIST] Is special form: %s\n", string(sym))
				return handler(core.NewList(v.Items()[1:]...), ctx)
			}
		}

		// Check if it's a macro call (function with __macro__ attribute)
		if sym, ok := v.Items()[0].(core.SymbolValue); ok {
			if isMacroCall(sym, ctx) {
				core.DebugLog("[EVAL-LIST] Is macro call: %s\n", string(sym))
				return evalMacroCall(v, ctx)
			}
		}

		// Otherwise it's a function call
		core.DebugLog("[EVAL-LIST] Treating as function call\n")
		return evalFunctionCallWithKeywords(v, ctx)

	case *core.DictValue:
		// Dicts need to be copied to avoid shared mutable state
		// Each evaluation of {} should create a fresh dict
		newDict := core.NewDict()
		// Copy key-value pairs from the original dict
		for _, key := range v.OriginalKeys() {
			if val, found := v.GetValue(key); found {
				newDict.SetValue(key, val)
			}
		}
		return newDict, nil

	case *core.SetValue:
		// Sets need to be copied to avoid shared mutable state
		// Each evaluation of {x, y, z} should create a fresh set
		newSet := core.NewSet()
		iter := v.Iterator()
		for {
			item, ok := iter.Next()
			if !ok {
				break
			}
			newSet.Add(item)
		}
		return newSet, nil

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

	ctx.PushStack(funcName, "", 0, 0)
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
		"=":                assignForm,
		"annotated-assign": annotatedAssignForm,
		"quote":            quoteForm,
		"lambda":           lambdaForm,
		"fn":               lambdaForm, // Alias for lambda

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
	core.DebugLog("[ASSIGN] assignForm called with %d args\n", args.Len())

	if args.Len() != 2 {
		return nil, fmt.Errorf("= requires 2 arguments, got %d", args.Len())
	}

	// Get the target
	target := args.Items()[0]
	core.DebugLog("[ASSIGN] Target type: %T\n", target)

	// Check if target is a list
	if targetList, ok := target.(*core.ListValue); ok {
		core.DebugLog("[ASSIGN] Target is a list with %d elements\n", targetList.Len())
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
					if targetList.Len() != 3 {
						return nil, fmt.Errorf("invalid dot notation in assignment target")
					}

					// Evaluate the object
					obj, err := Eval(targetList.Items()[1], ctx)
					if err != nil {
						return nil, err
					}

					// Get the attribute name (can be StringValue or SymbolValue)
					var attrName string
					if strName, ok := targetList.Items()[2].(core.StringValue); ok {
						attrName = string(strName)
					} else if symName, ok := targetList.Items()[2].(core.SymbolValue); ok {
						attrName = string(symName)
					} else {
						return nil, fmt.Errorf("attribute name must be a string or symbol")
					}

					// Evaluate the value
					value, err := Eval(args.Items()[1], ctx)
					if err != nil {
						return nil, err
					}

					// Set the attribute
					if objWithAttrs, ok := obj.(interface {
						SetAttr(string, core.Value) error
					}); ok {
						err := objWithAttrs.SetAttr(attrName, value)
						if err != nil {
							return nil, err
						}
						// Python assignments are statements and return None
						return core.None, nil
					}

					// Special handling for dicts
					if dict, ok := obj.(*core.DictValue); ok {
						dict.Set(attrName, value)
						// Python assignments are statements and return None
						return core.None, nil
					}

					return nil, fmt.Errorf("%s does not support attribute assignment", obj.Type())
				}
			}
		}

		// Tuple unpacking: (= (x, y) (10, 20))
		core.DebugLog("[ASSIGN] Tuple unpacking: %d targets\n", targetList.Len())
		// Evaluate the value
		core.DebugLog("[ASSIGN] Evaluating right-hand side\n")
		value, err := Eval(args.Items()[1], ctx)
		if err != nil {
			return nil, err
		}
		core.DebugLog("[ASSIGN] RHS evaluated to %T\n", value)

		// The value must be a tuple or list
		var values []core.Value
		switch v := value.(type) {
		case core.TupleValue:
			core.DebugLog("[ASSIGN] Unpacking tuple with %d elements\n", len(v))
			values = []core.Value(v)
		case *core.ListValue:
			core.DebugLog("[ASSIGN] Unpacking list with %d elements\n", v.Len())
			values = v.Items()
		default:
			return nil, fmt.Errorf("cannot unpack non-sequence %v", value.Type())
		}

		// Check that the number of targets matches the number of values
		if targetList.Len() != len(values) {
			if len(values) < targetList.Len() {
				return nil, fmt.Errorf("not enough values to unpack (expected %d, got %d)", targetList.Len(), len(values))
			}
			return nil, fmt.Errorf("too many values to unpack (expected %d, got %d)", targetList.Len(), len(values))
		}

		// Assign each value to its corresponding target
		core.DebugLog("[ASSIGN] Assigning %d values to targets\n", len(values))
		for i, t := range targetList.Items() {
			// Check if target is a symbol (simple variable)
			if sym, ok := t.(core.SymbolValue); ok {
				core.DebugLog("[ASSIGN] Defining %s = %T\n", string(sym), values[i])
				ctx.Define(string(sym), values[i])
				continue
			}

			// Check if target is dot notation (attribute access)
			if targetList2, ok := t.(*core.ListValue); ok && targetList2.Len() > 0 {
				if sym, ok := targetList2.Items()[0].(core.SymbolValue); ok && string(sym) == "." {
					// This is attribute assignment like self.a = value
					// Use the same logic as single attribute assignment
					// Evaluate to get the (object, attribute) pair, then set it
					if targetList2.Len() != 3 {
						return nil, fmt.Errorf("invalid dot notation in assignment target")
					}

					// Evaluate the object
					obj, err := Eval(targetList2.Items()[1], ctx)
					if err != nil {
						return nil, err
					}

					// Get the attribute name (can be StringValue or SymbolValue)
					var attrName string
					if strName, ok := targetList2.Items()[2].(core.StringValue); ok {
						attrName = string(strName)
					} else if symName, ok := targetList2.Items()[2].(core.SymbolValue); ok {
						attrName = string(symName)
					} else {
						return nil, fmt.Errorf("attribute name must be a string or symbol")
					}

					// Set the attribute using the same pattern as util.go
					if objWithAttrs, ok := obj.(interface {
						SetAttr(string, core.Value) error
					}); ok {
						err := objWithAttrs.SetAttr(attrName, values[i])
						if err != nil {
							return nil, err
						}
						continue
					}

					// Special handling for dicts
					if dict, ok := obj.(*core.DictValue); ok {
						dict.Set(attrName, values[i])
						continue
					}

					return nil, fmt.Errorf("%s does not support attribute assignment", obj.Type())
				}

				// Not dot notation - must be nested unpacking pattern like (a, b)
				// Use generic unpacking
				if err := UnpackPattern(targetList2, values[i], ctx); err != nil {
					return nil, err
				}
				continue
			}

			// Target is neither a symbol, dot notation, nor tuple pattern
			return nil, fmt.Errorf("assignment target must be a symbol, dot notation, or tuple pattern, got %v", t.Type())
		}

		core.DebugLog("[ASSIGN] Tuple unpacking complete\n")
		// Python assignments are statements and return None
		return core.None, nil
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
	// Python assignments are statements and return None
	return core.None, nil
}

// annotatedAssignForm implements annotated assignment (PEP 526): x: int = 5
// Syntax: (annotated-assign target annotation [value])
// Supports: variable (x: int = 5), attribute (obj.x: int = 5), subscript (obj[k]: int = 5)
func annotatedAssignForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() < 2 || args.Len() > 3 {
		return nil, fmt.Errorf("annotated-assign requires 2 or 3 arguments, got %d", args.Len())
	}

	target := args.Items()[0]

	// Get the annotation (always a string)
	annotationVal := args.Items()[1]
	annotationStr, ok := annotationVal.(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("annotation must be a string, got %v", annotationVal.Type())
	}
	annotation := string(annotationStr)

	// Check what kind of target we have
	if sym, ok := target.(core.SymbolValue); ok {
		// Simple variable: x: int = 5
		targetName := string(sym)

		// Ensure __annotations__ dict exists in current scope
		var annotationsDict *core.DictValue
		annVal, err := ctx.Lookup("__annotations__")
		if err == nil {
			// __annotations__ exists
			if dict, isDict := annVal.(*core.DictValue); isDict {
				annotationsDict = dict
			} else {
				// __annotations__ exists but is not a dict, replace it
				annotationsDict = core.NewDict()
				ctx.Define("__annotations__", annotationsDict)
			}
		} else {
			// Create __annotations__ dict
			annotationsDict = core.NewDict()
			ctx.Define("__annotations__", annotationsDict)
		}

		// Store the annotation
		annotationsDict.Set(targetName, core.StringValue(annotation))

		// If there's a value, evaluate and assign it
		if args.Len() == 3 {
			value, err := Eval(args.Items()[2], ctx)
			if err != nil {
				return nil, err
			}
			ctx.Define(targetName, value)
			return value, nil
		}

		// No value - just annotation
		return core.Nil, nil
	} else if targetList, ok := target.(*core.ListValue); ok && targetList.Len() > 0 {
		// Attribute or subscript: obj.x: int = 5 or obj[k]: int = 5
		// For these, we don't store annotations (Python doesn't either)
		// We just perform the assignment if there's a value

		if args.Len() == 3 {
			// Use the regular assignment form which already handles
			// attribute (.) and subscript (get-item) assignments
			assignArgs := core.NewList(target, args.Items()[2])
			return AssignForm(assignArgs, ctx)
		}

		// No value - just annotation, nothing to do
		return core.Nil, nil
	}

	return nil, fmt.Errorf("annotated assignment target must be a symbol, attribute, or subscript, got %T", target)
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

		// Check for flow control - propagate break/continue/return
		// These need to bubble up to the enclosing loop or function
		if _, ok := result.(*ReturnValue); ok {
			return result, nil
		}
		if _, ok := result.(*BreakValue); ok {
			return result, nil
		}
		if _, ok := result.(*ContinueValue); ok {
			return result, nil
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
		// If it's already an ImportError, return it directly so it can be caught by try/except
		if _, ok := err.(*core.ImportError); ok {
			return nil, err
		}
		// Otherwise wrap with context
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

// Exception represents a raised exception
type Exception struct {
	Type    string
	Message string
	Value   core.Value
	Cause   *Exception // Explicit cause (from "raise ... from ...")
	Context *Exception // Implicit context (exception during handling)
}

func (e *Exception) Error() string {
	if e.Message != "" {
		return fmt.Sprintf("%s: %s", e.Type, e.Message)
	}
	return e.Type
}

// ErrorWithChain returns the error string with the full exception chain
func (e *Exception) ErrorWithChain() string {
	var parts []string
	parts = append(parts, e.Error())

	// Show explicit cause first
	if e.Cause != nil {
		parts = append(parts, "\nThe above exception was the direct cause of the following exception:\n")
		parts = append(parts, e.Cause.ErrorWithChain())
	} else if e.Context != nil {
		// Show implicit context if no explicit cause
		parts = append(parts, "\nDuring handling of the above exception, another exception occurred:\n")
		parts = append(parts, e.Context.ErrorWithChain())
	}

	return strings.Join(parts, "")
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
		"AssertionError":    true,
	}
	return knownTypes[name]
}

// isLikelyExceptionType returns true if the name looks like an exception type
// based on naming conventions: starts with capital letter, or underscore+capital
func isLikelyExceptionType(name string) bool {
	if len(name) == 0 {
		return false
	}
	// Starts with capital letter
	if name[0] >= 'A' && name[0] <= 'Z' {
		return true
	}
	// Starts with underscore followed by capital letter (e.g. _Stop)
	if len(name) > 1 && name[0] == '_' && name[1] >= 'A' && name[1] <= 'Z' {
		return true
	}
	return false
}

// errorToExceptionInstance converts any error into a Python exception instance
// This handles both custom Exception types and Go error types (OSError, TypeError, etc.)
func errorToExceptionInstance(err error, ctx *core.Context) core.Value {
	if err == nil {
		return core.Nil
	}

	// Check if it's already a custom Exception with a Value
	if exc, ok := err.(*Exception); ok {
		if exc.Value != nil {
			return exc.Value
		}
		// Create instance from Exception type
		return createPythonExceptionInstance(ctx, exc.Type, exc.Error())
	}

	// Unwrap EvalError if needed
	var baseErr error = err
	var errMsg string

	if evalErr, ok := err.(*core.EvalError); ok && evalErr.Wrapped != nil {
		baseErr = evalErr.Wrapped
		errMsg = evalErr.Wrapped.Error()
	} else if evalErr, ok := err.(*core.EvalError); ok {
		errMsg = evalErr.Message
	} else {
		errMsg = err.Error()
	}

	// Check if error message starts with a Python exception type name
	// Format: "ExceptionType: message"
	if idx := strings.Index(errMsg, ": "); idx > 0 {
		possibleType := errMsg[:idx]
		// List of known exception types that might be in error messages
		knownTypes := []string{
			"SyntaxError", "TypeError", "ValueError", "NameError",
			"AttributeError", "KeyError", "IndexError", "ZeroDivisionError",
			"ImportError", "ModuleNotFoundError", "OSError", "FileNotFoundError",
			"RuntimeError", "NotImplementedError", "StopIteration", "AssertionError",
		}
		for _, knownType := range knownTypes {
			if possibleType == knownType {
				// Extract just the message part (after ": ")
				msgOnly := errMsg[idx+2:]
				return createPythonExceptionInstance(ctx, knownType, msgOnly)
			}
		}
	}

	// Map Go error types to Python exception classes
	switch baseErr.(type) {
	case *core.StopIteration:
		// StopIteration from generators
		return createPythonExceptionInstance(ctx, "StopIteration", errMsg)
	case *protocols.StopIteration:
		// StopIteration from iterators
		return createPythonExceptionInstance(ctx, "StopIteration", errMsg)
	case *core.FileNotFoundError:
		return createPythonExceptionInstance(ctx, "FileNotFoundError", errMsg)
	case *core.OSError:
		return createPythonExceptionInstance(ctx, "OSError", errMsg)
	case *core.NameError:
		return createPythonExceptionInstance(ctx, "NameError", errMsg)
	case *core.TypeError:
		return createPythonExceptionInstance(ctx, "TypeError", errMsg)
	case *core.ZeroDivisionError:
		return createPythonExceptionInstance(ctx, "ZeroDivisionError", errMsg)
	case *core.KeyError:
		return createPythonExceptionInstance(ctx, "KeyError", errMsg)
	case *core.IndexError:
		return createPythonExceptionInstance(ctx, "IndexError", errMsg)
	case *core.ModuleNotFoundError:
		// Must check ModuleNotFoundError before ImportError since it embeds ImportError
		return createPythonExceptionInstance(ctx, "ModuleNotFoundError", errMsg)
	case *core.ImportError:
		return createPythonExceptionInstance(ctx, "ImportError", errMsg)
	case *core.AttributeError:
		return createPythonExceptionInstance(ctx, "AttributeError", errMsg)
	case *core.ValueError:
		return createPythonExceptionInstance(ctx, "ValueError", errMsg)
	case *core.AssertionError:
		return createPythonExceptionInstance(ctx, "AssertionError", errMsg)
	default:
		// Generic exception for unknown error types
		return createPythonExceptionInstance(ctx, "Exception", errMsg)
	}
}

// tryForm implements the try/except/finally special form
// createPythonExceptionInstance creates a proper Python exception instance
// instead of a string, so that caught exceptions have the correct type and attributes
func createPythonExceptionInstance(ctx *core.Context, exceptionType string, message string) core.Value {
	// Try to get the exception class from context
	exceptionClass, err := ctx.Lookup(exceptionType)
	if err != nil {
		// Fall back to string if class not found
		return core.StringValue(message)
	}

	// Check if it's a class
	class, ok := exceptionClass.(*core.Class)
	if !ok {
		// Fall back to string if not a class
		return core.StringValue(message)
	}

	// Instantiate the exception class with the message
	instance, err := class.Call([]core.Value{core.StringValue(message)}, ctx)
	if err != nil {
		// Fall back to string if instantiation fails
		return core.StringValue(message)
	}

	return instance
}

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
	var elseClause *core.ListValue
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
				case "else":
					if i == 0 {
						return nil, fmt.Errorf("try must have a body before else")
					}
					elseClause = list
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

		// If we haven't seen except, else, or finally yet, it's part of the try body
		if len(exceptClauses) == 0 && elseClause == nil && finallyClause == nil {
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

	// If no error, run else clause (if present), then finally, and return
	if tryErr == nil {
		// Execute else clause if present (only runs when no exception occurred)
		if elseClause != nil && elseClause.Len() > 1 {
			for _, expr := range elseClause.Items()[1:] {
				var elseErr error
				result, elseErr = Eval(expr, ctx)
				if elseErr != nil {
					// Error in else clause becomes the new error
					if err := runFinally(); err != nil {
						return nil, err
					}
					return nil, elseErr
				}
			}
		}

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
		var excTypes []string // For tuple exception types like (ValueError, TypeError)
		var excVar string
		var handlerStart int = 1

		if exceptClause.Len() > 1 {
			secondElem := exceptClause.Items()[1]

			// Check if it's a tuple of exception types
			if tupleList, ok := secondElem.(*core.ListValue); ok && tupleList.Len() > 0 {
				if sym, ok := tupleList.Items()[0].(core.SymbolValue); ok && string(sym) == "tuple-literal" {
					// It's a tuple like (tuple-literal ValueError TypeError)
					for i := 1; i < tupleList.Len(); i++ {
						if typeSym, ok := tupleList.Items()[i].(core.SymbolValue); ok {
							excTypes = append(excTypes, string(typeSym))
						}
					}
					handlerStart = 2

					// Check for "as" syntax
					if exceptClause.Len() > 3 {
						if asSym, ok := exceptClause.Items()[2].(core.SymbolValue); ok && string(asSym) == "as" {
							if varSym, ok := exceptClause.Items()[3].(core.SymbolValue); ok {
								excVar = string(varSym)
								handlerStart = 4
							}
						}
					}
				}
			} else if sym, ok := secondElem.(core.SymbolValue); ok {
				// Check if second element is a symbol
				symStr := string(sym)

				// Check for "as" syntax for catch-all
				if symStr == "as" && exceptClause.Len() > 2 {
					// (except as var handler...)
					if varSym, ok := exceptClause.Items()[2].(core.SymbolValue); ok {
						excVar = string(varSym)
						handlerStart = 3
					}
				} else if isExceptionType(symStr) || isLikelyExceptionType(symStr) {
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
		if excType == "" && len(excTypes) == 0 {
			// Catch-all
			matches = true
		} else if len(excTypes) > 0 {
			// Tuple of exception types - match if ANY match
			excInstance := errorToExceptionInstance(tryErr, ctx)

			for _, exType := range excTypes {
				// Look up the target exception class
				targetClassVal, err := ctx.Lookup(exType)
				if err != nil {
					// Exception class not found - only match "Exception" or "Error"
					if exType == "Exception" || exType == "Error" {
						matches = true
						break
					}
				} else if targetClass, ok := targetClassVal.(*core.Class); ok {
					// Use isinstance semantics with proper inheritance
					if inst, ok := excInstance.(*core.Instance); ok {
						if core.IsInstanceOf(inst, targetClass) {
							matches = true
							break
						}
					} else if exType == "Exception" {
						// Not an instance - only match "Exception"
						matches = true
						break
					}
				} else {
					// Target is not a class - fallback to string matching
					if exc, ok := tryErr.(*Exception); ok {
						if exc.Type == exType || exType == "Exception" {
							matches = true
							break
						}
					} else if exType == "Exception" || exType == "Error" {
						matches = true
						break
					}
				}
			}
		} else {
			// Single exception type
			// Convert error to Python exception instance
			excInstance := errorToExceptionInstance(tryErr, ctx)

			// Look up the target exception class
			targetClassVal, err := ctx.Lookup(excType)
			if err != nil {
				// Exception class not found - only match "Exception" or "Error"
				matches = excType == "Exception" || excType == "Error"
			} else if targetClass, ok := targetClassVal.(*core.Class); ok {
				// Use isinstance semantics with proper inheritance
				if inst, ok := excInstance.(*core.Instance); ok {
					matches = core.IsInstanceOf(inst, targetClass)
				} else {
					// Not an instance - only match "Exception"
					matches = excType == "Exception"
				}
			} else {
				// Target is not a class - fallback to string matching
				if exc, ok := tryErr.(*Exception); ok {
					matches = exc.Type == excType || excType == "Exception"
				} else {
					matches = excType == "Exception" || excType == "Error"
				}
			}
		}

		if matches {
			// Use parent context for handler to preserve variable assignments
			handlerCtx := ctx

			// Convert error to exception instance
			excValue := errorToExceptionInstance(tryErr, ctx)

			// Bind exception variable if specified (in parent context to preserve assignments)
			if excVar != "" {
				ctx.Define(excVar, excValue)
			}

			// Store exception info in context for sys.exc_info()
			// Get the exception class
			var excTypeVal core.Value = core.None
			if inst, ok := excValue.(*core.Instance); ok {
				if classVal, hasClass := inst.GetAttr("__class__"); hasClass {
					excTypeVal = classVal
				}
			}

			// Save previous exception info to restore later
			prevExcType := handlerCtx.ExcType
			prevExcValue := handlerCtx.ExcValue
			prevExcTb := handlerCtx.ExcTb
			prevExcError, _ := handlerCtx.Lookup("__current_exception__")

			// Set current exception info
			handlerCtx.ExcType = excTypeVal
			handlerCtx.ExcValue = excValue
			handlerCtx.ExcTb = core.None // Traceback object not yet implemented
			// Store the exception instance for bare raise
			handlerCtx.Define("__current_exception__", excValue)

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

			// Restore previous exception info
			handlerCtx.ExcType = prevExcType
			handlerCtx.ExcValue = prevExcValue
			handlerCtx.ExcTb = prevExcTb
			// Restore previous exception error
			if prevExcError != nil {
				handlerCtx.Define("__current_exception__", prevExcError)
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
//	(raise) - re-raise current exception
func raiseForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	// Bare raise - re-raise current exception
	if args.Len() == 0 {
		// Try to get the current exception from context
		if excVal, err := ctx.Lookup("__current_exception__"); err == nil {
			// Convert exception instance back to error
			if inst, ok := excVal.(*core.Instance); ok {
				// Get exception type name
				var excType string
				if classVal, hasClass := inst.GetAttr("__class__"); hasClass {
					if class, ok := classVal.(*core.Class); ok {
						excType = class.Name
					}
				}
				// Get exception message
				var message string
				if argsAttr, hasArgs := inst.GetAttr("args"); hasArgs {
					if argsTuple, ok := argsAttr.(core.TupleValue); ok && len(argsTuple) > 0 {
						if msgStr, ok := argsTuple[0].(core.StringValue); ok {
							message = string(msgStr)
						}
					}
				}
				return nil, &Exception{Type: excType, Message: message}
			}
		}
		// No current exception to re-raise
		return nil, &Exception{Type: "RuntimeError", Message: "No active exception to re-raise"}
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

	// Three or more arguments - check for "raise X from Y"
	if args.Len() >= 3 {
		// Check if second argument is the "from" keyword
		if fromSym, ok := args.Items()[1].(core.SymbolValue); ok && string(fromSym) == "from" {
			// Evaluate the exception
			excVal, err := Eval(args.Items()[0], ctx)
			if err != nil {
				return nil, err
			}

			// Evaluate the cause
			causeVal, err := Eval(args.Items()[2], ctx)
			if err != nil {
				return nil, err
			}

			// Create the main exception
			mainExc := &Exception{
				Type:    "Exception",
				Message: "",
			}

			// Extract exception info from excVal
			if inst, ok := excVal.(*core.Instance); ok {
				mainExc.Type = inst.Class.Name
				// Try to get message
				if argsAttr, hasArgs := inst.GetAttr("args"); hasArgs {
					if argsTuple, ok := argsAttr.(core.TupleValue); ok && len(argsTuple) > 0 {
						if msgStr, ok := argsTuple[0].(core.StringValue); ok {
							mainExc.Message = string(msgStr)
						}
					}
				}
				if mainExc.Message == "" {
					if msgVal, found := inst.GetAttr("message"); found {
						mainExc.Message = core.PrintValueWithoutQuotes(msgVal)
					}
				}
				mainExc.Value = inst
			} else if class, ok := excVal.(*core.Class); ok {
				// Exception class - instantiate it
				instance, err := class.Call([]core.Value{}, ctx)
				if err != nil {
					return nil, err
				}
				mainExc.Type = class.Name
				mainExc.Value = instance
			} else if msg, ok := excVal.(core.StringValue); ok {
				mainExc.Message = string(msg)
			}

			// Extract cause info
			cause := &Exception{
				Type:    "Exception",
				Message: "",
			}
			if inst, ok := causeVal.(*core.Instance); ok {
				cause.Type = inst.Class.Name
				// Try to get message
				if argsAttr, hasArgs := inst.GetAttr("args"); hasArgs {
					if argsTuple, ok := argsAttr.(core.TupleValue); ok && len(argsTuple) > 0 {
						if msgStr, ok := argsTuple[0].(core.StringValue); ok {
							cause.Message = string(msgStr)
						}
					}
				}
				if cause.Message == "" {
					if msgVal, found := inst.GetAttr("message"); found {
						cause.Message = core.PrintValueWithoutQuotes(msgVal)
					}
				}
				cause.Value = inst
			} else if class, ok := causeVal.(*core.Class); ok {
				// Exception class - instantiate it
				instance, err := class.Call([]core.Value{}, ctx)
				if err != nil {
					return nil, err
				}
				cause.Type = class.Name
				cause.Value = instance
			} else if msg, ok := causeVal.(core.StringValue); ok {
				cause.Message = string(msg)
			}

			// Set __cause__ attribute on main exception's value if it's an instance
			if mainInst, ok := mainExc.Value.(*core.Instance); ok {
				if causeInst, ok := cause.Value.(*core.Instance); ok {
					mainInst.SetAttr("__cause__", causeInst)
				}
			}

			// Link the cause
			mainExc.Cause = cause

			return nil, mainExc
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
	iterableExpr := args.Items()[2]
	iterable, err := Eval(iterableExpr, ctx)
	if err != nil {
		// Debug: show what expression failed
		exprStr := core.PrintValue(iterableExpr)
		if len(exprStr) > 100 {
			exprStr = exprStr[:100] + "..."
		}
		return nil, fmt.Errorf("error evaluating iterable %s: %v", exprStr, err)
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
	iterableExpr := args.Items()[2]
	iterable, err := Eval(iterableExpr, ctx)
	if err != nil {
		// Debug: show what expression failed
		exprStr := core.PrintValue(iterableExpr)
		if len(exprStr) > 100 {
			exprStr = exprStr[:100] + "..."
		}
		return nil, fmt.Errorf("error evaluating iterable %s: %v", exprStr, err)
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

			// Extract variable name(s) from lambda
			if lambdaList.Len() < 3 {
				return nil, fmt.Errorf("invalid lambda in gen-comp")
			}
			params, ok := lambdaList.Items()[1].(*core.ListValue)
			if !ok || params.Len() != 1 {
				return nil, fmt.Errorf("gen-comp lambda must have exactly one parameter")
			}

			// Check if parameter is a list (for unpacking) or a symbol (single variable)
			var varName string
			var varNames []string
			param := params.Items()[0]

			if listParam, ok := param.(*core.ListValue); ok {
				// List parameter (parsed from tuple pattern) - extract variable names for unpacking
				varNames = make([]string, listParam.Len())
				for i, v := range listParam.Items() {
					if sym, ok := v.(core.SymbolValue); ok {
						varNames[i] = string(sym)
					} else {
						return nil, fmt.Errorf("gen-comp lambda tuple parameter must contain only symbols")
					}
				}
			} else if varSym, ok := param.(core.SymbolValue); ok {
				// Single symbol parameter
				varName = string(varSym)
			} else {
				return nil, fmt.Errorf("gen-comp lambda parameter must be a symbol or list, got %T", param)
			}

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
			gen, err := core.NewGeneratorExpression("genexpr", expr, varName, varNames, iterable, condition, ctx, Eval)
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
	iterableExpr := args.Items()[2]
	iterable, err := Eval(iterableExpr, ctx)
	if err != nil {
		// Debug: show what expression failed
		exprStr := core.PrintValue(iterableExpr)
		if len(exprStr) > 100 {
			exprStr = exprStr[:100] + "..."
		}
		return nil, fmt.Errorf("error evaluating iterable %s: %v", exprStr, err)
	}

	// Store the expression, condition (if present), and context
	expr := args.Items()[0]
	var condition core.Value
	if args.Len() == 4 {
		condition = args.Items()[3]
	}

	// Create a Generator object with the expression, variable, iterable, and condition
	gen, err := core.NewGeneratorExpression("genexpr", expr, varName, nil, iterable, condition, ctx, Eval)
	if err != nil {
		return nil, err
	}
	return gen, nil
}

// listLiteralForm implements the list-literal special form
// It evaluates all arguments and returns them as a list
// Supports starred expressions: [1, *other_list, 3]
func listLiteralForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	result := make([]core.Value, 0, args.Len())

	// Evaluate each element
	for _, arg := range args.Items() {
		// Check if this is an unpacking expression: (*unpack-iter expr)
		if list, ok := arg.(*core.ListValue); ok && list.Len() == 2 {
			if sym, ok := list.Items()[0].(core.SymbolValue); ok && string(sym) == "*unpack-iter" {
				// Evaluate the expression to unpack
				val, err := Eval(list.Items()[1], ctx)
				if err != nil {
					return nil, err
				}
				// Unpack the iterable into the result
				items, err := convertIterableToSlice(val)
				if err != nil {
					return nil, fmt.Errorf("cannot unpack non-iterable: %v", err)
				}
				result = append(result, items...)
				continue
			}
		}

		// Regular element - evaluate and append
		val, err := Eval(arg, ctx)
		if err != nil {
			return nil, err
		}
		result = append(result, val)
	}

	return core.NewList(result...), nil
}

// Track recursion depth for tuple literal evaluation to prevent infinite loops
var tupleLiteralDepth = 0

const maxTupleLiteralDepth = 1000

// tupleLiteralForm implements the tuple-literal special form
// Usage: (tuple-literal elem1 elem2 ...)
// Supports starred expressions: (1, *other_tuple, 3)
func tupleLiteralForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	// Increment depth counter and check for infinite recursion
	tupleLiteralDepth++
	defer func() { tupleLiteralDepth-- }()

	if tupleLiteralDepth > maxTupleLiteralDepth {
		return nil, fmt.Errorf("tuple literal evaluation depth exceeded %d - possible infinite loop", maxTupleLiteralDepth)
	}

	core.DebugLog("[TUPLE-LIT] tupleLiteralForm with %d args (depth: %d)\n", args.Len(), tupleLiteralDepth)
	result := make(core.TupleValue, 0, args.Len())

	// Evaluate each element
	for i, arg := range args.Items() {
		core.DebugLog("[TUPLE-LIT] Evaluating element %d: %T\n", i, arg)

		// Check if this is an unpacking expression: (*unpack-iter expr)
		if list, ok := arg.(*core.ListValue); ok && list.Len() == 2 {
			if sym, ok := list.Items()[0].(core.SymbolValue); ok && string(sym) == "*unpack-iter" {
				// Evaluate the expression to unpack
				val, err := Eval(list.Items()[1], ctx)
				if err != nil {
					return nil, err
				}
				// Unpack the iterable into the result
				items, err := convertIterableToSlice(val)
				if err != nil {
					return nil, fmt.Errorf("cannot unpack non-iterable: %v", err)
				}
				result = append(result, items...)
				continue
			}
		}

		// Regular element - evaluate and append
		val, err := Eval(arg, ctx)
		if err != nil {
			return nil, err
		}
		core.DebugLog("[TUPLE-LIT] Element %d evaluated to: %T\n", i, val)
		result = append(result, val)
	}

	core.DebugLog("[TUPLE-LIT] Returning tuple with %d elements\n", len(result))
	return result, nil
}
