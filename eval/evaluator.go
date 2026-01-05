// Package eval provides the evaluation system for M28 expressions.
package eval

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// Evaluator configuration constants
const (
	// MaxExpressionDisplayLength is the maximum length of expression strings
	// displayed in debug logging before truncation
	MaxExpressionDisplayLength = 150
)

func init() {
	// Set the EvalHook so that builtin eval() can work
	core.EvalHook = Eval
}

// Eval evaluates an expression in a context
func Eval(expr core.Value, ctx *core.Context) (core.Value, error) {
	// Extract location if expression is wrapped, push onto stack
	// This makes location available to all code via ctx.CurrentLocation()
	// and eliminates need for unwrapping in evaluator internals
	if located, ok := expr.(core.LocatedValue); ok {
		if located.Location != nil {
			ctx.PushLocation(located.Location)
			defer ctx.PopLocation()
		}
		expr = located.Unwrap()
	}

	// Increment evaluation counter and log progress periodically
	if ctx != nil {
		ctx.EvalCount++
		// Disable debug logging for normal operation
		logThis := false
		if logThis {
			// Show progress every 100 evaluations (lowered for debugging slow modules)
			exprStr := core.PrintValue(expr)
			if len(exprStr) > MaxExpressionDisplayLength {
				exprStr = exprStr[:MaxExpressionDisplayLength] + "..."
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

			core.Log.Debug(core.SubsystemEval, "Evaluation progress",
				"eval_count", ctx.EvalCount, "context_info", contextInfo, "expression", exprStr)
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
			// Attach location to NameError from context stack
			if nameErr, ok := err.(*core.NameError); ok {
				nameErr.Location = ctx.CurrentLocation()
			}
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
		// Use smart accessor to auto-unwrap LocatedValue
		if sym, ok := v.GetItemAsSymbol(0); ok {
			core.DebugLog("[EVAL-LIST] First element is symbol: %s\n", string(sym))

			if handler, ok := specialForms[string(sym)]; ok {
				core.DebugLog("[EVAL-LIST] Is special form: %s\n", string(sym))
				return handler(core.NewList(v.Items()[1:]...), ctx)
			}

			// Check if it's a macro call (function with __macro__ attribute)
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
		"quote":  quoteForm,
		"lambda": lambdaForm,
		// Note: "fn" was removed as an alias for lambda because it conflicts with
		// Python code that uses "fn" as a variable name (e.g., fn = some_func; fn(args))

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
	}
	specialForms[name] = handler
}

// ifForm delegates to IfForm in util.go
