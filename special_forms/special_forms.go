package special_forms

import (
	"fmt"
	"github.com/mmichie/m28/core"
)

type SpecialFormFunc func(core.Evaluator, []core.LispValue, core.Environment) (core.LispValue, error)

// Map of special forms that should be registered in the evaluator
// This is exported so the evaluator can access it directly
var SpecialForms map[core.LispSymbol]SpecialFormFunc

// Initialize the special forms map
func init() {
	SpecialForms = map[core.LispSymbol]SpecialFormFunc{
		// Control Flow
		"if":    EvalIfPython,
		"for":   EvalFor,
		"while": EvalWhilePython,
		"in":    EvalIn,
		"try":   EvalTry, // Try/except/finally implementation
		"except": func(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
			// This is just a placeholder - except blocks are handled by try
			return nil, fmt.Errorf("except can only be used within a try block")
		},
		"finally": func(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
			// This is just a placeholder - finally blocks are handled by try
			return nil, fmt.Errorf("finally can only be used within a try block")
		},
		"break":    EvalBreak,
		"continue": EvalContinue,
		"pass":     EvalPass,

		// Function and Class Definition
		"def":    EvalDef,
		"define": EvalDef,      // Also add 'define' as an alias for 'def'
		"class":  EvalClassNew, // Class implementation from class_implementation.go
		"lambda": EvalLambdaPython,

		// Module Management
		"import": EvalImport, // Python-style import

		// Exception Handling
		"raise":        EvalRaise,
		"assert":       EvalAssert,
		"defexception": EvalDefException,

		// Variable Scope
		"global":   EvalGlobal,
		"nonlocal": EvalNonlocal,

		// Misc
		//"with":   EvalWith, // Commented out to fix build
		"begin":  EvalBegin,
		"return": EvalReturn,
		"yield":  EvalYield,
		"del":    EvalDel,

		// List and Control Flow
		"cond":   EvalCond,
		"case":   EvalCase,
		"when":   EvalWhen,
		"unless": EvalUnless,
		"let":    EvalLet,
	}
}

/* Commented out to fix build errors
// Register exports the special forms to the evaluator
func Register(evaluator core.Evaluator) {
	for name, formFunc := range specialForms {
		wrapped := wrapSpecialForm(name, formFunc)
		evaluator.(*core.Evaluator).RegisterSpecialForm(name, wrapped)
	}
}

// Helper function to wrap special forms properly
func wrapSpecialForm(name core.LispSymbol, formFunc SpecialFormFunc) core.SpecialForm {
	return func(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
		return formFunc(e, args, env)
	}
}
*/

// GetAllSpecialForms returns a list of all registered special form names
func GetAllSpecialForms() []string {
	result := make([]string, 0, len(SpecialForms))
	for form := range SpecialForms {
		result = append(result, string(form))
	}
	return result
}

// HasSpecialForm checks if a symbol is a special form
func HasSpecialForm(symbol core.LispSymbol) bool {
	_, exists := SpecialForms[symbol]
	return exists
}
