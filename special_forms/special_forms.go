package special_forms

import (
	"github.com/mmichie/m28/core"
)

type SpecialFormFunc func(core.Evaluator, []core.LispValue, core.Environment) (core.LispValue, error)

// Map of special forms that should be registered in the evaluator
var specialForms map[core.LispSymbol]SpecialFormFunc

// Initialize the special forms map
func init() {
	specialForms = map[core.LispSymbol]SpecialFormFunc{
		// Control Flow
		"if":       EvalIfPython,
		"elif":     EvalElif,
		"else":     EvalElse,
		"for":      EvalFor,
		"while":    EvalWhilePython,
		"try":      EvalTry,
		"break":    EvalBreak,
		"continue": EvalContinue,
		"pass":     EvalPass,

		// Function and Class Definition
		"def":    EvalDef,
		"define": EvalDef, // Also add 'define' as an alias for 'def'
		"class":  EvalClass,
		"lambda": EvalLambdaPython,

		// Module Management
		"import": EvalImport,

		// Exception Handling
		"raise":  EvalRaise,
		"assert": EvalAssert,

		// Variable Scope
		"global":   EvalGlobal,
		"nonlocal": EvalNonlocal,

		// Misc
		"with":   EvalWith,
		"begin":  EvalBegin,
		"return": EvalReturn,
		"yield":  EvalYield,
		"del":    EvalDel,
	}
}

func GetSpecialForms() map[core.LispSymbol]SpecialFormFunc {
	return specialForms
}

// RegisterSpecialForms registers the special forms as special functions 
// that will be recognized by the evaluator, even in lambda bodies
func RegisterSpecialForms(env core.Environment) {
	// Register special forms in the environment as special markers
	for name, _ := range specialForms {
		// We register a special marker to indicate this is a special form
		// The actual implementation is in the evaluator
		env.Define(name, core.SpecialFormMarker{Name: name})
	}
}
