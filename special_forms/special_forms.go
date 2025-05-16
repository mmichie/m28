package special_forms

import (
	"fmt"
	"github.com/mmichie/m28/core"
)

type SpecialFormFunc func(core.Evaluator, []core.LispValue, core.Environment) (core.LispValue, error)

// Map of special forms that should be registered in the evaluator
var specialForms map[core.LispSymbol]SpecialFormFunc

// Initialize the special forms map

func init() {
	specialForms = map[core.LispSymbol]SpecialFormFunc{
		// Control Flow
		"if":    EvalIfPython,
		"elif":  EvalElif,
		"else":  EvalElse,
		"for":   EvalFor,
		"while": EvalWhilePython,
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

		// Object Oriented Programming
		// Dot notation forms are registered in RegisterDotForms

		// Module Management
		"import": EvalImport,

		// Exception Handling
		"raise":        EvalRaise,
		"assert":       EvalAssert,
		"defexception": EvalDefException,

		// Variable Scope
		"global":   EvalGlobal,
		"nonlocal": EvalNonlocal,
		"let":      EvalLet,

		// Misc
		"with":   EvalWith,
		"begin":  EvalBegin,
		"return": EvalReturn,
		"yield":  EvalYield,
		"del":    EvalDel,
	}

	// Register only the enhanced dot notation special forms
	// We no longer register the legacy dot forms first
	EnableEnhancedDotForms(specialForms)

	// Register property access helpers (not replaced by EnableEnhancedDotForms)
	specialForms["set-prop"] = EvalSetProperty
	specialForms["get-prop"] = EvalGetProperty

	// Register class forms from class_implementation.go
	RegisterClassForms(specialForms)

	// Call RegisterConcurrencyForms, but the actual handlers will be set later
	// by the concurrency package to avoid import cycles
	RegisterConcurrencyForms()
}

func GetSpecialForms() map[core.LispSymbol]SpecialFormFunc {
	return specialForms
}

// RegisterSpecialForms registers the special forms as special functions
// that will be recognized by the evaluator, even in lambda bodies
func RegisterSpecialForms(env core.Environment) {
	// Register special forms in the environment as special markers
	for name := range specialForms {
		// We register a special marker to indicate this is a special form
		// The actual implementation is in the evaluator
		env.Define(name, core.SpecialFormMarker{Name: name})
	}
}
