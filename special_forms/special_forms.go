package special_forms

import (
	"github.com/mmichie/m28/core"
)

type SpecialFormFunc func(core.Evaluator, []core.LispValue, core.Environment) (core.LispValue, error)

func GetSpecialForms() map[core.LispSymbol]SpecialFormFunc {
	return map[core.LispSymbol]SpecialFormFunc{
		// Control Flow
		"if":       EvalIfPython,
		"for":      EvalFor,
		"while":    EvalWhilePython,
		"try":      EvalTry,
		"break":    EvalBreak,
		"continue": EvalContinue,
		"pass":     EvalPass,

		// Function and Class Definition
		"def":    EvalDef,
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
		"return": EvalReturn,
		"yield":  EvalYield,
		"del":    EvalDel,
	}
}
