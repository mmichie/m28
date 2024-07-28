package special_forms

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

type SpecialFormFunc func(core.Evaluator, []core.LispValue, core.Environment) (core.LispValue, error)

func GetSpecialForms() map[core.LispSymbol]SpecialFormFunc {
	return map[core.LispSymbol]SpecialFormFunc{
		"class":    EvalClass,
		"def":      EvalDef,
		"if":       EvalIfPython,
		"for":      EvalFor,
		"while":    EvalWhilePython,
		"import":   EvalImport,
		"try":      EvalTry,
		"raise":    EvalRaise,
		"with":     EvalWith,
		"lambda":   EvalLambdaPython,
		"return":   EvalReturn,
		"yield":    EvalYield,
		"global":   EvalGlobal,
		"nonlocal": EvalNonlocal,
		"assert":   EvalAssert,
		"del":      EvalDel,
		"break":    EvalBreak,
		"continue": EvalContinue,
		"pass":     EvalPass,
	}
}

func EvalClass(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// Implementation for class definition
	return nil, fmt.Errorf("class: not implemented yet")
}

func EvalDef(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// Implementation for function definition
	return nil, fmt.Errorf("def: not implemented yet")
}

func EvalIfPython(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 || len(args) > 3 {
		return nil, fmt.Errorf("if requires 2 or 3 arguments")
	}

	condition, err := e.Eval(args[0], env)
	if err != nil {
		return nil, err
	}

	if core.IsTruthy(condition) {
		return e.Eval(args[1], env)
	} else if len(args) == 3 {
		return e.Eval(args[2], env)
	}

	return nil, nil
}

func EvalFor(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// Implementation for for loop
	return nil, fmt.Errorf("for: not implemented yet")
}

func EvalWhilePython(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// Implementation for while loop
	return nil, fmt.Errorf("while: not implemented yet")
}

func EvalImport(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// Implementation for import statement
	return nil, fmt.Errorf("import: not implemented yet")
}

func EvalTry(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// Implementation for try-except-finally
	return nil, fmt.Errorf("try: not implemented yet")
}

func EvalRaise(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// Implementation for raise
	return nil, fmt.Errorf("raise: not implemented yet")
}

func EvalWith(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// Implementation for with statement
	return nil, fmt.Errorf("with: not implemented yet")
}

func EvalLambdaPython(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// Implementation for lambda functions
	return nil, fmt.Errorf("lambda: not implemented yet")
}

func EvalReturn(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// Implementation for return statement
	return nil, fmt.Errorf("return: not implemented yet")
}

func EvalYield(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// Implementation for yield statement
	return nil, fmt.Errorf("yield: not implemented yet")
}

func EvalGlobal(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// Implementation for global statement
	return nil, fmt.Errorf("global: not implemented yet")
}

func EvalNonlocal(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// Implementation for nonlocal statement
	return nil, fmt.Errorf("nonlocal: not implemented yet")
}

func EvalAssert(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// Implementation for assert statement
	return nil, fmt.Errorf("assert: not implemented yet")
}

func EvalDel(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// Implementation for del statement
	return nil, fmt.Errorf("del: not implemented yet")
}

func EvalBreak(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// Implementation for break statement
	return nil, fmt.Errorf("break: not implemented yet")
}

func EvalContinue(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// Implementation for continue statement
	return nil, fmt.Errorf("continue: not implemented yet")
}

func EvalPass(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// Implementation for pass statement
	return nil, nil
}
