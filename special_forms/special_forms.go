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
	if len(args) < 2 {
		return nil, fmt.Errorf("class definition requires at least a name and a body")
	}
	className, ok := args[0].(core.LispSymbol)
	if !ok {
		return nil, fmt.Errorf("class name must be a symbol")
	}

	classEnv := env.NewEnvironment(env)
	for _, expr := range args[1:] {
		_, err := e.Eval(expr, classEnv)
		if err != nil {
			return nil, err
		}
	}

	class := &core.Lambda{Params: []core.LispSymbol{}, Body: core.LispList(args[1:]), Env: classEnv}
	env.Define(className, class)
	return class, nil
}

func EvalDef(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("def requires at least a name and a body")
	}

	// Check if the first argument is a list
	funcDef, ok := args[0].(core.LispList)
	if !ok || len(funcDef) == 0 {
		return nil, fmt.Errorf("invalid function definition")
	}

	// The first element of the list should be the function name
	funcName, ok := funcDef[0].(core.LispSymbol)
	if !ok {
		return nil, fmt.Errorf("function name must be a symbol")
	}

	// The rest of the elements in funcDef are the parameters
	params := make([]core.LispSymbol, len(funcDef)-1)
	for i, param := range funcDef[1:] {
		paramSymbol, ok := param.(core.LispSymbol)
		if !ok {
			return nil, fmt.Errorf("function parameter must be a symbol")
		}
		params[i] = paramSymbol
	}

	// The body is the rest of the args
	body := core.LispList(args[1:])

	function := &core.Lambda{Params: params, Body: body, Env: env, Closure: env}
	env.Define(funcName, function)
	return function, nil
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

	return core.PythonicNone{}, nil
}

func EvalFor(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 3 {
		return nil, fmt.Errorf("for loop requires at least 3 arguments")
	}

	iterVar, ok := args[0].(core.LispSymbol)
	if !ok {
		return nil, fmt.Errorf("iteration variable must be a symbol")
	}

	iterable, err := e.Eval(args[1], env)
	if err != nil {
		return nil, err
	}

	iter, ok := iterable.(core.LispList)
	if !ok {
		return nil, fmt.Errorf("for loop requires a list")
	}

	loopEnv := env.NewEnvironment(env)
	var result core.LispValue
	for _, item := range iter {
		loopEnv.Set(iterVar, item)
		for _, expr := range args[2:] {
			result, err = e.Eval(expr, loopEnv)
			if err != nil {
				return nil, err
			}
		}
	}

	return result, nil
}

func EvalWhilePython(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("while loop requires at least 2 arguments")
	}

	var result core.LispValue

	for {
		condition, err := e.Eval(args[0], env)
		if err != nil {
			return nil, err
		}

		if !core.IsTruthy(condition) {
			break
		}

		for _, expr := range args[1:] {
			result, err = e.Eval(expr, env)
			if err != nil {
				return nil, err
			}
		}
	}

	return result, nil
}

func EvalImport(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("import requires at least one argument")
	}

	moduleName, ok := args[0].(core.LispSymbol)
	if !ok {
		return nil, fmt.Errorf("module name must be a symbol")
	}

	// Here you would implement the actual module importing logic
	// For now, we'll just create a dummy module
	module := core.NewPythonicDict()
	env.Define(moduleName, module)

	return module, nil
}

func EvalTry(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("try requires at least a try block and an except block")
	}

	tryBlock := args[0]
	exceptBlocks := args[1:]

	result, err := e.Eval(tryBlock, env)
	if err == nil {
		return result, nil
	}

	for _, exceptBlock := range exceptBlocks {
		exceptClause, ok := exceptBlock.(core.LispList)
		if !ok || len(exceptClause) < 2 {
			continue
		}

		exceptionType, ok := exceptClause[0].(core.LispSymbol)
		if !ok {
			continue
		}

		if string(exceptionType) == "except" {
			return e.Eval(exceptClause[1], env)
		}
	}

	return nil, err
}

func EvalRaise(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("raise requires exactly one argument")
	}

	exception, err := e.Eval(args[0], env)
	if err != nil {
		return nil, err
	}

	return nil, fmt.Errorf("%v", exception)
}

func EvalWith(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// Simplified implementation without context manager support
	if len(args) < 2 {
		return nil, fmt.Errorf("with requires at least 2 arguments")
	}

	withEnv := env.NewEnvironment(env)

	var result core.LispValue
	var err error
	for _, expr := range args[1:] {
		result, err = e.Eval(expr, withEnv)
		if err != nil {
			return nil, err
		}
	}

	return result, nil
}

func EvalLambdaPython(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("lambda requires at least parameters and a body")
	}

	params, ok := args[0].(core.LispList)
	if !ok {
		return nil, fmt.Errorf("lambda parameters must be a list")
	}

	paramSymbols := make([]core.LispSymbol, len(params))
	for i, param := range params {
		symbol, ok := param.(core.LispSymbol)
		if !ok {
			return nil, fmt.Errorf("lambda parameter must be a symbol")
		}
		paramSymbols[i] = symbol
	}

	return &core.Lambda{Params: paramSymbols, Body: args[1], Env: env}, nil
}

func EvalReturn(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) > 1 {
		return nil, fmt.Errorf("return takes at most one argument")
	}

	if len(args) == 0 {
		return nil, nil
	}

	return e.Eval(args[0], env)
}

func EvalYield(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("yield requires exactly one argument")
	}

	return e.Eval(args[0], env)
}

func EvalGlobal(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	for _, arg := range args {
		symbol, ok := arg.(core.LispSymbol)
		if !ok {
			return nil, fmt.Errorf("global arguments must be symbols")
		}
		env.Define(symbol, nil)
	}
	return nil, nil
}

func EvalNonlocal(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// Simplified implementation without true nonlocal behavior
	for _, arg := range args {
		symbol, ok := arg.(core.LispSymbol)
		if !ok {
			return nil, fmt.Errorf("nonlocal arguments must be symbols")
		}
		env.Define(symbol, nil)
	}
	return nil, nil
}

func EvalAssert(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 1 || len(args) > 2 {
		return nil, fmt.Errorf("assert takes 1 or 2 arguments")
	}

	condition, err := e.Eval(args[0], env)
	if err != nil {
		return nil, err
	}

	if !core.IsTruthy(condition) {
		message := "Assertion failed"
		if len(args) == 2 {
			messageVal, err := e.Eval(args[1], env)
			if err != nil {
				return nil, err
			}
			message = fmt.Sprintf("%v", messageVal)
		}
		return nil, fmt.Errorf(message)
	}

	return nil, nil
}

func EvalDel(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// Simplified implementation without true deletion
	for _, arg := range args {
		symbol, ok := arg.(core.LispSymbol)
		if !ok {
			return nil, fmt.Errorf("del arguments must be symbols")
		}
		env.Set(symbol, nil)
	}
	return nil, nil
}

func EvalBreak(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	return nil, fmt.Errorf("break encountered")
}

func EvalContinue(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	return nil, fmt.Errorf("continue encountered")
}

func EvalPass(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	return nil, nil
}

func parseFunctionDefinition(args []core.LispValue) ([]core.LispSymbol, core.LispValue, error) {
	if len(args) < 1 {
		return nil, nil, fmt.Errorf("function definition requires parameters and a body")
	}

	paramList, ok := args[0].(core.LispList)
	if !ok {
		return nil, nil, fmt.Errorf("function parameters must be a list")
	}

	params := make([]core.LispSymbol, len(paramList))
	for i, param := range paramList {
		symbol, ok := param.(core.LispSymbol)
		if !ok {
			return nil, nil, fmt.Errorf("function parameter must be a symbol")
		}
		params[i] = symbol
	}

	body := args[1]

	return params, body, nil
}
