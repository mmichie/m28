package m28

type SpecialFormFunc func([]LispValue, *Environment) (LispValue, error)

var specialForms map[LispSymbol]SpecialFormFunc

func init() {
	specialForms = map[LispSymbol]SpecialFormFunc{
		"quote":        evalQuote,
		"if":           evalIf,
		"define":       evalDefine,
		"lambda":       evalLambda,
		"begin":        evalBegin,
		"do":           evalDo,
		"let":          evalLet,
		"set!":         evalSet,
		"cond":         evalCond,
		"case":         evalCase,
		"and":          evalAnd,
		"define-macro": evalDefineMacro,
		"macroexpand":  evalMacroexpand,
	}
}
