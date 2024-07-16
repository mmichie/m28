package m28

// LispValue represents any Lisp value
type LispValue interface{}

// LispSymbol represents a Lisp symbol
type LispSymbol string

// LispList represents a Lisp list
type LispList []LispValue

// LispFunc represents a Lisp function
type LispFunc func([]LispValue, *Environment) (LispValue, error)

// Lambda represents a lambda function
type Lambda struct {
	Params    []LispSymbol
	RestParam LispSymbol
	Body      LispValue
	Env       *Environment
	Closure   *Environment
}

// Macro represents a Lisp macro
type Macro struct {
	Params    []LispSymbol
	RestParam LispSymbol
	Body      LispValue
	Env       *Environment
}

// Quasiquote represents a quasiquoted expression
type Quasiquote struct {
	Expr LispValue
}

// Unquote represents an unquoted expression
type Unquote struct {
	Expr LispValue
}

// UnquoteSplicing represents an unquote-splicing expression
type UnquoteSplicing struct {
	Expr LispValue
}
