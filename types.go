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
	Params []LispSymbol
	Body   LispValue
	Env    *Environment
}
