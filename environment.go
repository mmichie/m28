package m28

// Environment represents a Lisp environment
type Environment struct {
	vars  map[LispSymbol]LispValue
	outer *Environment
}

// NewEnvironment creates a new environment
func NewEnvironment(outer *Environment) *Environment {
	env := &Environment{
		vars:  make(map[LispSymbol]LispValue),
		outer: outer,
	}
	setupBuiltins(env)
	return env
}

// Get retrieves a value from the environment
func (e *Environment) Get(symbol LispSymbol) (LispValue, bool) {
	value, ok := e.vars[symbol]
	if !ok && e.outer != nil {
		return e.outer.Get(symbol)
	}
	return value, ok
}

// Set sets a value in the environment
func (e *Environment) Set(symbol LispSymbol, value LispValue) {
	e.vars[symbol] = value
}
