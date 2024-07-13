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
	if outer == nil {
		setupBuiltins(env)
	}
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

// Define creates a new binding in the current environment
func (e *Environment) Define(symbol LispSymbol, value LispValue) {
	e.vars[symbol] = value
}

// SetMutable sets a value in the environment, searching up the chain
func (e *Environment) SetMutable(symbol LispSymbol, value LispValue) bool {
	if _, ok := e.vars[symbol]; ok {
		e.vars[symbol] = value
		return true
	}
	if e.outer != nil {
		return e.outer.SetMutable(symbol, value)
	}
	return false
}
