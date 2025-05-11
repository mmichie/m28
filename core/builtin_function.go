package core

// BuiltinFunction represents a builtin function that can be called from M28
type BuiltinFunction struct {
	Name     string
	Function func(args ...LispValue) (LispValue, error)
}

// NewBuiltinFunction creates a new builtin function
func NewBuiltinFunction(name string, function func(args ...LispValue) (LispValue, error)) *BuiltinFunction {
	return &BuiltinFunction{
		Name:     name,
		Function: function,
	}
}

// Apply implements the Applicable interface for BuiltinFunction
func (bf *BuiltinFunction) Apply(e Evaluator, args []LispValue, env Environment) (LispValue, error) {
	return bf.Function(args...)
}
