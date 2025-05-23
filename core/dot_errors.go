package core

import "fmt"

// Error message templates for dot notation
const (
	ErrDotMissingArgs      = "Dot notation requires both an object and a property/method name"
	ErrDotObjectEval       = "Failed to evaluate object for dot notation: %v"
	ErrDotPropertyEval     = "Failed to evaluate property name for dot notation: %v"
	ErrDotNoInterface      = "Type %T doesn't support dot notation (doesn't implement ObjProtocol interface)"
	ErrDotPropertyNotFound = "Object has no property '%s'"
	ErrDotModuleNoProperty = "Module '%s' has no property '%s'"
	ErrDotPropertyType     = "Property name must be a string or symbol, got %T"
	ErrDotMethodNotFound   = "Object has no method '%s'"
	ErrDotMethodCallError  = "Error calling method '%s': %v"
	ErrDotNotCallable      = "Property '%s' is not callable (type: %T)"
	ErrDotNestedAccess     = "Error accessing property %s.%s: %v"
	ErrDotEvaluatorMissing = "No evaluator available for applying lambda method '%s'"
	ErrTooManyArguments    = "Method '%s' takes %d arguments, got %d"
)

// Helper functions to generate error instances
func ErrDotNoPropertyf(name string) error {
	return fmt.Errorf(ErrDotPropertyNotFound, name)
}

func ErrDotNoMethodf(name string) error {
	return fmt.Errorf(ErrDotMethodNotFound, name)
}

func ErrDotMissingInterfacef(objType interface{}) error {
	return fmt.Errorf(ErrDotNoInterface, objType)
}

func ErrDotModulePropertyf(moduleName, propName string) error {
	return fmt.Errorf(ErrDotModuleNoProperty, moduleName, propName)
}

func ErrDotMethodCallf(methodName string, err error) error {
	return fmt.Errorf(ErrDotMethodCallError, methodName, err)
}

func ErrDotNotCallablef(propName string, propType interface{}) error {
	return fmt.Errorf(ErrDotNotCallable, propName, propType)
}

func ErrDotNestedAccessf(path, prop string, err error) error {
	return fmt.Errorf(ErrDotNestedAccess, path, prop, err)
}

func ErrTooManyArgumentsf(methodName string, expected, got int) error {
	return fmt.Errorf(ErrTooManyArguments, methodName, expected, got)
}

// NameError is raised when a name is not found
type NameError struct {
	Name string
}

func (e *NameError) Error() string {
	return fmt.Sprintf("name error: name '%s' is not defined", e.Name)
}

// TypeError is raised when a type error occurs
type TypeError struct {
	Message  string
	Expected string
	Got      string
}

func (e *TypeError) Error() string {
	return e.Message
}

// EvalError is raised during evaluation with context
type EvalError struct {
	Type       string
	Message    string
	File       string
	Line       int
	Column     int
	Suggestion string
	Wrapped    error
}

func (e *EvalError) Error() string {
	return fmt.Sprintf("%s: %s", e.Type, e.Message)
}

// IndexError represents an index out of bounds error
type IndexError struct {
	Index  int
	Length int
}

func (e *IndexError) Error() string {
	return fmt.Sprintf("list index out of range (index %d, length %d)", e.Index, e.Length)
}

// KeyError represents a missing dictionary key
type KeyError struct {
	Key Value
}

func (e *KeyError) Error() string {
	return fmt.Sprintf("key not found: %s", PrintValue(e.Key))
}

// ZeroDivisionError represents division by zero
type ZeroDivisionError struct{}

func (e *ZeroDivisionError) Error() string {
	return "division by zero"
}

// NewTypeError creates a new type error
func NewTypeError(expected string, got Value, context string) *TypeError {
	gotType := string(got.Type())
	
	// Use TypeDescriptor for better type names
	if desc := GetTypeDescriptorForValue(got); desc != nil {
		gotType = desc.PythonName
	}
	
	return &TypeError{
		Expected: expected,
		Got:      gotType,
		Message:  fmt.Sprintf("%s: expected %s, got %s", context, expected, gotType),
	}
}

// WrapEvalError wraps an existing error with evaluation context
func WrapEvalError(err error, message string, ctx *Context) *EvalError {
	if evalErr, ok := err.(*EvalError); ok {
		// Don't double-wrap eval errors
		return evalErr
	}
	
	return &EvalError{
		Type:    "EvalError",
		Message: message,
		Wrapped: err,
	}
}
