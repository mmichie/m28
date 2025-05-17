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
