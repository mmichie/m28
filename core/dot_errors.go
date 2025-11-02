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
	Name     string
	Location *SourceLocation
}

func (e *NameError) Error() string {
	msg := fmt.Sprintf("name error: name '%s' is not defined", e.Name)
	if e.Location != nil {
		return fmt.Sprintf("%s at %s", msg, e.Location.String())
	}
	return msg
}

// TypeError is raised when a type error occurs
type TypeError struct {
	Message  string
	Expected string
	Got      string
	Location *SourceLocation
}

func (e *TypeError) Error() string {
	if e.Location != nil {
		return fmt.Sprintf("%s at %s", e.Message, e.Location.String())
	}
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
	SyntaxKind int // Which frontend syntax (ast.SyntaxKind)
}

func (e *EvalError) Error() string {
	return fmt.Sprintf("%s: %s", e.Type, e.Message)
}

// IndexError represents an index out of bounds error
type IndexError struct {
	Index    int
	Length   int
	Location *SourceLocation
}

func (e *IndexError) Error() string {
	msg := fmt.Sprintf("list index out of range (index %d, length %d)", e.Index, e.Length)
	if e.Location != nil {
		return fmt.Sprintf("%s at %s", msg, e.Location.String())
	}
	return msg
}

// KeyError represents a missing dictionary key
type KeyError struct {
	Key      Value
	Location *SourceLocation
}

func (e *KeyError) Error() string {
	msg := fmt.Sprintf("key not found: %s", PrintValue(e.Key))
	if e.Location != nil {
		return fmt.Sprintf("%s at %s", msg, e.Location.String())
	}
	return msg
}

// ZeroDivisionError represents division by zero
type ZeroDivisionError struct {
	Location *SourceLocation
}

func (e *ZeroDivisionError) Error() string {
	msg := "division by zero"
	if e.Location != nil {
		return fmt.Sprintf("%s at %s", msg, e.Location.String())
	}
	return msg
}

// NewZeroDivisionError creates a new zero division error
func NewZeroDivisionError() *ZeroDivisionError {
	return &ZeroDivisionError{}
}

// SystemExit represents a request to exit the program
type SystemExit struct {
	Code     int
	Location *SourceLocation
}

func (e *SystemExit) Error() string {
	return "SystemExit"
}

// NewSystemExit creates a new SystemExit error
func NewSystemExit(code int) *SystemExit {
	return &SystemExit{Code: code}
}

// ImportError represents a module import error
type ImportError struct {
	ModuleName string
	Message    string
	Location   *SourceLocation
}

func (e *ImportError) Error() string {
	msg := e.Message
	if msg == "" {
		msg = fmt.Sprintf("no module named '%s'", e.ModuleName)
	}
	if e.Location != nil {
		return fmt.Sprintf("%s at %s", msg, e.Location.String())
	}
	return msg
}

// AttributeError represents an attribute access error
type AttributeError struct {
	ObjType  string
	AttrName string
	Message  string
	Location *SourceLocation
}

func (e *AttributeError) Error() string {
	msg := e.Message
	if msg == "" {
		msg = fmt.Sprintf("'%s' object has no attribute '%s'", e.ObjType, e.AttrName)
	}
	if e.Location != nil {
		return fmt.Sprintf("%s at %s", msg, e.Location.String())
	}
	return msg
}

// OSError represents an operating system error
type OSError struct {
	Message  string
	Errno    int // Optional: errno number
	Filename string
	Location *SourceLocation
}

func (e *OSError) Error() string {
	msg := e.Message
	if msg == "" {
		msg = "OS error"
	}
	if e.Filename != "" {
		msg = fmt.Sprintf("%s: %s", msg, e.Filename)
	}
	if e.Location != nil {
		return fmt.Sprintf("%s at %s", msg, e.Location.String())
	}
	return msg
}

// NewOSError creates a new OS error
func NewOSError(message string, filename string) *OSError {
	return &OSError{
		Message:  message,
		Filename: filename,
	}
}

// FileNotFoundError represents a file not found error (subclass of OSError in Python)
type FileNotFoundError struct {
	Message  string
	Errno    int // Optional: errno number
	Filename string
	Location *SourceLocation
}

func (e *FileNotFoundError) Error() string {
	msg := e.Message
	if msg == "" {
		msg = "No such file or directory"
	}
	if e.Filename != "" {
		msg = fmt.Sprintf("%s: %s", msg, e.Filename)
	}
	if e.Location != nil {
		return fmt.Sprintf("%s at %s", msg, e.Location.String())
	}
	return msg
}

// NewFileNotFoundError creates a new file not found error
func NewFileNotFoundError(message string, filename string) *FileNotFoundError {
	return &FileNotFoundError{
		Message:  message,
		Filename: filename,
	}
}

// NewTypeError creates a new type error
func NewTypeError(expected string, got Value, context string) *TypeError {
	var gotType string
	if got == nil {
		gotType = "nil"
	} else {
		gotType = string(got.Type())
	}

	// Use TypeDescriptor for better type names (only if got is not nil)
	if got != nil {
		if desc := GetTypeDescriptorForValue(got); desc != nil {
			gotType = desc.PythonName
		}
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

	// Check if it's a NameError and preserve the detail
	if nameErr, ok := err.(*NameError); ok {
		return &EvalError{
			Type:    "NameError",
			Message: fmt.Sprintf("name '%s' is not defined", nameErr.Name),
			Wrapped: err,
		}
	}

	return &EvalError{
		Type:    "EvalError",
		Message: message,
		Wrapped: err,
	}
}
