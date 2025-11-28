package core

import (
	"fmt"
)

// ErrorType is a special type for errors
const ErrorType Type = "error"

// ExceptionValue represents an exception in the language
type ExceptionValue struct {
	BaseObject
	Message string
	Cause   error
}

// NewException creates a new exception with a message
func NewException(message string) *ExceptionValue {
	exception := &ExceptionValue{
		BaseObject: *NewBaseObject(ErrorType),
		Message:    message,
	}
	return exception
}

// NewExceptionWithCause creates a new exception with a message and a cause
func NewExceptionWithCause(message string, cause error) *ExceptionValue {
	exception := &ExceptionValue{
		BaseObject: *NewBaseObject(ErrorType),
		Message:    message,
		Cause:      cause,
	}
	return exception
}

// Error implements the error interface
func (e *ExceptionValue) Error() string {
	if e.Cause != nil {
		return fmt.Sprintf("%s: %s", e.Type(), e.Message)
	}
	return e.Message
}

// String implements Value.String
func (e *ExceptionValue) String() string {
	if e.Cause != nil {
		return fmt.Sprintf("%s: %s\nCaused by: %v", e.Type(), e.Message, e.Cause)
	}
	return fmt.Sprintf("%s: %s", e.Type(), e.Message)
}

// PrintValue for error values
func PrintErrorValue(err error) string {
	if e, ok := err.(*ExceptionValue); ok {
		return e.String()
	}
	return err.Error()
}

// TracebackType represents a Python traceback object
type TracebackType struct {
	BaseObject
	Frame Value // tb_frame
}

// NewTraceback creates a new traceback object
func NewTraceback() *TracebackType {
	tb := &TracebackType{
		BaseObject: *NewBaseObject(Type("traceback")),
		Frame:      NewFrame(),
	}
	return tb
}

func (t *TracebackType) String() string {
	return "<traceback object>"
}

// GetAttr implements Object.GetAttr for traceback
func (t *TracebackType) GetAttr(name string) (Value, bool) {
	if name == "tb_frame" {
		return t.Frame, true
	}
	return t.BaseObject.GetAttr(name)
}

// FrameType represents a Python frame object
type FrameType struct {
	BaseObject
}

// NewFrame creates a new frame object
func NewFrame() *FrameType {
	return &FrameType{
		BaseObject: *NewBaseObject(Type("frame")),
	}
}

func (f *FrameType) String() string {
	return "<frame object>"
}

// CodeType represents a Python code object
type CodeType struct {
	BaseObject
	Function Value // Reference to the function this code belongs to
}

// PythonError wraps a Python exception instance so it can propagate through Go's error system
// while preserving the original exception type for try/except matching
type PythonError struct {
	Instance *Instance // The Python exception instance
}

// NewPythonError creates a new PythonError wrapping an exception instance
func NewPythonError(inst *Instance) *PythonError {
	return &PythonError{Instance: inst}
}

// Error implements the error interface
func (e *PythonError) Error() string {
	if e.Instance == nil {
		return "Exception"
	}
	// Format as "ExceptionType: message"
	className := e.Instance.Class.Name
	if argsAttr, hasArgs := e.Instance.GetAttr("args"); hasArgs {
		if argsTuple, ok := argsAttr.(TupleValue); ok && len(argsTuple) > 0 {
			return fmt.Sprintf("%s: %s", className, argsTuple[0].String())
		}
	}
	return className
}

// GetExceptionType returns the class name of the wrapped exception
func (e *PythonError) GetExceptionType() string {
	if e.Instance == nil || e.Instance.Class == nil {
		return "Exception"
	}
	return e.Instance.Class.Name
}

// IsSubclassOf checks if the wrapped exception is a subclass of the given type
func (e *PythonError) IsSubclassOf(typeName string) bool {
	if e.Instance == nil || e.Instance.Class == nil {
		return false
	}
	// Check the class and all its bases
	return isClassSubclassOf(e.Instance.Class, typeName)
}

// isClassSubclassOf recursively checks if a class is a subclass of the given type name
func isClassSubclassOf(cls *Class, typeName string) bool {
	if cls == nil {
		return false
	}
	if cls.Name == typeName {
		return true
	}
	// Check single parent (deprecated but still used)
	if cls.Parent != nil {
		if isClassSubclassOf(cls.Parent, typeName) {
			return true
		}
	}
	// Check multiple parents
	for _, parent := range cls.Parents {
		if isClassSubclassOf(parent, typeName) {
			return true
		}
	}
	return false
}

// NewCodeObject creates a new code object for a function
func NewCodeObject(fn Value) *CodeType {
	return &CodeType{
		BaseObject: *NewBaseObject(Type("code")),
		Function:   fn,
	}
}

func (c *CodeType) String() string {
	return "<code object>"
}
