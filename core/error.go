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