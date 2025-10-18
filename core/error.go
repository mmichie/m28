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
