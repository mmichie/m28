package core

import (
	"fmt"
)

// Error represents an error that occurred during evaluation
type Error struct {
	Message   string
	StackTrace []TraceEntry
}

// NewError creates a new error with the current stack trace
func NewError(ctx *Context, format string, args ...interface{}) *Error {
	return &Error{
		Message:    fmt.Sprintf(format, args...),
		StackTrace: append([]TraceEntry{}, ctx.CallStack...),
	}
}

// Error implements the error interface
func (e *Error) Error() string {
	trace := "Traceback (most recent call last):\n"
	for i := len(e.StackTrace) - 1; i >= 0; i-- {
		entry := e.StackTrace[i]
		trace += fmt.Sprintf("  File \"%s\", line %d, in %s\n", 
			entry.File, entry.Line, entry.Function)
	}
	return trace + e.Message
}

// ExceptionValue represents an exception that can be caught with try/except
type ExceptionValue struct {
	BaseObject
	Type     Type
	Message  string
	Data     Value
	Traceback []TraceEntry
}

// NewException creates a new exception
func NewException(exceptionType Type, message string, data Value, ctx *Context) *ExceptionValue {
	return &ExceptionValue{
		BaseObject: *NewBaseObject(exceptionType),
		Type:      exceptionType,
		Message:   message,
		Data:      data,
		Traceback: append([]TraceEntry{}, ctx.CallStack...),
	}
}

// String implements Value.String
func (e *ExceptionValue) String() string {
	return fmt.Sprintf("%s: %s", e.Type.Name(), e.Message)
}

// GetAttr implements Object.GetAttr
func (e *ExceptionValue) GetAttr(name string) (Value, bool) {
	switch name {
	case "message":
		return StringValue(e.Message), true
	case "traceback":
		trace := make(ListValue, len(e.Traceback))
		for i, entry := range e.Traceback {
			trace[i] = &DictValue{
				entries: map[string]Value{
					"function": StringValue(entry.Function),
					"file":     StringValue(entry.File),
					"line":     NumberValue(entry.Line),
					"column":   NumberValue(entry.Column),
				},
			}
		}
		return trace, true
	case "data":
		return e.Data, true
	default:
		return e.BaseObject.GetAttr(name)
	}
}