package core

import (
	"fmt"
	"strconv"
	"strings"
)

// NumberValue represents a numeric value
type NumberValue float64

// Type implements Value.Type
func (n NumberValue) Type() Type {
	return NumberType
}

// String implements Value.String
func (n NumberValue) String() string {
	// Format the number to avoid unnecessary decimal places
	s := strconv.FormatFloat(float64(n), 'f', -1, 64)
	return s
}

// StringValue represents a string value
type StringValue string

// Type implements Value.Type
func (s StringValue) Type() Type {
	return StringType
}

// String implements Value.String
func (s StringValue) String() string {
	return string(s)
}

// GetAttr implements basic string methods
func (s StringValue) GetAttr(name string) (Value, bool) {
	switch name {
	case "length":
		return NumberValue(len(s)), true
	case "upper":
		return &BuiltinMethod{
			fn: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				return StringValue(strings.ToUpper(string(s))), nil
			},
		}, true
	case "lower":
		return &BuiltinMethod{
			fn: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				return StringValue(strings.ToLower(string(s))), nil
			},
		}, true
	default:
		return nil, false
	}
}

// BoolValue represents a boolean value
type BoolValue bool

// Type implements Value.Type
func (b BoolValue) Type() Type {
	return BoolType
}

// String implements Value.String
func (b BoolValue) String() string {
	if b {
		return "true"
	}
	return "false"
}

// Pre-defined boolean constants
var (
	True  = BoolValue(true)
	False = BoolValue(false)
)

// SymbolValue represents a symbol identifier
type SymbolValue string

// Type implements Value.Type
func (s SymbolValue) Type() Type {
	return SymbolType
}

// String implements Value.String
func (s SymbolValue) String() string {
	return string(s)
}

// NilValue represents a nil/null/None value
type NilValue struct{}

// Type implements Value.Type
func (n NilValue) Type() Type {
	return NilType
}

// String implements Value.String
func (n NilValue) String() string {
	return "nil"
}

// Predefined nil value
var (
	Nil = NilValue{}
)

// BuiltinFunction represents a Go function that can be called from M28
type BuiltinFunction struct {
	BaseObject
	fn func(args []Value, ctx *Context) (Value, error)
}

// NewBuiltinFunction creates a new builtin function
func NewBuiltinFunction(fn func(args []Value, ctx *Context) (Value, error)) *BuiltinFunction {
	return &BuiltinFunction{
		BaseObject: *NewBaseObject(FunctionType),
		fn:         fn,
	}
}

// Call implements Callable.Call
func (f *BuiltinFunction) Call(args []Value, ctx *Context) (Value, error) {
	return f.fn(args, ctx)
}

// String implements Value.String
func (f *BuiltinFunction) String() string {
	return "<builtin function>"
}

// BuiltinMethod represents a method that's implemented in Go
type BuiltinMethod struct {
	BaseObject
	fn       func(receiver Value, args []Value, ctx *Context) (Value, error)
	receiver Value
}

// Bind implements Method.Bind
func (m *BuiltinMethod) Bind(receiver Value) Value {
	return &BuiltinMethod{
		BaseObject: *NewBaseObject(MethodType),
		fn:         m.fn,
		receiver:   receiver,
	}
}

// Call implements Callable.Call
func (m *BuiltinMethod) Call(args []Value, ctx *Context) (Value, error) {
	return m.fn(m.receiver, args, ctx)
}

// String implements Value.String
func (m *BuiltinMethod) String() string {
	return fmt.Sprintf("<builtin method of %v>", m.receiver.Type().Name())
}