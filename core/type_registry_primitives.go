package core

import (
	"fmt"
	"math"
	"strconv"
	"strings"
)

// registerNumberType registers the number type descriptor
func registerNumberType() {
	RegisterType(&TypeDescriptor{
		Name:       "number",
		PythonName: "int",
		BaseType:   NumberType,
		Methods: map[string]*MethodDescriptor{
			"__add__": {
				Name:    "__add__",
				Arity:   1,
				Doc:     "Return self+value",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					if len(args) != 1 {
						return nil, fmt.Errorf("__add__ takes exactly one argument")
					}
					a := float64(receiver.(NumberValue))
					b, ok := args[0].(NumberValue)
					if !ok {
						// For cross-type operations, we should let the operator handle it
						// But we can't return NotImplemented, so we return a specific error
						return nil, fmt.Errorf("unsupported operand type(s) for +: 'number' and '%s'", args[0].Type())
					}
					return NumberValue(a + float64(b)), nil
				},
			},
			"abs": {
				Name:    "abs",
				Arity:   0,
				Doc:     "Return the absolute value of the number",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					n := float64(receiver.(NumberValue))
					return NumberValue(math.Abs(n)), nil
				},
			},
			"__neg__": {
				Name:    "__neg__",
				Arity:   0,
				Doc:     "Return -self",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					n := float64(receiver.(NumberValue))
					return NumberValue(-n), nil
				},
			},
			"__abs__": {
				Name:    "__abs__",
				Arity:   0,
				Doc:     "Return abs(self)",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					n := float64(receiver.(NumberValue))
					return NumberValue(math.Abs(n)), nil
				},
			},
		},
		Constructor: func(args []Value, ctx *Context) (Value, error) {
			if len(args) == 0 {
				return NumberValue(0), nil
			}
			if len(args) == 1 {
				switch v := args[0].(type) {
				case NumberValue:
					return v, nil
				case StringValue:
					// Try to parse as int first
					s := strings.TrimSpace(string(v))
					if n, ok := ParseInt(s); ok {
						return NumberValue(n), nil
					}
					// Try to parse as float
					if f, ok := ParseFloat(s); ok {
						return NumberValue(f), nil
					}
					return nil, fmt.Errorf("invalid literal for int() with base 10: '%s'", s)
				case BoolValue:
					if v {
						return NumberValue(1), nil
					}
					return NumberValue(0), nil
				default:
					return nil, fmt.Errorf("int() argument must be a string or a number, not '%s'", v.Type())
				}
			}
			return nil, fmt.Errorf("int() takes at most 1 argument (%d given)", len(args))
		},
		Str: func(v Value) string {
			n := v.(NumberValue)
			if math.Floor(float64(n)) == float64(n) {
				return formatInt(int64(n))
			}
			return formatFloat(float64(n))
		},
		Repr: func(v Value) string {
			n := v.(NumberValue)
			if math.Floor(float64(n)) == float64(n) {
				return formatInt(int64(n))
			}
			return formatFloat(float64(n))
		},
	})
}

// registerBoolType registers the bool type descriptor
func registerBoolType() {
	RegisterType(&TypeDescriptor{
		Name:       "bool",
		PythonName: "bool",
		BaseType:   BoolType,
		Methods:    map[string]*MethodDescriptor{},
		Constructor: func(args []Value, ctx *Context) (Value, error) {
			if len(args) == 0 {
				return False, nil
			}
			if len(args) == 1 {
				return BoolValue(IsTruthy(args[0])), nil
			}
			return nil, fmt.Errorf("bool() takes at most 1 argument (%d given)", len(args))
		},
		Str: func(v Value) string {
			if v.(BoolValue) {
				return "True"
			}
			return "False"
		},
		Repr: func(v Value) string {
			if v.(BoolValue) {
				return "True"
			}
			return "False"
		},
	})
}

// registerNilType registers the nil/None type descriptor
func registerNilType() {
	RegisterType(&TypeDescriptor{
		Name:       "nil",
		PythonName: "NoneType",
		BaseType:   NilType,
		Methods:    map[string]*MethodDescriptor{},
		Str: func(v Value) string {
			return "None"
		},
		Repr: func(v Value) string {
			return "None"
		},
	})
}

// registerSymbolType registers the symbol type descriptor
func registerSymbolType() {
	RegisterType(&TypeDescriptor{
		Name:       "symbol",
		PythonName: "symbol",
		BaseType:   SymbolType,
		Methods:    map[string]*MethodDescriptor{},
		Str: func(v Value) string {
			return string(v.(SymbolValue))
		},
		Repr: func(v Value) string {
			return string(v.(SymbolValue))
		},
	})
}

// Helper functions for number parsing and formatting

// ParseInt attempts to parse a string as an integer
func ParseInt(s string) (int64, bool) {
	i, err := strconv.ParseInt(s, 10, 64)
	return i, err == nil
}

// ParseFloat attempts to parse a string as a float
func ParseFloat(s string) (float64, bool) {
	f, err := strconv.ParseFloat(s, 64)
	return f, err == nil
}

// formatInt formats an integer as a string
func formatInt(n int64) string {
	return strconv.FormatInt(n, 10)
}

// formatFloat formats a float as a string
func formatFloat(f float64) string {
	return strconv.FormatFloat(f, 'f', -1, 64)
}
