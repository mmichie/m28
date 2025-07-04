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

// registerBytesType registers the bytes type descriptor
func registerBytesType() {
	RegisterType(&TypeDescriptor{
		Name:       "bytes",
		PythonName: "bytes",
		BaseType:   BytesType,
		Methods: map[string]*MethodDescriptor{
			"decode": {
				Name:    "decode",
				Arity:   -1, // Variable args
				Doc:     "Decode bytes to string using the specified encoding",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b := receiver.(BytesValue)
					encoding := "utf-8"
					if len(args) > 0 {
						enc, ok := args[0].(StringValue)
						if !ok {
							return nil, fmt.Errorf("decode() argument 1 must be str, not %s", args[0].Type())
						}
						encoding = string(enc)
					}
					if encoding != "utf-8" {
						return nil, fmt.Errorf("only utf-8 encoding is currently supported")
					}
					return StringValue(string(b)), nil
				},
			},
			"hex": {
				Name:    "hex",
				Arity:   0,
				Doc:     "Return hexadecimal representation of bytes",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b := receiver.(BytesValue)
					result := ""
					for _, byte := range b {
						result += fmt.Sprintf("%02x", byte)
					}
					return StringValue(result), nil
				},
			},
			"__len__": {
				Name:    "__len__",
				Arity:   0,
				Doc:     "Return length of bytes",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b := receiver.(BytesValue)
					return NumberValue(len(b)), nil
				},
			},
			"__contains__": {
				Name:    "__contains__",
				Arity:   1,
				Doc:     "Check if byte is in bytes",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					if len(args) != 1 {
						return nil, fmt.Errorf("__contains__ takes exactly one argument")
					}
					b := receiver.(BytesValue)
					num, ok := args[0].(NumberValue)
					if !ok {
						return nil, fmt.Errorf("argument should be integer")
					}
					intVal := int(num)
					if intVal < 0 || intVal > 255 {
						return nil, fmt.Errorf("byte must be in range(0, 256)")
					}
					for _, byte := range b {
						if byte == uint8(intVal) {
							return True, nil
						}
					}
					return False, nil
				},
			},
			"__eq__": {
				Name:    "__eq__",
				Arity:   1,
				Doc:     "Return self==value",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					if len(args) != 1 {
						return nil, fmt.Errorf("__eq__ takes exactly one argument")
					}
					b1 := receiver.(BytesValue)
					b2, ok := args[0].(BytesValue)
					if !ok {
						return False, nil
					}
					if len(b1) != len(b2) {
						return False, nil
					}
					for i := range b1 {
						if b1[i] != b2[i] {
							return False, nil
						}
					}
					return True, nil
				},
			},
		},
		Constructor: func(args []Value, ctx *Context) (Value, error) {
			if len(args) == 0 {
				return BytesValue([]byte{}), nil
			}
			if len(args) == 1 {
				switch v := args[0].(type) {
				case StringValue:
					return BytesValue([]byte(string(v))), nil
				case ListValue:
					// Convert list of numbers to bytes
					result := make([]byte, len(v))
					for i, val := range v {
						num, ok := val.(NumberValue)
						if !ok {
							return nil, fmt.Errorf("'%s' object cannot be interpreted as an integer", val.Type())
						}
						intVal := int(num)
						if intVal < 0 || intVal > 255 {
							return nil, fmt.Errorf("bytes must be in range(0, 256)")
						}
						result[i] = byte(intVal)
					}
					return BytesValue(result), nil
				case *ByteArrayValue:
					// Copy from bytearray
					result := make([]byte, len(v.data))
					copy(result, v.data)
					return BytesValue(result), nil
				case BytesValue:
					// Copy from bytes
					result := make([]byte, len(v))
					copy(result, v)
					return BytesValue(result), nil
				default:
					return nil, fmt.Errorf("cannot convert '%s' object to bytes", v.Type())
				}
			}
			// bytes(string, encoding)
			if len(args) == 2 {
				str, ok := args[0].(StringValue)
				if !ok {
					return nil, fmt.Errorf("bytes() argument 1 must be str, not %s", args[0].Type())
				}
				enc, ok := args[1].(StringValue)
				if !ok {
					return nil, fmt.Errorf("bytes() argument 2 must be str, not %s", args[1].Type())
				}
				if string(enc) != "utf-8" {
					return nil, fmt.Errorf("only utf-8 encoding is currently supported")
				}
				return BytesValue([]byte(string(str))), nil
			}
			return nil, fmt.Errorf("bytes() takes at most 2 arguments (%d given)", len(args))
		},
		Str: func(v Value) string {
			return v.(BytesValue).String()
		},
		Repr: func(v Value) string {
			return v.(BytesValue).String()
		},
	})
}

// registerByteArrayType registers the bytearray type descriptor
func registerByteArrayType() {
	RegisterType(&TypeDescriptor{
		Name:       "bytearray",
		PythonName: "bytearray",
		BaseType:   ByteArrayType,
		Methods: map[string]*MethodDescriptor{
			"decode": {
				Name:    "decode",
				Arity:   -1, // Variable args
				Doc:     "Decode bytearray to string using the specified encoding",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b := receiver.(*ByteArrayValue)
					encoding := "utf-8"
					if len(args) > 0 {
						enc, ok := args[0].(StringValue)
						if !ok {
							return nil, fmt.Errorf("decode() argument 1 must be str, not %s", args[0].Type())
						}
						encoding = string(enc)
					}
					if encoding != "utf-8" {
						return nil, fmt.Errorf("only utf-8 encoding is currently supported")
					}
					return StringValue(string(b.data)), nil
				},
			},
			"hex": {
				Name:    "hex",
				Arity:   0,
				Doc:     "Return hexadecimal representation of bytearray",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b := receiver.(*ByteArrayValue)
					result := ""
					for _, byte := range b.data {
						result += fmt.Sprintf("%02x", byte)
					}
					return StringValue(result), nil
				},
			},
			"append": {
				Name:    "append",
				Arity:   1,
				Doc:     "Append a byte to the end of the bytearray",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					if len(args) != 1 {
						return nil, fmt.Errorf("append() takes exactly one argument")
					}
					b := receiver.(*ByteArrayValue)
					num, ok := args[0].(NumberValue)
					if !ok {
						return nil, fmt.Errorf("an integer is required")
					}
					intVal := int(num)
					if intVal < 0 || intVal > 255 {
						return nil, fmt.Errorf("byte must be in range(0, 256)")
					}
					b.data = append(b.data, byte(intVal))
					return None, nil
				},
			},
			"extend": {
				Name:    "extend",
				Arity:   1,
				Doc:     "Extend bytearray by appending elements from the iterable",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					if len(args) != 1 {
						return nil, fmt.Errorf("extend() takes exactly one argument")
					}
					b := receiver.(*ByteArrayValue)

					switch v := args[0].(type) {
					case BytesValue:
						b.data = append(b.data, v...)
					case *ByteArrayValue:
						b.data = append(b.data, v.data...)
					case ListValue:
						for _, val := range v {
							num, ok := val.(NumberValue)
							if !ok {
								return nil, fmt.Errorf("'%s' object cannot be interpreted as an integer", val.Type())
							}
							intVal := int(num)
							if intVal < 0 || intVal > 255 {
								return nil, fmt.Errorf("byte must be in range(0, 256)")
							}
							b.data = append(b.data, byte(intVal))
						}
					default:
						return nil, fmt.Errorf("bytearray.extend() argument must be iterable")
					}
					return None, nil
				},
			},
			"clear": {
				Name:    "clear",
				Arity:   0,
				Doc:     "Remove all bytes from the bytearray",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b := receiver.(*ByteArrayValue)
					b.data = b.data[:0]
					return None, nil
				},
			},
			"__len__": {
				Name:    "__len__",
				Arity:   0,
				Doc:     "Return length of bytearray",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b := receiver.(*ByteArrayValue)
					return NumberValue(len(b.data)), nil
				},
			},
			"__contains__": {
				Name:    "__contains__",
				Arity:   1,
				Doc:     "Check if byte is in bytearray",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					if len(args) != 1 {
						return nil, fmt.Errorf("__contains__ takes exactly one argument")
					}
					b := receiver.(*ByteArrayValue)
					num, ok := args[0].(NumberValue)
					if !ok {
						return nil, fmt.Errorf("argument should be integer")
					}
					intVal := int(num)
					if intVal < 0 || intVal > 255 {
						return nil, fmt.Errorf("byte must be in range(0, 256)")
					}
					for _, byte := range b.data {
						if byte == uint8(intVal) {
							return True, nil
						}
					}
					return False, nil
				},
			},
			"__eq__": {
				Name:    "__eq__",
				Arity:   1,
				Doc:     "Return self==value",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					if len(args) != 1 {
						return nil, fmt.Errorf("__eq__ takes exactly one argument")
					}
					b1 := receiver.(*ByteArrayValue)
					// Check if comparing with bytes
					if b2, ok := args[0].(BytesValue); ok {
						if len(b1.data) != len(b2) {
							return False, nil
						}
						for i := range b1.data {
							if b1.data[i] != b2[i] {
								return False, nil
							}
						}
						return True, nil
					}
					// Check if comparing with another bytearray
					b2, ok := args[0].(*ByteArrayValue)
					if !ok {
						return False, nil
					}
					if len(b1.data) != len(b2.data) {
						return False, nil
					}
					for i := range b1.data {
						if b1.data[i] != b2.data[i] {
							return False, nil
						}
					}
					return True, nil
				},
			},
		},
		Constructor: func(args []Value, ctx *Context) (Value, error) {
			if len(args) == 0 {
				return NewByteArray([]byte{}), nil
			}
			if len(args) == 1 {
				switch v := args[0].(type) {
				case StringValue:
					return NewByteArray([]byte(string(v))), nil
				case ListValue:
					// Convert list of numbers to bytearray
					result := make([]byte, len(v))
					for i, val := range v {
						num, ok := val.(NumberValue)
						if !ok {
							return nil, fmt.Errorf("'%s' object cannot be interpreted as an integer", val.Type())
						}
						intVal := int(num)
						if intVal < 0 || intVal > 255 {
							return nil, fmt.Errorf("bytes must be in range(0, 256)")
						}
						result[i] = byte(intVal)
					}
					return NewByteArray(result), nil
				case BytesValue:
					// Copy from bytes
					result := make([]byte, len(v))
					copy(result, v)
					return NewByteArray(result), nil
				case *ByteArrayValue:
					// Copy from bytearray
					result := make([]byte, len(v.data))
					copy(result, v.data)
					return NewByteArray(result), nil
				default:
					return nil, fmt.Errorf("cannot convert '%s' object to bytearray", v.Type())
				}
			}
			// bytearray(string, encoding)
			if len(args) == 2 {
				str, ok := args[0].(StringValue)
				if !ok {
					return nil, fmt.Errorf("bytearray() argument 1 must be str, not %s", args[0].Type())
				}
				enc, ok := args[1].(StringValue)
				if !ok {
					return nil, fmt.Errorf("bytearray() argument 2 must be str, not %s", args[1].Type())
				}
				if string(enc) != "utf-8" {
					return nil, fmt.Errorf("only utf-8 encoding is currently supported")
				}
				return NewByteArray([]byte(string(str))), nil
			}
			return nil, fmt.Errorf("bytearray() takes at most 2 arguments (%d given)", len(args))
		},
		Str: func(v Value) string {
			return v.(*ByteArrayValue).String()
		},
		Repr: func(v Value) string {
			return v.(*ByteArrayValue).String()
		},
	})
}
