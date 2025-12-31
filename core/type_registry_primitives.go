package core

import (
	"fmt"
	"math"
	"strconv"
	"strings"

	"github.com/shopspring/decimal"
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
						return nil, &TypeError{Message: "__add__ takes exactly one argument"}
					}

					// Extract numeric value from receiver (handle both NumberValue and int subclass instances)
					var a float64
					if n, ok := receiver.(NumberValue); ok {
						a = float64(n)
					} else if inst, ok := receiver.(*Instance); ok {
						if val, exists := inst.Attributes["__value__"]; exists {
							if n, ok := val.(NumberValue); ok {
								a = float64(n)
							} else {
								return NotImplemented, nil
							}
						} else {
							return NotImplemented, nil
						}
					} else {
						return NotImplemented, nil
					}

					// Handle NumberValue + NumberValue
					if b, ok := args[0].(NumberValue); ok {
						return NumberValue(a + float64(b)), nil
					}

					// Handle addition with int subclass instances
					if inst, ok := args[0].(*Instance); ok {
						if val, exists := inst.Attributes["__value__"]; exists {
							if b, ok := val.(NumberValue); ok {
								return NumberValue(a + float64(b)), nil
							}
						}
					}

					// Handle NumberValue + BoolValue (Python: bools behave like ints)
					if b, ok := args[0].(BoolValue); ok {
						bNum := 0.0
						if bool(b) {
							bNum = 1.0
						}
						return NumberValue(a + bNum), nil
					}

					// Handle NumberValue + ComplexValue
					if c, ok := args[0].(ComplexValue); ok {
						return ComplexValue(complex(a, 0) + complex128(c)), nil
					}

					// For other cross-type operations, return NotImplemented
					// This allows the reflected operation (__radd__) to be tried
					return NotImplemented, nil
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
			"__index__": {
				Name:    "__index__",
				Arity:   0,
				Doc:     "Return self converted to an integer (for use in slicing/indexing)",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					// Handle NumberValue directly
					if n, ok := receiver.(NumberValue); ok {
						intVal := int(n)
						// Check if the number is actually an integer
						if float64(intVal) != float64(n) {
							return nil, &TypeError{Message: "__index__ returned non-integer value"}
						}
						return NumberValue(float64(n)), nil
					}

					// Handle int subclass instances that store __value__
					if inst, ok := receiver.(*Instance); ok {
						if val, exists := inst.Attributes["__value__"]; exists {
							if n, ok := val.(NumberValue); ok {
								intVal := int(n)
								// Check if the number is actually an integer
								if float64(intVal) != float64(n) {
									return nil, &TypeError{Message: "__index__ returned non-integer value"}
								}
								return NumberValue(float64(n)), nil
							}
						}
						return nil, &AttributeError{Message: "int subclass instance has no __value__ attribute"}
					}

					return nil, &TypeError{Message: fmt.Sprintf("__index__ called on non-int receiver: %s", receiver.Type())}
				},
			},
			"__int__": {
				Name:    "__int__",
				Arity:   0,
				Doc:     "Return int(self) - truncate to integer",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					// Handle NumberValue directly
					if n, ok := receiver.(NumberValue); ok {
						return NumberValue(float64(int(n))), nil
					}

					// Handle int subclass instances that store __value__
					if inst, ok := receiver.(*Instance); ok {
						if val, exists := inst.Attributes["__value__"]; exists {
							if n, ok := val.(NumberValue); ok {
								return NumberValue(float64(int(n))), nil
							}
						}
						return nil, &AttributeError{Message: "int subclass instance has no __value__ attribute"}
					}

					return nil, &TypeError{Message: fmt.Sprintf("__int__ called on non-int receiver: %s", receiver.Type())}
				},
			},
			"to_bytes": {
				Name:    "to_bytes",
				Arity:   2,
				Doc:     "Return an array of bytes representing an integer",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					if len(args) != 2 {
						return nil, &TypeError{Message: fmt.Sprintf("to_bytes() takes exactly 2 arguments (%d given)", len(args))}
					}

					// Get the integer value
					var num int64
					if n, ok := receiver.(NumberValue); ok {
						num = int64(n)
						// Check if it's actually an integer
						if float64(num) != float64(n) {
							return nil, &TypeError{Message: "'float' object cannot be interpreted as an integer"}
						}
					} else if inst, ok := receiver.(*Instance); ok {
						if val, exists := inst.Attributes["__value__"]; exists {
							if n, ok := val.(NumberValue); ok {
								num = int64(n)
								if float64(num) != float64(n) {
									return nil, &TypeError{Message: "'float' object cannot be interpreted as an integer"}
								}
							} else {
								return nil, &TypeError{Message: "to_bytes() requires an integer"}
							}
						} else {
							return nil, &AttributeError{Message: "int subclass instance has no __value__ attribute"}
						}
					} else {
						return nil, &TypeError{Message: "to_bytes() requires an integer"}
					}

					// Get length argument
					lengthVal, ok := args[0].(NumberValue)
					if !ok {
						return nil, &TypeError{Message: "to_bytes() length argument must be an integer"}
					}
					length := int(lengthVal)
					if float64(length) != float64(lengthVal) || length < 0 {
						return nil, &TypeError{Message: "to_bytes() length argument must be a non-negative integer"}
					}

					// Get byteorder argument
					byteorder, ok := args[1].(StringValue)
					if !ok {
						return nil, &TypeError{Message: "to_bytes() byteorder argument must be a string"}
					}
					order := string(byteorder)
					if order != "little" && order != "big" {
						return nil, &ValueError{Message: "byteorder must be either 'little' or 'big'"}
					}

					// Check if number is negative
					if num < 0 {
						return nil, &ValueError{Message: "can't convert negative int to unsigned"}
					}

					// Check if number fits in the specified length
					maxVal := int64(1) << uint(length*8)
					if num >= maxVal {
						return nil, &ValueError{Message: "int too big to convert"}
					}

					// Convert to bytes
					result := make([]byte, length)
					if order == "little" {
						// Little-endian: least significant byte first
						for i := 0; i < length; i++ {
							result[i] = byte(num >> uint(i*8))
						}
					} else {
						// Big-endian: most significant byte first
						for i := 0; i < length; i++ {
							result[length-1-i] = byte(num >> uint(i*8))
						}
					}

					return BytesValue(result), nil
				},
			},
			"__reduce_ex__": {
				Name:    "__reduce_ex__",
				Arity:   1,
				Doc:     "Helper for pickle",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					// For numbers, pickle uses __reduce_ex__ protocol
					// Return: (__newobj__, (int, value))

					// Get the numeric value
					numVal := receiver.(NumberValue)

					// Get the int type object (cached, same instance every time)
					intDesc := GetTypeDescriptor(NumberType)
					if intDesc == nil {
						return nil, &TypeError{Message: "cannot get int type descriptor"}
					}
					intClass := intDesc.GetTypeObject()
					if intClass == nil {
						return nil, &TypeError{Message: "cannot get int type object"}
					}

					// Get the global __newobj__ function from copyreg module
					// For now, create it inline
					newobjFunc := NewNamedBuiltinFunction("__newobj__", func(args []Value, ctx *Context) (Value, error) {
						if len(args) < 1 {
							return nil, &TypeError{Message: "__newobj__ requires at least 1 argument"}
						}
						cls := args[0]
						clsArgs := args[1:]

						clsObj, ok := cls.(interface{ GetAttr(string) (Value, bool) })
						if !ok {
							return nil, &TypeError{Message: "class does not support attribute access"}
						}

						newMethod, exists := clsObj.GetAttr("__new__")
						if !exists {
							return nil, &AttributeError{Message: "class has no __new__ method"}
						}

						newCallable, ok := newMethod.(Callable)
						if !ok {
							return nil, &TypeError{Message: "__new__ is not callable"}
						}

						newArgs := append([]Value{cls}, clsArgs...)
						return newCallable.Call(newArgs, ctx)
					})

					newobjFunc.SetAttr("__module__", StringValue("copyreg"))
					newobjFunc.SetAttr("__name__", StringValue("__newobj__"))
					newobjFunc.SetAttr("__qualname__", StringValue("__newobj__"))

					// Return tuple: (__newobj__, (int, value))
					argsTuple := TupleValue{intClass, numVal}
					result := TupleValue{newobjFunc, argsTuple}
					return result, nil
				},
			},
			"is_integer": {
				Name:    "is_integer",
				Arity:   0,
				Doc:     "Return True if the float is an integer (has no fractional part)",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					n := float64(receiver.(NumberValue))
					// Check for infinity or NaN
					if math.IsInf(n, 0) || math.IsNaN(n) {
						return False, nil
					}
					return BoolValue(n == math.Trunc(n)), nil
				},
			},
			"as_integer_ratio": {
				Name:    "as_integer_ratio",
				Arity:   0,
				Doc:     "Return a pair of integers whose ratio equals the float",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					n := float64(receiver.(NumberValue))
					// Check for infinity
					if math.IsInf(n, 0) {
						return nil, &ValueError{Message: "cannot convert Infinity to integer ratio"}
					}
					// Check for NaN
					if math.IsNaN(n) {
						return nil, &ValueError{Message: "cannot convert NaN to integer ratio"}
					}

					// Handle zero
					if n == 0 {
						return TupleValue{NumberValue(0), NumberValue(1)}, nil
					}

					// Use math.Frexp to get mantissa and exponent
					// n = mantissa * 2^exponent where 0.5 <= |mantissa| < 1
					mantissa, exponent := math.Frexp(n)

					// Scale mantissa to integer
					// float64 has 53 bits of mantissa precision
					const mantissaBits = 53
					mantissa *= float64(int64(1) << mantissaBits)
					exponent -= mantissaBits

					// Now mantissa is an integer and n = mantissa * 2^exponent
					numerator := int64(mantissa)
					denominator := int64(1)

					if exponent > 0 {
						numerator <<= uint(exponent)
					} else if exponent < 0 {
						denominator <<= uint(-exponent)
					}

					// Reduce the fraction by GCD
					g := gcd(abs64(numerator), denominator)
					numerator /= g
					denominator /= g

					return TupleValue{NumberValue(numerator), NumberValue(denominator)}, nil
				},
			},
			"hex": {
				Name:    "hex",
				Arity:   0,
				Doc:     "Return a hexadecimal representation of a float",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					n := float64(receiver.(NumberValue))

					// Handle special cases
					if math.IsNaN(n) {
						return StringValue("nan"), nil
					}
					if math.IsInf(n, 1) {
						return StringValue("inf"), nil
					}
					if math.IsInf(n, -1) {
						return StringValue("-inf"), nil
					}
					if n == 0 {
						if math.Signbit(n) {
							return StringValue("-0x0.0p+0"), nil
						}
						return StringValue("0x0.0p+0"), nil
					}

					// Use Go's hex format and convert to Python format
					// Go: %x gives e.g. "0x1.921fb54442d18p+01"
					// Python: "0x1.921fb54442d18p+1" (no leading zero on exponent)
					result := strconv.FormatFloat(n, 'x', -1, 64)
					return StringValue(result), nil
				},
			},
			"__float__": {
				Name:    "__float__",
				Arity:   0,
				Doc:     "Return float(self)",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					return receiver, nil
				},
			},
			"conjugate": {
				Name:    "conjugate",
				Arity:   0,
				Doc:     "Return self (floats are their own conjugate)",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					return receiver, nil
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
					return nil, &ValueError{Message: fmt.Sprintf("invalid literal for int() with base 10: '%s'", s)}
				case BoolValue:
					if v {
						return NumberValue(1), nil
					}
					return NumberValue(0), nil
				default:
					return nil, &TypeError{Message: fmt.Sprintf("int() argument must be a string or a number, not '%s'", v.Type())}
				}
			}
			return nil, &TypeError{Message: fmt.Sprintf("int() takes at most 1 argument (%d given)", len(args))}
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
		Properties: map[string]*PropertyDescriptor{
			"real": {
				Name:     "real",
				ReadOnly: true,
				Doc:      "the real part of a complex number (for bool, the integer value)",
				Getter: func(v Value) (Value, error) {
					if bool(v.(BoolValue)) {
						return NewBigIntFromInt64(1), nil
					}
					return NewBigIntFromInt64(0), nil
				},
			},
			"imag": {
				Name:     "imag",
				ReadOnly: true,
				Doc:      "the imaginary part of a complex number (always 0 for bool)",
				Getter: func(v Value) (Value, error) {
					return NewBigIntFromInt64(0), nil
				},
			},
		},
		Methods: map[string]*MethodDescriptor{
			// Comparison operators - bools compare as ints (False=0, True=1)
			"__lt__": {
				Name:    "__lt__",
				Arity:   1,
				Doc:     "Return self<value",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					a := 0
					if bool(receiver.(BoolValue)) {
						a = 1
					}
					// Handle bool < bool or bool < number
					var b int
					switch v := args[0].(type) {
					case BoolValue:
						if bool(v) {
							b = 1
						}
					case NumberValue:
						b = int(v)
					default:
						return nil, &TypeError{Message: fmt.Sprintf("'<' not supported between instances of 'bool' and '%s'", args[0].Type())}
					}
					return BoolValue(a < b), nil
				},
			},
			"__le__": {
				Name:    "__le__",
				Arity:   1,
				Doc:     "Return self<=value",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					a := 0
					if bool(receiver.(BoolValue)) {
						a = 1
					}
					var b int
					switch v := args[0].(type) {
					case BoolValue:
						if bool(v) {
							b = 1
						}
					case NumberValue:
						b = int(v)
					default:
						return nil, &TypeError{Message: fmt.Sprintf("'<=' not supported between instances of 'bool' and '%s'", args[0].Type())}
					}
					return BoolValue(a <= b), nil
				},
			},
			"__gt__": {
				Name:    "__gt__",
				Arity:   1,
				Doc:     "Return self>value",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					a := 0
					if bool(receiver.(BoolValue)) {
						a = 1
					}
					var b int
					switch v := args[0].(type) {
					case BoolValue:
						if bool(v) {
							b = 1
						}
					case NumberValue:
						b = int(v)
					default:
						return nil, &TypeError{Message: fmt.Sprintf("'>' not supported between instances of 'bool' and '%s'", args[0].Type())}
					}
					return BoolValue(a > b), nil
				},
			},
			"__ge__": {
				Name:    "__ge__",
				Arity:   1,
				Doc:     "Return self>=value",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					a := 0
					if bool(receiver.(BoolValue)) {
						a = 1
					}
					var b int
					switch v := args[0].(type) {
					case BoolValue:
						if bool(v) {
							b = 1
						}
					case NumberValue:
						b = int(v)
					default:
						return nil, &TypeError{Message: fmt.Sprintf("'>=' not supported between instances of 'bool' and '%s'", args[0].Type())}
					}
					return BoolValue(a >= b), nil
				},
			},
			"__reduce_ex__": {
				Name:    "__reduce_ex__",
				Arity:   1,
				Doc:     "Helper for pickle",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					// Protocol number (we ignore it for now)
					// Return (__newobj__, (bool, int_value))
					// where __newobj__(cls, *args) calls cls.__new__(cls, *args)

					// Get the bool type object (cached, same instance every time)
					boolDesc := GetTypeDescriptor(BoolType)
					if boolDesc == nil {
						return nil, &TypeError{Message: "cannot get bool type descriptor"}
					}
					boolClass := boolDesc.GetTypeObject()
					if boolClass == nil {
						return nil, &TypeError{Message: "cannot get bool type object"}
					}

					// Convert bool to int (False=0, True=1)
					var intVal NumberValue
					if bool(receiver.(BoolValue)) {
						intVal = NumberValue(1)
					} else {
						intVal = NumberValue(0)
					}

					// Create __newobj__ function
					newobjFunc := NewNamedBuiltinFunction("__newobj__", func(args []Value, ctx *Context) (Value, error) {
						if len(args) < 1 {
							return nil, &TypeError{Message: "__newobj__ requires at least 1 argument"}
						}
						cls := args[0]
						clsArgs := args[1:]

						clsObj, ok := cls.(interface{ GetAttr(string) (Value, bool) })
						if !ok {
							return nil, &TypeError{Message: "class does not support attribute access"}
						}

						newMethod, exists := clsObj.GetAttr("__new__")
						if !exists {
							return nil, &AttributeError{Message: "class has no __new__ method"}
						}

						newCallable, ok := newMethod.(Callable)
						if !ok {
							return nil, &TypeError{Message: "__new__ is not callable"}
						}

						newArgs := append([]Value{cls}, clsArgs...)
						return newCallable.Call(newArgs, ctx)
					})

					newobjFunc.SetAttr("__module__", StringValue("copyreg"))
					newobjFunc.SetAttr("__name__", StringValue("__newobj__"))
					newobjFunc.SetAttr("__qualname__", StringValue("__newobj__"))

					// Return tuple: (newobj_func, (bool_class, int_value))
					argsTuple := TupleValue{boolClass, intVal}
					result := TupleValue{newobjFunc, argsTuple}
					return result, nil
				},
			},
		},
		Constructor: func(args []Value, ctx *Context) (Value, error) {
			if len(args) == 0 {
				return False, nil
			}
			if len(args) == 1 {
				return BoolValue(IsTruthy(args[0])), nil
			}
			return nil, &TypeError{Message: fmt.Sprintf("bool() takes at most 1 argument (%d given)", len(args))}
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

// gcd returns the greatest common divisor of a and b
func gcd(a, b int64) int64 {
	for b != 0 {
		a, b = b, a%b
	}
	return a
}

// abs64 returns the absolute value of an int64
func abs64(n int64) int64 {
	if n < 0 {
		return -n
	}
	return n
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
							return nil, &TypeError{Message: fmt.Sprintf("decode() argument 1 must be str, not %s", args[0].Type())}
						}
						encoding = string(enc)
					}
					if encoding != "utf-8" {
						return nil, &ValueError{Message: "only utf-8 encoding is currently supported"}
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
						return nil, &TypeError{Message: "__contains__ takes exactly one argument"}
					}
					b := receiver.(BytesValue)
					num, ok := args[0].(NumberValue)
					if !ok {
						return nil, &TypeError{Message: "argument should be integer"}
					}
					intVal := int(num)
					if intVal < 0 || intVal > 255 {
						return nil, &ValueError{Message: "byte must be in range(0, 256)"}
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
						return nil, &TypeError{Message: "__eq__ takes exactly one argument"}
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
			"upper": {
				Name:    "upper",
				Arity:   0,
				Doc:     "Return a copy with all ASCII characters converted to uppercase",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b := receiver.(BytesValue)
					result := make([]byte, len(b))
					for i, c := range b {
						if c >= 'a' && c <= 'z' {
							result[i] = c - 32
						} else {
							result[i] = c
						}
					}
					return BytesValue(result), nil
				},
			},
			"lower": {
				Name:    "lower",
				Arity:   0,
				Doc:     "Return a copy with all ASCII characters converted to lowercase",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b := receiver.(BytesValue)
					result := make([]byte, len(b))
					for i, c := range b {
						if c >= 'A' && c <= 'Z' {
							result[i] = c + 32
						} else {
							result[i] = c
						}
					}
					return BytesValue(result), nil
				},
			},
			"strip": {
				Name:    "strip",
				Arity:   -1,
				Doc:     "Return a copy with leading and trailing whitespace removed",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b := receiver.(BytesValue)
					chars := []byte(" \t\n\r\f\v")
					if len(args) > 0 {
						if stripBytes, ok := args[0].(BytesValue); ok {
							chars = []byte(stripBytes)
						}
					}
					start, end := 0, len(b)
					for start < end && containsByte(chars, b[start]) {
						start++
					}
					for end > start && containsByte(chars, b[end-1]) {
						end--
					}
					return BytesValue(b[start:end]), nil
				},
			},
			"lstrip": {
				Name:    "lstrip",
				Arity:   -1,
				Doc:     "Return a copy with leading whitespace removed",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b := receiver.(BytesValue)
					chars := []byte(" \t\n\r\f\v")
					if len(args) > 0 {
						if stripBytes, ok := args[0].(BytesValue); ok {
							chars = []byte(stripBytes)
						}
					}
					start := 0
					for start < len(b) && containsByte(chars, b[start]) {
						start++
					}
					return BytesValue(b[start:]), nil
				},
			},
			"rstrip": {
				Name:    "rstrip",
				Arity:   -1,
				Doc:     "Return a copy with trailing whitespace removed",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b := receiver.(BytesValue)
					chars := []byte(" \t\n\r\f\v")
					if len(args) > 0 {
						if stripBytes, ok := args[0].(BytesValue); ok {
							chars = []byte(stripBytes)
						}
					}
					end := len(b)
					for end > 0 && containsByte(chars, b[end-1]) {
						end--
					}
					return BytesValue(b[:end]), nil
				},
			},
			"split": {
				Name:    "split",
				Arity:   -1,
				Doc:     "Return a list of bytes split by separator",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b := receiver.(BytesValue)
					var sep []byte
					if len(args) > 0 && args[0] != Nil {
						if sepBytes, ok := args[0].(BytesValue); ok {
							sep = []byte(sepBytes)
						} else {
							return nil, &TypeError{Message: "a bytes-like object is required"}
						}
					}
					var parts [][]byte
					if sep == nil {
						parts = splitBytesWhitespace([]byte(b))
					} else {
						parts = splitBytes([]byte(b), sep)
					}
					list := NewList()
					for _, part := range parts {
						list.Append(BytesValue(part))
					}
					return list, nil
				},
			},
			"join": {
				Name:    "join",
				Arity:   1,
				Doc:     "Concatenate bytes with separator",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					sep := receiver.(BytesValue)
					if len(args) != 1 {
						return nil, &TypeError{Message: "join() takes exactly one argument"}
					}
					list, ok := args[0].(*ListValue)
					if !ok {
						return nil, &TypeError{Message: "can only join an iterable"}
					}
					var result []byte
					for i, item := range list.Items() {
						itemBytes, ok := item.(BytesValue)
						if !ok {
							return nil, &TypeError{Message: "sequence item: expected bytes"}
						}
						if i > 0 {
							result = append(result, sep...)
						}
						result = append(result, itemBytes...)
					}
					return BytesValue(result), nil
				},
			},
			"replace": {
				Name:    "replace",
				Arity:   -1,
				Doc:     "Return a copy with all occurrences of old replaced by new",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b := receiver.(BytesValue)
					if len(args) < 2 {
						return nil, &TypeError{Message: "replace() takes at least 2 arguments"}
					}
					old, ok := args[0].(BytesValue)
					if !ok {
						return nil, &TypeError{Message: "a bytes-like object is required"}
					}
					newBytes, ok := args[1].(BytesValue)
					if !ok {
						return nil, &TypeError{Message: "a bytes-like object is required"}
					}
					count := -1
					if len(args) > 2 {
						if c, ok := args[2].(NumberValue); ok {
							count = int(c)
						}
					}
					result := replaceBytes([]byte(b), []byte(old), []byte(newBytes), count)
					return BytesValue(result), nil
				},
			},
			"find": {
				Name:    "find",
				Arity:   -1,
				Doc:     "Return the lowest index where substring is found",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b := receiver.(BytesValue)
					if len(args) < 1 {
						return nil, &TypeError{Message: "find() takes at least 1 argument"}
					}
					sub, ok := args[0].(BytesValue)
					if !ok {
						return nil, &TypeError{Message: "a bytes-like object is required"}
					}
					idx := findBytes([]byte(b), []byte(sub))
					return NumberValue(idx), nil
				},
			},
			"index": {
				Name:    "index",
				Arity:   -1,
				Doc:     "Return the lowest index where substring is found, raises ValueError if not found",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b := receiver.(BytesValue)
					if len(args) < 1 {
						return nil, &TypeError{Message: "index() takes at least 1 argument"}
					}
					sub, ok := args[0].(BytesValue)
					if !ok {
						return nil, &TypeError{Message: "a bytes-like object is required"}
					}
					idx := findBytes([]byte(b), []byte(sub))
					if idx == -1 {
						return nil, &ValueError{Message: "subsection not found"}
					}
					return NumberValue(idx), nil
				},
			},
			"count": {
				Name:    "count",
				Arity:   1,
				Doc:     "Return the number of non-overlapping occurrences of substring",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b := receiver.(BytesValue)
					sub, ok := args[0].(BytesValue)
					if !ok {
						return nil, &TypeError{Message: "a bytes-like object is required"}
					}
					count := countBytes([]byte(b), []byte(sub))
					return NumberValue(count), nil
				},
			},
			"startswith": {
				Name:    "startswith",
				Arity:   1,
				Doc:     "Return True if bytes starts with the specified prefix",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b := receiver.(BytesValue)
					prefix, ok := args[0].(BytesValue)
					if !ok {
						return nil, &TypeError{Message: "a bytes-like object is required"}
					}
					if len(prefix) > len(b) {
						return False, nil
					}
					for i := range prefix {
						if b[i] != prefix[i] {
							return False, nil
						}
					}
					return True, nil
				},
			},
			"endswith": {
				Name:    "endswith",
				Arity:   1,
				Doc:     "Return True if bytes ends with the specified suffix",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b := receiver.(BytesValue)
					suffix, ok := args[0].(BytesValue)
					if !ok {
						return nil, &TypeError{Message: "a bytes-like object is required"}
					}
					if len(suffix) > len(b) {
						return False, nil
					}
					offset := len(b) - len(suffix)
					for i := range suffix {
						if b[offset+i] != suffix[i] {
							return False, nil
						}
					}
					return True, nil
				},
			},
			"isupper": {
				Name:    "isupper",
				Arity:   0,
				Doc:     "Return True if all cased characters are uppercase",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b := receiver.(BytesValue)
					hasCased := false
					for _, c := range b {
						if c >= 'a' && c <= 'z' {
							return False, nil
						}
						if c >= 'A' && c <= 'Z' {
							hasCased = true
						}
					}
					return BoolValue(hasCased), nil
				},
			},
			"islower": {
				Name:    "islower",
				Arity:   0,
				Doc:     "Return True if all cased characters are lowercase",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b := receiver.(BytesValue)
					hasCased := false
					for _, c := range b {
						if c >= 'A' && c <= 'Z' {
							return False, nil
						}
						if c >= 'a' && c <= 'z' {
							hasCased = true
						}
					}
					return BoolValue(hasCased), nil
				},
			},
			"isdigit": {
				Name:    "isdigit",
				Arity:   0,
				Doc:     "Return True if all characters are digits",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b := receiver.(BytesValue)
					if len(b) == 0 {
						return False, nil
					}
					for _, c := range b {
						if c < '0' || c > '9' {
							return False, nil
						}
					}
					return True, nil
				},
			},
			"isalpha": {
				Name:    "isalpha",
				Arity:   0,
				Doc:     "Return True if all characters are alphabetic",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b := receiver.(BytesValue)
					if len(b) == 0 {
						return False, nil
					}
					for _, c := range b {
						if !((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')) {
							return False, nil
						}
					}
					return True, nil
				},
			},
			"isalnum": {
				Name:    "isalnum",
				Arity:   0,
				Doc:     "Return True if all characters are alphanumeric",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b := receiver.(BytesValue)
					if len(b) == 0 {
						return False, nil
					}
					for _, c := range b {
						if !((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')) {
							return False, nil
						}
					}
					return True, nil
				},
			},
			"isspace": {
				Name:    "isspace",
				Arity:   0,
				Doc:     "Return True if all characters are whitespace",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b := receiver.(BytesValue)
					if len(b) == 0 {
						return False, nil
					}
					for _, c := range b {
						if c != ' ' && c != '\t' && c != '\n' && c != '\r' && c != '\f' && c != '\v' {
							return False, nil
						}
					}
					return True, nil
				},
			},
			"title": {
				Name:    "title",
				Arity:   0,
				Doc:     "Return a titlecased version",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b := receiver.(BytesValue)
					result := make([]byte, len(b))
					capitalizeNext := true
					for i, c := range b {
						if c >= 'a' && c <= 'z' {
							if capitalizeNext {
								result[i] = c - 32
							} else {
								result[i] = c
							}
							capitalizeNext = false
						} else if c >= 'A' && c <= 'Z' {
							if capitalizeNext {
								result[i] = c
							} else {
								result[i] = c + 32
							}
							capitalizeNext = false
						} else {
							result[i] = c
							capitalizeNext = true
						}
					}
					return BytesValue(result), nil
				},
			},
			"capitalize": {
				Name:    "capitalize",
				Arity:   0,
				Doc:     "Return a copy with first character capitalized",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b := receiver.(BytesValue)
					if len(b) == 0 {
						return BytesValue([]byte{}), nil
					}
					result := make([]byte, len(b))
					for i, c := range b {
						if i == 0 && c >= 'a' && c <= 'z' {
							result[i] = c - 32
						} else if i > 0 && c >= 'A' && c <= 'Z' {
							result[i] = c + 32
						} else {
							result[i] = c
						}
					}
					return BytesValue(result), nil
				},
			},
			"swapcase": {
				Name:    "swapcase",
				Arity:   0,
				Doc:     "Return a copy with uppercase characters converted to lowercase and vice versa",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b := receiver.(BytesValue)
					result := make([]byte, len(b))
					for i, c := range b {
						if c >= 'a' && c <= 'z' {
							result[i] = c - 32
						} else if c >= 'A' && c <= 'Z' {
							result[i] = c + 32
						} else {
							result[i] = c
						}
					}
					return BytesValue(result), nil
				},
			},
			"center": {
				Name:    "center",
				Arity:   -1,
				Doc:     "Return centered in a bytes of length width",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b := receiver.(BytesValue)
					if len(args) < 1 {
						return nil, &TypeError{Message: "center() takes at least 1 argument"}
					}
					width, ok := args[0].(NumberValue)
					if !ok {
						return nil, &TypeError{Message: "width must be an integer"}
					}
					w := int(width)
					if w <= len(b) {
						return b, nil
					}
					fillchar := byte(' ')
					if len(args) > 1 {
						if fb, ok := args[1].(BytesValue); ok && len(fb) == 1 {
							fillchar = fb[0]
						}
					}
					leftPad := (w - len(b)) / 2
					rightPad := w - len(b) - leftPad
					result := make([]byte, w)
					for i := 0; i < leftPad; i++ {
						result[i] = fillchar
					}
					copy(result[leftPad:], b)
					for i := 0; i < rightPad; i++ {
						result[leftPad+len(b)+i] = fillchar
					}
					return BytesValue(result), nil
				},
			},
			"ljust": {
				Name:    "ljust",
				Arity:   -1,
				Doc:     "Return left-justified in a bytes of length width",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b := receiver.(BytesValue)
					if len(args) < 1 {
						return nil, &TypeError{Message: "ljust() takes at least 1 argument"}
					}
					width, ok := args[0].(NumberValue)
					if !ok {
						return nil, &TypeError{Message: "width must be an integer"}
					}
					w := int(width)
					if w <= len(b) {
						return b, nil
					}
					fillchar := byte(' ')
					if len(args) > 1 {
						if fb, ok := args[1].(BytesValue); ok && len(fb) == 1 {
							fillchar = fb[0]
						}
					}
					result := make([]byte, w)
					copy(result, b)
					for i := len(b); i < w; i++ {
						result[i] = fillchar
					}
					return BytesValue(result), nil
				},
			},
			"rjust": {
				Name:    "rjust",
				Arity:   -1,
				Doc:     "Return right-justified in a bytes of length width",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b := receiver.(BytesValue)
					if len(args) < 1 {
						return nil, &TypeError{Message: "rjust() takes at least 1 argument"}
					}
					width, ok := args[0].(NumberValue)
					if !ok {
						return nil, &TypeError{Message: "width must be an integer"}
					}
					w := int(width)
					if w <= len(b) {
						return b, nil
					}
					fillchar := byte(' ')
					if len(args) > 1 {
						if fb, ok := args[1].(BytesValue); ok && len(fb) == 1 {
							fillchar = fb[0]
						}
					}
					result := make([]byte, w)
					pad := w - len(b)
					for i := 0; i < pad; i++ {
						result[i] = fillchar
					}
					copy(result[pad:], b)
					return BytesValue(result), nil
				},
			},
			"zfill": {
				Name:    "zfill",
				Arity:   1,
				Doc:     "Pad a bytes with ASCII '0' digits to fill a field of the given width",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b := receiver.(BytesValue)
					width, ok := args[0].(NumberValue)
					if !ok {
						return nil, &TypeError{Message: "width must be an integer"}
					}
					w := int(width)
					if w <= len(b) {
						return b, nil
					}
					result := make([]byte, w)
					pad := w - len(b)
					for i := 0; i < pad; i++ {
						result[i] = '0'
					}
					copy(result[pad:], b)
					return BytesValue(result), nil
				},
			},
			"istitle": {
				Name:    "istitle",
				Arity:   0,
				Doc:     "Return True if the bytes is a titlecased string",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b := receiver.(BytesValue)
					if len(b) == 0 {
						return False, nil
					}
					hasCased := false
					prevCased := false
					for _, c := range b {
						isUpper := c >= 'A' && c <= 'Z'
						isLower := c >= 'a' && c <= 'z'
						if isUpper {
							if prevCased {
								return False, nil
							}
							hasCased = true
							prevCased = true
						} else if isLower {
							if !prevCased {
								return False, nil
							}
							hasCased = true
							prevCased = true
						} else {
							prevCased = false
						}
					}
					return BoolValue(hasCased), nil
				},
			},
			"__lt__": {
				Name:    "__lt__",
				Arity:   1,
				Doc:     "Return self<value",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b1 := receiver.(BytesValue)
					b2, ok := args[0].(BytesValue)
					if !ok {
						return nil, &TypeError{Message: fmt.Sprintf("'<' not supported between instances of 'bytes' and '%s'", args[0].Type())}
					}
					minLen := len(b1)
					if len(b2) < minLen {
						minLen = len(b2)
					}
					for i := 0; i < minLen; i++ {
						if b1[i] < b2[i] {
							return True, nil
						}
						if b1[i] > b2[i] {
							return False, nil
						}
					}
					return BoolValue(len(b1) < len(b2)), nil
				},
			},
			"__le__": {
				Name:    "__le__",
				Arity:   1,
				Doc:     "Return self<=value",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b1 := receiver.(BytesValue)
					b2, ok := args[0].(BytesValue)
					if !ok {
						return nil, &TypeError{Message: fmt.Sprintf("'<=' not supported between instances of 'bytes' and '%s'", args[0].Type())}
					}
					minLen := len(b1)
					if len(b2) < minLen {
						minLen = len(b2)
					}
					for i := 0; i < minLen; i++ {
						if b1[i] < b2[i] {
							return True, nil
						}
						if b1[i] > b2[i] {
							return False, nil
						}
					}
					return BoolValue(len(b1) <= len(b2)), nil
				},
			},
			"__gt__": {
				Name:    "__gt__",
				Arity:   1,
				Doc:     "Return self>value",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b1 := receiver.(BytesValue)
					b2, ok := args[0].(BytesValue)
					if !ok {
						return nil, &TypeError{Message: fmt.Sprintf("'>' not supported between instances of 'bytes' and '%s'", args[0].Type())}
					}
					minLen := len(b1)
					if len(b2) < minLen {
						minLen = len(b2)
					}
					for i := 0; i < minLen; i++ {
						if b1[i] > b2[i] {
							return True, nil
						}
						if b1[i] < b2[i] {
							return False, nil
						}
					}
					return BoolValue(len(b1) > len(b2)), nil
				},
			},
			"__ge__": {
				Name:    "__ge__",
				Arity:   1,
				Doc:     "Return self>=value",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b1 := receiver.(BytesValue)
					b2, ok := args[0].(BytesValue)
					if !ok {
						return nil, &TypeError{Message: fmt.Sprintf("'>=' not supported between instances of 'bytes' and '%s'", args[0].Type())}
					}
					minLen := len(b1)
					if len(b2) < minLen {
						minLen = len(b2)
					}
					for i := 0; i < minLen; i++ {
						if b1[i] > b2[i] {
							return True, nil
						}
						if b1[i] < b2[i] {
							return False, nil
						}
					}
					return BoolValue(len(b1) >= len(b2)), nil
				},
			},
			"__rmul__": {
				Name:    "__rmul__",
				Arity:   1,
				Doc:     "Return value*self (reversed operand for multiplication)",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b := receiver.(BytesValue)
					n, ok := args[0].(NumberValue)
					if !ok {
						return NotImplemented, nil
					}
					count := int(n)
					if count <= 0 {
						return BytesValue([]byte{}), nil
					}
					result := make([]byte, 0, len(b)*count)
					for i := 0; i < count; i++ {
						result = append(result, b...)
					}
					return BytesValue(result), nil
				},
			},
		},
		Constructor: func(args []Value, ctx *Context) (Value, error) {
			if len(args) == 0 {
				return BytesValue([]byte{}), nil
			}
			if len(args) == 1 {
				val := args[0]

				// Try __bytes__ dunder method first
				if obj, ok := val.(Object); ok {
					if method, exists := obj.GetAttr("__bytes__"); exists {
						if callable, ok := method.(Callable); ok {
							result, err := callable.Call([]Value{}, ctx)
							if err != nil {
								return nil, err
							}
							// Ensure it returns bytes
							if bytes, ok := result.(BytesValue); ok {
								return bytes, nil
							}
							return nil, &TypeError{Message: fmt.Sprintf("__bytes__ returned non-bytes (type %s)", result.Type())}
						}
					}
				}

				switch v := val.(type) {
				case NumberValue:
					// bytes(int) creates zero-filled bytes of that length
					length := int(v)
					if length < 0 {
						return nil, &ValueError{Message: "negative count"}
					}
					return BytesValue(make([]byte, length)), nil
				case StringValue:
					return BytesValue([]byte(string(v))), nil
				case *ListValue:
					// Convert list of numbers to bytes
					result := make([]byte, v.Len())
					for i, val := range v.Items() {
						num, ok := val.(NumberValue)
						if !ok {
							return nil, &TypeError{Message: fmt.Sprintf("'%s' object cannot be interpreted as an integer", val.Type())}
						}
						intVal := int(num)
						if intVal < 0 || intVal > 255 {
							return nil, &ValueError{Message: "bytes must be in range(0, 256)"}
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
				case *SetValue:
					// Convert set to bytes - iterate and collect byte values
					result := make([]byte, 0, v.Size())
					for _, val := range v.items {
						num, ok := val.(NumberValue)
						if !ok {
							return nil, &TypeError{Message: fmt.Sprintf("'%s' object cannot be interpreted as an integer", val.Type())}
						}
						intVal := int(num)
						if intVal < 0 || intVal > 255 {
							return nil, &ValueError{Message: "bytes must be in range(0, 256)"}
						}
						result = append(result, byte(intVal))
					}
					return BytesValue(result), nil
				case *FrozenSetValue:
					// Convert frozenset to bytes - iterate and collect byte values
					result := make([]byte, 0, v.Size())
					for _, val := range v.items {
						num, ok := val.(NumberValue)
						if !ok {
							return nil, &TypeError{Message: fmt.Sprintf("'%s' object cannot be interpreted as an integer", val.Type())}
						}
						intVal := int(num)
						if intVal < 0 || intVal > 255 {
							return nil, &ValueError{Message: "bytes must be in range(0, 256)"}
						}
						result = append(result, byte(intVal))
					}
					return BytesValue(result), nil
				default:
					return nil, &TypeError{Message: fmt.Sprintf("cannot convert '%s' object to bytes", v.Type())}
				}
			}
			// bytes(string, encoding)
			if len(args) == 2 {
				str, ok := args[0].(StringValue)
				if !ok {
					return nil, &TypeError{Message: fmt.Sprintf("bytes() argument 1 must be str, not %s", args[0].Type())}
				}
				enc, ok := args[1].(StringValue)
				if !ok {
					return nil, &TypeError{Message: fmt.Sprintf("bytes() argument 2 must be str, not %s", args[1].Type())}
				}
				if string(enc) != "utf-8" {
					return nil, &ValueError{Message: "only utf-8 encoding is currently supported"}
				}
				return BytesValue([]byte(string(str))), nil
			}
			return nil, &TypeError{Message: fmt.Sprintf("bytes() takes at most 2 arguments (%d given)", len(args))}
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
			"__iter__": {
				Name:    "__iter__",
				Arity:   0,
				Doc:     "Return an iterator object",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					b := receiver.(*ByteArrayValue)
					// Use IteratorValue() which returns the iterator as a Value
					return b.IteratorValue(), nil
				},
			},
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
							return nil, &TypeError{Message: fmt.Sprintf("decode() argument 1 must be str, not %s", args[0].Type())}
						}
						encoding = string(enc)
					}
					if encoding != "utf-8" {
						return nil, &ValueError{Message: "only utf-8 encoding is currently supported"}
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
						return nil, &TypeError{Message: "append() takes exactly one argument"}
					}
					b := receiver.(*ByteArrayValue)
					num, ok := args[0].(NumberValue)
					if !ok {
						return nil, &TypeError{Message: "an integer is required"}
					}
					intVal := int(num)
					if intVal < 0 || intVal > 255 {
						return nil, &ValueError{Message: "byte must be in range(0, 256)"}
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
						return nil, &TypeError{Message: "extend() takes exactly one argument"}
					}
					b := receiver.(*ByteArrayValue)

					switch v := args[0].(type) {
					case BytesValue:
						b.data = append(b.data, v...)
					case *ByteArrayValue:
						b.data = append(b.data, v.data...)
					case *ListValue:
						for _, val := range v.Items() {
							num, ok := val.(NumberValue)
							if !ok {
								return nil, &TypeError{Message: fmt.Sprintf("'%s' object cannot be interpreted as an integer", val.Type())}
							}
							intVal := int(num)
							if intVal < 0 || intVal > 255 {
								return nil, &ValueError{Message: "byte must be in range(0, 256)"}
							}
							b.data = append(b.data, byte(intVal))
						}
					default:
						return nil, &TypeError{Message: "bytearray.extend() argument must be iterable"}
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
						return nil, &TypeError{Message: "__contains__ takes exactly one argument"}
					}
					b := receiver.(*ByteArrayValue)
					num, ok := args[0].(NumberValue)
					if !ok {
						return nil, &TypeError{Message: "argument should be integer"}
					}
					intVal := int(num)
					if intVal < 0 || intVal > 255 {
						return nil, &ValueError{Message: "byte must be in range(0, 256)"}
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
						return nil, &TypeError{Message: "__eq__ takes exactly one argument"}
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
				case *ListValue:
					// Convert list of numbers to bytearray
					result := make([]byte, v.Len())
					for i, val := range v.Items() {
						num, ok := val.(NumberValue)
						if !ok {
							return nil, &TypeError{Message: fmt.Sprintf("'%s' object cannot be interpreted as an integer", val.Type())}
						}
						intVal := int(num)
						if intVal < 0 || intVal > 255 {
							return nil, &ValueError{Message: "bytes must be in range(0, 256)"}
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
					return nil, &TypeError{Message: fmt.Sprintf("cannot convert '%s' object to bytearray", v.Type())}
				}
			}
			// bytearray(string, encoding)
			if len(args) == 2 {
				str, ok := args[0].(StringValue)
				if !ok {
					return nil, &TypeError{Message: fmt.Sprintf("bytearray() argument 1 must be str, not %s", args[0].Type())}
				}
				enc, ok := args[1].(StringValue)
				if !ok {
					return nil, &TypeError{Message: fmt.Sprintf("bytearray() argument 2 must be str, not %s", args[1].Type())}
				}
				if string(enc) != "utf-8" {
					return nil, &ValueError{Message: "only utf-8 encoding is currently supported"}
				}
				return NewByteArray([]byte(string(str))), nil
			}
			return nil, &TypeError{Message: fmt.Sprintf("bytearray() takes at most 2 arguments (%d given)", len(args))}
		},
		Str: func(v Value) string {
			return v.(*ByteArrayValue).String()
		},
		Repr: func(v Value) string {
			return v.(*ByteArrayValue).String()
		},
	})
}

// registerComplexType registers the complex type descriptor
func registerComplexType() {
	RegisterType(&TypeDescriptor{
		Name:       "complex",
		PythonName: "complex",
		BaseType:   ComplexType,
		Properties: map[string]*PropertyDescriptor{
			"real": {
				Name: "real",
				Doc:  "Return the real part of the complex number",
				Getter: func(receiver Value) (Value, error) {
					c := receiver.(ComplexValue)
					return NumberValue(real(complex128(c))), nil
				},
			},
			"imag": {
				Name: "imag",
				Doc:  "Return the imaginary part of the complex number",
				Getter: func(receiver Value) (Value, error) {
					c := receiver.(ComplexValue)
					return NumberValue(imag(complex128(c))), nil
				},
			},
		},
		Methods: map[string]*MethodDescriptor{
			"conjugate": {
				Name:    "conjugate",
				Arity:   0,
				Doc:     "Return the complex conjugate",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					c := receiver.(ComplexValue)
					r := real(complex128(c))
					i := imag(complex128(c))
					return ComplexValue(complex(r, -i)), nil
				},
			},
			"__neg__": {
				Name:    "__neg__",
				Arity:   0,
				Doc:     "Return -self",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					c := receiver.(ComplexValue)
					return ComplexValue(-complex128(c)), nil
				},
			},
			"__pos__": {
				Name:    "__pos__",
				Arity:   0,
				Doc:     "Return +self",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					return receiver, nil
				},
			},
			"__abs__": {
				Name:    "__abs__",
				Arity:   0,
				Doc:     "Return the absolute value (magnitude)",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					c := receiver.(ComplexValue)
					r := real(complex128(c))
					i := imag(complex128(c))
					return NumberValue(math.Sqrt(r*r + i*i)), nil
				},
			},
			"__bool__": {
				Name:    "__bool__",
				Arity:   0,
				Doc:     "Return True if not zero",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					c := receiver.(ComplexValue)
					r := real(complex128(c))
					i := imag(complex128(c))
					return BoolValue(r != 0 || i != 0), nil
				},
			},
			"__hash__": {
				Name:    "__hash__",
				Arity:   0,
				Doc:     "Return hash value",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					c := receiver.(ComplexValue)
					r := real(complex128(c))
					i := imag(complex128(c))
					// Simple hash combining real and imaginary parts
					hash := int64(r*1000003) ^ int64(i*1000003)
					return NumberValue(hash), nil
				},
			},
			"__eq__": {
				Name:    "__eq__",
				Arity:   1,
				Doc:     "Return self==value",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					c1 := receiver.(ComplexValue)
					switch c2 := args[0].(type) {
					case ComplexValue:
						return BoolValue(c1 == c2), nil
					case NumberValue:
						// Compare with real number (imag must be 0)
						r := real(complex128(c1))
						i := imag(complex128(c1))
						return BoolValue(i == 0 && r == float64(c2)), nil
					}
					return False, nil
				},
			},
			"__rmul__": {
				Name:    "__rmul__",
				Arity:   1,
				Doc:     "Return value*self (reversed operand for multiplication)",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					c := receiver.(ComplexValue)
					switch n := args[0].(type) {
					case NumberValue:
						return ComplexValue(complex(float64(n), 0) * complex128(c)), nil
					case ComplexValue:
						return ComplexValue(complex128(n) * complex128(c)), nil
					}
					return NotImplemented, nil
				},
			},
			"__rsub__": {
				Name:    "__rsub__",
				Arity:   1,
				Doc:     "Return value-self (reversed operand for subtraction)",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					c := receiver.(ComplexValue)
					switch n := args[0].(type) {
					case NumberValue:
						return ComplexValue(complex(float64(n), 0) - complex128(c)), nil
					case ComplexValue:
						return ComplexValue(complex128(n) - complex128(c)), nil
					}
					return NotImplemented, nil
				},
			},
			"__radd__": {
				Name:    "__radd__",
				Arity:   1,
				Doc:     "Return value+self (reversed operand for addition)",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					c := receiver.(ComplexValue)
					switch n := args[0].(type) {
					case NumberValue:
						return ComplexValue(complex(float64(n), 0) + complex128(c)), nil
					case ComplexValue:
						return ComplexValue(complex128(n) + complex128(c)), nil
					}
					return NotImplemented, nil
				},
			},
		},
		Constructor: func(args []Value, ctx *Context) (Value, error) {
			if len(args) == 0 {
				return ComplexValue(0), nil
			}
			if len(args) == 1 {
				switch v := args[0].(type) {
				case NumberValue:
					return ComplexValue(complex(float64(v), 0)), nil
				case ComplexValue:
					return v, nil
				case StringValue:
					// Parse string like "1+2j" or "3j" or "5"
					s := strings.TrimSpace(string(v))
					c, err := parseComplexString(s)
					if err != nil {
						return nil, &ValueError{Message: fmt.Sprintf("complex() arg is a malformed string: %s", err)}
					}
					return c, nil
				default:
					return nil, &TypeError{Message: fmt.Sprintf("complex() first argument must be a string or a number, not '%s'", v.Type())}
				}
			}
			if len(args) == 2 {
				var realPart, imagPart float64
				switch r := args[0].(type) {
				case NumberValue:
					realPart = float64(r)
				case ComplexValue:
					return nil, &TypeError{Message: "complex() can't take second arg if first is a string"}
				default:
					return nil, &TypeError{Message: fmt.Sprintf("complex() first argument must be a string or a number, not '%s'", r.Type())}
				}
				switch i := args[1].(type) {
				case NumberValue:
					imagPart = float64(i)
				default:
					return nil, &TypeError{Message: fmt.Sprintf("complex() second argument must be a number, not '%s'", i.Type())}
				}
				return ComplexValue(complex(realPart, imagPart)), nil
			}
			return nil, &TypeError{Message: fmt.Sprintf("complex() takes at most 2 arguments (%d given)", len(args))}
		},
		Str: func(v Value) string {
			return v.(ComplexValue).String()
		},
		Repr: func(v Value) string {
			return v.(ComplexValue).String()
		},
	})
}

// parseComplexString parses a string like "1+2j", "3j", "5", "-2-3j"
func parseComplexString(s string) (ComplexValue, error) {
	s = strings.TrimSpace(s)
	if s == "" {
		return 0, fmt.Errorf("empty string")
	}

	// Remove parentheses if present
	if strings.HasPrefix(s, "(") && strings.HasSuffix(s, ")") {
		s = s[1 : len(s)-1]
	}

	// Check for 'j' at the end
	hasJ := strings.HasSuffix(s, "j") || strings.HasSuffix(s, "J")

	if !hasJ {
		// Try to parse as pure real
		real, err := strconv.ParseFloat(s, 64)
		if err != nil {
			return 0, err
		}
		return ComplexValue(complex(real, 0)), nil
	}

	// Remove trailing 'j'
	s = s[:len(s)-1]

	// Pure imaginary: "3", "-3", "", "+"
	// Find the last + or - that's not at the start and not after 'e' or 'E'
	lastSign := -1
	for i := len(s) - 1; i > 0; i-- {
		if (s[i] == '+' || s[i] == '-') && s[i-1] != 'e' && s[i-1] != 'E' {
			lastSign = i
			break
		}
	}

	if lastSign == -1 {
		// Pure imaginary
		if s == "" || s == "+" {
			return ComplexValue(complex(0, 1)), nil
		}
		if s == "-" {
			return ComplexValue(complex(0, -1)), nil
		}
		imag, err := strconv.ParseFloat(s, 64)
		if err != nil {
			return 0, err
		}
		return ComplexValue(complex(0, imag)), nil
	}

	// Has both real and imaginary parts
	realStr := s[:lastSign]
	imagStr := s[lastSign:]

	realPart, err := strconv.ParseFloat(realStr, 64)
	if err != nil {
		return 0, err
	}

	if imagStr == "+" {
		return ComplexValue(complex(realPart, 1)), nil
	}
	if imagStr == "-" {
		return ComplexValue(complex(realPart, -1)), nil
	}

	imagPart, err := strconv.ParseFloat(imagStr, 64)
	if err != nil {
		return 0, err
	}

	return ComplexValue(complex(realPart, imagPart)), nil
}

// registerDecimalType registers the decimal type descriptor
func registerDecimalType() {
	RegisterType(&TypeDescriptor{
		Name:       "decimal",
		PythonName: "Decimal",
		BaseType:   DecimalType,
		Methods: map[string]*MethodDescriptor{
			// Arithmetic operations
			"__add__": {
				Name:    "__add__",
				Arity:   1,
				Doc:     "Return self+value",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					d1 := receiver.(*DecimalValue).GetDecimal()
					switch v := args[0].(type) {
					case *DecimalValue:
						return NewDecimal(d1.Add(v.GetDecimal())), nil
					case NumberValue:
						d2 := decimal.NewFromFloat(float64(v))
						return NewDecimal(d1.Add(d2)), nil
					default:
						return nil, &TypeError{Message: fmt.Sprintf("unsupported operand type(s) for +: 'Decimal' and '%s'", args[0].Type())}
					}
				},
			},
			"__sub__": {
				Name:    "__sub__",
				Arity:   1,
				Doc:     "Return self-value",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					d1 := receiver.(*DecimalValue).GetDecimal()
					switch v := args[0].(type) {
					case *DecimalValue:
						return NewDecimal(d1.Sub(v.GetDecimal())), nil
					case NumberValue:
						d2 := decimal.NewFromFloat(float64(v))
						return NewDecimal(d1.Sub(d2)), nil
					default:
						return nil, &TypeError{Message: fmt.Sprintf("unsupported operand type(s) for -: 'Decimal' and '%s'", args[0].Type())}
					}
				},
			},
			"__mul__": {
				Name:    "__mul__",
				Arity:   1,
				Doc:     "Return self*value",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					d1 := receiver.(*DecimalValue).GetDecimal()
					switch v := args[0].(type) {
					case *DecimalValue:
						return NewDecimal(d1.Mul(v.GetDecimal())), nil
					case NumberValue:
						d2 := decimal.NewFromFloat(float64(v))
						return NewDecimal(d1.Mul(d2)), nil
					default:
						return nil, &TypeError{Message: fmt.Sprintf("unsupported operand type(s) for *: 'Decimal' and '%s'", args[0].Type())}
					}
				},
			},
			"__truediv__": {
				Name:    "__truediv__",
				Arity:   1,
				Doc:     "Return self/value",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					d1 := receiver.(*DecimalValue).GetDecimal()
					var d2 decimal.Decimal
					switch v := args[0].(type) {
					case *DecimalValue:
						d2 = v.GetDecimal()
					case NumberValue:
						d2 = decimal.NewFromFloat(float64(v))
					default:
						return nil, &TypeError{Message: fmt.Sprintf("unsupported operand type(s) for /: 'Decimal' and '%s'", args[0].Type())}
					}
					if d2.IsZero() {
						return nil, &ZeroDivisionError{}
					}
					return NewDecimal(d1.Div(d2)), nil
				},
			},
			"__neg__": {
				Name:    "__neg__",
				Arity:   0,
				Doc:     "Return -self",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					d := receiver.(*DecimalValue).GetDecimal()
					return NewDecimal(d.Neg()), nil
				},
			},
			"__abs__": {
				Name:    "__abs__",
				Arity:   0,
				Doc:     "Return abs(self)",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					d := receiver.(*DecimalValue).GetDecimal()
					return NewDecimal(d.Abs()), nil
				},
			},
			// Comparison operations
			"__eq__": {
				Name:    "__eq__",
				Arity:   1,
				Doc:     "Return self==value",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					d1 := receiver.(*DecimalValue).GetDecimal()
					switch v := args[0].(type) {
					case *DecimalValue:
						return BoolValue(d1.Equal(v.GetDecimal())), nil
					case NumberValue:
						d2 := decimal.NewFromFloat(float64(v))
						return BoolValue(d1.Equal(d2)), nil
					default:
						return False, nil
					}
				},
			},
			"__lt__": {
				Name:    "__lt__",
				Arity:   1,
				Doc:     "Return self<value",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					d1 := receiver.(*DecimalValue).GetDecimal()
					switch v := args[0].(type) {
					case *DecimalValue:
						return BoolValue(d1.LessThan(v.GetDecimal())), nil
					case NumberValue:
						d2 := decimal.NewFromFloat(float64(v))
						return BoolValue(d1.LessThan(d2)), nil
					default:
						return nil, &TypeError{Message: fmt.Sprintf("'<' not supported between instances of 'Decimal' and '%s'", args[0].Type())}
					}
				},
			},
			"__le__": {
				Name:    "__le__",
				Arity:   1,
				Doc:     "Return self<=value",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					d1 := receiver.(*DecimalValue).GetDecimal()
					switch v := args[0].(type) {
					case *DecimalValue:
						return BoolValue(d1.LessThanOrEqual(v.GetDecimal())), nil
					case NumberValue:
						d2 := decimal.NewFromFloat(float64(v))
						return BoolValue(d1.LessThanOrEqual(d2)), nil
					default:
						return nil, &TypeError{Message: fmt.Sprintf("'<=' not supported between instances of 'Decimal' and '%s'", args[0].Type())}
					}
				},
			},
			"__gt__": {
				Name:    "__gt__",
				Arity:   1,
				Doc:     "Return self>value",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					d1 := receiver.(*DecimalValue).GetDecimal()
					switch v := args[0].(type) {
					case *DecimalValue:
						return BoolValue(d1.GreaterThan(v.GetDecimal())), nil
					case NumberValue:
						d2 := decimal.NewFromFloat(float64(v))
						return BoolValue(d1.GreaterThan(d2)), nil
					default:
						return nil, &TypeError{Message: fmt.Sprintf("'>' not supported between instances of 'Decimal' and '%s'", args[0].Type())}
					}
				},
			},
			"__ge__": {
				Name:    "__ge__",
				Arity:   1,
				Doc:     "Return self>=value",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					d1 := receiver.(*DecimalValue).GetDecimal()
					switch v := args[0].(type) {
					case *DecimalValue:
						return BoolValue(d1.GreaterThanOrEqual(v.GetDecimal())), nil
					case NumberValue:
						d2 := decimal.NewFromFloat(float64(v))
						return BoolValue(d1.GreaterThanOrEqual(d2)), nil
					default:
						return nil, &TypeError{Message: fmt.Sprintf("'>=' not supported between instances of 'Decimal' and '%s'", args[0].Type())}
					}
				},
			},
			"__ne__": {
				Name:    "__ne__",
				Arity:   1,
				Doc:     "Return self!=value",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					d1 := receiver.(*DecimalValue).GetDecimal()
					switch v := args[0].(type) {
					case *DecimalValue:
						return BoolValue(!d1.Equal(v.GetDecimal())), nil
					case NumberValue:
						d2 := decimal.NewFromFloat(float64(v))
						return BoolValue(!d1.Equal(d2)), nil
					default:
						return True, nil
					}
				},
			},
			// Decimal-specific methods
			"sqrt": {
				Name:    "sqrt",
				Arity:   0,
				Doc:     "Return the square root of self",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					d := receiver.(*DecimalValue).GetDecimal()
					if d.IsNegative() {
						return nil, &ValueError{Message: "cannot take square root of negative number"}
					}
					// Use big.Float for square root
					f, _ := d.Float64()
					result := math.Sqrt(f)
					return NewDecimalFromFloat(result), nil
				},
			},
			"quantize": {
				Name:    "quantize",
				Arity:   1,
				Doc:     "Quantize to a fixed exponent",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					d := receiver.(*DecimalValue).GetDecimal()
					exp, ok := args[0].(*DecimalValue)
					if !ok {
						return nil, &TypeError{Message: "quantize() argument must be a Decimal"}
					}
					// Round to the same exponent as exp
					expInt := exp.GetDecimal().Exponent()
					return NewDecimal(d.Round(expInt)), nil
				},
			},
			"normalize": {
				Name:    "normalize",
				Arity:   0,
				Doc:     "Normalize the decimal by stripping trailing zeros",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					d := receiver.(*DecimalValue).GetDecimal()
					// Parse and reformat to normalize
					str := d.String()
					normalized, _ := decimal.NewFromString(str)
					return NewDecimal(normalized), nil
				},
			},
			"is_finite": {
				Name:    "is_finite",
				Arity:   0,
				Doc:     "Return True if the value is finite",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					// shopspring/decimal doesn't have infinity/nan support the same way
					// For now, all decimals are finite
					return True, nil
				},
			},
			"is_nan": {
				Name:    "is_nan",
				Arity:   0,
				Doc:     "Return True if the value is NaN",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					// shopspring/decimal doesn't support NaN
					return False, nil
				},
			},
			"is_zero": {
				Name:    "is_zero",
				Arity:   0,
				Doc:     "Return True if the value is zero",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					d := receiver.(*DecimalValue).GetDecimal()
					return BoolValue(d.IsZero()), nil
				},
			},
			"as_tuple": {
				Name:    "as_tuple",
				Arity:   0,
				Doc:     "Return a tuple representation (sign, digits, exponent)",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					d := receiver.(*DecimalValue).GetDecimal()
					sign := 0
					if d.IsNegative() {
						sign = 1
					}

					// Get coefficient and exponent
					coef := d.Coefficient()
					exp := d.Exponent()

					// Convert coefficient to digits
					digitStr := coef.String()
					if sign == 1 && len(digitStr) > 0 && digitStr[0] == '-' {
						digitStr = digitStr[1:]
					}

					digits := make([]Value, len(digitStr))
					for i, ch := range digitStr {
						digits[i] = NumberValue(ch - '0')
					}

					return TupleValue([]Value{
						NumberValue(sign),
						TupleValue(digits),
						NumberValue(exp),
					}), nil
				},
			},
			"to_integral": {
				Name:    "to_integral",
				Arity:   0,
				Doc:     "Round to the nearest integer",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					d := receiver.(*DecimalValue).GetDecimal()
					return NewDecimal(d.Round(0)), nil
				},
			},
			"__float__": {
				Name:    "__float__",
				Arity:   0,
				Doc:     "Convert to float",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					d := receiver.(*DecimalValue).GetDecimal()
					f, _ := d.Float64()
					return NumberValue(f), nil
				},
			},
			"__int__": {
				Name:    "__int__",
				Arity:   0,
				Doc:     "Convert to int",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					d := receiver.(*DecimalValue).GetDecimal()
					return NumberValue(d.IntPart()), nil
				},
			},
		},
		Str: func(v Value) string {
			return v.(*DecimalValue).String()
		},
		Repr: func(v Value) string {
			return "Decimal('" + v.(*DecimalValue).String() + "')"
		},
	})
}

// Helper functions for bytes operations

// containsByte checks if a byte is in a slice of bytes
func containsByte(chars []byte, b byte) bool {
	for _, c := range chars {
		if c == b {
			return true
		}
	}
	return false
}

// splitBytesWhitespace splits bytes on whitespace, like Python's split()
func splitBytesWhitespace(b []byte) [][]byte {
	var result [][]byte
	start := -1
	for i, c := range b {
		isSpace := c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f' || c == '\v'
		if isSpace {
			if start >= 0 {
				result = append(result, b[start:i])
				start = -1
			}
		} else {
			if start < 0 {
				start = i
			}
		}
	}
	if start >= 0 {
		result = append(result, b[start:])
	}
	return result
}

// splitBytes splits bytes by a separator
func splitBytes(b, sep []byte) [][]byte {
	if len(sep) == 0 {
		// Split on empty separator: split into individual bytes
		result := make([][]byte, len(b))
		for i := range b {
			result[i] = b[i : i+1]
		}
		return result
	}
	var result [][]byte
	start := 0
	for i := 0; i <= len(b)-len(sep); i++ {
		match := true
		for j := 0; j < len(sep); j++ {
			if b[i+j] != sep[j] {
				match = false
				break
			}
		}
		if match {
			result = append(result, b[start:i])
			start = i + len(sep)
			i = start - 1 // -1 because loop will increment
		}
	}
	result = append(result, b[start:])
	return result
}

// replaceBytes replaces occurrences of old with new in b
func replaceBytes(b, old, new []byte, count int) []byte {
	if count == 0 || len(old) == 0 {
		return b
	}
	var result []byte
	start := 0
	replaced := 0
	for i := 0; i <= len(b)-len(old); i++ {
		if count >= 0 && replaced >= count {
			break
		}
		match := true
		for j := 0; j < len(old); j++ {
			if b[i+j] != old[j] {
				match = false
				break
			}
		}
		if match {
			result = append(result, b[start:i]...)
			result = append(result, new...)
			start = i + len(old)
			i = start - 1
			replaced++
		}
	}
	result = append(result, b[start:]...)
	return result
}

// findBytes finds the first occurrence of sub in b
func findBytes(b, sub []byte) int {
	if len(sub) == 0 {
		return 0
	}
	if len(sub) > len(b) {
		return -1
	}
	for i := 0; i <= len(b)-len(sub); i++ {
		match := true
		for j := 0; j < len(sub); j++ {
			if b[i+j] != sub[j] {
				match = false
				break
			}
		}
		if match {
			return i
		}
	}
	return -1
}

// countBytes counts non-overlapping occurrences of sub in b
func countBytes(b, sub []byte) int {
	if len(sub) == 0 {
		return len(b) + 1
	}
	count := 0
	for i := 0; i <= len(b)-len(sub); i++ {
		match := true
		for j := 0; j < len(sub); j++ {
			if b[i+j] != sub[j] {
				match = false
				break
			}
		}
		if match {
			count++
			i += len(sub) - 1 // Skip past match (non-overlapping)
		}
	}
	return count
}
