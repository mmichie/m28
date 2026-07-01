package core

import (
	"fmt"
	"math"
	"math/big"
	"strconv"
)

// floatToIntValue truncates a float toward zero into a Python int, promoting to
// an arbitrary-precision BigIntValue when the magnitude exceeds float64's exact
// integer range. It mirrors CPython's int(float)/float.__trunc__ error cases.
func floatToIntValue(f float64) (Value, error) {
	if math.IsInf(f, 0) {
		return nil, &OverflowError{Message: "cannot convert float infinity to integer"}
	}
	if math.IsNaN(f) {
		return nil, &ValueError{Message: "cannot convert float NaN to integer"}
	}
	t := math.Trunc(f)
	if t >= MinSafeInt && t <= MaxSafeInt {
		return NumberValue(t), nil
	}
	bi, _ := big.NewFloat(t).Int(nil)
	return NewBigInt(bi), nil
}

// roundHalfEven rounds f to nd decimal places using round-half-to-even, matching
// CPython's round(). It rounds the exact float64 value (via decimal formatting,
// which Go performs round-half-to-even) rather than pre-multiplying by 10^nd,
// avoiding the double-rounding error that makes 2.675*100 land on 267.5.
func roundHalfEven(f float64, nd int) float64 {
	if math.IsNaN(f) || math.IsInf(f, 0) {
		return f
	}
	if nd >= 0 {
		r, err := strconv.ParseFloat(strconv.FormatFloat(f, 'f', nd, 64), 64)
		if err != nil {
			return f
		}
		return r
	}
	// Negative ndigits: round to the nearest 10^(-nd). Format at 0 decimals
	// after scaling down, then scale back, keeping half-to-even at the target
	// place.
	scale := math.Pow(10, float64(-nd))
	r, err := strconv.ParseFloat(strconv.FormatFloat(f/scale, 'f', 0, 64), 64)
	if err != nil {
		return f
	}
	return r * scale
}

// registerFloatType registers the float type descriptor. Float and int are
// distinct Python types, so float gets its own methods (is_integer, hex,
// float-valued __round__, no to_bytes/__index__) rather than sharing int's.
func registerFloatType() {
	RegisterType(&TypeDescriptor{
		Name:       "float",
		PythonName: "float",
		BaseType:   FloatType,
		Repr:       func(v Value) string { return PyFloatRepr(float64(v.(FloatValue))) },
		Str:        func(v Value) string { return PyFloatRepr(float64(v.(FloatValue))) },
		Methods: map[string]*MethodDescriptor{
			"__neg__": floatUnary("__neg__", "Return -self", func(n float64) float64 { return -n }),
			"__pos__": floatUnary("__pos__", "Return +self", func(n float64) float64 { return n }),
			"__abs__": floatUnary("__abs__", "Return abs(self)", math.Abs),
			"__float__": {
				Name: "__float__", Arity: 0, Doc: "Return float(self)", Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					return receiver, nil
				},
			},
			"__int__": {
				Name: "__int__", Arity: 0, Doc: "Return int(self)", Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					f, _ := AsFloat(receiver)
					return floatToIntValue(f)
				},
			},
			"__trunc__": {
				Name: "__trunc__", Arity: 0, Doc: "Truncate self toward zero", Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					f, _ := AsFloat(receiver)
					return floatToIntValue(f)
				},
			},
			"__floor__": {
				Name: "__floor__", Arity: 0, Doc: "Return the floor of self as an int", Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					f, _ := AsFloat(receiver)
					return floatToIntValue(math.Floor(f))
				},
			},
			"__ceil__": {
				Name: "__ceil__", Arity: 0, Doc: "Return the ceiling of self as an int", Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					f, _ := AsFloat(receiver)
					return floatToIntValue(math.Ceil(f))
				},
			},
			"__round__": {
				Name: "__round__", Arity: -1, Doc: "Round self to ndigits", Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					f, _ := AsFloat(receiver)
					// round(x) with no/None ndigits returns an int.
					if len(args) == 0 || args[0] == Nil {
						return floatToIntValue(roundHalfEven(f, 0))
					}
					nd, ok := AsFloat(args[0])
					if !ok {
						return nil, &TypeError{Message: "'" + string(args[0].Type()) + "' object cannot be interpreted as an integer"}
					}
					return FloatValue(roundHalfEven(f, int(nd))), nil
				},
			},
			"__bool__": {
				Name: "__bool__", Arity: 0, Doc: "Return bool(self)", Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					f, _ := AsFloat(receiver)
					return BoolValue(f != 0), nil
				},
			},
			"is_integer": {
				Name: "is_integer", Arity: 0, Doc: "Return True if the float has no fractional part", Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					f, _ := AsFloat(receiver)
					if math.IsInf(f, 0) || math.IsNaN(f) {
						return False, nil
					}
					return BoolValue(f == math.Trunc(f)), nil
				},
			},
			"hex": {
				Name: "hex", Arity: 0, Doc: "Return a hexadecimal representation of the float", Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					f, _ := AsFloat(receiver)
					return StringValue(FloatHex(f)), nil
				},
			},
			"as_integer_ratio": {
				Name: "as_integer_ratio", Arity: 0, Doc: "Return a pair of integers whose ratio equals the float", Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					f, _ := AsFloat(receiver)
					return floatAsIntegerRatio(f)
				},
			},
			"conjugate": {
				Name: "conjugate", Arity: 0, Doc: "Return self (floats are their own conjugate)", Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					return receiver, nil
				},
			},
			// NOTE: intentionally no __hash__ here. int has no registered __hash__
			// either, so both fall through hash()'s ValueToKey path, which keys an
			// int and an equal float identically -- keeping hash(1) == hash(1.0).
		},
		Constructor: func(args []Value, ctx *Context) (Value, error) {
			if len(args) == 0 {
				return FloatValue(0), nil
			}
			if len(args) == 1 {
				if f, ok := AsFloat(args[0]); ok {
					return FloatValue(f), nil
				}
				return nil, &TypeError{Message: fmt.Sprintf("float() argument must be a string or a number, not '%s'", args[0].Type())}
			}
			return nil, &TypeError{Message: fmt.Sprintf("float expected at most 1 argument, got %d", len(args))}
		},
	})
}

// floatUnary builds a MethodDescriptor for a unary float->float operation whose
// result is a FloatValue.
func floatUnary(name, doc string, op func(float64) float64) *MethodDescriptor {
	return &MethodDescriptor{
		Name: name, Arity: 0, Doc: doc, Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			f, _ := AsFloat(receiver)
			return FloatValue(op(f)), nil
		},
	}
}

// floatAsIntegerRatio implements float.as_integer_ratio().
func floatAsIntegerRatio(n float64) (Value, error) {
	if math.IsInf(n, 0) {
		return nil, &OverflowError{Message: "cannot convert Infinity to integer ratio"}
	}
	if math.IsNaN(n) {
		return nil, &ValueError{Message: "cannot convert NaN to integer ratio"}
	}
	if n == 0 {
		return TupleValue{NumberValue(0), NumberValue(1)}, nil
	}
	mantissa, exponent := math.Frexp(n)
	const mantissaBits = 53
	mantissa *= float64(int64(1) << mantissaBits)
	exponent -= mantissaBits
	numerator := int64(mantissa)
	denominator := int64(1)
	if exponent > 0 {
		numerator <<= uint(exponent)
	} else if exponent < 0 {
		denominator <<= uint(-exponent)
	}
	g := gcd(abs64(numerator), denominator)
	numerator /= g
	denominator /= g
	return TupleValue{NumberValue(numerator), NumberValue(denominator)}, nil
}
