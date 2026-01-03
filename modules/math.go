package modules

import (
	"math"

	"github.com/mmichie/m28/common/builders"
	"github.com/mmichie/m28/common/errors"
	"github.com/mmichie/m28/core"
)

// InitMathModule creates and returns the math module with comprehensive CPython compatibility
// This C extension module provides mathematical functions as defined in CPython's mathmodule.c
func InitMathModule() *core.DictValue {
	mathModule := core.NewDict()

	// Trigonometric functions
	mathModule.Set("sqrt", core.NewBuiltinFunction(builders.UnaryNumber("sqrt", func(n float64) (float64, error) {
		if n < 0 {
			return 0, errors.NewValueError("sqrt", "math domain error")
		}
		return math.Sqrt(n), nil
	})))
	mathModule.Set("sin", core.NewBuiltinFunction(builders.UnaryNumberSimple("sin", math.Sin)))
	mathModule.Set("cos", core.NewBuiltinFunction(builders.UnaryNumberSimple("cos", math.Cos)))
	mathModule.Set("tan", core.NewBuiltinFunction(builders.UnaryNumberSimple("tan", math.Tan)))
	mathModule.Set("asin", core.NewBuiltinFunction(builders.UnaryNumberSimple("asin", math.Asin)))
	mathModule.Set("acos", core.NewBuiltinFunction(builders.UnaryNumberSimple("acos", math.Acos)))
	mathModule.Set("atan", core.NewBuiltinFunction(builders.UnaryNumberSimple("atan", math.Atan)))
	mathModule.Set("atan2", core.NewBuiltinFunction(builders.BinaryNumberSimple("atan2", math.Atan2)))

	// Power and exponential functions
	mathModule.Set("pow", core.NewBuiltinFunction(builders.BinaryNumberSimple("pow", math.Pow)))
	mathModule.Set("exp", core.NewBuiltinFunction(builders.UnaryNumberSimple("exp", math.Exp)))
	mathModule.Set("log", core.NewBuiltinFunction(builders.UnaryNumberSimple("log", math.Log)))
	mathModule.Set("log10", core.NewBuiltinFunction(builders.UnaryNumberSimple("log10", math.Log10)))
	mathModule.Set("log2", core.NewBuiltinFunction(builders.UnaryNumberSimple("log2", math.Log2)))

	// Rounding functions - check for __floor__, __ceil__, __trunc__ dunder methods first
	mathModule.Set("floor", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, errors.NewTypeError("floor", "exactly 1 argument", "got different count")
		}
		// Check for __floor__ dunder method
		if obj, ok := args[0].(core.Object); ok {
			if method, found := obj.GetAttr("__floor__"); found {
				if callable, isCallable := method.(core.Callable); isCallable {
					return callable.Call([]core.Value{}, ctx)
				}
			}
		}
		// Fall back to numeric conversion
		num, ok := args[0].(core.NumberValue)
		if !ok {
			return nil, errors.NewTypeError("floor", "number", string(args[0].Type()))
		}
		return core.NumberValue(math.Floor(float64(num))), nil
	}))
	mathModule.Set("ceil", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, errors.NewTypeError("ceil", "exactly 1 argument", "got different count")
		}
		// Check for __ceil__ dunder method
		if obj, ok := args[0].(core.Object); ok {
			if method, found := obj.GetAttr("__ceil__"); found {
				if callable, isCallable := method.(core.Callable); isCallable {
					return callable.Call([]core.Value{}, ctx)
				}
			}
		}
		// Fall back to numeric conversion
		num, ok := args[0].(core.NumberValue)
		if !ok {
			return nil, errors.NewTypeError("ceil", "number", string(args[0].Type()))
		}
		return core.NumberValue(math.Ceil(float64(num))), nil
	}))
	mathModule.Set("trunc", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, errors.NewTypeError("trunc", "exactly 1 argument", "got different count")
		}
		// Check for __trunc__ dunder method
		if obj, ok := args[0].(core.Object); ok {
			if method, found := obj.GetAttr("__trunc__"); found {
				if callable, isCallable := method.(core.Callable); isCallable {
					return callable.Call([]core.Value{}, ctx)
				}
			}
		}
		// Fall back to numeric conversion
		num, ok := args[0].(core.NumberValue)
		if !ok {
			return nil, errors.NewTypeError("trunc", "number", string(args[0].Type()))
		}
		return core.NumberValue(math.Trunc(float64(num))), nil
	}))
	mathModule.Set("modf", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, errors.NewTypeError("modf", "exactly 1 argument", "got different count")
		}
		num, ok := args[0].(core.NumberValue)
		if !ok {
			return nil, errors.NewTypeError("modf", "float", string(args[0].Type()))
		}
		intPart, frac := math.Modf(float64(num))
		return core.TupleValue{core.NumberValue(frac), core.NumberValue(intPart)}, nil
	}))

	// Other math functions
	mathModule.Set("abs", core.NewBuiltinFunction(builders.UnaryNumberSimple("abs", math.Abs)))
	mathModule.Set("fabs", core.NewBuiltinFunction(builders.UnaryNumberSimple("fabs", math.Abs))) // fabs is abs for floats
	mathModule.Set("hypot", core.NewBuiltinFunction(builders.BinaryNumberSimple("hypot", math.Hypot)))
	mathModule.Set("copysign", core.NewBuiltinFunction(builders.BinaryNumberSimple("copysign", math.Copysign)))

	// Hyperbolic functions
	mathModule.Set("sinh", core.NewBuiltinFunction(builders.UnaryNumberSimple("sinh", math.Sinh)))
	mathModule.Set("cosh", core.NewBuiltinFunction(builders.UnaryNumberSimple("cosh", math.Cosh)))
	mathModule.Set("tanh", core.NewBuiltinFunction(builders.UnaryNumberSimple("tanh", math.Tanh)))
	mathModule.Set("asinh", core.NewBuiltinFunction(builders.UnaryNumberSimple("asinh", math.Asinh)))
	mathModule.Set("acosh", core.NewBuiltinFunction(builders.UnaryNumberSimple("acosh", math.Acosh)))
	mathModule.Set("atanh", core.NewBuiltinFunction(builders.UnaryNumberSimple("atanh", math.Atanh)))

	// Gamma functions (needed by random module)
	mathModule.Set("lgamma", core.NewBuiltinFunction(builders.UnaryNumber("lgamma", func(x float64) (float64, error) {
		if x <= 0 && x == math.Floor(x) {
			return 0, errors.NewValueError("lgamma", "math domain error")
		}
		result, _ := math.Lgamma(x) // Ignore sign, return log of absolute value
		return result, nil
	})))
	mathModule.Set("gamma", core.NewBuiltinFunction(builders.UnaryNumber("gamma", func(x float64) (float64, error) {
		if x <= 0 && x == math.Floor(x) {
			return 0, errors.NewValueError("gamma", "math domain error")
		}
		return math.Gamma(x), nil
	})))

	// Angle conversion
	mathModule.Set("degrees", core.NewBuiltinFunction(builders.UnaryNumber("degrees", func(x float64) (float64, error) {
		return x * 180.0 / math.Pi, nil
	})))
	mathModule.Set("radians", core.NewBuiltinFunction(builders.UnaryNumber("radians", func(x float64) (float64, error) {
		return x * math.Pi / 180.0, nil
	})))

	// Floating point inspection
	mathModule.Set("isnan", core.NewBuiltinFunction(builders.UnaryNumber("isnan", func(x float64) (float64, error) {
		if math.IsNaN(x) {
			return 1, nil // True
		}
		return 0, nil // False
	})))
	mathModule.Set("isinf", core.NewBuiltinFunction(builders.UnaryNumber("isinf", func(x float64) (float64, error) {
		if math.IsInf(x, 0) {
			return 1, nil // True
		}
		return 0, nil // False
	})))
	mathModule.Set("isfinite", core.NewBuiltinFunction(builders.UnaryNumber("isfinite", func(x float64) (float64, error) {
		if !math.IsNaN(x) && !math.IsInf(x, 0) {
			return 1, nil // True
		}
		return 0, nil // False
	})))

	// Integer math functions
	mathModule.Set("factorial", core.NewBuiltinFunction(builders.UnaryNumber("factorial", func(x float64) (float64, error) {
		n := int(x)
		if x != float64(n) || n < 0 {
			return 0, errors.NewValueError("factorial", "factorial() only accepts non-negative integers")
		}
		if n > 170 {
			return 0, errors.NewValueError("factorial", "factorial() argument is too large")
		}
		result := 1
		for i := 2; i <= n; i++ {
			result *= i
		}
		return float64(result), nil
	})))

	mathModule.Set("gcd", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, errors.NewRuntimeError("gcd", "gcd expected 2 arguments")
		}
		aNum, ok1 := args[0].(core.NumberValue)
		bNum, ok2 := args[1].(core.NumberValue)
		if !ok1 || !ok2 {
			return nil, errors.NewRuntimeError("gcd", "arguments must be integers")
		}
		a, b := int(aNum), int(bNum)
		if float64(a) != float64(aNum) || float64(b) != float64(bNum) {
			return nil, errors.NewRuntimeError("gcd", "arguments must be integers")
		}
		// Make absolute
		if a < 0 {
			a = -a
		}
		if b < 0 {
			b = -b
		}
		// Euclidean algorithm
		for b != 0 {
			a, b = b, a%b
		}
		return core.NumberValue(a), nil
	}))

	// Constants
	mathModule.Set("pi", core.NumberValue(math.Pi))
	mathModule.Set("e", core.NumberValue(math.E))
	mathModule.Set("tau", core.NumberValue(2*math.Pi)) // Python 3.6+
	mathModule.Set("inf", core.NumberValue(math.Inf(1)))
	mathModule.Set("nan", core.NumberValue(math.NaN()))

	return mathModule
}
