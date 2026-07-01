package modules

import (
	"math"
	"math/big"

	"github.com/mmichie/m28/common/builders"
	"github.com/mmichie/m28/common/errors"
	"github.com/mmichie/m28/core"
)

// InitMathModule creates and returns the math module with comprehensive CPython compatibility
// This C extension module provides mathematical functions as defined in CPython's mathmodule.c
func InitMathModule() *core.DictValue {
	mathModule := core.NewDict()

	// Trigonometric functions
	mathModule.Set("sqrt", core.NewBuiltinFunction(builders.UnaryFloat("sqrt", func(n float64) (float64, error) {
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
		num, ok := core.AsFloat(args[0])
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
		num, ok := core.AsFloat(args[0])
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
		num, ok := core.AsFloat(args[0])
		if !ok {
			return nil, errors.NewTypeError("trunc", "number", string(args[0].Type()))
		}
		return core.NumberValue(math.Trunc(float64(num))), nil
	}))
	mathModule.Set("modf", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, errors.NewTypeError("modf", "exactly 1 argument", "got different count")
		}
		num, ok := core.AsFloat(args[0])
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
	mathModule.Set("lgamma", core.NewBuiltinFunction(builders.UnaryFloat("lgamma", func(x float64) (float64, error) {
		if x <= 0 && x == math.Floor(x) {
			return 0, errors.NewValueError("lgamma", "math domain error")
		}
		result, _ := math.Lgamma(x) // Ignore sign, return log of absolute value
		return result, nil
	})))
	mathModule.Set("gamma", core.NewBuiltinFunction(builders.UnaryFloat("gamma", func(x float64) (float64, error) {
		if x <= 0 && x == math.Floor(x) {
			return 0, errors.NewValueError("gamma", "math domain error")
		}
		return math.Gamma(x), nil
	})))

	// Error functions
	mathModule.Set("erf", core.NewBuiltinFunction(builders.UnaryNumberSimple("erf", math.Erf)))
	mathModule.Set("erfc", core.NewBuiltinFunction(builders.UnaryNumberSimple("erfc", math.Erfc)))

	// Angle conversion
	mathModule.Set("degrees", core.NewBuiltinFunction(builders.UnaryFloat("degrees", func(x float64) (float64, error) {
		return x * 180.0 / math.Pi, nil
	})))
	mathModule.Set("radians", core.NewBuiltinFunction(builders.UnaryFloat("radians", func(x float64) (float64, error) {
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
		aNum, ok1 := core.AsFloat(args[0])
		bNum, ok2 := core.AsFloat(args[1])
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

	// ---- Additional CPython math functions ----

	// ldexp(x, i) returns x * (2**i)
	mathModule.Set("ldexp", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, errors.NewTypeError("ldexp", "exactly 2 arguments", "got different count")
		}
		x, ok1 := core.AsFloat(args[0])
		i, ok2 := core.AsFloat(args[1])
		if !ok1 || !ok2 {
			return nil, errors.NewTypeError("ldexp", "number", "got non-number")
		}
		return core.NumberValue(math.Ldexp(float64(x), int(i))), nil
	}))

	// frexp(x) returns (m, e) such that x == m * 2**e and 0.5 <= abs(m) < 1
	mathModule.Set("frexp", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, errors.NewTypeError("frexp", "exactly 1 argument", "got different count")
		}
		x, ok := core.AsFloat(args[0])
		if !ok {
			return nil, errors.NewTypeError("frexp", "number", string(args[0].Type()))
		}
		frac, exp := math.Frexp(float64(x))
		return core.TupleValue{core.NumberValue(frac), core.NumberValue(exp)}, nil
	}))

	mathModule.Set("fmod", core.NewBuiltinFunction(builders.BinaryNumberSimple("fmod", math.Mod)))
	mathModule.Set("remainder", core.NewBuiltinFunction(builders.BinaryNumberSimple("remainder", math.Remainder)))
	mathModule.Set("expm1", core.NewBuiltinFunction(builders.UnaryNumberSimple("expm1", math.Expm1)))
	mathModule.Set("log1p", core.NewBuiltinFunction(builders.UnaryNumberSimple("log1p", math.Log1p)))
	mathModule.Set("cbrt", core.NewBuiltinFunction(builders.UnaryNumberSimple("cbrt", math.Cbrt)))
	mathModule.Set("exp2", core.NewBuiltinFunction(builders.UnaryNumberSimple("exp2", math.Exp2)))
	mathModule.Set("nextafter", core.NewBuiltinFunction(builders.BinaryNumberSimple("nextafter", math.Nextafter)))

	// fma(x, y, z): fused multiply-add (x*y + z) with a single rounding step
	mathModule.Set("fma", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 3 {
			return nil, errors.NewTypeError("fma", "exactly 3 arguments", "got different count")
		}
		x, ok1 := core.AsFloat(args[0])
		y, ok2 := core.AsFloat(args[1])
		z, ok3 := core.AsFloat(args[2])
		if !ok1 || !ok2 || !ok3 {
			return nil, errors.NewTypeError("fma", "number", "got non-number")
		}
		return core.NumberValue(math.FMA(float64(x), float64(y), float64(z))), nil
	}))

	// ulp(x): value of the least significant bit of x
	mathModule.Set("ulp", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, errors.NewTypeError("ulp", "exactly 1 argument", "got different count")
		}
		xv, ok := core.AsFloat(args[0])
		if !ok {
			return nil, errors.NewTypeError("ulp", "number", string(args[0].Type()))
		}
		x := math.Abs(float64(xv))
		switch {
		case math.IsNaN(x), math.IsInf(x, 0):
			return core.NumberValue(x), nil
		case x == 0:
			return core.NumberValue(math.SmallestNonzeroFloat64), nil
		}
		up := math.Nextafter(x, math.Inf(1))
		if math.IsInf(up, 0) {
			// x is the largest finite float; measure the downward step instead
			return core.NumberValue(x - math.Nextafter(x, math.Inf(-1))), nil
		}
		return core.NumberValue(up - x), nil
	}))

	// isqrt(n): integer square root (floor of the exact square root of n)
	mathModule.Set("isqrt", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, errors.NewTypeError("isqrt", "exactly 1 argument", "got different count")
		}
		n, err := mathNonNegInt("isqrt", args[0])
		if err != nil {
			return nil, err
		}
		r := int64(math.Sqrt(float64(n)))
		for r > 0 && r*r > n {
			r--
		}
		for (r+1)*(r+1) <= n {
			r++
		}
		return core.NumberValue(float64(r)), nil
	}))

	// isclose(a, b, *, rel_tol=1e-09, abs_tol=0.0)
	mathModule.Set("isclose", &core.BuiltinFunctionWithKwargs{
		BaseObject: *core.NewBaseObject(core.BuiltinFunctionType),
		Name:       "isclose",
		Fn: func(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) != 2 {
				return nil, errors.NewTypeError("isclose", "exactly 2 positional arguments", "got different count")
			}
			a, ok1 := core.AsFloat(args[0])
			b, ok2 := core.AsFloat(args[1])
			if !ok1 || !ok2 {
				return nil, errors.NewTypeError("isclose", "number", "got non-number")
			}
			relTol, absTol := 1e-09, 0.0
			for name, dst := range map[string]*float64{"rel_tol": &relTol, "abs_tol": &absTol} {
				if v, ok := kwargs[name]; ok {
					n, ok := core.AsFloat(v)
					if !ok {
						return nil, errors.NewTypeError("isclose", "number", name)
					}
					*dst = float64(n)
				}
			}
			if relTol < 0 || absTol < 0 {
				return nil, errors.NewValueError("isclose", "tolerances must be non-negative")
			}
			af, bf := float64(a), float64(b)
			if af == bf {
				return core.True, nil
			}
			if math.IsInf(af, 0) || math.IsInf(bf, 0) {
				return core.False, nil
			}
			diff := math.Abs(bf - af)
			result := diff <= math.Abs(relTol*bf) || diff <= math.Abs(relTol*af) || diff <= absTol
			return core.BoolValue(result), nil
		},
	})

	// comb(n, k): binomial coefficient "n choose k"
	mathModule.Set("comb", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, errors.NewTypeError("comb", "exactly 2 arguments", "got different count")
		}
		n, err := mathNonNegInt("comb", args[0])
		if err != nil {
			return nil, err
		}
		k, err := mathNonNegInt("comb", args[1])
		if err != nil {
			return nil, err
		}
		if k > n {
			return core.NumberValue(0), nil
		}
		if k > n-k {
			k = n - k
		}
		// Each partial result C(n, i) is an integer, so the division is exact.
		result := big.NewInt(1)
		tmp := new(big.Int)
		for i := int64(0); i < k; i++ {
			result.Mul(result, tmp.SetInt64(n-i))
			result.Div(result, tmp.SetInt64(i+1))
		}
		f, _ := new(big.Float).SetInt(result).Float64()
		return core.NumberValue(f), nil
	}))

	// perm(n, k=None): number of k-permutations of n; full factorial when k is None
	mathModule.Set("perm", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 || len(args) > 2 {
			return nil, errors.NewTypeError("perm", "1 or 2 arguments", "got different count")
		}
		n, err := mathNonNegInt("perm", args[0])
		if err != nil {
			return nil, err
		}
		k := n
		if len(args) == 2 && args[1] != core.None {
			if k, err = mathNonNegInt("perm", args[1]); err != nil {
				return nil, err
			}
		}
		if k > n {
			return core.NumberValue(0), nil
		}
		result := big.NewInt(1)
		tmp := new(big.Int)
		for i := int64(0); i < k; i++ {
			result.Mul(result, tmp.SetInt64(n-i))
		}
		f, _ := new(big.Float).SetInt(result).Float64()
		return core.NumberValue(f), nil
	}))

	// prod(iterable, *, start=1): product of all elements
	mathModule.Set("prod", &core.BuiltinFunctionWithKwargs{
		BaseObject: *core.NewBaseObject(core.BuiltinFunctionType),
		Name:       "prod",
		Fn: func(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) != 1 {
				return nil, errors.NewTypeError("prod", "exactly 1 positional argument", "got different count")
			}
			result := 1.0
			if v, ok := kwargs["start"]; ok {
				n, ok := core.AsFloat(v)
				if !ok {
					return nil, errors.NewTypeError("prod", "number", "start")
				}
				result = float64(n)
			}
			items, err := core.IterableValuesCtx(args[0], ctx)
			if err != nil {
				return nil, err
			}
			for _, it := range items {
				n, ok := core.AsFloat(it)
				if !ok {
					return nil, errors.NewTypeError("prod", "number", string(it.Type()))
				}
				result *= float64(n)
			}
			return core.NumberValue(result), nil
		},
	})

	// dist(p, q): Euclidean distance between two points given as coordinate sequences
	mathModule.Set("dist", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, errors.NewTypeError("dist", "exactly 2 arguments", "got different count")
		}
		p, err := core.IterableValuesCtx(args[0], ctx)
		if err != nil {
			return nil, err
		}
		q, err := core.IterableValuesCtx(args[1], ctx)
		if err != nil {
			return nil, err
		}
		if len(p) != len(q) {
			return nil, errors.NewValueError("dist", "both points must have the same number of dimensions")
		}
		var sum float64
		for i := range p {
			px, ok1 := core.AsFloat(p[i])
			qx, ok2 := core.AsFloat(q[i])
			if !ok1 || !ok2 {
				return nil, errors.NewTypeError("dist", "number", "coordinate")
			}
			d := float64(px) - float64(qx)
			sum += d * d
		}
		return core.NumberValue(math.Sqrt(sum)), nil
	}))

	// fsum(iterable): accurate floating-point sum (Shewchuk's algorithm)
	mathModule.Set("fsum", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, errors.NewTypeError("fsum", "exactly 1 argument", "got different count")
		}
		items, err := core.IterableValuesCtx(args[0], ctx)
		if err != nil {
			return nil, err
		}
		values := make([]float64, 0, len(items))
		for _, it := range items {
			n, ok := core.AsFloat(it)
			if !ok {
				return nil, errors.NewTypeError("fsum", "number", string(it.Type()))
			}
			values = append(values, float64(n))
		}
		return core.NumberValue(mathFsum(values)), nil
	}))

	// lcm(*integers): least common multiple
	mathModule.Set("lcm", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		result := int64(1)
		for _, a := range args {
			n, err := mathIntArg("lcm", a)
			if err != nil {
				return nil, err
			}
			if n < 0 {
				n = -n
			}
			if n == 0 || result == 0 {
				result = 0
				continue
			}
			result = result / gcdInt(result, n) * n
		}
		return core.NumberValue(float64(result)), nil
	}))

	// Constants
	mathModule.Set("pi", core.NumberValue(math.Pi))
	mathModule.Set("e", core.NumberValue(math.E))
	mathModule.Set("tau", core.NumberValue(2*math.Pi)) // Python 3.6+
	mathModule.Set("inf", core.NumberValue(math.Inf(1)))
	mathModule.Set("nan", core.NumberValue(math.NaN()))

	return mathModule
}

// mathIntArg extracts an integer-valued argument, rejecting non-integral floats.
func mathIntArg(fname string, v core.Value) (int64, error) {
	n, ok := core.AsFloat(v)
	if !ok {
		return 0, errors.NewTypeError(fname, "integer", string(v.Type()))
	}
	f := float64(n)
	if math.IsInf(f, 0) || math.IsNaN(f) || f != math.Trunc(f) {
		return 0, errors.NewTypeError(fname, "integer", "float")
	}
	return int64(f), nil
}

// mathNonNegInt extracts a non-negative integer argument.
func mathNonNegInt(fname string, v core.Value) (int64, error) {
	i, err := mathIntArg(fname, v)
	if err != nil {
		return 0, err
	}
	if i < 0 {
		return 0, errors.NewValueError(fname, fname+"() not defined for negative values")
	}
	return i, nil
}

// gcdInt returns the greatest common divisor of two integers.
func gcdInt(a, b int64) int64 {
	if a < 0 {
		a = -a
	}
	if b < 0 {
		b = -b
	}
	for b != 0 {
		a, b = b, a%b
	}
	return a
}

// mathFsum computes an accurately rounded sum using Shewchuk's partials
// algorithm (the same approach CPython's math.fsum uses).
func mathFsum(values []float64) float64 {
	var partials []float64
	for _, x := range values {
		i := 0
		for _, y := range partials {
			if math.Abs(x) < math.Abs(y) {
				x, y = y, x
			}
			hi := x + y
			lo := y - (hi - x)
			if lo != 0 {
				partials[i] = lo
				i++
			}
			x = hi
		}
		partials = append(partials[:i], x)
	}
	var total float64
	for _, p := range partials {
		total += p
	}
	return total
}
