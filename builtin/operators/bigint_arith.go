package operators

import (
	"math"
	"math/big"

	"github.com/mmichie/m28/core"
)

// numericOperand decomposes a value into its float64 and *big.Int views for
// mixed big-integer arithmetic. The returned *big.Int is nil when the value is
// a non-integral float (so the caller knows the result must be a float), and ok
// is false for non-numeric values (so the caller falls through to its normal
// TypeError path). Bools count as 0/1, matching Python.
func numericOperand(v core.Value) (asFloat float64, asInt *big.Int, ok bool) {
	switch x := v.(type) {
	case core.BigIntValue:
		return x.ToFloat64(), x.GetBigInt(), true
	case core.NumberValue:
		f := float64(x)
		if f == math.Trunc(f) && !math.IsInf(f, 0) && !math.IsNaN(f) {
			return f, core.PromoteToBigInt(x).GetBigInt(), true
		}
		return f, nil, true // non-integral float
	case core.FloatValue:
		// A float operand forces the whole expression to float (asInt nil).
		return float64(x), nil, true
	case core.BoolValue:
		if bool(x) {
			return 1, big.NewInt(1), true
		}
		return 0, big.NewInt(0), true
	}
	return 0, nil, false
}

// demoteBig wraps a *big.Int result, returning a plain NumberValue when it fits
// so callers see ints small enough to be ordinary numbers, matching the rest of
// M28's arithmetic.
func demoteBig(b *big.Int) core.Value {
	bigVal := core.NewBigInt(b)
	if n, ok := core.DemoteToNumber(bigVal); ok {
		return n
	}
	return bigVal
}

// pyFloorDivMod returns Python's floor division quotient and modulo for two big
// integers. Go's QuoRem truncates toward zero (remainder takes the sign of the
// dividend); Python floors (remainder takes the sign of the divisor), so adjust
// when the remainder is non-zero and its sign differs from the divisor's.
func pyFloorDivMod(a, b *big.Int) (q, r *big.Int) {
	q = new(big.Int)
	r = new(big.Int)
	q.QuoRem(a, b, r)
	if r.Sign() != 0 && (r.Sign() < 0) != (b.Sign() < 0) {
		q.Sub(q, big.NewInt(1))
		r.Add(r, b)
	}
	return q, r
}

// pyFloatMod returns Python's float modulo: like math.Mod but with the result
// taking the sign of the divisor.
func pyFloatMod(a, b float64) float64 {
	r := math.Mod(a, b)
	if r != 0 && (r < 0) != (b < 0) {
		r += b
	}
	return r
}

// bigIntArith evaluates a binary arithmetic operator when at least one operand
// is a BigIntValue. handled is false when neither operand is a big int or when
// an operand is non-numeric, so the caller falls through to its existing dunder
// and type-switch logic. A non-integral float operand promotes the whole
// expression to float (Python's int/float rule); otherwise the operation runs
// in arbitrary precision and demotes back to a NumberValue when it fits.
func bigIntArith(op string, left, right core.Value) (core.Value, bool, error) {
	_, lBig := left.(core.BigIntValue)
	_, rBig := right.(core.BigIntValue)
	if !lBig && !rBig {
		return nil, false, nil
	}
	lf, la, lOK := numericOperand(left)
	rf, ra, rOK := numericOperand(right)
	if !lOK || !rOK {
		return nil, false, nil // non-numeric operand: let the caller raise TypeError
	}
	useFloat := la == nil || ra == nil

	switch op {
	case "+":
		if useFloat {
			return core.FloatValue(lf + rf), true, nil
		}
		return demoteBig(new(big.Int).Add(la, ra)), true, nil
	case "-":
		if useFloat {
			return core.FloatValue(lf - rf), true, nil
		}
		return demoteBig(new(big.Int).Sub(la, ra)), true, nil
	case "*":
		if useFloat {
			return core.FloatValue(lf * rf), true, nil
		}
		return demoteBig(new(big.Int).Mul(la, ra)), true, nil
	case "/":
		// True division always yields a float in Python.
		if useFloat {
			if rf == 0 {
				return nil, true, core.NewZeroDivisionError()
			}
			return core.FloatValue(lf / rf), true, nil
		}
		if ra.Sign() == 0 {
			return nil, true, core.NewZeroDivisionError()
		}
		quo, _ := new(big.Float).Quo(new(big.Float).SetInt(la), new(big.Float).SetInt(ra)).Float64()
		return core.FloatValue(quo), true, nil
	case "//":
		if useFloat {
			if rf == 0 {
				return nil, true, core.NewZeroDivisionError()
			}
			return core.FloatValue(math.Floor(lf / rf)), true, nil
		}
		if ra.Sign() == 0 {
			return nil, true, core.NewZeroDivisionError()
		}
		quo, _ := pyFloorDivMod(la, ra)
		return demoteBig(quo), true, nil
	case "%":
		if useFloat {
			if rf == 0 {
				return nil, true, core.NewZeroDivisionError()
			}
			return core.FloatValue(pyFloatMod(lf, rf)), true, nil
		}
		if ra.Sign() == 0 {
			return nil, true, core.NewZeroDivisionError()
		}
		_, rem := pyFloorDivMod(la, ra)
		return demoteBig(rem), true, nil
	}
	return nil, false, nil
}
