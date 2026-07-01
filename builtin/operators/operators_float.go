package operators

import (
	"math"

	"github.com/mmichie/m28/core"
)

// isFixedReal reports whether v is a non-bigint real number (int NumberValue,
// float, or bool) — the operands the float/int fast paths handle directly.
// BigIntValue is deliberately excluded so those cases fall to bigIntArith, which
// preserves arbitrary precision.
func isFixedReal(v core.Value) bool {
	switch v.(type) {
	case core.NumberValue, core.FloatValue, core.BoolValue:
		return true
	}
	return false
}

// floatBinaryFast evaluates a binary arithmetic op when at least one operand is
// a Python float and both operands are real numbers. The result is always a
// FloatValue and overflow yields inf (never a big int), matching CPython's
// int/float promotion. handled is false when no float is involved or an operand
// is non-numeric, so the caller falls through to its int / dunder / sequence
// logic.
func floatBinaryFast(op string, left, right core.Value) (core.Value, bool, error) {
	if !core.IsFloatValue(left) && !core.IsFloatValue(right) {
		return nil, false, nil
	}
	lf, lok := core.AsFloat(left)
	rf, rok := core.AsFloat(right)
	if !lok || !rok {
		return nil, false, nil // non-numeric operand (e.g. float * str): not our case
	}
	switch op {
	case "+":
		return core.FloatValue(lf + rf), true, nil
	case "-":
		return core.FloatValue(lf - rf), true, nil
	case "*":
		return core.FloatValue(lf * rf), true, nil
	case "/":
		if rf == 0 {
			return nil, true, core.NewZeroDivisionError()
		}
		return core.FloatValue(lf / rf), true, nil
	case "//":
		if rf == 0 {
			return nil, true, core.NewZeroDivisionError()
		}
		return core.FloatValue(math.Floor(lf / rf)), true, nil
	case "%":
		if rf == 0 {
			return nil, true, core.NewZeroDivisionError()
		}
		return core.FloatValue(pyFloatMod(lf, rf)), true, nil
	}
	return nil, false, nil
}
