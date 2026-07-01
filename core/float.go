package core

import (
	"math"
	"math/big"
	"strconv"
	"strings"
)

// FloatValue represents a Python float (IEEE 754 double). It is a distinct type
// from NumberValue/BigIntValue (which are Python's int) so that type(), repr,
// isinstance, and arithmetic promotion follow CPython semantics: a float stays a
// float even when its value is integral (e.g. 1.0, 4/2), and float overflow
// yields inf rather than promoting to an arbitrary-precision integer.
type FloatValue float64

// Type implements Value.Type
func (f FloatValue) Type() Type {
	return FloatType
}

// String implements Value.String using CPython's float repr (str(float) ==
// repr(float) since Python 3.1).
func (f FloatValue) String() string {
	return PyFloatRepr(float64(f))
}

// GetAttr resolves float methods. Float and int currently share the numeric type
// descriptor; identity (type()/isinstance) is handled separately by Go type.
func (f FloatValue) GetAttr(name string) (Value, bool) {
	if desc := GetTypeDescriptor(FloatType); desc != nil {
		if val, err := desc.GetAttribute(f, name); err == nil {
			return val, true
		}
	}
	if desc := GetTypeDescriptor(NumberType); desc != nil {
		if val, err := desc.GetAttribute(f, name); err == nil {
			return val, true
		}
	}
	return nil, false
}

// PyFloatRepr formats a float64 exactly as CPython's repr(float) does: the
// shortest decimal string that round-trips, always visibly a float (an integral
// value like 1.0 renders as "1.0", never "1"), with scientific notation chosen
// on the same thresholds CPython uses (decimal point position <= -4 or > 16).
func PyFloatRepr(f float64) string {
	switch {
	case math.IsInf(f, 1):
		return "inf"
	case math.IsInf(f, -1):
		return "-inf"
	case math.IsNaN(f):
		return "nan"
	}

	// Shortest round-trip in scientific form: "d[.ddd]e±XX" (Go guarantees the
	// minimum digits that parse back to f, with no trailing fractional zeros).
	s := strconv.FormatFloat(f, 'e', -1, 64)

	neg := false
	if s[0] == '-' {
		neg = true
		s = s[1:]
	}
	eIdx := strings.IndexByte(s, 'e')
	mant := s[:eIdx]
	exp, _ := strconv.Atoi(s[eIdx+1:])

	// digits: mantissa with the decimal point removed.
	digits := mant
	if dot := strings.IndexByte(mant, '.'); dot >= 0 {
		digits = mant[:dot] + mant[dot+1:]
	}
	// decpt is the position of the decimal point relative to the start of
	// digits (value == 0.<digits> * 10^decpt). In the "D.DDDe±exp" form the
	// point sits after the first digit, so decpt = exp + 1.
	decpt := exp + 1

	var out string
	if decpt <= -4 || decpt > 16 {
		// Scientific notation.
		var b strings.Builder
		b.WriteByte(digits[0])
		if len(digits) > 1 {
			b.WriteByte('.')
			b.WriteString(digits[1:])
		}
		b.WriteByte('e')
		e := decpt - 1
		if e >= 0 {
			b.WriteByte('+')
		} else {
			b.WriteByte('-')
			e = -e
		}
		es := strconv.Itoa(e)
		if len(es) < 2 {
			b.WriteByte('0')
		}
		b.WriteString(es)
		out = b.String()
	} else if decpt <= 0 {
		out = "0." + strings.Repeat("0", -decpt) + digits
	} else if decpt >= len(digits) {
		// Integral value: pad with zeros and add ".0" so it reads as a float.
		out = digits + strings.Repeat("0", decpt-len(digits)) + ".0"
	} else {
		out = digits[:decpt] + "." + digits[decpt:]
	}

	if neg {
		return "-" + out
	}
	return out
}

// AsFloat returns the float64 view of any real numeric value (int NumberValue,
// FloatValue, BigIntValue, or bool), and whether v was numeric. It is the
// canonical way to read a number's magnitude regardless of int/float identity.
func AsFloat(v Value) (float64, bool) {
	switch n := v.(type) {
	case NumberValue:
		return float64(n), true
	case FloatValue:
		return float64(n), true
	case BigIntValue:
		return n.ToFloat64(), true
	case BoolValue:
		if bool(n) {
			return 1, true
		}
		return 0, true
	}
	return 0, false
}

// IsFloatValue reports whether v is a Python float.
func IsFloatValue(v Value) bool {
	_, ok := v.(FloatValue)
	return ok
}

// floatEqualsBigInt reports whether a float exactly equals an arbitrary-precision
// integer, matching CPython's lossless int/float comparison: a non-integral or
// non-finite float never equals an integer; otherwise both sides are compared as
// exact rationals (here, as big.Floats since f is an integral value).
func floatEqualsBigInt(f float64, b BigIntValue) bool {
	if math.IsInf(f, 0) || math.IsNaN(f) || f != math.Trunc(f) {
		return false
	}
	bf := new(big.Float).SetFloat64(f)
	bi := new(big.Float).SetInt(b.GetBigInt())
	return bf.Cmp(bi) == 0
}
