package core

import (
	"math"
	"strconv"
	"testing"
)

// TestPyFloatRepr verifies FloatValue formatting matches CPython's repr(float)
// exactly, including the scientific-notation thresholds and forced ".0".
func TestPyFloatRepr(t *testing.T) {
	cases := []struct {
		in   string // parsed with strconv.ParseFloat
		want string // CPython repr(float(in))
	}{
		{"1.0", "1.0"},
		{"0.5", "0.5"},
		{"100.0", "100.0"},
		{"-0.0", "-0.0"},
		{"0.1", "0.1"},
		{"0.2", "0.2"},
		{"3.14", "3.14"},
		{"1e16", "1e+16"},
		{"1e15", "1000000000000000.0"},
		{"1e17", "1e+17"},
		{"1.5e-8", "1.5e-08"},
		{"1e-4", "0.0001"},
		{"1e-5", "1e-05"},
		{"1234567890123456.0", "1234567890123456.0"},
		{"12345678901234567.0", "1.2345678901234568e+16"},
		{"1e300", "1e+300"},
		{"-2.5", "-2.5"},
		{"0.0", "0.0"},
		{"123.456", "123.456"},
		{"2.0", "2.0"},
		{"1e21", "1e+21"},
		{"1e22", "1e+22"},
		{"9999999999999998.0", "9999999999999998.0"},
		{"0.0001", "0.0001"},
		{"0.00001", "1e-05"},
		{"1234.5", "1234.5"},
		{"-inf", "-inf"},
		{"inf", "inf"},
		{"nan", "nan"},
	}
	for _, c := range cases {
		f, err := strconv.ParseFloat(c.in, 64)
		if err != nil {
			t.Fatalf("bad test input %q: %v", c.in, err)
		}
		if got := PyFloatRepr(f); got != c.want {
			t.Errorf("PyFloatRepr(%s) = %q, want %q", c.in, got, c.want)
		}
	}
}

// TestPyFloatReprRoundTrip ensures every formatted float parses back to the
// same bits (the defining property of CPython's shortest-repr algorithm).
func TestPyFloatReprRoundTrip(t *testing.T) {
	vals := []float64{
		0, 1, -1, 0.1, 3.141592653589793, 2.718281828459045,
		1e-300, 1e300, math.MaxFloat64, math.SmallestNonzeroFloat64,
		123456.789, -987654.321, 1.0 / 3.0,
	}
	for _, v := range vals {
		s := PyFloatRepr(v)
		back, err := strconv.ParseFloat(s, 64)
		if err != nil {
			t.Errorf("PyFloatRepr(%v) = %q does not parse: %v", v, s, err)
			continue
		}
		if back != v {
			t.Errorf("round-trip failed: %v -> %q -> %v", v, s, back)
		}
	}
}
