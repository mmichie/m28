package operators

import (
	"math"
	"testing"

	"github.com/mmichie/m28/core"
)

func TestAddBasic(t *testing.T) {
	ctx := core.NewContext(nil)

	tests := []struct {
		name     string
		args     []core.Value
		expected core.Value
		wantErr  bool
	}{
		{
			name:     "add two integers",
			args:     []core.Value{core.NumberValue(5), core.NumberValue(3)},
			expected: core.NumberValue(8),
		},
		{
			name:     "add multiple integers",
			args:     []core.Value{core.NumberValue(1), core.NumberValue(2), core.NumberValue(3)},
			expected: core.NumberValue(6),
		},
		{
			name:     "add floats",
			args:     []core.Value{core.NumberValue(1.5), core.NumberValue(2.5)},
			expected: core.NumberValue(4.0),
		},
		{
			name:     "add number and bool (True)",
			args:     []core.Value{core.NumberValue(5), core.True},
			expected: core.NumberValue(6),
		},
		{
			name:     "add number and bool (False)",
			args:     []core.Value{core.NumberValue(5), core.False},
			expected: core.NumberValue(5),
		},
		{
			name:     "add strings",
			args:     []core.Value{core.StringValue("hello"), core.StringValue(" world")},
			expected: core.StringValue("hello world"),
		},
		{
			name:     "add lists",
			args:     []core.Value{core.NewList(core.NumberValue(1)), core.NewList(core.NumberValue(2))},
			expected: core.NewList(core.NumberValue(1), core.NumberValue(2)),
		},
		{
			name:     "unary plus on number",
			args:     []core.Value{core.NumberValue(5)},
			expected: core.NumberValue(5),
		},
		{
			name:     "unary plus on negative number",
			args:     []core.Value{core.NumberValue(-5)},
			expected: core.NumberValue(-5),
		},
		{
			name:     "add no arguments",
			args:     []core.Value{},
			expected: core.NumberValue(0),
		},
		{
			name:     "add complex numbers",
			args:     []core.Value{core.ComplexValue(complex(1, 2)), core.ComplexValue(complex(3, 4))},
			expected: core.ComplexValue(complex(4, 6)),
		},
		{
			name:     "add number and complex",
			args:     []core.Value{core.NumberValue(5), core.ComplexValue(complex(3, 4))},
			expected: core.ComplexValue(complex(8, 4)),
		},
	}

	addFunc := Add()
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := addFunc(tt.args, ctx)
			if (err != nil) != tt.wantErr {
				t.Errorf("Add() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr && !valuesEqual(result, tt.expected) {
				t.Errorf("Add() = %v, want %v", result, tt.expected)
			}
		})
	}
}

func TestSubtractBasic(t *testing.T) {
	ctx := core.NewContext(nil)

	tests := []struct {
		name     string
		args     []core.Value
		expected core.Value
		wantErr  bool
	}{
		{
			name:     "subtract two integers",
			args:     []core.Value{core.NumberValue(10), core.NumberValue(3)},
			expected: core.NumberValue(7),
		},
		{
			name:     "subtract multiple integers",
			args:     []core.Value{core.NumberValue(10), core.NumberValue(2), core.NumberValue(3)},
			expected: core.NumberValue(5),
		},
		{
			name:     "subtract floats",
			args:     []core.Value{core.NumberValue(5.5), core.NumberValue(2.5)},
			expected: core.NumberValue(3.0),
		},
		{
			name:     "unary minus on positive",
			args:     []core.Value{core.NumberValue(5)},
			expected: core.NumberValue(-5),
		},
		{
			name:     "unary minus on negative",
			args:     []core.Value{core.NumberValue(-5)},
			expected: core.NumberValue(5),
		},
		{
			name:     "subtract with bool",
			args:     []core.Value{core.NumberValue(5), core.True},
			expected: core.NumberValue(4),
		},
		{
			name:     "subtract complex numbers",
			args:     []core.Value{core.ComplexValue(complex(5, 3)), core.ComplexValue(complex(2, 1))},
			expected: core.ComplexValue(complex(3, 2)),
		},
		{
			name:    "subtract no arguments",
			args:    []core.Value{},
			wantErr: true,
		},
	}

	subtractFunc := Subtract()
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := subtractFunc(tt.args, ctx)
			if (err != nil) != tt.wantErr {
				t.Errorf("Subtract() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr && !valuesEqual(result, tt.expected) {
				t.Errorf("Subtract() = %v, want %v", result, tt.expected)
			}
		})
	}
}

func TestMultiplyBasic(t *testing.T) {
	ctx := core.NewContext(nil)

	tests := []struct {
		name     string
		args     []core.Value
		expected core.Value
		wantErr  bool
	}{
		{
			name:     "multiply two integers",
			args:     []core.Value{core.NumberValue(5), core.NumberValue(3)},
			expected: core.NumberValue(15),
		},
		{
			name:     "multiply multiple integers",
			args:     []core.Value{core.NumberValue(2), core.NumberValue(3), core.NumberValue(4)},
			expected: core.NumberValue(24),
		},
		{
			name:     "multiply floats",
			args:     []core.Value{core.NumberValue(2.5), core.NumberValue(4.0)},
			expected: core.NumberValue(10.0),
		},
		{
			name:     "multiply by zero",
			args:     []core.Value{core.NumberValue(5), core.NumberValue(0)},
			expected: core.NumberValue(0),
		},
		{
			name:     "multiply string by number",
			args:     []core.Value{core.StringValue("ab"), core.NumberValue(3)},
			expected: core.StringValue("ababab"),
		},
		{
			name:     "multiply list by number",
			args:     []core.Value{core.NewList(core.NumberValue(1), core.NumberValue(2)), core.NumberValue(2)},
			expected: core.NewList(core.NumberValue(1), core.NumberValue(2), core.NumberValue(1), core.NumberValue(2)),
		},
		{
			name:     "multiply with bool",
			args:     []core.Value{core.NumberValue(5), core.True},
			expected: core.NumberValue(5),
		},
		{
			name:     "multiply complex numbers",
			args:     []core.Value{core.ComplexValue(complex(2, 3)), core.ComplexValue(complex(1, 2))},
			expected: core.ComplexValue(complex(-4, 7)), // (2+3i)*(1+2i) = 2+4i+3i-6 = -4+7i
		},
		{
			name:     "multiply no arguments",
			args:     []core.Value{},
			expected: core.NumberValue(1),
		},
	}

	multiplyFunc := Multiply()
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := multiplyFunc(tt.args, ctx)
			if (err != nil) != tt.wantErr {
				t.Errorf("Multiply() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr && !valuesEqual(result, tt.expected) {
				t.Errorf("Multiply() = %v, want %v", result, tt.expected)
			}
		})
	}
}

func TestDivideBasic(t *testing.T) {
	ctx := core.NewContext(nil)

	tests := []struct {
		name     string
		args     []core.Value
		expected core.Value
		wantErr  bool
	}{
		{
			name:     "divide two integers",
			args:     []core.Value{core.NumberValue(10), core.NumberValue(2)},
			expected: core.NumberValue(5),
		},
		{
			name:     "divide floats",
			args:     []core.Value{core.NumberValue(7.5), core.NumberValue(2.5)},
			expected: core.NumberValue(3.0),
		},
		{
			name:     "divide with float result",
			args:     []core.Value{core.NumberValue(7), core.NumberValue(2)},
			expected: core.NumberValue(3.5),
		},
		{
			name:     "divide complex numbers",
			args:     []core.Value{core.ComplexValue(complex(4, 2)), core.ComplexValue(complex(2, 0))},
			expected: core.ComplexValue(complex(2, 1)),
		},
		{
			name:    "divide by zero",
			args:    []core.Value{core.NumberValue(5), core.NumberValue(0)},
			wantErr: true,
		},
		{
			name:    "divide with no arguments",
			args:    []core.Value{},
			wantErr: true,
		},
	}

	divideFunc := Divide()
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := divideFunc(tt.args, ctx)
			if (err != nil) != tt.wantErr {
				t.Errorf("Divide() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr {
				if !valuesApproxEqual(result, tt.expected, 1e-10) {
					t.Errorf("Divide() = %v, want %v", result, tt.expected)
				}
			}
		})
	}
}

func TestFloorDivideBasic(t *testing.T) {
	ctx := core.NewContext(nil)

	tests := []struct {
		name     string
		args     []core.Value
		expected core.Value
		wantErr  bool
	}{
		{
			name:     "floor divide integers evenly",
			args:     []core.Value{core.NumberValue(10), core.NumberValue(2)},
			expected: core.NumberValue(5),
		},
		{
			name:     "floor divide with remainder",
			args:     []core.Value{core.NumberValue(7), core.NumberValue(2)},
			expected: core.NumberValue(3),
		},
		{
			name:     "floor divide negative",
			args:     []core.Value{core.NumberValue(-7), core.NumberValue(2)},
			expected: core.NumberValue(-4),
		},
		{
			name:     "floor divide floats",
			args:     []core.Value{core.NumberValue(7.5), core.NumberValue(2.0)},
			expected: core.NumberValue(3),
		},
		{
			name:    "floor divide by zero",
			args:    []core.Value{core.NumberValue(5), core.NumberValue(0)},
			wantErr: true,
		},
	}

	floorDivideFunc := FloorDivide()
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := floorDivideFunc(tt.args, ctx)
			if (err != nil) != tt.wantErr {
				t.Errorf("FloorDivide() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr && !valuesEqual(result, tt.expected) {
				t.Errorf("FloorDivide() = %v, want %v", result, tt.expected)
			}
		})
	}
}

func TestModuloBasic(t *testing.T) {
	ctx := core.NewContext(nil)

	tests := []struct {
		name     string
		args     []core.Value
		expected core.Value
		wantErr  bool
	}{
		{
			name:     "modulo basic",
			args:     []core.Value{core.NumberValue(10), core.NumberValue(3)},
			expected: core.NumberValue(1),
		},
		{
			name:     "modulo evenly divisible",
			args:     []core.Value{core.NumberValue(10), core.NumberValue(2)},
			expected: core.NumberValue(0),
		},
		{
			name:     "modulo negative dividend",
			args:     []core.Value{core.NumberValue(-10), core.NumberValue(3)},
			expected: core.NumberValue(2), // Python's floored modulo: -10 = 3 * (-4) + 2
		},
		{
			name:     "modulo negative divisor",
			args:     []core.Value{core.NumberValue(10), core.NumberValue(-3)},
			expected: core.NumberValue(-2), // Python's floored modulo: 10 = -3 * (-4) + (-2)
		},
		{
			name:     "modulo floats",
			args:     []core.Value{core.NumberValue(7.5), core.NumberValue(2.0)},
			expected: core.NumberValue(1.5),
		},
		{
			name:    "modulo by zero",
			args:    []core.Value{core.NumberValue(5), core.NumberValue(0)},
			wantErr: true,
		},
	}

	moduloFunc := Modulo()
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := moduloFunc(tt.args, ctx)
			if (err != nil) != tt.wantErr {
				t.Errorf("Modulo() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr {
				if !valuesApproxEqual(result, tt.expected, 1e-10) {
					t.Errorf("Modulo() = %v, want %v", result, tt.expected)
				}
			}
		})
	}
}

func TestPowerBasic(t *testing.T) {
	ctx := core.NewContext(nil)

	tests := []struct {
		name     string
		args     []core.Value
		expected core.Value
		wantErr  bool
	}{
		{
			name:     "power basic",
			args:     []core.Value{core.NumberValue(2), core.NumberValue(3)},
			expected: core.NumberValue(8),
		},
		{
			name:     "power to zero",
			args:     []core.Value{core.NumberValue(5), core.NumberValue(0)},
			expected: core.NumberValue(1),
		},
		{
			name:     "power negative exponent",
			args:     []core.Value{core.NumberValue(2), core.NumberValue(-2)},
			expected: core.NumberValue(0.25),
		},
		{
			name:     "power fractional exponent",
			args:     []core.Value{core.NumberValue(4), core.NumberValue(0.5)},
			expected: core.NumberValue(2),
		},
		{
			name:     "power with float base",
			args:     []core.Value{core.NumberValue(2.5), core.NumberValue(2)},
			expected: core.NumberValue(6.25),
		},
		{
			name:    "power with too few arguments",
			args:    []core.Value{core.NumberValue(2)},
			wantErr: true,
		},
	}

	powerFunc := Power()
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := powerFunc(tt.args, ctx)
			if (err != nil) != tt.wantErr {
				t.Errorf("Power() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr {
				if !valuesApproxEqual(result, tt.expected, 1e-10) {
					t.Errorf("Power() = %v, want %v", result, tt.expected)
				}
			}
		})
	}
}

func TestMatMulBasic(t *testing.T) {
	ctx := core.NewContext(nil)

	matmulFunc := MatMul()

	// Test that matmul requires at least 2 arguments
	_, err := matmulFunc([]core.Value{core.NumberValue(5)}, ctx)
	if err == nil {
		t.Error("MatMul() should error with < 2 arguments")
	}
}

// Helper functions for test comparison

func valuesEqual(a, b core.Value) bool {
	// Handle nil cases
	if a == nil && b == nil {
		return true
	}
	if a == nil || b == nil {
		return false
	}

	// Compare by type
	switch av := a.(type) {
	case core.NumberValue:
		bv, ok := b.(core.NumberValue)
		if !ok {
			return false
		}
		return float64(av) == float64(bv)
	case core.StringValue:
		bv, ok := b.(core.StringValue)
		if !ok {
			return false
		}
		return string(av) == string(bv)
	case core.BoolValue:
		bv, ok := b.(core.BoolValue)
		if !ok {
			return false
		}
		return bool(av) == bool(bv)
	case core.ComplexValue:
		bv, ok := b.(core.ComplexValue)
		if !ok {
			return false
		}
		return complex128(av) == complex128(bv)
	case *core.ListValue:
		bv, ok := b.(*core.ListValue)
		if !ok {
			return false
		}
		if av.Len() != bv.Len() {
			return false
		}
		avItems := av.Items()
		bvItems := bv.Items()
		for i := 0; i < av.Len(); i++ {
			if !valuesEqual(avItems[i], bvItems[i]) {
				return false
			}
		}
		return true
	default:
		// For other types, use pointer equality
		return a == b
	}
}

func valuesApproxEqual(a, b core.Value, epsilon float64) bool {
	// Handle numbers with approximate comparison
	av, aOk := a.(core.NumberValue)
	bv, bOk := b.(core.NumberValue)
	if aOk && bOk {
		return math.Abs(float64(av)-float64(bv)) < epsilon
	}

	// Handle complex numbers with approximate comparison
	ac, aOk := a.(core.ComplexValue)
	bc, bOk := b.(core.ComplexValue)
	if aOk && bOk {
		diff := complex128(ac) - complex128(bc)
		return math.Abs(real(diff)) < epsilon && math.Abs(imag(diff)) < epsilon
	}

	// For other types, use exact equality
	return valuesEqual(a, b)
}
