package operators

import (
	"testing"

	"github.com/mmichie/m28/core"
)

func TestLeftShiftBasic(t *testing.T) {
	ctx := core.NewContext(nil)

	tests := []struct {
		name     string
		args     []core.Value
		expected core.Value
		wantErr  bool
	}{
		{
			name:     "left shift basic",
			args:     []core.Value{core.NumberValue(5), core.NumberValue(2)},
			expected: core.NumberValue(20), // 5 << 2 = 20
		},
		{
			name:     "left shift by zero",
			args:     []core.Value{core.NumberValue(5), core.NumberValue(0)},
			expected: core.NumberValue(5),
		},
		{
			name:     "left shift one",
			args:     []core.Value{core.NumberValue(1), core.NumberValue(3)},
			expected: core.NumberValue(8), // 1 << 3 = 8
		},
		{
			name:    "left shift with too few arguments",
			args:    []core.Value{core.NumberValue(5)},
			wantErr: true,
		},
	}

	leftShiftFunc := LeftShift()
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := leftShiftFunc(tt.args, ctx)
			if (err != nil) != tt.wantErr {
				t.Errorf("LeftShift() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr && !valuesEqual(result, tt.expected) {
				t.Errorf("LeftShift() = %v, want %v", result, tt.expected)
			}
		})
	}
}

func TestRightShiftBasic(t *testing.T) {
	ctx := core.NewContext(nil)

	tests := []struct {
		name     string
		args     []core.Value
		expected core.Value
		wantErr  bool
	}{
		{
			name:     "right shift basic",
			args:     []core.Value{core.NumberValue(20), core.NumberValue(2)},
			expected: core.NumberValue(5), // 20 >> 2 = 5
		},
		{
			name:     "right shift by zero",
			args:     []core.Value{core.NumberValue(5), core.NumberValue(0)},
			expected: core.NumberValue(5),
		},
		{
			name:     "right shift to zero",
			args:     []core.Value{core.NumberValue(3), core.NumberValue(3)},
			expected: core.NumberValue(0), // 3 >> 3 = 0
		},
		{
			name:    "right shift with too few arguments",
			args:    []core.Value{core.NumberValue(5)},
			wantErr: true,
		},
	}

	rightShiftFunc := RightShift()
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := rightShiftFunc(tt.args, ctx)
			if (err != nil) != tt.wantErr {
				t.Errorf("RightShift() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr && !valuesEqual(result, tt.expected) {
				t.Errorf("RightShift() = %v, want %v", result, tt.expected)
			}
		})
	}
}

func TestBitwiseAndBasic(t *testing.T) {
	ctx := core.NewContext(nil)

	tests := []struct {
		name     string
		args     []core.Value
		expected core.Value
		wantErr  bool
	}{
		{
			name:     "bitwise and basic",
			args:     []core.Value{core.NumberValue(12), core.NumberValue(10)},
			expected: core.NumberValue(8), // 1100 & 1010 = 1000
		},
		{
			name:     "bitwise and with zero",
			args:     []core.Value{core.NumberValue(15), core.NumberValue(0)},
			expected: core.NumberValue(0),
		},
		{
			name:     "bitwise and all ones",
			args:     []core.Value{core.NumberValue(7), core.NumberValue(7)},
			expected: core.NumberValue(7),
		},
		{
			name:    "bitwise and with too few arguments",
			args:    []core.Value{core.NumberValue(5)},
			wantErr: true,
		},
	}

	bitwiseAndFunc := BitwiseAnd()
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := bitwiseAndFunc(tt.args, ctx)
			if (err != nil) != tt.wantErr {
				t.Errorf("BitwiseAnd() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr && !valuesEqual(result, tt.expected) {
				t.Errorf("BitwiseAnd() = %v, want %v", result, tt.expected)
			}
		})
	}
}

func TestBitwiseOrBasic(t *testing.T) {
	ctx := core.NewContext(nil)

	tests := []struct {
		name     string
		args     []core.Value
		expected core.Value
		wantErr  bool
	}{
		{
			name:     "bitwise or basic",
			args:     []core.Value{core.NumberValue(12), core.NumberValue(10)},
			expected: core.NumberValue(14), // 1100 | 1010 = 1110
		},
		{
			name:     "bitwise or with zero",
			args:     []core.Value{core.NumberValue(15), core.NumberValue(0)},
			expected: core.NumberValue(15),
		},
		{
			name:     "bitwise or all ones",
			args:     []core.Value{core.NumberValue(7), core.NumberValue(7)},
			expected: core.NumberValue(7),
		},
		{
			name:    "bitwise or with too few arguments",
			args:    []core.Value{core.NumberValue(5)},
			wantErr: true,
		},
	}

	bitwiseOrFunc := BitwiseOr()
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := bitwiseOrFunc(tt.args, ctx)
			if (err != nil) != tt.wantErr {
				t.Errorf("BitwiseOr() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr && !valuesEqual(result, tt.expected) {
				t.Errorf("BitwiseOr() = %v, want %v", result, tt.expected)
			}
		})
	}
}

func TestBitwiseXorBasic(t *testing.T) {
	ctx := core.NewContext(nil)

	tests := []struct {
		name     string
		args     []core.Value
		expected core.Value
		wantErr  bool
	}{
		{
			name:     "bitwise xor basic",
			args:     []core.Value{core.NumberValue(12), core.NumberValue(10)},
			expected: core.NumberValue(6), // 1100 ^ 1010 = 0110
		},
		{
			name:     "bitwise xor with zero",
			args:     []core.Value{core.NumberValue(15), core.NumberValue(0)},
			expected: core.NumberValue(15),
		},
		{
			name:     "bitwise xor same value",
			args:     []core.Value{core.NumberValue(7), core.NumberValue(7)},
			expected: core.NumberValue(0),
		},
		{
			name:    "bitwise xor with too few arguments",
			args:    []core.Value{core.NumberValue(5)},
			wantErr: true,
		},
	}

	bitwiseXorFunc := BitwiseXor()
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := bitwiseXorFunc(tt.args, ctx)
			if (err != nil) != tt.wantErr {
				t.Errorf("BitwiseXor() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr && !valuesEqual(result, tt.expected) {
				t.Errorf("BitwiseXor() = %v, want %v", result, tt.expected)
			}
		})
	}
}

func TestBitwiseInvertBasic(t *testing.T) {
	ctx := core.NewContext(nil)

	tests := []struct {
		name     string
		args     []core.Value
		expected core.Value
		wantErr  bool
	}{
		{
			name:     "bitwise invert zero",
			args:     []core.Value{core.NumberValue(0)},
			expected: core.NumberValue(-1),
		},
		{
			name:     "bitwise invert positive",
			args:     []core.Value{core.NumberValue(5)},
			expected: core.NumberValue(-6), // ~5 = -6 in two's complement
		},
		{
			name:     "bitwise invert negative",
			args:     []core.Value{core.NumberValue(-1)},
			expected: core.NumberValue(0),
		},
		{
			name:     "bitwise invert -6",
			args:     []core.Value{core.NumberValue(-6)},
			expected: core.NumberValue(5),
		},
		{
			name:    "bitwise invert with no arguments",
			args:    []core.Value{},
			wantErr: true,
		},
	}

	bitwiseInvertFunc := BitwiseInvert()
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := bitwiseInvertFunc(tt.args, ctx)
			if (err != nil) != tt.wantErr {
				t.Errorf("BitwiseInvert() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr && !valuesEqual(result, tt.expected) {
				t.Errorf("BitwiseInvert() = %v, want %v", result, tt.expected)
			}
		})
	}
}
