package builders_test

import (
	"errors"
	"math"
	"strings"
	"testing"

	"github.com/mmichie/m28/common/builders"
	"github.com/mmichie/m28/core"
)

// Test helpers
func num(n float64) core.Value             { return core.NumberValue(n) }
func str(s string) core.Value              { return core.StringValue(s) }
func list(values ...core.Value) core.Value { return core.NewList(values...) }

func TestUnaryNumber(t *testing.T) {
	// Test simple unary number function
	abs := builders.UnaryNumberSimple("abs", math.Abs)

	tests := []struct {
		name    string
		args    []core.Value
		want    float64
		wantErr bool
		errMsg  string
	}{
		{
			name:    "positive number",
			args:    []core.Value{num(42)},
			want:    42,
			wantErr: false,
		},
		{
			name:    "negative number",
			args:    []core.Value{num(-42)},
			want:    42,
			wantErr: false,
		},
		{
			name:    "zero",
			args:    []core.Value{num(0)},
			want:    0,
			wantErr: false,
		},
		{
			name:    "wrong type",
			args:    []core.Value{str("42")},
			want:    0,
			wantErr: true,
			errMsg:  "TypeError: abs: expected number, got string",
		},
		{
			name:    "too many args",
			args:    []core.Value{num(42), num(10)},
			want:    0,
			wantErr: true,
			errMsg:  "ArgumentError: abs: takes exactly 1 argument (2 given)",
		},
		{
			name:    "no args",
			args:    []core.Value{},
			want:    0,
			wantErr: true,
			errMsg:  "ArgumentError: abs: takes exactly 1 argument (0 given)",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := abs(tt.args, nil)

			if (err != nil) != tt.wantErr {
				t.Errorf("UnaryNumber() error = %v, wantErr %v", err, tt.wantErr)
			}

			if err != nil {
				if tt.errMsg != "" && err.Error() != tt.errMsg {
					t.Errorf("UnaryNumber() error = %v, want %v", err.Error(), tt.errMsg)
				}
			} else {
				if n, ok := got.(core.NumberValue); ok {
					if float64(n) != tt.want {
						t.Errorf("UnaryNumber() = %v, want %v", float64(n), tt.want)
					}
				} else {
					t.Errorf("UnaryNumber() returned non-number: %v", got)
				}
			}
		})
	}
}

func TestUnaryNumberWithError(t *testing.T) {
	// Test unary number function that can return errors
	sqrt := builders.UnaryNumber("sqrt", func(n float64) (float64, error) {
		if n < 0 {
			return 0, errors.New("math domain error")
		}
		return math.Sqrt(n), nil
	})

	tests := []struct {
		name    string
		args    []core.Value
		want    float64
		wantErr bool
		errMsg  string
	}{
		{
			name:    "positive number",
			args:    []core.Value{num(4)},
			want:    2,
			wantErr: false,
		},
		{
			name:    "negative number",
			args:    []core.Value{num(-4)},
			want:    0,
			wantErr: true,
			errMsg:  "RuntimeError: sqrt: math domain error",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := sqrt(tt.args, nil)

			if (err != nil) != tt.wantErr {
				t.Errorf("UnaryNumber() error = %v, wantErr %v", err, tt.wantErr)
			}

			if err != nil {
				if tt.errMsg != "" && err.Error() != tt.errMsg {
					t.Errorf("UnaryNumber() error = %v, want %v", err.Error(), tt.errMsg)
				}
			} else {
				if n, ok := got.(core.NumberValue); ok {
					if float64(n) != tt.want {
						t.Errorf("UnaryNumber() = %v, want %v", float64(n), tt.want)
					}
				}
			}
		})
	}
}

func TestBinaryNumber(t *testing.T) {
	// Test binary number function
	pow := builders.BinaryNumberSimple("pow", math.Pow)

	tests := []struct {
		name    string
		args    []core.Value
		want    float64
		wantErr bool
		errMsg  string
	}{
		{
			name:    "2^3",
			args:    []core.Value{num(2), num(3)},
			want:    8,
			wantErr: false,
		},
		{
			name:    "negative exponent",
			args:    []core.Value{num(2), num(-1)},
			want:    0.5,
			wantErr: false,
		},
		{
			name:    "wrong type first arg",
			args:    []core.Value{str("2"), num(3)},
			want:    0,
			wantErr: true,
			errMsg:  "TypeError: pow: expected number, got string",
		},
		{
			name:    "wrong type second arg",
			args:    []core.Value{num(2), str("3")},
			want:    0,
			wantErr: true,
			errMsg:  "TypeError: pow: expected number, got string",
		},
		{
			name:    "too few args",
			args:    []core.Value{num(2)},
			want:    0,
			wantErr: true,
			errMsg:  "ArgumentError: pow: takes exactly 2 arguments (1 given)",
		},
		{
			name:    "too many args",
			args:    []core.Value{num(2), num(3), num(4)},
			want:    0,
			wantErr: true,
			errMsg:  "ArgumentError: pow: takes exactly 2 arguments (3 given)",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := pow(tt.args, nil)

			if (err != nil) != tt.wantErr {
				t.Errorf("BinaryNumber() error = %v, wantErr %v", err, tt.wantErr)
			}

			if err != nil {
				if tt.errMsg != "" && err.Error() != tt.errMsg {
					t.Errorf("BinaryNumber() error = %v, want %v", err.Error(), tt.errMsg)
				}
			} else {
				if n, ok := got.(core.NumberValue); ok {
					if float64(n) != tt.want {
						t.Errorf("BinaryNumber() = %v, want %v", float64(n), tt.want)
					}
				}
			}
		})
	}
}

func TestVariadicNumber(t *testing.T) {
	// Test variadic number function
	sum := builders.VariadicNumber("sum", 1, func(nums []float64) (float64, error) {
		total := 0.0
		for _, n := range nums {
			total += n
		}
		return total, nil
	})

	tests := []struct {
		name    string
		args    []core.Value
		want    float64
		wantErr bool
		errMsg  string
	}{
		{
			name:    "single number",
			args:    []core.Value{num(42)},
			want:    42,
			wantErr: false,
		},
		{
			name:    "multiple numbers",
			args:    []core.Value{num(1), num(2), num(3), num(4)},
			want:    10,
			wantErr: false,
		},
		{
			name:    "no args",
			args:    []core.Value{},
			want:    0,
			wantErr: true,
			errMsg:  "ArgumentError: sum: at least 1 argument required",
		},
		{
			name:    "mixed types",
			args:    []core.Value{num(1), str("2"), num(3)},
			want:    0,
			wantErr: true,
			errMsg:  "TypeError: sum: all arguments must be numbers, argument 2 is string",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := sum(tt.args, nil)

			if (err != nil) != tt.wantErr {
				t.Errorf("VariadicNumber() error = %v, wantErr %v", err, tt.wantErr)
			}

			if err != nil {
				if tt.errMsg != "" && err.Error() != tt.errMsg {
					t.Errorf("VariadicNumber() error = %v, want %v", err.Error(), tt.errMsg)
				}
			} else {
				if n, ok := got.(core.NumberValue); ok {
					if float64(n) != tt.want {
						t.Errorf("VariadicNumber() = %v, want %v", float64(n), tt.want)
					}
				}
			}
		})
	}
}

func TestUnaryString(t *testing.T) {
	// Test unary string function
	upper := builders.UnaryStringSimple("upper", strings.ToUpper)

	tests := []struct {
		name    string
		args    []core.Value
		want    string
		wantErr bool
		errMsg  string
	}{
		{
			name:    "lowercase string",
			args:    []core.Value{str("hello")},
			want:    "HELLO",
			wantErr: false,
		},
		{
			name:    "mixed case",
			args:    []core.Value{str("Hello World")},
			want:    "HELLO WORLD",
			wantErr: false,
		},
		{
			name:    "empty string",
			args:    []core.Value{str("")},
			want:    "",
			wantErr: false,
		},
		{
			name:    "wrong type",
			args:    []core.Value{num(42)},
			want:    "",
			wantErr: true,
			errMsg:  "TypeError: upper: expected string, got number",
		},
		{
			name:    "no args",
			args:    []core.Value{},
			want:    "",
			wantErr: true,
			errMsg:  "ArgumentError: upper: takes exactly 1 argument (0 given)",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := upper(tt.args, nil)

			if (err != nil) != tt.wantErr {
				t.Errorf("UnaryString() error = %v, wantErr %v", err, tt.wantErr)
			}

			if err != nil {
				if tt.errMsg != "" && err.Error() != tt.errMsg {
					t.Errorf("UnaryString() error = %v, want %v", err.Error(), tt.errMsg)
				}
			} else {
				if s, ok := got.(core.StringValue); ok {
					if string(s) != tt.want {
						t.Errorf("UnaryString() = %v, want %v", string(s), tt.want)
					}
				}
			}
		})
	}
}

func TestPredicateNumber(t *testing.T) {
	// Test predicate number function
	isPositive := builders.PredicateNumber("is_positive", func(n float64) bool {
		return n > 0
	})

	tests := []struct {
		name    string
		args    []core.Value
		want    bool
		wantErr bool
		errMsg  string
	}{
		{
			name:    "positive number",
			args:    []core.Value{num(42)},
			want:    true,
			wantErr: false,
		},
		{
			name:    "negative number",
			args:    []core.Value{num(-42)},
			want:    false,
			wantErr: false,
		},
		{
			name:    "zero",
			args:    []core.Value{num(0)},
			want:    false,
			wantErr: false,
		},
		{
			name:    "wrong type",
			args:    []core.Value{str("42")},
			want:    false,
			wantErr: true,
			errMsg:  "TypeError: is_positive: expected number, got string",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := isPositive(tt.args, nil)

			if (err != nil) != tt.wantErr {
				t.Errorf("PredicateNumber() error = %v, wantErr %v", err, tt.wantErr)
			}

			if err != nil {
				if tt.errMsg != "" && err.Error() != tt.errMsg {
					t.Errorf("PredicateNumber() error = %v, want %v", err.Error(), tt.errMsg)
				}
			} else {
				if b, ok := got.(core.BoolValue); ok {
					if bool(b) != tt.want {
						t.Errorf("PredicateNumber() = %v, want %v", bool(b), tt.want)
					}
				}
			}
		})
	}
}

func TestPredicateString(t *testing.T) {
	// Test predicate string function
	isEmpty := builders.PredicateString("is_empty", func(s string) bool {
		return len(s) == 0
	})

	tests := []struct {
		name    string
		args    []core.Value
		want    bool
		wantErr bool
		errMsg  string
	}{
		{
			name:    "empty string",
			args:    []core.Value{str("")},
			want:    true,
			wantErr: false,
		},
		{
			name:    "non-empty string",
			args:    []core.Value{str("hello")},
			want:    false,
			wantErr: false,
		},
		{
			name:    "whitespace string",
			args:    []core.Value{str("  ")},
			want:    false,
			wantErr: false,
		},
		{
			name:    "wrong type",
			args:    []core.Value{num(0)},
			want:    false,
			wantErr: true,
			errMsg:  "TypeError: is_empty: expected string, got number",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := isEmpty(tt.args, nil)

			if (err != nil) != tt.wantErr {
				t.Errorf("PredicateString() error = %v, wantErr %v", err, tt.wantErr)
			}

			if err != nil {
				if tt.errMsg != "" && err.Error() != tt.errMsg {
					t.Errorf("PredicateString() error = %v, want %v", err.Error(), tt.errMsg)
				}
			} else {
				if b, ok := got.(core.BoolValue); ok {
					if bool(b) != tt.want {
						t.Errorf("PredicateString() = %v, want %v", bool(b), tt.want)
					}
				}
			}
		})
	}
}

func TestUnarySequence(t *testing.T) {
	// Test unary sequence function
	length := builders.UnarySequence("len", func(seq core.Value) (core.Value, error) {
		switch v := seq.(type) {
		case *core.ListValue:
			return core.NumberValue(v.Len()), nil
		case core.StringValue:
			return core.NumberValue(len(v)), nil
		default:
			return nil, errors.New("unsupported sequence type")
		}
	})

	tests := []struct {
		name    string
		args    []core.Value
		want    float64
		wantErr bool
		errMsg  string
	}{
		{
			name:    "list",
			args:    []core.Value{list(num(1), num(2), num(3))},
			want:    3,
			wantErr: false,
		},
		{
			name:    "string",
			args:    []core.Value{str("hello")},
			want:    5,
			wantErr: false,
		},
		{
			name:    "empty list",
			args:    []core.Value{list()},
			want:    0,
			wantErr: false,
		},
		{
			name:    "not a sequence",
			args:    []core.Value{num(42)},
			want:    0,
			wantErr: true,
			errMsg:  "TypeError: len: expected sequence, got number",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := length(tt.args, nil)

			if (err != nil) != tt.wantErr {
				t.Errorf("UnarySequence() error = %v, wantErr %v", err, tt.wantErr)
			}

			if err != nil {
				if tt.errMsg != "" && err.Error() != tt.errMsg {
					t.Errorf("UnarySequence() error = %v, want %v", err.Error(), tt.errMsg)
				}
			} else {
				if n, ok := got.(core.NumberValue); ok {
					if float64(n) != tt.want {
						t.Errorf("UnarySequence() = %v, want %v", float64(n), tt.want)
					}
				}
			}
		})
	}
}

func TestUnaryAny(t *testing.T) {
	// Test unary any function
	typeOf := builders.UnaryAny("type_of", func(val core.Value) (core.Value, error) {
		return core.StringValue(val.Type()), nil
	})

	tests := []struct {
		name    string
		args    []core.Value
		want    string
		wantErr bool
	}{
		{
			name:    "number",
			args:    []core.Value{num(42)},
			want:    "number",
			wantErr: false,
		},
		{
			name:    "string",
			args:    []core.Value{str("hello")},
			want:    "string",
			wantErr: false,
		},
		{
			name:    "list",
			args:    []core.Value{list(num(1), num(2))},
			want:    "list",
			wantErr: false,
		},
		{
			name:    "bool",
			args:    []core.Value{core.True},
			want:    "bool",
			wantErr: false,
		},
		{
			name:    "nil",
			args:    []core.Value{core.Nil},
			want:    "nil",
			wantErr: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := typeOf(tt.args, nil)

			if (err != nil) != tt.wantErr {
				t.Errorf("UnaryAny() error = %v, wantErr %v", err, tt.wantErr)
			}

			if !tt.wantErr {
				if s, ok := got.(core.StringValue); ok {
					if string(s) != tt.want {
						t.Errorf("UnaryAny() = %v, want %v", string(s), tt.want)
					}
				}
			}
		})
	}
}
