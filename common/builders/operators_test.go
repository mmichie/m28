package builders_test

import (
	"testing"

	"github.com/mmichie/m28/common/builders"
	"github.com/mmichie/m28/core"
)

func TestOperatorAdd(t *testing.T) {
	// Test the + operator
	add := builders.Add()

	tests := []struct {
		name    string
		args    []core.Value
		want    core.Value
		wantErr bool
		errMsg  string
	}{
		// Number addition
		{
			name:    "add no args",
			args:    []core.Value{},
			want:    num(0),
			wantErr: false,
		},
		{
			name:    "add one number",
			args:    []core.Value{num(5)},
			want:    num(5),
			wantErr: false,
		},
		{
			name:    "add two numbers",
			args:    []core.Value{num(1), num(2)},
			want:    num(3),
			wantErr: false,
		},
		{
			name:    "add three numbers",
			args:    []core.Value{num(1), num(2), num(3)},
			want:    num(6),
			wantErr: false,
		},
		{
			name:    "add negative numbers",
			args:    []core.Value{num(-1), num(-2)},
			want:    num(-3),
			wantErr: false,
		},
		{
			name:    "add floats",
			args:    []core.Value{num(1.5), num(2.5)},
			want:    num(4),
			wantErr: false,
		},
		// String concatenation
		{
			name:    "concat two strings",
			args:    []core.Value{str("hello"), str(" world")},
			want:    str("hello world"),
			wantErr: false,
		},
		{
			name:    "concat three strings",
			args:    []core.Value{str("a"), str("b"), str("c")},
			want:    str("abc"),
			wantErr: false,
		},
		{
			name:    "concat empty strings",
			args:    []core.Value{str(""), str("test"), str("")},
			want:    str("test"),
			wantErr: false,
		},
		// List concatenation
		{
			name:    "concat two lists",
			args:    []core.Value{list(num(1), num(2)), list(num(3), num(4))},
			want:    list(num(1), num(2), num(3), num(4)),
			wantErr: false,
		},
		{
			name:    "concat three lists",
			args:    []core.Value{list(num(1)), list(num(2)), list(num(3))},
			want:    list(num(1), num(2), num(3)),
			wantErr: false,
		},
		{
			name:    "concat empty lists",
			args:    []core.Value{list(), list(num(1)), list()},
			want:    list(num(1)),
			wantErr: false,
		},
		// Error cases
		{
			name:    "mixed types",
			args:    []core.Value{num(1), str("2")},
			want:    nil,
			wantErr: true,
			errMsg:  "", // Just check that an error occurs, not the exact message
		},
		{
			name:    "unsupported type",
			args:    []core.Value{core.True, core.False},
			want:    nil,
			wantErr: true,
			errMsg:  "TypeError: +: unsupported operand type(s) for +: 'bool'",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := add(tt.args, nil)

			if (err != nil) != tt.wantErr {
				t.Errorf("Add() error = %v, wantErr %v", err, tt.wantErr)
			}

			if err != nil {
				if tt.errMsg != "" && err.Error() != tt.errMsg {
					t.Errorf("Add() error = %v, want %v", err.Error(), tt.errMsg)
				}
			} else {
				// Compare based on type
				switch want := tt.want.(type) {
				case core.NumberValue:
					if n, ok := got.(core.NumberValue); ok {
						if float64(n) != float64(want) {
							t.Errorf("Add() = %v, want %v", float64(n), float64(want))
						}
					} else {
						t.Errorf("Add() returned non-number: %v", got)
					}
				case core.StringValue:
					if s, ok := got.(core.StringValue); ok {
						if string(s) != string(want) {
							t.Errorf("Add() = %v, want %v", string(s), string(want))
						}
					} else {
						t.Errorf("Add() returned non-string: %v", got)
					}
				case *core.ListValue:
					if l, ok := got.(*core.ListValue); ok {
						if l.Len() != want.Len() {
							t.Errorf("Add() returned list of length %v, want %v", l.Len(), want.Len())
						}
						// Simple comparison for test
						lItems := l.Items()
						wItems := want.Items()
						for i := range lItems {
							if ln, ok := lItems[i].(core.NumberValue); ok {
								if wn, ok := wItems[i].(core.NumberValue); ok {
									if float64(ln) != float64(wn) {
										t.Errorf("Add() list[%d] = %v, want %v", i, float64(ln), float64(wn))
									}
								}
							}
						}
					} else {
						t.Errorf("Add() returned non-list: %v", got)
					}
				}
			}
		})
	}
}

func TestOperatorMultiply(t *testing.T) {
	// Test the * operator
	multiply := builders.Multiply()

	tests := []struct {
		name    string
		args    []core.Value
		want    core.Value
		wantErr bool
		errMsg  string
	}{
		// Number multiplication
		{
			name:    "multiply no args",
			args:    []core.Value{},
			want:    num(1),
			wantErr: false,
		},
		{
			name:    "multiply one number",
			args:    []core.Value{num(5)},
			want:    num(5),
			wantErr: false,
		},
		{
			name:    "multiply two numbers",
			args:    []core.Value{num(3), num(4)},
			want:    num(12),
			wantErr: false,
		},
		{
			name:    "multiply three numbers",
			args:    []core.Value{num(2), num(3), num(4)},
			want:    num(24),
			wantErr: false,
		},
		{
			name:    "multiply by zero",
			args:    []core.Value{num(100), num(0)},
			want:    num(0),
			wantErr: false,
		},
		{
			name:    "multiply negative numbers",
			args:    []core.Value{num(-2), num(3)},
			want:    num(-6),
			wantErr: false,
		},
		{
			name:    "multiply floats",
			args:    []core.Value{num(2.5), num(4)},
			want:    num(10),
			wantErr: false,
		},
		// String repetition
		{
			name:    "string * number",
			args:    []core.Value{str("abc"), num(3)},
			want:    str("abcabcabc"),
			wantErr: false,
		},
		{
			name:    "number * string",
			args:    []core.Value{num(3), str("xyz")},
			want:    str("xyzxyzxyz"),
			wantErr: false,
		},
		{
			name:    "string * zero",
			args:    []core.Value{str("test"), num(0)},
			want:    str(""),
			wantErr: false,
		},
		{
			name:    "string * negative",
			args:    []core.Value{str("test"), num(-5)},
			want:    str(""),
			wantErr: false,
		},
		{
			name:    "empty string * number",
			args:    []core.Value{str(""), num(10)},
			want:    str(""),
			wantErr: false,
		},
		// Error cases
		{
			name:    "string * string",
			args:    []core.Value{str("a"), str("b")},
			want:    nil,
			wantErr: true,
			errMsg:  "TypeError: *: unsupported operand type(s) for *: 'string'",
		},
		{
			name:    "list * number",
			args:    []core.Value{list(num(1)), num(2)},
			want:    list(num(1), num(1)),
			wantErr: false,
		},
		{
			name:    "number * list",
			args:    []core.Value{num(3), list(str("a"))},
			want:    list(str("a"), str("a"), str("a")),
			wantErr: false,
		},
		{
			name:    "list * negative number",
			args:    []core.Value{list(num(1), num(2)), num(-1)},
			want:    list(),
			wantErr: false,
		},
		{
			name:    "bool multiplication not supported",
			args:    []core.Value{core.True, num(2)},
			want:    nil,
			wantErr: true,
			errMsg:  "TypeError: *: unsupported operand type(s) for *: 'bool'",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := multiply(tt.args, nil)

			if (err != nil) != tt.wantErr {
				t.Errorf("Multiply() error = %v, wantErr %v", err, tt.wantErr)
			}

			if err != nil {
				if tt.errMsg != "" && err.Error() != tt.errMsg {
					t.Errorf("Multiply() error = %v, want %v", err.Error(), tt.errMsg)
				}
			} else {
				// Compare based on type
				switch want := tt.want.(type) {
				case core.NumberValue:
					if n, ok := got.(core.NumberValue); ok {
						if float64(n) != float64(want) {
							t.Errorf("Multiply() = %v, want %v", float64(n), float64(want))
						}
					} else {
						t.Errorf("Multiply() returned non-number: %v", got)
					}
				case core.StringValue:
					if s, ok := got.(core.StringValue); ok {
						if string(s) != string(want) {
							t.Errorf("Multiply() = %v, want %v", string(s), string(want))
						}
					} else {
						t.Errorf("Multiply() returned non-string: %v", got)
					}
				case *core.ListValue:
					if l, ok := got.(*core.ListValue); ok {
						if l.Len() != want.Len() {
							t.Errorf("Multiply() list length = %v, want %v", l.Len(), want.Len())
						} else {
							lItems := l.Items()
							wItems := want.Items()
							for i := range lItems {
								if !core.EqualValues(lItems[i], wItems[i]) {
									t.Errorf("Multiply() list[%d] = %v, want %v", i, lItems[i], wItems[i])
								}
							}
						}
					} else {
						t.Errorf("Multiply() returned non-list: %v", got)
					}
				}
			}
		})
	}
}

// Test OperatorBuilder with custom handlers
func TestOperatorBuilder(t *testing.T) {
	// Test custom operator with multiple handlers
	customOp := builders.NewOperator("custom", "").
		WithArgs(2, 2).
		WithHandler(builders.NumberHandler(func(nums []float64) (core.Value, error) {
			return core.NumberValue(nums[0] + nums[1]), nil
		})).
		WithHandler(builders.StringHandler(func(strs []string) (core.Value, error) {
			return core.StringValue(strs[0] + "-" + strs[1]), nil
		})).
		Build()

	tests := []struct {
		name    string
		args    []core.Value
		want    core.Value
		wantErr bool
		errMsg  string
	}{
		{
			name:    "number handling",
			args:    []core.Value{num(10), num(20)},
			want:    num(30),
			wantErr: false,
		},
		{
			name:    "string handling",
			args:    []core.Value{str("hello"), str("world")},
			want:    str("hello-world"),
			wantErr: false,
		},
		{
			name:    "too few args",
			args:    []core.Value{num(1)},
			want:    nil,
			wantErr: true,
			errMsg:  "ArgumentError: custom: takes exactly 2 arguments (1 given)",
		},
		{
			name:    "too many args",
			args:    []core.Value{num(1), num(2), num(3)},
			want:    nil,
			wantErr: true,
			errMsg:  "ArgumentError: custom: takes exactly 2 arguments (3 given)",
		},
		{
			name:    "unsupported type",
			args:    []core.Value{core.True, core.False},
			want:    nil,
			wantErr: true,
			errMsg:  "TypeError: custom: unsupported operand type(s) for custom: 'bool'",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := customOp(tt.args, nil)

			if (err != nil) != tt.wantErr {
				t.Errorf("OperatorBuilder() error = %v, wantErr %v", err, tt.wantErr)
			}

			if err != nil {
				if tt.errMsg != "" && err.Error() != tt.errMsg {
					t.Errorf("OperatorBuilder() error = %v, want %v", err.Error(), tt.errMsg)
				}
			} else {
				// Compare based on type
				switch want := tt.want.(type) {
				case core.NumberValue:
					if n, ok := got.(core.NumberValue); ok {
						if float64(n) != float64(want) {
							t.Errorf("OperatorBuilder() = %v, want %v", float64(n), float64(want))
						}
					}
				case core.StringValue:
					if s, ok := got.(core.StringValue); ok {
						if string(s) != string(want) {
							t.Errorf("OperatorBuilder() = %v, want %v", string(s), string(want))
						}
					}
				}
			}
		})
	}
}

// Test handlers
func TestHandlers(t *testing.T) {
	// Test ListHandler
	listOp := builders.NewOperator("list_op", "").
		WithHandler(builders.ListHandler(func(lists []*core.ListValue) (core.Value, error) {
			// Return length of all lists combined
			total := 0
			for _, list := range lists {
				total += list.Len()
			}
			return core.NumberValue(total), nil
		})).
		Build()

	// Test MixedNumberStringHandler
	repeatOp := builders.NewOperator("repeat", "").
		WithArgs(2, 2).
		WithHandler(builders.MixedNumberStringHandler(func(str string, num int) (core.Value, error) {
			result := ""
			for i := 0; i < num; i++ {
				result += str
			}
			return core.StringValue(result), nil
		})).
		Build()

	t.Run("ListHandler", func(t *testing.T) {
		got, err := listOp([]core.Value{list(num(1), num(2)), list(num(3))}, nil)
		if err != nil {
			t.Errorf("ListHandler() error = %v", err)
		}
		if n, ok := got.(core.NumberValue); ok {
			if float64(n) != 3 {
				t.Errorf("ListHandler() = %v, want 3", float64(n))
			}
		}
	})

	t.Run("MixedNumberStringHandler string first", func(t *testing.T) {
		got, err := repeatOp([]core.Value{str("x"), num(3)}, nil)
		if err != nil {
			t.Errorf("MixedNumberStringHandler() error = %v", err)
		}
		if s, ok := got.(core.StringValue); ok {
			if string(s) != "xxx" {
				t.Errorf("MixedNumberStringHandler() = %v, want xxx", string(s))
			}
		}
	})

	t.Run("MixedNumberStringHandler number first", func(t *testing.T) {
		got, err := repeatOp([]core.Value{num(2), str("y")}, nil)
		if err != nil {
			t.Errorf("MixedNumberStringHandler() error = %v", err)
		}
		if s, ok := got.(core.StringValue); ok {
			if string(s) != "yy" {
				t.Errorf("MixedNumberStringHandler() = %v, want yy", string(s))
			}
		}
	})
}
