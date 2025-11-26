package builtin

import (
	"math"
	"testing"

	"github.com/mmichie/m28/core"
)

func TestIntBuiltin(t *testing.T) {
	ctx := core.NewContext(nil)
	RegisterTypes(ctx)

	intFunc, err := ctx.Lookup("int")
	if err != nil {
		t.Fatalf("int() builtin not registered: %v", err)
	}

	callable, ok := intFunc.(core.Callable)
	if !ok {
		t.Fatalf("int is not callable")
	}

	tests := []struct {
		name     string
		args     []core.Value
		expected core.Value
		wantErr  bool
	}{
		{
			name:     "int from float",
			args:     []core.Value{core.NumberValue(3.7)},
			expected: core.NumberValue(3),
		},
		{
			name:     "int from string",
			args:     []core.Value{core.StringValue("42")},
			expected: core.NumberValue(42),
		},
		{
			name:     "int from negative string",
			args:     []core.Value{core.StringValue("-15")},
			expected: core.NumberValue(-15),
		},
		{
			name:     "int from bool true",
			args:     []core.Value{core.True},
			expected: core.NumberValue(1),
		},
		{
			name:     "int from bool false",
			args:     []core.Value{core.False},
			expected: core.NumberValue(0),
		},
		{
			name:     "int with no arguments",
			args:     []core.Value{},
			expected: core.NumberValue(0),
		},
		{
			name:    "int from invalid string",
			args:    []core.Value{core.StringValue("not a number")},
			wantErr: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := callable.Call(tt.args, ctx)
			if (err != nil) != tt.wantErr {
				t.Errorf("int() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr {
				if numResult, ok := result.(core.NumberValue); ok {
					if numExpected, ok := tt.expected.(core.NumberValue); ok {
						if math.Abs(float64(numResult-numExpected)) > 0.001 {
							t.Errorf("int() = %v, want %v", result, tt.expected)
						}
					}
				} else {
					t.Errorf("int() returned non-number: %v", result)
				}
			}
		})
	}
}

func TestFloatBuiltin(t *testing.T) {
	ctx := core.NewContext(nil)
	RegisterTypes(ctx)

	floatFunc, err := ctx.Lookup("float")
	if err != nil {
		t.Fatalf("float() builtin not registered: %v", err)
	}

	callable, ok := floatFunc.(core.Callable)
	if !ok {
		t.Fatalf("float is not callable")
	}

	tests := []struct {
		name     string
		args     []core.Value
		expected core.Value
		wantErr  bool
	}{
		{
			name:     "float from int",
			args:     []core.Value{core.NumberValue(42)},
			expected: core.NumberValue(42.0),
		},
		{
			name:     "float from string",
			args:     []core.Value{core.StringValue("3.14")},
			expected: core.NumberValue(3.14),
		},
		{
			name:     "float from negative string",
			args:     []core.Value{core.StringValue("-2.5")},
			expected: core.NumberValue(-2.5),
		},
		{
			name:     "float from bool true",
			args:     []core.Value{core.True},
			expected: core.NumberValue(1.0),
		},
		{
			name:     "float from bool false",
			args:     []core.Value{core.False},
			expected: core.NumberValue(0.0),
		},
		{
			name:     "float with no arguments",
			args:     []core.Value{},
			expected: core.NumberValue(0.0),
		},
		{
			name:    "float from invalid string",
			args:    []core.Value{core.StringValue("not a number")},
			wantErr: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := callable.Call(tt.args, ctx)
			if (err != nil) != tt.wantErr {
				t.Errorf("float() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr {
				if numResult, ok := result.(core.NumberValue); ok {
					if numExpected, ok := tt.expected.(core.NumberValue); ok {
						if math.Abs(float64(numResult-numExpected)) > 0.001 {
							t.Errorf("float() = %v, want %v", result, tt.expected)
						}
					}
				} else {
					t.Errorf("float() returned non-number: %v", result)
				}
			}
		})
	}
}

func TestStrBuiltin(t *testing.T) {
	ctx := core.NewContext(nil)
	RegisterTypes(ctx)

	strFunc, err := ctx.Lookup("str")
	if err != nil {
		t.Fatalf("str() builtin not registered: %v", err)
	}

	callable, ok := strFunc.(core.Callable)
	if !ok {
		t.Fatalf("str is not callable")
	}

	tests := []struct {
		name     string
		args     []core.Value
		expected core.Value
		wantErr  bool
	}{
		{
			name:     "str from number",
			args:     []core.Value{core.NumberValue(42)},
			expected: core.StringValue("42"),
		},
		{
			name:     "str from float",
			args:     []core.Value{core.NumberValue(3.14)},
			expected: core.StringValue("3.14"),
		},
		{
			name:     "str from bool true",
			args:     []core.Value{core.True},
			expected: core.StringValue("True"),
		},
		{
			name:     "str from bool false",
			args:     []core.Value{core.False},
			expected: core.StringValue("False"),
		},
		{
			name:     "str from string",
			args:     []core.Value{core.StringValue("hello")},
			expected: core.StringValue("hello"),
		},
		{
			name:     "str with no arguments",
			args:     []core.Value{},
			expected: core.StringValue(""),
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := callable.Call(tt.args, ctx)
			if (err != nil) != tt.wantErr {
				t.Errorf("str() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr && result != tt.expected {
				t.Errorf("str() = %v, want %v", result, tt.expected)
			}
		})
	}
}

func TestBoolBuiltin(t *testing.T) {
	ctx := core.NewContext(nil)
	RegisterTypes(ctx)

	boolFunc, err := ctx.Lookup("bool")
	if err != nil {
		t.Fatalf("bool() builtin not registered: %v", err)
	}

	callable, ok := boolFunc.(core.Callable)
	if !ok {
		t.Fatalf("bool is not callable")
	}

	tests := []struct {
		name     string
		args     []core.Value
		expected core.Value
		wantErr  bool
	}{
		{
			name:     "bool from truthy number",
			args:     []core.Value{core.NumberValue(42)},
			expected: core.True,
		},
		{
			name:     "bool from zero",
			args:     []core.Value{core.NumberValue(0)},
			expected: core.False,
		},
		{
			name:     "bool from truthy string",
			args:     []core.Value{core.StringValue("hello")},
			expected: core.True,
		},
		{
			name:     "bool from empty string",
			args:     []core.Value{core.StringValue("")},
			expected: core.False,
		},
		{
			name:     "bool from True",
			args:     []core.Value{core.True},
			expected: core.True,
		},
		{
			name:     "bool from False",
			args:     []core.Value{core.False},
			expected: core.False,
		},
		{
			name:     "bool from None",
			args:     []core.Value{core.None},
			expected: core.False,
		},
		{
			name:     "bool with no arguments",
			args:     []core.Value{},
			expected: core.False,
		},
		{
			name:     "bool from non-empty list",
			args:     []core.Value{core.NewList(core.NumberValue(1))},
			expected: core.True,
		},
		{
			name:     "bool from empty list",
			args:     []core.Value{core.NewList()},
			expected: core.False,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := callable.Call(tt.args, ctx)
			if (err != nil) != tt.wantErr {
				t.Errorf("bool() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr && result != tt.expected {
				t.Errorf("bool() = %v, want %v", result, tt.expected)
			}
		})
	}
}
