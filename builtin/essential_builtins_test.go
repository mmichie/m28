package builtin

import (
	"testing"

	"github.com/mmichie/m28/core"
)

func TestAllBuiltin(t *testing.T) {
	ctx := core.NewContext(nil)
	RegisterEssentialBuiltins(ctx)

	allFunc, err := ctx.Lookup("all")
	if err != nil {
		t.Fatalf("all() builtin not registered: %v", err)
	}

	callable, ok := allFunc.(core.Callable)
	if !ok {
		t.Fatalf("all is not callable")
	}

	tests := []struct {
		name     string
		args     []core.Value
		expected core.Value
		wantErr  bool
	}{
		{
			name:     "all empty list",
			args:     []core.Value{core.NewList()},
			expected: core.True,
		},
		{
			name:     "all truthy values",
			args:     []core.Value{core.NewList(core.True, core.NumberValue(1), core.StringValue("hello"))},
			expected: core.True,
		},
		{
			name:     "all with one falsy",
			args:     []core.Value{core.NewList(core.True, core.False, core.NumberValue(1))},
			expected: core.False,
		},
		{
			name:     "all with zero",
			args:     []core.Value{core.NewList(core.NumberValue(1), core.NumberValue(0), core.NumberValue(3))},
			expected: core.False,
		},
		{
			name:     "all with empty string",
			args:     []core.Value{core.NewList(core.StringValue("hello"), core.StringValue(""), core.StringValue("world"))},
			expected: core.False,
		},
		{
			name:     "all empty tuple",
			args:     []core.Value{core.TupleValue{}},
			expected: core.True,
		},
		{
			name:     "all tuple with truthy",
			args:     []core.Value{core.TupleValue{core.NumberValue(1), core.NumberValue(2), core.NumberValue(3)}},
			expected: core.True,
		},
		{
			name:    "all with no arguments",
			args:    []core.Value{},
			wantErr: true,
		},
		{
			name:    "all with too many arguments",
			args:    []core.Value{core.NewList(), core.NewList()},
			wantErr: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := callable.Call(tt.args, ctx)
			if (err != nil) != tt.wantErr {
				t.Errorf("all() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr && result != tt.expected {
				t.Errorf("all() = %v, want %v", result, tt.expected)
			}
		})
	}
}

func TestAnyBuiltin(t *testing.T) {
	ctx := core.NewContext(nil)
	RegisterEssentialBuiltins(ctx)

	anyFunc, err := ctx.Lookup("any")
	if err != nil {
		t.Fatalf("any() builtin not registered: %v", err)
	}

	callable, ok := anyFunc.(core.Callable)
	if !ok {
		t.Fatalf("any is not callable")
	}

	tests := []struct {
		name     string
		args     []core.Value
		expected core.Value
		wantErr  bool
	}{
		{
			name:     "any empty list",
			args:     []core.Value{core.NewList()},
			expected: core.False,
		},
		{
			name:     "any all truthy",
			args:     []core.Value{core.NewList(core.True, core.NumberValue(1), core.StringValue("hello"))},
			expected: core.True,
		},
		{
			name:     "any with one truthy",
			args:     []core.Value{core.NewList(core.False, core.True, core.NumberValue(0))},
			expected: core.True,
		},
		{
			name:     "any all falsy",
			args:     []core.Value{core.NewList(core.False, core.NumberValue(0), core.StringValue(""))},
			expected: core.False,
		},
		{
			name:     "any with one non-zero",
			args:     []core.Value{core.NewList(core.NumberValue(0), core.NumberValue(1), core.NumberValue(0))},
			expected: core.True,
		},
		{
			name:     "any empty tuple",
			args:     []core.Value{core.TupleValue{}},
			expected: core.False,
		},
		{
			name:     "any tuple with one truthy",
			args:     []core.Value{core.TupleValue{core.NumberValue(0), core.False, core.NumberValue(5)}},
			expected: core.True,
		},
		{
			name:    "any with no arguments",
			args:    []core.Value{},
			wantErr: true,
		},
		{
			name:    "any with too many arguments",
			args:    []core.Value{core.NewList(), core.NewList()},
			wantErr: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := callable.Call(tt.args, ctx)
			if (err != nil) != tt.wantErr {
				t.Errorf("any() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr && result != tt.expected {
				t.Errorf("any() = %v, want %v", result, tt.expected)
			}
		})
	}
}

func TestErrorBuiltin(t *testing.T) {
	ctx := core.NewContext(nil)
	RegisterEssentialBuiltins(ctx)

	errorFunc, err := ctx.Lookup("error")
	if err != nil {
		t.Fatalf("error() builtin not registered: %v", err)
	}

	callable, ok := errorFunc.(core.Callable)
	if !ok {
		t.Fatalf("error is not callable")
	}

	tests := []struct {
		name        string
		args        []core.Value
		wantErr     bool
		expectedMsg string
	}{
		{
			name:        "error with message",
			args:        []core.Value{core.StringValue("test error")},
			wantErr:     true,
			expectedMsg: "test error",
		},
		{
			name:        "error with no arguments",
			args:        []core.Value{},
			wantErr:     true,
			expectedMsg: "RuntimeError",
		},
		{
			name:    "error with too many arguments",
			args:    []core.Value{core.StringValue("error1"), core.StringValue("error2")},
			wantErr: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			_, err := callable.Call(tt.args, ctx)
			if (err != nil) != tt.wantErr {
				t.Errorf("error() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if tt.wantErr && tt.expectedMsg != "" && err != nil {
				if err.Error() != tt.expectedMsg {
					t.Errorf("error() message = %v, want %v", err.Error(), tt.expectedMsg)
				}
			}
		})
	}
}
