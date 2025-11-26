package operators

import (
	"testing"

	"github.com/mmichie/m28/core"
)

func TestNotBasic(t *testing.T) {
	ctx := core.NewContext(nil)

	tests := []struct {
		name     string
		args     []core.Value
		expected core.Value
		wantErr  bool
	}{
		{
			name:     "not true",
			args:     []core.Value{core.True},
			expected: core.False,
		},
		{
			name:     "not false",
			args:     []core.Value{core.False},
			expected: core.True,
		},
		{
			name:     "not truthy number",
			args:     []core.Value{core.NumberValue(5)},
			expected: core.False,
		},
		{
			name:     "not falsy number (zero)",
			args:     []core.Value{core.NumberValue(0)},
			expected: core.True,
		},
		{
			name:     "not truthy string",
			args:     []core.Value{core.StringValue("hello")},
			expected: core.False,
		},
		{
			name:     "not falsy string (empty)",
			args:     []core.Value{core.StringValue("")},
			expected: core.True,
		},
		{
			name:     "not None",
			args:     []core.Value{core.None},
			expected: core.True,
		},
		{
			name:     "not truthy list",
			args:     []core.Value{core.NewList(core.NumberValue(1))},
			expected: core.False,
		},
		{
			name:     "not falsy list (empty)",
			args:     []core.Value{core.NewList()},
			expected: core.True,
		},
		{
			name:    "not with no arguments",
			args:    []core.Value{},
			wantErr: true,
		},
	}

	notFunc := Not()
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := notFunc(tt.args, ctx)
			if (err != nil) != tt.wantErr {
				t.Errorf("Not() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr && !valuesEqual(result, tt.expected) {
				t.Errorf("Not() = %v, want %v", result, tt.expected)
			}
		})
	}
}

func TestAndBasic(t *testing.T) {
	ctx := core.NewContext(nil)

	tests := []struct {
		name     string
		args     []core.Value
		expected core.Value
		wantErr  bool
	}{
		{
			name:     "and true true",
			args:     []core.Value{core.True, core.True},
			expected: core.True,
		},
		{
			name:     "and true false",
			args:     []core.Value{core.True, core.False},
			expected: core.False,
		},
		{
			name:     "and false true",
			args:     []core.Value{core.False, core.True},
			expected: core.False,
		},
		{
			name:     "and false false",
			args:     []core.Value{core.False, core.False},
			expected: core.False,
		},
		{
			name:     "and returns first falsy",
			args:     []core.Value{core.NumberValue(5), core.NumberValue(0), core.NumberValue(10)},
			expected: core.NumberValue(0),
		},
		{
			name:     "and returns last if all truthy",
			args:     []core.Value{core.NumberValue(5), core.NumberValue(10), core.StringValue("hello")},
			expected: core.StringValue("hello"),
		},
		{
			name:     "and with empty string",
			args:     []core.Value{core.StringValue("hello"), core.StringValue("")},
			expected: core.StringValue(""),
		},
		{
			name:     "and single argument",
			args:     []core.Value{core.True},
			expected: core.True,
		},
	}

	andFunc := And()
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := andFunc(tt.args, ctx)
			if (err != nil) != tt.wantErr {
				t.Errorf("And() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr && !valuesEqual(result, tt.expected) {
				t.Errorf("And() = %v, want %v", result, tt.expected)
			}
		})
	}
}

func TestOrBasic(t *testing.T) {
	ctx := core.NewContext(nil)

	tests := []struct {
		name     string
		args     []core.Value
		expected core.Value
		wantErr  bool
	}{
		{
			name:     "or true true",
			args:     []core.Value{core.True, core.True},
			expected: core.True,
		},
		{
			name:     "or true false",
			args:     []core.Value{core.True, core.False},
			expected: core.True,
		},
		{
			name:     "or false true",
			args:     []core.Value{core.False, core.True},
			expected: core.True,
		},
		{
			name:     "or false false",
			args:     []core.Value{core.False, core.False},
			expected: core.False,
		},
		{
			name:     "or returns first truthy",
			args:     []core.Value{core.NumberValue(0), core.NumberValue(5), core.NumberValue(10)},
			expected: core.NumberValue(5),
		},
		{
			name:     "or returns last if all falsy",
			args:     []core.Value{core.NumberValue(0), core.False, core.StringValue("")},
			expected: core.StringValue(""),
		},
		{
			name:     "or with non-empty string",
			args:     []core.Value{core.StringValue(""), core.StringValue("hello")},
			expected: core.StringValue("hello"),
		},
		{
			name:     "or single argument",
			args:     []core.Value{core.True},
			expected: core.True,
		},
	}

	orFunc := Or()
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := orFunc(tt.args, ctx)
			if (err != nil) != tt.wantErr {
				t.Errorf("Or() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr && !valuesEqual(result, tt.expected) {
				t.Errorf("Or() = %v, want %v", result, tt.expected)
			}
		})
	}
}

func TestInBasic(t *testing.T) {
	ctx := core.NewContext(nil)

	tests := []struct {
		name     string
		args     []core.Value
		expected core.Value
		wantErr  bool
	}{
		{
			name: "in list true",
			args: []core.Value{
				core.NumberValue(2),
				core.NewList(core.NumberValue(1), core.NumberValue(2), core.NumberValue(3)),
			},
			expected: core.True,
		},
		{
			name: "in list false",
			args: []core.Value{
				core.NumberValue(5),
				core.NewList(core.NumberValue(1), core.NumberValue(2), core.NumberValue(3)),
			},
			expected: core.False,
		},
		{
			name:     "in string true",
			args:     []core.Value{core.StringValue("ell"), core.StringValue("hello")},
			expected: core.True,
		},
		{
			name:     "in string false",
			args:     []core.Value{core.StringValue("xyz"), core.StringValue("hello")},
			expected: core.False,
		},
		{
			name:    "in with too few arguments",
			args:    []core.Value{core.NumberValue(5)},
			wantErr: true,
		},
	}

	inFunc := In()
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := inFunc(tt.args, ctx)
			if (err != nil) != tt.wantErr {
				t.Errorf("In() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr && !valuesEqual(result, tt.expected) {
				t.Errorf("In() = %v, want %v", result, tt.expected)
			}
		})
	}
}

func TestNotInBasic(t *testing.T) {
	ctx := core.NewContext(nil)

	tests := []struct {
		name     string
		args     []core.Value
		expected core.Value
		wantErr  bool
	}{
		{
			name: "not in list true",
			args: []core.Value{
				core.NumberValue(5),
				core.NewList(core.NumberValue(1), core.NumberValue(2), core.NumberValue(3)),
			},
			expected: core.True,
		},
		{
			name: "not in list false",
			args: []core.Value{
				core.NumberValue(2),
				core.NewList(core.NumberValue(1), core.NumberValue(2), core.NumberValue(3)),
			},
			expected: core.False,
		},
		{
			name:     "not in string true",
			args:     []core.Value{core.StringValue("xyz"), core.StringValue("hello")},
			expected: core.True,
		},
		{
			name:     "not in string false",
			args:     []core.Value{core.StringValue("ell"), core.StringValue("hello")},
			expected: core.False,
		},
		{
			name:    "not in with too few arguments",
			args:    []core.Value{core.NumberValue(5)},
			wantErr: true,
		},
	}

	notInFunc := NotIn()
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := notInFunc(tt.args, ctx)
			if (err != nil) != tt.wantErr {
				t.Errorf("NotIn() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr && !valuesEqual(result, tt.expected) {
				t.Errorf("NotIn() = %v, want %v", result, tt.expected)
			}
		})
	}
}

func TestIsBasic(t *testing.T) {
	ctx := core.NewContext(nil)

	// Create some objects to test identity
	obj1 := core.NewDict()
	obj2 := core.NewDict()

	tests := []struct {
		name     string
		args     []core.Value
		expected core.Value
		wantErr  bool
	}{
		{
			name:     "is same object",
			args:     []core.Value{obj1, obj1},
			expected: core.True,
		},
		{
			name:     "is different objects",
			args:     []core.Value{obj1, obj2},
			expected: core.False,
		},
		{
			name:     "is None None",
			args:     []core.Value{core.None, core.None},
			expected: core.True,
		},
		{
			name:     "is True True",
			args:     []core.Value{core.True, core.True},
			expected: core.True,
		},
		{
			name:     "is False False",
			args:     []core.Value{core.False, core.False},
			expected: core.True,
		},
		{
			name:     "is value not None",
			args:     []core.Value{core.NumberValue(5), core.None},
			expected: core.False,
		},
		{
			name:    "is with too few arguments",
			args:    []core.Value{core.NumberValue(5)},
			wantErr: true,
		},
	}

	isFunc := Is()
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := isFunc(tt.args, ctx)
			if (err != nil) != tt.wantErr {
				t.Errorf("Is() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr && !valuesEqual(result, tt.expected) {
				t.Errorf("Is() = %v, want %v", result, tt.expected)
			}
		})
	}
}

func TestIsNotBasic(t *testing.T) {
	ctx := core.NewContext(nil)

	obj1 := core.NewDict()
	obj2 := core.NewDict()

	tests := []struct {
		name     string
		args     []core.Value
		expected core.Value
		wantErr  bool
	}{
		{
			name:     "is not same object",
			args:     []core.Value{obj1, obj1},
			expected: core.False,
		},
		{
			name:     "is not different objects",
			args:     []core.Value{obj1, obj2},
			expected: core.True,
		},
		{
			name:     "is not None None",
			args:     []core.Value{core.None, core.None},
			expected: core.False,
		},
		{
			name:     "is not value not None",
			args:     []core.Value{core.NumberValue(5), core.None},
			expected: core.True,
		},
		{
			name:    "is not with too few arguments",
			args:    []core.Value{core.NumberValue(5)},
			wantErr: true,
		},
	}

	isNotFunc := IsNot()
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := isNotFunc(tt.args, ctx)
			if (err != nil) != tt.wantErr {
				t.Errorf("IsNot() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr && !valuesEqual(result, tt.expected) {
				t.Errorf("IsNot() = %v, want %v", result, tt.expected)
			}
		})
	}
}
