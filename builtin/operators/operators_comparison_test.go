package operators

import (
	"testing"

	"github.com/mmichie/m28/core"
)

func TestEqualBasic(t *testing.T) {
	ctx := core.NewContext(nil)

	tests := []struct {
		name     string
		args     []core.Value
		expected core.Value
		wantErr  bool
	}{
		{
			name:     "equal numbers true",
			args:     []core.Value{core.NumberValue(5), core.NumberValue(5)},
			expected: core.True,
		},
		{
			name:     "equal numbers false",
			args:     []core.Value{core.NumberValue(5), core.NumberValue(3)},
			expected: core.False,
		},
		{
			name:     "equal strings true",
			args:     []core.Value{core.StringValue("hello"), core.StringValue("hello")},
			expected: core.True,
		},
		{
			name:     "equal strings false",
			args:     []core.Value{core.StringValue("hello"), core.StringValue("world")},
			expected: core.False,
		},
		{
			name:     "equal bools true",
			args:     []core.Value{core.True, core.True},
			expected: core.True,
		},
		{
			name:     "equal bools false",
			args:     []core.Value{core.True, core.False},
			expected: core.False,
		},
		{
			name:     "equal None",
			args:     []core.Value{core.None, core.None},
			expected: core.True,
		},
		{
			name:     "number and bool (True == 1)",
			args:     []core.Value{core.NumberValue(1), core.True},
			expected: core.True,
		},
		{
			name:     "number and bool (False == 0)",
			args:     []core.Value{core.NumberValue(0), core.False},
			expected: core.True,
		},
		{
			name:     "different types",
			args:     []core.Value{core.NumberValue(5), core.StringValue("5")},
			expected: core.False,
		},
		{
			name:    "too few arguments",
			args:    []core.Value{core.NumberValue(5)},
			wantErr: true,
		},
	}

	equalFunc := Equal()
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := equalFunc(tt.args, ctx)
			if (err != nil) != tt.wantErr {
				t.Errorf("Equal() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr && !valuesEqual(result, tt.expected) {
				t.Errorf("Equal() = %v, want %v", result, tt.expected)
			}
		})
	}
}

func TestNotEqualBasic(t *testing.T) {
	ctx := core.NewContext(nil)

	tests := []struct {
		name     string
		args     []core.Value
		expected core.Value
		wantErr  bool
	}{
		{
			name:     "not equal numbers true",
			args:     []core.Value{core.NumberValue(5), core.NumberValue(3)},
			expected: core.True,
		},
		{
			name:     "not equal numbers false",
			args:     []core.Value{core.NumberValue(5), core.NumberValue(5)},
			expected: core.False,
		},
		{
			name:     "not equal strings true",
			args:     []core.Value{core.StringValue("hello"), core.StringValue("world")},
			expected: core.True,
		},
		{
			name:     "not equal different types",
			args:     []core.Value{core.NumberValue(5), core.StringValue("5")},
			expected: core.True,
		},
		{
			name:    "too few arguments",
			args:    []core.Value{core.NumberValue(5)},
			wantErr: true,
		},
	}

	notEqualFunc := NotEqual()
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := notEqualFunc(tt.args, ctx)
			if (err != nil) != tt.wantErr {
				t.Errorf("NotEqual() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr && !valuesEqual(result, tt.expected) {
				t.Errorf("NotEqual() = %v, want %v", result, tt.expected)
			}
		})
	}
}

func TestLessThanBasic(t *testing.T) {
	ctx := core.NewContext(nil)

	tests := []struct {
		name     string
		args     []core.Value
		expected core.Value
		wantErr  bool
	}{
		{
			name:     "less than true",
			args:     []core.Value{core.NumberValue(3), core.NumberValue(5)},
			expected: core.True,
		},
		{
			name:     "less than false (equal)",
			args:     []core.Value{core.NumberValue(5), core.NumberValue(5)},
			expected: core.False,
		},
		{
			name:     "less than false (greater)",
			args:     []core.Value{core.NumberValue(7), core.NumberValue(5)},
			expected: core.False,
		},
		{
			name:     "less than strings true",
			args:     []core.Value{core.StringValue("apple"), core.StringValue("banana")},
			expected: core.True,
		},
		{
			name:     "less than strings false",
			args:     []core.Value{core.StringValue("banana"), core.StringValue("apple")},
			expected: core.False,
		},
		{
			name:     "less than with bool",
			args:     []core.Value{core.False, core.True},
			expected: core.True,
		},
		{
			name:    "too few arguments",
			args:    []core.Value{core.NumberValue(5)},
			wantErr: true,
		},
	}

	lessThanFunc := LessThan()
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := lessThanFunc(tt.args, ctx)
			if (err != nil) != tt.wantErr {
				t.Errorf("LessThan() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr && !valuesEqual(result, tt.expected) {
				t.Errorf("LessThan() = %v, want %v", result, tt.expected)
			}
		})
	}
}

func TestLessThanOrEqualBasic(t *testing.T) {
	ctx := core.NewContext(nil)

	tests := []struct {
		name     string
		args     []core.Value
		expected core.Value
		wantErr  bool
	}{
		{
			name:     "less than or equal true (less)",
			args:     []core.Value{core.NumberValue(3), core.NumberValue(5)},
			expected: core.True,
		},
		{
			name:     "less than or equal true (equal)",
			args:     []core.Value{core.NumberValue(5), core.NumberValue(5)},
			expected: core.True,
		},
		{
			name:     "less than or equal false",
			args:     []core.Value{core.NumberValue(7), core.NumberValue(5)},
			expected: core.False,
		},
		{
			name:     "less than or equal strings",
			args:     []core.Value{core.StringValue("apple"), core.StringValue("apple")},
			expected: core.True,
		},
		{
			name:    "too few arguments",
			args:    []core.Value{core.NumberValue(5)},
			wantErr: true,
		},
	}

	lessThanOrEqualFunc := LessThanOrEqual()
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := lessThanOrEqualFunc(tt.args, ctx)
			if (err != nil) != tt.wantErr {
				t.Errorf("LessThanOrEqual() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr && !valuesEqual(result, tt.expected) {
				t.Errorf("LessThanOrEqual() = %v, want %v", result, tt.expected)
			}
		})
	}
}

func TestGreaterThanBasic(t *testing.T) {
	ctx := core.NewContext(nil)

	tests := []struct {
		name     string
		args     []core.Value
		expected core.Value
		wantErr  bool
	}{
		{
			name:     "greater than true",
			args:     []core.Value{core.NumberValue(7), core.NumberValue(5)},
			expected: core.True,
		},
		{
			name:     "greater than false (equal)",
			args:     []core.Value{core.NumberValue(5), core.NumberValue(5)},
			expected: core.False,
		},
		{
			name:     "greater than false (less)",
			args:     []core.Value{core.NumberValue(3), core.NumberValue(5)},
			expected: core.False,
		},
		{
			name:     "greater than strings true",
			args:     []core.Value{core.StringValue("banana"), core.StringValue("apple")},
			expected: core.True,
		},
		{
			name:     "greater than with bool",
			args:     []core.Value{core.True, core.False},
			expected: core.True,
		},
		{
			name:    "too few arguments",
			args:    []core.Value{core.NumberValue(5)},
			wantErr: true,
		},
	}

	greaterThanFunc := GreaterThan()
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := greaterThanFunc(tt.args, ctx)
			if (err != nil) != tt.wantErr {
				t.Errorf("GreaterThan() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr && !valuesEqual(result, tt.expected) {
				t.Errorf("GreaterThan() = %v, want %v", result, tt.expected)
			}
		})
	}
}

func TestGreaterThanOrEqualBasic(t *testing.T) {
	ctx := core.NewContext(nil)

	tests := []struct {
		name     string
		args     []core.Value
		expected core.Value
		wantErr  bool
	}{
		{
			name:     "greater than or equal true (greater)",
			args:     []core.Value{core.NumberValue(7), core.NumberValue(5)},
			expected: core.True,
		},
		{
			name:     "greater than or equal true (equal)",
			args:     []core.Value{core.NumberValue(5), core.NumberValue(5)},
			expected: core.True,
		},
		{
			name:     "greater than or equal false",
			args:     []core.Value{core.NumberValue(3), core.NumberValue(5)},
			expected: core.False,
		},
		{
			name:     "greater than or equal strings",
			args:     []core.Value{core.StringValue("banana"), core.StringValue("banana")},
			expected: core.True,
		},
		{
			name:    "too few arguments",
			args:    []core.Value{core.NumberValue(5)},
			wantErr: true,
		},
	}

	greaterThanOrEqualFunc := GreaterThanOrEqual()
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := greaterThanOrEqualFunc(tt.args, ctx)
			if (err != nil) != tt.wantErr {
				t.Errorf("GreaterThanOrEqual() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr && !valuesEqual(result, tt.expected) {
				t.Errorf("GreaterThanOrEqual() = %v, want %v", result, tt.expected)
			}
		})
	}
}

