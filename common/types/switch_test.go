package types

import (
	"fmt"
	"testing"

	"github.com/mmichie/m28/core"
)

func TestTypeSwitch(t *testing.T) {
	tests := []struct {
		name     string
		value    core.Value
		expected core.Value
	}{
		{
			name:     "number case",
			value:    core.NumberValue(42),
			expected: core.NumberValue(84), // doubled
		},
		{
			name:     "string case",
			value:    core.StringValue("hello"),
			expected: core.StringValue("hellohello"), // repeated
		},
		{
			name:     "list case",
			value:    core.ListValue{core.NumberValue(1), core.NumberValue(2)},
			expected: core.NumberValue(2), // length
		},
		{
			name:     "nil case",
			value:    core.Nil,
			expected: core.StringValue("was nil"),
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := Switch(tt.value).
				Number(func(n float64) (core.Value, error) {
					return core.NumberValue(n * 2), nil
				}).
				String(func(s string) (core.Value, error) {
					return core.StringValue(s + s), nil
				}).
				List(func(l core.ListValue) (core.Value, error) {
					return core.NumberValue(float64(len(l))), nil
				}).
				Nil(func() (core.Value, error) {
					return core.StringValue("was nil"), nil
				}).
				Default(func(v core.Value) (core.Value, error) {
					return nil, fmt.Errorf("unhandled type: %s", v.Type())
				}).
				Execute()

			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}

			if !core.EqualValues(result, tt.expected) {
				t.Errorf("got %v, want %v", result, tt.expected)
			}
		})
	}
}

func TestTypeSwitchCustomCase(t *testing.T) {
	// Test custom predicate
	isEvenNumber := func(v core.Value) bool {
		if n, ok := AsNumber(v); ok {
			return int(n)%2 == 0
		}
		return false
	}

	result, err := Switch(core.NumberValue(42)).
		Case(isEvenNumber, func(v core.Value) (core.Value, error) {
			return core.StringValue("even"), nil
		}).
		Number(func(n float64) (core.Value, error) {
			return core.StringValue("odd"), nil
		}).
		Execute()

	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	expected := core.StringValue("even")
	if !core.EqualValues(result, expected) {
		t.Errorf("got %v, want %v", result, expected)
	}
}

func TestTypeSwitchNoMatch(t *testing.T) {
	// Test when no case matches and no default
	result, err := Switch(core.NumberValue(42)).
		String(func(s string) (core.Value, error) {
			return core.StringValue("was string"), nil
		}).
		Execute()

	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if result != nil {
		t.Errorf("expected nil result, got %v", result)
	}
}

func TestTypeSwitchType(t *testing.T) {
	// Test matching by type name
	result, err := Switch(core.NumberValue(42)).
		Type(core.NumberType, func(v core.Value) (core.Value, error) {
			return core.StringValue("was number type"), nil
		}).
		Execute()

	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	expected := core.StringValue("was number type")
	if !core.EqualValues(result, expected) {
		t.Errorf("got %v, want %v", result, expected)
	}
}
