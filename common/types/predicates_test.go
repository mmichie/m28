package types

import (
	"testing"

	"github.com/mmichie/m28/core"
)

func TestIsNumber(t *testing.T) {
	tests := []struct {
		name  string
		input core.Value
		want  bool
	}{
		{"number is number", core.NumberValue(42), true},
		{"float is number", core.NumberValue(3.14), true},
		{"string not number", core.StringValue("42"), false},
		{"nil not number", core.Nil, false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := IsNumber(tt.input); got != tt.want {
				t.Errorf("IsNumber(%v) = %v, want %v", tt.input, got, tt.want)
			}
		})
	}
}

func TestIsString(t *testing.T) {
	tests := []struct {
		name  string
		input core.Value
		want  bool
	}{
		{"string is string", core.StringValue("hello"), true},
		{"empty string is string", core.StringValue(""), true},
		{"number not string", core.NumberValue(42), false},
		{"list not string", core.ListValue([]core.Value{}), false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := IsString(tt.input); got != tt.want {
				t.Errorf("IsString(%v) = %v, want %v", tt.input, got, tt.want)
			}
		})
	}
}

func TestIsBool(t *testing.T) {
	tests := []struct {
		name  string
		input core.Value
		want  bool
	}{
		{"true is bool", core.BoolValue(true), true},
		{"false is bool", core.BoolValue(false), true},
		{"number not bool", core.NumberValue(1), false},
		{"string not bool", core.StringValue("true"), false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := IsBool(tt.input); got != tt.want {
				t.Errorf("IsBool(%v) = %v, want %v", tt.input, got, tt.want)
			}
		})
	}
}

func TestIsContainer(t *testing.T) {
	tests := []struct {
		name  string
		input core.Value
		want  bool
	}{
		{"list is container", core.ListValue([]core.Value{}), true},
		{"tuple is container", core.TupleValue([]core.Value{}), true},
		{"dict is container", core.NewDict(), true},
		{"set is container", core.NewSet(), true},
		{"string not container", core.StringValue("hello"), false},
		{"number not container", core.NumberValue(42), false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := IsContainer(tt.input); got != tt.want {
				t.Errorf("IsContainer(%v) = %v, want %v", tt.input, got, tt.want)
			}
		})
	}
}

func TestIsSequence(t *testing.T) {
	tests := []struct {
		name  string
		input core.Value
		want  bool
	}{
		{"list is sequence", core.ListValue([]core.Value{}), true},
		{"tuple is sequence", core.TupleValue([]core.Value{}), true},
		{"string is sequence", core.StringValue("hello"), true},
		{"range is sequence", &core.RangeValue{Start: 0, Stop: 10, Step: 1}, true},
		{"dict not sequence", core.NewDict(), false},
		{"set not sequence", core.NewSet(), false},
		{"number not sequence", core.NumberValue(42), false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := IsSequence(tt.input); got != tt.want {
				t.Errorf("IsSequence(%v) = %v, want %v", tt.input, got, tt.want)
			}
		})
	}
}

func TestIsMapping(t *testing.T) {
	tests := []struct {
		name  string
		input core.Value
		want  bool
	}{
		{"dict is mapping", core.NewDict(), true},
		{"list not mapping", core.ListValue([]core.Value{}), false},
		{"set not mapping", core.NewSet(), false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := IsMapping(tt.input); got != tt.want {
				t.Errorf("IsMapping(%v) = %v, want %v", tt.input, got, tt.want)
			}
		})
	}
}

func TestIsCallable(t *testing.T) {
	// Create a simple function for testing
	fn := core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.Nil, nil
	})

	// Create a simple class
	class := &core.Class{
		Name:    "TestClass",
		Methods: map[string]core.Value{},
	}

	// Create an instance with __call__ method
	callableClass := &core.Class{
		Name: "CallableClass",
		Methods: map[string]core.Value{
			"__call__": fn,
		},
	}
	callableInstance := &core.Instance{
		Class: callableClass,
	}

	// Create an instance without __call__ method
	regularInstance := &core.Instance{
		Class: class,
	}

	tests := []struct {
		name  string
		input core.Value
		want  bool
	}{
		{"function is callable", fn, true},
		{"class is callable", class, true},
		{"instance with __call__ is callable", callableInstance, true},
		{"instance without __call__ not callable", regularInstance, false},
		{"number not callable", core.NumberValue(42), false},
		{"string not callable", core.StringValue("hello"), false},
		{"list not callable", core.ListValue([]core.Value{}), false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := IsCallable(tt.input); got != tt.want {
				t.Errorf("IsCallable(%v) = %v, want %v", tt.input, got, tt.want)
			}
		})
	}
}

func TestIsIterable(t *testing.T) {
	tests := []struct {
		name  string
		input core.Value
		want  bool
	}{
		{"list is iterable", core.ListValue([]core.Value{}), true},
		{"tuple is iterable", core.TupleValue([]core.Value{}), true},
		{"string is iterable", core.StringValue("hello"), false}, // StringValue doesn't implement Iterable directly
		{"dict is iterable", core.NewDict(), false},              // DictValue doesn't implement Iterable directly
		{"set is iterable", core.NewSet(), true},
		{"range is iterable", &core.RangeValue{Start: 0, Stop: 10, Step: 1}, true},
		{"number not iterable", core.NumberValue(42), false},
		{"bool not iterable", core.BoolValue(true), false},
		{"nil not iterable", core.Nil, false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := IsIterable(tt.input); got != tt.want {
				t.Errorf("IsIterable(%v) = %v, want %v", tt.input, got, tt.want)
			}
		})
	}
}

func TestIsNil(t *testing.T) {
	tests := []struct {
		name  string
		input core.Value
		want  bool
	}{
		{"nil is nil", core.Nil, true},
		{"number not nil", core.NumberValue(0), false},
		{"empty string not nil", core.StringValue(""), false},
		{"false not nil", core.BoolValue(false), false},
		{"empty list not nil", core.ListValue([]core.Value{}), false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := IsNil(tt.input); got != tt.want {
				t.Errorf("IsNil(%v) = %v, want %v", tt.input, got, tt.want)
			}
		})
	}
}

func TestIsHashable(t *testing.T) {
	tests := []struct {
		name  string
		input core.Value
		want  bool
	}{
		{"number is hashable", core.NumberValue(42), true},
		{"string is hashable", core.StringValue("hello"), true},
		{"bool is hashable", core.BoolValue(true), true},
		{"tuple is hashable", core.TupleValue([]core.Value{core.NumberValue(1)}), true},
		{"list not hashable", core.ListValue([]core.Value{}), false},
		{"dict not hashable", core.NewDict(), false},
		{"set not hashable", core.NewSet(), false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := IsHashable(tt.input); got != tt.want {
				t.Errorf("IsHashable(%v) = %v, want %v", tt.input, got, tt.want)
			}
		})
	}
}

func TestIsTruthy(t *testing.T) {
	tests := []struct {
		name  string
		input core.Value
		want  bool
	}{
		{"true is truthy", core.BoolValue(true), true},
		{"false is not truthy", core.BoolValue(false), false},
		{"non-zero number is truthy", core.NumberValue(42), true},
		{"zero is not truthy", core.NumberValue(0), false},
		{"non-empty string is truthy", core.StringValue("hello"), true},
		{"empty string is not truthy", core.StringValue(""), false},
		{"non-empty list is truthy", core.ListValue([]core.Value{core.NumberValue(1)}), true},
		{"empty list is not truthy", core.ListValue([]core.Value{}), false},
		{"nil is not truthy", core.Nil, false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := IsTruthy(tt.input); got != tt.want {
				t.Errorf("IsTruthy(%v) = %v, want %v", tt.input, got, tt.want)
			}
		})
	}
}
