package types

import (
	"testing"

	"github.com/mmichie/m28/common/errors"
	"github.com/mmichie/m28/core"
)

// MockObject implements a simple object with dunder methods for testing
type MockObject struct {
	value int
}

func (m *MockObject) Type() core.Type {
	return "mock"
}

func (m *MockObject) String() string {
	return "MockObject"
}

func (m *MockObject) GetAttr(name string) (core.Value, bool) {
	switch name {
	case "__add__":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) != 1 {
				return nil, errors.NewRuntimeError("__add__", "__add__ takes exactly 1 argument")
			}
			if other, ok := args[0].(*MockObject); ok {
				return &MockObject{value: m.value + other.value}, nil
			}
			return nil, errors.NewRuntimeError("__add__", "unsupported operand type")
		}), true
	case "__len__":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.NumberValue(float64(m.value)), nil
		}), true
	case "__str__":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.StringValue("mock_str"), nil
		}), true
	case "__bool__":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.BoolValue(m.value != 0), nil
		}), true
	default:
		return nil, false
	}
}

func (m *MockObject) SetAttr(name string, value core.Value) error {
	return errors.NewRuntimeError("setattr", "MockObject is immutable")
}

func TestCallDunder(t *testing.T) {
	obj := &MockObject{value: 42}
	other := &MockObject{value: 8}

	// Test __add__
	result, found, err := CallDunder(obj, "__add__", []core.Value{other}, nil)
	if !found {
		t.Error("expected to find __add__ method")
	}
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if resultObj, ok := result.(*MockObject); ok {
		if resultObj.value != 50 {
			t.Errorf("expected 50, got %d", resultObj.value)
		}
	} else {
		t.Errorf("expected MockObject result, got %T", result)
	}
}

func TestHasDunder(t *testing.T) {
	obj := &MockObject{value: 42}

	// Test existing method
	if !HasDunder(obj, "__add__") {
		t.Error("expected to have __add__")
	}

	// Test non-existing method
	if HasDunder(obj, "__sub__") {
		t.Error("expected not to have __sub__")
	}

	// Test non-object - NumberValue can have methods through TypeDescriptor
	// so we test with a type that definitely doesn't have GetAttr
	if HasDunder(core.Nil, "__add__") {
		t.Error("nil should not have dunder methods")
	}
}

func TestCallLen(t *testing.T) {
	obj := &MockObject{value: 5}

	length, found, err := CallLen(obj, nil)
	if !found {
		t.Error("expected to find __len__ method")
	}
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if length != 5 {
		t.Errorf("expected length 5, got %d", length)
	}
}

func TestCallStr(t *testing.T) {
	obj := &MockObject{value: 42}

	str, found, err := CallStr(obj, nil)
	if !found {
		t.Error("expected to find __str__ method")
	}
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if str != "mock_str" {
		t.Errorf("expected 'mock_str', got %s", str)
	}
}

func TestCallBool(t *testing.T) {
	// Test truthy object
	obj1 := &MockObject{value: 42}
	b1, found, err := CallBool(obj1, nil)
	if !found {
		t.Error("expected to find __bool__ method")
	}
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if !b1 {
		t.Error("expected true for non-zero value")
	}

	// Test falsy object
	obj2 := &MockObject{value: 0}
	b2, _, _ := CallBool(obj2, nil)
	if b2 {
		t.Error("expected false for zero value")
	}
}

func TestCallAdd(t *testing.T) {
	obj1 := &MockObject{value: 30}
	obj2 := &MockObject{value: 12}

	result, found, err := CallAdd(obj1, obj2, nil)
	if !found {
		t.Error("expected to find __add__ method")
	}
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if resultObj, ok := result.(*MockObject); ok {
		if resultObj.value != 42 {
			t.Errorf("expected 42, got %d", resultObj.value)
		}
	} else {
		t.Errorf("expected MockObject result, got %T", result)
	}
}
