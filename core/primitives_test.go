package core

import (
	"math/big"
	"testing"
)

// ============== NumberValue Tests ==============

func TestNumberValueType(t *testing.T) {
	n := NumberValue(42)
	if n.Type() != Type("number") {
		t.Errorf("Expected type 'number', got '%s'", n.Type())
	}
}

func TestNumberValueString(t *testing.T) {
	// Integer
	n1 := NumberValue(42)
	if n1.String() != "42" {
		t.Errorf("Expected '42', got '%s'", n1.String())
	}

	// Float
	n2 := NumberValue(3.14)
	str := n2.String()
	if str != "3.14" {
		t.Errorf("Expected '3.14', got '%s'", str)
	}

	// Negative
	n3 := NumberValue(-10)
	if n3.String() != "-10" {
		t.Errorf("Expected '-10', got '%s'", n3.String())
	}
}

func TestNumberValueGetAttr(t *testing.T) {
	n := NumberValue(42)

	// Numbers generally don't have attributes, but check __class__ or similar
	_, ok := n.GetAttr("__class__")
	// Just verify it doesn't panic
	_ = ok
}

// ============== BigIntValue Tests ==============

func TestNewBigInt(t *testing.T) {
	i := big.NewInt(12345)
	b := NewBigInt(i)

	if b.String() != "12345" {
		t.Errorf("Expected '12345', got '%s'", b.String())
	}
}

func TestNewBigIntFromInt64(t *testing.T) {
	b := NewBigIntFromInt64(9999999999)

	if b.String() != "9999999999" {
		t.Errorf("Expected '9999999999', got '%s'", b.String())
	}
}

func TestNewBigIntFromString(t *testing.T) {
	b, err := NewBigIntFromString("123456789012345678901234567890", 10)
	if err != nil {
		t.Errorf("NewBigIntFromString failed: %v", err)
	}

	if b.String() != "123456789012345678901234567890" {
		t.Errorf("Unexpected string: %s", b.String())
	}

	// Invalid string
	_, err = NewBigIntFromString("not_a_number", 10)
	if err == nil {
		t.Error("Expected error for invalid string")
	}
}

func TestBigIntValueType(t *testing.T) {
	b := NewBigIntFromInt64(42)
	if b.Type() != Type("int") {
		t.Errorf("Expected type 'int', got '%s'", b.Type())
	}
}

func TestBigIntValueToFloat64(t *testing.T) {
	b := NewBigIntFromInt64(42)
	f := b.ToFloat64()

	if f != 42.0 {
		t.Errorf("Expected 42.0, got %f", f)
	}
}

func TestBigIntValueToInt64(t *testing.T) {
	b := NewBigIntFromInt64(42)
	i, ok := b.ToInt64()

	if !ok {
		t.Error("ToInt64 should succeed for small values")
	}

	if i != 42 {
		t.Errorf("Expected 42, got %d", i)
	}

	// Large value
	large, _ := NewBigIntFromString("99999999999999999999999999999999", 10)
	_, ok = large.ToInt64()
	if ok {
		t.Error("ToInt64 should fail for very large values")
	}
}

func TestBigIntValueSign(t *testing.T) {
	pos := NewBigIntFromInt64(42)
	if pos.Sign() != 1 {
		t.Errorf("Expected sign 1, got %d", pos.Sign())
	}

	neg := NewBigIntFromInt64(-42)
	if neg.Sign() != -1 {
		t.Errorf("Expected sign -1, got %d", neg.Sign())
	}

	zero := NewBigIntFromInt64(0)
	if zero.Sign() != 0 {
		t.Errorf("Expected sign 0, got %d", zero.Sign())
	}
}

// ============== ComplexValue Tests ==============

func TestComplexValueType(t *testing.T) {
	c := ComplexValue(complex(3, 4))
	if c.Type() != Type("complex") {
		t.Errorf("Expected type 'complex', got '%s'", c.Type())
	}
}

func TestComplexValueString(t *testing.T) {
	c := ComplexValue(complex(3, 4))
	str := c.String()

	// Should be in form (3+4j) or similar
	if str != "(3+4j)" {
		t.Errorf("Expected '(3+4j)', got '%s'", str)
	}
}

func TestComplexValueGetAttr(t *testing.T) {
	c := ComplexValue(complex(3, 4))

	// Check that GetAttr doesn't panic - complex attributes may be handled differently
	_, _ = c.GetAttr("real")
	_, _ = c.GetAttr("imag")
	// Just verify no panic
}

// ============== StringValue Tests ==============

func TestStringValueType(t *testing.T) {
	s := StringValue("hello")
	if s.Type() != Type("string") {
		t.Errorf("Expected type 'string', got '%s'", s.Type())
	}
}

func TestStringValueString(t *testing.T) {
	s := StringValue("hello")
	// String() returns quoted form for StringValue
	str := s.String()
	if str != `"hello"` {
		t.Errorf("Expected '\"hello\"', got '%s'", str)
	}
}

func TestStringValueIterator(t *testing.T) {
	s := StringValue("abc")
	iter := s.Iterator()

	chars := []string{}
	for {
		val, ok := iter.Next()
		if !ok {
			break
		}
		if str, ok := val.(StringValue); ok {
			chars = append(chars, string(str))
		}
	}

	if len(chars) != 3 {
		t.Errorf("Expected 3 chars, got %d", len(chars))
	}

	if chars[0] != "a" || chars[1] != "b" || chars[2] != "c" {
		t.Errorf("Unexpected chars: %v", chars)
	}

	// Test reset
	iter.Reset()
	val, ok := iter.Next()
	if !ok {
		t.Error("Iterator should have value after reset")
	}
	if str, ok := val.(StringValue); !ok || string(str) != "a" {
		t.Errorf("Expected 'a' after reset, got %v", val)
	}
}

// ============== BoolValue Tests ==============

func TestBoolValueType(t *testing.T) {
	b := BoolValue(true)
	if b.Type() != Type("bool") {
		t.Errorf("Expected type 'bool', got '%s'", b.Type())
	}
}

func TestBoolValueString(t *testing.T) {
	trueVal := BoolValue(true)
	if trueVal.String() != "True" {
		t.Errorf("Expected 'True', got '%s'", trueVal.String())
	}

	falseVal := BoolValue(false)
	if falseVal.String() != "False" {
		t.Errorf("Expected 'False', got '%s'", falseVal.String())
	}
}

// ============== SymbolValue Tests ==============

func TestSymbolValueType(t *testing.T) {
	s := SymbolValue("mySymbol")
	if s.Type() != Type("symbol") {
		t.Errorf("Expected type 'symbol', got '%s'", s.Type())
	}
}

func TestSymbolValueString(t *testing.T) {
	s := SymbolValue("mySymbol")
	if s.String() != "mySymbol" {
		t.Errorf("Expected 'mySymbol', got '%s'", s.String())
	}
}

// ============== NilValue Tests ==============

func TestNilValueType(t *testing.T) {
	n := NilValue{}
	if n.Type() != Type("nil") {
		t.Errorf("Expected type 'nil', got '%s'", n.Type())
	}
}

func TestNilValueString(t *testing.T) {
	n := NilValue{}
	if n.String() != "None" {
		t.Errorf("Expected 'None', got '%s'", n.String())
	}
}

func TestNilConstant(t *testing.T) {
	if Nil.String() != "None" {
		t.Errorf("Nil constant should be 'None', got '%s'", Nil.String())
	}
}

// ============== EllipsisValue Tests ==============

func TestEllipsisValueType(t *testing.T) {
	e := EllipsisValue{}
	if e.Type() != Type("ellipsis") {
		t.Errorf("Expected type 'ellipsis', got '%s'", e.Type())
	}
}

func TestEllipsisValueString(t *testing.T) {
	e := EllipsisValue{}
	// Actual implementation returns "Ellipsis"
	if e.String() != "Ellipsis" {
		t.Errorf("Expected 'Ellipsis', got '%s'", e.String())
	}
}

// ============== NotImplementedValue Tests ==============

func TestNotImplementedValueType(t *testing.T) {
	n := NotImplementedValue{}
	// Actual type is "NotImplementedType"
	if n.Type() != Type("NotImplementedType") {
		t.Errorf("Expected type 'NotImplementedType', got '%s'", n.Type())
	}
}

func TestNotImplementedValueString(t *testing.T) {
	n := NotImplementedValue{}
	if n.String() != "NotImplemented" {
		t.Errorf("Expected 'NotImplemented', got '%s'", n.String())
	}
}

// ============== BuiltinFunction Tests ==============

func TestNewBuiltinFunction(t *testing.T) {
	fn := NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
		return NumberValue(42), nil
	})

	if fn == nil {
		t.Fatal("NewBuiltinFunction should not return nil")
	}
}

func TestBuiltinFunctionCall(t *testing.T) {
	fn := NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
		if len(args) > 0 {
			if num, ok := args[0].(NumberValue); ok {
				return NumberValue(float64(num) * 2), nil
			}
		}
		return NumberValue(0), nil
	})

	ctx := NewContext(nil)
	result, err := fn.Call([]Value{NumberValue(21)}, ctx)
	if err != nil {
		t.Errorf("Call failed: %v", err)
	}

	if num, ok := result.(NumberValue); !ok || float64(num) != 42 {
		t.Errorf("Expected 42, got %v", result)
	}
}

func TestNewNamedBuiltinFunction(t *testing.T) {
	fn := NewNamedBuiltinFunction("myFunc", func(args []Value, ctx *Context) (Value, error) {
		return Nil, nil
	})

	// Check by using GetAttr for __name__
	name, ok := fn.GetAttr("__name__")
	if !ok {
		t.Error("Should have __name__ attribute")
	}
	if str, ok := name.(StringValue); !ok || string(str) != "myFunc" {
		t.Errorf("Expected name 'myFunc', got '%v'", name)
	}
}

// ============== IsInteger / IsInSafeRange Tests ==============

func TestIsInteger(t *testing.T) {
	if !IsInteger(42.0) {
		t.Error("42.0 should be an integer")
	}

	if IsInteger(3.14) {
		t.Error("3.14 should not be an integer")
	}

	if !IsInteger(-10.0) {
		t.Error("-10.0 should be an integer")
	}
}

func TestIsInSafeRange(t *testing.T) {
	if !IsInSafeRange(1e10) {
		t.Error("1e10 should be in safe range")
	}

	// Very large number might not be safe
	// The threshold is typically 2^53 for JavaScript-style safe integers
}

// ============== True/False Constants ==============

func TestBoolConstants(t *testing.T) {
	if True != BoolValue(true) {
		t.Error("True constant should be BoolValue(true)")
	}

	if False != BoolValue(false) {
		t.Error("False constant should be BoolValue(false)")
	}
}
