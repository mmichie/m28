package core

import (
	"testing"
)

// ============== EqualValues Tests ==============

func TestEqualValuesNumbers(t *testing.T) {
	if !EqualValues(NumberValue(42), NumberValue(42)) {
		t.Error("Equal numbers should be equal")
	}

	if EqualValues(NumberValue(1), NumberValue(2)) {
		t.Error("Different numbers should not be equal")
	}
}

func TestEqualValuesStrings(t *testing.T) {
	if !EqualValues(StringValue("hello"), StringValue("hello")) {
		t.Error("Equal strings should be equal")
	}

	if EqualValues(StringValue("hello"), StringValue("world")) {
		t.Error("Different strings should not be equal")
	}
}

func TestEqualValuesBools(t *testing.T) {
	if !EqualValues(BoolValue(true), BoolValue(true)) {
		t.Error("Equal bools should be equal")
	}

	if EqualValues(BoolValue(true), BoolValue(false)) {
		t.Error("Different bools should not be equal")
	}
}

func TestEqualValuesNil(t *testing.T) {
	if !EqualValues(Nil, Nil) {
		t.Error("Nil should equal Nil")
	}
}

func TestEqualValuesDifferentTypes(t *testing.T) {
	if EqualValues(NumberValue(1), StringValue("1")) {
		t.Error("Different types should not be equal")
	}
}

func TestEqualValuesLists(t *testing.T) {
	l1 := NewList(NumberValue(1), NumberValue(2))
	l2 := NewList(NumberValue(1), NumberValue(2))
	l3 := NewList(NumberValue(1), NumberValue(3))

	if !EqualValues(l1, l2) {
		t.Error("Equal lists should be equal")
	}

	if EqualValues(l1, l3) {
		t.Error("Different lists should not be equal")
	}
}

func TestEqualValuesTuples(t *testing.T) {
	t1 := TupleValue{NumberValue(1), NumberValue(2)}
	t2 := TupleValue{NumberValue(1), NumberValue(2)}
	t3 := TupleValue{NumberValue(1), NumberValue(3)}

	if !EqualValues(t1, t2) {
		t.Error("Equal tuples should be equal")
	}

	if EqualValues(t1, t3) {
		t.Error("Different tuples should not be equal")
	}
}

func TestEqualValuesDicts(t *testing.T) {
	d1 := NewDict()
	d1.Set("key", NumberValue(1))

	d2 := NewDict()
	d2.Set("key", NumberValue(1))

	d3 := NewDict()
	d3.Set("key", NumberValue(2))

	if !EqualValues(d1, d2) {
		t.Error("Equal dicts should be equal")
	}

	if EqualValues(d1, d3) {
		t.Error("Different dicts should not be equal")
	}
}

// ============== IsHashable Tests ==============

func TestIsHashableNumbers(t *testing.T) {
	if !IsHashable(NumberValue(42)) {
		t.Error("Numbers should be hashable")
	}
}

func TestIsHashableStrings(t *testing.T) {
	if !IsHashable(StringValue("hello")) {
		t.Error("Strings should be hashable")
	}
}

func TestIsHashableBools(t *testing.T) {
	if !IsHashable(BoolValue(true)) {
		t.Error("Bools should be hashable")
	}
}

func TestIsHashableTuples(t *testing.T) {
	tuple := TupleValue{NumberValue(1), NumberValue(2)}
	if !IsHashable(tuple) {
		t.Error("Tuples of hashable values should be hashable")
	}
}

func TestIsHashableLists(t *testing.T) {
	list := NewList(NumberValue(1))
	if IsHashable(list) {
		t.Error("Lists should not be hashable")
	}
}

func TestIsHashableDicts(t *testing.T) {
	dict := NewDict()
	if IsHashable(dict) {
		t.Error("Dicts should not be hashable")
	}
}

func TestIsHashableSets(t *testing.T) {
	set := NewSet()
	if IsHashable(set) {
		t.Error("Sets should not be hashable")
	}
}

// ============== ValueToKey Tests ==============

func TestValueToKeyNumber(t *testing.T) {
	key := ValueToKey(NumberValue(42))
	if key == "" {
		t.Error("ValueToKey should return non-empty key for number")
	}
}

func TestValueToKeyString(t *testing.T) {
	key := ValueToKey(StringValue("hello"))
	if key == "" {
		t.Error("ValueToKey should return non-empty key for string")
	}
}

func TestValueToKeyConsistency(t *testing.T) {
	// Same value should produce same key
	k1 := ValueToKey(NumberValue(42))
	k2 := ValueToKey(NumberValue(42))

	if k1 != k2 {
		t.Error("Same value should produce same key")
	}
}

// ============== Compare Tests ==============

func TestCompareNumbers(t *testing.T) {
	if Compare(NumberValue(1), NumberValue(2)) >= 0 {
		t.Error("1 < 2")
	}

	if Compare(NumberValue(2), NumberValue(1)) <= 0 {
		t.Error("2 > 1")
	}

	if Compare(NumberValue(1), NumberValue(1)) != 0 {
		t.Error("1 == 1")
	}
}

func TestCompareStrings(t *testing.T) {
	if Compare(StringValue("a"), StringValue("b")) >= 0 {
		t.Error("'a' < 'b'")
	}

	if Compare(StringValue("b"), StringValue("a")) <= 0 {
		t.Error("'b' > 'a'")
	}

	if Compare(StringValue("a"), StringValue("a")) != 0 {
		t.Error("'a' == 'a'")
	}
}

// ============== IsTruthy Tests ==============

func TestIsTruthyNumbers(t *testing.T) {
	if !IsTruthy(NumberValue(1)) {
		t.Error("1 should be truthy")
	}

	if !IsTruthy(NumberValue(-1)) {
		t.Error("-1 should be truthy")
	}

	if IsTruthy(NumberValue(0)) {
		t.Error("0 should be falsy")
	}
}

func TestIsTruthyStrings(t *testing.T) {
	if !IsTruthy(StringValue("hello")) {
		t.Error("Non-empty string should be truthy")
	}

	if IsTruthy(StringValue("")) {
		t.Error("Empty string should be falsy")
	}
}

func TestIsTruthyBools(t *testing.T) {
	if !IsTruthy(BoolValue(true)) {
		t.Error("True should be truthy")
	}

	if IsTruthy(BoolValue(false)) {
		t.Error("False should be falsy")
	}
}

func TestIsTruthyNil(t *testing.T) {
	if IsTruthy(Nil) {
		t.Error("Nil should be falsy")
	}
}

func TestIsTruthyLists(t *testing.T) {
	if !IsTruthy(NewList(NumberValue(1))) {
		t.Error("Non-empty list should be truthy")
	}

	if IsTruthy(NewList()) {
		t.Error("Empty list should be falsy")
	}
}

func TestIsTruthyDicts(t *testing.T) {
	d := NewDict()
	d.Set("key", NumberValue(1))
	if !IsTruthy(d) {
		t.Error("Non-empty dict should be truthy")
	}

	if IsTruthy(NewDict()) {
		t.Error("Empty dict should be falsy")
	}
}

// ============== PrintValue Tests ==============

func TestPrintValueNumber(t *testing.T) {
	s := PrintValue(NumberValue(42))
	if s != "42" {
		t.Errorf("Expected '42', got '%s'", s)
	}
}

func TestPrintValueString(t *testing.T) {
	s := PrintValue(StringValue("hello"))
	// Should include quotes
	if s != `"hello"` {
		t.Errorf("Expected '\"hello\"', got '%s'", s)
	}
}

func TestPrintValueWithoutQuotesString(t *testing.T) {
	s := PrintValueWithoutQuotes(StringValue("hello"))
	if s != "hello" {
		t.Errorf("Expected 'hello', got '%s'", s)
	}
}

// ============== ProcessEscapeSequences Tests ==============

func TestProcessEscapeSequences(t *testing.T) {
	// ProcessEscapeSequences is a stub that returns input as-is
	// Just verify it doesn't panic
	result := ProcessEscapeSequences("hello\\nworld")
	if result == "" && len("hello\\nworld") > 0 {
		t.Error("Should return non-empty for non-empty input")
	}
}
