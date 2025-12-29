package core

import (
	"strings"
	"testing"
)

// ============== ListValue Tests ==============

func TestNewList(t *testing.T) {
	// Empty list
	l1 := NewList()
	if l1.Len() != 0 {
		t.Errorf("Expected empty list, got length %d", l1.Len())
	}

	// List with values
	l2 := NewList(NumberValue(1), NumberValue(2), NumberValue(3))
	if l2.Len() != 3 {
		t.Errorf("Expected length 3, got %d", l2.Len())
	}
}

func TestListItems(t *testing.T) {
	l := NewList(NumberValue(1), NumberValue(2))
	items := l.Items()

	if len(items) != 2 {
		t.Errorf("Expected 2 items, got %d", len(items))
	}

	// Verify items are copies
	items[0] = NumberValue(99)
	origItems := l.Items()
	if num, ok := origItems[0].(NumberValue); !ok || float64(num) != 1 {
		t.Error("Items() should return a copy, not modify original")
	}
}

func TestListGetItem(t *testing.T) {
	l := NewList(StringValue("a"), StringValue("b"), StringValue("c"))

	// Positive index
	val, err := l.GetItem(0)
	if err != nil {
		t.Errorf("GetItem(0) failed: %v", err)
	}
	if str, ok := val.(StringValue); !ok || string(str) != "a" {
		t.Errorf("Expected 'a', got %v", val)
	}

	// Negative index
	val, err = l.GetItem(-1)
	if err != nil {
		t.Errorf("GetItem(-1) failed: %v", err)
	}
	if str, ok := val.(StringValue); !ok || string(str) != "c" {
		t.Errorf("Expected 'c', got %v", val)
	}

	// Out of bounds
	_, err = l.GetItem(10)
	if err == nil {
		t.Error("Expected error for out of bounds index")
	}
}

func TestListSetItem(t *testing.T) {
	l := NewList(NumberValue(1), NumberValue(2), NumberValue(3))

	// Set positive index
	err := l.SetItem(1, NumberValue(99))
	if err != nil {
		t.Errorf("SetItem failed: %v", err)
	}

	val, _ := l.GetItem(1)
	if num, ok := val.(NumberValue); !ok || float64(num) != 99 {
		t.Errorf("Expected 99, got %v", val)
	}

	// Set negative index
	err = l.SetItem(-1, NumberValue(100))
	if err != nil {
		t.Errorf("SetItem(-1) failed: %v", err)
	}

	val, _ = l.GetItem(-1)
	if num, ok := val.(NumberValue); !ok || float64(num) != 100 {
		t.Errorf("Expected 100, got %v", val)
	}

	// Out of bounds
	err = l.SetItem(10, NumberValue(0))
	if err == nil {
		t.Error("Expected error for out of bounds index")
	}
}

func TestListAppend(t *testing.T) {
	l := NewList()
	l.Append(NumberValue(1))
	l.Append(NumberValue(2))

	if l.Len() != 2 {
		t.Errorf("Expected length 2, got %d", l.Len())
	}

	val, _ := l.GetItem(1)
	if num, ok := val.(NumberValue); !ok || float64(num) != 2 {
		t.Errorf("Expected 2, got %v", val)
	}
}

func TestListExtend(t *testing.T) {
	l := NewList(NumberValue(1))
	l.Extend([]Value{NumberValue(2), NumberValue(3)})

	if l.Len() != 3 {
		t.Errorf("Expected length 3, got %d", l.Len())
	}
}

func TestListIterator(t *testing.T) {
	l := NewList(NumberValue(1), NumberValue(2), NumberValue(3))
	iter := l.Iterator()

	count := 0
	for {
		_, ok := iter.Next()
		if !ok {
			break
		}
		count++
	}

	if count != 3 {
		t.Errorf("Expected 3 iterations, got %d", count)
	}

	// Test reset
	iter.Reset()
	val, ok := iter.Next()
	if !ok {
		t.Error("Iterator should have value after reset")
	}
	if num, ok := val.(NumberValue); !ok || float64(num) != 1 {
		t.Errorf("Expected 1 after reset, got %v", val)
	}
}

func TestListString(t *testing.T) {
	l := NewList(NumberValue(1), NumberValue(2))
	str := l.String()

	if str != "[1, 2]" {
		t.Errorf("Expected '[1, 2]', got %q", str)
	}
}

func TestListType(t *testing.T) {
	l := NewList()
	if l.Type() != Type("list") {
		t.Errorf("Expected type 'list', got '%s'", l.Type())
	}
}

// ============== DictValue Tests ==============

func TestNewDict(t *testing.T) {
	d := NewDict()

	if d.Size() != 0 {
		t.Errorf("Expected empty dict, got size %d", d.Size())
	}
}

func TestDictSetGet(t *testing.T) {
	d := NewDict()

	d.Set("key1", NumberValue(42))

	val, ok := d.Get("key1")
	if !ok {
		t.Error("Key should exist")
	}
	if num, ok := val.(NumberValue); !ok || float64(num) != 42 {
		t.Errorf("Expected 42, got %v", val)
	}

	// Non-existent key
	_, ok = d.Get("nonexistent")
	if ok {
		t.Error("Non-existent key should return false")
	}
}

func TestDictSetValueGetValue(t *testing.T) {
	d := NewDict()

	// Set with Value key
	err := d.SetValue(StringValue("key"), NumberValue(100))
	if err != nil {
		t.Errorf("SetValue failed: %v", err)
	}

	// Get with Value key
	val, ok := d.GetValue(StringValue("key"))
	if !ok {
		t.Error("Key should exist")
	}
	if num, ok := val.(NumberValue); !ok || float64(num) != 100 {
		t.Errorf("Expected 100, got %v", val)
	}
}

func TestDictDelete(t *testing.T) {
	d := NewDict()
	d.Set("key", NumberValue(1))

	d.Delete("key")

	_, ok := d.Get("key")
	if ok {
		t.Error("Key should be deleted")
	}
}

func TestDictDeleteValue(t *testing.T) {
	d := NewDict()
	d.SetValue(NumberValue(42), StringValue("value"))

	deleted := d.DeleteValue(NumberValue(42))
	if !deleted {
		t.Error("DeleteValue should return true for existing key")
	}

	deleted = d.DeleteValue(NumberValue(99))
	if deleted {
		t.Error("DeleteValue should return false for non-existent key")
	}
}

func TestDictKeys(t *testing.T) {
	d := NewDict()
	d.Set("a", NumberValue(1))
	d.Set("b", NumberValue(2))

	keys := d.Keys()
	if len(keys) != 2 {
		t.Errorf("Expected 2 keys, got %d", len(keys))
	}
}

func TestDictOriginalKeys(t *testing.T) {
	d := NewDict()
	d.SetValue(NumberValue(1), StringValue("one"))
	d.SetValue(NumberValue(2), StringValue("two"))

	keys := d.OriginalKeys()
	if len(keys) != 2 {
		t.Errorf("Expected 2 original keys, got %d", len(keys))
	}

	// Check that keys are NumberValues, not strings
	foundNum := false
	for _, k := range keys {
		if _, ok := k.(NumberValue); ok {
			foundNum = true
			break
		}
	}
	if !foundNum {
		t.Error("OriginalKeys should return original Value types")
	}
}

func TestDictIterator(t *testing.T) {
	d := NewDict()
	d.Set("a", NumberValue(1))
	d.Set("b", NumberValue(2))

	iter := d.Iterator()
	count := 0
	for {
		_, ok := iter.Next()
		if !ok {
			break
		}
		count++
	}

	if count != 2 {
		t.Errorf("Expected 2 iterations, got %d", count)
	}
}

func TestDictString(t *testing.T) {
	d := NewDict()
	d.Set("key", NumberValue(42))

	str := d.String()
	// Dict string format may vary, just check it contains key-value
	if !strings.Contains(str, "key") || !strings.Contains(str, "42") {
		t.Errorf("String should contain key and value, got %q", str)
	}
}

func TestDictType(t *testing.T) {
	d := NewDict()
	if d.Type() != Type("dict") {
		t.Errorf("Expected type 'dict', got '%s'", d.Type())
	}
}

// ============== TupleValue Tests ==============

func TestTupleBasic(t *testing.T) {
	tuple := TupleValue{NumberValue(1), NumberValue(2), NumberValue(3)}

	if len(tuple) != 3 {
		t.Errorf("Expected length 3, got %d", len(tuple))
	}
}

func TestTupleGetItem(t *testing.T) {
	tuple := TupleValue{StringValue("a"), StringValue("b")}

	val, err := tuple.GetItem(0)
	if err != nil {
		t.Errorf("GetItem failed: %v", err)
	}
	if str, ok := val.(StringValue); !ok || string(str) != "a" {
		t.Errorf("Expected 'a', got %v", val)
	}

	// Negative index
	val, err = tuple.GetItem(-1)
	if err != nil {
		t.Errorf("GetItem(-1) failed: %v", err)
	}
	if str, ok := val.(StringValue); !ok || string(str) != "b" {
		t.Errorf("Expected 'b', got %v", val)
	}

	// Out of bounds
	_, err = tuple.GetItem(10)
	if err == nil {
		t.Error("Expected error for out of bounds")
	}
}

func TestTupleIterator(t *testing.T) {
	tuple := TupleValue{NumberValue(1), NumberValue(2)}
	iter := tuple.Iterator()

	count := 0
	for {
		_, ok := iter.Next()
		if !ok {
			break
		}
		count++
	}

	if count != 2 {
		t.Errorf("Expected 2 iterations, got %d", count)
	}
}

func TestTupleString(t *testing.T) {
	tuple := TupleValue{NumberValue(1), NumberValue(2)}
	str := tuple.String()

	if str != "(1, 2)" {
		t.Errorf("Expected '(1, 2)', got %q", str)
	}

	// Single element tuple
	single := TupleValue{NumberValue(1)}
	str = single.String()
	if str != "(1,)" {
		t.Errorf("Expected '(1,)', got %q", str)
	}

	// Empty tuple
	empty := TupleValue{}
	str = empty.String()
	if str != "()" {
		t.Errorf("Expected '()', got %q", str)
	}
}

func TestTupleType(t *testing.T) {
	tuple := TupleValue{}
	if tuple.Type() != Type("tuple") {
		t.Errorf("Expected type 'tuple', got '%s'", tuple.Type())
	}
}

// ============== SetValue Tests ==============

func TestNewSet(t *testing.T) {
	s := NewSet()
	if s.Size() != 0 {
		t.Errorf("Expected empty set, got size %d", s.Size())
	}
}

func TestSetAddContains(t *testing.T) {
	s := NewSet()

	s.Add(NumberValue(1))
	s.Add(NumberValue(2))

	if !s.Contains(NumberValue(1)) {
		t.Error("Set should contain 1")
	}

	if !s.Contains(NumberValue(2)) {
		t.Error("Set should contain 2")
	}

	if s.Contains(NumberValue(3)) {
		t.Error("Set should not contain 3")
	}
}

func TestSetDuplicates(t *testing.T) {
	s := NewSet()

	s.Add(NumberValue(1))
	s.Add(NumberValue(1))
	s.Add(NumberValue(1))

	if s.Size() != 1 {
		t.Errorf("Set should deduplicate, expected size 1, got %d", s.Size())
	}
}

func TestSetRemove(t *testing.T) {
	s := NewSet()
	s.Add(NumberValue(1))
	s.Add(NumberValue(2))

	s.Remove(NumberValue(1))

	if s.Contains(NumberValue(1)) {
		t.Error("1 should be removed")
	}

	if !s.Contains(NumberValue(2)) {
		t.Error("2 should still exist")
	}
}

func TestSetItems(t *testing.T) {
	s := NewSet()
	s.Add(NumberValue(1))
	s.Add(NumberValue(2))

	items := s.Items()
	if len(items) != 2 {
		t.Errorf("Expected 2 items, got %d", len(items))
	}
}

func TestSetIterator(t *testing.T) {
	s := NewSet()
	s.Add(NumberValue(1))
	s.Add(NumberValue(2))

	iter := s.Iterator()
	count := 0
	for {
		_, ok := iter.Next()
		if !ok {
			break
		}
		count++
	}

	if count != 2 {
		t.Errorf("Expected 2 iterations, got %d", count)
	}
}

func TestSetType(t *testing.T) {
	s := NewSet()
	if s.Type() != Type("set") {
		t.Errorf("Expected type 'set', got '%s'", s.Type())
	}
}

// ============== SliceValue Tests ==============

func TestSliceValue(t *testing.T) {
	// Slice with all values
	slice := &SliceValue{
		Start: NumberValue(1),
		Stop:  NumberValue(10),
		Step:  NumberValue(2),
	}

	if slice.Type() != Type("slice") {
		t.Errorf("Expected type 'slice', got '%s'", slice.Type())
	}

	str := slice.String()
	if !strings.Contains(str, "slice") {
		t.Errorf("String should contain 'slice', got %q", str)
	}
}

func TestSliceGetAttr(t *testing.T) {
	slice := &SliceValue{
		Start: NumberValue(1),
		Stop:  NumberValue(10),
		Step:  NumberValue(2),
	}

	start, ok := slice.GetAttr("start")
	if !ok {
		t.Error("start attr should exist")
	}
	if num, ok := start.(NumberValue); !ok || float64(num) != 1 {
		t.Errorf("Expected start=1, got %v", start)
	}

	stop, ok := slice.GetAttr("stop")
	if !ok {
		t.Error("stop attr should exist")
	}
	if num, ok := stop.(NumberValue); !ok || float64(num) != 10 {
		t.Errorf("Expected stop=10, got %v", stop)
	}

	step, ok := slice.GetAttr("step")
	if !ok {
		t.Error("step attr should exist")
	}
	if num, ok := step.(NumberValue); !ok || float64(num) != 2 {
		t.Errorf("Expected step=2, got %v", step)
	}
}
