package core

import (
	"fmt"
	"testing"
)

func kwargNames(k *Kwargs) []string {
	out := make([]string, 0, k.Len())
	for _, e := range k.Entries() {
		out = append(out, e.Name)
	}
	return out
}

func TestKwargsNilSafety(t *testing.T) {
	var k *Kwargs
	if k.Len() != 0 {
		t.Errorf("nil Len() = %d, want 0", k.Len())
	}
	if _, ok := k.Get("x"); ok {
		t.Error("nil Get() reported a hit")
	}
	if k.Has("x") {
		t.Error("nil Has() reported true")
	}
	if k.Entries() != nil {
		t.Error("nil Entries() != nil")
	}
	if d := k.ToDict(); d.Size() != 0 {
		t.Errorf("nil ToDict() size = %d, want 0", d.Size())
	}
	if c := k.Clone(); c == nil || c.Len() != 0 {
		t.Error("nil Clone() should be an empty non-nil Kwargs")
	}
}

func TestKwargsInsertionOrder(t *testing.T) {
	k := NewKwargs(0)
	names := []string{"z", "m", "a", "k", "b", "q", "c", "w"}
	for i, n := range names {
		k.Set(n, NumberValue(i))
	}
	got := kwargNames(k)
	for i, n := range names {
		if got[i] != n {
			t.Fatalf("order mismatch at %d: got %v, want %v", i, got, names)
		}
	}
	// Overwrite keeps the original position.
	k.Set("a", NumberValue(99))
	if got := kwargNames(k); got[2] != "a" {
		t.Errorf("overwrite moved key: %v", got)
	}
	if v, _ := k.Get("a"); v != NumberValue(99) {
		t.Errorf("overwrite did not update value: %v", v)
	}
	if k.Len() != len(names) {
		t.Errorf("Len() = %d, want %d", k.Len(), len(names))
	}
}

func TestKwargsDelete(t *testing.T) {
	k := NewKwargs(0)
	for _, n := range []string{"a", "b", "c"} {
		k.Set(n, StringValue(n))
	}
	if !k.Delete("b") {
		t.Fatal("Delete(b) = false")
	}
	if k.Delete("b") {
		t.Fatal("second Delete(b) = true")
	}
	got := kwargNames(k)
	if len(got) != 2 || got[0] != "a" || got[1] != "c" {
		t.Errorf("after delete: %v, want [a c]", got)
	}
}

func TestKwargsIndexThreshold(t *testing.T) {
	// Push past kwargsIdxThreshold to exercise the indexed path.
	k := NewKwargs(0)
	n := kwargsIdxThreshold * 2
	for i := 0; i < n; i++ {
		k.Set(fmt.Sprintf("key%02d", i), NumberValue(i))
	}
	if k.idx == nil {
		t.Fatal("index not built past threshold")
	}
	for i := 0; i < n; i++ {
		name := fmt.Sprintf("key%02d", i)
		v, ok := k.Get(name)
		if !ok || v != NumberValue(i) {
			t.Fatalf("indexed Get(%s) = %v, %v", name, v, ok)
		}
	}
	// Order still intact.
	got := kwargNames(k)
	for i := 0; i < n; i++ {
		if got[i] != fmt.Sprintf("key%02d", i) {
			t.Fatalf("indexed order broken at %d: %s", i, got[i])
		}
	}
	// Deleting below the threshold drops the index but keeps order.
	for i := 0; i < n-2; i++ {
		k.Delete(fmt.Sprintf("key%02d", i))
	}
	if k.idx != nil {
		t.Error("index not dropped below threshold")
	}
	got = kwargNames(k)
	if len(got) != 2 || got[0] != fmt.Sprintf("key%02d", n-2) {
		t.Errorf("post-delete order: %v", got)
	}
}

func TestKwargsCloneIndependence(t *testing.T) {
	k := NewKwargs(0)
	k.Set("a", NumberValue(1))
	c := k.Clone()
	c.Set("a", NumberValue(2))
	c.Set("b", NumberValue(3))
	if v, _ := k.Get("a"); v != NumberValue(1) {
		t.Error("Clone shares storage with original")
	}
	if k.Has("b") {
		t.Error("Clone append leaked into original")
	}
}

func TestKwargsToDictOrder(t *testing.T) {
	k := NewKwargs(0)
	names := []string{"z", "a", "m"}
	for i, n := range names {
		k.Set(n, NumberValue(i))
	}
	d := k.ToDict()
	if d.Size() != 3 {
		t.Fatalf("ToDict size = %d", d.Size())
	}
	i := 0
	d.ForEach(func(key, _ Value) bool {
		if string(key.(StringValue)) != names[i] {
			t.Errorf("ToDict order at %d: %v, want %s", i, key, names[i])
		}
		i++
		return true
	})
	// ToDict must be a fresh dict each call (no aliasing).
	if k.ToDict() == d {
		t.Error("ToDict returned a shared dict")
	}
}
