package core

import (
	"math"
	"math/big"
	"testing"
)

// mustHash fails the test on error.
func mustHash(t *testing.T, v Value) uint64 {
	t.Helper()
	h, err := HashValue(v, nil)
	if err != nil {
		t.Fatalf("HashValue(%v) error: %v", v, err)
	}
	return h
}

// TestHashNumericUnification: the Python invariant x == y => hash(x) == hash(y)
// across int/float/bool/bigint representations.
func TestHashNumericUnification(t *testing.T) {
	cases := []struct{ a, b Value }{
		{NumberValue(1), FloatValue(1.0)},
		{NumberValue(1), BoolValue(true)},
		{NumberValue(0), BoolValue(false)},
		{NumberValue(0), FloatValue(-0.0)}, // -0.0 == 0
		{NumberValue(4), NewBigIntFromInt64(4)},
		{FloatValue(2.5), FloatValue(2.5)},
		{NumberValue(-7), FloatValue(-7.0)},
		{FloatValue(1e20), NewBigInt(new(big.Int).SetUint64(0)).addPow10(20)},
		{ComplexValue(complex(3, 0)), NumberValue(3)}, // complex(3,0) == 3
	}
	for _, c := range cases {
		if mustHash(t, c.a) != mustHash(t, c.b) {
			t.Errorf("hash(%v) != hash(%v) but values are equal", c.a, c.b)
		}
	}
}

// addPow10 builds 10^n as a BigIntValue for the test above.
func (b BigIntValue) addPow10(n int64) BigIntValue {
	v := new(big.Int).Exp(big.NewInt(10), big.NewInt(n), nil)
	return NewBigInt(v)
}

// TestHashSmallIntsIdentity: CPython hashes small ints to themselves.
func TestHashSmallIntsIdentity(t *testing.T) {
	for _, n := range []int64{0, 1, 2, 42, 1000000, -5, -1000000} {
		want := n
		if want == -1 {
			want = -2
		}
		if got := int64(mustHash(t, NumberValue(n))); got != want {
			t.Errorf("hash(%d) = %d, want %d", n, got, want)
		}
	}
	if got := int64(mustHash(t, NumberValue(-1))); got != -2 {
		t.Errorf("hash(-1) = %d, want -2 (CPython reserves -1)", got)
	}
}

// TestHashBigIntModulus checks the 2^61-1 reduction against known values.
func TestHashBigIntModulus(t *testing.T) {
	// 10^20 mod (2^61 - 1) = 848750603811160107 (CPython: hash(10**20))
	v, _ := NewBigIntFromString("100000000000000000000", 10)
	if got := int64(mustHash(t, v)); got != 848750603811160107 {
		t.Errorf("hash(10**20) = %d, want 848750603811160107", got)
	}
	// Negative counterpart is symmetric.
	vn, _ := NewBigIntFromString("-100000000000000000000", 10)
	if got := int64(mustHash(t, vn)); got != -848750603811160107 {
		t.Errorf("hash(-10**20) = %d, want -848750603811160107", got)
	}
}

// TestHashFloatSpecials covers inf/nan and large-but-exact doubles.
func TestHashFloatSpecials(t *testing.T) {
	if got := int64(mustHash(t, FloatValue(math.Inf(1)))); got != 314159 {
		t.Errorf("hash(inf) = %d, want 314159", got)
	}
	if got := int64(mustHash(t, FloatValue(math.Inf(-1)))); got != -314159 {
		t.Errorf("hash(-inf) = %d, want -314159", got)
	}
	// 2^70 is integral but beyond the fast path; must agree with the bigint.
	big70 := NewBigInt(new(big.Int).Lsh(big.NewInt(1), 70))
	if mustHash(t, FloatValue(math.Ldexp(1, 70))) != mustHash(t, big70) {
		t.Error("hash(2.0**70) != hash(2**70)")
	}
}

// TestHashTupleEquality: equal tuples hash equal, different ones (nearly
// always) do not — including the encodings that collided under ValueToKey.
func TestHashTupleEquality(t *testing.T) {
	t1 := TupleValue{StringValue("a"), StringValue("b")}
	t2 := TupleValue{StringValue("a"), StringValue("b")}
	if mustHash(t, t1) != mustHash(t, t2) {
		t.Error("equal tuples must hash equal")
	}
	// The classic smuggling collision from the old encoding.
	smuggled := TupleValue{StringValue("a,s:b")}
	if mustHash(t, t1) == mustHash(t, smuggled) {
		t.Error(`("a","b") and ("a,s:b",) should not collide`)
	}
	// Deep nesting must not collapse.
	deep := func(leaf Value) Value {
		v := leaf
		for i := 0; i < 10; i++ {
			v = TupleValue{v}
		}
		return v
	}
	if mustHash(t, deep(NumberValue(1))) == mustHash(t, deep(NumberValue(2))) {
		t.Error("deeply nested distinct tuples should not collide")
	}
	// Unhashable element propagates.
	if _, err := HashValue(TupleValue{NewList()}, nil); err == nil {
		t.Error("tuple containing a list must be unhashable")
	}
}

// TestHashUnhashable: mutable containers are unhashable.
func TestHashUnhashable(t *testing.T) {
	for _, v := range []Value{NewList(), NewDict(), NewSet()} {
		if _, err := HashValue(v, nil); err == nil {
			t.Errorf("%s must be unhashable", v.Type())
		}
	}
}

// TestDictEngineBasics exercises insert/lookup/delete/ordering.
func TestDictEngineBasics(t *testing.T) {
	d := NewDict()
	if err := d.SetItem(NumberValue(1), StringValue("one"), nil); err != nil {
		t.Fatal(err)
	}
	d.SetStr("name", StringValue("m28"))
	if err := d.SetItem(TupleValue{NumberValue(1), NumberValue(2)}, StringValue("tup"), nil); err != nil {
		t.Fatal(err)
	}

	// int/float unify; first key object is kept
	if err := d.SetItem(FloatValue(1.0), StringValue("uno"), nil); err != nil {
		t.Fatal(err)
	}
	if d.Size() != 3 {
		t.Fatalf("size = %d, want 3", d.Size())
	}
	v, ok, _ := d.GetItem(NumberValue(1), nil)
	if !ok || v.(StringValue) != "uno" {
		t.Fatalf("d[1] = %v, want uno", v)
	}
	keys := d.OriginalKeys()
	if _, isNum := keys[0].(NumberValue); !isNum {
		t.Errorf("first-inserted key object should remain NumberValue, got %T", keys[0])
	}

	// GetStr fast path agrees with GetItem
	if v, ok := d.GetStr("name"); !ok || v.(StringValue) != "m28" {
		t.Fatal("GetStr miss")
	}
	if v, ok, _ := d.GetItem(StringValue("name"), nil); !ok || v.(StringValue) != "m28" {
		t.Fatal("GetItem(string) miss")
	}

	// Delete and reinsert preserves Python 3.7 order semantics
	if removed, _ := d.DelItem(NumberValue(1), nil); !removed {
		t.Fatal("delete failed")
	}
	if d.Size() != 2 {
		t.Fatalf("size after delete = %d", d.Size())
	}
	d.SetStr("last", None)
	got := d.OriginalKeys()
	if len(got) != 3 {
		t.Fatalf("keys = %d, want 3", len(got))
	}
	if s, ok := got[len(got)-1].(StringValue); !ok || s != "last" {
		t.Errorf("insertion order broken: last key = %v", got[len(got)-1])
	}
}

// TestDictEngineChurn stresses tombstone compaction and growth.
func TestDictEngineChurn(t *testing.T) {
	d := NewDict()
	const n = 10000
	for i := 0; i < n; i++ {
		if err := d.SetItem(NumberValue(i), NumberValue(i*2), nil); err != nil {
			t.Fatal(err)
		}
	}
	// Delete every other key
	for i := 0; i < n; i += 2 {
		if removed, _ := d.DelItem(NumberValue(i), nil); !removed {
			t.Fatalf("delete %d failed", i)
		}
	}
	if d.Size() != n/2 {
		t.Fatalf("size = %d, want %d", d.Size(), n/2)
	}
	// Everything odd still resolves; everything even is gone
	for i := 0; i < n; i++ {
		v, ok, _ := d.GetItem(NumberValue(i), nil)
		if i%2 == 0 && ok {
			t.Fatalf("key %d should be gone", i)
		}
		if i%2 == 1 {
			if !ok {
				t.Fatalf("key %d lost", i)
			}
			if int(v.(NumberValue)) != i*2 {
				t.Fatalf("key %d value corrupted: %v", i, v)
			}
		}
	}
	// Order survives churn
	keys := d.OriginalKeys()
	for j := 1; j < len(keys); j++ {
		if int(keys[j].(NumberValue)) <= int(keys[j-1].(NumberValue)) {
			t.Fatal("insertion order lost after churn")
		}
	}
	// PopLast drains in LIFO order
	k, _, ok := d.PopLast()
	if !ok || int(k.(NumberValue)) != n-1 {
		t.Fatalf("PopLast = %v, want %d", k, n-1)
	}
}
