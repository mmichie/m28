package core

import (
	"fmt"
	"hash/maphash"
	"math"
	"math/big"
	"reflect"
)

// This file implements Python-compatible value hashing for the dict/set
// engine. The single invariant everything else relies on:
//
//	EqualValues(x, y) (or __eq__) true  =>  HashValue(x) == HashValue(y)
//
// Numbers follow CPython's modular scheme (modulus 2^61-1) so that equal
// values hash equal across NumberValue/FloatValue/BoolValue/BigIntValue.
// Strings use hash/maphash with a per-process seed (CPython randomizes
// per process too). Classes hash by name because class __eq__ is
// name-based; symbols hash like their text for the same reason.

const (
	pyHashModulus = uint64(1)<<61 - 1 // 2^61 - 1, CPython's _PyHASH_MODULUS
	pyHashBits    = 61
	pyHashInf     = int64(314159)

	xxPrime1 = uint64(11400714785074694791)
	xxPrime2 = uint64(14029467366897019727)
	xxPrime5 = uint64(2870177450012600261)
)

var stringHashSeed = maphash.MakeSeed()

// hashString hashes string content. CPython pins hash("") == 0 regardless of
// hash randomization; some code relies on it, and it costs nothing.
func hashString(s string) uint64 {
	if len(s) == 0 {
		return 0
	}
	return maphash.String(stringHashSeed, s)
}

func hashBytes(b []byte) uint64 {
	if len(b) == 0 {
		return 0
	}
	return maphash.Bytes(stringHashSeed, b)
}

// pyHashDouble is CPython's _Py_HashDouble: an exact modular hash of the
// float's value, guaranteeing hash(4.0) == hash(4) == hash(BigInt(4)).
func pyHashDouble(v float64) int64 {
	// Fast path: integral values below the modulus hash to themselves
	// (n mod (2^61-1) == n). This is the overwhelmingly common dict-key case
	// (loop counters, ids); it skips the frexp decomposition entirely.
	// 2^61-1 itself is not representable in float64, so strict |v| < 2^61
	// suffices for exactness.
	if v == math.Trunc(v) && v < 2305843009213693952.0 && v > -2305843009213693952.0 {
		r := int64(v)
		if r == -1 {
			r = -2
		}
		return r
	}
	if math.IsNaN(v) {
		// Numbers are unboxed in M28, so CPython >= 3.10's identity-based NaN
		// hash is not expressible; use the classic constant.
		return 0
	}
	if math.IsInf(v, 1) {
		return pyHashInf
	}
	if math.IsInf(v, -1) {
		return -pyHashInf
	}

	neg := math.Signbit(v)
	m, e := math.Frexp(math.Abs(v)) // v = m * 2^e with 0.5 <= m < 1

	var x uint64
	for m != 0 {
		x = ((x << 28) & pyHashModulus) | x>>(pyHashBits-28)
		m *= 268435456.0 // 2^28
		e -= 28
		y := uint64(m) // pull out integer part
		m -= float64(y)
		x += y
		if x >= pyHashModulus {
			x -= pyHashModulus
		}
	}

	// Adjust for the exponent; first reduce it modulo pyHashBits.
	if e >= 0 {
		e = e % pyHashBits
	} else {
		e = pyHashBits - 1 - ((-1 - e) % pyHashBits)
	}
	x = ((x << uint(e)) & pyHashModulus) | x>>(uint(pyHashBits-e))

	r := int64(x)
	if neg {
		r = -r
	}
	if r == -1 {
		r = -2
	}
	return r
}

var bigHashModulus = new(big.Int).SetUint64(pyHashModulus)

// pyHashBigInt reduces |n| mod 2^61-1 and applies the sign, matching
// CPython's long_hash, so BigInt hashes agree with equal floats and ints.
func pyHashBigInt(n *big.Int) int64 {
	m := new(big.Int).Abs(n)
	m.Mod(m, bigHashModulus)
	r := int64(m.Uint64())
	if n.Sign() < 0 {
		r = -r
	}
	if r == -1 {
		r = -2
	}
	return r
}

// hashPointer spreads a pointer identity into a hash (Fibonacci scramble so
// aligned addresses do not cluster in the low bits).
func hashPointer(p uintptr) uint64 {
	return uint64(p) * 0x9E3779B97F4A7C15
}

// identityHash hashes a reference value by pointer when possible, falling
// back to a stable-per-object best effort for exotic kinds.
func identityHash(v Value) uint64 {
	rv := reflect.ValueOf(v)
	switch rv.Kind() {
	case reflect.Pointer, reflect.UnsafePointer, reflect.Map, reflect.Chan, reflect.Func:
		return hashPointer(rv.Pointer())
	case reflect.Slice:
		if rv.Len() > 0 || rv.Cap() > 0 {
			return hashPointer(rv.Pointer())
		}
		return 0
	default:
		// Value types without tracked identity: hash by type + rendering.
		// Equality still decides matches; this only picks a bucket.
		return hashString(fmt.Sprintf("%T:%s", v, v.String()))
	}
}

// hashTuple is CPython's xxPRIME-based tuple hash over element hashes.
func hashTuple(t TupleValue, ctx *Context) (uint64, error) {
	acc := xxPrime5
	for _, item := range t {
		lane, err := HashValue(item, ctx)
		if err != nil {
			return 0, err
		}
		acc += lane * xxPrime2
		acc = (acc << 31) | (acc >> 33) // rotl 31
		acc *= xxPrime1
	}
	acc += uint64(len(t)) ^ (xxPrime5 ^ 3527539)
	if acc == ^uint64(0) {
		acc = 1546275796
	}
	return acc, nil
}

// hashFrozensetElems is CPython's frozenset_hash: a commutative combine, so
// element order (and therefore internal storage order) cannot matter.
func hashFrozensetElems(elems []Value, ctx *Context) (uint64, error) {
	var h uint64
	for _, e := range elems {
		eh, err := HashValue(e, ctx)
		if err != nil {
			return 0, err
		}
		h ^= ((eh ^ 89869747) ^ (eh << 16)) * 3644798167
	}
	h ^= (uint64(len(elems)) + 1) * 1927868237
	h = h*69069 + 907133923
	if h == ^uint64(0) {
		h = 590923713
	}
	return h, nil
}

// findUserHash walks the class hierarchy for a user-defined __hash__.
// Returns (callable, true) when one is defined, (nil, true) when __hash__ is
// explicitly None (unhashable), and (nil, false) when nothing user-defined
// exists.
func findUserHash(class *Class) (Value, bool) {
	mro := class.mroOrNil()
	if mro == nil {
		// Inconsistent hierarchy: fall back to a defensive BFS.
		seen := map[*Class]bool{}
		queue := []*Class{class}
		for len(queue) > 0 {
			c := queue[0]
			queue = queue[1:]
			if c == nil || seen[c] {
				continue
			}
			seen[c] = true
			if h, decided, found := ownHashEntry(c); found {
				return h, decided
			}
			queue = append(queue, c.Parents...)
		}
		return nil, false
	}
	for _, c := range mro {
		if h, decided, found := ownHashEntry(c); found {
			return h, decided
		}
	}
	return nil, false
}

// ownHashEntry checks a single class's own namespace for __hash__.
// found reports a definition exists; the Value is nil for __hash__ = None
// (unhashable), mirroring findUserHash's contract.
func ownHashEntry(c *Class) (Value, bool, bool) {
	if attr, ok := c.Attributes["__hash__"]; ok {
		if _, isNil := attr.(NilValue); isNil {
			return nil, true, true
		}
		return attr, true, true
	}
	if m, ok := c.Methods["__hash__"]; ok {
		return m, true, true
	}
	return nil, false, false
}

// unhashableError builds the standard TypeError for a value.
func unhashableError(v Value) error {
	return &TypeError{Message: fmt.Sprintf("unhashable type: '%s'", v.Type())}
}

// HashValue computes the Python hash of v as a uint64 bit pattern (signed
// hashes are stored as their two's-complement bits). ctx is needed only to
// invoke user __hash__ methods; with a nil ctx such instances fall back to
// identity hashing, which matches the legacy no-context dict API. Unhashable
// types return a TypeError.
func HashValue(v Value, ctx *Context) (uint64, error) {
	switch val := v.(type) {
	case NumberValue:
		return uint64(pyHashDouble(float64(val))), nil
	case FloatValue:
		return uint64(pyHashDouble(float64(val))), nil
	case BoolValue:
		if bool(val) {
			return 1, nil
		}
		return 0, nil
	case BigIntValue:
		return uint64(pyHashBigInt(val.GetBigInt())), nil
	case StringValue:
		return hashString(string(val)), nil
	case SymbolValue:
		// Symbols hash like their text so any symbol/string equality remains
		// hash-consistent; unequal values sharing a bucket is harmless.
		return hashString(string(val)), nil
	case NilValue:
		return 0xFCA86420, nil
	case BytesValue:
		return hashBytes([]byte(val)), nil
	case ComplexValue:
		// hash(z) = hash(real) + 1000003*hash(imag), so complex(x, 0)
		// hashes like x (they compare equal).
		re := uint64(pyHashDouble(real(complex128(val))))
		im := uint64(pyHashDouble(imag(complex128(val))))
		h := re + 1000003*im
		if int64(h) == -1 {
			h = ^uint64(1) // -2
		}
		return h, nil
	case TupleValue:
		return hashTuple(val, ctx)
	case *FrozenSetValue:
		return val.hashWithCtx(ctx)
	case *ListValue, *DictValue, *SetValue, *ByteArrayValue:
		return 0, unhashableError(v)
	case *Class:
		// Class __eq__ is name-based (int == int.mro()[0]), and strings can
		// compare equal to classes by name, so classes hash exactly like
		// their name string.
		return hashString(val.Name), nil
	case *TupleInstance:
		// namedtuples etc. hash like their backing tuple unless the class
		// defines its own __hash__/__eq__.
		if h, handled, err := classUserHash(val.Class, v, ctx); handled {
			return h, err
		}
		if _, hasEq := val.Class.Methods["__eq__"]; !hasEq {
			return hashTuple(val.Data, ctx)
		}
		return 0, unhashableError(v)
	case *Instance:
		if h, handled, err := classUserHash(val.Class, v, ctx); handled {
			return h, err
		}
		if !IsHashable(v) {
			return 0, unhashableError(v)
		}
		// Subclasses of str/int/float hash by their backing content so they
		// land in the same bucket as the plain value they equal.
		if val.BackingStr != nil {
			return hashString(string(*val.BackingStr)), nil
		}
		if n, ok := NumBacking(v); ok {
			return uint64(pyHashDouble(float64(n))), nil
		}
		if val.BackingDict != nil {
			return 0, unhashableError(v)
		}
		return hashPointer(reflect.ValueOf(v).Pointer()), nil
	default:
		// Builtin type wrappers (IntType, StrType, ...) key like their class.
		if tp, ok := v.(interface{ GetClass() *Class }); ok {
			if c := tp.GetClass(); c != nil {
				return hashString(c.Name), nil
			}
		}
		if !IsHashable(v) {
			return 0, unhashableError(v)
		}
		return identityHash(v), nil
	}
}

// classUserHash resolves and invokes a user-defined __hash__ for an object of
// the given class. handled reports whether a user definition decided the
// outcome (including __hash__ = None, which is a TypeError).
func classUserHash(class *Class, self Value, ctx *Context) (uint64, bool, error) {
	if class == nil {
		return 0, false, nil
	}
	hashAttr, found := findUserHash(class)
	if !found {
		return 0, false, nil
	}
	if hashAttr == nil {
		return 0, true, unhashableError(self)
	}
	if ctx == nil {
		// Legacy no-context path: identity, never invoking user code.
		return identityHash(self), true, nil
	}
	// Prefer the bound method from the object itself (mirrors how the old
	// computeInstanceKey obtained it); fall back to calling the class
	// attribute with an explicit self.
	if obj, ok := self.(interface{ GetAttr(string) (Value, bool) }); ok {
		if bound, ok := obj.GetAttr("__hash__"); ok {
			if callable, ok := bound.(interface {
				Call([]Value, *Context) (Value, error)
			}); ok {
				result, err := callable.Call(nil, ctx)
				if err != nil {
					return 0, true, err
				}
				return hashResultToUint(result, self)
			}
		}
	}
	if callable, ok := hashAttr.(interface {
		Call([]Value, *Context) (Value, error)
	}); ok {
		result, err := callable.Call([]Value{self}, ctx)
		if err != nil {
			return 0, true, err
		}
		return hashResultToUint(result, self)
	}
	return 0, true, &TypeError{Message: fmt.Sprintf("__hash__ method of '%s' object is not callable", self.Type())}
}

// hashResultToUint converts a user __hash__ return value into the internal
// uint64 hash, applying CPython's -1 -> -2 adjustment.
func hashResultToUint(result Value, self Value) (uint64, bool, error) {
	switch num := result.(type) {
	case NumberValue:
		h := int64(float64(num))
		if h == -1 {
			h = -2
		}
		return uint64(h), true, nil
	case BigIntValue:
		return uint64(pyHashBigInt(num.GetBigInt())), true, nil
	case BoolValue:
		if bool(num) {
			return 1, true, nil
		}
		return 0, true, nil
	default:
		return 0, true, &TypeError{Message: fmt.Sprintf("__hash__ method should return an integer, got %s", result.Type())}
	}
}

// hashWithCtx computes (and caches) the frozenset's hash from its element
// values using CPython's commutative algorithm, so storage order is
// irrelevant. The cache field uses 0 as "not computed".
func (fs *FrozenSetValue) hashWithCtx(ctx *Context) (uint64, error) {
	if fs.hash != 0 {
		return fs.hash, nil
	}
	h, err := hashFrozensetElems(fs.Items(), ctx)
	if err != nil {
		return 0, err
	}
	if h == 0 {
		h = 1 // keep 0 free as the "not computed" marker
	}
	fs.hash = h
	return h, nil
}

// PyHash exposes the hash as Python's signed integer for the hash() builtin.
// Python reserves -1 (CPython's error sentinel), so it maps to -2.
func PyHash(v Value, ctx *Context) (int64, error) {
	h, err := HashValue(v, ctx)
	if err != nil {
		return 0, err
	}
	if int64(h) == -1 {
		return -2, nil
	}
	return int64(h), nil
}
