# Dict Engine: compact ordered hash table

Status: implemented 2026-07. Replaces the string-serialization dict storage.
Related issues: M28-3o7, M28-2go, M28-2ri, M28-v1p, M28-jmo, M28-4ng, M28-99t,
M28-h4p2, M28-4pzl (fixed); M28-acf, M28-afw, M28-nhy (out of scope, open).

## Why the old design had to go

The previous `DictValue` was three parallel structures — `entries
map[string]Value`, `keys map[string]Value`, `orderedKeys []string` — where key
identity was a *serialized string* produced by `ValueToKey` ("s:foo", "n:1",
"t:(s:a,s:b)", "p:0xc000..."). Identity-by-encoding produced:

- Collisions: tuple encodings were unescaped, so `("a,s:b",)` == `("a","b")`
  as keys; nesting deeper than 5 collapsed to the constant `"p:recursive"`.
- Gaps: `BigIntValue` had no case, so `d[10**20]` raised "unhashable type".
- Wrong semantics: instances keyed by pointer; `__hash__`/`__eq__` retrofitted
  as a full O(n) scan per operation (quadratic builds); dict literals did not
  dedup equal instance keys.
- Cost: every operation formatted a string; composite keys walked
  `runtime.Stack` for a goroutine ID; deletes spliced `orderedKeys` (O(n));
  1M-entry dicts peaked at 5.5x CPython RSS.

## Design

One engine, CPython-shaped (compact dict + open-addressed index):

```go
type dictEntry struct {
    hash  uint64 // cached HashValue of key
    key   Value  // original key object; nil marks a tombstone
    value Value
}

type DictValue struct {
    BaseObject
    entries  []dictEntry // insertion order, may contain tombstones
    index    []int32     // open-addressed slots: empty / dummy / entry index
    used     int         // live entries
    tombs    int         // tombstones in entries
    modCount uint64      // structural-change counter for iterator guards
}
```

- **Probe sequence**: CPython's `i = (5*i + perturb + 1) & mask`,
  `perturb >>= 5`, seeded by the key hash. Index table is a power of two,
  grown at 2/3 load to `used*3` slots (growth compacts tombstones away).
- **Lookup**: compare slot's cached hash first; on match, `SameObject`, then
  equality — `EqualValues` for non-instances, `EqualValuesWithError(stored,
  search, ctx)` when a side is an `*Instance` (propagates `__eq__` errors,
  stored key first, matching CPython's comparison order).
- **Insert**: existing key keeps the *first-inserted* key object and its slot
  in the order (CPython semantics: `{True: 'a', 1: 'b'}` reprs as
  `{True: 'b'}`); new keys append to `entries`.
- **Delete**: index slot becomes a dummy, entry becomes a tombstone — O(1).
  `entries` is compacted when tombstones exceed live entries.
- **Iteration**: walk `entries`, skip tombstones. Iterators snapshot keys and
  raise "dictionary changed size during iteration" via the `modCount` guard,
  as before.

### Hashing (`core/hash.go`)

`HashValue(v Value, ctx *Context) (uint64, error)` with the Python invariant
`x == y => hash(x) == hash(y)`:

- Numbers (`NumberValue`, `FloatValue`, `BoolValue`, `BigIntValue`): CPython's
  modular hash (P = 2^61-1). `hashDouble` decomposes the float exactly;
  bigints reduce `|n| mod P` with sign. Guarantees
  `hash(1) == hash(1.0) == hash(True)` and int/float/bigint agreement for
  equal values.
- Strings: `hash/maphash` with a per-process seed (Python also randomizes
  per process). Str-subclass instances hash via their backing string;
  int/float subclasses via their numeric backing.
- Bytes: content hash. Complex: `hash(real) + 1000003*hash(imag)`.
- Tuples: CPython's xxPRIME combine over element hashes; unhashable elements
  propagate the error. No depth cap — cycles are impossible to build from
  immutable values.
- Frozensets: commutative shuffle-XOR over element hashes (CPython
  algorithm), cached.
- Classes and builtin type wrappers: hash of the class *name*, because class
  `__eq__` is name-based and hash must follow eq.
- Instances: `__hash__` when defined (called with ctx, may raise; result
  reduced to uint64), else identity (pointer).
- Everything else hashable-by-identity (functions, modules): pointer.
- Lists/dicts/sets: `TypeError: unhashable type`.

The `hash()` builtin returns this same value (as int64, with CPython's
-1 -> -2 adjustment), so user-visible hashes are consistent with dict
behavior.

### API

Full-fidelity (ctx-aware, used by dict methods, eval, and anything that can
see instance keys):

- `GetItem(key Value, ctx *Context) (Value, bool, error)`
- `SetItem(key, value Value, ctx *Context) error`
- `DelItem(key Value, ctx *Context) (bool, error)`
- `ForEach(fn func(key, value Value) bool)` — insertion order, live
- `PopLast() (key, value Value, ok bool)` — for popitem

String fast path (module globals, class namespaces, kwargs — the name-lookup
hot path; no allocation, no ctx):

- `GetStr(name string) (Value, bool)` / `SetStr(name string, value Value)`
- `DeleteStr(name string) bool`
- `HashStr(name) uint64` + `GetStrHashed`/`SetStrHashed` — hash a name once
  and probe many scopes (Context.lookupWithDepth) or precompute at compile
  time (module-tier IR nodes store the hash instead of the old "s:"-key).

Preserved signatures (reimplemented): `Size`, `GetValue/SetValue/DeleteValue`
(no ctx: instance keys degrade to identity semantics — same as the old
non-method paths), `OriginalKeys() []Value`, `Update`, `Clear`, `Iterator`.

Deprecated adapters, kept so ~400 mechanical call sites (mostly
`modules/*.go` building string-keyed namespaces) keep working unchanged:

- `SetWithKey(keyRepr string, origKey, value Value)` — ignores `keyRepr`,
  delegates to `SetValue(origKey, value)`. Always correct.
- `Get/Set/Delete(keyRepr string)`, `Keys() []string`,
  `OriginalKeyValue(keyRepr)` — parse/serialize primitive keyReprs
  ("s:", "n:", "i:", "f:", "b:", "nil") and delegate. Correct for
  string/number/bool/None keys — every current external use. Composite keys
  through these adapters are unsupported (they were silently corrupted
  before). New code must use the APIs above. Sweep tracked as a chore.

### What this fixes

Identity is now hash+eq, not encoding, so: bigint keys work; tuple keys
cannot collide by construction; nesting depth is unlimited; instance keys
bucket by `__hash__` and compare by `__eq__` only within a bucket (O(1)
expected instead of O(n) scans); dict literals dedup like `__setitem__`;
first-inserted key objects are preserved; delete is O(1); one structure
instead of three cuts per-entry memory roughly in half or better.

### Out of scope, unchanged

- kwargs ordering (PEP 468) — kwargs still travel as Go maps (M28-acf).
- `{**d}` in Python-syntax files — parser gap (M28-afw); the s-expression
  splat path now goes through `Update` (correct and O(n)).
- Concurrency: dicts remain unsynchronized; concurrent writers can corrupt
  the table or panic, as with Go maps before (M28-nhy). A lock would tax the
  name-lookup hot path; revisit with sharding or COW if `(go ...)` becomes
  load-bearing.
- NaN keys still collapse to one entry (numbers are unboxed values; identity
  cannot distinguish two NaNs). Documented divergence.

### Sets

`SetValue`/`FrozenSetValue` migrate onto the same table (value slot unused)
in a follow-up phase within the same epic, removing the last identity uses
of `ValueToKey`.
