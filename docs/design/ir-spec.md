# M28 Resolved IR Specification

**Version:** 1.0 (2026-07-02)
**Status:** Normative for `eval/resolve.go`, `eval/resolve_ir.go`, and
`core.SlotFrame`. Change the code, change this document, bump the version.
**Companion:** `docs/design/compiler-architecture.md` (L3/L4) is the map;
this is the contract.

The resolved IR is the execution form of the resolution layer (layers 1-3)
and the value-unboxing campaign (layer 4, epic M28-9pm). It is a strict
subset optimization: every construct it does not model falls back to a
slower, always-correct path, and every fallback boundary is defined here.

---

## 1. Pipeline position

```
surface forms (s-expressions + LocatedValue)      L2
    | analyzeLocals      -- eligibility + slot map          (per function,
    | resolveBody        -- locals -> slotRef rewrite        lazy, memoized
    | compileIR          -- forms -> self-evaluating nodes   on first call)
    v
resolved IR (irNode trees)                        L3
    | callSlots           -- flat tagged frame, no map scope
    v
execution                                         L4
```

`UserFunction.ensureResolved` runs the three passes once per function object.
Failure at any pass leaves the function on the map-scope tree walk; success at
resolveBody but partial modeling at compileIR leaves unmodeled subtrees as
rewritten surface forms evaluated generically ("layer-2 islands").

## 2. Eligibility (analyzeLocals)

A function is slot-eligible iff:

- **Signature:** only required positional parameters (no defaults, `*args`,
  `**kwargs`, keyword-only). `simpleParamNames`.
- **Body:** every special form in the body is *modeled*. The modeled set:
  - `slotSafeForms`: do, begin, =, for, while, if, return, break, continue,
    and, or, get-item, set-item, del-item, \_\_slice\_\_, dict-literal,
    list-literal, tuple-literal, isinstance, issubclass, raise. These have
    pure value-position children; their handlers evaluate children through
    `Eval`, and their structural markers (`from`, `*unpack`/`**unpack`,
    `"__call__"`) are non-identifier symbols or non-symbol values that the
    rewriter leaves intact.
  - `.` (custom): `(. obj attr args...)` — obj and args are values; the attr
    at items[2] is **structural** (it may collide with a local name and is
    never rewritten or walked).
  - `=` with attribute/subscript targets: `(. obj attr)` / `(get-item d k)`
    targets bind nothing local; their object/index positions are values.
  - `del` of attribute/subscript targets. Bare `del local` would unbind a
    slot: reject.
  - Comprehensions (`compHeads`: list-comp, set-comp): closed sub-scopes.
    The first clause's iterable is enclosing-scope code (walked and later
    rewritten); the innards (body, conditions, later iterables) may read only
    comprehension variables and non-locals, verified by the free-name check
    (`compFree` ∩ final locals = ∅). Comp variables never become slots.
    dict-comp (irregular shape) and generator expressions (lazy — the frame
    may be gone) are NOT modeled.
  - `try` (custom): exact modern clause shapes only — `(except (do ...))`,
    `(except TypeSym (do ...))`, `(except (tuple-literal Sym...) (do ...))`,
    `(except <expr-list> (do ...))`, `(else (do ...))`, `(finally (do ...))`.
    Handler bodies run in the function context (tryForm `handlerCtx := ctx`),
    so their assignments are ordinary locals. Symbol/tuple type names are
    matched BY NAME through the map chain at runtime and stay verbatim; a
    local shadowing a used type name disqualifies (`tryTypeNames` post-walk
    check). `as` bindings (they Define into the map, invisible to slots),
    `except*`, the legacy `(except var ...)` catch-all, multi-statement
    handlers, and literal types all disqualify.
- **Dynamic namespace:** no call to, and no *value-position read of*,
  `dynamicScopeFns` (locals, globals, vars, exec, eval, \_\_import\_\_, dir)
  or `super`. Direct `super()` calls already bail via the special-form gate;
  the value-position rule closes aliasing (`s = super`). super resolves
  self/cls by name at call time; a slot frame has no name-addressable dict.
- **Assignment targets:** bare symbols (become slots), or dot/get-item
  targets (bind nothing). Tuple targets, `:=`, `annotated-assign`: reject.
- **No closures or resumption:** def, lambda, class, with, yield, match-stmt,
  global, nonlocal, quote/quasiquote (identifier symbols are data there),
  import: reject.
- **Calls:** keyword arguments (`[sym, "=", value]` runs) and `*`/`**` unpack
  markers make a call non-rewritable — a bare symbol is structural there.
  Checked for both named and computed callees (keyword *method* calls lower
  to a computed-callee shape: `((. obj m) **unpack (dict-literal ...))`).

## 3. Slot rules

- Slots are indexed 0..n-1: parameters first in declaration order, then
  locals in order of first textual assignment.
- An unbound slot reads as nil and raises `UnboundLocalError` (catchable as
  both UnboundLocalError and NameError), matching CPython's
  read-before-assignment behavior.
- Comprehension variables and except-`as` names are never slots (comp vars
  bind a child scope at runtime; as-bindings disqualify the function).
- Loop variables leak their final value, as in Python.

## 4. The rewrite (resolveBody)

Every read and `=`/`for` target of a slot-mapped name becomes `*slotRef`.
Positions that are **never rewritten**: form heads, the dot form's attr name,
except-clause type names/tuples, comprehension variables and innards
(everything but the first clause's iterable), structural markers (`in`,
`as`, `from`, `else`, unpack markers, the `"__call__"` StringValue), and
anything inside forms the rewrite bails on. Free names stay `SymbolValue`
and resolve through the scope chain at run time.

Interface-value change detection during rewriting uses `sameValue`, which
compares only pointer and scalar kinds: `BytesValue`/`TupleValue` are slices
and `LocatedValue` can wrap one — plain `==` panics. "Changed" is always a
safe answer.

## 5. Node inventory (compileIR)

| Node | Forms | evalNum | execStmt | Notes |
|------|-------|:-:|:-:|-------|
| `*slotRef` | local read/write target | yes | — | frame access; also a rewrite artifact consumed by ForForm/UnpackPattern |
| `constNode` | literals | yes | — | kind pre-classified at compile (`newConstNode`) |
| `symbolNode` | free names | — | — | `ctx.Lookup` with NameError location |
| `seqNode` | do/begin | — | yes | non-final items in statement position; final item is the block value |
| `ifNode` | 2/3-branch if | — | yes | numeric condition = `f != 0`; truthiness via `isTruthyWithErrors` (propagates `__bool__` errors, per IfForm) |
| `returnNode` | return | — | — | boxes its value (the call ABI is boxed) |
| `assignNode` | `(= slot v)` | — | yes | `execDiscard` stores unboxed via SetNum; expression position memoizes one box via Get |
| `callNode` | positional calls | — | — | argv boxed; guarded against keyword/unpack markers (`compileCallChecked`) |
| `operatorNode` | + - * / // % ** == != < <= > >= | yes | — | cached registry callable + typed fast paths (§6) |
| `dotNode` | `(. obj attr ...)` | — | — | pre-built immutable args list handed to DotForm; StringValue positions stay raw (attr, `"__call__"` marker) |
| `forNode` | 4/5-item for | — | yes | range → raw counting loop, induction var via SetNum; other sequences → ForForm with the evaluated value wrapped in a constNode (side effects once) |
| `whileNode` | 3-item while | — | yes | condition via typed kernel; boxed truthiness via `core.IsTruthy` (does NOT propagate `__bool__` errors, per WhileForm) |

Unmodeled-but-admitted special forms (comprehensions, try, and/or, literals,
subscripts, raise, break/continue) compile to their original rewritten lists
and dispatch to their handlers via generic Eval — `compileList`'s default
case must route ANY registered special form to `orig`, never to a call.

## 6. The typed kernel (evalNumOf)

**Kind lattice:** `SlotBoxed` | `SlotInt` (a `core.NumberValue`) |
`SlotFloat` (a `core.FloatValue`). There is deliberately no bool kind:
Go statically interns 1-byte boxes, so `BoolValue` conversion never
allocates (measured; a bool tag would buy nothing).

**Contract:** `evalNumOf(e, ctx) (num float64, kind uint8, boxed Value, err)`.
Evaluation happens **exactly once**. A numeric result returns `(num, kind,
nil, nil)`. A result that cannot stay unboxed returns the value the
evaluation already produced (`boxed != nil`); callers must never
re-evaluate — side effects fire once. Generic results are classified after
`Eval` (two type asserts), so numeric-returning calls feed unboxed consumers.

**Arithmetic semantics** (operatorNode):
- Int × Int (+, -, *): raw float64 math with `PromoteIntOverflow`
  bit-identical to the boxed fast paths; promotion returns a boxed
  BigIntValue.
- Any float operand: raw IEEE float math, result kind Float — overflow goes
  to inf, never bigint — matching `floatBinaryFast`.
- Comparisons: raw IEEE across kinds (`1 == 1.0`, NaN compares false),
  result is an interned BoolValue.
- `/`, `//`, `%`, `**` and any non-numeric operand: box once, dispatch to
  the cached registry operator (ZeroDivisionError etc. live there).

**Box-at-escape rule:** values box (via `boxNum`: BoxNumber for Int — the
[-5,256] shared cache — FloatValue for Float) only when crossing into boxed
territory: call arguments, returns, container stores, generic-handler
dispatch.

## 7. SlotFrame (core/context.go)

Parallel arrays `nums []float64 / tags []uint8 / refs []Value`, value-embedded
in Context (stage-0 spike: 3.17ns vs 3.39ns tagged-struct vs 50.7ns boxed per
guarded read-add-write; parallel keeps 9 hot bytes per slot).

**Invariants:**
- `Set(i, v)` is the boxed write choke point: classifies kind, mirrors the
  scalar, keeps the box in refs. Every boxed binder routes here — callSlots
  argument binding, assignNode, assignFormInternal slot targets,
  UnpackPattern slot targets, comprehension mini-frames.
- `SetNum(i, f, kind)` is the unboxed write: mirrors the scalar, invalidates
  refs (nil).
- `Get(i)` re-boxes lazily on a nil ref for numeric tags and **memoizes**, so
  read-heavy patterns pay one boxing per write, not per read.
- Unbound = tag SlotBoxed + nil ref.
- A new binder that writes frames any other way is a bug the shadow mode
  exists to catch.

**Shadow verification** (`M28_DEBUG_SLOTS=1`): every `Get` re-checks
tag/mirror/ref coherence and panics on divergence. Run the full corpus,
Go suite, and conformance set under it whenever a binder or kind is added.

## 8. Statement position (stmtNode)

`evalStmt(e, ctx)` executes a node for effect: `stmtNode` implementors
(seqNode, ifNode, forNode, whileNode, assignNode) skip materializing their
results; everything else evaluates normally. The return is a control
sentinel (Return/Break/Continue) or nil — ordinary values are dropped at the
source. Statement context is established by: seqNode non-final items, seqNode
`execStmt` (all items, when the block itself is a statement), if branches
under `execStmt`, and loop bodies in forNode/whileNode. The final item of a
value-position block and any expression position evaluate normally, so
`(= x (for ...))`-style s-expression code keeps its value semantics.

## 9. Fallback ladder

1. Ineligible function → map-scope tree walk (layer 1).
2. resolveBody bail (keyword/unpack call discovered late) → map scope.
3. compileIR unmodeled subtree → rewritten surface form evaluated
   generically; slotRefs inside self-evaluate (layer 2).
4. Runtime kind miss (`boxed != nil`) → boxed generic dispatch for that
   operation only.
5. forNode non-range sequence → ForForm on the pre-evaluated value.

Every rung is semantics-identical; only speed differs. The conformance
suite's failure-name-set diff (old binary vs new) is the gate that has
caught every real divergence to date.

## 10. Frontend contract

Frontends emit surface forms; the IR recognizes the canonical lowerings the
Python frontend produces — `(for target seq (do body...))`,
`(while cond (do body...))`, do-wrapped clause bodies in try, single-clause
comprehension shapes — plus the s-expression `in` variant of for. A frontend
emitting anything else gets correct-but-generic execution. New frontends
should target the canonical shapes to inherit the fast paths.
