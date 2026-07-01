# M28 Compiler Architecture

**Version:** 2.0 (2026-07-01)
**Status:** Living document, and the north star for structural work.

Version 1.0 (November 2025, in git history) proposed a multi-backend execution
ladder: interpreter, then a register bytecode VM (3-10x), then LLVM (50-100x),
then WASM. That ladder was superseded by measurement — see
[The evidence that changed the plan](#the-evidence-that-changed-the-plan).
This version keeps v1's multi-frontend insight and replaces its execution and
performance story with what the data supports.

---

## Executive summary

M28 is a **multi-frontend, single-runtime language platform**: multiple
surface syntaxes (Python, M28 s-expressions, potentially others) share one
semantic core — a Python-compatible object model — and one execution engine,
a **resolved tree-walking interpreter**.

The v1 architecture assumed speed would come from execution tiers. Our own
experiments killed that assumption: at the same value model, a resolved
tree-walk equals a bytecode VM. Speed comes from resolution, operator
specialization, and eventually value representation — all changes to the one
engine we already have. Compliance comes from consolidating the runtime so
each Python semantic has exactly one implementation. Differentiation from
CPython comes from the product layer around the engine — embedding,
distribution, sandboxing, macros — not from the execution strategy.

**Principles**

1. **Evidence over architecture.** No tier, pass, or rewrite lands without a
   measurement that demands it. The v1 plan failed this test; the vmspike and
   the perf loop (`benchmarks/LOOP.md`) are the correction.
2. **One implementation per semantic.** Every duplicated semantic is a
   conformance-divergence factory. Epic: M28-j9o.
3. **Python semantics are the contract.** All frontends, current and future,
   inherit the Python object model. The conformance suite is the contract's
   teeth. Campaign: M28-a6b8.
4. **Resolution is the compiler.** parse -> macroexpand -> resolve -> lower ->
   execute. Names get their meaning at resolve time, not at eval time.
   Epic: M28-b5c6.
5. **Differentiate in the product layer.** Epic: M28-9t2.

Work items in this document reference beads issues (`bd show <id>`).

---

## Where things stand (July 2026)

- **Both frontends are real.** `.py` files and CPython's pure-Python stdlib
  parse and execute directly (`parser/python_tokenizer.go`,
  `parser/python_parser*.go` -> `core/ast` -> `ToIR()` -> s-expression surface
  forms). `.m28` files accept full Python syntax as well as s-expressions.
- **Conformance:** 526/943 CPython-derived unit tests passing
  (`conformance.json`; campaign epic M28-a6b8, the active P0 work).
- **Performance:** macro-benchmark geomean is ~3.7x CPython, down from ~126x
  at campaign start (epic M28-e42e). The worst remaining case is
  `regex_match` — M28 interprets CPython's `re.py` wrapper per call.
- **The resolution layer exists** (layers 1-3): `eval/resolve.go`
  (`analyzeLocals`, `resolveBody` -> slot-indexed locals) and
  `eval/resolve_ir.go` (`compileIR` -> self-evaluating typed IR nodes), applied
  lazily to functions with simple positional signatures, with an always-correct
  fallback to the plain tree walk.
- **Scale:** ~126k LOC of Go. core 38k, modules 19.5k, eval 19.4k,
  builtin 18.8k, parser 17k.

---

## The evidence that changed the plan

The vmspike experiment (`vmspike/`, commit b276449) built a throwaway bytecode
VM and a resolved tree-walk over the same benchmark and measured the ladder
(N=2000, speedups vs the then-current tree-walk):

| Rung | Model | Speedup |
|------|-------|---------|
| resolved-dispatch | typed IR + slot locals, tree-walk | 2.3x |
| resolved-fast | resolved + inline numeric operators | 10.7x |
| vm-boxed | bytecode + slots, same boxed values | 9.5x |
| vm-unboxed | bytecode + raw float64 | 94x |

Reading: **a bytecode VM equals a resolved tree-walk at the same value model**
(9.5x vs 10.7x — bytecode itself buys nothing). The real levers are
(1) operator specialization, (2) resolution + slot locals, (3) value unboxing,
and all three are available to the tree-walk. A VM earns its complexity only
for resumable frames (generators, async, deep recursion) — never for
arithmetic speed.

The subsequent campaign validated profile-first over architecture-first. The
two biggest single wins were not architectural at all: a per-key
goroutine-ID stack walk hiding under dict/global lookups (`ValueToKey`;
geomean ~79x -> ~5.6x) and GC frequency (GOGC=400 default; ~5.1x -> ~3.7x).
The lessons are encoded in `benchmarks/LOOP.md`: judge changes by M28 absolute
time, alloc-count is not wall-clock, and check GC tuning before big rewrites.

---

## The architecture: six layers

```
   .py           .m28          (future language)
    |              |              |
    v              v              v
+---------------------------------------+  L1  FRONTENDS (syntax only)
|  python tokenizer/parser | m28 reader |
+---------------------------------------+
    |  core/ast + ToIR()      |
    v                         v
+---------------------------------------+  L2  SURFACE FORMS
|  s-expressions (core.Value)           |      macros, REPL,
|  + LocatedValue source positions      |      metaprogramming
+---------------------------------------+
    |  resolve + lower  (THE COMPILER)
    v
+---------------------------------------+  L3  RESOLVER + TYPED IR
|  slot locals, resolved heads, irNodes |
+---------------------------------------+
    |  execute
    v
+---------------------------------------+  L4  EXECUTION
|  resolved tree-walk                   |      (frame tier later, only
|  (fallback: plain s-expr walk)        |       for resumable semantics)
+---------------------------------------+
    |  every semantic operation
    v
+---------------------------------------+  L5  RUNTIME / OBJECT MODEL
|  types, dunders, MRO, dicts, imports  |      the ONE place Python
+---------------------------------------+      semantics live

+---------------------------------------+  L6  PRODUCT SURFACES
|  CLI | REPL | embed API | wasm builds |
+---------------------------------------+
```

The v1 "no MxN problem" insight survives: M frontends and N consumers meet at
a shared pivot. What defines the pivot is not a Go package — it is the
**surface-form contract plus the IR spec plus the conformance suite**.

### L1: Frontends

A frontend turns source text into surface forms with source locations.
Nothing else. Today:

- **Python:** `parser/python_tokenizer.go` (indentation-aware) +
  `parser/python_parser*.go` -> `core/ast` nodes -> `ToIR()` -> surface forms.
- **M28:** `parser/parser.go` emits surface forms directly, desugaring
  Pythonic sugar (infix, `x = v`, `f(x)`) inline.

Planned (M28-a472): a `Frontend` interface with dialect selection by file
extension or pragma. Today `.m28` execution and `EvalString` try the full
Python parser and fall back to the s-expression parser on failure
(`main.go`, `eval/eval_string.go`) — a double parse that produces error
messages blaming the wrong grammar.

**Constraint for future languages:** a new frontend inherits the Python object
model. That is the price of one runtime — the same deal JVM and BEAM languages
make — and it is what keeps a new language at M+N cost instead of MxN.
Package moves into a `frontend/` directory tree are deferred until the
consolidation epic lands; moving files does not move semantics.

### L2: Surface forms

S-expressions as `core.Value`, wrapped in `LocatedValue` for positions. This
is v1's "HIR" — it stays, but as the *surface*, not the executed form. Macros
(`docs/MACROS.md`), the REPL, and metaprogramming live here; this layer is why
the executed IR can change shape without breaking user-visible
metaprogramming.

### L3: Resolver + typed IR — the compiler proper

What exists (applied lazily per function, memoized, always with fallback):

- `eval/resolve.go` — `analyzeLocals` classifies slot eligibility;
  `resolveBody` rewrites local reads/writes to slot references.
- `eval/resolve_ir.go` — `compileIR` lowers the rewritten body to
  self-evaluating IR nodes (const/symbol/seq/if/return/assign/call/operator)
  with an inline numeric fast path. Tracebacks are preserved byte-identically
  (LocatedValue handling is unchanged).

Planned, in order:

1. **Resolver as single arbiter** (M28-ehi). Today meaning is decided before
   scope: special forms dispatch from a map ahead of user bindings
   (`eval/evaluator.go`; the `quote`-yields-to-binding hack patches one case),
   and `Context.Lookup` consults the operator registry before any scope
   (`core/context.go`), so operators and form names are not shadowable.
   Resolution must decide what a head symbol means: it is a special form only
   if scope does not bind it. Fixes the shadowing bug class and removes
   per-eval map lookups.
2. **Closure cells** (M28-osj). Real cell objects (shared slots) so captured
   locals work with slot frames; `__closure__` becomes real. Prerequisite for
   widening.
3. **Widen eligibility** (M28-6mf) construct by construct — defaults/kwargs,
   try/with, comprehensions, tuple targets — until the IR path is the common
   path and raw s-expr walking is only the REPL/macro/bootstrap path.
4. **IR spec** (M28-a88f). The missing piece is the contract, not more node
   types: node inventory, scoping and slot rules, eligibility gates, fallback
   semantics, and how frontends target surface forms. `docs/design/ir-spec.md`,
   versioned.

### L4: Execution

The resolved tree-walk is the engine. There is exactly one future execution
addition on the books, and it is semantic, not speed:

- **Frame-based tier for resumable semantics** (M28-8jo, deferred).
  Generators currently run on a bespoke step machine
  (`eval/generator_exec.go`), async wraps goroutines, and recursion depth is
  bounded by Go stacks. When that pressure is real, a frame-based interpreter
  over the *same resolved IR and same value model* is the answer. Design
  constraints: `docs/design/mir-bytecode-vm.md`.

Explicitly not on the books: an LLVM backend (native compilation of untyped
Python semantics is PyPy/Graal-scale effort; if native ever matters it is a
typed-subset AOT gated on the type system epic M28-b20f), and a WASM codegen
backend (Go compiles the whole interpreter to wasm/wasip1 — M28-5uc).

### L5: Runtime / object model

The single place Python semantics live: types, dunder dispatch, MRO,
containers, the import system. Everything above calls into it; fast paths
elsewhere are legal only as provably-equivalent inlinings of the canonical
implementation, with the conformance suite as the proof.

Today this layer fails the "single place" test — see the
[consolidation debt register](#consolidation-debt-register). Fixing that is
epic M28-j9o and is a de facto prerequisite for the resolver work: the
compiler can only trust a canonical semantic path once there is one.

### L6: Product surfaces

Where "more useful than CPython alone" lives (epic M28-9t2):

- **Embeddable Go API** (M28-6cc6): "Lua for Go, but Python" — config,
  plugins, game scripting, rule engines. `embed/` exists; the API needs
  design and commitment.
- **Self-contained single binary** (M28-hmz): today
  `modules/python_finder.go` shells out to `python3` to find a stdlib, which
  silently breaks the static-binary story. Bundle a pinned stdlib snapshot.
- **Capability-based sandboxing** (M28-6y9): per-interpreter fs/net/clock/
  import policy at the Go module boundary — something CPython embedding
  cannot offer.
- **Concurrency without a GIL** (M28-b9y): `go`/channels already run on
  goroutines. The credible model is isolates — one interpreter per domain,
  channels between them, no shared mutable Python heap. Shared-heap free
  threading is an explicit non-goal.
- **Browser/portable deployment** (M28-5uc): wasm/wasip1 builds of the
  interpreter.
- **Dual syntax + macros over Python semantics**: the one thing CPython
  structurally cannot offer (`docs/MACROS.md`, `docs/design/s-strings.md`).
- **Tooling**: structured logging exists (`core/logging.go`); LSP/DAP and
  developer tooling under epic M28-2d6f.

---

## Consolidation debt register

The current known violations of "one implementation per semantic", with their
issues. Each lands as its own conformance-gated ratchet.

| Semantic | Today | Issue |
|----------|-------|-------|
| Arithmetic dispatch | >=5 implementations of `+`: `builtin/operators/operators_arithmetic.go` (addTwo + types.Switch), `core/type_registry_primitives.go` dunders, `builtin/types.go` int/float classes via `__value__`, `core/protocols/numeric.go`, `eval/resolve_ir.go` fastNumOp; bigint promotion duplicated | M28-lel |
| Format specs | `builtin/string_format.go` vs `core/string_methods.go` formatValueWithSpec (str.format drops grouping separators) vs `common/types/dunder.go` CallFormat; f-string parsing exists twice | M28-efl |
| MRO | BFS traversal in `core/class.go` while comments claim C3; diamond inheritance diverges | M28-b4b |
| Dict/set keying | string encoding via `core.ValueToKey` + parallel key maps instead of hash/eq | M28-5k9 |
| Method registries | `TypeDescriptor` vs `MethodRegistry`, 84 distinct GetAttr implementations | M28-6jr |
| Module caches | `core.ModuleRegistry` + `modules.moduleCache` + `sys.modules`, reconciled by hand twice; dead `DefaultModuleLoader` | M28-bfw |
| Error types | eval-local error types parallel to core Python errors | M28-7886 |
| Dead code | `env/`, `LispNumber`/`LispString`, legacy char-based parser family, `string_legacy.go`, dual param binding | M28-ob9 |

---

## Performance strategy

The ladder, in risk-adjusted order (epic M28-e42e, runbook
`benchmarks/LOOP.md`):

1. **Operator specialization** — mostly done (`+ - * < <= > >= == !=`);
   remaining `/ // % **` (M28-2gb).
2. **Resolution** — layers 1-3 landed; widening (M28-6mf) is both the perf
   rung and the architecture.
3. **Value unboxing** — the path past CPython (vmspike unboxed rung: 94x).
   Deliberately last: re-profile from the current ~3.7x baseline first;
   GOGC=400 already captured much of the GC win, and unboxing is the biggest,
   riskiest change.

Plus the one targeted wall: **native `re` fast path** (M28-dgh) — `import re`
resolves to CPython's `re/__init__.py`, so every match pays ~2ms of
interpreter overhead running the pure-Python wrapper; route it to
`modules/c_sre.go`.

Method (hard-won, encoded in LOOP.md): judge by M28 absolute ms, never the
noisy CPython ratio; alloc-count without wall-clock is not a win; profile the
whole program, not the case you expect; GC tuning before rewrites. Non-goals:
JIT, competing with PyPy.

---

## Compliance strategy

- **The conformance ratchet** (M28-a6b8): CPython-derived unit tests scored in
  `conformance.json` (526/943). Every semantic change is gated on it; the
  number only goes up.
- **CPython stdlib policy** (CLAUDE.md): pure-Python stdlib runs directly;
  only C extensions get Go implementations under `modules/`; stubbing a
  pure-Python module requires a documented language-feature reason.
- **One implementation per semantic** (M28-j9o): most conformance failures are
  object-model gaps, and duplicated semantics are how divergence sneaks back.
- **Tracebacks are part of the contract**: the IR path preserves
  LocatedValue handling so tracebacks stay byte-identical to the tree-walk.

---

## What we are explicitly NOT doing

- **Bytecode VM for speed** — falsified by vmspike (b276449).
- **LLVM backend / JIT** — wrong effort-to-win ratio for untyped Python
  semantics; typed-subset AOT only, gated on M28-b20f, only with real demand.
- **WASM codegen backend** — Go's wasm/wasip1 build target ships the whole
  interpreter instead (M28-5uc).
- **Shared-heap free threading** — isolates + channels instead (M28-b9y).
- **CPython C-API compatibility** — that moat belongs to CPython; M28's
  extension story is Go-native modules.
- **Package reshuffle before consolidation** — `frontend/`/`backend/`
  directory moves wait until M28-j9o lands.
- **Speculative IR optimization passes** — folding/DCE/inlining land only if
  a profile demands them (that closed M28-160a).
- **Stubbing pure-Python stdlib** — fix the language feature instead.

---

## Sequencing

1. **Docs + tracker aligned to evidence** — this document; beads epics
   reorganized (done 2026-07-01).
2. **Runtime consolidation** (M28-j9o) — ratchet by ratchet, conformance-gated,
   alongside the ongoing conformance campaign (M28-a6b8).
3. **Resolver work** (M28-b5c6): arbiter (M28-ehi) -> cells (M28-osj) ->
   widen (M28-6mf), with the IR spec (M28-a88f) written as the coverage grows.
4. **Perf ladder** (M28-e42e) continues measurement-first; unboxing decided
   after a fresh profile.
5. **Product track** (M28-9t2) in parallel — embed API and stdlib bundling
   touch different code.

---

## How to add a frontend (current reality)

1. Write a tokenizer/parser that produces either `core/ast` nodes with
   `ToIR()` (what Python does) or surface forms directly (what M28 does).
   Preserve source locations (`LocatedValue`).
2. Lower your language's constructs to Python-semantics surface forms —
   your language inherits M28's object model, numbers, and exceptions.
3. Register dialect selection (extension/pragma) — see M28-a472 for the
   interface this is converging on.
4. Do not touch L3-L5. If your language needs a semantic the runtime lacks,
   that is a runtime feature first, a frontend feature second.

---

## References

**Internal**
- `vmspike/` and commit b276449 — the experiment that set the execution
  strategy.
- `benchmarks/LOOP.md` — performance loop runbook.
- `eval/resolve.go`, `eval/resolve_ir.go` — the live resolution layer.
- [MIR Bytecode VM](./mir-bytecode-vm.md) — superseded speed-tier design;
  retained constraints for the future frame tier (M28-8jo).
- [Python frontend docs](./python-frontend-design.md) — historical design
  records of the shipped frontend.

**External**
- Crafting Interpreters (Nystrom) — tree-walk and bytecode trade-offs.
- The Implementation of Lua 5.0; V8 Ignition design notes.
- GraalVM Truffle — the multi-frontend/one-runtime deal.
- PyPy architecture — why AOT/JIT for Python is a research program.
