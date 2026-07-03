# Performance loop runbook

A `/loop`-able protocol for driving M28's performance toward CPython. Each
iteration measures, finds the worst offender, profiles it, fixes the hot Go
code, and commits only if it is faster **and** still correct.

This mirrors the CPython-conformance loop, but the signal is a slowdown ratio
instead of a pass count, and the profiler tells you what to fix.

## The two tiers

- **Macro** (`make bench`): runs every `benchmarks/cases/*.py` under both
  `./bin/m28` and `python3`, best-of-N. Writes `benchmarks/bench.json` and diffs
  `benchmarks/baseline.json`. **This picks the target.**
  - The `ratio`/`adj` columns ("how many x slower than CPython") are *context*.
    Each case's CPython time is near its own startup/noise floor (CPython
    finishes in milliseconds; M28's geomean is ~3.7x CPython as of 2026-07),
    so the ratio carries CPython-side noise and `adj` (startup-subtracted) is
    noisier still — use them to rank subsystems, not to judge a change.
  - The `vs base` column compares **M28's absolute time** to the baseline. That
    is the stable signal (M28 time is dominated by steady-state work) and is
    what tells you whether a change actually helped or regressed a case.
- **Micro** (`make bench-go`): Go `testing.B` benchmarks of internal hot paths
  (eval dispatch, scope lookup, numeric dispatch, GetAttr, parse). Gates the
  change on `ns/op` and — more importantly — deterministic `allocs/op`.
  **This proves the fix helped without regressing.**

## One iteration

1. **Measure.** `make bench`. Read the table; note the geomean and the
   worst-ratio case to attack (ignore `[MISMATCH]`/`[ERROR]`/`[TIMEOUT]` cases
   for timing — a mismatch/error is a correctness bug to file separately, and a
   timeout means the case needs a smaller N, not that it is the slowest).

2. **Profile the worst case.** Build is already done by `make bench`. Then:
   ```
   ./bin/m28 -cpuprofile /tmp/cpu.prof benchmarks/cases/<case>.py
   go tool pprof -top -nodecount=25 /tmp/cpu.prof
   ```
   For allocation pressure (often the real cause in a tree-walker):
   ```
   ./bin/m28 -memprofile /tmp/mem.prof benchmarks/cases/<case>.py
   go tool pprof -top -sample_index=alloc_space -nodecount=25 /tmp/mem.prof
   ```
   The top frames are your suspects (e.g. `Context.lookupWithDepth`,
   `eval.Eval`, map allocation in scope creation).

3. **Save the micro baseline** (so you can prove improvement):
   `make bench-go-save`.

4. **Fix the hot Go code.** Target the top frame. Typical wins in this codebase:
   fewer map allocations per call/scope, avoiding interface boxing in the
   numeric path, caching operator/attr lookups, shrinking per-call Context setup.

5. **Prove the micro win.** `make bench-go-stat` (needs `benchstat`). Require the
   targeted benchmark's `ns/op` and `allocs/op` to drop and **no other micro
   benchmark to regress** beyond noise.

6. **Correctness gate — non-negotiable.** `make test`. A tree-walker's cheapest
   "speedup" is silently breaking semantics. The repo currently has a small set
   of *pre-existing* failures (e.g. `core/TestListIterator`,
   `common/builders/TestOperatorMultiply`), so the gate is **"no NEW failures"**:
   the set of failing tests after your change must be a subset of the set before
   it. Capture the baseline once at the start of a campaign:
   ```
   make test 2>&1 | grep -E '^(FAIL|--- FAIL)' | sort > /tmp/m28-test-baseline.txt
   ```
   and after each change diff against it. If your change adds any failure, revert.

7. **Re-score.** `make bench`. Commit **only if**:
   - your target case's `vs base` shows "better" and no other case shows
     "WORSE" (both measured on M28 absolute time), and
   - `make test` is green.
   Otherwise revert the change and pick the next target.

8. **Commit** with a conventional message, e.g.
   `perf(eval): cache operator lookup to cut scope_lookup 18%`.

## Guardrails

- **Noise.** Times are best-of-N; treat <5% moves on M28 absolute time as
  noise. The macro tier is for *direction* (which subsystem); the micro tier
  (especially deterministic allocs/op) is the trustworthy gate.
- **Don't tunnel-vision.** Always check the geomean and per-case regression
  column, not just the case you attacked.
- **Fixed baseline.** CPython version is the yardstick; don't change it
  mid-campaign. Re-baseline deliberately with `make bench-update-baseline`
  (and commit `baseline.json`) only when you want to lock in progress.
- **Startup is a real target.** The `_startup` case isolates per-process
  startup (builtin registration, builtins preload). If its ratio is high, that
  is its own optimization.

## Current targets (2026-07-02, epic M28-e42e)

Campaign state: geomean ~2.2x CPython (from ~126x at campaign start). The
value-unboxing campaign (epic M28-9pm, stages 0-4; contract in
`docs/design/ir-spec.md`) landed tagged slot frames, the evalNum typed
kernel, forNode/whileNode, and the truthiness fast paths. M28 now BEATS
CPython on `recursion_fib` (0.6x) and `scope_lookup` (0.8x); slot-compiled
loop kernels run at fixed per-call allocations (28/op for-range, 9/op while
— see the `Kernel*` micro benches, which are the campaign gates). Remaining,
in rough value order:

- `regex_match` (6.4x) — M28 interprets CPython's `re.py` wrapper per call;
  needs the native c_sre fast path (M28-dgh), not micro-tweaks. Biggest
  remaining ratio; partially helped by containment (-14%) but walled.
- `exceptions` (4.9x) — the raise/catch path costs ~21µs per exception
  (instance construction, matching, context chaining; bead filed). tryNode
  (pre-parsed clause structure) is the sibling bead.
- `attr_method` (3.7x) — slot-call frame construction is 76% of call allocs
  (frame-reuse bead: needs a funcEnv-escape proof before pooling);
  remaining DotForm internals (BoundMethod per access, Class instantiation).
- `arith_int` (2.9x) / `iteration` (2.5x) — MODULE-LEVEL code: it never
  slot-compiles, so it kept none of the unboxing wins. The lever is
  compiling module bodies like function bodies (module-as-frame), or
  accepting that module top-level is cold in real programs.
- `/`, `//`, `%`, `**` still lack typed-kernel fast paths (M28-2gb) — they
  box and dispatch to the registry.
- Slot-call frame reuse (see above) is the biggest single remaining lever
  for call-heavy code: fib's residual 28 allocs/call are Context+SlotFrame
  construction.

## Adding a case

Drop a self-contained `.py` in `benchmarks/cases/` that (a) runs under both
CPython and M28, (b) prints a single integer checksum (so the harness can
verify both interpreters agree), and (c) loops enough that steady-state work
dominates startup. Then `make bench-update-baseline`.
