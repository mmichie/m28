# Bytecode VM Design — Superseded as a Speed Tier

**Status:** SUPERSEDED (June 2026) as a performance strategy. Retained only as
design constraints for a possible future **frame-based execution tier for
resumable semantics** (beads issue M28-8jo, deferred).

Version 1.0 of this document (November 2025, in git history) designed a
register-based MIR bytecode VM promising 3-10x speedups, with type feedback,
profile-guided specialization, and a `.m28c` serialization format. Do not
implement it. The premise was measured and falsified.

---

## Why this was superseded

The vmspike experiment (`vmspike/`, commit b276449) built both a stack
bytecode VM and a resolved tree-walk over the same workload (N=2000) and
measured, relative to the then-current tree-walk:

| Rung | Model | Speedup |
|------|-------|---------|
| resolved-dispatch | typed IR + slot locals, tree-walk | 2.3x |
| resolved-fast | resolved + inline numeric operators | 10.7x |
| vm-boxed | bytecode + slots, same boxed values | 9.5x |
| vm-unboxed | bytecode + raw float64 | 94x |

**A bytecode VM equals a resolved tree-walk at the same value model** (9.5x vs
10.7x). The speedups the v1 document attributed to bytecode actually come from
resolution (slot locals), operator specialization, and value representation —
all of which the tree-walk gets without a second execution engine. Those
became the resolution layer (`eval/resolve.go`, `eval/resolve_ir.go`) and the
performance ladder in `benchmarks/LOOP.md` (epic M28-e42e).

The maintenance risk the v1 document listed ("two execution engines to
maintain") was real, and the payoff was not.

---

## What survives: a frame tier for resumable semantics

There is exactly one honest reason to introduce a frame/bytecode-shaped
representation, and it is semantic, not speed:

- **Generators** currently run on a bespoke flattened step machine
  (`eval/generator_exec.go`: `ExecutionStep`/`GeneratorExecState` with
  loop/try/with stacks). It works, but it re-implements control flow and every
  new construct must be taught to it.
- **Async** wraps goroutines rather than suspendable frames.
- **Deep recursion** is bounded by Go stacks and the artificial recursion
  limit; heap-allocated frames would remove the coupling.

When (and only when) those pressures block conformance or real programs,
build the frame tier under these constraints:

1. **Input is the resolved IR** (`eval/resolve_ir.go` nodes), not raw
   s-expressions. The resolver stays the single compiler.
2. **Same value model, same runtime layer.** Frames change *control*
   representation only; all semantic operations still route through the
   object model. No frame-tier-private semantics.
3. **Frames are heap-allocated and resumable** — that is the entire point.
   Register allocation, type feedback, inline caches, and `.m28c`
   serialization from v1 are out of scope.
4. **The resolved tree-walk remains the reference implementation.**
   Differential-test the frame tier against it on the conformance suite;
   tracebacks must stay identical.
5. **Adopt it per-construct** (generators first), keeping the existing paths
   as fallback, exactly like the resolution layer rolled out.

Serialization/caching of compiled forms is a separate concern from execution:
if import/startup parse cost ever measures as a problem, that is
M28-2cc6 (on-disk cache of parsed forms), and it needs no VM.

---

## References

- `vmspike/vm.go`, `vmspike/resolved.go` — the throwaway spike (kept as the
  record of the experiment).
- Commit b276449 — spike results and conclusions.
- [Compiler Architecture](./compiler-architecture.md) — current architecture;
  see "L4: Execution".
- `eval/generator_exec.go` — the step machine a frame tier would replace.
