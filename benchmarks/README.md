# M28 performance benchmarks

A two-tier harness for measuring and improving M28's runtime performance,
designed to be driven iteratively (see [LOOP.md](LOOP.md) for the `/loop`
runbook).

## Tiers

### Macro — M28 vs CPython (`make bench`)

Each program in `cases/*.py` is run under both `./bin/m28` and `python3`
(best-of-N wall time). The scorer (`cmd/bench`) reports, per case, the slowdown
ratio `m28 / cpython` plus the geometric mean, writes `bench.json`, and diffs
the committed `baseline.json`.

```bash
make bench                       # score, best-of-3, vs baseline
make bench BENCH_N=5             # best-of-5
make bench-update-baseline       # lock in current numbers as the new baseline
go run ./cmd/bench -filter arith # only matching cases
```

Reading the table:

- **`ratio` / `adj`** — "how many x slower than CPython" (context for ranking
  subsystems). Because M28 is 50–1000x slower, each case's CPython time sits
  near its own startup/noise floor, so these columns are noisy. `adj` subtracts
  per-process startup and is noisier still. Do **not** judge a change by them.
- **`vs base`** — change in M28's **absolute** time vs the baseline. This is the
  stable, trustworthy signal for "did this change help or regress?"
- **`_startup`** isolates per-process startup (builtin registration, builtins
  preload); it is reported separately and excluded from the geomean.
- Cases that disagree with CPython are flagged `[MISMATCH]` (a correctness bug),
  `[ERROR]`, or `[TIMEOUT]` (needs a smaller N) and excluded from the geomean.

### Micro — Go hot-path benchmarks (`make bench-go`)

`micro_bench_test.go` benchmarks internal hot paths in isolation: eval
dispatch, deep scope lookup, numeric operator dispatch, attribute/method
lookup, and parsing.

```bash
make bench-go          # ns/op + allocs/op, single shot
make bench-go-save     # save current results as a comparison point
make bench-go-stat     # re-run and benchstat vs the saved results
```

Gate changes on **`allocs/op`** and **`B/op`** — these are deterministic across
runs, unlike `ns/op`. Use `benchstat` (`go install
golang.org/x/perf/cmd/benchstat@latest`) for statistically sound `ns/op`
comparison.

## Profiling a slow case

The `m28` binary takes profiling flags (added for this harness):

```bash
./bin/m28 -cpuprofile /tmp/cpu.prof benchmarks/cases/recursion_fib.py
go tool pprof -top -nodecount=25 /tmp/cpu.prof

./bin/m28 -memprofile /tmp/mem.prof benchmarks/cases/recursion_fib.py
go tool pprof -top -sample_index=alloc_space /tmp/mem.prof
```

Profiles flush on normal exit; cases that call `sys.exit()` will not flush.

## A note on the "M28 is faster" myth

A short program like the legacy `python_bench_light.py` can show M28 finishing
faster than CPython — but that only measures **startup**, where M28's Go binary
beats CPython's interpreter boot. On steady-state work (what the `cases/`
programs measure, with startup isolated) M28 is currently 50–1000x slower, as
expected for a tree-walking interpreter. The harness exists to close that gap.

## Layout

```
benchmarks/
  cases/*.py            macro cases (run under both interpreters)
  baseline.json         committed reference numbers (per machine)
  bench.json            latest run (gitignored)
  micro_bench_test.go   Tier-2 Go microbenchmarks
  LOOP.md               the iterate-to-improve runbook
cmd/bench/main.go       the macro scorer
```

## Adding a case

Drop a self-contained `.py` in `cases/` that (a) runs under both CPython and
M28, (b) prints a single integer checksum so the harness can verify the two
interpreters agree, and (c) loops enough that steady-state work dominates
startup but still finishes in a few seconds under M28 (it is the slow one). Then
re-baseline with `make bench-update-baseline`.

The older `python_bench.py` / `python_bench_light.py` remain for manual
`time`-based spot checks.
