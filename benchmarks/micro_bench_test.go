// Package benchmarks holds the Tier-2 (micro) benchmarks of the M28
// performance harness. Each benchmark isolates one internal hot path so a
// change can be checked for ns/op and (deterministic) allocs/op regressions
// with `benchstat`. See benchmarks/LOOP.md for the workflow.
//
// Run:  go test ./benchmarks/ -bench=. -benchmem -run='^$'
//
// This file deliberately lives in its own package and imports the interpreter
// packages as a normal consumer would, mirroring the real startup sequence in
// main.go (RegisterAllBuiltins + RegisterAllForms).
package benchmarks

import (
	"sync"
	"testing"

	"github.com/mmichie/m28/builtin"
	"github.com/mmichie/m28/builtin/operators"
	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/eval"
	"github.com/mmichie/m28/parser"
	"github.com/mmichie/m28/special_forms"
)

// Builtins and special forms register into process-global registries that
// reject duplicate registration, so the context is built exactly once and
// shared across benchmarks. Setup is never inside a timed loop, and the
// benchmarks below do not mutate shared state in conflicting ways.
var (
	sharedCtx  *core.Context
	sharedOnce sync.Once
)

// benchContext returns the shared global context, wired the same way main.go
// wires the interpreter (minus module loading, not needed for these paths).
func benchContext() *core.Context {
	sharedOnce.Do(func() {
		ctx := core.NewContext(nil)
		ctx.Define("true", core.BoolValue(true))
		ctx.Define("false", core.BoolValue(false))
		ctx.Define("True", core.BoolValue(true))
		ctx.Define("False", core.BoolValue(false))
		ctx.Define("None", core.None)
		ctx.Define("nil", core.NilValue{})
		builtin.RegisterAllBuiltins(ctx)
		special_forms.RegisterAllForms()
		sharedCtx = ctx
	})
	return sharedCtx
}

// BenchmarkEvalArithLoop is the integrated path: a parsed program is evaluated
// repeatedly, exercising the special-form dispatcher, scope lookups, iteration
// and arithmetic together. Parsing happens once, outside the timed loop.
func BenchmarkEvalArithLoop(b *testing.B) {
	ctx := benchContext()
	src := "(= total 0)\n(for i in (range 1000)\n  (= total (+ total (* i 2))))\n"
	expr, err := parser.NewParser().Parse(src)
	if err != nil {
		b.Fatalf("parse: %v", err)
	}
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if _, err := eval.Eval(expr, ctx); err != nil {
			b.Fatalf("eval: %v", err)
		}
	}
}

// BenchmarkContextLookupDeep isolates Context.lookupWithDepth: resolve a name
// that lives 8 scopes up the chain (a function calling through nested blocks).
func BenchmarkContextLookupDeep(b *testing.B) {
	root := benchContext()
	root.Define("target", core.NumberValue(42))
	leaf := root
	for range 8 {
		leaf = core.NewContext(leaf)
	}
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if _, err := leaf.Lookup("target"); err != nil {
			b.Fatalf("lookup: %v", err)
		}
	}
}

// BenchmarkNumericAddInt isolates the arithmetic operator dispatch for the
// common small-integer case.
func BenchmarkNumericAddInt(b *testing.B) {
	_ = benchContext() // ensure operators are registered in the global registry
	add := operators.Add()
	ctx := core.NewContext(nil)
	args := []core.Value{core.NumberValue(123456), core.NumberValue(7890)}
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if _, err := add(args, ctx); err != nil {
			b.Fatalf("add: %v", err)
		}
	}
}

// BenchmarkGetAttrMethod isolates attribute/method resolution on a container
// value (the GetAttrWithRegistry path).
func BenchmarkGetAttrMethod(b *testing.B) {
	_ = benchContext()
	var v core.Value = core.NewList(core.NumberValue(1), core.NumberValue(2), core.NumberValue(3))
	obj, ok := v.(interface {
		GetAttr(string) (core.Value, bool)
	})
	if !ok {
		b.Skip("list value does not implement GetAttr")
	}
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if _, ok := obj.GetAttr("append"); !ok {
			b.Fatal("append method not found")
		}
	}
}

// BenchmarkParseProgram measures parse throughput (a component of startup cost
// and of every -e/-c invocation).
func BenchmarkParseProgram(b *testing.B) {
	src := "(= total 0)\n(for i in (range 1000)\n  (= total (+ total (* i 2))))\n" +
		"(def f (n) (if (< n 2) n (+ (f (- n 1)) (f (- n 2)))))\n"
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if _, err := parser.NewParser().Parse(src); err != nil {
			b.Fatalf("parse: %v", err)
		}
	}
}

// --- Unboxing campaign stage-0 kernels (epic M28-9pm) ---
//
// Each kernel is a slot-compiled FUNCTION (simple positional signature, all
// modeled forms), so these measure the resolved IR path that value unboxing
// will change — not the module-level generic walker. They are the per-stage
// gates for the campaign: stage 2 (evalNum) must drive KernelLoopAccum's
// allocs/op toward zero without moving KernelOverflowPromote's results.

// kernelFn parses and defines src once, then returns the named callable.
func kernelFn(b *testing.B, src, name string) core.Callable {
	b.Helper()
	ctx := benchContext()
	expr, err := parser.NewParser().Parse(src)
	if err != nil {
		b.Fatalf("parse: %v", err)
	}
	if _, err := eval.Eval(expr, ctx); err != nil {
		b.Fatalf("def: %v", err)
	}
	fn, err := ctx.Lookup(name)
	if err != nil {
		b.Fatalf("lookup %s: %v", name, err)
	}
	callable, ok := fn.(core.Callable)
	if !ok {
		b.Fatalf("%s is not callable", name)
	}
	return callable
}

// BenchmarkKernelLoopAccum: the canonical loop-carried integer accumulator.
// Today every iteration boxes the induction variable, the multiply result,
// and the running total.
func BenchmarkKernelLoopAccum(b *testing.B) {
	fn := kernelFn(b, "(def kla (n) (do (= total 0) (for i (range n) (= total (+ total (* i 2)))) (return total)))", "kla")
	ctx := benchContext()
	args := []core.Value{core.NumberValue(1000)}
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if _, err := fn.Call(args, ctx); err != nil {
			b.Fatalf("call: %v", err)
		}
	}
}

// BenchmarkKernelMixedIntFloat: int induction variable against a float
// accumulator — the kind-transition case the tag design must keep cheap.
func BenchmarkKernelMixedIntFloat(b *testing.B) {
	fn := kernelFn(b, "(def kmf (n) (do (= total 0.5) (for i (range n) (= total (+ total (* i 1.5)))) (return total)))", "kmf")
	ctx := benchContext()
	args := []core.Value{core.NumberValue(1000)}
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if _, err := fn.Call(args, ctx); err != nil {
			b.Fatalf("call: %v", err)
		}
	}
}

// BenchmarkKernelOverflowPromote: every multiply leaves float64's exact-int
// range and promotes to bigint. Guards the promotion semantics and prices the
// slow path (which unboxing must not regress).
func BenchmarkKernelOverflowPromote(b *testing.B) {
	fn := kernelFn(b, "(def kop (n) (do (= x 4503599627370496) (= y 0) (for i (range n) (= y (* x 8))) (return y)))", "kop")
	ctx := benchContext()
	args := []core.Value{core.NumberValue(100)}
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if _, err := fn.Call(args, ctx); err != nil {
			b.Fatalf("call: %v", err)
		}
	}
}

// BenchmarkKernelCompareBranch: comparisons feeding branches — the bool-kind
// case (stage 4).
func BenchmarkKernelCompareBranch(b *testing.B) {
	fn := kernelFn(b, "(def kcb (n) (do (= c 0) (for i (range n) (if (< i 500) (= c (+ c 1)) (= c (+ c 2)))) (return c)))", "kcb")
	ctx := benchContext()
	args := []core.Value{core.NumberValue(1000)}
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if _, err := fn.Call(args, ctx); err != nil {
			b.Fatalf("call: %v", err)
		}
	}
}

// BenchmarkKernelCallFib: the call-boundary shape (stage 3's ABI decision):
// every recursive call boxes its argument and return value.
func BenchmarkKernelCallFib(b *testing.B) {
	fn := kernelFn(b, "(def kfib (n) (if (< n 2) (return n) (return (+ (kfib (- n 1)) (kfib (- n 2))))))", "kfib")
	ctx := benchContext()
	args := []core.Value{core.NumberValue(15)}
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if _, err := fn.Call(args, ctx); err != nil {
			b.Fatalf("call: %v", err)
		}
	}
}

// --- Stage-0 frame-representation spike (decides stage 1's layout) ---
//
// Two candidate tagged-frame layouts, measured on the access pattern the
// evalNum kernel will generate per loop iteration: guarded reads of two
// numeric slots, arithmetic, tagged write-back. The boxed variant is the
// current model, for scale. Indices are perturbed per iteration so the
// compiler cannot hoist the tag checks.

const (
	spikeKindBoxed = iota
	spikeKindInt
)

type spikeSlot struct {
	kind uint8
	num  float64
	ref  core.Value
}

var spikeSink float64

func BenchmarkFrameTaggedStruct(b *testing.B) {
	frame := make([]spikeSlot, 8)
	for i := range frame {
		frame[i] = spikeSlot{kind: spikeKindInt, num: float64(i)}
	}
	b.ReportAllocs()
	b.ResetTimer()
	var acc float64
	for i := 0; i < b.N; i++ {
		a, c := &frame[i&3], &frame[(i+1)&3]
		if a.kind == spikeKindInt && c.kind == spikeKindInt {
			r := a.num + c.num
			d := &frame[(i+2)&3]
			d.kind = spikeKindInt
			d.num = r
			acc += r
		}
	}
	spikeSink = acc
}

func BenchmarkFrameParallelArrays(b *testing.B) {
	nums := make([]float64, 8)
	tags := make([]uint8, 8)
	refs := make([]core.Value, 8)
	_ = refs
	for i := range nums {
		nums[i] = float64(i)
		tags[i] = spikeKindInt
	}
	b.ReportAllocs()
	b.ResetTimer()
	var acc float64
	for i := 0; i < b.N; i++ {
		ia, ic := i&3, (i+1)&3
		if tags[ia] == spikeKindInt && tags[ic] == spikeKindInt {
			r := nums[ia] + nums[ic]
			id := (i + 2) & 3
			tags[id] = spikeKindInt
			nums[id] = r
			acc += r
		}
	}
	spikeSink = acc
}

func BenchmarkFrameBoxed(b *testing.B) {
	frame := make([]core.Value, 8)
	for i := range frame {
		frame[i] = core.NumberValue(i)
	}
	b.ReportAllocs()
	b.ResetTimer()
	var acc float64
	for i := 0; i < b.N; i++ {
		a, aok := frame[i&3].(core.NumberValue)
		c, cok := frame[(i+1)&3].(core.NumberValue)
		if aok && cok {
			r := float64(a) + float64(c)
			frame[(i+2)&3] = core.NumberValue(r)
			acc += r
		}
	}
	spikeSink = acc
}

// BenchmarkKernelWhileLoop: the while-loop analogue of KernelLoopAccum — the
// condition runs through the typed kernel and the body in statement position
// (stage 4, whileNode).
func BenchmarkKernelWhileLoop(b *testing.B) {
	fn := kernelFn(b, "(def kwl (n) (do (= total 0) (= i 0) (while (< i n) (do (= total (+ total (* i 2))) (= i (+ i 1)))) (return total)))", "kwl")
	ctx := benchContext()
	args := []core.Value{core.NumberValue(1000)}
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if _, err := fn.Call(args, ctx); err != nil {
			b.Fatalf("call: %v", err)
		}
	}
}
