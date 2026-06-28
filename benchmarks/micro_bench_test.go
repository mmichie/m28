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
