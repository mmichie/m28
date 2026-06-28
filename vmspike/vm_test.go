package vmspike

import (
	"fmt"
	"sync"
	"testing"

	"github.com/mmichie/m28/builtin"
	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/eval"
	"github.com/mmichie/m28/parser"
	"github.com/mmichie/m28/special_forms"
)

// benchN is the loop trip count, shared by the tree-walker program and the VM.
const benchN = 2000

var (
	setupOnce sync.Once
	benchCtx  *core.Context
	benchProg core.Value
)

// expected is the closed-form result: sum over i in [0,N) of (1+2+3+4+5+i).
func expected() float64 {
	return float64(15*benchN) + float64(benchN*(benchN-1))/2
}

func setup() {
	setupOnce.Do(func() {
		ctx := core.NewContext(nil)
		ctx.Define("true", core.BoolValue(true))
		ctx.Define("false", core.BoolValue(false))
		ctx.Define("True", core.BoolValue(true))
		ctx.Define("False", core.BoolValue(false))
		ctx.Define("None", core.None)
		ctx.Define("nil", core.NilValue{})
		builtin.RegisterAllBuiltins(ctx)
		special_forms.RegisterAllForms()
		benchCtx = ctx

		// (do ...) sequences the statements and yields the last expression (total).
		src := fmt.Sprintf("(do (= a 1) (= b 2) (= c 3) (= d 4) (= e 5) (= total 0) "+
			"(for i in (range %d) (= total (+ total a b c d e i))) total)", benchN)
		prog, err := parser.NewParser().Parse(src)
		if err != nil {
			panic(err)
		}
		benchProg = prog
	})
}

// TestVMMatchesTreeWalk asserts all three engines compute the same result, so
// the benchmark below is a fair, apples-to-apples comparison.
func TestVMMatchesTreeWalk(t *testing.T) {
	setup()
	want := expected()

	tw, err := eval.Eval(benchProg, benchCtx)
	if err != nil {
		t.Fatalf("tree-walk eval: %v", err)
	}
	twNum, ok := tw.(core.NumberValue)
	if !ok {
		t.Fatalf("tree-walk returned %T, want NumberValue", tw)
	}
	if float64(twNum) != want {
		t.Errorf("tree-walk = %v, want %v", float64(twNum), want)
	}

	code := scopeLookupCode()
	boxed, ok := RunBoxed(code, benchN).(core.NumberValue)
	if !ok || float64(boxed) != want {
		t.Errorf("vm-boxed = %v, want %v", boxed, want)
	}
	if unboxed := RunUnboxed(code, benchN); unboxed != want {
		t.Errorf("vm-unboxed = %v, want %v", unboxed, want)
	}
}

var (
	sinkV core.Value
	sinkF float64
)

func BenchmarkTreeWalk(b *testing.B) {
	setup()
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		v, err := eval.Eval(benchProg, benchCtx)
		if err != nil {
			b.Fatal(err)
		}
		sinkV = v
	}
}

func BenchmarkVMBoxed(b *testing.B) {
	setup()
	code := scopeLookupCode()
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		sinkV = RunBoxed(code, benchN)
	}
}

func BenchmarkVMUnboxed(b *testing.B) {
	setup()
	code := scopeLookupCode()
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		sinkF = RunUnboxed(code, benchN)
	}
}
