package vmspike

import (
	"testing"

	"github.com/mmichie/m28/core"
)

// TestResolvedMatches checks both resolved-tree variants compute the right
// result (and so are fair to benchmark against the others).
func TestResolvedMatches(t *testing.T) {
	setup()
	want := expected()

	fast := runResolved(buildResolved(func(l, r rnode) rnode { return rAddFast{l, r} }), benchN)
	if f, ok := fast.(core.NumberValue); !ok || float64(f) != want {
		t.Errorf("resolved-fast = %v, want %v", fast, want)
	}

	add := makeDispatchAdd(benchCtx)
	disp := runResolved(buildResolved(func(l, r rnode) rnode { return rAddDispatch{l, r, add} }), benchN)
	if d, ok := disp.(core.NumberValue); !ok || float64(d) != want {
		t.Errorf("resolved-dispatch = %v, want %v", disp, want)
	}
}

func BenchmarkResolvedDispatch(b *testing.B) {
	setup()
	add := makeDispatchAdd(benchCtx)
	tree := buildResolved(func(l, r rnode) rnode { return rAddDispatch{l, r, add} })
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		sinkV = runResolved(tree, benchN)
	}
}

func BenchmarkResolvedFast(b *testing.B) {
	setup()
	tree := buildResolved(func(l, r rnode) rnode { return rAddFast{l, r} })
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		sinkV = runResolved(tree, benchN)
	}
}
