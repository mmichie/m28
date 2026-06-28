package eval

import (
	"testing"

	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/parser"
)

// testSpecials is the set of special-form heads the analyzer reasons about,
// independent of the live interpreter registry (so these tests do not depend on
// global registration order or mutate it).
var testSpecials = map[string]bool{
	"do": true, "begin": true, "=": true, "for": true, "while": true,
	"if": true, "return": true,
	"def": true, "lambda": true, "global": true, "nonlocal": true,
	"del": true, "with": true, "try": true, "yield": true,
}

func testIsSpecial(h string) bool { return testSpecials[h] }

func mustParse(t *testing.T, src string) core.Value {
	t.Helper()
	v, err := parser.NewParser().Parse(src)
	if err != nil {
		t.Fatalf("parse %q: %v", src, err)
	}
	return v
}

func TestAnalyzeLocals_SimpleFunctionGetsSlots(t *testing.T) {
	// A scope_lookup-style body: only =, for, arithmetic, and a final read.
	body := mustParse(t, "(do (= a 1) (= b 2) (= total 0) "+
		"(for i in (range 5) (= total (+ total a b i))) total)")
	res, ok := analyzeLocals(nil, body, testIsSpecial)
	if !ok {
		t.Fatal("expected a simple function to be slot-compilable")
	}
	for _, name := range []string{"a", "b", "total", "i"} {
		if _, has := res.slots[name]; !has {
			t.Errorf("expected a slot for local %q; slots=%v", name, res.slots)
		}
	}
	// Names that are merely referenced (builtins/operators) are not locals.
	if _, has := res.slots["range"]; has {
		t.Error("range is a free name, must not get a slot")
	}
}

func TestAnalyzeLocals_ParamsTakeFirstSlots(t *testing.T) {
	body := mustParse(t, "(do (if (< n 2) (return n)) (return (+ n 1)))")
	res, ok := analyzeLocals([]string{"n"}, body, testIsSpecial)
	if !ok {
		t.Fatal("expected ok for an if/return body")
	}
	if res.slots["n"] != 0 {
		t.Errorf("parameter n should occupy slot 0, got %d", res.slots["n"])
	}
}

func TestAnalyzeLocals_RejectsUnmodeledConstructs(t *testing.T) {
	cases := map[string]string{
		"nested def":    "(do (def (g x) x) 1)",
		"lambda":        "(do (= f (lambda (x) x)) 1)",
		"global":        "(do (global x) (= x 1))",
		"nonlocal":      "(do (nonlocal x))",
		"del":           "(do (= x 1) (del x))",
		"with":          "(do (with (open f) as h) 1)",
		"tuple target":  "(do (= (a b) (1 2)))",
		"locals() call": "(do (= x (locals)))",
	}
	for name, src := range cases {
		body := mustParse(t, src)
		if _, ok := analyzeLocals(nil, body, testIsSpecial); ok {
			t.Errorf("%s: expected the analyzer to reject this body", name)
		}
	}
}
