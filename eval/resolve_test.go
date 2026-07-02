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
	"list-comp": true, "set-comp": true, "dict-comp": true, "gen-expr": true,
	"generator-exp": true,
	"match-stmt": true, "quote": true, "quasiquote": true, ":=": true,
	"annotated-assign": true, ".": true,
	"and": true, "or": true, "break": true, "continue": true,
	"get-item": true, "set-item": true, "del-item": true, "__slice__": true,
	"dict-literal": true, "list-literal": true, "tuple-literal": true,
	"isinstance": true, "issubclass": true, "raise": true,
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

// collectResolved walks a rewritten body, returning the (name->slot) of every
// *slotRef it contains and the set of bare symbol names left unresolved.
func collectResolved(v core.Value) (slotRefs map[string]int, symbols map[string]bool) {
	slotRefs = map[string]int{}
	symbols = map[string]bool{}
	var walk func(core.Value)
	walk = func(v core.Value) {
		if lv, ok := v.(core.LocatedValue); ok {
			walk(lv.Value)
			return
		}
		switch n := v.(type) {
		case *slotRef:
			slotRefs[n.name] = n.slot
		case core.SymbolValue:
			symbols[string(n)] = true
		case *core.ListValue:
			for _, it := range n.ItemsRef() {
				walk(it)
			}
		}
	}
	walk(v)
	return
}

func TestResolveBody_RewritesLocalsToSlots(t *testing.T) {
	body := mustParse(t, "(do (= a 1) (= b 2) (return (+ a b c)))")
	res, ok := analyzeLocals(nil, body, testIsSpecial)
	if !ok {
		t.Fatal("expected analyzable body")
	}
	rewritten, ok := resolveBody(body, res.slots, testIsSpecial)
	if !ok {
		t.Fatal("expected resolveBody to succeed")
	}

	refs, syms := collectResolved(rewritten)
	// Locals a, b become slot refs at their analyzed indices.
	if refs["a"] != res.slots["a"] || refs["b"] != res.slots["b"] {
		t.Errorf("locals not resolved to their slots: refs=%v slots=%v", refs, res.slots)
	}
	// Free name c stays a symbol; locals must NOT linger as bare symbols.
	if !syms["c"] {
		t.Error("free name c should remain a bare symbol")
	}
	if syms["a"] || syms["b"] {
		t.Errorf("locals a/b should not remain bare symbols: %v", syms)
	}
}

func TestResolveBody_RewritesForTarget(t *testing.T) {
	body := mustParse(t, "(do (= total 0) (for i in (range 5) (= total (+ total i))) total)")
	res, ok := analyzeLocals(nil, body, testIsSpecial)
	if !ok {
		t.Fatal("expected analyzable body")
	}
	rewritten, ok := resolveBody(body, res.slots, testIsSpecial)
	if !ok {
		t.Fatal("expected resolveBody to succeed")
	}
	refs, syms := collectResolved(rewritten)
	if _, has := refs["i"]; !has {
		t.Error("for-loop target i should be resolved to a slot")
	}
	if syms["i"] || syms["total"] {
		t.Errorf("loop target and accumulator should not remain bare symbols: %v", syms)
	}
	// The 'in' keyword and the free name range stay symbols.
	if !syms["in"] || !syms["range"] {
		t.Errorf("structural 'in' and free 'range' should remain symbols: %v", syms)
	}
}

func TestResolveBody_BailsOnKeywordCall(t *testing.T) {
	// A keyword argument is the flat run [sym, "=", value]; rewriting the name
	// would break keyword detection, so resolveBody must bail.
	kwCall := core.NewList(
		core.SymbolValue("f"),
		core.SymbolValue("a"), core.SymbolValue("="), core.NumberValue(1),
	)
	if !callHasKeywordsOrUnpack(kwCall.ItemsRef()) {
		t.Fatal("callHasKeywordsOrUnpack should flag a keyword argument")
	}
	body := core.NewList(core.SymbolValue("do"), kwCall)
	slots := map[string]int{"a": 0}
	if _, ok := resolveBody(body, slots, testIsSpecial); ok {
		t.Error("resolveBody should bail on a call with keyword arguments")
	}

	// An ordinary positional call resolves fine.
	posCall := core.NewList(core.SymbolValue("f"), core.SymbolValue("a"))
	if callHasKeywordsOrUnpack(posCall.ItemsRef()) {
		t.Error("a positional call must not be flagged as keyword/unpack")
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

// --- comprehension containment (compHeads) ---

func TestAnalyzeLocals_CompInnardsOnlyCompVarAndGlobals(t *testing.T) {
	// The comprehension body reads only its own variable; the first iterable
	// reads a function local (fine: it runs in the enclosing scope).
	body := mustParse(t, "(do (= n 5) (= r (list-comp (* x x) x (range n))) (return r))")
	res, ok := analyzeLocals(nil, body, testIsSpecial)
	if !ok {
		t.Fatal("a function whose comprehension innards read no locals must stay slot-compilable")
	}
	if _, has := res.slots["x"]; has {
		t.Error("the comprehension variable binds a child scope, not a function slot")
	}
	for _, name := range []string{"n", "r"} {
		if _, has := res.slots[name]; !has {
			t.Errorf("expected slot for local %q; slots=%v", name, res.slots)
		}
	}
}

func TestAnalyzeLocals_CompInnardsReadingLocalRejected(t *testing.T) {
	cases := map[string]string{
		"body reads local":       "(do (= k 1) (= r (list-comp (+ x k) x (range 5))))",
		"cond reads local":       "(do (= k 1) (= r (list-comp x x (range 5) (< x k))))",
		"body reads param":       "paramcase", // handled below with params
		"body calls local":       "(do (= f 1) (= r (list-comp (f x) x (range 5))))",
		"assign inside body":     "(do (= r (list-comp (= y 5) x (range 5))))",
		"locals() inside body":   "(do (= r (list-comp (locals) x (range 5))))",
		"gen-expr still bails":   "(do (= r (gen-expr x x (range 5))))",
		"dict-comp still bails":  "(do (= r (dict-comp x x x (range 5))))",
		"tuple comp var":         "(do (= r (list-comp x (a, b) pairs)))",
	}
	for name, src := range cases {
		var (
			body   core.Value
			params []string
		)
		if src == "paramcase" {
			body = mustParse(t, "(return (list-comp (+ x k) x (range 5)))")
			params = []string{"k"}
		} else {
			body = mustParse(t, src)
		}
		if _, ok := analyzeLocals(params, body, testIsSpecial); ok {
			t.Errorf("%s: expected the analyzer to reject this body", name)
		}
	}
}

func TestAnalyzeLocals_CompVarMayShadowLocal(t *testing.T) {
	// The comprehension variable shadows a function local: innards see the
	// comprehension binding (child scope), the first iterable sees the local.
	// Neither engine lets the comprehension touch the function's x, so the
	// function stays compilable.
	body := mustParse(t, "(do (= x 3) (= r (list-comp x x (range x))) (return r))")
	res, ok := analyzeLocals(nil, body, testIsSpecial)
	if !ok {
		t.Fatal("comp var shadowing a local must not disqualify the function")
	}
	if _, has := res.slots["x"]; !has {
		t.Error("the function local x still needs its slot")
	}
}

func TestAnalyzeLocals_NestedCompAndMultiClause(t *testing.T) {
	// Nested comprehension reading only comp vars and globals: fine.
	ok1 := "(do (= r (list-comp (list-comp (* x y) y (range x)) x (range 3))))"
	if _, ok := analyzeLocals(nil, mustParse(t, ok1), testIsSpecial); !ok {
		t.Error("nested comprehension over comp vars/globals must be accepted")
	}
	// Multi-clause: a later clause's iterable runs inside the comprehension, so
	// a function-local read there disqualifies.
	bad := "(do (= k 1) (= r (list-comp x ((a (range 3)) (x (range k))))))"
	if _, ok := analyzeLocals(nil, mustParse(t, bad), testIsSpecial); ok {
		t.Error("later-clause iterable reading a local must be rejected")
	}
	// ...but reading an earlier clause's variable is fine.
	good := "(do (= r (list-comp x ((a (range 3)) (x (range a))))))"
	if _, ok := analyzeLocals(nil, mustParse(t, good), testIsSpecial); !ok {
		t.Error("later-clause iterable reading an earlier comp var must be accepted")
	}
}

func TestResolveBody_CompFirstIterableRewritten(t *testing.T) {
	body := mustParse(t, "(do (= n 5) (= r (list-comp (* x x) x (range n))) (return r))")
	res, ok := analyzeLocals(nil, body, testIsSpecial)
	if !ok {
		t.Fatal("analyzeLocals rejected a containable comprehension body")
	}
	rewritten, ok := resolveBody(body, res.slots, testIsSpecial)
	if !ok {
		t.Fatal("resolveBody bailed on a containable comprehension body")
	}
	slotRefs, symbols := collectResolved(rewritten)
	if _, has := slotRefs["n"]; !has {
		t.Error("the first iterable's local read (n) must be slot-rewritten")
	}
	if symbols["n"] {
		t.Error("no bare symbol n should remain after rewriting")
	}
	// The comprehension body's variable stays a bare symbol for the handler.
	if _, has := slotRefs["x"]; has {
		t.Error("the comprehension variable must not be slot-rewritten")
	}
	if !symbols["x"] {
		t.Error("the comprehension variable should remain a symbol")
	}
}

// --- widened form coverage (dot access, subscripts, literals, loop control) ---

func TestAnalyzeLocals_MethodShapesAccepted(t *testing.T) {
	// A typical method body: attribute reads/writes, subscripts, literals,
	// and/or, break/continue — all now modeled.
	body := mustParse(t, "(do"+
		" (= (. self x) x)"+
		" (= (get-item (. self d) k) 1)"+
		" (= v (+ (get-item (. self d) k) (. self x)))"+
		" (for i (list-literal 1 2 3) (do (if (and (> i 0) (< i 2)) (break)) (continue)))"+
		" (if (or (isinstance v int) False) (raise (ValueError v)))"+
		" (del (get-item (. self d) k))"+
		" (return (tuple-literal v (dict-literal k v))))")
	res, ok := analyzeLocals([]string{"self", "x", "k"}, body, testIsSpecial)
	if !ok {
		t.Fatal("method-shaped body must be slot-compilable")
	}
	// The attribute name x (in self.x) must not create a second slot: x is the
	// param; the structural attr position is skipped.
	if res.slots["x"] != 1 {
		t.Errorf("param x should be slot 1, got %v", res.slots)
	}
	for _, name := range []string{"v", "i"} {
		if _, has := res.slots[name]; !has {
			t.Errorf("expected slot for local %q; slots=%v", name, res.slots)
		}
	}
}

func TestResolveBody_DotAttrNameNotRewritten(t *testing.T) {
	// The attr name collides with a local; only the object and value positions
	// may become slot refs.
	body := mustParse(t, "(do (= x 1) (= (. obj x) x) (return (. obj x)))")
	res, ok := analyzeLocals([]string{"obj"}, body, testIsSpecial)
	if !ok {
		t.Fatal("dot-target body must analyze")
	}
	rewritten, ok := resolveBody(body, res.slots, testIsSpecial)
	if !ok {
		t.Fatal("dot-target body must resolve")
	}
	refs, syms := collectResolved(rewritten)
	if _, has := refs["obj"]; !has {
		t.Error("object position should be slot-rewritten")
	}
	if _, has := refs["x"]; !has {
		t.Error("value reads of x should be slot-rewritten")
	}
	// The structural attr name stays a bare symbol in both dot forms.
	if !syms["x"] {
		t.Error("the attr-name x must remain a bare symbol")
	}
}

func TestAnalyzeLocals_StillRejectsUnsafe(t *testing.T) {
	cases := map[string]string{
		"bare del of local":     "(do (= x 1) (del x))",
		"super aliased":         "(do (= s super) (return (s)))",
		"locals aliased":        "(do (= f locals) (return (f)))",
		"walrus":                "(do (return (:= y 5)))",
		"annotated assign":      "(do (annotated-assign y int 5))",
		"with":                  "(do (with (open f) as h) 1)",
		"try":                   "(do (try (= x 1)))",
		"match":                 "(do (match-stmt x))",
		"quote":                 "(do (quote (a b)))",
		"keyword method call":   "(do (return ((. obj m) **unpack (dict-literal k 1))))",
		"tuple assign target":   "(do (= (a b) (tuple-literal 1 2)))",
	}
	for name, src := range cases {
		body := mustParse(t, src)
		res, ok := analyzeLocals(nil, body, testIsSpecial)
		if !ok {
			continue // rejected at analysis: good
		}
		// Some shapes pass analysis but must fail the rewrite (keyword calls).
		if _, ok := resolveBody(body, res.slots, testIsSpecial); ok {
			t.Errorf("%s: expected analysis or resolve to reject this body", name)
		}
	}
}
