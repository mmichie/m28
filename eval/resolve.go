package eval

import (
	"reflect"
	"strings"

	"github.com/mmichie/m28/core"
)

// This file is the first piece of the "resolution layer": a static analysis
// that decides whether a function body is simple enough to execute with
// slot-indexed locals (a flat []core.Value frame) instead of M28's map-based
// scope lookup, and if so computes the local-name -> slot mapping.
//
// It is deliberately CONSERVATIVE: anything it does not fully model makes the
// function non-compilable, and the caller falls back to the existing tree
// walker. This keeps the optimization opt-in per function and the language
// semantics unchanged. Execution against the slot map is wired up separately.

// localResolution is the result of a successful analysis.
type localResolution struct {
	// slots maps each local variable name to its frame index. Parameters come
	// first (in declaration order), then names first assigned in the body.
	slots map[string]int
}

// slotSafeForms are the special-form heads whose local-binding behavior the
// analyzer fully understands. A body containing any OTHER special form is
// rejected (returns ok=false), because an unmodeled form might introduce a
// binding or scope effect the slot frame would get wrong.
//
// Beyond the control forms, this includes every special form whose children
// are pure value positions: their handlers evaluate each child through Eval
// (where slotRefs self-evaluate), they bind no names, and any structural
// markers they carry (`from` in raise, `*unpack`/`**unpack` in literals, the
// "__call__" StringValue in dot calls) are either non-identifier symbols that
// can never be locals or non-symbol values, so rewriteItems leaves them
// intact. Forms that treat identifier symbols structurally (quote/quasiquote,
// import, global/nonlocal) or bind names (def/lambda/class, with/try/except,
// match-stmt, annotated-assign, :=) must stay out.
var slotSafeForms = map[string]bool{
	"do":     true,
	"begin":  true,
	"=":      true,
	"for":    true,
	"while":  true,
	"if":     true,
	"return": true,
	// Loop control: zero-argument statements; ForForm/WhileForm (and seqNode)
	// already propagate their sentinel results.
	"break":    true,
	"continue": true,
	// Short-circuit boolean forms: operands are value positions.
	"and": true,
	"or":  true,
	// Subscript protocol: object, indices, and stored values are all values.
	"get-item":  true,
	"set-item":  true,
	"del-item":  true,
	"__slice__": true,
	// Container literals: elements (and spread markers, see above) are values.
	"dict-literal":  true,
	"list-literal":  true,
	"tuple-literal": true,
	// Value-position builtin forms.
	"isinstance": true,
	"issubclass": true,
	"raise":      true,
}

// dynamicScopeFns name builtins that read or mutate the local namespace by name
// at runtime. Their presence disqualifies slot compilation, since a slot frame
// has no name-addressable dict for them to observe.
var dynamicScopeFns = map[string]bool{
	"locals":     true,
	"globals":    true,
	"vars":       true,
	"exec":       true,
	"eval":       true,
	"__import__": true,
	"dir":        true,
}

// compHeads are the eagerly-evaluated comprehension forms the analyzer models
// as closed sub-scopes instead of disqualifiers. Their loop variable binds a
// child scope at runtime (comprehensionLoop's context or compileCompExpr's
// one-slot frame), never the function frame, so a function containing one can
// still be slot-compiled — provided the comprehension's innards (body,
// condition, non-first iterables) read no function locals, which analyzeLocals
// verifies via the free-name check. Generator expressions are excluded: they
// evaluate lazily, possibly after the frame is gone. Dict comprehensions are
// excluded until their argument shape is normalized.
var compHeads = map[string]bool{
	"list-comp": true,
	"set-comp":  true,
}

// analyzeLocals computes the slot map for a function with the given (simple,
// positional) parameter names and body. It returns ok=false if the body uses
// any construct the slot model does not yet handle, in which case the caller
// must keep using the map-based scope.
//
// isSpecialForm reports whether a head symbol names a special form; production
// callers pass the interpreter's registry, tests pass a controlled set.
func analyzeLocals(paramNames []string, body core.Value, isSpecialForm func(string) bool) (localResolution, bool) {
	slots := make(map[string]int)
	addLocal := func(name string) {
		if _, seen := slots[name]; !seen {
			slots[name] = len(slots)
		}
	}
	for _, p := range paramNames {
		addLocal(p)
	}

	safe := true
	// compFree collects the free names read inside comprehension innards; those
	// run against the map-scope chain (where slot locals are invisible), so any
	// overlap with the final local set disqualifies the function (checked after
	// the walk, when the local set is complete).
	compFree := make(map[string]bool)
	var walk func(core.Value)
	walk = func(v core.Value) {
		if !safe {
			return
		}
		v = unwrapLocated(v)
		if sym, ok := v.(core.SymbolValue); ok {
			// A bare read of a dynamic-namespace builtin (or super) can smuggle
			// a namespace observer past the head-position checks (f = locals;
			// f()), and super() resolves self/cls by name at call time. A slot
			// frame has no name-addressable dict for either, so disqualify.
			if dynamicScopeFns[string(sym)] || string(sym) == "super" {
				safe = false
			}
			return
		}
		lst, isList := v.(*core.ListValue)
		if !isList || lst.Len() == 0 {
			// Literal or other non-list: a value, never a binding.
			return
		}
		items := lst.ItemsRef()

		head, headIsSym := unwrapLocated(items[0]).(core.SymbolValue)
		if !headIsSym {
			// Computed callee, e.g. ((get-fn) args). Descend into everything.
			for _, it := range items {
				walk(it)
			}
			return
		}
		h := string(head)

		// A comprehension is a closed sub-scope: its first iterable runs in the
		// enclosing scope (walked normally, and later slot-rewritten), while its
		// innards bind only comprehension-local names at runtime.
		if compHeads[h] {
			if !walkCompForm(items, walk, compFree, isSpecialForm) {
				safe = false
			}
			return
		}

		// Attribute access / method call: (. obj attr args...). The attr at
		// items[2] is structural — an identifier that may collide with a local
		// name — and must not be treated as a read; obj and args are values.
		if h == "." {
			if lst.Len() < 3 {
				safe = false
				return
			}
			walk(items[1])
			for _, it := range items[3:] {
				walk(it)
			}
			return
		}

		// del: deleting an attribute or subscript ((del (. o a)) / (del
		// (get-item d k))) touches no local binding — walk the target's value
		// positions. Deleting a bare local would unbind a slot; reject.
		if h == "del" {
			for _, it := range items[1:] {
				if _, isSym := unwrapLocated(it).(core.SymbolValue); isSym {
					safe = false
					return
				}
				walk(it)
			}
			return
		}

		// Any special form we do not explicitly model disqualifies the function
		// (def/lambda/global/nonlocal/with/try/yield/generator-exp/...).
		if isSpecialForm(h) && !slotSafeForms[h] {
			safe = false
			return
		}

		switch h {
		case "=":
			// (= target value): a bare-symbol target binds a local. An
			// attribute or subscript target ((. obj attr) / (get-item d k))
			// binds nothing local — its object/index positions are value reads.
			if lst.Len() < 3 {
				safe = false
				return
			}
			tgtV := unwrapLocated(items[1])
			if tgt, ok := tgtV.(core.SymbolValue); ok {
				addLocal(string(tgt))
			} else if tgtList, ok := tgtV.(*core.ListValue); ok && tgtList.Len() > 0 {
				tHead, ok := unwrapLocated(tgtList.ItemsRef()[0]).(core.SymbolValue)
				if !ok || (string(tHead) != "." && string(tHead) != "get-item") {
					// Tuple and other targets not modeled yet.
					safe = false
					return
				}
				walk(items[1])
			} else {
				safe = false
				return
			}
			walk(items[2])

		case "for":
			// (for target [in] iter body...): target at items[1] binds a local.
			if lst.Len() < 3 {
				safe = false
				return
			}
			if tgt, ok := unwrapLocated(items[1]).(core.SymbolValue); ok {
				addLocal(string(tgt))
			} else {
				safe = false
				return
			}
			for _, it := range items[2:] {
				walk(it)
			}

		default:
			// A safe control form (do/while/if/return) or a function call.
			// Reject calls that can manipulate the namespace dynamically;
			// otherwise descend into callee and operands to find bindings.
			if dynamicScopeFns[h] {
				safe = false
				return
			}
			for _, it := range items {
				walk(it)
			}
		}
	}

	walk(body)
	if !safe {
		return localResolution{}, false
	}
	// Comprehension innards resolve names through the map-scope chain at
	// runtime; a function local read there would be invisible (slot frames have
	// no name-addressable dict), so any overlap disqualifies the function.
	for name := range compFree {
		if _, isLocal := slots[name]; isLocal {
			return localResolution{}, false
		}
	}
	return localResolution{slots: slots}, true
}

// walkCompForm validates a comprehension form during analysis. The first
// clause's iterable is enclosing-scope code and is walked with the normal
// walker (walkEnclosing); the innards — body expression, conditions, and the
// iterables of later clauses — execute in the comprehension's own child scope
// and are walked by walkCompInnards, which records their free names in free.
// Returns false for any shape or content the model cannot guarantee safe.
// items includes the head, so positions are shifted one right of the form
// handlers' args: (list-comp expr var iterable [cond]) has expr at items[1].
func walkCompForm(items []core.Value, walkEnclosing func(core.Value), free map[string]bool, isSpecialForm func(string) bool) bool {
	if len(items) < 3 {
		return false
	}
	// Multi-clause form: items[2] is the clauses list (mirrors the handlers'
	// detection, which type-asserts without unwrapping).
	if clauses, ok := items[2].(*core.ListValue); ok {
		if len(items) != 3 {
			return false
		}
		cl := clauses.ItemsRef()
		if len(cl) == 0 {
			return false
		}
		bound := make(map[string]bool, len(cl))
		for i, cv := range cl {
			clause, ok := cv.(*core.ListValue)
			if !ok || clause.Len() < 2 || clause.Len() > 3 {
				return false
			}
			cItems := clause.ItemsRef()
			varSym, ok := cItems[0].(core.SymbolValue)
			if !ok || strings.HasPrefix(string(varSym), "(") {
				// Tuple-pattern targets stay on the generic path.
				return false
			}
			if i == 0 {
				// First iterable: enclosing scope.
				walkEnclosing(cItems[1])
			} else if !walkCompInnards(cItems[1], bound, free, isSpecialForm) {
				return false
			}
			bound[string(varSym)] = true
			if clause.Len() == 3 && !walkCompInnards(cItems[2], bound, free, isSpecialForm) {
				return false
			}
		}
		return walkCompInnards(items[1], bound, free, isSpecialForm)
	}
	// Single-clause form: (head expr var iterable [cond]).
	if len(items) < 4 || len(items) > 5 {
		return false
	}
	varSym, ok := items[2].(core.SymbolValue)
	if !ok || strings.HasPrefix(string(varSym), "(") {
		return false
	}
	walkEnclosing(items[3])
	bound := map[string]bool{string(varSym): true}
	if len(items) == 5 && !walkCompInnards(items[4], bound, free, isSpecialForm) {
		return false
	}
	return walkCompInnards(items[1], bound, free, isSpecialForm)
}

// walkCompInnards validates comprehension-scope code and records the names it
// reads that are not comprehension variables (bound). Anything that could bind
// a name, observe the namespace dynamically, or that the model does not fully
// understand returns false. Call heads are recorded as free reads too — a
// function local shadowing a callee name must disqualify.
func walkCompInnards(v core.Value, bound, free map[string]bool, isSpecialForm func(string) bool) bool {
	v = unwrapLocated(v)
	switch n := v.(type) {
	case core.SymbolValue:
		if !bound[string(n)] {
			free[string(n)] = true
		}
		return true
	case *core.ListValue:
		if n.Len() == 0 {
			return true
		}
		items := n.ItemsRef()
		head, headIsSym := unwrapLocated(items[0]).(core.SymbolValue)
		if !headIsSym {
			for _, it := range items {
				if !walkCompInnards(it, bound, free, isSpecialForm) {
					return false
				}
			}
			return true
		}
		h := string(head)
		if compHeads[h] {
			return walkNestedCompInnards(items, bound, free, isSpecialForm)
		}
		if isSpecialForm(h) && !slotSafeForms[h] {
			return false
		}
		if dynamicScopeFns[h] {
			return false
		}
		// Bindings inside innards are comprehension-local at runtime; rather
		// than model that interaction, reject and keep the function generic.
		if h == "=" || h == "for" {
			return false
		}
		if !bound[h] {
			free[h] = true
		}
		for _, it := range items[1:] {
			if !walkCompInnards(it, bound, free, isSpecialForm) {
				return false
			}
		}
		return true
	default:
		return true
	}
}

// walkNestedCompInnards handles a comprehension nested inside another
// comprehension's innards. Unlike the outermost case, even its first iterable
// runs in the outer comprehension's scope, so every part is innards; its own
// variables extend the bound set for its body and condition.
func walkNestedCompInnards(items []core.Value, bound, free map[string]bool, isSpecialForm func(string) bool) bool {
	if len(items) < 3 {
		return false
	}
	inner := make(map[string]bool, len(bound)+2)
	for k := range bound {
		inner[k] = true
	}
	if clauses, ok := items[2].(*core.ListValue); ok {
		if len(items) != 3 {
			return false
		}
		cl := clauses.ItemsRef()
		if len(cl) == 0 {
			return false
		}
		for _, cv := range cl {
			clause, ok := cv.(*core.ListValue)
			if !ok || clause.Len() < 2 || clause.Len() > 3 {
				return false
			}
			cItems := clause.ItemsRef()
			varSym, ok := cItems[0].(core.SymbolValue)
			if !ok || strings.HasPrefix(string(varSym), "(") {
				return false
			}
			if !walkCompInnards(cItems[1], inner, free, isSpecialForm) {
				return false
			}
			inner[string(varSym)] = true
			if clause.Len() == 3 && !walkCompInnards(cItems[2], inner, free, isSpecialForm) {
				return false
			}
		}
		return walkCompInnards(items[1], inner, free, isSpecialForm)
	}
	if len(items) < 4 || len(items) > 5 {
		return false
	}
	varSym, ok := items[2].(core.SymbolValue)
	if !ok || strings.HasPrefix(string(varSym), "(") {
		return false
	}
	if !walkCompInnards(items[3], inner, free, isSpecialForm) {
		return false
	}
	inner[string(varSym)] = true
	if len(items) == 5 && !walkCompInnards(items[4], inner, free, isSpecialForm) {
		return false
	}
	return walkCompInnards(items[1], inner, free, isSpecialForm)
}

// sameValue reports whether a rewrite returned the identical value (change
// detection for alloc avoidance). Plain interface comparison panics when the
// dynamic type is or CONTAINS something uncomparable — BytesValue/TupleValue
// are slices, and LocatedValue is a struct whose interface field may hold one
// — so compare only kinds that cannot panic: pointer identity and scalar
// value types. Everything else answers "changed", which is always safe (it
// only costs a rebuild) and runs at compile time, off the hot path.
func sameValue(a, b core.Value) bool {
	if a == nil || b == nil {
		return a == nil && b == nil
	}
	ra, rb := reflect.ValueOf(a), reflect.ValueOf(b)
	if ra.Type() != rb.Type() {
		return false
	}
	switch ra.Kind() {
	case reflect.Pointer, reflect.Map, reflect.Chan, reflect.Func, reflect.UnsafePointer:
		return ra.Pointer() == rb.Pointer()
	case reflect.String, reflect.Bool,
		reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64,
		reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64,
		reflect.Float32, reflect.Float64, reflect.Complex64, reflect.Complex128:
		return a == b
	default:
		return false
	}
}

// slotRef is a resolved reference to a function-local stored in the slot frame
// (Context.Locals). resolveBody replaces a local variable's SymbolValue with a
// *slotRef at every read and assignment-target position in a slot-eligible
// body, so the evaluator indexes a slice instead of doing a scope-map lookup.
// name is kept only for the UnboundLocalError raised on an unbound slot read.
type slotRef struct {
	slot int
	name string
}

func (s *slotRef) Type() core.Type { return "slot-ref" }
func (s *slotRef) String() string  { return s.name }

// resolveBody rewrites a function body (already accepted by analyzeLocals, which
// produced slots) so every reference to a local — at read positions and as the
// target of `=`/`for` — becomes a *slotRef. Free names (builtins, globals,
// enclosing-scope reads) stay symbols and still resolve through the scope chain
// at run time. It returns ok=false on any construct it cannot safely rewrite —
// chiefly a call with keyword or *-unpack arguments, where a bare symbol is
// structural (the keyword name / unpack marker) rather than a variable read —
// in which case the caller runs the function the ordinary map-based way.
func resolveBody(body core.Value, slots map[string]int, isSpecialForm func(string) bool) (core.Value, bool) {
	return resolveNode(body, slots, isSpecialForm)
}

func resolveNode(v core.Value, slots map[string]int, isSpecialForm func(string) bool) (core.Value, bool) {
	// Preserve source-location wrappers: rewrite the inner value, re-wrap with
	// the same location so tracebacks inside slot functions keep their lines.
	if lv, ok := v.(core.LocatedValue); ok {
		inner, ok := resolveNode(lv.Value, slots, isSpecialForm)
		if !ok {
			return nil, false
		}
		if sameValue(inner, lv.Value) {
			return v, true
		}
		return core.LocatedValue{Value: inner, Location: lv.Location}, true
	}

	switch n := v.(type) {
	case core.SymbolValue:
		// A bare name: a local becomes a slot read, a free name stays a symbol.
		if idx, ok := slots[string(n)]; ok {
			return &slotRef{slot: idx, name: string(n)}, true
		}
		return v, true

	case *core.ListValue:
		if n.Len() == 0 {
			return v, true
		}
		items := n.ItemsRef()
		head, headIsSym := unwrapLocated(items[0]).(core.SymbolValue)
		if !headIsSym {
			// Computed callee, e.g. ((get-fn) args) — including keyword method
			// calls, which lower to ((. obj m) **unpack (dict-literal ...)).
			// Keyword/unpack markers are structural symbols, so such calls
			// cannot be slot-rewritten (same contract as the named-call branch).
			if callHasKeywordsOrUnpack(items) {
				return nil, false
			}
			return rewriteItems(v, items, slots, isSpecialForm)
		}
		h := string(head)

		// A comprehension: only its first iterable is enclosing-scope code, so
		// only that position is rewritten. Its innards were verified by
		// analyzeLocals to read no function locals; they are left untouched for
		// the form handler (and its own compiled fast path, compileCompExpr).
		if compHeads[h] {
			return resolveCompForm(v, items, slots, isSpecialForm)
		}

		// Attribute access / method call: rewrite the object and argument
		// positions; the attr name at items[2] is structural (it may equal a
		// local's name and must stay a bare symbol for DotForm).
		if h == "." {
			if len(items) < 3 {
				return nil, false
			}
			out := make([]core.Value, len(items))
			copy(out, items)
			changed := false
			for i, it := range items {
				if i == 0 || i == 2 {
					continue // head and attr name stay verbatim
				}
				r, ok := resolveNode(it, slots, isSpecialForm)
				if !ok {
					return nil, false
				}
				if !sameValue(r, it) {
					changed = true
				}
				out[i] = r
			}
			if !changed {
				return v, true
			}
			return core.NewList(out...), true
		}

		// del of an attribute/subscript target: value positions only
		// (analyzeLocals rejected bare-symbol deletes).
		if h == "del" {
			return rewriteItems(v, items, slots, isSpecialForm)
		}

		// Any unmodeled special form should already have been rejected by
		// analyzeLocals; bail defensively if the two passes ever disagree.
		if isSpecialForm(h) && !slotSafeForms[h] {
			return nil, false
		}

		switch h {
		case "=":
			// (= target value): a bare local becomes a slot target; an
			// attribute/subscript target list has its value positions rewritten
			// (the "." case above skips the attr name).
			if n.Len() < 3 {
				return nil, false
			}
			var tgt core.Value
			if _, isSym := unwrapLocated(items[1]).(core.SymbolValue); isSym {
				t, ok := resolveTarget(items[1], slots)
				if !ok {
					return nil, false
				}
				tgt = t
			} else if _, isList := unwrapLocated(items[1]).(*core.ListValue); isList {
				t, ok := resolveNode(items[1], slots, isSpecialForm)
				if !ok {
					return nil, false
				}
				tgt = t
			} else {
				return nil, false
			}
			val, ok := resolveNode(items[2], slots, isSpecialForm)
			if !ok {
				return nil, false
			}
			out := make([]core.Value, n.Len())
			copy(out, items)
			out[1] = tgt
			out[2] = val
			return core.NewList(out...), true

		case "for":
			// (for target [in] iter body...): the target binds a local slot.
			if n.Len() < 3 {
				return nil, false
			}
			tgt, ok := resolveTarget(items[1], slots)
			if !ok {
				return nil, false
			}
			out := make([]core.Value, n.Len())
			out[0] = items[0]
			out[1] = tgt
			for i := 2; i < len(items); i++ {
				r, ok := resolveNode(items[i], slots, isSpecialForm)
				if !ok {
					return nil, false
				}
				out[i] = r
			}
			return core.NewList(out...), true

		default:
			// A safe control form (do/begin/while/if/return) is all value
			// positions; its head is a reserved word never present in slots, so
			// rewriting from index 0 leaves the head untouched.
			if slotSafeForms[h] {
				return rewriteItems(v, items, slots, isSpecialForm)
			}
			// A function/operator call. Bail on keyword or unpack arguments,
			// where a bare symbol must not be turned into a slot read.
			if callHasKeywordsOrUnpack(items) {
				return nil, false
			}
			return rewriteItems(v, items, slots, isSpecialForm)
		}

	default:
		// Literals and other self-evaluating values: unchanged.
		return v, true
	}
}

// rewriteItems rebuilds a list with each element resolved as a value position.
// It returns the original list unchanged when nothing was rewritten, avoiding a
// needless allocation.
func rewriteItems(orig core.Value, items []core.Value, slots map[string]int, isSpecialForm func(string) bool) (core.Value, bool) {
	out := make([]core.Value, len(items))
	changed := false
	for i, it := range items {
		r, ok := resolveNode(it, slots, isSpecialForm)
		if !ok {
			return nil, false
		}
		if !sameValue(r, it) {
			changed = true
		}
		out[i] = r
	}
	if !changed {
		return orig, true
	}
	return core.NewList(out...), true
}

// resolveCompForm rewrites a comprehension form for a slot body: only the first
// clause's iterable — the one position evaluated in the enclosing scope — is
// resolved; the variable, body, condition, and later iterables are left intact
// (they run in the comprehension's own scope). Shape checks mirror walkCompForm;
// a mismatch means the passes disagree, so bail to the safe fallback.
func resolveCompForm(orig core.Value, items []core.Value, slots map[string]int, isSpecialForm func(string) bool) (core.Value, bool) {
	if len(items) < 3 {
		return nil, false
	}
	// Multi-clause: rewrite the first clause's iterable in place.
	if clauses, ok := items[2].(*core.ListValue); ok {
		if len(items) != 3 {
			return nil, false
		}
		cl := clauses.ItemsRef()
		if len(cl) == 0 {
			return nil, false
		}
		first, ok := cl[0].(*core.ListValue)
		if !ok || first.Len() < 2 {
			return nil, false
		}
		fItems := first.ItemsRef()
		newIter, ok := resolveNode(fItems[1], slots, isSpecialForm)
		if !ok {
			return nil, false
		}
		if sameValue(newIter, fItems[1]) {
			return orig, true
		}
		newFirst := make([]core.Value, len(fItems))
		copy(newFirst, fItems)
		newFirst[1] = newIter
		newClauses := make([]core.Value, len(cl))
		copy(newClauses, cl)
		newClauses[0] = core.NewList(newFirst...)
		out := make([]core.Value, len(items))
		copy(out, items)
		out[2] = core.NewList(newClauses...)
		return core.NewList(out...), true
	}
	// Single-clause: (head expr var iterable [cond]).
	if len(items) < 4 || len(items) > 5 {
		return nil, false
	}
	newIter, ok := resolveNode(items[3], slots, isSpecialForm)
	if !ok {
		return nil, false
	}
	if sameValue(newIter, items[3]) {
		return orig, true
	}
	out := make([]core.Value, len(items))
	copy(out, items)
	out[3] = newIter
	return core.NewList(out...), true
}

// resolveTarget rewrites an `=`/`for` assignment target. analyzeLocals
// guarantees it is a bare symbol already present in slots; anything else means
// the passes disagree, so bail to the safe fallback.
func resolveTarget(v core.Value, slots map[string]int) (core.Value, bool) {
	sym, ok := unwrapLocated(v).(core.SymbolValue)
	if !ok {
		return nil, false
	}
	idx, ok := slots[string(sym)]
	if !ok {
		return nil, false
	}
	return &slotRef{slot: idx, name: string(sym)}, true
}

// callHasKeywordsOrUnpack reports whether a call's arguments include a keyword
// (`name = value`, the flat run [sym, "=", value]) or a *args/**kwargs unpack
// marker. In those positions a bare symbol is structural, so the call cannot be
// slot-rewritten. The callee at items[0] is not examined.
func callHasKeywordsOrUnpack(items []core.Value) bool {
	for i := 1; i < len(items); i++ {
		sym, ok := unwrapLocated(items[i]).(core.SymbolValue)
		if !ok {
			continue
		}
		// *unpack, **unpack, and legacy *name / **name markers.
		if strings.HasPrefix(string(sym), "*") {
			return true
		}
		// Keyword argument: a bare "=" separates name and value.
		if i+1 < len(items) {
			if eq, ok := unwrapLocated(items[i+1]).(core.SymbolValue); ok && string(eq) == "=" {
				return true
			}
		}
	}
	return false
}

// isSpecialFormName reports whether s names a registered special form. It is the
// production predicate passed to analyzeLocals/resolveBody (tests pass their own
// controlled set).
func isSpecialFormName(s string) bool {
	_, ok := specialForms[s]
	return ok
}
