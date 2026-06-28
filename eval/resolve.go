package eval

import (
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
var slotSafeForms = map[string]bool{
	"do":     true,
	"begin":  true,
	"=":      true,
	"for":    true,
	"while":  true,
	"if":     true,
	"return": true,
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
	var walk func(core.Value)
	walk = func(v core.Value) {
		if !safe {
			return
		}
		v = unwrapLocated(v)
		lst, isList := v.(*core.ListValue)
		if !isList || lst.Len() == 0 {
			// Symbol reference, literal, etc. -- a read, never a binding.
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

		// Any special form we do not explicitly model disqualifies the function
		// (def/lambda/global/nonlocal/del/with/try/yield/comprehensions/...).
		if isSpecialForm(h) && !slotSafeForms[h] {
			safe = false
			return
		}

		switch h {
		case "=":
			// (= target value): only a bare-symbol target binds a local here.
			if lst.Len() < 3 {
				safe = false
				return
			}
			if tgt, ok := unwrapLocated(items[1]).(core.SymbolValue); ok {
				addLocal(string(tgt))
			} else {
				// Tuple/attr/index targets not modeled yet.
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
	return localResolution{slots: slots}, true
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
		if inner == lv.Value {
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
			// Computed callee, e.g. ((get-fn) args): every element is a value.
			return rewriteItems(v, items, slots, isSpecialForm)
		}
		h := string(head)

		// Any unmodeled special form should already have been rejected by
		// analyzeLocals; bail defensively if the two passes ever disagree.
		if isSpecialForm(h) && !slotSafeForms[h] {
			return nil, false
		}

		switch h {
		case "=":
			// (= target value): target is a bare local (analyzeLocals ensured it).
			if n.Len() < 3 {
				return nil, false
			}
			tgt, ok := resolveTarget(items[1], slots)
			if !ok {
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
		if r != it {
			changed = true
		}
		out[i] = r
	}
	if !changed {
		return orig, true
	}
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
