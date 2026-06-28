package eval

import "github.com/mmichie/m28/core"

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
