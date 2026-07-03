package eval

import (
	"fmt"

	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/core/protocols"
)

// This file is resolution layer 3: it compiles a slot-eligible function body
// (already rewritten by resolveBody so locals are *slotRef) into a small tree
// of IR nodes that evaluate themselves directly, bypassing the generic Eval
// type-switch, the per-special-form argument-list copy, and the keyword/unpack
// machinery of the generic call path.
//
// Design notes:
//   - IR nodes implement core.Value and a private evalIR method. Eval gains a
//     single `case irNode` so a node can be reached from anywhere a core.Value
//     is evaluated (notably when ForForm/WhileForm/DoForm evaluate a child).
//   - LocatedValue wrappers are PRESERVED. compileIR rewrites the value inside a
//     wrapper but keeps the wrapper, so Eval still pushes/pops source locations
//     exactly as before; tracebacks and error locations are unchanged.
//   - compileIR never fails. Any construct it does not model is returned
//     unchanged (still slot-rewritten), so Eval runs it the layer-2 way. This
//     keeps the optimization a strict subset with a always-correct fallback.
//   - Eligibility is unchanged: analyzeLocals already guarantees a body contains
//     only do/begin/=/for/while/if/return special forms, comprehension forms
//     (compHeads, routed back to their handlers), and positional calls, so the
//     heads compileIR must recognize are a closed set.

// irNode is a compiled, self-evaluating expression node.
type irNode interface {
	core.Value
	evalIR(ctx *core.Context) (core.Value, error)
}

// fastOpNames are the binary operators compileIR lowers to an operatorNode: a
// direct, cached call to the registry operator with an inline native-number
// fast path. Operators always resolve from the global registry ahead of any
// binding (see Context.Lookup), so caching the callable matches normal dispatch.
var fastOpNames = map[string]bool{
	"+": true, "-": true, "*": true, "/": true, "//": true, "%": true, "**": true,
	"==": true, "!=": true, "<": true, "<=": true, ">": true, ">=": true,
}

// compileIR turns a slot-rewritten value into an IR node, or returns it
// unchanged when it models nothing faster (the Eval fallback).
func compileIR(v core.Value) core.Value {
	switch n := v.(type) {
	case core.LocatedValue:
		inner := compileIR(n.Value)
		if sameValue(inner, n.Value) {
			return v
		}
		// Keep the wrapper so Eval still tracks the source location.
		return core.LocatedValue{Value: inner, Location: n.Location}

	case *slotRef:
		// A local read: already an irNode (see evalIR below).
		return v

	case core.SymbolValue:
		return &symbolNode{name: string(n)}

	case core.NumberValue, core.StringValue, core.BoolValue, core.NilValue:
		return newConstNode(v)

	case *core.ListValue:
		return compileList(n, v)

	default:
		// Dict/set literals and anything else self-evaluating: leave to Eval.
		return v
	}
}

// compileList dispatches on a list's head. orig is the original list value,
// returned verbatim whenever a form is not modeled.
func compileList(n *core.ListValue, orig core.Value) core.Value {
	if n.Len() == 0 {
		return orig
	}
	items := n.ItemsRef()
	head, ok := unwrapLocated(items[0]).(core.SymbolValue)
	if !ok {
		// Computed callee, e.g. ((get-fn) args): a positional call.
		return compileCallChecked(items, "", orig)
	}

	switch h := string(head); h {
	case "do", "begin":
		return compileSeq(items)
	case "if":
		return compileIf(n, items, orig)
	case "return":
		return compileReturn(items, orig)
	case "=":
		return compileAssign(items, orig)
	case "for":
		return compileFor(items, orig)
	case "while":
		return compileWhile(items, orig)
	case ".":
		return compileDot(items, orig)
	default:
		// Any special form that analysis admits but this compiler does not
		// model (dot access, and/or, literals, break/continue, comprehensions,
		// raise, subscripts, ...) must run through its handler via generic
		// Eval — the rewritten tree still carries slotRefs, which
		// self-evaluate. Compiling it as a call would misroute it to a
		// same-named function lookup.
		if isSpecialFormName(h) {
			return orig
		}
		// A non-special-form head: an operator or function call.
		if fastOpNames[h] {
			if fn, ok := core.GetOperatorFunc(h); ok {
				return compileOperator(h, fn, items)
			}
		}
		return compileCallChecked(items, h, orig)
	}
}

// --- the typed evaluation kernel (unboxing stage 2, M28-9pm.3) ---
//
// evalNumOf evaluates an expression preferring the unboxed path: numeric
// results come back as a raw float64 plus kind (core.SlotInt/SlotFloat)
// without ever materializing a box. THE CONTRACT: evaluation happens exactly
// once — when the expression cannot stay unboxed, the boxed value it already
// computed is returned (boxed != nil); callers must never re-evaluate.
// Anything not modeled here falls to generic Eval and is classified after the
// fact, which is free (two type asserts) and lets a numeric-returning call
// feed an unboxed consumer.
func evalNumOf(e core.Value, ctx *core.Context) (num float64, kind uint8, boxed core.Value, err error) {
	if located, ok := e.(core.LocatedValue); ok {
		if located.Location != nil {
			ctx.PushLocation(located.Location)
			defer ctx.PopLocation()
		}
		e = located.Unwrap()
	}
	switch n := e.(type) {
	case *slotRef:
		if f, k, ok := ctx.Locals.GetNum(n.slot); ok {
			return f, k, nil, nil
		}
		v := ctx.Locals.Get(n.slot)
		if v == nil {
			return 0, 0, nil, core.WrapEvalError(
				&core.UnboundLocalError{Name: n.name, Location: ctx.CurrentLocation()},
				"unbound local", ctx)
		}
		return 0, core.SlotBoxed, v, nil
	case *constNode:
		if n.kind != core.SlotBoxed {
			return n.num, n.kind, nil, nil
		}
		return 0, core.SlotBoxed, n.v, nil
	case *operatorNode:
		return n.evalNum(ctx)
	default:
		v, err := Eval(e, ctx)
		if err != nil {
			return 0, 0, nil, err
		}
		switch t := v.(type) {
		case core.NumberValue:
			return float64(t), core.SlotInt, nil, nil
		case core.FloatValue:
			return float64(t), core.SlotFloat, nil, nil
		}
		return 0, core.SlotBoxed, v, nil
	}
}

// boxNum materializes an evalNumOf result that needs to escape.
func boxNum(num float64, kind uint8) core.Value {
	if kind == core.SlotFloat {
		return core.FloatValue(num)
	}
	return core.BoxNumber(num)
}

// --- leaf nodes ---

// constNode is a self-evaluating literal captured at compile time. Numeric
// constants carry their pre-classified kind and raw value so the typed kernel
// reads them without unwrapping.
type constNode struct {
	v    core.Value
	num  float64
	kind uint8 // core.SlotInt / core.SlotFloat / core.SlotBoxed
}

func newConstNode(v core.Value) *constNode {
	switch t := v.(type) {
	case core.NumberValue:
		return &constNode{v: v, num: float64(t), kind: core.SlotInt}
	case core.FloatValue:
		return &constNode{v: v, num: float64(t), kind: core.SlotFloat}
	}
	return &constNode{v: v, kind: core.SlotBoxed}
}

func (n *constNode) Type() core.Type                              { return n.v.Type() }
func (n *constNode) String() string                               { return n.v.String() }
func (n *constNode) evalIR(ctx *core.Context) (core.Value, error) { return n.v, nil }

// symbolNode is a free-name read (builtin, global, or enclosing-scope local).
// It mirrors Eval's SymbolValue case, including NameError location annotation.
type symbolNode struct{ name string }

func (n *symbolNode) Type() core.Type { return "symbol-ref" }
func (n *symbolNode) String() string  { return n.name }
func (n *symbolNode) evalIR(ctx *core.Context) (core.Value, error) {
	val, err := ctx.Lookup(n.name)
	if err != nil {
		if nameErr, ok := err.(*core.NameError); ok {
			nameErr.Location = ctx.CurrentLocation()
		}
		return nil, core.WrapEvalError(err, "name error", ctx)
	}
	return val, nil
}

// evalIR for *slotRef: a direct slot-frame read. Mirrors Eval's *slotRef case.
func (s *slotRef) evalIR(ctx *core.Context) (core.Value, error) {
	val := ctx.Locals.Get(s.slot)
	if val == nil {
		return nil, core.WrapEvalError(
			&core.UnboundLocalError{Name: s.name, Location: ctx.CurrentLocation()},
			"unbound local", ctx)
	}
	return val, nil
}

// --- sequence / control nodes ---

// seqNode is a (do ...) / (begin ...) block. Mirrors DoForm, including
// propagation of return/break/continue sentinels.
type seqNode struct{ items []core.Value }

// stmtNode is implemented by IR nodes that can execute in statement position
// — result discarded — without materializing it. execStmt returns a non-nil
// control value ONLY for flow sentinels (Return/Break/Continue), which the
// caller must propagate; ordinary results are dropped at the source, which is
// where the loop-carried rebox-per-iteration would otherwise happen.
type stmtNode interface {
	execStmt(ctx *core.Context) (control core.Value, err error)
}

// evalStmt evaluates e in statement position: stmtNodes skip materializing
// their result; everything else evaluates normally and reports sentinels.
// LocatedValue wrappers maintain the location stack exactly as Eval would.
func evalStmt(e core.Value, ctx *core.Context) (control core.Value, err error) {
	if lv, ok := e.(core.LocatedValue); ok {
		if lv.Location != nil {
			ctx.PushLocation(lv.Location)
			defer ctx.PopLocation()
		}
		e = lv.Value
	}
	if s, ok := e.(stmtNode); ok {
		return s.execStmt(ctx)
	}
	v, err := Eval(e, ctx)
	if err != nil {
		return nil, err
	}
	switch v.(type) {
	case *ReturnValue, *BreakValue, *ContinueValue:
		return v, nil
	}
	return nil, nil
}

func (n *seqNode) Type() core.Type { return "do-seq" }
func (n *seqNode) String() string  { return "(do ...)" }
func (n *seqNode) evalIR(ctx *core.Context) (core.Value, error) {
	var result core.Value = core.Nil
	var err error
	last := len(n.items) - 1
	for i, e := range n.items {
		// Non-final items are statements; the final item's value is the
		// block's value and evaluates normally.
		if i < last {
			control, err := evalStmt(e, ctx)
			if err != nil {
				return nil, err
			}
			if control != nil {
				return control, nil
			}
			continue
		}
		result, err = Eval(e, ctx)
		if err != nil {
			return nil, err
		}
		switch result.(type) {
		case *ReturnValue, *BreakValue, *ContinueValue:
			return result, nil
		}
	}
	return result, nil
}

// execStmt: a block in statement position discards every item's value,
// including the last (its value only mattered as the block's value, which the
// caller is discarding).
func (n *seqNode) execStmt(ctx *core.Context) (core.Value, error) {
	for _, e := range n.items {
		control, err := evalStmt(e, ctx)
		if err != nil {
			return nil, err
		}
		if control != nil {
			return control, nil
		}
	}
	return nil, nil
}

func compileSeq(items []core.Value) core.Value {
	body := make([]core.Value, len(items)-1)
	for i := 1; i < len(items); i++ {
		body[i-1] = compileIR(items[i])
	}
	return &seqNode{items: body}
}

// ifNode is a two- or three-branch if. Mirrors IfForm's simple shapes; elif
// chains lower to a nested if in the else position, so no elif handling is
// needed here. isTruthyWithErrors matches IfForm's truthiness (with __bool__).
type ifNode struct{ cond, then, els core.Value } // els nil => no else branch

func (n *ifNode) Type() core.Type { return "if-node" }
func (n *ifNode) String() string  { return "(if ...)" }
func (n *ifNode) evalIR(ctx *core.Context) (core.Value, error) {
	// A numeric condition short-circuits truthiness without boxing: f != 0
	// matches bool(int)/bool(float) exactly (0/0.0/-0.0 falsy, NaN truthy).
	num, kind, boxed, err := evalNumOf(n.cond, ctx)
	if err != nil {
		return nil, err
	}
	var truthy bool
	if boxed == nil && kind != core.SlotBoxed {
		truthy = num != 0
	} else {
		truthy, err = isTruthyWithErrors(boxed, ctx)
		if err != nil {
			return nil, err
		}
	}
	if truthy {
		return Eval(n.then, ctx)
	}
	if n.els == nil {
		return core.Nil, nil
	}
	return Eval(n.els, ctx)
}

// execStmt: an if in statement position runs the taken branch in statement
// position too — the branch value was only ever the if's value.
func (n *ifNode) execStmt(ctx *core.Context) (core.Value, error) {
	num, kind, boxed, err := evalNumOf(n.cond, ctx)
	if err != nil {
		return nil, err
	}
	var truthy bool
	if boxed == nil && kind != core.SlotBoxed {
		truthy = num != 0
	} else {
		truthy, err = isTruthyWithErrors(boxed, ctx)
		if err != nil {
			return nil, err
		}
	}
	if truthy {
		return evalStmt(n.then, ctx)
	}
	if n.els == nil {
		return nil, nil
	}
	return evalStmt(n.els, ctx)
}

func compileIf(n *core.ListValue, items []core.Value, orig core.Value) core.Value {
	// items: (if cond then [else]). Model only the plain 2/3-branch forms; an
	// else that is itself an (elif ...) list or a bare elif/else symbol is a
	// hand-written chain best left to IfForm.
	switch n.Len() {
	case 3:
		return &ifNode{cond: compileIR(items[1]), then: compileIR(items[2])}
	case 4:
		third := unwrapLocated(items[3])
		if sym, ok := third.(core.SymbolValue); ok && (string(sym) == "elif" || string(sym) == "else") {
			return orig
		}
		if lst, ok := third.(*core.ListValue); ok && lst.Len() > 0 {
			if sym, ok := unwrapLocated(lst.Items()[0]).(core.SymbolValue); ok && string(sym) == "elif" {
				return orig
			}
		}
		return &ifNode{cond: compileIR(items[1]), then: compileIR(items[2]), els: compileIR(items[3])}
	default:
		return orig
	}
}

// returnNode is (return [expr]). Mirrors ReturnForm.
type returnNode struct{ val core.Value } // nil => bare return

func (n *returnNode) Type() core.Type { return "return-node" }
func (n *returnNode) String() string  { return "(return ...)" }
func (n *returnNode) evalIR(ctx *core.Context) (core.Value, error) {
	if n.val == nil {
		return &ReturnValue{Value: core.Nil}, nil
	}
	v, err := Eval(n.val, ctx)
	if err != nil {
		return nil, err
	}
	return &ReturnValue{Value: v}, nil
}

func compileReturn(items []core.Value, orig core.Value) core.Value {
	switch len(items) {
	case 1:
		return &returnNode{}
	case 2:
		return &returnNode{val: compileIR(items[1])}
	default:
		return orig // (return a b ...) is an arity error; let ReturnForm raise it.
	}
}

// assignNode is (= <slot> value). Mirrors the slot fast path in
// assignFormInternal: store into the frame and return the assigned value.
type assignNode struct {
	slot int
	val  core.Value
}

func (n *assignNode) Type() core.Type { return "assign-node" }
func (n *assignNode) String() string  { return "(= ...)" }
func (n *assignNode) evalIR(ctx *core.Context) (core.Value, error) {
	if err := n.execDiscard(ctx); err != nil {
		return nil, err
	}
	// The assignment's value is demanded (expression position): Get memoizes
	// the box into the frame, so this costs one boxing at most.
	return ctx.Locals.Get(n.slot), nil
}

// execStmt: assignment in statement position — never a control sentinel.
func (n *assignNode) execStmt(ctx *core.Context) (core.Value, error) {
	return nil, n.execDiscard(ctx)
}

// execDiscard performs the assignment without materializing the result — the
// statement-position path, where the loop-carried accumulator win lives: an
// unboxed numeric right-hand side stores as raw float64 + tag.
func (n *assignNode) execDiscard(ctx *core.Context) error {
	num, kind, boxed, err := evalNumOf(n.val, ctx)
	if err != nil {
		return err
	}
	if boxed != nil {
		ctx.Locals.Set(n.slot, boxed)
		return nil
	}
	ctx.Locals.SetNum(n.slot, num, kind)
	return nil
}

func compileAssign(items []core.Value, orig core.Value) core.Value {
	// Only the slot-target single assignment (= <slotRef> value) is modeled;
	// resolveBody emits exactly this for a bare local. Anything else (multiple
	// assignment, tuple/attr/index targets) stays on the AssignForm path.
	if len(items) == 3 {
		if sr, ok := unwrapLocated(items[1]).(*slotRef); ok {
			return &assignNode{slot: sr.slot, val: compileIR(items[2])}
		}
	}
	return orig
}

// forNode is the compiled simple for loop: single slot target, one body, no
// else clause (compileFor only accepts that shape). Iteration over a range
// runs as a raw float64 counting loop writing the induction variable unboxed
// (SetNum) — no counter boxing, no iterator protocol. Every other sequence
// falls back to ForForm, passing the ALREADY-EVALUATED sequence wrapped in a
// constNode so the sequence expression's side effects fire exactly once.
type forNode struct {
	target   core.Value // *slotRef or SymbolValue: the fallback pattern
	slot     int        // slot index, or -1 for name binding (module scope)
	bindName string     // set when slot < 0
	bindKey  string     // "s:" + bindName, prebuilt
	bindSync bool       // export-sync decision, precomputed
	seq      core.Value // compiled sequence expression
	body     core.Value // compiled body
}

// bind writes one induction value, matching the generic path exactly:
// slot targets mirror UnpackPattern's slotRef case; name targets mirror its
// SymbolValue case (ctx.Define), which keeps module globals name-addressable
// mid-loop (globals()/imports see every rebind).
func (n *forNode) bind(ctx *core.Context, cur float64) {
	if n.slot >= 0 {
		ctx.Locals.SetNum(n.slot, cur, core.SlotInt)
	} else {
		// Mirrors UnpackPattern's SymbolValue case (ctx.Define), with the
		// ModuleDict key prebuilt.
		ctx.DefineKeyed(n.bindName, n.bindKey, n.bindSync, core.BoxNumber(cur))
	}
}

func (n *forNode) Type() core.Type { return "for-node" }
func (n *forNode) String() string  { return "(for ...)" }
func (n *forNode) evalIR(ctx *core.Context) (core.Value, error) {
	seqVal, err := Eval(n.seq, ctx)
	if err != nil {
		return nil, err
	}
	if rng, ok := seqVal.(*core.RangeValue); ok {
		// Mirrors rangeIterator exactly: float64 counter advanced by Step,
		// yielding int-kind values (range always yields NumberValue). The body
		// runs in statement position: Python for-statements have no value, and
		// the loop's own value (Nil) matches an empty loop; sentinels still
		// propagate. A for in expression position with a non-Nil last body
		// value is unreachable from the Python frontend.
		step := rng.Step
		for cur := rng.Start; (step > 0 && cur < rng.Stop) || (step < 0 && cur > rng.Stop); cur += step {
			n.bind(ctx, cur)
			control, err := evalStmt(n.body, ctx)
			if err != nil {
				return nil, err
			}
			if control != nil {
				switch control.(type) {
				case *BreakValue:
					return core.Nil, nil
				case *ContinueValue:
					continue
				default: // *ReturnValue
					return control, nil
				}
			}
		}
		return core.Nil, nil
	}
	// Generic sequence: reuse ForForm's full driver (iterator protocol, dict
	// keys, strings, ...) on the evaluated value.
	return ForForm(core.NewList(n.target, newConstNode(seqVal), n.body), ctx)
}

// execStmt: a for loop in statement position — evalIR already discards body
// values; only sentinels differ (Return propagates, the loop value is dropped).
func (n *forNode) execStmt(ctx *core.Context) (core.Value, error) {
	v, err := n.evalIR(ctx)
	if err != nil {
		return nil, err
	}
	if _, ok := v.(*ReturnValue); ok {
		return v, nil
	}
	return nil, nil
}

// compileFor lowers a simple (for <slot> seq body) to a forNode. Only the
// canonical single-target, single-body shape compiles; for/else and
// multi-body forms fall through to ForForm unchanged.
func compileFor(items []core.Value, orig core.Value) core.Value {
	// Canonical Python lowering: (for target seq body). The s-expression
	// surface also allows (for target in seq body); both compile.
	switch len(items) {
	case 4:
		sr, ok := unwrapLocated(items[1]).(*slotRef)
		if !ok {
			return orig
		}
		return &forNode{target: sr, slot: sr.slot, seq: compileIR(items[2]), body: compileIR(items[3])}
	case 5:
		sr, ok := unwrapLocated(items[1]).(*slotRef)
		if !ok {
			return orig
		}
		if in, ok := unwrapLocated(items[2]).(core.SymbolValue); !ok || string(in) != "in" {
			return orig
		}
		return &forNode{target: sr, slot: sr.slot, seq: compileIR(items[3]), body: compileIR(items[4])}
	default:
		return orig
	}
}

// whileNode is the compiled simple while loop: condition and single body, no
// else clause (compileWhile only accepts that shape). The condition runs
// through the typed kernel — a numeric condition is f != 0 without boxing —
// and truthiness of boxed values uses core.IsTruthy, exactly as WhileForm
// does (unlike IfForm, WhileForm does not propagate __bool__ errors; that
// difference is preserved).
type whileNode struct {
	cond core.Value
	body core.Value
}

func (n *whileNode) Type() core.Type { return "while-node" }
func (n *whileNode) String() string  { return "(while ...)" }

// test reports one evaluation of the loop condition.
func (n *whileNode) test(ctx *core.Context) (bool, error) {
	num, kind, boxed, err := evalNumOf(n.cond, ctx)
	if err != nil {
		return false, err
	}
	if boxed == nil && kind != core.SlotBoxed {
		return num != 0, nil
	}
	return core.IsTruthy(boxed), nil
}

func (n *whileNode) evalIR(ctx *core.Context) (core.Value, error) {
	var lastResult core.Value = core.Nil
	for {
		truthy, err := n.test(ctx)
		if err != nil {
			return nil, err
		}
		if !truthy {
			return lastResult, nil
		}
		result, err := Eval(n.body, ctx)
		if err != nil {
			return nil, err
		}
		switch result.(type) {
		case *BreakValue:
			return lastResult, nil
		case *ContinueValue:
			continue
		case *ReturnValue:
			return result, nil
		}
		lastResult = result
	}
}

// execStmt: a while in statement position runs its body in statement position
// too — no per-iteration result materialization.
func (n *whileNode) execStmt(ctx *core.Context) (core.Value, error) {
	for {
		truthy, err := n.test(ctx)
		if err != nil {
			return nil, err
		}
		if !truthy {
			return nil, nil
		}
		control, err := evalStmt(n.body, ctx)
		if err != nil {
			return nil, err
		}
		if control != nil {
			switch control.(type) {
			case *BreakValue:
				return nil, nil
			case *ContinueValue:
				continue
			default: // *ReturnValue
				return control, nil
			}
		}
	}
}

// compileWhile lowers a simple (while cond body) to a whileNode. while/else
// and multi-body forms fall through to WhileForm unchanged.
func compileWhile(items []core.Value, orig core.Value) core.Value {
	if len(items) != 3 {
		return orig
	}
	return &whileNode{cond: compileIR(items[1]), body: compileIR(items[2])}
}

// --- call nodes ---

// callNode is a positional function call (resolveBody guarantees no keyword or
// *-unpack arguments reach a slot body). It mirrors the positional tail of
// evalFunctionCallWithKeywords: evaluate callee and args, then dispatch.
type callNode struct {
	callee core.Value
	args   []core.Value
	name   string // head symbol name for tracebacks ("" if computed callee)
}

func (n *callNode) Type() core.Type { return "call-node" }
func (n *callNode) String() string  { return "(call ...)" }
func (n *callNode) evalIR(ctx *core.Context) (core.Value, error) {
	fn, err := Eval(n.callee, ctx)
	if err != nil {
		return nil, err
	}
	argv := make([]core.Value, len(n.args))
	for i, a := range n.args {
		v, err := Eval(a, ctx)
		if err != nil {
			return nil, err
		}
		argv[i] = v
	}
	return callValue(fn, argv, n.name, ctx)
}

func compileCall(items []core.Value, name string) core.Value {
	args := make([]core.Value, len(items)-1)
	for i := 1; i < len(items); i++ {
		args[i-1] = compileIR(items[i])
	}
	return &callNode{callee: compileIR(items[0]), args: args, name: name}
}

// dotNode is a pre-parsed attribute access / method call. The handler args
// list is built once at compile time — the object expression and method
// arguments compiled to IR — and shared across evaluations: DotForm and its
// helpers only ever read the list (audited: no mutation), and the dispatcher's
// per-eval NewList copy plus the generic list-eval preamble (special-form map
// lookup, macro/decorator checks) are exactly the overhead this node removes.
// DotForm itself runs unchanged, so descriptor, property, classmethod/
// staticmethod, super, and auto-call semantics are untouched.
type dotNode struct{ args *core.ListValue }

func (n *dotNode) Type() core.Type { return "dot-node" }
func (n *dotNode) String() string  { return "(. ...)" }
func (n *dotNode) evalIR(ctx *core.Context) (core.Value, error) {
	return DotForm(n.args, ctx)
}

// compileDot builds a dotNode from (. obj attr args...). The attr name and
// any StringValue argument stay raw: DotForm type-asserts core.StringValue
// structurally (the attr itself, and the "__call__" no-arg-call marker), so
// wrapping them in constNode would break that detection — and raw strings
// self-evaluate in Eval's first switch case anyway.
func compileDot(items []core.Value, orig core.Value) core.Value {
	if len(items) < 3 {
		return orig // malformed; let DotForm raise its own arity error
	}
	out := make([]core.Value, len(items)-1)
	out[0] = compileIR(items[1]) // object expression
	out[1] = items[2]            // attr name: structural, never compiled
	for i := 3; i < len(items); i++ {
		if _, isStr := unwrapLocated(items[i]).(core.StringValue); isStr {
			out[i-1] = items[i]
			continue
		}
		out[i-1] = compileIR(items[i])
	}
	return &dotNode{args: core.NewList(out...)}
}

// compileCallChecked guards compileCall against keyword/unpack markers, which
// callNode's positional evaluation would misread as name lookups. resolveBody
// bails on such calls, so this is defense-in-depth for any path handing
// compileIR an unrewritten tree.
func compileCallChecked(items []core.Value, name string, orig core.Value) core.Value {
	if callHasKeywordsOrUnpack(items) {
		return orig
	}
	return compileCall(items, name)
}

// operatorNode is a binary (or unary/n-ary) operator call with a cached registry
// callable and an inline native-number fast path for the common binary case.
type operatorNode struct {
	op   string
	fn   core.Value // cached registry operator
	args []core.Value
}

func (n *operatorNode) Type() core.Type { return "op-node" }
func (n *operatorNode) String() string  { return "(" + n.op + " ...)" }
func (n *operatorNode) evalIR(ctx *core.Context) (core.Value, error) {
	num, kind, boxed, err := n.evalNum(ctx)
	if err != nil {
		return nil, err
	}
	if boxed != nil {
		return boxed, nil
	}
	return boxNum(num, kind), nil
}

// evalNum is the operator's typed path: binary arithmetic and comparisons on
// unboxed operands, exactly-once evaluation. Int op Int keeps int semantics
// (PromoteIntOverflow for +,-,*); any float operand switches to float
// semantics (raw IEEE — overflow goes to inf, NaN compares false), matching
// the NumberValue/FloatValue fast paths this replaces. Everything else —
// non-numeric operands, unmodeled operators, n-ary shapes — evaluates each
// argument once, boxes as needed, and dispatches to the cached registry
// operator, byte-identical to the previous evalIR.
func (n *operatorNode) evalNum(ctx *core.Context) (float64, uint8, core.Value, error) {
	if len(n.args) == 2 {
		lnum, lkind, lbox, err := evalNumOf(n.args[0], ctx)
		if err != nil {
			return 0, 0, nil, err
		}
		rnum, rkind, rbox, err := evalNumOf(n.args[1], ctx)
		if err != nil {
			return 0, 0, nil, err
		}
		if lbox == nil && rbox == nil {
			// Both operands numeric and unboxed.
			if lkind == core.SlotInt && rkind == core.SlotInt {
				switch n.op {
				case "+":
					sum := lnum + rnum
					if p, ok := core.PromoteIntOverflow("+", lnum, rnum, sum); ok {
						return 0, core.SlotBoxed, p, nil
					}
					return sum, core.SlotInt, nil, nil
				case "-":
					diff := lnum - rnum
					if p, ok := core.PromoteIntOverflow("-", lnum, rnum, diff); ok {
						return 0, core.SlotBoxed, p, nil
					}
					return diff, core.SlotInt, nil, nil
				case "*":
					prod := lnum * rnum
					if p, ok := core.PromoteIntOverflow("*", lnum, rnum, prod); ok {
						return 0, core.SlotBoxed, p, nil
					}
					return prod, core.SlotInt, nil, nil
				}
			} else {
				// At least one float operand: float semantics, no promotion.
				switch n.op {
				case "+":
					return lnum + rnum, core.SlotFloat, nil, nil
				case "-":
					return lnum - rnum, core.SlotFloat, nil, nil
				case "*":
					return lnum * rnum, core.SlotFloat, nil, nil
				}
			}
			// Comparisons share IEEE semantics across int/float kinds
			// (1 == 1.0, NaN compares false). Booleans box statically.
			switch n.op {
			case "<":
				return 0, core.SlotBoxed, core.BoolValue(lnum < rnum), nil
			case "<=":
				return 0, core.SlotBoxed, core.BoolValue(lnum <= rnum), nil
			case ">":
				return 0, core.SlotBoxed, core.BoolValue(lnum > rnum), nil
			case ">=":
				return 0, core.SlotBoxed, core.BoolValue(lnum >= rnum), nil
			case "==":
				return 0, core.SlotBoxed, core.BoolValue(lnum == rnum), nil
			case "!=":
				return 0, core.SlotBoxed, core.BoolValue(lnum != rnum), nil
			}
		}
		// Mixed or non-numeric: box what needs boxing (each side was evaluated
		// exactly once above) and dispatch to the registry operator.
		if lbox == nil {
			lbox = boxNum(lnum, lkind)
		}
		if rbox == nil {
			rbox = boxNum(rnum, rkind)
		}
		v, err := callValue(n.fn, []core.Value{lbox, rbox}, n.op, ctx)
		if err != nil {
			return 0, 0, nil, err
		}
		return 0, core.SlotBoxed, v, nil
	}
	argv := make([]core.Value, len(n.args))
	for i, a := range n.args {
		v, err := Eval(a, ctx)
		if err != nil {
			return 0, 0, nil, err
		}
		argv[i] = v
	}
	v, err := callValue(n.fn, argv, n.op, ctx)
	if err != nil {
		return 0, 0, nil, err
	}
	return 0, core.SlotBoxed, v, nil
}

func compileOperator(op string, fn core.Value, items []core.Value) core.Value {
	args := make([]core.Value, len(items)-1)
	for i := 1; i < len(items); i++ {
		args[i-1] = compileIR(items[i])
	}
	return &operatorNode{op: op, fn: fn, args: args}
}

// fastNumOp computes a binary operator on two native numbers, returning ok=false
// for operators without an inline path (handled by the cached callable). The +,
// -, and * cases mirror addTwo/subtractTwo/multiplyTwo exactly, including the
// integer-overflow promotion to big.Int.
func fastNumOp(op string, l, r core.NumberValue) (core.Value, bool) {
	switch op {
	case "+":
		sum := float64(l) + float64(r)
		if p, ok := core.PromoteIntOverflow("+", float64(l), float64(r), sum); ok {
			return p, true
		}
		return core.BoxNumber(sum), true
	case "-":
		diff := float64(l) - float64(r)
		if p, ok := core.PromoteIntOverflow("-", float64(l), float64(r), diff); ok {
			return p, true
		}
		return core.BoxNumber(diff), true
	case "*":
		prod := float64(l) * float64(r)
		if p, ok := core.PromoteIntOverflow("*", float64(l), float64(r), prod); ok {
			return p, true
		}
		return core.BoxNumber(prod), true
	case "<":
		return core.BoolValue(l < r), true
	case "<=":
		return core.BoolValue(l <= r), true
	case ">":
		return core.BoolValue(l > r), true
	case ">=":
		return core.BoolValue(l >= r), true
	case "==":
		return core.BoolValue(l == r), true
	case "!=":
		return core.BoolValue(l != r), true
	}
	return nil, false
}

// callValue dispatches a fully-evaluated positional call, mirroring the tail of
// evalFunctionCallWithKeywords (stack frame, kwargs-capable vs plain callable vs
// __call__, and the error-preservation rules) so tracebacks and exception
// matching are unchanged.
func callValue(fn core.Value, args []core.Value, name string, ctx *core.Context) (core.Value, error) {
	file, line, col := "", 0, 0
	if loc := ctx.CurrentLocation(); loc != nil {
		file, line, col = loc.File, loc.Line, loc.Column
	}

	if kwargsFunc, ok := fn.(interface {
		CallWithKeywords([]core.Value, map[string]core.Value, *core.Context) (core.Value, error)
	}); ok {
		funcName := name
		if funcName == "" {
			funcName = "<anonymous>"
		}
		ctx.PushStack(funcName, file, line, col)
		defer ctx.PopStack()

		result, err := kwargsFunc.CallWithKeywords(args, nil, ctx)
		if err != nil {
			if _, ok := err.(*core.PythonError); ok {
				return nil, err
			}
			if _, ok := err.(*Exception); ok {
				return nil, err
			}
			if _, ok := err.(*protocols.StopIteration); ok {
				return nil, err
			}
			if _, ok := err.(*core.StopIteration); ok {
				return nil, err
			}
			return nil, core.WrapEvalError(err, fmt.Sprintf("error in %s: %v", funcName, err), ctx)
		}
		return result, nil
	}

	callable, ok := fn.(core.Callable)
	if !ok {
		if result, found, err := types.CallCall(fn, args, ctx); found {
			return result, err
		}
		return nil, &core.TypeError{Message: fmt.Sprintf("'%s' object is not callable", core.GetPythonTypeName(fn))}
	}

	funcName := name
	if funcName == "" {
		funcName = introspectCallName(fn)
	}
	ctx.PushStack(funcName, file, line, col)
	defer ctx.PopStack()

	result, err := callable.Call(args, ctx)
	if err != nil {
		if _, ok := err.(*core.PythonError); ok {
			return nil, err
		}
		if _, ok := err.(*Exception); ok {
			return nil, err
		}
		if _, isEvalError := err.(*core.EvalError); isEvalError {
			return nil, err
		}
		if funcName == "assert" {
			return nil, core.WrapEvalError(err, err.Error(), ctx)
		}
		return nil, core.WrapEvalError(err, fmt.Sprintf("error in %s: %v", funcName, err), ctx)
	}
	return result, nil
}

// introspectCallName derives a traceback name for a callee with no head symbol,
// matching evalFunctionCallWithKeywords' fallback introspection.
func introspectCallName(fn core.Value) string {
	switch f := fn.(type) {
	case core.SymbolValue:
		return string(f)
	case *core.BuiltinFunction:
		if nameVal, ok := f.GetAttr("__name__"); ok {
			if nameStr, ok := nameVal.(core.StringValue); ok {
				return string(nameStr)
			}
		}
		return "<builtin>"
	case *core.BoundMethod:
		return fmt.Sprintf("%s.%s", f.TypeDesc.PythonName, f.Method.Name)
	case *UserFunction:
		if f.name != "" {
			return f.name
		}
		return "<anonymous>"
	default:
		return "<anonymous>"
	}
}

// --- module-scope compilation (the module tier, follow-on to M28-9pm) ---
//
// Module-level code cannot use slots: globals must stay name-addressable
// during execution (a function called mid-loop reads and writes them through
// globals(), imports see the live dict). But the statement SKELETON — loops,
// branches, assignments, expressions — can still compile to the same IR,
// with reads through symbolNode (ctx.Lookup) and writes through the exact
// calls the generic handlers make (assignVariable / ctx.Define). Measured
// before this existed: the same loop ran 39x slower at module level than in
// a function.
//
// Compilation is per statement with per-construct fallback: anything not
// modeled here (def, class, import, try, ...) evaluates generically,
// unchanged.

// globalAssignNode is (= name value) at module scope. The value expression
// runs through the typed kernel; the store goes through assignVariable —
// byte-identical to assignFormInternal's bare-symbol path (global/nonlocal
// declarations, ModuleDict sync, builtin shadowing all live there).
type globalAssignNode struct {
	name       string
	key        string // "s:" + name, prebuilt (no per-write concat)
	syncModule bool   // Define's export-skip decision, precomputed
	val        core.Value
}

func moduleSyncable(name string) bool {
	if len(name) >= 2 && name[:2] == "__" && name[len(name)-2:] == "__" {
		return false
	}
	return len(name) == 0 || name[0] != '_'
}

func (n *globalAssignNode) Type() core.Type { return "global-assign-node" }
func (n *globalAssignNode) String() string  { return "(= " + n.name + " ...)" }
func (n *globalAssignNode) evalIR(ctx *core.Context) (core.Value, error) {
	num, kind, boxed, err := evalNumOf(n.val, ctx)
	if err != nil {
		return nil, err
	}
	if boxed == nil {
		boxed = boxNum(num, kind)
	}
	// Declared global/nonlocal names (rare at module scope) keep the exact
	// generic path; everything else takes the keyed Define fast path, which
	// is Define minus the per-write key concatenation.
	if ctx.IsGlobal(n.name) || ctx.IsNonlocal(n.name) {
		if err := assignVariable(ctx, n.name, boxed); err != nil {
			return nil, err
		}
		return boxed, nil
	}
	ctx.DefineKeyed(n.name, n.key, n.syncModule, boxed)
	return boxed, nil
}

// execStmt: the dict store needs the boxed value anyway; only the return
// materialization is skipped.
func (n *globalAssignNode) execStmt(ctx *core.Context) (core.Value, error) {
	_, err := n.evalIR(ctx)
	return nil, err
}

// EvalModuleStatement evaluates one top-level statement, compiling its
// statement skeleton to IR first. Drop-in replacement for Eval at module
// statement loops (file execution, module import).
func EvalModuleStatement(stmt core.Value, ctx *core.Context) (core.Value, error) {
	return Eval(compileModuleIR(stmt), ctx)
}

// compileModuleIR compiles a module-scope statement. Statement-bearing forms
// (do, if, for, while, =) recurse in module mode; everything else delegates
// to the expression compiler, whose unmodeled-form guard keeps special forms
// on their handlers.
func compileModuleIR(v core.Value) core.Value {
	switch n := v.(type) {
	case core.LocatedValue:
		inner := compileModuleIR(n.Value)
		if sameValue(inner, n.Value) {
			return v
		}
		return core.LocatedValue{Value: inner, Location: n.Location}

	case *core.ListValue:
		if n.Len() == 0 {
			return compileIR(v)
		}
		items := n.ItemsRef()
		head, ok := unwrapLocated(items[0]).(core.SymbolValue)
		if !ok {
			return compileIR(v)
		}
		switch string(head) {
		case "do", "begin":
			body := make([]core.Value, n.Len()-1)
			for i := 1; i < n.Len(); i++ {
				body[i-1] = compileModuleIR(items[i])
			}
			return &seqNode{items: body}
		case "if":
			return compileModuleIf(n, items, v)
		case "=":
			// Only the bare (= name value) shape; chained, tuple, attribute,
			// and subscript targets keep the generic handler.
			if n.Len() == 3 {
				if sym, ok := unwrapLocated(items[1]).(core.SymbolValue); ok {
					name := string(sym)
					return &globalAssignNode{name: name, key: "s:" + name, syncModule: moduleSyncable(name), val: compileIR(items[2])}
				}
			}
			return v
		case "for":
			return compileModuleFor(items, v)
		case "while":
			if n.Len() != 3 {
				return v
			}
			return &whileNode{cond: compileIR(items[1]), body: compileModuleIR(items[2])}
		default:
			return compileIR(v)
		}

	default:
		return compileIR(v)
	}
}

// compileModuleIf mirrors compileIf's shape checks with module-mode branches.
func compileModuleIf(n *core.ListValue, items []core.Value, orig core.Value) core.Value {
	switch n.Len() {
	case 3:
		return &ifNode{cond: compileIR(items[1]), then: compileModuleIR(items[2])}
	case 4:
		third := unwrapLocated(items[3])
		if sym, ok := third.(core.SymbolValue); ok && (string(sym) == "elif" || string(sym) == "else") {
			return orig
		}
		if lst, ok := third.(*core.ListValue); ok && lst.Len() > 0 {
			if sym, ok := unwrapLocated(lst.ItemsRef()[0]).(core.SymbolValue); ok && string(sym) == "elif" {
				return orig
			}
		}
		return &ifNode{cond: compileIR(items[1]), then: compileModuleIR(items[2]), els: compileModuleIR(items[3])}
	default:
		return orig
	}
}

// compileModuleFor lowers a module-scope for with a bare-symbol target to a
// name-binding forNode. Tuple targets and other shapes keep ForForm.
func compileModuleFor(items []core.Value, orig core.Value) core.Value {
	switch len(items) {
	case 4:
		sym, ok := unwrapLocated(items[1]).(core.SymbolValue)
		if !ok {
			return orig
		}
		name := string(sym)
		return &forNode{target: items[1], slot: -1, bindName: name, bindKey: "s:" + name, bindSync: moduleSyncable(name), seq: compileIR(items[2]), body: compileModuleIR(items[3])}
	case 5:
		sym, ok := unwrapLocated(items[1]).(core.SymbolValue)
		if !ok {
			return orig
		}
		if in, ok := unwrapLocated(items[2]).(core.SymbolValue); !ok || string(in) != "in" {
			return orig
		}
		name := string(sym)
		return &forNode{target: items[1], slot: -1, bindName: name, bindKey: "s:" + name, bindSync: moduleSyncable(name), seq: compileIR(items[3]), body: compileModuleIR(items[4])}
	default:
		return orig
	}
}
