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
		if inner == n.Value {
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
		return &constNode{v: v}

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
		return compileCall(items, "")
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
	default:
		// Comprehension forms pass analysis (closed sub-scopes) but must run
		// through their special-form handlers — which have their own compiled
		// fast path (compileCompExpr) — not as a function call.
		if compHeads[h] {
			return orig
		}
		// A non-special-form head: an operator or function call. analyzeLocals
		// guarantees no unmodeled special form reaches here.
		if fastOpNames[h] {
			if fn, ok := core.GetOperatorFunc(h); ok {
				return compileOperator(h, fn, items)
			}
		}
		return compileCall(items, h)
	}
}

// --- leaf nodes ---

// constNode is a self-evaluating literal captured at compile time.
type constNode struct{ v core.Value }

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
	val := ctx.Locals[s.slot]
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

func (n *seqNode) Type() core.Type { return "do-seq" }
func (n *seqNode) String() string  { return "(do ...)" }
func (n *seqNode) evalIR(ctx *core.Context) (core.Value, error) {
	var result core.Value = core.Nil
	var err error
	for _, e := range n.items {
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
	c, err := Eval(n.cond, ctx)
	if err != nil {
		return nil, err
	}
	truthy, err := isTruthyWithErrors(c, ctx)
	if err != nil {
		return nil, err
	}
	if truthy {
		return Eval(n.then, ctx)
	}
	if n.els == nil {
		return core.Nil, nil
	}
	return Eval(n.els, ctx)
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
	v, err := Eval(n.val, ctx)
	if err != nil {
		return nil, err
	}
	ctx.Locals[n.slot] = v
	return v, nil
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

// compileFor rebuilds a simple (for <slot> seq body) so ForForm still drives
// iteration (its iterator protocol, unpacking, and flow control are reused),
// while the sequence and body run as compiled IR. Only the canonical
// single-target, single-body shape is rewritten; for/else and multi-body forms
// fall through to ForForm unchanged.
func compileFor(items []core.Value, orig core.Value) core.Value {
	if len(items) != 4 {
		return orig
	}
	if _, ok := unwrapLocated(items[1]).(*slotRef); !ok {
		return orig
	}
	return core.NewList(items[0], items[1], compileIR(items[2]), compileIR(items[3]))
}

// compileWhile rebuilds a simple (while cond body) so WhileForm still drives the
// loop while the condition and body run as compiled IR. while/else and
// multi-body forms fall through to WhileForm unchanged.
func compileWhile(items []core.Value, orig core.Value) core.Value {
	if len(items) != 3 {
		return orig
	}
	return core.NewList(items[0], compileIR(items[1]), compileIR(items[2]))
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
	if len(n.args) == 2 {
		l, err := Eval(n.args[0], ctx)
		if err != nil {
			return nil, err
		}
		r, err := Eval(n.args[1], ctx)
		if err != nil {
			return nil, err
		}
		if ln, ok := l.(core.NumberValue); ok {
			if rn, ok := r.(core.NumberValue); ok {
				if res, ok := fastNumOp(n.op, ln, rn); ok {
					return res, nil
				}
			}
		}
		return callValue(n.fn, []core.Value{l, r}, n.op, ctx)
	}
	argv := make([]core.Value, len(n.args))
	for i, a := range n.args {
		v, err := Eval(a, ctx)
		if err != nil {
			return nil, err
		}
		argv[i] = v
	}
	return callValue(n.fn, argv, n.op, ctx)
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
		return core.NumberValue(sum), true
	case "-":
		diff := float64(l) - float64(r)
		if p, ok := core.PromoteIntOverflow("-", float64(l), float64(r), diff); ok {
			return p, true
		}
		return core.NumberValue(diff), true
	case "*":
		prod := float64(l) * float64(r)
		if p, ok := core.PromoteIntOverflow("*", float64(l), float64(r), prod); ok {
			return p, true
		}
		return core.NumberValue(prod), true
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
