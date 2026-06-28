package vmspike

import (
	"github.com/mmichie/m28/builtin/operators"
	"github.com/mmichie/m28/core"
)

// This file is part of the same throwaway spike as vm.go. It models the OTHER
// option: keeping a tree-walking interpreter but giving it a clean "resolution"
// layer -- a typed, pre-resolved IR with slot-indexed locals -- instead of the
// current untyped core.Value walk with map-based scope lookup and string-based
// special-form dispatch.
//
// Each node evaluates against a flat []core.Value slot array (locals), so there
// is no Context map traversal and no per-node structural re-sniffing. Dispatch
// is a Go interface call per node (a vtable), which is what a clean AST
// interpreter looks like. It is still a recursive tree-walk, NOT bytecode --
// so comparing it against the current evaluator isolates the "resolution +
// fast locals" win, and comparing it against the bytecode VM isolates the
// additional win of a flat dispatch loop.

// rnode is a resolved IR node.
type rnode interface {
	eval(loc []core.Value) core.Value
}

type rConst struct{ v core.Value }

func (n rConst) eval(loc []core.Value) core.Value { return n.v }

type rLoad struct{ slot int }

func (n rLoad) eval(loc []core.Value) core.Value { return loc[n.slot] }

type rStore struct {
	slot int
	e    rnode
}

func (n rStore) eval(loc []core.Value) core.Value {
	v := n.e.eval(loc)
	loc[n.slot] = v
	return v
}

type rLt struct{ l, r rnode }

func (n rLt) eval(loc []core.Value) core.Value {
	a := float64(n.l.eval(loc).(core.NumberValue))
	b := float64(n.r.eval(loc).(core.NumberValue))
	return core.BoolValue(a < b)
}

type rWhile struct {
	cond rnode
	body rnode
}

func (n rWhile) eval(loc []core.Value) core.Value {
	for bool(n.cond.eval(loc).(core.BoolValue)) {
		n.body.eval(loc)
	}
	return core.Nil
}

type rSeq struct{ stmts []rnode }

func (n rSeq) eval(loc []core.Value) core.Value {
	var last core.Value = core.Nil
	for _, s := range n.stmts {
		last = s.eval(loc)
	}
	return last
}

// rAddFast inlines NumberValue addition with the same overflow-to-bigint check
// addTwo uses -- i.e. resolution PLUS operator specialization.
type rAddFast struct{ l, r rnode }

func (n rAddFast) eval(loc []core.Value) core.Value {
	a := float64(n.l.eval(loc).(core.NumberValue))
	b := float64(n.r.eval(loc).(core.NumberValue))
	sum := a + b
	if p, ok := core.PromoteIntOverflow("+", a, b, sum); ok {
		return p
	}
	return core.NumberValue(sum)
}

// rAddDispatch routes addition through M28's real operator machinery
// (operators.Add -> addTwo), so it keeps full operator-overloading semantics.
// It isolates the pure resolution/fast-locals win (same arithmetic path as the
// current evaluator, just without the map lookups and structural dispatch).
type rAddDispatch struct {
	l, r rnode
	add  func(a, b core.Value) core.Value
}

func (n rAddDispatch) eval(loc []core.Value) core.Value {
	return n.add(n.l.eval(loc), n.r.eval(loc))
}

// makeDispatchAdd builds a binary-add closure over M28's real + operator.
func makeDispatchAdd(ctx *core.Context) func(a, b core.Value) core.Value {
	add := operators.Add()
	return func(a, b core.Value) core.Value {
		v, err := add([]core.Value{a, b}, ctx)
		if err != nil {
			panic(err)
		}
		return v
	}
}

// buildResolved assembles the resolved tree for the scope_lookup body, using
// mkAdd to construct each addition (fast-inlined or dispatched).
func buildResolved(mkAdd func(l, r rnode) rnode) rnode {
	// total = total + a + b + c + d + e + i   (left-associative)
	sum := mkAdd(rLoad{slotTotal}, rLoad{slotA})
	sum = mkAdd(sum, rLoad{slotB})
	sum = mkAdd(sum, rLoad{slotC})
	sum = mkAdd(sum, rLoad{slotD})
	sum = mkAdd(sum, rLoad{slotE})
	sum = mkAdd(sum, rLoad{slotI})

	body := rSeq{stmts: []rnode{
		rStore{slotTotal, sum},
		rStore{slotI, mkAdd(rLoad{slotI}, rConst{core.NumberValue(1)})},
	}}

	return rSeq{stmts: []rnode{
		rStore{slotA, rConst{core.NumberValue(1)}},
		rStore{slotB, rConst{core.NumberValue(2)}},
		rStore{slotC, rConst{core.NumberValue(3)}},
		rStore{slotD, rConst{core.NumberValue(4)}},
		rStore{slotE, rConst{core.NumberValue(5)}},
		rStore{slotTotal, rConst{core.NumberValue(0)}},
		rStore{slotI, rConst{core.NumberValue(0)}},
		rWhile{cond: rLt{rLoad{slotI}, rLoad{slotN}}, body: body},
		rLoad{slotTotal},
	}}
}

// runResolved executes a resolved tree with a fresh slot array.
func runResolved(tree rnode, n int) core.Value {
	loc := make([]core.Value, numSlots)
	loc[slotN] = core.NumberValue(float64(n))
	return tree.eval(loc)
}
