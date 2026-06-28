// Package vmspike is a THROWAWAY spike, not part of M28's runtime. It is a
// minimal stack-based bytecode VM that executes ONE hand-assembled function --
// the body of the scope_lookup benchmark:
//
//	a=1; b=2; c=3; d=4; e=5; total=0
//	for i in range(N):
//	    total = total + a + b + c + d + e + i
//	return total
//
// Its only purpose is to measure how much faster a bytecode + array-slot
// execution model is than M28's tree-walking evaluator, to decide whether a
// real bytecode VM is worth building. There is no general compiler here; the
// bytecode is assembled by hand for this single function.
//
// Two execution variants quantify the two separable wins:
//   - RunBoxed   keeps M28's core.NumberValue (and the same overflow-to-bigint
//     check addTwo uses), so it isolates the execution-model win alone:
//     bytecode + slot-indexed locals vs. tree-walk + map-based scope lookup.
//   - RunUnboxed uses raw float64 locals/stack, showing the additional ceiling
//     available from a specialized (unboxed) value representation.
package vmspike

import "github.com/mmichie/m28/core"

// Op is a VM opcode.
type Op uint8

const (
	OpLoadImm        Op = iota // Arg = small int literal; push it
	OpLoadFast                 // Arg = local slot; push locals[Arg]
	OpStoreFast                // Arg = local slot; locals[Arg] = pop()
	OpAdd                      // b=pop(); a=pop(); push(a+b)
	OpLessThan                 // b=pop(); a=pop(); push(a<b)
	OpPopJumpIfFalse           // cond=pop(); if !cond { ip = Arg }
	OpJump                     // ip = Arg
	OpReturn                   // return pop()
)

// Instr is a decoded instruction (struct-of-arrays would be marginally faster,
// but this keeps the spike readable; the dispatch switch is what we measure).
type Instr struct {
	Op  Op
	Arg int32
}

// Local variable slots for the scope_lookup body.
const (
	slotA = iota
	slotB
	slotC
	slotD
	slotE
	slotTotal
	slotI
	slotN
	numSlots
)

// scopeLookupCode hand-assembles the bytecode, resolving jump targets from the
// actual instruction layout so there are no fragile hand-counted offsets.
func scopeLookupCode() []Instr {
	var c []Instr
	emit := func(op Op, arg int32) int { c = append(c, Instr{op, arg}); return len(c) - 1 }

	// a=1; b=2; c=3; d=4; e=5; total=0; i=0
	emit(OpLoadImm, 1)
	emit(OpStoreFast, slotA)
	emit(OpLoadImm, 2)
	emit(OpStoreFast, slotB)
	emit(OpLoadImm, 3)
	emit(OpStoreFast, slotC)
	emit(OpLoadImm, 4)
	emit(OpStoreFast, slotD)
	emit(OpLoadImm, 5)
	emit(OpStoreFast, slotE)
	emit(OpLoadImm, 0)
	emit(OpStoreFast, slotTotal)
	emit(OpLoadImm, 0)
	emit(OpStoreFast, slotI)

	loopStart := len(c)
	// while i < N:
	emit(OpLoadFast, slotI)
	emit(OpLoadFast, slotN)
	emit(OpLessThan, 0)
	jEnd := emit(OpPopJumpIfFalse, 0) // target backpatched to end

	// total = total + a + b + c + d + e + i
	emit(OpLoadFast, slotTotal)
	emit(OpLoadFast, slotA)
	emit(OpAdd, 0)
	emit(OpLoadFast, slotB)
	emit(OpAdd, 0)
	emit(OpLoadFast, slotC)
	emit(OpAdd, 0)
	emit(OpLoadFast, slotD)
	emit(OpAdd, 0)
	emit(OpLoadFast, slotE)
	emit(OpAdd, 0)
	emit(OpLoadFast, slotI)
	emit(OpAdd, 0)
	emit(OpStoreFast, slotTotal)

	// i = i + 1
	emit(OpLoadFast, slotI)
	emit(OpLoadImm, 1)
	emit(OpAdd, 0)
	emit(OpStoreFast, slotI)

	emit(OpJump, int32(loopStart))

	end := len(c)
	emit(OpLoadFast, slotTotal)
	emit(OpReturn, 0)

	c[jEnd].Arg = int32(end)
	return c
}

// RunBoxed executes the bytecode using M28's core.Value representation, with
// the same NumberValue + PromoteIntOverflow arithmetic addTwo uses.
func RunBoxed(code []Instr, n int) core.Value {
	locals := make([]core.Value, numSlots)
	locals[slotN] = core.NumberValue(float64(n))
	stack := make([]core.Value, 0, 8)
	ip := 0
	for {
		in := code[ip]
		ip++
		switch in.Op {
		case OpLoadImm:
			stack = append(stack, core.NumberValue(float64(in.Arg)))
		case OpLoadFast:
			stack = append(stack, locals[in.Arg])
		case OpStoreFast:
			locals[in.Arg] = stack[len(stack)-1]
			stack = stack[:len(stack)-1]
		case OpAdd:
			m := len(stack)
			a := float64(stack[m-2].(core.NumberValue))
			b := float64(stack[m-1].(core.NumberValue))
			sum := a + b
			if p, ok := core.PromoteIntOverflow("+", a, b, sum); ok {
				stack[m-2] = p
			} else {
				stack[m-2] = core.NumberValue(sum)
			}
			stack = stack[:m-1]
		case OpLessThan:
			m := len(stack)
			a := float64(stack[m-2].(core.NumberValue))
			b := float64(stack[m-1].(core.NumberValue))
			stack[m-2] = core.BoolValue(a < b)
			stack = stack[:m-1]
		case OpPopJumpIfFalse:
			m := len(stack)
			cond := bool(stack[m-1].(core.BoolValue))
			stack = stack[:m-1]
			if !cond {
				ip = int(in.Arg)
			}
		case OpJump:
			ip = int(in.Arg)
		case OpReturn:
			return stack[len(stack)-1]
		}
	}
}

// RunUnboxed executes the same bytecode on a raw float64 stack/locals (no
// boxing, no interface assertions, no overflow check) -- the ceiling a
// specialized value representation would unlock.
func RunUnboxed(code []Instr, n int) float64 {
	var locals [numSlots]float64
	locals[slotN] = float64(n)
	stack := make([]float64, 0, 8)
	ip := 0
	for {
		in := code[ip]
		ip++
		switch in.Op {
		case OpLoadImm:
			stack = append(stack, float64(in.Arg))
		case OpLoadFast:
			stack = append(stack, locals[in.Arg])
		case OpStoreFast:
			locals[in.Arg] = stack[len(stack)-1]
			stack = stack[:len(stack)-1]
		case OpAdd:
			m := len(stack)
			stack[m-2] += stack[m-1]
			stack = stack[:m-1]
		case OpLessThan:
			m := len(stack)
			if stack[m-2] < stack[m-1] {
				stack[m-2] = 1
			} else {
				stack[m-2] = 0
			}
			stack = stack[:m-1]
		case OpPopJumpIfFalse:
			m := len(stack)
			cond := stack[m-1] != 0
			stack = stack[:m-1]
			if !cond {
				ip = int(in.Arg)
			}
		case OpJump:
			ip = int(in.Arg)
		case OpReturn:
			return stack[len(stack)-1]
		}
	}
}
