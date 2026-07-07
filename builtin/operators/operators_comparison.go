package operators

import (
	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/core/protocols"
)

// compareTuples performs lexicographic comparison of two tuples
// Returns -1 if left < right, 0 if equal, 1 if left > right
func compareTuples(left, right core.TupleValue, ctx *core.Context) (int, error) {
	minLen := len(left)
	if len(right) < minLen {
		minLen = len(right)
	}

	for i := 0; i < minLen; i++ {
		// Compare elements recursively
		cmp, err := compareValues(left[i], right[i], ctx)
		if err != nil {
			return 0, err
		}
		if cmp != 0 {
			return cmp, nil
		}
	}

	// All compared elements are equal, compare by length
	if len(left) < len(right) {
		return -1, nil
	} else if len(left) > len(right) {
		return 1, nil
	}
	return 0, nil
}

// compareValues compares two values and returns -1, 0, or 1
func compareValues(left, right core.Value, ctx *core.Context) (int, error) {
	// Handle tuples recursively
	if leftTuple, ok := left.(core.TupleValue); ok {
		if rightTuple, ok := right.(core.TupleValue); ok {
			return compareTuples(leftTuple, rightTuple, ctx)
		}
		return 0, core.NewComparisonError("<", left, right)
	}

	// Real numbers where a float is involved: compare as float64 (int/float
	// mixes, e.g. 1 < 2.5 or min(1, 2.5)). Pure int/int and big-int cases fall
	// through to the exact-integer paths below.
	if core.IsFloatValue(left) || core.IsFloatValue(right) {
		if lf, lok := core.AsFloat(left); lok {
			if rf, rok := core.AsFloat(right); rok {
				if lf < rf {
					return -1, nil
				} else if lf > rf {
					return 1, nil
				}
				return 0, nil
			}
		}
	}

	// Handle numbers
	if leftNum, ok := left.(core.NumberValue); ok {
		if rightNum, ok := right.(core.NumberValue); ok {
			if float64(leftNum) < float64(rightNum) {
				return -1, nil
			} else if float64(leftNum) > float64(rightNum) {
				return 1, nil
			}
			return 0, nil
		}
		// int vs big int: compare in arbitrary precision.
		if rightBig, ok := right.(core.BigIntValue); ok {
			return core.PromoteToBigInt(leftNum).GetBigInt().Cmp(rightBig.GetBigInt()), nil
		}
		return 0, core.NewComparisonError("<", left, right)
	}

	// Handle strings
	if leftStr, ok := left.(core.StringValue); ok {
		if rightStr, ok := right.(core.StringValue); ok {
			if string(leftStr) < string(rightStr) {
				return -1, nil
			} else if string(leftStr) > string(rightStr) {
				return 1, nil
			}
			return 0, nil
		}
		return 0, core.NewComparisonError("<", left, right)
	}

	// Handle BigInt
	if leftBig, ok := left.(core.BigIntValue); ok {
		if rightBig, ok := right.(core.BigIntValue); ok {
			return leftBig.GetBigInt().Cmp(rightBig.GetBigInt()), nil
		}
		if rightNum, ok := right.(core.NumberValue); ok {
			rightBig := core.PromoteToBigInt(rightNum)
			return leftBig.GetBigInt().Cmp(rightBig.GetBigInt()), nil
		}
		return 0, core.NewComparisonError("<", left, right)
	}

	return 0, core.NewComparisonError("<", left, right)
}

// Comparison Operators

// Equal implements the == operator using protocol-based dispatch
func Equal() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("==", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		return compareEqual(args[0], args[1], ctx)
	}
}

// compareEqual compares two values for equality
func compareEqual(left, right core.Value, ctx *core.Context) (core.Value, error) {
	// tuple-subclass instances (namedtuple etc.) compare like the tuple they
	// inherit from, unless the class defines its own __eq__.
	left = core.UnwrapTupleInstance(left)
	right = core.UnwrapTupleInstance(right)

	// Special handling for Class objects - use the default __eq__ from type, not instance methods
	// This prevents trying to call instance methods like TestCase.__eq__(self, other) on the class itself
	if leftClass, ok := left.(*core.Class); ok {
		// For classes, compare by identity or use the built-in __eq__ from Class.GetAttr
		// which returns a bound BuiltinFunction, not instance methods
		// Just use the fallback equality check
		if rightClass, ok := right.(*core.Class); ok {
			// Both are classes - compare by name
			return core.BoolValue(leftClass.Name == rightClass.Name), nil
		}
		// Left is class, right is not - they're not equal
		return core.BoolValue(false), nil
	}

	// str-subclass instances compare by their backing string value (the
	// inherited str.__eq__ behavior), unless a subclass defines a custom __eq__.
	// Done before the __eq__ dispatch so object's default __eq__ (identity) does
	// not shadow string equality for plain str subclasses.
	if ls, ok := core.StrBacking(left); ok {
		if rs, ok := core.StrBacking(right); ok && !core.HasUserEq(left) && !core.HasUserEq(right) {
			return core.BoolValue(ls == rs), nil
		}
	}

	// Likewise, int/float-subclass instances compare by their numeric __value__,
	// unless a subclass overrides __eq__. Restricted to cases involving an
	// Instance so plain number comparisons keep using the BigInt-aware logic.
	_, lInst := left.(*core.Instance)
	_, rInst := right.(*core.Instance)
	if lInst || rInst {
		if ln, ok := core.NumBacking(left); ok {
			if rn, ok := core.NumBacking(right); ok && !core.HasUserEq(left) && !core.HasUserEq(right) {
				return core.BoolValue(ln == rn), nil
			}
		}
	}

	// Python operator precedence: if right's class is a strict subclass of left's class,
	// try right's method first
	var leftClass, rightClass *core.Class
	if leftInst, ok := left.(*core.Instance); ok {
		leftClass = leftInst.Class
	}
	if rightInst, ok := right.(*core.Instance); ok {
		rightClass = rightInst.Class
	}

	// If right operand's class is a strict subclass of left operand's class,
	// try right operand's __eq__ first
	if leftClass != nil && rightClass != nil && core.IsStrictSubclass(rightClass, leftClass) {
		// Try __eq__ on right operand first (reflected operation)
		if result, found, err := types.CallDunder(right, "__eq__", []core.Value{left}, ctx); found {
			return result, err
		}
		// Then try __eq__ on left operand
		if result, found, err := types.CallDunder(left, "__eq__", []core.Value{right}, ctx); found {
			return result, err
		}
	} else {
		// Normal order: try left first, then right
		// Try __eq__ on left operand
		if result, found, err := types.CallDunder(left, "__eq__", []core.Value{right}, ctx); found {
			return result, err
		}

		// Special handling for Class objects on the right side too
		if _, ok := right.(*core.Class); ok {
			// Right is a class, left is not - they're not equal
			return core.BoolValue(false), nil
		}

		// Try __eq__ on right operand (Python doesn't have __req__)
		if result, found, err := types.CallDunder(right, "__eq__", []core.Value{left}, ctx); found {
			return result, err
		}
	}

	// For lists and list subclasses, use context-aware equality so that:
	// - Mutations during element __eq__ are visible (live comparison, not snapshots)
	// - Reflected __eq__ is tried when the first returns NotImplemented
	_, leftIsListV := left.(*core.ListValue)
	_, leftIsListI := left.(*core.ListInstance)
	_, rightIsListV := right.(*core.ListValue)
	_, rightIsListI := right.(*core.ListInstance)
	if leftIsListV || leftIsListI || rightIsListV || rightIsListI {
		eq, err := core.EqualValuesWithError(left, right, ctx)
		if err != nil {
			return nil, err
		}
		return core.BoolValue(eq), nil
	}

	// For dicts, use context-aware equality to propagate __eq__ exceptions
	if ld, ok := left.(*core.DictValue); ok {
		if rd, ok := right.(*core.DictValue); ok {
			eq, err := core.DictEqualWithError(ld, rd, ctx)
			if err != nil {
				return nil, err
			}
			return core.BoolValue(eq), nil
		}
		return core.BoolValue(false), nil
	}

	// Both operands are instances whose __eq__ were already tried above (both
	// the forward and reflected calls returned NotImplemented, or neither class
	// defines __eq__). Python's final fallback is object identity, NOT another
	// __eq__ call — routing through EqualValues here would redundantly re-invoke
	// __eq__ a third time (it falls back to identity anyway). Compare directly.
	if _, lok := left.(*core.Instance); lok {
		if _, rok := right.(*core.Instance); rok {
			return core.BoolValue(left == right), nil
		}
	}

	// Fall back to built-in equality
	return core.BoolValue(core.EqualValues(left, right)), nil
}

// NotEqual implements the != operator using protocol-based dispatch
func NotEqual() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("!=", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		return compareNotEqual(args[0], args[1], ctx)
	}
}

// compareNotEqual compares two values for inequality
func compareNotEqual(left, right core.Value, ctx *core.Context) (core.Value, error) {
	// Python operator precedence: if right's class is a strict subclass of left's class,
	// try right's method first
	var leftClass, rightClass *core.Class
	if leftInst, ok := left.(*core.Instance); ok {
		leftClass = leftInst.Class
	}
	if rightInst, ok := right.(*core.Instance); ok {
		rightClass = rightInst.Class
	}

	// str/num-subclass instances: != is the negation of value equality, mirroring
	// the == fast paths in compareEqual. Only fires when a subclass instance is
	// involved and neither side overrides __eq__/__ne__, so the __ne__=None
	// blocking idiom and custom __ne__ below are untouched.
	if backedValueNeFastPath(left, right) {
		equal, err := compareEqual(left, right, ctx)
		if err != nil {
			return nil, err
		}
		return core.BoolValue(!core.IsTruthy(equal)), nil
	}

	// Determine try order: if right's class strictly subclasses left's, the
	// reflected __ne__ (on right) is tried first (Python operator precedence).
	firstObj, secondObj := left, right
	if leftClass != nil && rightClass != nil && core.IsStrictSubclass(rightClass, leftClass) {
		firstObj, secondObj = right, left
	}

	// Try __ne__ on each operand in turn. A method returning a concrete
	// (non-NotImplemented) result decides the comparison. CallDunderRaw reports
	// whether a __ne__ was actually present, so we can distinguish "returned
	// NotImplemented" from "no __ne__".
	sawNe := false
	for _, pair := range [][2]core.Value{{firstObj, secondObj}, {secondObj, firstObj}} {
		result, exists, err := types.CallDunderRaw(pair[0], "__ne__", []core.Value{pair[1]}, ctx)
		if err != nil {
			return nil, err
		}
		if !exists {
			continue
		}
		sawNe = true
		if _, isNI := result.(*core.NotImplementedValue); !isNI {
			return result, nil
		}
	}

	if sawNe {
		// A __ne__ was present but returned NotImplemented. object.__ne__ has
		// already consulted __eq__, so the final fallback is identity, matching
		// CPython (a != b is `a is not b`).
		return core.BoolValue(!core.SameObject(left, right)), nil
	}

	// Neither operand provides __ne__: fall back to the negation of equality.
	equal, err := compareEqual(left, right, ctx)
	if err != nil {
		return nil, err
	}
	return core.BoolValue(!core.IsTruthy(equal)), nil
}

// backedValueNeFastPath reports whether left != right should be decided purely by
// negating value equality: a str- or int/float-subclass instance is involved on
// at least one side, both sides reduce to the same backing kind, and neither side
// overrides __eq__ or __ne__.
func backedValueNeFastPath(left, right core.Value) bool {
	_, lInst := left.(*core.Instance)
	_, rInst := right.(*core.Instance)
	if !lInst && !rInst {
		return false
	}
	if core.HasUserEq(left) || core.HasUserEq(right) || core.HasUserNe(left) || core.HasUserNe(right) {
		return false
	}
	if _, ok := core.StrBacking(left); ok {
		if _, ok := core.StrBacking(right); ok {
			return true
		}
	}
	if _, ok := core.NumBacking(left); ok {
		if _, ok := core.NumBacking(right); ok {
			return true
		}
	}
	return false
}

// LessThan implements the < operator using protocol-based dispatch
func LessThan() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("<", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		if a, ok := args[0].(core.NumberValue); ok {
			if b, ok := args[1].(core.NumberValue); ok {
				return core.BoolValue(a < b), nil
			}
		}
		return compareLessThan(args[0], args[1], ctx)
	}
}

// compareLessThan compares if left < right
// tryOrderedComparison applies Python's rich-comparison operand ordering for an
// ordered comparison: leftMethod is the operator's dunder on the left operand
// (e.g. "__le__") and rightMethod is its reflection on the right (e.g. "__ge__").
// Normally the left operand's method is tried first, then the right's reflected
// method. But when the right operand's class is a strict (real, MRO-based)
// subclass of the left operand's class, the reflected method on the right is
// tried FIRST (PEP 207 subclass precedence) — so e.g. B() <= C(), with C a
// subclass of B, calls C.__ge__ before B.__le__. Virtual/ABC-registered
// subclasses are not real subtypes and keep normal order. Returns (result,
// handled, err); handled is false only when neither dunder produced a usable
// (non-NotImplemented) result, so the caller can continue to other fallbacks.
func tryOrderedComparison(left, right core.Value, leftMethod, rightMethod string, ctx *core.Context) (core.Value, bool, error) {
	var leftClass, rightClass *core.Class
	if li, ok := left.(*core.Instance); ok {
		leftClass = li.Class
	}
	if ri, ok := right.(*core.Instance); ok {
		rightClass = ri.Class
	}

	if leftClass != nil && rightClass != nil && core.IsStrictSubclass(rightClass, leftClass) {
		// Reflected method on the subclass (right) first, then the left's method.
		if result, found, err := types.CallDunder(right, rightMethod, []core.Value{left}, ctx); found {
			return result, true, err
		}
		if result, found, err := types.CallDunder(left, leftMethod, []core.Value{right}, ctx); found {
			return result, true, err
		}
		return nil, false, nil
	}

	// Normal order: left's method first, then the right's reflected method.
	if result, found, err := types.CallDunder(left, leftMethod, []core.Value{right}, ctx); found {
		return result, true, err
	}
	if result, found, err := types.CallDunder(right, rightMethod, []core.Value{left}, ctx); found {
		return result, true, err
	}
	return nil, false, nil
}

func compareLessThan(left, right core.Value, ctx *core.Context) (core.Value, error) {
	left = core.UnwrapTupleInstance(left)
	right = core.UnwrapTupleInstance(right)
	// Try __lt__ on the left / reflected __gt__ on the right, honoring subclass
	// precedence (right's method first when right is a strict subclass of left).
	if result, handled, err := tryOrderedComparison(left, right, "__lt__", "__gt__", ctx); handled {
		return result, err
	}

	// Use protocol-based comparison
	if leftCmp, ok := protocols.AsComparable(left); ok {
		cmp, err := leftCmp.Compare(right)
		if err == nil {
			return core.BoolValue(cmp < 0), nil
		}
	}

	// Handle BigInt comparison
	leftBig, leftIsBig := left.(core.BigIntValue)
	rightBig, rightIsBig := right.(core.BigIntValue)

	if leftIsBig || rightIsBig {
		// Convert both to BigInt for comparison
		if !leftIsBig {
			if leftNum, ok := left.(core.NumberValue); ok {
				if !core.IsInteger(float64(leftNum)) {
					return nil, core.NewComparisonError("<", left, right)
				}
				leftBig = core.PromoteToBigInt(leftNum)
			} else {
				return nil, core.NewComparisonError("<", left, right)
			}
		}
		if !rightIsBig {
			if rightNum, ok := right.(core.NumberValue); ok {
				if !core.IsInteger(float64(rightNum)) {
					return nil, core.NewComparisonError("<", left, right)
				}
				rightBig = core.PromoteToBigInt(rightNum)
			} else {
				return nil, core.NewComparisonError("<", left, right)
			}
		}

		return core.BoolValue(leftBig.GetBigInt().Cmp(rightBig.GetBigInt()) < 0), nil
	}

	// str and str-subclass instances order by their backing string value
	// (reached when no __lt__/__gt__ resolved the comparison above).
	if ls, ok := core.StrBacking(left); ok {
		if rs, ok2 := core.StrBacking(right); ok2 {
			return core.BoolValue(string(ls) < string(rs)), nil
		}
	}

	// Fall back to type-based comparison
	return types.Switch(left).
		Number(func(leftNum float64) (core.Value, error) {
			return types.Switch(right).
				Number(func(rightNum float64) (core.Value, error) {
					return core.BoolValue(leftNum < rightNum), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, core.NewComparisonError("<", left, right)
				}).
				Execute()
		}).
		String(func(leftStr string) (core.Value, error) {
			return types.Switch(right).
				String(func(rightStr string) (core.Value, error) {
					return core.BoolValue(leftStr < rightStr), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, core.NewComparisonError("<", left, right)
				}).
				Execute()
		}).
		Tuple(func(leftTuple core.TupleValue) (core.Value, error) {
			return types.Switch(right).
				Tuple(func(rightTuple core.TupleValue) (core.Value, error) {
					cmp, err := compareTuples(leftTuple, rightTuple, ctx)
					if err != nil {
						return nil, err
					}
					return core.BoolValue(cmp < 0), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, core.NewComparisonError("<", left, right)
				}).
				Execute()
		}).
		Default(func(l core.Value) (core.Value, error) {
			return nil, core.NewComparisonError("<", left, right)
		}).
		Execute()
}

// LessThanOrEqual implements the <= operator using protocol-based dispatch
func LessThanOrEqual() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("<=", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		if a, ok := args[0].(core.NumberValue); ok {
			if b, ok := args[1].(core.NumberValue); ok {
				return core.BoolValue(a <= b), nil
			}
		}
		return compareLessThanOrEqual(args[0], args[1], ctx)
	}
}

// compareLessThanOrEqual compares if left <= right
func compareLessThanOrEqual(left, right core.Value, ctx *core.Context) (core.Value, error) {
	left = core.UnwrapTupleInstance(left)
	right = core.UnwrapTupleInstance(right)
	// Try __le__ on the left / reflected __ge__ on the right, honoring subclass
	// precedence (right's method first when right is a strict subclass of left).
	if result, handled, err := tryOrderedComparison(left, right, "__le__", "__ge__", ctx); handled {
		return result, err
	}

	// Use protocol-based comparison
	if leftCmp, ok := protocols.AsComparable(left); ok {
		cmp, err := leftCmp.Compare(right)
		if err == nil {
			return core.BoolValue(cmp <= 0), nil
		}
	}

	// str and str-subclass instances order by their backing string value.
	if ls, ok := core.StrBacking(left); ok {
		if rs, ok2 := core.StrBacking(right); ok2 {
			return core.BoolValue(string(ls) <= string(rs)), nil
		}
	}

	// Fall back to type-based comparison
	return types.Switch(left).
		Number(func(leftNum float64) (core.Value, error) {
			return types.Switch(right).
				Number(func(rightNum float64) (core.Value, error) {
					return core.BoolValue(leftNum <= rightNum), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, core.NewComparisonError("<=", left, right)
				}).
				Execute()
		}).
		String(func(leftStr string) (core.Value, error) {
			return types.Switch(right).
				String(func(rightStr string) (core.Value, error) {
					return core.BoolValue(leftStr <= rightStr), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, core.NewComparisonError("<=", left, right)
				}).
				Execute()
		}).
		Tuple(func(leftTuple core.TupleValue) (core.Value, error) {
			return types.Switch(right).
				Tuple(func(rightTuple core.TupleValue) (core.Value, error) {
					cmp, err := compareTuples(leftTuple, rightTuple, ctx)
					if err != nil {
						return nil, err
					}
					return core.BoolValue(cmp <= 0), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, core.NewComparisonError("<=", left, right)
				}).
				Execute()
		}).
		Default(func(l core.Value) (core.Value, error) {
			return nil, core.NewComparisonError("<=", left, right)
		}).
		Execute()
}

// GreaterThan implements the > operator using protocol-based dispatch
func GreaterThan() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs(">", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		if a, ok := args[0].(core.NumberValue); ok {
			if b, ok := args[1].(core.NumberValue); ok {
				return core.BoolValue(a > b), nil
			}
		}
		return compareGreaterThan(args[0], args[1], ctx)
	}
}

// compareGreaterThan compares if left > right
func compareGreaterThan(left, right core.Value, ctx *core.Context) (core.Value, error) {
	left = core.UnwrapTupleInstance(left)
	right = core.UnwrapTupleInstance(right)
	// Try __gt__ on the left / reflected __lt__ on the right, honoring subclass
	// precedence (right's method first when right is a strict subclass of left).
	if result, handled, err := tryOrderedComparison(left, right, "__gt__", "__lt__", ctx); handled {
		return result, err
	}

	// Use protocol-based comparison
	if leftCmp, ok := protocols.AsComparable(left); ok {
		cmp, err := leftCmp.Compare(right)
		if err == nil {
			return core.BoolValue(cmp > 0), nil
		}
	}

	// Handle BigInt comparison
	leftBig, leftIsBig := left.(core.BigIntValue)
	rightBig, rightIsBig := right.(core.BigIntValue)

	if leftIsBig || rightIsBig {
		// Convert both to BigInt for comparison
		if !leftIsBig {
			if leftNum, ok := left.(core.NumberValue); ok {
				if !core.IsInteger(float64(leftNum)) {
					return nil, core.NewComparisonError(">", left, right)
				}
				leftBig = core.PromoteToBigInt(leftNum)
			} else {
				return nil, core.NewComparisonError(">", left, right)
			}
		}
		if !rightIsBig {
			if rightNum, ok := right.(core.NumberValue); ok {
				if !core.IsInteger(float64(rightNum)) {
					return nil, core.NewComparisonError(">", left, right)
				}
				rightBig = core.PromoteToBigInt(rightNum)
			} else {
				return nil, core.NewComparisonError(">", left, right)
			}
		}

		return core.BoolValue(leftBig.GetBigInt().Cmp(rightBig.GetBigInt()) > 0), nil
	}

	// str and str-subclass instances order by their backing string value.
	if ls, ok := core.StrBacking(left); ok {
		if rs, ok2 := core.StrBacking(right); ok2 {
			return core.BoolValue(string(ls) > string(rs)), nil
		}
	}

	// Fall back to type-based comparison
	return types.Switch(left).
		Number(func(leftNum float64) (core.Value, error) {
			return types.Switch(right).
				Number(func(rightNum float64) (core.Value, error) {
					return core.BoolValue(leftNum > rightNum), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, core.NewComparisonError(">", left, right)
				}).
				Execute()
		}).
		String(func(leftStr string) (core.Value, error) {
			return types.Switch(right).
				String(func(rightStr string) (core.Value, error) {
					return core.BoolValue(leftStr > rightStr), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, core.NewComparisonError(">", left, right)
				}).
				Execute()
		}).
		Tuple(func(leftTuple core.TupleValue) (core.Value, error) {
			return types.Switch(right).
				Tuple(func(rightTuple core.TupleValue) (core.Value, error) {
					cmp, err := compareTuples(leftTuple, rightTuple, ctx)
					if err != nil {
						return nil, err
					}
					return core.BoolValue(cmp > 0), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, core.NewComparisonError(">", left, right)
				}).
				Execute()
		}).
		Default(func(l core.Value) (core.Value, error) {
			return nil, core.NewComparisonError(">", left, right)
		}).
		Execute()
}

// GreaterThanOrEqual implements the >= operator using protocol-based dispatch
func GreaterThanOrEqual() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs(">=", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		if a, ok := args[0].(core.NumberValue); ok {
			if b, ok := args[1].(core.NumberValue); ok {
				return core.BoolValue(a >= b), nil
			}
		}
		return compareGreaterThanOrEqual(args[0], args[1], ctx)
	}
}

// compareGreaterThanOrEqual compares if left >= right
func compareGreaterThanOrEqual(left, right core.Value, ctx *core.Context) (core.Value, error) {
	left = core.UnwrapTupleInstance(left)
	right = core.UnwrapTupleInstance(right)
	// Try __ge__ on the left / reflected __le__ on the right, honoring subclass
	// precedence (right's method first when right is a strict subclass of left).
	if result, handled, err := tryOrderedComparison(left, right, "__ge__", "__le__", ctx); handled {
		return result, err
	}

	// Use protocol-based comparison
	if leftCmp, ok := protocols.AsComparable(left); ok {
		cmp, err := leftCmp.Compare(right)
		if err == nil {
			return core.BoolValue(cmp >= 0), nil
		}
	}

	// str and str-subclass instances order by their backing string value.
	if ls, ok := core.StrBacking(left); ok {
		if rs, ok2 := core.StrBacking(right); ok2 {
			return core.BoolValue(string(ls) >= string(rs)), nil
		}
	}

	// Fall back to type-based comparison
	return types.Switch(left).
		Number(func(leftNum float64) (core.Value, error) {
			return types.Switch(right).
				Number(func(rightNum float64) (core.Value, error) {
					return core.BoolValue(leftNum >= rightNum), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, core.NewComparisonError(">=", left, right)
				}).
				Execute()
		}).
		String(func(leftStr string) (core.Value, error) {
			return types.Switch(right).
				String(func(rightStr string) (core.Value, error) {
					return core.BoolValue(leftStr >= rightStr), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, core.NewComparisonError(">=", left, right)
				}).
				Execute()
		}).
		Tuple(func(leftTuple core.TupleValue) (core.Value, error) {
			return types.Switch(right).
				Tuple(func(rightTuple core.TupleValue) (core.Value, error) {
					cmp, err := compareTuples(leftTuple, rightTuple, ctx)
					if err != nil {
						return nil, err
					}
					return core.BoolValue(cmp >= 0), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, core.NewComparisonError(">=", left, right)
				}).
				Execute()
		}).
		Default(func(l core.Value) (core.Value, error) {
			return nil, core.NewComparisonError(">=", left, right)
		}).
		Execute()
}
