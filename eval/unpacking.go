// unpacking.go contains generic tuple/sequence unpacking logic
// used by assignments, for-loops, and other unpacking contexts
package eval

import (
	"fmt"
	"github.com/mmichie/m28/core"
)

// UnpackPattern recursively unpacks a value according to a pattern and binds variables to context.
// This is the generic unpacking implementation used by:
// - Assignment statements (a, b = ...)
// - For loops (for x, y in ...)
// - Function parameters (future)
//
// pattern can be:
//   - core.SymbolValue: simple variable binding
//   - *core.ListValue: tuple/list unpacking (may contain nested patterns)
//
// value: the value to unpack (must be iterable if pattern is a list)
// ctx: the context to bind variables in
func UnpackPattern(pattern core.Value, value core.Value, ctx *core.Context) error {
	// Unwrap LocatedValue wrappers from pattern to get the actual pattern structure
	pattern = unwrapLocated(pattern)

	switch p := pattern.(type) {
	case core.SymbolValue:
		// Simple binding: just assign value to variable
		ctx.Define(string(p), value)
		return nil

	case *core.ListValue:
		// A `.` (attribute) or `get-item` (subscript) S-expr is a SINGLE
		// assignable target, not a tuple pattern — e.g. `with cm as obj.attr:`
		// or `for lst[0] in ...:`. Assign the value to it instead of trying to
		// unpack the value as a sequence.
		if p.Len() > 0 {
			if head, ok := unwrapLocated(p.Items()[0]).(core.SymbolValue); ok {
				switch string(head) {
				case ".", "get-item":
					return assignComplexTarget(p, value, ctx)
				}
			}
		}

		// Check if this is a tuple-literal or list-literal form: (tuple-literal elem1 elem2 ...) or (list-literal elem1 elem2 ...)
		// If so, skip the literal marker and use the rest as the pattern
		actualPattern := p
		if p.Len() > 0 {
			firstItem := unwrapLocated(p.Items()[0])
			if sym, ok := firstItem.(core.SymbolValue); ok {
				if string(sym) == "tuple-literal" || string(sym) == "list-literal" {
					// Skip the tuple-literal or list-literal marker
					actualPattern = core.NewList(p.Items()[1:]...)
				}
			}
		}

		// Tuple/list unpacking: value must be a sequence
		// Extract sequence elements from value
		values, err := extractSequenceValues(value)
		if err != nil {
			return err
		}

		// Look for a star pattern (`*name`) — at most one is allowed per
		// PEP 3132. The star target collects everything not consumed by
		// the fixed patterns before and after it (as a list).
		patternItems := actualPattern.Items()
		starIdx := -1
		for i, sub := range patternItems {
			if sym, ok := unwrapLocated(sub).(core.SymbolValue); ok {
				if len(sym) > 0 && sym[0] == '*' {
					if starIdx != -1 {
						return fmt.Errorf("multiple starred expressions in assignment")
					}
					starIdx = i
				}
			}
		}

		if starIdx == -1 {
			// No star — exact length match required
			if len(values) != len(patternItems) {
				if len(values) < len(patternItems) {
					return fmt.Errorf("not enough values to unpack (expected %d, got %d)", len(patternItems), len(values))
				}
				return fmt.Errorf("too many values to unpack (expected %d, got %d)", len(patternItems), len(values))
			}
			for i, subPattern := range patternItems {
				if err := UnpackPattern(subPattern, values[i], ctx); err != nil {
					return err
				}
			}
			return nil
		}

		// Star present: minimum required values = patternItems - 1 (all fixed targets)
		fixedCount := len(patternItems) - 1
		if len(values) < fixedCount {
			return fmt.Errorf("not enough values to unpack (expected at least %d, got %d)", fixedCount, len(values))
		}
		// Bind fixed targets before the star
		for i := 0; i < starIdx; i++ {
			if err := UnpackPattern(patternItems[i], values[i], ctx); err != nil {
				return err
			}
		}
		// Bind the star target to the collected middle portion
		starName := string(unwrapLocated(patternItems[starIdx]).(core.SymbolValue))[1:]
		afterCount := len(patternItems) - starIdx - 1
		middleEnd := len(values) - afterCount
		middle := make([]core.Value, middleEnd-starIdx)
		copy(middle, values[starIdx:middleEnd])
		ctx.Define(starName, core.NewList(middle...))
		// Bind fixed targets after the star
		for j := 0; j < afterCount; j++ {
			if err := UnpackPattern(patternItems[starIdx+1+j], values[middleEnd+j], ctx); err != nil {
				return err
			}
		}
		return nil

	default:
		return fmt.Errorf("invalid unpacking pattern: %T (expected symbol or tuple)", pattern)
	}
}

// assignComplexTarget assigns an already-evaluated value to a single attribute
// (`(. obj attr)`) or subscript (`(get-item obj idx)`) target. Mirrors the
// corresponding cases in assignForm, but for a value that is already evaluated
// (as in `with cm as obj.attr:` / `for lst[0] in ...`).
func assignComplexTarget(target *core.ListValue, value core.Value, ctx *core.Context) error {
	head, _ := unwrapLocated(target.Items()[0]).(core.SymbolValue)
	switch string(head) {
	case "get-item":
		if target.Len() != 3 {
			return core.NewValueError("invalid index expression")
		}
		_, err := SetItemForm(core.NewList(target.Items()[1], target.Items()[2], value), ctx)
		return err
	case ".":
		if target.Len() != 3 {
			return core.NewValueError("invalid dot notation in assignment target")
		}
		obj, err := Eval(target.Items()[1], ctx)
		if err != nil {
			return err
		}
		attrNameVal := unwrapLocated(target.Items()[2])
		var attrName string
		switch n := attrNameVal.(type) {
		case core.StringValue:
			attrName = string(n)
		case core.SymbolValue:
			attrName = string(n)
		default:
			return &core.TypeError{Message: fmt.Sprintf("attribute name must be a string or symbol, got %T", attrNameVal)}
		}
		if dict, ok := obj.(*core.DictValue); ok {
			dict.Set(attrName, value)
			return nil
		}
		if objWithAttrs, ok := obj.(interface {
			SetAttr(string, core.Value) error
		}); ok {
			return objWithAttrs.SetAttr(attrName, value)
		}
		return &core.AttributeError{ObjType: string(obj.Type()), Message: fmt.Sprintf("'%s' does not support attribute assignment", obj.Type())}
	}
	return fmt.Errorf("invalid assignment target")
}

// extractSequenceValues extracts a slice of values from a sequence type
// Supports: lists, tuples, and any iterable object
func extractSequenceValues(value core.Value) ([]core.Value, error) {
	switch v := value.(type) {
	case *core.ListValue:
		return v.Items(), nil

	case core.TupleValue:
		return v, nil

	case core.StringValue:
		// Strings are iterable - each character becomes a string
		runes := []rune(string(v))
		values := make([]core.Value, len(runes))
		for i, r := range runes {
			values[i] = core.StringValue(string(r))
		}
		return values, nil

	default:
		// Try to get an iterator
		if iteratorObj, hasIter := value.(interface{ Iterator() core.Iterator }); hasIter {
			iter := iteratorObj.Iterator()
			values := []core.Value{}
			for {
				val, ok := iter.Next()
				if !ok {
					break
				}
				values = append(values, val)
			}
			return values, nil
		}

		return nil, fmt.Errorf("cannot unpack non-sequence type '%s'", value.Type())
	}
}
