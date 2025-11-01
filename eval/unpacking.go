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
	core.DebugLog("[UNPACK] pattern=%T(%v), value=%T(%v)\n", pattern, pattern, value, value)

	switch p := pattern.(type) {
	case core.SymbolValue:
		// Simple binding: just assign value to variable
		core.DebugLog("[UNPACK] Binding %s = %v\n", string(p), value)
		ctx.Define(string(p), value)
		return nil

	case *core.ListValue:
		// Tuple/list unpacking: value must be a sequence
		// Extract sequence elements from value
		values, err := extractSequenceValues(value)
		if err != nil {
			return err
		}

		// Check length match
		core.DebugLog("[UNPACK] Length check: pattern.Len()=%d, values.Len()=%d\n", p.Len(), len(values))
		if len(values) != p.Len() {
			if len(values) < p.Len() {
				return fmt.Errorf("[UNPACK] not enough values to unpack (expected %d, got %d)", p.Len(), len(values))
			}
			return fmt.Errorf("[UNPACK] too many values to unpack (expected %d, got %d)", p.Len(), len(values))
		}

		// Recursively unpack each sub-pattern with its corresponding value
		for i, subPattern := range p.Items() {
			if err := UnpackPattern(subPattern, values[i], ctx); err != nil {
				return err
			}
		}
		return nil

	default:
		return fmt.Errorf("invalid unpacking pattern: %T (expected symbol or tuple)", pattern)
	}
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
		if iteratorObj, ok := value.(interface{ Iterator() core.Iterator }); ok {
			iter := iteratorObj.Iterator()
			values := []core.Value{}
			for {
				val, done := iter.Next()
				if done {
					break
				}
				values = append(values, val)
			}
			return values, nil
		}

		return nil, fmt.Errorf("cannot unpack non-sequence type '%s'", value.Type())
	}
}
