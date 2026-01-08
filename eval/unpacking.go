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

		// Check length match
		if len(values) != actualPattern.Len() {
			if len(values) < actualPattern.Len() {
				return fmt.Errorf("not enough values to unpack (expected %d, got %d)", actualPattern.Len(), len(values))
			}
			return fmt.Errorf("too many values to unpack (expected %d, got %d)", actualPattern.Len(), len(values))
		}

		// Recursively unpack each sub-pattern with its corresponding value
		for i, subPattern := range actualPattern.Items() {
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
