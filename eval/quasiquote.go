package eval

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// quasiquoteForm implements the quasiquote special form
// Syntax: (quasiquote expr)
// Like quote, but allows selective evaluation via unquote and unquote-splicing
func quasiquoteForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() != 1 {
		return nil, fmt.Errorf("quasiquote requires exactly 1 argument")
	}

	return expandQuasiquote(args.Items()[0], ctx)
}

// expandQuasiquote recursively expands a quasiquoted expression
func expandQuasiquote(expr core.Value, ctx *core.Context) (core.Value, error) {
	switch v := expr.(type) {
	case *core.ListValue:
		// Check if it's an unquote or unquote-splicing form
		if v.Len() > 0 {
			if sym, ok := v.Items()[0].(core.SymbolValue); ok {
				switch string(sym) {
				case "unquote":
					// (unquote expr) - evaluate the expression
					if v.Len() != 2 {
						return nil, fmt.Errorf("unquote requires exactly 1 argument")
					}
					return Eval(v.Items()[1], ctx)

				case "unquote-splicing":
					// unquote-splicing can only appear inside a list context
					// at the top level, it's an error
					return nil, fmt.Errorf("unquote-splicing not inside list")
				}
			}
		}

		// Regular list - recursively expand elements
		result := make([]core.Value, 0, v.Len())
		for _, elem := range v.Items() {
			// Check if this element is unquote-splicing
			if list, ok := elem.(*core.ListValue); ok && list.Len() > 0 {
				if sym, ok := list.Items()[0].(core.SymbolValue); ok && string(sym) == "unquote-splicing" {
					// (unquote-splicing expr) - evaluate and splice
					if list.Len() != 2 {
						return nil, fmt.Errorf("unquote-splicing requires exactly 1 argument")
					}

					// Evaluate the expression
					val, err := Eval(list.Items()[1], ctx)
					if err != nil {
						return nil, err
					}

					// Convert to list and splice
					spliced, err := valueToListForSplicing(val)
					if err != nil {
						return nil, fmt.Errorf("unquote-splicing: %v", err)
					}

					// Splice the elements
					result = append(result, spliced.Items()...)
					continue
				}
			}

			// Regular element - recursively expand
			expanded, err := expandQuasiquote(elem, ctx)
			if err != nil {
				return nil, err
			}
			result = append(result, expanded)
		}
		return core.NewList(result...), nil

	default:
		// Self-evaluating values (numbers, strings, bools, symbols, nil)
		// Return as-is (like quote)
		return expr, nil
	}
}

// valueToListForSplicing converts a value to a list for splicing
func valueToListForSplicing(val core.Value) (*core.ListValue, error) {
	switch v := val.(type) {
	case *core.ListValue:
		return v, nil
	case core.TupleValue:
		return core.NewList(v...), nil
	case *core.SetValue:
		// Convert set to list
		list := make([]core.Value, 0)
		iter := v.Iterator()
		for {
			item, hasNext := iter.Next()
			if !hasNext {
				break
			}
			list = append(list, item)
		}
		return core.NewList(list...), nil
	default:
		return nil, fmt.Errorf("can only splice sequences, got %s", val.Type())
	}
}

// RegisterQuasiquoteForms registers quasiquote-related special forms
func RegisterQuasiquoteForms() {
	RegisterSpecialForm("quasiquote", quasiquoteForm)
	// Note: unquote and unquote-splicing are handled within quasiquote
	// They're not standalone special forms
}
