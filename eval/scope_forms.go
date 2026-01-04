package eval

import (
	"fmt"

	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/core"
)

// GlobalForm implements the 'global' special form
// Syntax: (global name1 name2 ...)
// Marks the given names as global in the current scope
func GlobalForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() < 1 {
		return nil, &core.TypeError{Message: "global requires at least one variable name"}
	}

	// Mark each name as global
	for _, arg := range args.Items() {
		argVal := unwrapLocated(arg)
		sym, ok := argVal.(core.SymbolValue)
		if !ok {
			return nil, &core.TypeError{Message: fmt.Sprintf("global requires symbol arguments, got %s", argVal.Type())}
		}

		// Mark this variable as global in the current scope
		ctx.DeclareGlobal(string(sym))
	}

	return core.Nil, nil
}

// NonlocalForm implements the 'nonlocal' special form
// Syntax: (nonlocal name1 name2 ...)
// Marks the given names as nonlocal in the current scope
func NonlocalForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() < 1 {
		return nil, &core.TypeError{Message: "nonlocal requires at least one variable name"}
	}

	// Mark each name as nonlocal
	for _, arg := range args.Items() {
		argVal := unwrapLocated(arg)
		sym, ok := argVal.(core.SymbolValue)
		if !ok {
			return nil, &core.TypeError{Message: fmt.Sprintf("nonlocal requires symbol arguments, got %s", argVal.Type())}
		}

		// Mark this variable as nonlocal in the current scope
		// This will error if the variable doesn't exist in an enclosing scope
		if err := ctx.DeclareNonlocal(string(sym)); err != nil {
			return nil, err
		}
	}

	return core.Nil, nil
}

// DelForm implements the 'del' special form
// Syntax: (del target1 target2 ...)
// Deletes variables, attributes, or items
func DelForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() < 1 {
		return nil, &core.TypeError{Message: "del requires at least one target"}
	}

	// Process each deletion target
	for _, target := range args.Items() {
		if err := deleteTarget(target, ctx); err != nil {
			return nil, err
		}
	}

	return core.Nil, nil
}

// resolveSlice converts a SliceValue to start/stop/step indices for deletion
func resolveSlice(slice *core.SliceValue, length int) (start, stop, step int, err error) {
	// Convert slice values to integers
	var startPtr, stopPtr, stepPtr *int

	if slice.Start != nil && slice.Start != core.Nil {
		val, e := types.ToIndex(slice.Start, nil)
		if e != nil {
			return 0, 0, 0, e
		}
		startPtr = &val
	}

	if slice.Stop != nil && slice.Stop != core.Nil {
		val, e := types.ToIndex(slice.Stop, nil)
		if e != nil {
			return 0, 0, 0, e
		}
		stopPtr = &val
	}

	if slice.Step != nil && slice.Step != core.Nil {
		val, e := types.ToIndex(slice.Step, nil)
		if e != nil {
			return 0, 0, 0, e
		}
		if val == 0 {
			return 0, 0, 0, &core.ValueError{Message: "slice step cannot be zero"}
		}
		stepPtr = &val
	}

	// Use the same normalization logic as slicing (from indexing.go)
	start, stop, step = normalizeSliceIndices(length, startPtr, stopPtr, stepPtr)
	return start, stop, step, nil
}

// deleteTarget handles deletion of a single target
func deleteTarget(target core.Value, ctx *core.Context) error {
	target = unwrapLocated(target)
	switch t := target.(type) {
	case core.SymbolValue:
		// Delete variable from appropriate scope
		// del x
		name := string(t)

		// Before deleting, look up the value and call __del__ if it's an instance
		if val, err := ctx.Lookup(name); err == nil {
			if inst, ok := val.(*core.Instance); ok {
				// Check if the instance has a __del__ method
				if delMethod, ok := inst.Class.GetMethod("__del__"); ok {
					// Call __del__ with self as the only argument
					if callable, ok := delMethod.(interface {
						Call([]core.Value, *core.Context) (core.Value, error)
					}); ok {
						_, _ = callable.Call([]core.Value{inst}, ctx)
						// Ignore errors from __del__ as per Python semantics
					}
				}
			}
		}

		// Check if variable is declared as global
		if ctx.IsGlobal(name) {
			if err := ctx.Global.Delete(name); err != nil {
				return fmt.Errorf("cannot delete global name '%s': %w", name, err)
			}
			return nil
		}

		// Check if variable is declared as nonlocal
		if ctx.IsNonlocal(name) {
			// Find and delete from enclosing scope
			current := ctx.Outer
			for current != nil {
				if _, ok := current.Vars[name]; ok {
					if err := current.Delete(name); err != nil {
						return fmt.Errorf("cannot delete nonlocal name '%s': %w", name, err)
					}
					return nil
				}
				current = current.Outer
			}
			return &core.NameError{Name: name}
		}

		// Delete from current scope
		if err := ctx.Delete(name); err != nil {
			return &core.NameError{Name: name}
		}
		return nil

	case *core.ListValue:
		if t.Len() == 0 {
			return &core.ValueError{Message: "cannot delete empty list expression"}
		}

		// Check the first element to determine the type of deletion
		first := unwrapLocated(t.Items()[0])

		// Handle dot notation: (. obj attr)
		if sym, ok := first.(core.SymbolValue); ok && string(sym) == "." {
			if t.Len() != 3 {
				return &core.TypeError{Message: "dot notation requires exactly 2 arguments for deletion"}
			}

			// Evaluate the object
			obj, err := Eval(t.Items()[1], ctx)
			if err != nil {
				return fmt.Errorf("error evaluating object for attribute deletion: %w", err)
			}

			// Get attribute name
			attrNameVal := t.Items()[2]
			// Unwrap LocatedValue if present
			if located, ok := attrNameVal.(core.LocatedValue); ok {
				attrNameVal = located.Unwrap()
			}
			attrName, ok := attrNameVal.(core.StringValue)
			if !ok {
				return &core.TypeError{Message: fmt.Sprintf("attribute name must be a string, got %s", attrNameVal.Type())}
			}

			// Try __delattr__ dunder method first
			if objWithDelAttr, ok := obj.(interface {
				GetAttr(string) (core.Value, bool)
			}); ok {
				if method, exists := objWithDelAttr.GetAttr("__delattr__"); exists {
					// Call the method
					if callable, ok := method.(core.Callable); ok {
						_, err := callable.Call([]core.Value{core.StringValue(attrName)}, ctx)
						if err != nil {
							return fmt.Errorf("error calling __delattr__: %w", err)
						}
						return nil
					}
				}
			}

			// Try direct attribute deletion
			if objWithDelAttr, ok := obj.(interface{ DelAttr(string) error }); ok {
				if err := objWithDelAttr.DelAttr(string(attrName)); err != nil {
					return &core.AttributeError{ObjType: string(obj.Type()), AttrName: string(attrName), Message: fmt.Sprintf("cannot delete attribute '%s': %v", attrName, err)}
				}
				return nil
			}

			return &core.AttributeError{ObjType: string(obj.Type()), Message: fmt.Sprintf("'%s' object does not support attribute deletion", obj.Type())}
		}

		// Handle indexing: (get-item obj key)
		if sym, ok := first.(core.SymbolValue); ok && string(sym) == "get-item" {
			if t.Len() != 3 {
				return &core.TypeError{Message: "get-item requires exactly 2 arguments for deletion"}
			}

			// Evaluate the object
			obj, err := Eval(t.Items()[1], ctx)
			if err != nil {
				return fmt.Errorf("error evaluating object for item deletion: %w", err)
			}

			// Evaluate the key
			key, err := Eval(t.Items()[2], ctx)
			if err != nil {
				return fmt.Errorf("error evaluating key for item deletion: %w", err)
			}

			// Try __delitem__ dunder method first
			if found, err := types.CallDelItem(obj, key, ctx); found {
				if err != nil {
					return fmt.Errorf("error deleting item: %w", err)
				}
				return nil
			}

			// Try dict deletion
			if dict, ok := obj.(*core.DictValue); ok {
				if !dict.DeleteValue(key) {
					return &core.KeyError{Key: key}
				}
				return nil
			}

			// Try list deletion
			if list, ok := obj.(*core.ListValue); ok {
				// Handle slice deletion: del list[start:stop:step]
				if slice, ok := key.(*core.SliceValue); ok {
					// Calculate slice bounds
					length := list.Len()
					start, stop, step, err := resolveSlice(slice, length)
					if err != nil {
						return fmt.Errorf("error resolving slice: %w", err)
					}

					// Create set of indices to delete
					indicesToDelete := make(map[int]bool)
					if step > 0 {
						for i := start; i < stop; i += step {
							indicesToDelete[i] = true
						}
					} else {
						for i := start; i > stop; i += step {
							indicesToDelete[i] = true
						}
					}

					// Create new list without deleted elements
					newItems := make([]core.Value, 0, length-len(indicesToDelete))
					for i, item := range list.Items() {
						if !indicesToDelete[i] {
							newItems = append(newItems, item)
						}
					}

					// Replace list items
					*list = *core.NewList(newItems...)
					return nil
				}

				// Handle single index deletion: del list[i]
				idx, ok := key.(core.NumberValue)
				if !ok {
					return &core.TypeError{Message: fmt.Sprintf("list indices must be integers or slices, not %s", key.Type())}
				}
				intIdx := int(idx)
				if intIdx < 0 {
					intIdx = list.Len() + intIdx
				}
				if intIdx < 0 || intIdx >= list.Len() {
					return &core.IndexError{Index: intIdx, Length: list.Len()}
				}
				// Delete by creating new list without the element
				newItems := make([]core.Value, 0, list.Len()-1)
				for i, item := range list.Items() {
					if i != intIdx {
						newItems = append(newItems, item)
					}
				}
				// Replace list items
				*list = *core.NewList(newItems...)
				return nil
			}

			return &core.TypeError{Message: fmt.Sprintf("'%s' object does not support item deletion", obj.Type())}
		}

		return &core.ValueError{Message: fmt.Sprintf("cannot delete complex expression: %v", target)}

	default:
		return &core.TypeError{Message: fmt.Sprintf("cannot delete %s", target.Type())}
	}
}
