// File contains loop-related special forms for the M28 language
package eval

import (
	"fmt"
	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/core/protocols"
	"strings"
)

// isStopIteration checks if an error is a StopIteration exception
// Handles both protocols.StopIteration and errors with "StopIteration" in message
func isStopIteration(err error) bool {
	if err == nil {
		return false
	}
	// Check for protocols.StopIteration type
	if _, ok := err.(*protocols.StopIteration); ok {
		return true
	}
	// Check for StopIteration in error message (for core.stopIterationError)
	return strings.Contains(err.Error(), "StopIteration")
}

// Break signals a break from a loop
type BreakValue struct {
	core.BaseObject
}

// Type implements Value.Type
func (b *BreakValue) Type() core.Type {
	return "break"
}

// String implements Value.String
func (b *BreakValue) String() string {
	return "<break>"
}

// Continue signals to continue to the next iteration of a loop
type ContinueValue struct {
	core.BaseObject
}

// Type implements Value.Type
func (c *ContinueValue) Type() core.Type {
	return "continue"
}

// String implements Value.String
func (c *ContinueValue) String() string {
	return "<continue>"
}

// Create singleton values for break and continue
var (
	Break    = &BreakValue{*core.NewBaseObject("break")}
	Continue = &ContinueValue{*core.NewBaseObject("continue")}
)

// WhileForm implements the while loop special form
// Syntax: (while condition body...)
func WhileForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() < 2 {
		return nil, ErrArgCount("while requires at least 2 arguments (condition and body)")
	}

	// Extract condition and body
	condition := args.Items()[0]
	body := args.Items()[1:]

	// Create a body function that evaluates all expressions
	bodyFunc := func() (core.Value, error) {
		var result core.Value = core.Nil
		var err error

		for _, expr := range body {
			result, err = Eval(expr, ctx)
			if err != nil {
				return nil, err
			}

			// Check for flow control
			if _, ok := result.(*BreakValue); ok {
				return Break, nil
			}
			if _, ok := result.(*ContinueValue); ok {
				return Continue, nil
			}
			if _, ok := result.(*ReturnValue); ok {
				return result, nil
			}
		}

		return result, nil
	}

	var lastResult core.Value = core.Nil

	for {
		// Evaluate the condition
		condValue, err := Eval(condition, ctx)
		if err != nil {
			return nil, err
		}

		// Exit loop if condition is not truthy
		if !core.IsTruthy(condValue) {
			break
		}

		// Execute the body
		result, err := bodyFunc()
		if err != nil {
			return nil, err
		}

		// Handle special return values
		if result == Break {
			break
		}
		if result == Continue {
			continue
		}
		if ret, ok := result.(*ReturnValue); ok {
			return ret, nil
		}

		lastResult = result
	}

	return lastResult, nil
}

// ForForm implements the for loop special form
// Syntax: (for (var sequence) body...) or (for var sequence body...) or (for var1 var2 ... in sequence body...)
func ForForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() < 2 {
		return nil, ErrArgCount("for requires at least 2 arguments")
	}

	var varNames []core.SymbolValue
	var sequenceExpr core.Value
	var body []core.Value

	// Check if first arg is a list (old syntax) or symbol (new syntax)
	if binding, ok := args.Items()[0].(*core.ListValue); ok && binding.Len() >= 1 {
		// Check if next arg is 'in' keyword - if so, use Python-style syntax
		if args.Len() >= 3 {
			if sym, ok := args.Items()[1].(core.SymbolValue); ok && string(sym) == "in" {
				// Python-style: (for (var1 var2) in sequence body...)
				for i := 0; i < binding.Len(); i++ {
					varSym, ok := binding.Items()[i].(core.SymbolValue)
					if !ok {
						return nil, TypeError{Expected: "symbol", Got: binding.Items()[i].Type()}
					}
					varNames = append(varNames, varSym)
				}
				sequenceExpr = args.Items()[2]
				body = args.Items()[3:]
			} else if binding.Len() >= 2 {
				// Old syntax: (for (var sequence) body...) or (for (var1 var2 sequence) body...)
				// Extract all symbols except the last element
				for i := 0; i < binding.Len()-1; i++ {
					sym, ok := binding.Items()[i].(core.SymbolValue)
					if !ok {
						return nil, TypeError{Expected: "symbol", Got: binding.Items()[i].Type()}
					}
					varNames = append(varNames, sym)
				}
				sequenceExpr = binding.Items()[binding.Len()-1]
				body = args.Items()[1:]
			} else {
				return nil, ArgumentError{"for requires at least 2 elements in binding list"}
			}
		} else if binding.Len() >= 2 {
			// Old syntax with no 'in' keyword
			for i := 0; i < binding.Len()-1; i++ {
				sym, ok := binding.Items()[i].(core.SymbolValue)
				if !ok {
					return nil, TypeError{Expected: "symbol", Got: binding.Items()[i].Type()}
				}
				varNames = append(varNames, sym)
			}
			sequenceExpr = binding.Items()[binding.Len()-1]
			body = args.Items()[1:]
		} else {
			return nil, ArgumentError{"for requires at least 2 elements in binding list"}
		}
	} else {
		// New syntax: check for multiple vars with 'in' keyword
		// Look for 'in' keyword
		inIndex := -1
		for i := 1; i < args.Len(); i++ {
			if sym, ok := args.Items()[i].(core.SymbolValue); ok && string(sym) == "in" {
				inIndex = i
				break
			}
		}

		if inIndex > 0 && inIndex < args.Len()-1 {
			// Found 'in': (for var1 var2 ... in sequence body...)
			for i := 0; i < inIndex; i++ {
				sym, ok := args.Items()[i].(core.SymbolValue)
				if !ok {
					return nil, TypeError{Expected: "symbol", Got: args.Items()[i].Type()}
				}
				varNames = append(varNames, sym)
			}
			sequenceExpr = args.Items()[inIndex+1]
			body = args.Items()[inIndex+2:]
		} else if sym, ok := args.Items()[0].(core.SymbolValue); ok && args.Len() >= 3 {
			// Single var syntax: (for var sequence body...)
			varNames = []core.SymbolValue{sym}
			sequenceExpr = args.Items()[1]
			body = args.Items()[2:]
		} else {
			return nil, ArgumentError{"for requires (var sequence) or var sequence or var1 var2 ... in sequence syntax"}
		}
	}

	// Evaluate the sequence
	sequence, err := Eval(sequenceExpr, ctx)
	if err != nil {
		return nil, err
	}

	// Create a body function that evaluates all expressions
	bodyFunc := func(item core.Value) (core.Value, error) {
		// Handle tuple unpacking if multiple variables
		if len(varNames) > 1 {
			// Try to unpack the item as a sequence
			var values []core.Value

			switch v := item.(type) {
			case *core.ListValue:
				values = v.Items()
			case core.TupleValue:
				values = v
			default:
				// Try to get an iterator
				if iteratorObj, ok := item.(interface{ Iterator() core.Iterator }); ok {
					iter := iteratorObj.Iterator()
					values = []core.Value{}
					for {
						val, done := iter.Next()
						if done {
							break
						}
						values = append(values, val)
					}
				} else {
					return nil, fmt.Errorf("cannot unpack non-sequence type %s", item.Type())
				}
			}

			// Check if we have the right number of values
			if len(values) != len(varNames) {
				return nil, fmt.Errorf("too many values to unpack (expected %d, got %d)", len(varNames), len(values))
			}

			// Bind each value to its corresponding variable
			for i, varName := range varNames {
				ctx.Define(string(varName), values[i])
			}
		} else {
			// Single variable - bind directly
			ctx.Define(string(varNames[0]), item)
		}

		var result core.Value = core.Nil
		var err error

		for _, expr := range body {
			result, err = Eval(expr, ctx)
			if err != nil {
				return nil, err
			}

			// Check for flow control
			if _, ok := result.(*BreakValue); ok {
				return Break, nil
			}
			if _, ok := result.(*ContinueValue); ok {
				return Continue, nil
			}
			if _, ok := result.(*ReturnValue); ok {
				return result, nil
			}
		}

		return result, nil
	}

	var lastResult core.Value = core.Nil

	// Handle different sequence types
	switch seq := sequence.(type) {
	case *core.ListValue:
		// Iterate through list items
		for _, item := range seq.Items() {
			result, err := bodyFunc(item)
			if err != nil {
				return nil, err
			}

			if result == Break {
				break
			}
			if result == Continue {
				continue
			}
			if ret, ok := result.(*ReturnValue); ok {
				return ret, nil
			}

			lastResult = result
		}

	case core.TupleValue:
		// Iterate through tuple items
		for _, item := range seq {
			result, err := bodyFunc(item)
			if err != nil {
				return nil, err
			}

			if result == Break {
				break
			}
			if result == Continue {
				continue
			}
			if ret, ok := result.(*ReturnValue); ok {
				return ret, nil
			}

			lastResult = result
		}

	case core.StringValue:
		// Iterate through string characters
		for _, char := range string(seq) {
			result, err := bodyFunc(core.StringValue(string(char)))
			if err != nil {
				return nil, err
			}

			if result == Break {
				break
			}
			if result == Continue {
				continue
			}
			if ret, ok := result.(*ReturnValue); ok {
				return ret, nil
			}

			lastResult = result
		}

	default:
		// First, try calling __iter__ if it exists
		if iter, found, err := types.CallIter(sequence, ctx); found {
			if err != nil {
				return nil, err
			}

			// Debug: log what __iter__ returned
			core.DebugLog("[FOR LOOP] CallIter on %T returned %T (found=%v)\n", sequence, iter, found)
			if list, ok := iter.(*core.ListValue); ok {
				core.DebugLog("[FOR LOOP] WARNING: __iter__ returned a *core.ListValue with %d items: %v\n", list.Len(), list)
				if list.Len() == 1 {
					if str, ok := list.Items()[0].(core.StringValue); ok && str == "__iter__" {
						return nil, fmt.Errorf("BUG: __iter__ on %T returned [\"__iter__\"] instead of an iterator - this suggests a method call problem", sequence)
					}
				}
			}

			// Check if the iterator actually has __next__
			// This handles cases where __iter__ returns self but doesn't implement __next__
			if _, hasNext := types.GetDunder(iter, "__next__"); !hasNext {
				// Debug: log what we got
				core.DebugLog("[FOR LOOP] __iter__ returned %T (value: %v) which has no __next__ method\n", iter, iter)
				core.DebugLog("[FOR LOOP] Original sequence was %T\n", sequence)

				// Try protocol-based iteration on the original sequence
				if pIter, ok := protocols.GetIterableOps(sequence); ok {
					for {
						item, err := pIter.Next()
						if err != nil {
							if isStopIteration(err) {
								break
							}
							return nil, err
						}

						result, err := bodyFunc(item)
						if err != nil {
							return nil, err
						}

						if result == Break {
							break
						}
						if result == Continue {
							continue
						}
						if ret, ok := result.(*ReturnValue); ok {
							return ret, nil
						}

						lastResult = result
					}
				} else {
					return nil, fmt.Errorf("sequence type %T (value: %v) returned from __iter__ has no __next__ and no protocol support", iter, iter)
				}
			} else {
				// Use the iterator returned by __iter__
				for {
					// Call __next__ on the iterator
					item, found, err := types.CallNext(iter, ctx)
					if err != nil {
						// Check if it's StopIteration
						if isStopIteration(err) {
							break
						}
						return nil, err
					}
					if !found {
						// No __next__ method - shouldn't happen if __iter__ worked
						return nil, fmt.Errorf("iterator type %T (value: %v) has no __next__ method", iter, iter)
					}

					result, err := bodyFunc(item)
					if err != nil {
						return nil, err
					}

					if result == Break {
						break
					}
					if result == Continue {
						continue
					}
					if ret, ok := result.(*ReturnValue); ok {
						return ret, nil
					}

					lastResult = result
				}
			}
		} else if pIter, ok := protocols.GetIterableOps(sequence); ok {
			// Try protocol-based iteration
			for {
				item, err := pIter.Next()
				if err != nil {
					// Check if it's StopIteration
					if isStopIteration(err) {
						break
					}
					return nil, err
				}

				result, err := bodyFunc(item)
				if err != nil {
					return nil, err
				}

				if result == Break {
					break
				}
				if result == Continue {
					continue
				}
				if ret, ok := result.(*ReturnValue); ok {
					return ret, nil
				}

				lastResult = result
			}
		} else if iterable, ok := sequence.(core.Iterable); ok {
			// Fall back to old core.Iterable interface
			iterator := iterable.Iterator()
			for {
				item, hasNext := iterator.Next()
				if !hasNext {
					break
				}

				result, err := bodyFunc(item)
				if err != nil {
					return nil, err
				}

				if result == Break {
					break
				}
				if result == Continue {
					continue
				}
				if ret, ok := result.(*ReturnValue); ok {
					return ret, nil
				}

				lastResult = result
			}
		} else {
			return nil, TypeError{Expected: "iterable", Got: sequence.Type()}
		}
	}

	return lastResult, nil
}

// BreakForm implements the break special form
func BreakForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() != 0 {
		return nil, ErrArgCount("break takes no arguments")
	}
	return Break, nil
}

// ContinueForm implements the continue special form
func ContinueForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() != 0 {
		return nil, ErrArgCount("continue takes no arguments")
	}
	return Continue, nil
}

// External function to expose WhileForm for registration
func WhileFormHandler(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	return WhileForm(args, ctx)
}

// External function to expose ForForm for registration
func ForFormHandler(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	return ForForm(args, ctx)
}

// External function to expose BreakForm for registration
func BreakFormHandler(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	return BreakForm(args, ctx)
}

// External function to expose ContinueForm for registration
func ContinueFormHandler(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	return ContinueForm(args, ctx)
}
