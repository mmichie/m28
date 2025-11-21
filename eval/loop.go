// File contains loop-related special forms for the M28 language
package eval

import (
	"fmt"
	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/core/protocols"
	"os"
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
// Supports nested unpacking: (for (x (y z)) in sequence body...)
func ForForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() < 2 {
		return nil, ErrArgCount("for requires at least 2 arguments")
	}

	var pattern core.Value // Can be a symbol or a list (possibly nested)
	var sequenceExpr core.Value
	var body []core.Value

	// Check if first arg is a list (old syntax) or symbol (new syntax)
	// It might also be a quoted list: (quote (var1 var2))
	firstArg := args.Items()[0]
	wasQuoted := false

	// Handle quoted patterns: (quote (var1 var2)) → extract the list inside
	// This is needed because Python transpiler quotes the variable list to prevent evaluation
	if quotedList, ok := firstArg.(*core.ListValue); ok && quotedList.Len() == 2 {
		if sym, ok := quotedList.Items()[0].(core.SymbolValue); ok && string(sym) == "quote" {
			// Extract the pattern from inside the quote
			firstArg = quotedList.Items()[1]
			wasQuoted = true
		}
	}

	if binding, ok := firstArg.(*core.ListValue); ok && binding.Len() >= 1 {
		// Check for 'in' keyword first to distinguish Python-style from old 3-arg syntax
		hasInKeyword := false
		if args.Len() >= 2 {
			if sym, ok := args.Items()[1].(core.SymbolValue); ok && string(sym) == "in" {
				hasInKeyword = true
			}
		}

		if hasInKeyword {
			// Python-style with 'in': (for (var1 var2) in sequence body...)
			// Pattern is the entire binding list
			if binding.Len() == 1 {
				pattern = binding.Items()[0]
			} else {
				pattern = binding
			}
			sequenceExpr = args.Items()[2]
			body = args.Items()[3:]
		} else if binding.Len() == 2 && !wasQuoted {
			// Old 3-arg syntax: (for (var sequence) body...)
			// binding has 2 elements: var and sequence
			// Remaining args are body expressions
			// Note: wasQuoted check ensures transpiled code goes to else branch
			pattern = binding.Items()[0]
			sequenceExpr = binding.Items()[1]
			body = args.Items()[1:]
		} else {
			// New 4-arg syntax or multi-var unpacking: (for (var1 var2 var3) sequence body...)
			// OR transpiled quoted form: (for (quote (var1 var2)) sequence body...)
			// binding has 1, 2 (if quoted), or 3+ elements
			// sequence is next arg, rest are body
			pattern = binding
			sequenceExpr = args.Items()[1]
			body = args.Items()[2:]
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
			// Pattern is all elements before 'in'
			patternElements := args.Items()[0:inIndex]
			if len(patternElements) == 1 {
				pattern = patternElements[0]
			} else {
				pattern = core.NewList(patternElements...)
			}
			sequenceExpr = args.Items()[inIndex+1]
			body = args.Items()[inIndex+2:]
		} else if args.Len() >= 3 {
			// Single var syntax: (for var sequence body...)
			pattern = args.Items()[0]
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
		// Use generic unpacking to bind variables
		if err := UnpackPattern(pattern, item, ctx); err != nil {
			return nil, err
		}

		if debugLoops := os.Getenv("M28_DEBUG_LOOPS"); debugLoops != "" {
			fmt.Fprintf(os.Stderr, "[DEBUG LOOP] Body has %d expressions\n", len(body))
			for idx, expr := range body {
				fmt.Fprintf(os.Stderr, "[DEBUG LOOP]   Body[%d]: %T = %v\n", idx, expr, expr)
			}
		}

		var result core.Value = core.Nil
		var err error

		for i, expr := range body {
			result, err = Eval(expr, ctx)
			if err != nil {
				return nil, err
			}

			// Check for flow control
			if _, ok := result.(*BreakValue); ok {
				if debugLoops := os.Getenv("M28_DEBUG_LOOPS"); debugLoops != "" {
					fmt.Fprintf(os.Stderr, "[DEBUG LOOP] Body expr %d returned Break\n", i)
				}
				return Break, nil
			}
			if _, ok := result.(*ContinueValue); ok {
				if debugLoops := os.Getenv("M28_DEBUG_LOOPS"); debugLoops != "" {
					fmt.Fprintf(os.Stderr, "[DEBUG LOOP] Body expr %d returned Continue\n", i)
				}
				return Continue, nil
			}
			if _, ok := result.(*ReturnValue); ok {
				if debugLoops := os.Getenv("M28_DEBUG_LOOPS"); debugLoops != "" {
					fmt.Fprintf(os.Stderr, "[DEBUG LOOP] Body expr %d returned Return\n", i)
				}
				return result, nil
			}
			if debugLoops := os.Getenv("M28_DEBUG_LOOPS"); debugLoops != "" {
				fmt.Fprintf(os.Stderr, "[DEBUG LOOP] Body expr %d returned %T: %v\n", i, result, result)
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
