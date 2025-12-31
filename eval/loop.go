// File contains loop-related special forms for the M28 language
package eval

import (
	"errors"
	"fmt"
	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/core/protocols"
	"os"
	"strings"
)

// isStopIteration checks if an error is a StopIteration exception
// Uses errors.As to properly check for StopIteration types
func isStopIteration(err error) bool {
	if err == nil {
		return false
	}
	// Check for protocols.StopIteration type using errors.As
	var stopIter *protocols.StopIteration
	if errors.As(err, &stopIter) {
		return true
	}
	// Check for core.StopIteration type (from generators)
	type coreStopIteration interface {
		Error() string
	}
	// Fallback: check error message for StopIteration
	// This handles core.stopIterationError and other StopIteration variants
	return strings.Contains(err.Error(), "StopIteration")
}

// isStopAsyncIteration checks if an error is a StopAsyncIteration exception
func isStopAsyncIteration(err error) bool {
	if err == nil {
		return false
	}
	// Check error message for StopAsyncIteration
	errStr := err.Error()
	if strings.Contains(errStr, "StopAsyncIteration") {
		return true
	}
	// Also check for Exception type with StopAsyncIteration
	if exc, ok := err.(*Exception); ok && exc.Type == "StopAsyncIteration" {
		return true
	}
	return false
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

	// Extract condition
	condition := args.Items()[0]
	allArgs := args.Items()[1:]

	var body []core.Value
	var elseClause []core.Value

	// Check if this is Python-style (body wrapped in list) or S-expression style
	if len(allArgs) >= 1 {
		firstArg := allArgs[0]

		// Python style: (while cond (do body...) [(do else...)])
		// Check if first arg after condition is a list starting with 'do
		if bodyList, ok := firstArg.(*core.ListValue); ok && bodyList.Len() > 0 {
			firstItem := bodyList.Items()[0]
			if sym, ok := firstItem.(core.SymbolValue); ok && string(sym) == "do" {
				// Python style - body is wrapped in (do ...)
				body = bodyList.Items()[1:]

				// Check for else clause (also wrapped in (do ...))
				if len(allArgs) >= 2 {
					if elseList, ok := allArgs[1].(*core.ListValue); ok && elseList.Len() > 0 {
						if sym, ok := elseList.Items()[0].(core.SymbolValue); ok && string(sym) == "do" {
							elseClause = elseList.Items()[1:]
						}
					}
				}
			} else {
				// S-expression style with list as first body expr
				// Look for 'else symbol in remaining args
				body, elseClause = extractElseClause(allArgs)
			}
		} else {
			// S-expression style: (while cond body1 body2 ... [else else1 else2 ...])
			body, elseClause = extractElseClause(allArgs)
		}
	}

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
	brokeOut := false

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
			brokeOut = true
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

	// Execute else clause if we didn't break out
	if !brokeOut && len(elseClause) > 0 {
		for _, expr := range elseClause {
			var err error
			lastResult, err = Eval(expr, ctx)
			if err != nil {
				return nil, err
			}
			// Check for return in else clause
			if ret, ok := lastResult.(*ReturnValue); ok {
				return ret, nil
			}
		}
	}

	return lastResult, nil
}

// extractElseClause splits args into body and else clause at 'else symbol
func extractElseClause(args []core.Value) (body []core.Value, elseClause []core.Value) {
	for i, expr := range args {
		if sym, ok := unwrapLocated(expr).(core.SymbolValue); ok && string(sym) == "else" {
			body = args[:i]
			if i+1 < len(args) {
				elseClause = args[i+1:]
			}
			return
		}
	}
	// No else clause found
	body = args
	return
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
	firstArg := unwrapLocated(args.Items()[0])
	wasQuoted := false

	// Handle quoted patterns: (quote (var1 var2)) → extract the list inside
	// This is needed because Python transpiler quotes the variable list to prevent evaluation
	if quotedList, ok := firstArg.(*core.ListValue); ok && quotedList.Len() == 2 {
		firstQuoteElem := unwrapLocated(quotedList.Items()[0])
		if sym, ok := firstQuoteElem.(core.SymbolValue); ok && string(sym) == "quote" {
			// Extract the pattern from inside the quote
			firstArg = quotedList.Items()[1]
			wasQuoted = true
		}
	}

	if binding, ok := firstArg.(*core.ListValue); ok && binding.Len() >= 1 {
		// Check for 'in' keyword first to distinguish Python-style from old 3-arg syntax
		hasInKeyword := false
		if args.Len() >= 2 {
			secondArg := unwrapLocated(args.Items()[1])
			if sym, ok := secondArg.(core.SymbolValue); ok && string(sym) == "in" {
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
			argVal := unwrapLocated(args.Items()[i])
			if sym, ok := argVal.(core.SymbolValue); ok && string(sym) == "in" {
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
					return nil, &core.TypeError{Message: fmt.Sprintf("iter() returned non-iterator of type '%s'", iter.Type())}
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
						// No __next__ method - can happen if __next__ is deleted during iteration
						return nil, &core.TypeError{Message: fmt.Sprintf("iter() returned non-iterator of type '%s'", iter.Type())}
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
		} else if obj, ok := sequence.(core.Object); ok {
			// Try sequence protocol: __getitem__ and optional __len__
			if getitemVal, hasGetitem := obj.GetAttr("__getitem__"); hasGetitem {
				// Object has __getitem__, iterate using integer indices
				index := 0
				for {
					// Try to get item at current index
					indexVal := core.NumberValue(float64(index))
					var item core.Value
					var err error

					// Call __getitem__(index)
					if getitemFunc, ok := getitemVal.(core.Callable); ok {
						item, err = getitemFunc.Call([]core.Value{indexVal}, ctx)
						if err != nil {
							// Check if it's an IndexError (end of sequence)
							if exc, ok := err.(*Exception); ok && exc.Type == "IndexError" {
								break // End of iteration
							}
							return nil, err
						}
					} else {
						return nil, fmt.Errorf("__getitem__ is not callable")
					}

					// Execute body with the item
					result, err := bodyFunc(item)
					if err != nil {
						return nil, err
					}

					if result == Break {
						break
					}
					if result == Continue {
						index++
						continue
					}
					if ret, ok := result.(*ReturnValue); ok {
						return ret, nil
					}

					lastResult = result
					index++
				}
			} else if asyncIter, found, err := types.CallAiter(obj, ctx); found {
				// Try async iteration protocol: __aiter__ / __anext__
				if err != nil {
					return nil, err
				}

				// Loop using __anext__
				for {
					// Call __anext__ on the async iterator
					anextResult, found, err := types.CallAnext(asyncIter, ctx)
					if err != nil {
						// Check for StopAsyncIteration
						if isStopAsyncIteration(err) {
							break
						}
						return nil, err
					}
					if !found {
						return nil, fmt.Errorf("async iterator type %T has no __anext__ method", asyncIter)
					}

					// If __anext__ returns a coroutine, execute it
					var item core.Value = anextResult
					if coro, ok := anextResult.(*core.Coroutine); ok {
						// Execute the coroutine by calling its function
						if callable, callOk := coro.Function.(interface {
							Call([]core.Value, *core.Context) (core.Value, error)
						}); callOk {
							item, err = callable.Call(coro.Args, ctx)
							if err != nil {
								// Check for StopAsyncIteration
								if isStopAsyncIteration(err) {
									break
								}
								return nil, err
							}
						}
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
