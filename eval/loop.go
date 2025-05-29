// File contains loop-related special forms for the M28 language
package eval

import (
	"github.com/mmichie/m28/core"
)

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
func WhileForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) < 2 {
		return nil, ErrArgCount("while requires at least 2 arguments (condition and body)")
	}

	// Extract condition and body
	condition := args[0]
	body := args[1:]

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
// Syntax: (for (var sequence) body...) or (for var sequence body...)
func ForForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) < 2 {
		return nil, ErrArgCount("for requires at least 2 arguments")
	}

	var varName core.SymbolValue
	var sequenceExpr core.Value
	var body core.ListValue

	// Check if first arg is a list (old syntax) or symbol (new syntax)
	if binding, ok := args[0].(core.ListValue); ok && len(binding) == 2 {
		// Old syntax: (for (var sequence) body...)
		var ok bool
		varName, ok = binding[0].(core.SymbolValue)
		if !ok {
			return nil, TypeError{Expected: "symbol", Got: binding[0].Type()}
		}
		sequenceExpr = binding[1]
		body = args[1:]
	} else if sym, ok := args[0].(core.SymbolValue); ok && len(args) >= 3 {
		// Check for Python-style syntax: (for var in sequence body...)
		if len(args) >= 4 {
			if inSym, ok := args[1].(core.SymbolValue); ok && string(inSym) == "in" {
				// Python-style syntax: (for var in sequence body...)
				varName = sym
				sequenceExpr = args[2]
				body = args[3:]
			} else {
				// New syntax: (for var sequence body...)
				varName = sym
				sequenceExpr = args[1]
				body = args[2:]
			}
		} else {
			// New syntax: (for var sequence body...)
			varName = sym
			sequenceExpr = args[1]
			body = args[2:]
		}
	} else {
		return nil, ArgumentError{"for requires (var sequence) or var sequence or var in sequence syntax"}
	}

	// Evaluate the sequence
	sequence, err := Eval(sequenceExpr, ctx)
	if err != nil {
		return nil, err
	}

	// Create a body function that evaluates all expressions
	bodyFunc := func(item core.Value) (core.Value, error) {
		// Bind the current item to the variable in the current context
		ctx.Define(string(varName), item)

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
	case core.ListValue:
		// Iterate through list items
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
		// Try to get iterator if the object implements Iterable
		if iterable, ok := sequence.(core.Iterable); ok {
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
			return nil, TypeError{Expected: "sequence", Got: sequence.Type()}
		}
	}

	return lastResult, nil
}

// BreakForm implements the break special form
func BreakForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) != 0 {
		return nil, ErrArgCount("break takes no arguments")
	}
	return Break, nil
}

// ContinueForm implements the continue special form
func ContinueForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) != 0 {
		return nil, ErrArgCount("continue takes no arguments")
	}
	return Continue, nil
}

// External function to expose WhileForm for registration
func WhileFormHandler(args core.ListValue, ctx *core.Context) (core.Value, error) {
	return WhileForm(args, ctx)
}

// External function to expose ForForm for registration
func ForFormHandler(args core.ListValue, ctx *core.Context) (core.Value, error) {
	return ForForm(args, ctx)
}

// External function to expose BreakForm for registration
func BreakFormHandler(args core.ListValue, ctx *core.Context) (core.Value, error) {
	return BreakForm(args, ctx)
}

// External function to expose ContinueForm for registration
func ContinueFormHandler(args core.ListValue, ctx *core.Context) (core.Value, error) {
	return ContinueForm(args, ctx)
}
