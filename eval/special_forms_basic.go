// Package eval provides basic special forms evaluation
package eval

import (
	"fmt"
	"strings"

	"github.com/mmichie/m28/core"
)

func ifForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	// Delegate to util.go implementation which supports elif
	return IfForm(args, ctx)
}

// assignForm implements the = special form for assignment
func assignForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	core.DebugLog("[ASSIGN] assignForm called with %d args\n", args.Len())

	if args.Len() != 2 {
		return nil, fmt.Errorf("= requires 2 arguments, got %d", args.Len())
	}

	// Get the target
	target := args.Items()[0]
	core.DebugLog("[ASSIGN] Target type: %T\n", target)

	// Check if target is a list
	if targetList, ok := target.(*core.ListValue); ok {
		core.DebugLog("[ASSIGN] Target is a list with %d elements\n", targetList.Len())
		// Check if it's a special form (indexed or property assignment)
		if targetList.Len() > 0 {
			if sym, ok := targetList.Items()[0].(core.SymbolValue); ok {
				switch string(sym) {
				case "get-item":
					// Indexed assignment: lst[i] = value
					if targetList.Len() != 3 {
						return nil, fmt.Errorf("invalid index expression")
					}
					// Evaluate the value
					value, err := Eval(args.Items()[1], ctx)
					if err != nil {
						return nil, err
					}
					// Use SetItemForm to handle the assignment
					return SetItemForm(core.NewList(targetList.Items()[1], targetList.Items()[2], value), ctx)

				case ".":
					// Property assignment: obj.prop = value
					if targetList.Len() != 3 {
						return nil, fmt.Errorf("invalid dot notation in assignment target")
					}

					// Evaluate the object
					obj, err := Eval(targetList.Items()[1], ctx)
					if err != nil {
						return nil, err
					}

					// Get the attribute name (can be StringValue or SymbolValue)
					attrNameVal := targetList.Items()[2]
					// Unwrap LocatedValue if present
					if located, ok := attrNameVal.(core.LocatedValue); ok {
						attrNameVal = located.Unwrap()
					}
					var attrName string
					if strName, ok := attrNameVal.(core.StringValue); ok {
						attrName = string(strName)
					} else if symName, ok := attrNameVal.(core.SymbolValue); ok {
						attrName = string(symName)
					} else {
						return nil, fmt.Errorf("attribute name must be a string or symbol, got %T", attrNameVal)
					}

					// Evaluate the value
					value, err := Eval(args.Items()[1], ctx)
					if err != nil {
						return nil, err
					}

					// Set the attribute
					if objWithAttrs, ok := obj.(interface {
						SetAttr(string, core.Value) error
					}); ok {
						err := objWithAttrs.SetAttr(attrName, value)
						if err != nil {
							return nil, err
						}
						// Return the assigned value to support chained assignments
						return value, nil
					}

					// Special handling for dicts
					if dict, ok := obj.(*core.DictValue); ok {
						dict.Set(attrName, value)
						// Return the assigned value to support chained assignments
						return value, nil
					}

					return nil, fmt.Errorf("%s does not support attribute assignment", obj.Type())
				}
			}
		}

		// Tuple unpacking: (= (x, y) (10, 20))
		core.DebugLog("[ASSIGN] Tuple unpacking: %d targets\n", targetList.Len())
		// Evaluate the value
		core.DebugLog("[ASSIGN] Evaluating right-hand side\n")
		value, err := Eval(args.Items()[1], ctx)
		if err != nil {
			return nil, err
		}
		core.DebugLog("[ASSIGN] RHS evaluated to %T\n", value)

		// The value must be a tuple or list
		var values []core.Value
		switch v := value.(type) {
		case core.TupleValue:
			core.DebugLog("[ASSIGN] Unpacking tuple with %d elements\n", len(v))
			values = []core.Value(v)
		case *core.ListValue:
			core.DebugLog("[ASSIGN] Unpacking list with %d elements\n", v.Len())
			values = v.Items()
		default:
			return nil, fmt.Errorf("cannot unpack non-sequence %v", value.Type())
		}

		// Check that the number of targets matches the number of values
		if targetList.Len() != len(values) {
			if len(values) < targetList.Len() {
				return nil, fmt.Errorf("not enough values to unpack (expected %d, got %d)", targetList.Len(), len(values))
			}
			return nil, fmt.Errorf("too many values to unpack (expected %d, got %d)", targetList.Len(), len(values))
		}

		// Assign each value to its corresponding target
		core.DebugLog("[ASSIGN] Assigning %d values to targets\n", len(values))
		for i, t := range targetList.Items() {
			// Check if target is a symbol (simple variable)
			if sym, ok := t.(core.SymbolValue); ok {
				core.DebugLog("[ASSIGN] Defining %s = %T\n", string(sym), values[i])
				ctx.Define(string(sym), values[i])
				continue
			}

			// Check if target is dot notation (attribute access)
			if targetList2, ok := t.(*core.ListValue); ok && targetList2.Len() > 0 {
				if sym, ok := targetList2.Items()[0].(core.SymbolValue); ok && string(sym) == "." {
					// This is attribute assignment like self.a = value
					// Use the same logic as single attribute assignment
					// Evaluate to get the (object, attribute) pair, then set it
					if targetList2.Len() != 3 {
						return nil, fmt.Errorf("invalid dot notation in assignment target")
					}

					// Evaluate the object
					obj, err := Eval(targetList2.Items()[1], ctx)
					if err != nil {
						return nil, err
					}

					// Get the attribute name (can be StringValue or SymbolValue)
					attrNameVal := targetList2.Items()[2]
					// Unwrap LocatedValue if present
					if located, ok := attrNameVal.(core.LocatedValue); ok {
						attrNameVal = located.Unwrap()
					}
					var attrName string
					if strName, ok := attrNameVal.(core.StringValue); ok {
						attrName = string(strName)
					} else if symName, ok := attrNameVal.(core.SymbolValue); ok {
						attrName = string(symName)
					} else {
						return nil, fmt.Errorf("attribute name must be a string or symbol, got %T", attrNameVal)
					}

					// Set the attribute using the same pattern as util.go
					if objWithAttrs, ok := obj.(interface {
						SetAttr(string, core.Value) error
					}); ok {
						err := objWithAttrs.SetAttr(attrName, values[i])
						if err != nil {
							return nil, err
						}
						continue
					}

					// Special handling for dicts
					if dict, ok := obj.(*core.DictValue); ok {
						dict.Set(attrName, values[i])
						continue
					}

					return nil, fmt.Errorf("%s does not support attribute assignment", obj.Type())
				}

				// Not dot notation - must be nested unpacking pattern like (a, b)
				// Use generic unpacking
				if err := UnpackPattern(targetList2, values[i], ctx); err != nil {
					return nil, err
				}
				continue
			}

			// Target is neither a symbol, dot notation, nor tuple pattern
			return nil, fmt.Errorf("assignment target must be a symbol, dot notation, or tuple pattern, got %v", t.Type())
		}

		core.DebugLog("[ASSIGN] Tuple unpacking complete\n")
		// Return the unpacked value to support chained assignments
		return value, nil
	}

	// Single variable assignment
	sym, ok := target.(core.SymbolValue)
	if !ok {
		return nil, fmt.Errorf("assignment target must be a symbol, got %v", target.Type())
	}

	// Evaluate the value
	value, err := Eval(args.Items()[1], ctx)
	if err != nil {
		return nil, err
	}

	// Variable assignment - always define/update in current scope
	symName := string(sym)
	ctx.Define(symName, value)
	// Return the assigned value to support chained assignments
	core.DebugLog("[ASSIGN] Returning value %v for variable %s\n", value, symName)
	return value, nil
}

// annotatedAssignForm implements annotated assignment (PEP 526): x: int = 5
// Syntax: (annotated-assign target annotation [value])
// Supports: variable (x: int = 5), attribute (obj.x: int = 5), subscript (obj[k]: int = 5)
func annotatedAssignForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() < 2 || args.Len() > 3 {
		return nil, fmt.Errorf("annotated-assign requires 2 or 3 arguments, got %d", args.Len())
	}

	target := args.Items()[0]
	// Unwrap LocatedValue if present (Python AST wraps with source location)
	if located, ok := target.(core.LocatedValue); ok {
		target = located.Unwrap()
	}

	// Get the annotation (can be a string or symbol)
	annotationVal := args.Items()[1]
	var annotationType core.Value

	// Handle both old format (string) and new format (symbol)
	switch ann := annotationVal.(type) {
	case core.StringValue:
		// Old format: annotation is a string literal (for compatibility)
		annotationType = ann
	case core.SymbolValue:
		// New format: annotation is a symbol to be evaluated
		var err error
		annotationType, err = Eval(annotationVal, ctx)
		if err != nil {
			return nil, fmt.Errorf("error evaluating annotation: %v", err)
		}
	default:
		// Try to evaluate it as an expression
		var err error
		annotationType, err = Eval(annotationVal, ctx)
		if err != nil {
			return nil, fmt.Errorf("annotation must be a string or symbol, got %v: %v", annotationVal.Type(), err)
		}
	}

	// Check what kind of target we have
	if sym, ok := target.(core.SymbolValue); ok {
		// Simple variable: x: int = 5
		targetName := string(sym)

		// Ensure __annotations__ dict exists in current scope
		var annotationsDict *core.DictValue
		annVal, err := ctx.Lookup("__annotations__")
		if err == nil {
			// __annotations__ exists
			if dict, isDict := annVal.(*core.DictValue); isDict {
				annotationsDict = dict
			} else {
				// __annotations__ exists but is not a dict, replace it
				annotationsDict = core.NewDict()
				ctx.Define("__annotations__", annotationsDict)
			}
		} else {
			// Create __annotations__ dict
			annotationsDict = core.NewDict()
			ctx.Define("__annotations__", annotationsDict)
		}

		// Store the annotation (evaluated type object)
		annotationsDict.Set(targetName, annotationType)

		// If there's a value, evaluate and assign it
		if args.Len() == 3 {
			value, err := Eval(args.Items()[2], ctx)
			if err != nil {
				return nil, err
			}
			ctx.Define(targetName, value)
			return value, nil
		}

		// No value - just annotation
		return core.Nil, nil
	} else if targetList, ok := target.(*core.ListValue); ok && targetList.Len() > 0 {
		// Attribute or subscript: obj.x: int = 5 or obj[k]: int = 5
		// For these, we don't store annotations (Python doesn't either)
		// We just perform the assignment if there's a value

		if args.Len() == 3 {
			// Use the regular assignment form which already handles
			// attribute (.) and subscript (get-item) assignments
			assignArgs := core.NewList(target, args.Items()[2])
			return AssignForm(assignArgs, ctx)
		}

		// No value - just annotation, nothing to do
		return core.Nil, nil
	}

	return nil, fmt.Errorf("annotated assignment target must be a symbol, attribute, or subscript, got %T", target)
}

// quoteForm implements the quote special form
func quoteForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() != 1 {
		return nil, fmt.Errorf("quote requires 1 argument")
	}

	// Return the argument unevaluated
	return args.Items()[0], nil
}

// doForm implements the do special form for grouping expressions
func doForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() == 0 {
		return core.Nil, nil
	}

	// Evaluate all expressions in sequence
	var result core.Value = core.Nil
	var err error

	for _, expr := range args.Items() {
		result, err = Eval(expr, ctx)
		if err != nil {
			return nil, err
		}

		// Check for flow control - propagate break/continue/return
		// These need to bubble up to the enclosing loop or function
		if _, ok := result.(*ReturnValue); ok {
			return result, nil
		}
		if _, ok := result.(*BreakValue); ok {
			return result, nil
		}
		if _, ok := result.(*ContinueValue); ok {
			return result, nil
		}
	}

	// Return the value of the last expression
	return result, nil
}

// returnForm implements the return special form
func returnForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() > 1 {
		return nil, fmt.Errorf("return takes at most 1 argument")
	}

	var value core.Value = core.Nil
	var err error

	if args.Len() == 1 {
		value, err = Eval(args.Items()[0], ctx)
		if err != nil {
			return nil, err
		}
	}

	return &ReturnValue{Value: value}, nil
}

// importForm implements the import special form

func convertIterableToSlice(iterable core.Value) ([]core.Value, error) {
	var items []core.Value
	switch v := iterable.(type) {
	case *core.ListValue:
		items = v.Items()
	case core.TupleValue:
		items = v
	case core.StringValue:
		// Convert string to list of characters
		for _, ch := range string(v) {
			items = append(items, core.StringValue(string(ch)))
		}
	default:
		// Try using Iterator interface
		if iterableObj, ok := v.(core.Iterable); ok {
			iter := iterableObj.Iterator()
			for {
				val, hasNext := iter.Next()
				if !hasNext {
					break
				}
				items = append(items, val)
			}
		} else {
			return nil, fmt.Errorf("value must be iterable, got %s", v.Type())
		}
	}
	return items, nil
}

// unpackTuplePattern unpacks a value according to a pattern like "(x, y)" or "(x, (y, z))"
// and binds the variables in the given context
func unpackTuplePattern(pattern string, value core.Value, ctx *core.Context) error {
	// Remove outer parentheses
	pattern = strings.TrimSpace(pattern)
	if !strings.HasPrefix(pattern, "(") || !strings.HasSuffix(pattern, ")") {
		return fmt.Errorf("invalid tuple pattern: %s", pattern)
	}
	pattern = pattern[1 : len(pattern)-1]

	// Parse variable names from the pattern
	// Simple approach: split by comma at the top level
	varNames := parseTuplePatternVars(pattern)

	// Convert value to a slice
	var values []core.Value
	switch v := value.(type) {
	case core.TupleValue:
		values = []core.Value(v)
	case *core.ListValue:
		values = v.Items()
	default:
		return fmt.Errorf("cannot unpack non-sequence type %s", value.Type())
	}

	// Check length matches
	if len(values) != len(varNames) {
		return fmt.Errorf("cannot unpack %d values into %d variables", len(values), len(varNames))
	}

	// Bind each variable
	for i, varName := range varNames {
		varName = strings.TrimSpace(varName)
		// Check if this is a nested pattern
		if strings.HasPrefix(varName, "(") && strings.HasSuffix(varName, ")") {
			// Recursive unpacking
			if err := unpackTuplePattern(varName, values[i], ctx); err != nil {
				return err
			}
		} else {
			// Simple variable binding
			ctx.Define(varName, values[i])
		}
	}

	return nil
}

// parseTuplePatternVars splits a tuple pattern into variable names
// Example: "x, y" -> ["x", "y"]
// Example: "x, (y, z)" -> ["x", "(y, z)"]
func parseTuplePatternVars(pattern string) []string {
	var result []string
	var current strings.Builder
	depth := 0

	for _, ch := range pattern {
		switch ch {
		case '(':
			depth++
			current.WriteRune(ch)
		case ')':
			depth--
			current.WriteRune(ch)
		case ',':
			if depth == 0 {
				// Top-level comma, split here
				result = append(result, current.String())
				current.Reset()
			} else {
				current.WriteRune(ch)
			}
		default:
			current.WriteRune(ch)
		}
	}

	// Add the last part
	if current.Len() > 0 {
		result = append(result, current.String())
	}

	return result
}

// comprehensionLoop executes a comprehension loop over items with optional filtering
// The callback is called for each item that passes the condition (if provided)
func comprehensionLoop(
	items []core.Value,
	varName string,
	condition core.Value, // nil if no condition
	ctx *core.Context,
	callback func(loopCtx *core.Context) error,
) error {
	loopCtx := core.NewContext(ctx)
	for _, item := range items {
		// Check if varName is a tuple pattern like "(x, y)" or "(x, (y, z))"
		if strings.HasPrefix(varName, "(") && strings.HasSuffix(varName, ")") {
			// Parse the tuple pattern and unpack the item
			if err := unpackTuplePattern(varName, item, loopCtx); err != nil {
				return fmt.Errorf("error unpacking loop variable: %v", err)
			}
		} else {
			// Simple variable binding
			loopCtx.Define(varName, item)
		}

		// Check condition if present
		if condition != nil {
			condResult, err := Eval(condition, loopCtx)
			if err != nil {
				return fmt.Errorf("error evaluating condition: %v", err)
			}

			// Skip if condition is falsy
			if !core.IsTruthy(condResult) {
				continue
			}
		}

		// Execute the callback for this item
		if err := callback(loopCtx); err != nil {
			return err
		}
	}
	return nil
}

// listCompForm implements list comprehensions
// Forms:
//
//	(list-comp expr var iterable)
//	(list-comp expr var iterable condition)
//	(list-comp expr ((var1 iter1 [cond1]) (var2 iter2 [cond2]) ...))  // nested
func ListCompForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() < 2 {
		return nil, fmt.Errorf("list-comp requires at least 2 arguments")
	}

	// Check if this is multi-clause format (args.Items()[1] is a list)
	if clausesList, ok := args.Items()[1].(*core.ListValue); ok {
		// Multi-clause nested comprehension (2 args: expr, clauses)
		if args.Len() != 2 {
			return nil, fmt.Errorf("multi-clause list-comp requires exactly 2 arguments")
		}
		return listCompMultiClause(args.Items()[0], clausesList, ctx)
	}

	// Single-clause format (backward compatible)
	// Requires 3-4 args: expr, var, iterable, [condition]
	if args.Len() < 3 || args.Len() > 4 {
		return nil, fmt.Errorf("single-clause list-comp requires 3 or 4 arguments")
	}

	// Get the variable name
	varSym, ok := args.Items()[1].(core.SymbolValue)
	if !ok {
		return nil, fmt.Errorf("list comprehension variable must be a symbol")
	}
	varName := string(varSym)

	// Evaluate the iterable
	iterableExpr := args.Items()[2]
	iterable, err := Eval(iterableExpr, ctx)
	if err != nil {
		// Debug: show what expression failed
		exprStr := core.PrintValue(iterableExpr)
		if len(exprStr) > 100 {
			exprStr = exprStr[:100] + "..."
		}
		return nil, fmt.Errorf("error evaluating iterable %s: %v", exprStr, err)
	}

	// Convert iterable to a sequence we can iterate over
	items, err := convertIterableToSlice(iterable)
	if err != nil {
		return nil, fmt.Errorf("list comprehension: %v", err)
	}

	// Create result list
	result := make([]core.Value, 0)

	// Get optional condition
	var condition core.Value
	if args.Len() == 4 {
		condition = args.Items()[3]
	}

	// Execute the comprehension loop
	err = comprehensionLoop(items, varName, condition, ctx, func(loopCtx *core.Context) error {
		// Evaluate the expression
		exprResult, err := Eval(args.Items()[0], loopCtx)
		if err != nil {
			return fmt.Errorf("error evaluating expression: %v", err)
		}

		// Add to result
		result = append(result, exprResult)
		return nil
	})

	if err != nil {
		return nil, err
	}

	return core.NewList(result...), nil
}

// listCompMultiClause handles nested list comprehensions
// Format: (list-comp expr ((var1 iter1 [cond1]) (var2 iter2 [cond2]) ...))
func listCompMultiClause(expr core.Value, clausesList *core.ListValue, ctx *core.Context) (core.Value, error) {
	result := make([]core.Value, 0)

	// Parse clauses
	type clause struct {
		varName   string
		iterable  core.Value
		condition core.Value
	}

	clauses := make([]clause, 0, clausesList.Len())
	for i, clauseVal := range clausesList.Items() {
		clauseList, ok := clauseVal.(*core.ListValue)
		if !ok {
			return nil, fmt.Errorf("clause %d must be a list", i)
		}

		if clauseList.Len() < 2 || clauseList.Len() > 3 {
			return nil, fmt.Errorf("clause %d must have 2 or 3 elements (var, iter, [condition])", i)
		}

		varSym, ok := clauseList.Items()[0].(core.SymbolValue)
		if !ok {
			return nil, fmt.Errorf("clause %d variable must be a symbol", i)
		}

		var condition core.Value
		if clauseList.Len() == 3 {
			condition = clauseList.Items()[2]
		}

		clauses = append(clauses, clause{
			varName:   string(varSym),
			iterable:  clauseList.Items()[1], // Not evaluated yet
			condition: condition,
		})
	}

	// Recursively evaluate nested loops
	var evalClauses func(clauseIdx int, loopCtx *core.Context) error
	evalClauses = func(clauseIdx int, loopCtx *core.Context) error {
		if clauseIdx >= len(clauses) {
			// All loops processed, evaluate the expression
			exprResult, err := Eval(expr, loopCtx)
			if err != nil {
				return fmt.Errorf("error evaluating expression: %v", err)
			}
			result = append(result, exprResult)
			return nil
		}

		// Evaluate the iterable for this clause
		c := clauses[clauseIdx]
		iterable, err := Eval(c.iterable, loopCtx)
		if err != nil {
			return fmt.Errorf("error evaluating iterable in clause %d: %v", clauseIdx, err)
		}

		// Convert to slice
		items, err := convertIterableToSlice(iterable)
		if err != nil {
			return fmt.Errorf("clause %d: %v", clauseIdx, err)
		}

		// Loop over items
		return comprehensionLoop(items, c.varName, c.condition, loopCtx, func(innerCtx *core.Context) error {
			// Recurse to next clause
			return evalClauses(clauseIdx+1, innerCtx)
		})
	}

	// Start evaluation from first clause
	if err := evalClauses(0, ctx); err != nil {
		return nil, err
	}

	return core.NewList(result...), nil
}

// DictCompForm implements the dict-comp special form
// Syntax:
//
//	(dict-comp key-expr value-expr var iterable)
//	(dict-comp key-expr value-expr var iterable condition)
//	(dict-comp key-expr value-expr ((var1 iter1 [cond1]) (var2 iter2 [cond2]) ...))  // nested
func DictCompForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() < 3 {
		return nil, fmt.Errorf("dict-comp requires at least 3 arguments")
	}

	// Check if this is multi-clause format (args.Items()[2] is a list)
	if clausesList, ok := args.Items()[2].(*core.ListValue); ok {
		// Multi-clause nested comprehension (3 args: key-expr, value-expr, clauses)
		if args.Len() != 3 {
			return nil, fmt.Errorf("multi-clause dict-comp requires exactly 3 arguments")
		}
		return dictCompMultiClause(args.Items()[0], args.Items()[1], clausesList, ctx)
	}

	// Single-clause format (backward compatible)
	// Requires 4-5 args: key-expr, value-expr, var, iterable, [condition]
	if args.Len() < 4 || args.Len() > 5 {
		return nil, fmt.Errorf("single-clause dict-comp requires 4 or 5 arguments")
	}

	// Get the variable name
	varSym, ok := args.Items()[2].(core.SymbolValue)
	if !ok {
		return nil, fmt.Errorf("dict comprehension variable must be a symbol")
	}
	varName := string(varSym)

	// Evaluate the iterable
	iterable, err := Eval(args.Items()[3], ctx)
	if err != nil {
		return nil, fmt.Errorf("error evaluating iterable: %v", err)
	}

	// Convert iterable to a sequence we can iterate over
	items, err := convertIterableToSlice(iterable)
	if err != nil {
		return nil, fmt.Errorf("dict comprehension: %v", err)
	}

	// Create result dict
	result := core.NewDict()

	// Get optional condition
	var condition core.Value
	if args.Len() == 5 {
		condition = args.Items()[4]
	}

	// Execute the comprehension loop
	err = comprehensionLoop(items, varName, condition, ctx, func(loopCtx *core.Context) error {
		// Evaluate the key expression
		keyResult, err := Eval(args.Items()[0], loopCtx)
		if err != nil {
			return fmt.Errorf("error evaluating key expression: %v", err)
		}

		// Evaluate the value expression
		valueResult, err := Eval(args.Items()[1], loopCtx)
		if err != nil {
			return fmt.Errorf("error evaluating value expression: %v", err)
		}

		// Add to result dict
		keyStr := core.ValueToKey(keyResult)
		result.SetWithKey(keyStr, keyResult, valueResult)
		return nil
	})

	if err != nil {
		return nil, err
	}

	return result, nil
}

// dictCompMultiClause handles nested dict comprehensions
// Format: (dict-comp key-expr value-expr ((var1 iter1 [cond1]) (var2 iter2 [cond2]) ...))
func dictCompMultiClause(keyExpr, valueExpr core.Value, clausesList *core.ListValue, ctx *core.Context) (core.Value, error) {
	result := core.NewDict()

	// Parse clauses
	type clause struct {
		varName   string
		iterable  core.Value
		condition core.Value
	}

	clauses := make([]clause, 0, clausesList.Len())
	for i, clauseVal := range clausesList.Items() {
		clauseList, ok := clauseVal.(*core.ListValue)
		if !ok {
			return nil, fmt.Errorf("clause %d must be a list", i)
		}

		if clauseList.Len() < 2 || clauseList.Len() > 3 {
			return nil, fmt.Errorf("clause %d must have 2 or 3 elements (var, iter, [condition])", i)
		}

		varSym, ok := clauseList.Items()[0].(core.SymbolValue)
		if !ok {
			return nil, fmt.Errorf("clause %d variable must be a symbol", i)
		}

		var condition core.Value
		if clauseList.Len() == 3 {
			condition = clauseList.Items()[2]
		}

		clauses = append(clauses, clause{
			varName:   string(varSym),
			iterable:  clauseList.Items()[1],
			condition: condition,
		})
	}

	// Recursively evaluate nested loops
	var evalClauses func(clauseIdx int, loopCtx *core.Context) error
	evalClauses = func(clauseIdx int, loopCtx *core.Context) error {
		if clauseIdx >= len(clauses) {
			// All loops processed, evaluate key and value
			keyResult, err := Eval(keyExpr, loopCtx)
			if err != nil {
				return fmt.Errorf("error evaluating key expression: %v", err)
			}

			valueResult, err := Eval(valueExpr, loopCtx)
			if err != nil {
				return fmt.Errorf("error evaluating value expression: %v", err)
			}

			keyStr := core.ValueToKey(keyResult)
			result.SetWithKey(keyStr, keyResult, valueResult)
			return nil
		}

		// Evaluate the iterable for this clause
		c := clauses[clauseIdx]
		iterable, err := Eval(c.iterable, loopCtx)
		if err != nil {
			return fmt.Errorf("error evaluating iterable in clause %d: %v", clauseIdx, err)
		}

		// Convert to slice
		items, err := convertIterableToSlice(iterable)
		if err != nil {
			return fmt.Errorf("clause %d: %v", clauseIdx, err)
		}

		// Loop over items
		return comprehensionLoop(items, c.varName, c.condition, loopCtx, func(innerCtx *core.Context) error {
			return evalClauses(clauseIdx+1, innerCtx)
		})
	}

	if err := evalClauses(0, ctx); err != nil {
		return nil, err
	}

	return result, nil
}

// SetCompForm implements the set-comp special form
// Syntax:
//
//	(set-comp expr var iterable)
//	(set-comp expr var iterable condition)
//	(set-comp expr ((var1 iter1 [cond1]) (var2 iter2 [cond2]) ...))  // nested
func SetCompForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() < 2 {
		return nil, fmt.Errorf("set-comp requires at least 2 arguments")
	}

	// Check if this is multi-clause format (args.Items()[1] is a list)
	if clausesList, ok := args.Items()[1].(*core.ListValue); ok {
		// Multi-clause nested comprehension (2 args: expr, clauses)
		if args.Len() != 2 {
			return nil, fmt.Errorf("multi-clause set-comp requires exactly 2 arguments")
		}
		return setCompMultiClause(args.Items()[0], clausesList, ctx)
	}

	// Single-clause format (backward compatible)
	// Requires 3-4 args: expr, var, iterable, [condition]
	if args.Len() < 3 || args.Len() > 4 {
		return nil, fmt.Errorf("single-clause set-comp requires 3 or 4 arguments")
	}

	// Get the variable name
	varSym, ok := args.Items()[1].(core.SymbolValue)
	if !ok {
		return nil, fmt.Errorf("set comprehension variable must be a symbol")
	}
	varName := string(varSym)

	// Evaluate the iterable
	iterableExpr := args.Items()[2]
	iterable, err := Eval(iterableExpr, ctx)
	if err != nil {
		// Debug: show what expression failed
		exprStr := core.PrintValue(iterableExpr)
		if len(exprStr) > 100 {
			exprStr = exprStr[:100] + "..."
		}
		return nil, fmt.Errorf("error evaluating iterable %s: %v", exprStr, err)
	}

	// Convert iterable to a sequence we can iterate over
	items, err := convertIterableToSlice(iterable)
	if err != nil {
		return nil, fmt.Errorf("set comprehension: %v", err)
	}

	// Create result set
	result := core.NewSet()

	// Get optional condition
	var condition core.Value
	if args.Len() == 4 {
		condition = args.Items()[3]
	}

	// Execute the comprehension loop
	err = comprehensionLoop(items, varName, condition, ctx, func(loopCtx *core.Context) error {
		// Evaluate the expression
		exprResult, err := Eval(args.Items()[0], loopCtx)
		if err != nil {
			return fmt.Errorf("error evaluating expression: %v", err)
		}

		// Add to result set
		result.Add(exprResult)
		return nil
	})

	if err != nil {
		return nil, err
	}

	return result, nil
}

// setCompMultiClause handles nested set comprehensions
// Format: (set-comp expr ((var1 iter1 [cond1]) (var2 iter2 [cond2]) ...))
func setCompMultiClause(expr core.Value, clausesList *core.ListValue, ctx *core.Context) (core.Value, error) {
	result := core.NewSet()

	// Parse clauses
	type clause struct {
		varName   string
		iterable  core.Value
		condition core.Value
	}

	clauses := make([]clause, 0, clausesList.Len())
	for i, clauseVal := range clausesList.Items() {
		clauseList, ok := clauseVal.(*core.ListValue)
		if !ok {
			return nil, fmt.Errorf("clause %d must be a list", i)
		}

		if clauseList.Len() < 2 || clauseList.Len() > 3 {
			return nil, fmt.Errorf("clause %d must have 2 or 3 elements (var, iter, [condition])", i)
		}

		varSym, ok := clauseList.Items()[0].(core.SymbolValue)
		if !ok {
			return nil, fmt.Errorf("clause %d variable must be a symbol", i)
		}

		var condition core.Value
		if clauseList.Len() == 3 {
			condition = clauseList.Items()[2]
		}

		clauses = append(clauses, clause{
			varName:   string(varSym),
			iterable:  clauseList.Items()[1],
			condition: condition,
		})
	}

	// Recursively evaluate nested loops
	var evalClauses func(clauseIdx int, loopCtx *core.Context) error
	evalClauses = func(clauseIdx int, loopCtx *core.Context) error {
		if clauseIdx >= len(clauses) {
			// All loops processed, evaluate the expression
			exprResult, err := Eval(expr, loopCtx)
			if err != nil {
				return fmt.Errorf("error evaluating expression: %v", err)
			}
			result.Add(exprResult)
			return nil
		}

		// Evaluate the iterable for this clause
		c := clauses[clauseIdx]
		iterable, err := Eval(c.iterable, loopCtx)
		if err != nil {
			return fmt.Errorf("error evaluating iterable in clause %d: %v", clauseIdx, err)
		}

		// Convert to slice
		items, err := convertIterableToSlice(iterable)
		if err != nil {
			return fmt.Errorf("clause %d: %v", clauseIdx, err)
		}

		// Loop over items
		return comprehensionLoop(items, c.varName, c.condition, loopCtx, func(innerCtx *core.Context) error {
			return evalClauses(clauseIdx+1, innerCtx)
		})
	}

	if err := evalClauses(0, ctx); err != nil {
		return nil, err
	}

	return result, nil
}

// genExprMultiClause handles nested generator expressions with lazy evaluation
// Format: (gen-comp expr ((var1 iter1 [cond1]) (var2 iter2 [cond2]) ...))
func genExprMultiClause(expr core.Value, clausesList *core.ListValue, ctx *core.Context) (core.Value, error) {
	// Parse clauses into GenClause structs
	clauses := make([]core.GenClause, 0, clausesList.Len())
	for i, clauseVal := range clausesList.Items() {
		clauseList, ok := clauseVal.(*core.ListValue)
		if !ok {
			return nil, fmt.Errorf("clause %d must be a list", i)
		}

		if clauseList.Len() < 2 || clauseList.Len() > 3 {
			return nil, fmt.Errorf("clause %d must have 2 or 3 elements (var, iter, [condition])", i)
		}

		// Get variable (or variables for tuple unpacking)
		var varName string
		var varNames []string

		varArg := clauseList.Items()[0]
		if varSym, ok := varArg.(core.SymbolValue); ok {
			// Single variable
			varName = string(varSym)
		} else if varList, ok := varArg.(*core.ListValue); ok {
			// Multiple variables (tuple unpacking)
			varNames = make([]string, varList.Len())
			for j, v := range varList.Items() {
				if sym, ok := v.(core.SymbolValue); ok {
					varNames[j] = string(sym)
				} else {
					return nil, fmt.Errorf("clause %d: variable list must contain symbols", i)
				}
			}
		} else {
			return nil, fmt.Errorf("clause %d: variable must be a symbol or list of symbols", i)
		}

		// Get condition if present
		var condition core.Value
		if clauseList.Len() == 3 {
			condition = clauseList.Items()[2]
		}

		clauses = append(clauses, core.GenClause{
			VarName:   varName,
			VarNames:  varNames,
			Iterable:  clauseList.Items()[1], // Store unevaluated iterable
			Condition: condition,
		})
	}

	// Create lazy generator
	return core.NewLazyGeneratorExpression("multi-clause-genexpr", expr, clauses, ctx, Eval), nil
}

// GenExprForm implements generator expressions
// Forms:
//
//	Old format: (gen-expr expr var iterable [condition])
//	Lambda format: (gen-comp (lambda (var) expr) iterable [(lambda (var) condition)])
//	Multi-clause format: (gen-comp expr ((var1 iter1 [cond1]) (var2 iter2 [cond2]) ...))
//
// Returns a Generator object that lazily evaluates the expression
func GenExprForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() < 2 || args.Len() > 4 {
		return nil, fmt.Errorf("gen-expr/gen-comp requires 2-4 arguments")
	}

	// Check for multi-clause format: (gen-comp expr ((var iter [cond])...))
	if args.Len() == 2 {
		if clausesList, ok := args.Items()[1].(*core.ListValue); ok && clausesList.Len() > 0 {
			// Check if first item is a list (indicating multi-clause format)
			if firstClause, ok := clausesList.Items()[0].(*core.ListValue); ok && firstClause.Len() >= 2 {
				// Multi-clause format detected
				return genExprMultiClause(args.Items()[0], clausesList, ctx)
			}
		}
	}

	// Check if this is lambda format: first arg is a lambda
	if lambdaList, ok := args.Items()[0].(*core.ListValue); ok && lambdaList.Len() > 0 {
		if lambdaSym, ok := lambdaList.Items()[0].(core.SymbolValue); ok && string(lambdaSym) == "lambda" {
			// Lambda format: (gen-comp (lambda (var) expr) iterable [(lambda (var) condition)])
			if args.Len() < 2 || args.Len() > 3 {
				return nil, fmt.Errorf("gen-comp lambda format requires 2 or 3 arguments")
			}

			// Extract variable name(s) from lambda
			if lambdaList.Len() < 3 {
				return nil, fmt.Errorf("invalid lambda in gen-comp")
			}
			params, ok := lambdaList.Items()[1].(*core.ListValue)
			if !ok || params.Len() != 1 {
				return nil, fmt.Errorf("gen-comp lambda must have exactly one parameter")
			}

			// Check if parameter is a list (for unpacking) or a symbol (single variable)
			var varName string
			var varNames []string
			param := params.Items()[0]

			if listParam, ok := param.(*core.ListValue); ok {
				// List parameter (parsed from tuple pattern) - extract variable names for unpacking
				varNames = make([]string, listParam.Len())
				for i, v := range listParam.Items() {
					if sym, ok := v.(core.SymbolValue); ok {
						varNames[i] = string(sym)
					} else {
						return nil, fmt.Errorf("gen-comp lambda tuple parameter must contain only symbols")
					}
				}
			} else if varSym, ok := param.(core.SymbolValue); ok {
				// Single symbol parameter
				varName = string(varSym)
			} else {
				return nil, fmt.Errorf("gen-comp lambda parameter must be a symbol or list, got %T", param)
			}

			// Expression is the lambda body
			expr := lambdaList.Items()[2]

			// Store the unevaluated iterable expression for lazy evaluation
			iterableExpr := args.Items()[1]

			// Optional condition (also a lambda)
			var condition core.Value
			if args.Len() == 3 {
				if condLambda, ok := args.Items()[2].(*core.ListValue); ok && condLambda.Len() >= 3 {
					condition = condLambda.Items()[2] // lambda body
				}
			}

			// Create single-clause lazy generator
			// Note: Store unevaluated iterable for true lazy evaluation
			clause := core.GenClause{
				VarName:   varName,
				VarNames:  varNames,
				Iterable:  iterableExpr, // Store unevaluated
				Condition: condition,
			}
			return core.NewLazyGeneratorExpression("genexpr", expr, []core.GenClause{clause}, ctx, Eval), nil
		}
	}

	// Old format: (gen-expr expr var iterable [condition])
	if args.Len() < 3 || args.Len() > 4 {
		return nil, fmt.Errorf("gen-expr old format requires 3 or 4 arguments")
	}

	// Get the variable name
	varSym, ok := args.Items()[1].(core.SymbolValue)
	if !ok {
		return nil, fmt.Errorf("generator expression variable must be a symbol")
	}
	varName := string(varSym)

	// Store the unevaluated iterable expression for lazy evaluation
	iterableExpr := args.Items()[2]

	// Store the expression, condition (if present), and context
	expr := args.Items()[0]
	var condition core.Value
	if args.Len() == 4 {
		condition = args.Items()[3]
	}

	// Create single-clause lazy generator
	// Note: Store unevaluated iterable for true lazy evaluation
	clause := core.GenClause{
		VarName:   varName,
		VarNames:  nil,
		Iterable:  iterableExpr, // Store unevaluated
		Condition: condition,
	}
	return core.NewLazyGeneratorExpression("genexpr", expr, []core.GenClause{clause}, ctx, Eval), nil
}

// listLiteralForm implements the list-literal special form
// It evaluates all arguments and returns them as a list
// Supports starred expressions: [1, *other_list, 3]
func listLiteralForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	result := make([]core.Value, 0, args.Len())

	// Evaluate each element
	for _, arg := range args.Items() {
		// Unwrap LocatedValue for the arg itself
		unwrappedArg := arg
		if located, ok := arg.(core.LocatedValue); ok {
			unwrappedArg = located.Unwrap()
		}

		// Check if this is an unpacking expression: (*unpack-iter expr)
		if list, ok := unwrappedArg.(*core.ListValue); ok && list.Len() == 2 {
			// Unwrap LocatedValue when checking for marker
			firstElem := list.Items()[0]
			if located, ok := firstElem.(core.LocatedValue); ok {
				firstElem = located.Unwrap()
			}
			if sym, ok := firstElem.(core.SymbolValue); ok && string(sym) == "*unpack-iter" {
				// Evaluate the expression to unpack
				val, err := Eval(list.Items()[1], ctx)
				if err != nil {
					return nil, err
				}
				// Unpack the iterable into the result
				items, err := convertIterableToSlice(val)
				if err != nil {
					return nil, fmt.Errorf("cannot unpack non-iterable: %v", err)
				}
				result = append(result, items...)
				continue
			}
		}

		// Regular element - evaluate and append
		val, err := Eval(arg, ctx)
		if err != nil {
			return nil, err
		}
		result = append(result, val)
	}

	return core.NewList(result...), nil
}

// Track recursion depth for tuple literal evaluation to prevent infinite loops
var tupleLiteralDepth = 0

const maxTupleLiteralDepth = 1000

// tupleLiteralForm implements the tuple-literal special form
// Usage: (tuple-literal elem1 elem2 ...)
// Supports starred expressions: (1, *other_tuple, 3)
func tupleLiteralForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	// Increment depth counter and check for infinite recursion
	tupleLiteralDepth++
	defer func() { tupleLiteralDepth-- }()

	if tupleLiteralDepth > maxTupleLiteralDepth {
		return nil, fmt.Errorf("tuple literal evaluation depth exceeded %d - possible infinite loop", maxTupleLiteralDepth)
	}

	core.DebugLog("[TUPLE-LIT] tupleLiteralForm with %d args (depth: %d)\n", args.Len(), tupleLiteralDepth)
	result := make(core.TupleValue, 0, args.Len())

	// Evaluate each element
	for i, arg := range args.Items() {
		core.DebugLog("[TUPLE-LIT] Evaluating element %d: %T\n", i, arg)

		// Unwrap LocatedValue for the arg itself
		unwrappedArg := arg
		if located, ok := arg.(core.LocatedValue); ok {
			unwrappedArg = located.Unwrap()
		}

		// Check if this is an unpacking expression: (*unpack-iter expr)
		if list, ok := unwrappedArg.(*core.ListValue); ok && list.Len() == 2 {
			// Unwrap LocatedValue when checking for marker
			firstElem := list.Items()[0]
			if located, ok := firstElem.(core.LocatedValue); ok {
				firstElem = located.Unwrap()
			}
			if sym, ok := firstElem.(core.SymbolValue); ok && string(sym) == "*unpack-iter" {
				// Evaluate the expression to unpack
				val, err := Eval(list.Items()[1], ctx)
				if err != nil {
					return nil, err
				}
				// Unpack the iterable into the result
				items, err := convertIterableToSlice(val)
				if err != nil {
					return nil, fmt.Errorf("cannot unpack non-iterable: %v", err)
				}
				result = append(result, items...)
				continue
			}
		}

		// Regular element - evaluate and append
		val, err := Eval(arg, ctx)
		if err != nil {
			return nil, err
		}
		core.DebugLog("[TUPLE-LIT] Element %d evaluated to: %T\n", i, val)
		result = append(result, val)
	}

	core.DebugLog("[TUPLE-LIT] Returning tuple with %d elements\n", len(result))
	return result, nil
}
