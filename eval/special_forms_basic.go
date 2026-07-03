// Package eval provides basic special forms evaluation
package eval

import (
	"fmt"
	"strings"

	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/core"
)

// annotatedAssignForm implements annotated assignment (PEP 526): x: int = 5
// Syntax: (annotated-assign target annotation [value])
// Supports: variable (x: int = 5), attribute (obj.x: int = 5), subscript (obj[k]: int = 5)
func annotatedAssignForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() < 2 || args.Len() > 3 {
		return nil, &core.TypeError{Message: fmt.Sprintf("annotated-assign requires 2 or 3 arguments, got %d", args.Len())}
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
			return nil, fmt.Errorf("error evaluating annotation: %w", err)
		}
	default:
		// Try to evaluate it as an expression
		var err error
		annotationType, err = Eval(annotationVal, ctx)
		if err != nil {
			return nil, &core.TypeError{Message: fmt.Sprintf("annotation must be a string or symbol, got %v: %v", annotationVal.Type(), err)}
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
		annotationsDict.SetStr(targetName, annotationType)

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

	return nil, &core.TypeError{Message: fmt.Sprintf("annotated assignment target must be a symbol, attribute, or subscript, got %T", target)}
}

func convertIterableToSlice(iterable core.Value, ctx *core.Context) ([]core.Value, error) {
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
		// Try using the Go Iterator interface
		if iterableObj, ok := v.(core.Iterable); ok {
			iter := iterableObj.Iterator()
			for {
				val, hasNext := iter.Next()
				if !hasNext {
					break
				}
				items = append(items, val)
			}
			return items, nil
		}
		// Fall back to the Python iterator protocol (__iter__/__next__), so
		// comprehensions iterate custom classes, MutableMapping subclasses
		// (e.g. os.environ) and other instances exactly like a for loop does.
		if iter, found, err := types.CallIter(v, ctx); found {
			if err != nil {
				return nil, err
			}
			for {
				item, ok, err := types.CallNext(iter, ctx)
				if err != nil {
					if isStopIteration(err) {
						break
					}
					return nil, err
				}
				if !ok {
					break
				}
				items = append(items, item)
			}
			return items, nil
		}
		return nil, &core.TypeError{Message: fmt.Sprintf("value must be iterable, got %s", v.Type())}
	}
	return items, nil
}

// unpackTuplePattern unpacks a value according to a pattern like "(x, y)" or "(x, (y, z))"
// and binds the variables in the given context
func unpackTuplePattern(pattern string, value core.Value, ctx *core.Context) error {
	// Remove outer parentheses
	pattern = strings.TrimSpace(pattern)
	if !strings.HasPrefix(pattern, "(") || !strings.HasSuffix(pattern, ")") {
		return core.NewValueError(fmt.Sprintf("invalid tuple pattern: %s", pattern))
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
		return &core.TypeError{Message: fmt.Sprintf("cannot unpack non-sequence type %s", value.Type())}
	}

	// Check length matches
	if len(values) != len(varNames) {
		return core.NewValueError(fmt.Sprintf("cannot unpack %d values into %d variables", len(values), len(varNames)))
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
	// varName is either a plain identifier or a tuple pattern like "(x, y)";
	// decide once, not per element.
	isTuplePattern := strings.HasPrefix(varName, "(") && strings.HasSuffix(varName, ")")
	for _, item := range items {
		if isTuplePattern {
			// Parse the tuple pattern and unpack the item
			if err := unpackTuplePattern(varName, item, loopCtx); err != nil {
				return fmt.Errorf("error unpacking loop variable: %w", err)
			}
		} else {
			// Simple variable binding
			loopCtx.Define(varName, item)
		}

		// Check condition if present
		if condition != nil {
			condResult, err := Eval(condition, loopCtx)
			if err != nil {
				return fmt.Errorf("error evaluating condition: %w", err)
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

// compileCompExpr rewrites a single-clause comprehension body or condition so
// reads (and writes) of the comprehension variable become slot 0 of a one-slot
// frame, then compiles it to IR (see resolve.go / resolve_ir.go). This lets the
// per-element evaluation skip the map-scope Define/Lookup and the generic call
// machinery entirely. ok=false when the expression contains anything the
// resolver does not model (nested comprehensions, lambdas, keyword or unpack
// calls, assignments to other names, unmodeled special forms); the caller must
// then fall back to the generic comprehensionLoop, which preserves the old
// behavior exactly.
func compileCompExpr(expr core.Value, varName string) (core.Value, bool) {
	// A nested comprehension's innards (and its iterables) resolve names
	// through the map-scope chain at runtime, but the outer variable lives
	// only in this one-slot frame — invisible to that chain. If any nested
	// comprehension reads it, keep the whole comprehension on the generic
	// map-scope path.
	if nestedCompReadsName(expr, varName) {
		return nil, false
	}
	rewritten, ok := resolveBody(expr, map[string]int{varName: 0}, isSpecialFormName)
	if !ok {
		return nil, false
	}
	return compileIR(rewritten), true
}

// nestedCompReadsName reports whether a comprehension form nested anywhere
// inside v reads name as a free variable (i.e. not bound by the nested
// comprehension's own clause variables). Unanalyzable nested shapes answer
// true, which safely forces the generic path.
func nestedCompReadsName(v core.Value, name string) bool {
	v = unwrapLocated(v)
	lst, ok := v.(*core.ListValue)
	if !ok || lst.Len() == 0 {
		return false
	}
	items := lst.ItemsRef()
	if head, ok := unwrapLocated(items[0]).(core.SymbolValue); ok && compHeads[string(head)] {
		free := make(map[string]bool)
		if !walkNestedCompInnards(items, map[string]bool{}, free, isSpecialFormName) {
			return true
		}
		return free[name]
	}
	for _, it := range items {
		if nestedCompReadsName(it, name) {
			return true
		}
	}
	return false
}

// runCompLoopCompiled drives a single-clause comprehension whose body and
// optional condition were compiled by compileCompExpr. Per element it stores
// the item into the one-slot frame and evaluates the compiled IR; sink receives
// each produced value. Error wrapping mirrors the generic loop so messages are
// unchanged. Like comprehensionLoop, the frame context is shared across
// elements (each iteration rebinds the variable in the same scope).
func runCompLoopCompiled(items []core.Value, expr, cond core.Value, ctx *core.Context, sink func(core.Value) error) error {
	compCtx := core.NewContext(ctx)
	compCtx.Locals = core.NewSlotFrame(1)
	for _, item := range items {
		compCtx.Locals.Set(0, item)
		if cond != nil {
			condResult, err := Eval(cond, compCtx)
			if err != nil {
				return fmt.Errorf("error evaluating condition: %w", err)
			}
			if !core.IsTruthy(condResult) {
				continue
			}
		}
		exprResult, err := Eval(expr, compCtx)
		if err != nil {
			return fmt.Errorf("error evaluating expression: %w", err)
		}
		if err := sink(exprResult); err != nil {
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
		return nil, &core.TypeError{Message: "list-comp requires at least 2 arguments"}
	}

	argItems := args.ItemsRef()

	// Check if this is multi-clause format (argItems[1] is a list)
	if clausesList, ok := argItems[1].(*core.ListValue); ok {
		// Multi-clause nested comprehension (2 args: expr, clauses)
		if args.Len() != 2 {
			return nil, &core.TypeError{Message: "multi-clause list-comp requires exactly 2 arguments"}
		}
		return listCompMultiClause(argItems[0], clausesList, ctx)
	}

	// Single-clause format (backward compatible)
	// Requires 3-4 args: expr, var, iterable, [condition]
	if args.Len() < 3 || args.Len() > 4 {
		return nil, &core.TypeError{Message: "single-clause list-comp requires 3 or 4 arguments"}
	}

	// Get the variable name
	varSym, ok := argItems[1].(core.SymbolValue)
	if !ok {
		return nil, &core.TypeError{Message: "list comprehension variable must be a symbol"}
	}
	varName := string(varSym)

	// Evaluate the iterable
	iterableExpr := argItems[2]
	iterable, err := Eval(iterableExpr, ctx)
	if err != nil {
		// Debug: show what expression failed
		exprStr := core.PrintValue(iterableExpr)
		if len(exprStr) > 100 {
			exprStr = exprStr[:100] + "..."
		}
		return nil, fmt.Errorf("error evaluating iterable %s: %w", exprStr, err)
	}

	// Convert iterable to a sequence we can iterate over
	items, err := convertIterableToSlice(iterable, ctx)
	if err != nil {
		return nil, fmt.Errorf("list comprehension: %w", err)
	}

	expr := argItems[0]

	// Get optional condition
	var condition core.Value
	if args.Len() == 4 {
		condition = argItems[3]
	}

	// Create result list
	result := make([]core.Value, 0, len(items))

	// Fast path: compile the body and condition against a one-slot frame for
	// the comprehension variable (tuple patterns like "(x, y)" stay generic).
	if !strings.HasPrefix(varName, "(") {
		if compiledExpr, ok := compileCompExpr(expr, varName); ok {
			compiledCond, condOK := core.Value(nil), true
			if condition != nil {
				compiledCond, condOK = compileCompExpr(condition, varName)
			}
			if condOK {
				err := runCompLoopCompiled(items, compiledExpr, compiledCond, ctx, func(v core.Value) error {
					result = append(result, v)
					return nil
				})
				if err != nil {
					return nil, err
				}
				return core.NewList(result...), nil
			}
		}
	}

	// Generic path: map-scope loop.
	err = comprehensionLoop(items, varName, condition, ctx, func(loopCtx *core.Context) error {
		// Evaluate the expression
		exprResult, err := Eval(expr, loopCtx)
		if err != nil {
			return fmt.Errorf("error evaluating expression: %w", err)
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
			return nil, &core.TypeError{Message: fmt.Sprintf("clause %d must be a list", i)}
		}

		if clauseList.Len() < 2 || clauseList.Len() > 3 {
			return nil, &core.TypeError{Message: fmt.Sprintf("clause %d must have 2 or 3 elements (var, iter, [condition])", i)}
		}

		varSym, ok := clauseList.Items()[0].(core.SymbolValue)
		if !ok {
			return nil, &core.TypeError{Message: fmt.Sprintf("clause %d variable must be a symbol", i)}
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
				return fmt.Errorf("error evaluating expression: %w", err)
			}
			result = append(result, exprResult)
			return nil
		}

		// Evaluate the iterable for this clause
		c := clauses[clauseIdx]
		iterable, err := Eval(c.iterable, loopCtx)
		if err != nil {
			return fmt.Errorf("error evaluating iterable in clause %d: %w", clauseIdx, err)
		}

		// Convert to slice
		items, err := convertIterableToSlice(iterable, ctx)
		if err != nil {
			return fmt.Errorf("clause %d: %w", clauseIdx, err)
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
		return nil, &core.TypeError{Message: "dict-comp requires at least 3 arguments"}
	}

	argItems := args.ItemsRef()

	// Check if this is multi-clause format (argItems[2] is a list)
	if clausesList, ok := argItems[2].(*core.ListValue); ok {
		// Multi-clause nested comprehension (3 args: key-expr, value-expr, clauses)
		if args.Len() != 3 {
			return nil, &core.TypeError{Message: "multi-clause dict-comp requires exactly 3 arguments"}
		}
		return dictCompMultiClause(argItems[0], argItems[1], clausesList, ctx)
	}

	// Single-clause format (backward compatible)
	// Requires 4-5 args: key-expr, value-expr, var, iterable, [condition]
	if args.Len() < 4 || args.Len() > 5 {
		return nil, &core.TypeError{Message: "single-clause dict-comp requires 4 or 5 arguments"}
	}

	// Get the variable name
	varSym, ok := argItems[2].(core.SymbolValue)
	if !ok {
		return nil, &core.TypeError{Message: "dict comprehension variable must be a symbol"}
	}
	varName := string(varSym)

	// Evaluate the iterable
	iterable, err := Eval(argItems[3], ctx)
	if err != nil {
		return nil, fmt.Errorf("error evaluating iterable: %w", err)
	}

	// Convert iterable to a sequence we can iterate over
	items, err := convertIterableToSlice(iterable, ctx)
	if err != nil {
		return nil, fmt.Errorf("dict comprehension: %w", err)
	}

	keyExpr, valueExpr := argItems[0], argItems[1]

	// Create result dict
	result := core.NewDict()

	// Get optional condition
	var condition core.Value
	if args.Len() == 5 {
		condition = argItems[4]
	}

	// Execute the comprehension loop
	err = comprehensionLoop(items, varName, condition, ctx, func(loopCtx *core.Context) error {
		// Evaluate the key expression
		keyResult, err := Eval(keyExpr, loopCtx)
		if err != nil {
			return fmt.Errorf("error evaluating key expression: %w", err)
		}

		// Evaluate the value expression
		valueResult, err := Eval(valueExpr, loopCtx)
		if err != nil {
			return fmt.Errorf("error evaluating value expression: %w", err)
		}

		// Add to result dict (full semantics: hashability, user
		// __hash__/__eq__, equal-key dedup)
		if err := result.SetItem(keyResult, valueResult, loopCtx); err != nil {
			return err
		}
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
			return nil, &core.TypeError{Message: fmt.Sprintf("clause %d must be a list", i)}
		}

		if clauseList.Len() < 2 || clauseList.Len() > 3 {
			return nil, &core.TypeError{Message: fmt.Sprintf("clause %d must have 2 or 3 elements (var, iter, [condition])", i)}
		}

		varSym, ok := clauseList.Items()[0].(core.SymbolValue)
		if !ok {
			return nil, &core.TypeError{Message: fmt.Sprintf("clause %d variable must be a symbol", i)}
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
				return fmt.Errorf("error evaluating key expression: %w", err)
			}

			valueResult, err := Eval(valueExpr, loopCtx)
			if err != nil {
				return fmt.Errorf("error evaluating value expression: %w", err)
			}

			if err := result.SetItem(keyResult, valueResult, loopCtx); err != nil {
				return err
			}
			return nil
		}

		// Evaluate the iterable for this clause
		c := clauses[clauseIdx]
		iterable, err := Eval(c.iterable, loopCtx)
		if err != nil {
			return fmt.Errorf("error evaluating iterable in clause %d: %w", clauseIdx, err)
		}

		// Convert to slice
		items, err := convertIterableToSlice(iterable, ctx)
		if err != nil {
			return fmt.Errorf("clause %d: %w", clauseIdx, err)
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
		return nil, &core.TypeError{Message: "set-comp requires at least 2 arguments"}
	}

	argItems := args.ItemsRef()

	// Check if this is multi-clause format (argItems[1] is a list)
	if clausesList, ok := argItems[1].(*core.ListValue); ok {
		// Multi-clause nested comprehension (2 args: expr, clauses)
		if args.Len() != 2 {
			return nil, &core.TypeError{Message: "multi-clause set-comp requires exactly 2 arguments"}
		}
		return setCompMultiClause(argItems[0], clausesList, ctx)
	}

	// Single-clause format (backward compatible)
	// Requires 3-4 args: expr, var, iterable, [condition]
	if args.Len() < 3 || args.Len() > 4 {
		return nil, &core.TypeError{Message: "single-clause set-comp requires 3 or 4 arguments"}
	}

	// Get the variable name
	varSym, ok := argItems[1].(core.SymbolValue)
	if !ok {
		return nil, &core.TypeError{Message: "set comprehension variable must be a symbol"}
	}
	varName := string(varSym)

	// Evaluate the iterable
	iterableExpr := argItems[2]
	iterable, err := Eval(iterableExpr, ctx)
	if err != nil {
		// Debug: show what expression failed
		exprStr := core.PrintValue(iterableExpr)
		if len(exprStr) > 100 {
			exprStr = exprStr[:100] + "..."
		}
		return nil, fmt.Errorf("error evaluating iterable %s: %w", exprStr, err)
	}

	// Convert iterable to a sequence we can iterate over
	items, err := convertIterableToSlice(iterable, ctx)
	if err != nil {
		return nil, fmt.Errorf("set comprehension: %w", err)
	}

	expr := argItems[0]

	// Create result set
	result := core.NewSet()

	// Get optional condition
	var condition core.Value
	if args.Len() == 4 {
		condition = argItems[3]
	}

	// Fast path: compile the body and condition against a one-slot frame for
	// the comprehension variable (tuple patterns like "(x, y)" stay generic).
	if !strings.HasPrefix(varName, "(") {
		if compiledExpr, ok := compileCompExpr(expr, varName); ok {
			compiledCond, condOK := core.Value(nil), true
			if condition != nil {
				compiledCond, condOK = compileCompExpr(condition, varName)
			}
			if condOK {
				err := runCompLoopCompiled(items, compiledExpr, compiledCond, ctx, func(v core.Value) error {
					result.Add(v)
					return nil
				})
				if err != nil {
					return nil, err
				}
				return result, nil
			}
		}
	}

	// Generic path: map-scope loop.
	err = comprehensionLoop(items, varName, condition, ctx, func(loopCtx *core.Context) error {
		// Evaluate the expression
		exprResult, err := Eval(expr, loopCtx)
		if err != nil {
			return fmt.Errorf("error evaluating expression: %w", err)
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
			return nil, &core.TypeError{Message: fmt.Sprintf("clause %d must be a list", i)}
		}

		if clauseList.Len() < 2 || clauseList.Len() > 3 {
			return nil, &core.TypeError{Message: fmt.Sprintf("clause %d must have 2 or 3 elements (var, iter, [condition])", i)}
		}

		varSym, ok := clauseList.Items()[0].(core.SymbolValue)
		if !ok {
			return nil, &core.TypeError{Message: fmt.Sprintf("clause %d variable must be a symbol", i)}
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
				return fmt.Errorf("error evaluating expression: %w", err)
			}
			result.Add(exprResult)
			return nil
		}

		// Evaluate the iterable for this clause
		c := clauses[clauseIdx]
		iterable, err := Eval(c.iterable, loopCtx)
		if err != nil {
			return fmt.Errorf("error evaluating iterable in clause %d: %w", clauseIdx, err)
		}

		// Convert to slice
		items, err := convertIterableToSlice(iterable, ctx)
		if err != nil {
			return fmt.Errorf("clause %d: %w", clauseIdx, err)
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
			return nil, &core.TypeError{Message: fmt.Sprintf("clause %d must be a list", i)}
		}

		if clauseList.Len() < 2 || clauseList.Len() > 3 {
			return nil, &core.TypeError{Message: fmt.Sprintf("clause %d must have 2 or 3 elements (var, iter, [condition])", i)}
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
					return nil, &core.TypeError{Message: fmt.Sprintf("clause %d: variable list must contain symbols", i)}
				}
			}
		} else {
			return nil, &core.TypeError{Message: fmt.Sprintf("clause %d: variable must be a symbol or list of symbols", i)}
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
		return nil, &core.TypeError{Message: "gen-expr/gen-comp requires 2-4 arguments"}
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
				return nil, &core.TypeError{Message: "gen-comp lambda format requires 2 or 3 arguments"}
			}

			// Extract variable name(s) from lambda
			if lambdaList.Len() < 3 {
				return nil, core.NewValueError("invalid lambda in gen-comp")
			}
			params, ok := lambdaList.Items()[1].(*core.ListValue)
			if !ok || params.Len() != 1 {
				return nil, &core.TypeError{Message: "gen-comp lambda must have exactly one parameter"}
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
						return nil, &core.TypeError{Message: "gen-comp lambda tuple parameter must contain only symbols"}
					}
				}
			} else if varSym, ok := param.(core.SymbolValue); ok {
				// Single symbol parameter
				varName = string(varSym)
			} else {
				return nil, &core.TypeError{Message: fmt.Sprintf("gen-comp lambda parameter must be a symbol or list, got %T", param)}
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
		return nil, &core.TypeError{Message: "gen-expr old format requires 3 or 4 arguments"}
	}

	// Get the variable name
	varSym, ok := args.Items()[1].(core.SymbolValue)
	if !ok {
		return nil, &core.TypeError{Message: "generator expression variable must be a symbol"}
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
				items, err := convertIterableToSlice(val, ctx)
				if err != nil {
					return nil, &core.TypeError{Message: fmt.Sprintf("cannot unpack non-iterable: %v", err)}
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
		return nil, &core.EvalError{Type: "RecursionError", Message: fmt.Sprintf("tuple literal evaluation depth exceeded %d - possible infinite loop", maxTupleLiteralDepth)}
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
				items, err := convertIterableToSlice(val, ctx)
				if err != nil {
					return nil, &core.TypeError{Message: fmt.Sprintf("cannot unpack non-iterable: %v", err)}
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
