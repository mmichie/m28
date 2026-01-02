// Package eval provides pattern matching special forms
package eval

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// MatchForm implements the match special form for Python 3.10+ pattern matching
// Syntax: (match subject (case pattern [guard] body...) ...)
// Where:
//   - subject: The value to match against
//   - pattern: Can be literal, identifier (variable binding), _ (wildcard), or class pattern
//   - guard: Optional condition that must be true for the case to match
//   - body: Statements to execute if the case matches
func MatchForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() < 2 {
		return nil, &core.TypeError{Message: "match requires at least a subject and one case"}
	}

	// Evaluate the subject once
	subject, err := Eval(args.Items()[0], ctx)
	if err != nil {
		return nil, err
	}

	// Process each case clause
	for i := 1; i < args.Len(); i++ {
		caseClause := args.Items()[i]

		// Unwrap LocatedValue if present
		if located, ok := caseClause.(core.LocatedValue); ok {
			caseClause = located.Unwrap()
		}

		caseList, ok := caseClause.(*core.ListValue)
		if !ok || caseList.Len() < 3 {
			return nil, &core.TypeError{Message: fmt.Sprintf("case clause must be a list with at least 3 elements, got %T with len %d", caseClause, caseList.Len())}
		}

		// First element should be the "case" symbol
		firstElem := caseList.Items()[0]
		if located, ok := firstElem.(core.LocatedValue); ok {
			firstElem = located.Unwrap()
		}
		if sym, ok := firstElem.(core.SymbolValue); !ok || string(sym) != "case" {
			return nil, &core.TypeError{Message: "case clause must start with 'case'"}
		}

		// Second element is the pattern
		pattern := caseList.Items()[1]
		if located, ok := pattern.(core.LocatedValue); ok {
			pattern = located.Unwrap()
		}

		// Remaining elements are guard (optional) and body
		bodyStart := 2
		var guard core.Value

		// Check if there's a guard (indicated by "if" symbol followed by condition)
		if caseList.Len() > 3 {
			potentialIf := caseList.Items()[2]
			if located, ok := potentialIf.(core.LocatedValue); ok {
				potentialIf = located.Unwrap()
			}
			if sym, ok := potentialIf.(core.SymbolValue); ok && string(sym) == "if" {
				if caseList.Len() > 4 {
					guard = caseList.Items()[3]
					bodyStart = 4
				}
			}
		}

		// Create a new context for pattern matching (bindings are local to the case)
		caseCtx := core.NewContext(ctx)

		// Try to match the pattern
		matched, err := matchPattern(subject, pattern, caseCtx)
		if err != nil {
			return nil, err
		}

		if matched {
			// Check guard if present
			if guard != nil {
				guardResult, err := Eval(guard, caseCtx)
				if err != nil {
					return nil, err
				}
				if !core.IsTruthy(guardResult) {
					continue // Guard failed, try next case
				}
			}

			// Execute the body
			var result core.Value = core.Nil
			for j := bodyStart; j < caseList.Len(); j++ {
				result, err = Eval(caseList.Items()[j], caseCtx)
				if err != nil {
					return nil, err
				}
				// Handle control flow
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
			return result, nil
		}
	}

	// No case matched - return None (Python semantics)
	return core.Nil, nil
}

// matchPattern attempts to match a value against a pattern
// Returns true if the pattern matches, false otherwise
// For variable patterns, binds the value to the variable in ctx
func matchPattern(value core.Value, pattern core.Value, ctx *core.Context) (bool, error) {
	// Handle nil pattern
	if pattern == nil {
		return value == nil || value == core.Nil, nil
	}

	// Unwrap LocatedValue
	if located, ok := pattern.(core.LocatedValue); ok {
		pattern = located.Unwrap()
	}

	// Handle nil after unwrapping
	if pattern == nil {
		return value == nil || value == core.Nil, nil
	}

	switch p := pattern.(type) {
	case core.SymbolValue:
		name := string(p)

		// Wildcard pattern - always matches, no binding
		if name == "_" {
			return true, nil
		}

		// Check if it's a known constant (True, False, None)
		switch name {
		case "True":
			return value == core.True, nil
		case "False":
			return value == core.False, nil
		case "None":
			return value == core.Nil || value == core.None, nil
		}

		// Variable pattern - bind the value and always match
		ctx.Define(name, value)
		return true, nil

	case core.NumberValue:
		// Literal number pattern
		if v, ok := value.(core.NumberValue); ok {
			return float64(v) == float64(p), nil
		}
		return false, nil

	case core.StringValue:
		// Literal string pattern
		if v, ok := value.(core.StringValue); ok {
			return string(v) == string(p), nil
		}
		return false, nil

	case core.BoolValue:
		// Literal bool pattern
		if v, ok := value.(core.BoolValue); ok {
			return bool(v) == bool(p), nil
		}
		return false, nil

	case *core.ListValue:
		// Could be:
		// - Tuple/list pattern: (tuple-literal p1 p2 ...)
		// - Or pattern: (or p1 p2 ...)
		// - Class pattern: (ClassName p1 p2 ...)
		// - Sequence pattern: [p1, p2, ...]

		if p.Len() == 0 {
			// Empty list pattern matches empty list
			if list, ok := value.(*core.ListValue); ok {
				return list.Len() == 0, nil
			}
			return false, nil
		}

		firstElem := p.Items()[0]
		if located, ok := firstElem.(core.LocatedValue); ok {
			firstElem = located.Unwrap()
		}

		if sym, ok := firstElem.(core.SymbolValue); ok {
			symStr := string(sym)

			switch symStr {
			case "or":
				// Or pattern: (or p1 p2 ...)
				for i := 1; i < p.Len(); i++ {
					matched, err := matchPattern(value, p.Items()[i], ctx)
					if err != nil {
						return false, err
					}
					if matched {
						return true, nil
					}
				}
				return false, nil

			case "as":
				// As pattern: (as pattern name)
				// Match the pattern AND bind the value to the name
				if p.Len() != 3 {
					return false, nil
				}
				subPattern := p.Items()[1]
				nameVal := p.Items()[2]
				if located, ok := nameVal.(core.LocatedValue); ok {
					nameVal = located.Unwrap()
				}
				nameSym, ok := nameVal.(core.SymbolValue)
				if !ok {
					return false, nil
				}
				// First match the pattern
				matched, err := matchPattern(value, subPattern, ctx)
				if err != nil {
					return false, err
				}
				if matched {
					// Bind the entire value to the name
					ctx.Define(string(nameSym), value)
				}
				return matched, nil

			case "tuple-literal":
				// Tuple pattern: (tuple-literal p1 p2 ...) with optional star pattern
				tuple, ok := value.(core.TupleValue)
				if !ok {
					return false, nil
				}
				return matchSequencePattern([]core.Value(tuple), p.Items()[1:], ctx)

			case "list-literal":
				// List pattern: (list-literal p1 p2 ...) with optional star pattern
				list, ok := value.(*core.ListValue)
				if !ok {
					return false, nil
				}
				return matchSequencePattern(list.Items(), p.Items()[1:], ctx)

			case "dict-literal":
				// Dict pattern: (dict-literal key1 pattern1 key2 pattern2 ...)
				dict, ok := value.(*core.DictValue)
				if !ok {
					return false, nil
				}
				return matchDictPattern(dict, p.Items()[1:], ctx)

			default:
				// Could be a class pattern: (ClassName args...)
				// For now, check if it's a class in the context
				classVal, err := ctx.Lookup(symStr)
				if err == nil {
					if class, ok := classVal.(*core.Class); ok {
						return matchClassPattern(value, class, p, ctx)
					}
				}
				// Not a recognized pattern
				return false, nil
			}
		}

		return false, nil

	case core.TupleValue:
		// Tuple pattern matches tuple values
		tuple, ok := value.(core.TupleValue)
		if !ok {
			return false, nil
		}
		if len(tuple) != len(p) {
			return false, nil
		}
		for i := range p {
			matched, err := matchPattern(tuple[i], p[i], ctx)
			if err != nil {
				return false, err
			}
			if !matched {
				return false, nil
			}
		}
		return true, nil

	default:
		// For other types, try equality comparison
		return core.Compare(value, pattern) == 0, nil
	}
}

// matchClassPattern matches a class pattern like Point(x, y) against a value
func matchClassPattern(value core.Value, class *core.Class, pattern *core.ListValue, ctx *core.Context) (bool, error) {
	// Value must be an instance of the class
	inst, ok := value.(*core.Instance)
	if !ok {
		return false, nil
	}

	// Check if instance is of the right class (or a subclass)
	if !core.IsInstanceOf(inst, class) {
		return false, nil
	}

	// For now, support positional arguments that map to __match_args__ or common attributes
	// In Python, class patterns use __match_args__ to determine positional argument names
	// For simplicity, we'll check if the class has __match_args__ attribute

	// Get the patterns for arguments (skip the class name at index 0)
	argPatterns := pattern.Items()[1:]
	if len(argPatterns) == 0 {
		// No arguments - just check the class
		return true, nil
	}

	// Try to get __match_args__ from the class
	var matchArgs []string
	if matchArgsVal, hasMatchArgs := class.GetAttr("__match_args__"); hasMatchArgs {
		if matchArgsTuple, ok := matchArgsVal.(core.TupleValue); ok {
			for _, arg := range matchArgsTuple {
				if strArg, ok := arg.(core.StringValue); ok {
					matchArgs = append(matchArgs, string(strArg))
				}
			}
		}
	}

	// If no __match_args__, we can't do positional matching
	if len(matchArgs) == 0 {
		return false, nil
	}

	// Check we have the right number of arguments
	if len(argPatterns) > len(matchArgs) {
		return false, nil
	}

	// Match each argument pattern against the corresponding attribute
	for i, argPattern := range argPatterns {
		attrName := matchArgs[i]
		attrVal, hasAttr := inst.GetAttr(attrName)
		if !hasAttr {
			return false, nil
		}
		matched, err := matchPattern(attrVal, argPattern, ctx)
		if err != nil {
			return false, err
		}
		if !matched {
			return false, nil
		}
	}

	return true, nil
}

// matchSequencePattern matches a sequence (list or tuple) against patterns
// Handles star patterns like [a, *rest, b]
func matchSequencePattern(values []core.Value, patterns []core.Value, ctx *core.Context) (bool, error) {
	// Find if there's a star pattern and where
	starIndex := -1
	for i, pat := range patterns {
		if isStarPattern(pat) {
			if starIndex != -1 {
				// Multiple star patterns - error
				return false, &core.TypeError{Message: "multiple starred expressions in pattern"}
			}
			starIndex = i
		}
	}

	if starIndex == -1 {
		// No star pattern - exact match required
		if len(values) != len(patterns) {
			return false, nil
		}
		for i, pat := range patterns {
			matched, err := matchPattern(values[i], pat, ctx)
			if err != nil {
				return false, err
			}
			if !matched {
				return false, nil
			}
		}
		return true, nil
	}

	// Has a star pattern
	beforeCount := starIndex
	afterCount := len(patterns) - starIndex - 1
	minRequired := beforeCount + afterCount

	if len(values) < minRequired {
		return false, nil
	}

	// Match patterns before the star
	for i := 0; i < beforeCount; i++ {
		matched, err := matchPattern(values[i], patterns[i], ctx)
		if err != nil {
			return false, err
		}
		if !matched {
			return false, nil
		}
	}

	// Match patterns after the star
	for i := 0; i < afterCount; i++ {
		valueIdx := len(values) - afterCount + i
		patternIdx := starIndex + 1 + i
		matched, err := matchPattern(values[valueIdx], patterns[patternIdx], ctx)
		if err != nil {
			return false, err
		}
		if !matched {
			return false, nil
		}
	}

	// Bind the star pattern
	starPat := patterns[starIndex]
	starName := getStarPatternName(starPat)
	if starName != "" && starName != "_" {
		// Collect the middle elements
		middleStart := beforeCount
		middleEnd := len(values) - afterCount
		middleValues := values[middleStart:middleEnd]
		ctx.Define(starName, core.NewList(middleValues...))
	}

	return true, nil
}

// isStarPattern checks if a pattern is a star pattern (star name)
func isStarPattern(pat core.Value) bool {
	if located, ok := pat.(core.LocatedValue); ok {
		pat = located.Unwrap()
	}
	list, ok := pat.(*core.ListValue)
	if !ok || list.Len() != 2 {
		return false
	}
	first := list.Items()[0]
	if located, ok := first.(core.LocatedValue); ok {
		first = located.Unwrap()
	}
	if sym, ok := first.(core.SymbolValue); ok {
		return string(sym) == "star"
	}
	return false
}

// getStarPatternName gets the name from a star pattern
func getStarPatternName(pat core.Value) string {
	if located, ok := pat.(core.LocatedValue); ok {
		pat = located.Unwrap()
	}
	list, ok := pat.(*core.ListValue)
	if !ok || list.Len() != 2 {
		return ""
	}
	second := list.Items()[1]
	if located, ok := second.(core.LocatedValue); ok {
		second = located.Unwrap()
	}
	if sym, ok := second.(core.SymbolValue); ok {
		return string(sym)
	}
	return ""
}

// matchDictPattern matches a dict against a dict pattern
// Pattern format: key1, pattern1, key2, pattern2, ...
func matchDictPattern(dict *core.DictValue, patterns []core.Value, ctx *core.Context) (bool, error) {
	if len(patterns)%2 != 0 {
		return false, &core.TypeError{Message: "dict pattern must have even number of key-pattern pairs"}
	}

	// For each key-pattern pair in the pattern
	for i := 0; i < len(patterns); i += 2 {
		keyPattern := patterns[i]
		valuePattern := patterns[i+1]

		// Unwrap the key
		if located, ok := keyPattern.(core.LocatedValue); ok {
			keyPattern = located.Unwrap()
		}

		// Evaluate the key to get the actual key value
		// For pattern matching, keys should be literals (string, number, etc.)
		var keyValue core.Value
		switch k := keyPattern.(type) {
		case core.StringValue:
			keyValue = k
		case core.NumberValue:
			keyValue = k
		case core.BoolValue:
			keyValue = k
		case core.SymbolValue:
			// Could be a variable or a constant
			name := string(k)
			switch name {
			case "True":
				keyValue = core.True
			case "False":
				keyValue = core.False
			case "None":
				keyValue = core.Nil
			default:
				// Try to look up as a variable
				val, err := ctx.Lookup(name)
				if err != nil {
					return false, fmt.Errorf("undefined key in dict pattern: %s", name)
				}
				keyValue = val
			}
		default:
			// For other types, try to evaluate
			return false, fmt.Errorf("unsupported key type in dict pattern: %T", keyPattern)
		}

		// Get the value from the dict
		val, found := dict.GetValue(keyValue)
		if !found {
			return false, nil
		}

		// Match the value against the pattern
		matched, err := matchPattern(val, valuePattern, ctx)
		if err != nil {
			return false, err
		}
		if !matched {
			return false, nil
		}
	}

	return true, nil
}
