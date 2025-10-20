package eval

import (
	"fmt"
	"regexp"
	"strings"
	"sync"

	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/parser"
)

// gensymCounter tracks unique IDs for gensym generation
var (
	gensymCounter int
	gensymMutex   sync.Mutex
)

// sStringForm implements the s-string special form for code generation
// AST structure: (s-string template-string (type expr) (type expr) ...)
// Types: "value", "code", "splice", "dict-splice", "gensym"
func sStringForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() < 1 {
		return nil, fmt.Errorf("s-string requires at least a template string")
	}

	// Get the template string
	template, ok := args.Items()[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("s-string: first argument must be a string, got %v", args.Items()[0].Type())
	}

	// If there are no interpolations, just parse and return the template
	if args.Len() == 1 {
		result, err := parseTemplate(string(template))
		if err != nil {
			return nil, fmt.Errorf("s-string: error parsing template: %v", err)
		}
		return result, nil
	}

	// Create gensym context for this s-string
	gensymMap := make(map[string]string)

	// Process each interpolation
	interpolatedValues := make(map[int]string, args.Len()-1)
	splicedLists := make(map[int]*core.ListValue)

	for i, interpArg := range args.Items()[1:] {
		// Each interpolation is a list: (type expr)
		interpList, ok := interpArg.(*core.ListValue)
		if !ok || interpList.Len() != 2 {
			return nil, fmt.Errorf("s-string: invalid interpolation format at position %d", i)
		}

		// Get interpolation type
		interpType, ok := interpList.Items()[0].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("s-string: interpolation type must be a string at position %d", i)
		}

		// Get expression to interpolate
		expr := interpList.Items()[1]

		// Handle different interpolation types
		switch string(interpType) {
		case "value":
			// Evaluate expression and convert to code representation
			val, err := Eval(expr, ctx)
			if err != nil {
				return nil, fmt.Errorf("s-string: error evaluating value interpolation: %v", err)
			}
			interpolatedValues[i] = valueToCode(val)

		case "code":
			// Insert expression as code (quoted)
			interpolatedValues[i] = exprToCode(expr)

		case "splice":
			// Evaluate expression and prepare to splice into list
			val, err := Eval(expr, ctx)
			if err != nil {
				return nil, fmt.Errorf("s-string: error evaluating splice interpolation: %v", err)
			}

			// Convert value to list for splicing
			listVal, err := valueToList(val)
			if err != nil {
				return nil, fmt.Errorf("s-string: splice requires a sequence, got %v: %v", val.Type(), err)
			}
			splicedLists[i] = listVal
			interpolatedValues[i] = "" // Placeholder, will be handled during template expansion

		case "dict-splice":
			// Dict splicing not yet supported in template expansion
			return nil, fmt.Errorf("s-string: dict-splice not yet implemented")

		case "gensym":
			// Generate unique symbol
			sym, ok := expr.(core.SymbolValue)
			if !ok {
				return nil, fmt.Errorf("s-string: gensym requires a symbol, got %v", expr.Type())
			}
			interpolatedValues[i] = generateGensym(string(sym), gensymMap)

		default:
			return nil, fmt.Errorf("s-string: unknown interpolation type: %s", interpType)
		}
	}

	// Expand template with interpolated values
	expandedCode, err := expandTemplate(string(template), interpolatedValues, splicedLists)
	if err != nil {
		return nil, fmt.Errorf("s-string: error expanding template: %v", err)
	}

	// Parse the expanded code as M28 expressions
	result, err := parseTemplate(expandedCode)
	if err != nil {
		return nil, fmt.Errorf("s-string: error parsing expanded template: %v", err)
	}

	return result, nil
}

// sStringRawForm handles raw s-strings (no interpolation)
// AST structure: (s-string-raw literal-string)
// Raw s-strings return the literal string value, not parsed code
func sStringRawForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() != 1 {
		return nil, fmt.Errorf("s-string-raw requires exactly 1 argument")
	}

	// Get the literal string
	literal, ok := args.Items()[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("s-string-raw: argument must be a string, got %v", args.Items()[0].Type())
	}

	// Return the literal string as-is (no parsing, no interpolation)
	return literal, nil
}

// valueToCode converts a runtime value to its code representation
func valueToCode(val core.Value) string {
	switch v := val.(type) {
	case core.NumberValue:
		return fmt.Sprintf("%v", v)
	case core.StringValue:
		// Quote string values
		return fmt.Sprintf("%q", string(v))
	case core.BoolValue:
		if bool(v) {
			return "true"
		}
		return "false"
	case core.NilValue:
		return "nil"
	case core.SymbolValue:
		// Symbols need to be quoted in code
		return fmt.Sprintf("'%s", string(v))
	case *core.ListValue:
		// Convert list to code representation
		elements := make([]string, v.Len())
		for i, elem := range v.Items() {
			elements[i] = valueToCode(elem)
		}
		return fmt.Sprintf("(%s)", strings.Join(elements, " "))
	default:
		// For other types, use string representation
		return fmt.Sprintf("%v", val)
	}
}

// exprToCode converts an expression AST to its code representation
func exprToCode(expr core.Value) string {
	switch v := expr.(type) {
	case core.SymbolValue:
		return string(v)
	case core.NumberValue, core.StringValue, core.BoolValue, core.NilValue:
		return valueToCode(v)
	case *core.ListValue:
		elements := make([]string, v.Len())
		for i, elem := range v.Items() {
			elements[i] = exprToCode(elem)
		}
		return fmt.Sprintf("(%s)", strings.Join(elements, " "))
	default:
		return fmt.Sprintf("%v", expr)
	}
}

// valueToList converts a value to a list for splicing
func valueToList(val core.Value) (*core.ListValue, error) {
	switch v := val.(type) {
	case *core.ListValue:
		return v, nil
	case core.TupleValue:
		return core.NewList(v...), nil
	case *core.SetValue:
		// Convert set to list using iterator
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
		// Try to use iterator
		if iterable, ok := val.(core.Iterable); ok {
			list := make([]core.Value, 0)
			iter := iterable.Iterator()
			for {
				item, hasNext := iter.Next()
				if !hasNext {
					break
				}
				list = append(list, item)
			}
			return core.NewList(list...), nil
		}
		return nil, fmt.Errorf("cannot convert %v to list", val.Type())
	}
}

// generateGensym creates a unique symbol name
func generateGensym(base string, gensymMap map[string]string) string {
	// Check if we already generated a gensym for this base in this context
	if existing, ok := gensymMap[base]; ok {
		return existing
	}

	// Generate new unique symbol
	gensymMutex.Lock()
	gensymCounter++
	unique := fmt.Sprintf("%s_%d", base, gensymCounter)
	gensymMutex.Unlock()

	gensymMap[base] = unique
	return unique
}

// expandTemplate replaces placeholders in template with interpolated values
func expandTemplate(template string, values map[int]string, spliced map[int]*core.ListValue) (string, error) {
	// Pattern to match placeholders like {0}, {1}, etc.
	re := regexp.MustCompile(`\{(\d+)\}`)

	// Track if we're inside a list context for proper splicing
	result := re.ReplaceAllStringFunc(template, func(match string) string {
		// Extract the index
		var idx int
		fmt.Sscanf(match, "{%d}", &idx)

		// Check if this is a splice
		if splicedList, ok := spliced[idx]; ok {
			// Convert list elements to code strings
			elements := make([]string, splicedList.Len())
			for i, elem := range splicedList.Items() {
				elements[i] = valueToCode(elem)
			}
			// Splice elements directly (no outer parens)
			return strings.Join(elements, " ")
		}

		// Regular value or code interpolation
		if val, ok := values[idx]; ok {
			return val
		}

		// Should not happen if parser is correct
		return match
	})

	return result, nil
}

// parseTemplate parses a string as M28 code and returns the AST
func parseTemplate(code string) (core.Value, error) {
	p := parser.NewParser()
	result, err := p.Parse(code)
	if err != nil {
		return nil, err
	}
	return result, nil
}

// RegisterSStringForms registers s-string special forms
func RegisterSStringForms() {
	RegisterSpecialForm("s-string", sStringForm)
	RegisterSpecialForm("s-string-raw", sStringRawForm)
}
