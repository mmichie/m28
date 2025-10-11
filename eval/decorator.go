package eval

import (
	"fmt"
	"strings"

	"github.com/mmichie/m28/core"
)

// decoratorForm handles decorator application: (@decorator form)
// Evaluates form first, then applies decorator to the result
func decoratorForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("decorator requires at least a decorator and a form")
	}

	// First argument should be the decorator symbol (starting with @)
	decoratorSym, ok := args[0].(core.SymbolValue)
	if !ok {
		return nil, fmt.Errorf("decorator: first argument must be a symbol, got %v", args[0].Type())
	}

	decoratorName := string(decoratorSym)
	if !strings.HasPrefix(decoratorName, "@") {
		return nil, fmt.Errorf("decorator: symbol must start with @, got %s", decoratorName)
	}

	// Get the actual decorator function name (without @)
	actualDecoratorName := decoratorName[1:]

	// Evaluate the form to be decorated (usually a def)
	// This will define the function and return it
	decorated, err := Eval(args[1], ctx)
	if err != nil {
		return nil, fmt.Errorf("error evaluating decorated form: %v", err)
	}

	// Look up the decorator function
	decoratorFunc, err := ctx.Lookup(actualDecoratorName)
	if err != nil {
		return nil, fmt.Errorf("decorator '%s' not found: %v", actualDecoratorName, err)
	}

	// Decorator must be callable
	callable, ok := decoratorFunc.(core.Callable)
	if !ok {
		return nil, fmt.Errorf("decorator '%s' is not callable", actualDecoratorName)
	}

	// Call the decorator with the decorated value
	result, err := callable.Call([]core.Value{decorated}, ctx)
	if err != nil {
		return nil, fmt.Errorf("error applying decorator '%s': %v", actualDecoratorName, err)
	}

	// If the decorated form was a function definition, we need to re-assign it
	// Check if the original form was a (def name ...) form
	if defForm, ok := args[1].(core.ListValue); ok && len(defForm) > 0 {
		if sym, ok := defForm[0].(core.SymbolValue); ok && string(sym) == "def" {
			// Extract the function name from the def form
			var funcName string
			if len(defForm) > 1 {
				// Check different def forms
				if nameSym, ok := defForm[1].(core.SymbolValue); ok {
					// (def name ...)
					funcName = string(nameSym)
				} else if nameList, ok := defForm[1].(core.ListValue); ok && len(nameList) > 0 {
					// (def (name args) ...)
					if nameSym, ok := nameList[0].(core.SymbolValue); ok {
						funcName = string(nameSym)
					}
				}
			}

			// Re-assign the decorated function
			if funcName != "" {
				ctx.Define(funcName, result)
			}
		}
	}

	return result, nil
}

// RegisterDecoratorForms registers decorator-related special forms
func RegisterDecoratorForms() {
	// Register the decorator application form
	// When we see (@decorator form), this handler is called
	// Note: We can't pre-register all @-prefixed symbols, so we check at eval time
}

// isDecoratorForm checks if a list is a decorator application
func isDecoratorForm(list core.ListValue) bool {
	if len(list) < 2 {
		return false
	}

	// Check if first element is a symbol starting with @
	if sym, ok := list[0].(core.SymbolValue); ok {
		return strings.HasPrefix(string(sym), "@")
	}

	return false
}

// evalDecoratorForm evaluates a decorator form
func evalDecoratorForm(list core.ListValue, ctx *core.Context) (core.Value, error) {
	return decoratorForm(list, ctx)
}

// isMacroCall checks if a symbol refers to a macro
func isMacroCall(sym core.SymbolValue, ctx *core.Context) bool {
	// Look up the symbol
	val, err := ctx.Lookup(string(sym))
	if err != nil {
		return false
	}

	// Check if it has __macro__ attribute set to true
	if obj, ok := val.(interface {
		GetAttr(string) (core.Value, bool)
	}); ok {
		if macroAttr, ok := obj.GetAttr("__macro__"); ok {
			if boolVal, ok := macroAttr.(core.BoolValue); ok {
				return bool(boolVal)
			}
		}
	}

	return false
}

// evalMacroCall evaluates a macro call by expanding it first
func evalMacroCall(list core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(list) == 0 {
		return nil, fmt.Errorf("empty macro call")
	}

	// Get the macro function
	macroSym, ok := list[0].(core.SymbolValue)
	if !ok {
		return nil, fmt.Errorf("macro name must be a symbol")
	}

	macroFunc, err := ctx.Lookup(string(macroSym))
	if err != nil {
		return nil, fmt.Errorf("macro not found: %s", macroSym)
	}

	// Macro must be callable
	callable, ok := macroFunc.(core.Callable)
	if !ok {
		return nil, fmt.Errorf("macro is not callable: %s", macroSym)
	}

	// Call the macro with UNEVALUATED arguments
	// This is the key difference from regular functions
	args := make([]core.Value, len(list)-1)
	for i, arg := range list[1:] {
		args[i] = arg // Pass raw AST, not evaluated
	}

	// Expand the macro
	expanded, err := callable.Call(args, ctx)
	if err != nil {
		return nil, fmt.Errorf("error expanding macro %s: %v", macroSym, err)
	}

	// Now evaluate the expanded form
	result, err := Eval(expanded, ctx)
	if err != nil {
		return nil, fmt.Errorf("error evaluating macro expansion: %v", err)
	}

	return result, nil
}
