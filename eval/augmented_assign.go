package eval

import (
	"fmt"
	"strings"

	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/core"
)

// augmentedOps maps augmented assignment operators to their base operators
var augmentedOps = map[string]string{
	"+=":  "+",
	"-=":  "-",
	"*=":  "*",
	"/=":  "/",
	"//=": "//",
	"%=":  "%",
	"**=": "**",
	"&=":  "&",
	"|=":  "|",
	"^=":  "^",
	"<<=": "<<",
	">>=": ">>",
}

// augmentedDunderMethods maps augmented operators to their in-place dunder methods
var augmentedDunderMethods = map[string]string{
	"+=":  "__iadd__",
	"-=":  "__isub__",
	"*=":  "__imul__",
	"/=":  "__itruediv__",
	"//=": "__ifloordiv__",
	"%=":  "__imod__",
	"**=": "__ipow__",
	"&=":  "__iand__",
	"|=":  "__ior__",
	"^=":  "__ixor__",
	"<<=": "__ilshift__",
	">>=": "__irshift__",
}

// performAugmentedOp performs an augmented operation, trying in-place dunder method first
func performAugmentedOp(op string, currentValue, augValue core.Value, ctx *core.Context) (core.Value, error) {
	// Try in-place dunder method first (e.g., __iadd__)
	if dunderMethod, ok := augmentedDunderMethods[op]; ok {
		if result, found, err := types.CallDunder(currentValue, dunderMethod, []core.Value{augValue}, ctx); found {
			return result, err
		}
	}

	// Fall back to regular operation (e.g., __add__)
	baseOp, ok := augmentedOps[op]
	if !ok {
		return nil, fmt.Errorf("unknown augmented assignment operator: %s", op)
	}

	// Create the operation expression and evaluate it
	opExpr := core.NewList(
		core.SymbolValue(baseOp),
		currentValue,
		augValue,
	)
	return Eval(opExpr, ctx)
}

// AugmentedAssignForm handles augmented assignment operators
// (+= x y) -> tries __iadd__ first, then falls back to (= x (+ x y))
func AugmentedAssignForm(op string, args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() != 2 {
		return nil, fmt.Errorf("%s requires exactly 2 arguments, got %d", op, args.Len())
	}

	target := args.Items()[0]
	augValue := args.Items()[1]

	// Handle different target types
	switch t := target.(type) {
	case core.SymbolValue:
		// Simple variable: x += y
		// First get current value
		currentValue, err := ctx.Lookup(string(t))
		if err != nil {
			return nil, fmt.Errorf("undefined variable: %s", string(t))
		}

		// Evaluate the augment value
		evalAugValue, err := Eval(augValue, ctx)
		if err != nil {
			return nil, err
		}

		// Perform the augmented operation (tries __iadd__ first, then falls back)
		result, err := performAugmentedOp(op, currentValue, evalAugValue, ctx)
		if err != nil {
			return nil, err
		}

		// Assign the result
		ctx.Define(string(t), result)
		// Python assignments are statements and return None
		return core.None, nil

	case *core.ListValue:
		// Could be indexing or dot notation
		if t.Len() >= 3 {
			if sym, ok := t.Items()[0].(core.SymbolValue); ok {
				switch string(sym) {
				case "get-item":
					// Index assignment: lst[i] += y
					return augmentedIndexAssign(t, op, augValue, ctx)
				case ".":
					// Property assignment: obj.prop += y
					return augmentedPropertyAssign(t, op, augValue, ctx)
				}
			}
		}
		return nil, fmt.Errorf("invalid assignment target: %v", t)

	default:
		return nil, fmt.Errorf("cannot assign to %s", target.Type())
	}
}

// augmentedIndexAssign handles lst[i] += value
func augmentedIndexAssign(indexExpr *core.ListValue, op string, augValue core.Value, ctx *core.Context) (core.Value, error) {
	// indexExpr is (get-item obj key)
	if indexExpr.Len() != 3 {
		return nil, fmt.Errorf("invalid index expression")
	}

	items := indexExpr.Items()
	// Get current value
	currentValue, err := GetItemForm(core.NewList(items[1], items[2]), ctx)
	if err != nil {
		return nil, err
	}

	// Evaluate augment value
	evalAugValue, err := Eval(augValue, ctx)
	if err != nil {
		return nil, err
	}

	// Perform the augmented operation (tries __iadd__ first, then falls back)
	result, err := performAugmentedOp(op, currentValue, evalAugValue, ctx)
	if err != nil {
		return nil, err
	}

	// Set the new value
	return SetItemForm(core.NewList(items[1], items[2], result), ctx)
}

// augmentedPropertyAssign handles obj.prop += value
func augmentedPropertyAssign(dotExpr *core.ListValue, op string, augValue core.Value, ctx *core.Context) (core.Value, error) {
	// dotExpr is (. obj "prop")
	if dotExpr.Len() < 3 {
		return nil, fmt.Errorf("invalid property expression")
	}

	// Get current value
	currentValue, err := DotForm(dotExpr, ctx)
	if err != nil {
		return nil, err
	}

	// Evaluate augment value
	evalAugValue, err := Eval(augValue, ctx)
	if err != nil {
		return nil, err
	}

	// Perform the augmented operation (tries __iadd__ first, then falls back)
	result, err := performAugmentedOp(op, currentValue, evalAugValue, ctx)
	if err != nil {
		return nil, err
	}

	// Create assignment expression and evaluate it
	assignExpr := core.NewList(
		core.SymbolValue("="),
		dotExpr,
		result,
	)
	return AssignForm(core.NewList(assignExpr.Items()[1:]...), ctx)
}

// RegisterAugmentedAssignment registers all augmented assignment operators
func RegisterAugmentedAssignment() {
	for op := range augmentedOps {
		// Capture op in closure
		operator := op
		RegisterSpecialForm(operator, func(args *core.ListValue, ctx *core.Context) (core.Value, error) {
			return AugmentedAssignForm(operator, args, ctx)
		})
	}
}

// IsAugmentedAssignOp checks if a string is an augmented assignment operator
func IsAugmentedAssignOp(s string) bool {
	// Check if it ends with = and has a known operator prefix
	if !strings.HasSuffix(s, "=") || len(s) < 2 {
		return false
	}
	_, ok := augmentedOps[s]
	return ok
}
