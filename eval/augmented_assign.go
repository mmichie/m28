package eval

import (
	"fmt"
	"strings"

	"github.com/mmichie/m28/core"
)

// augmentedOps maps augmented assignment operators to their base operators
var augmentedOps = map[string]string{
	"+=":  "+",
	"-=":  "-",
	"*=":  "*",
	"/=":  "/",
	"%=":  "%",
	"**=": "**",
}

// AugmentedAssignForm handles augmented assignment operators
// (+= x y) -> (= x (+ x y))
func AugmentedAssignForm(op string, args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("%s requires exactly 2 arguments, got %d", op, len(args))
	}

	// Get the base operator
	baseOp, ok := augmentedOps[op]
	if !ok {
		return nil, fmt.Errorf("unknown augmented assignment operator: %s", op)
	}

	target := args[0]
	augValue := args[1]

	// Handle different target types
	switch t := target.(type) {
	case core.SymbolValue:
		// Simple variable: x += y -> (= x (+ x y))
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

		// Create the operation expression
		opExpr := core.ListValue{
			core.SymbolValue(baseOp),
			currentValue,
			evalAugValue,
		}

		// Evaluate the operation
		result, err := Eval(opExpr, ctx)
		if err != nil {
			return nil, err
		}

		// Assign the result
		ctx.Define(string(t), result)
		return result, nil

	case core.ListValue:
		// Could be indexing or dot notation
		if len(t) >= 3 {
			if sym, ok := t[0].(core.SymbolValue); ok {
				switch string(sym) {
				case "get-item":
					// Index assignment: lst[i] += y
					return augmentedIndexAssign(t, baseOp, augValue, ctx)
				case ".":
					// Property assignment: obj.prop += y
					return augmentedPropertyAssign(t, baseOp, augValue, ctx)
				}
			}
		}
		return nil, fmt.Errorf("invalid assignment target: %v", t)

	default:
		return nil, fmt.Errorf("cannot assign to %s", target.Type())
	}
}

// augmentedIndexAssign handles lst[i] += value
func augmentedIndexAssign(indexExpr core.ListValue, baseOp string, augValue core.Value, ctx *core.Context) (core.Value, error) {
	// indexExpr is (get-item obj key)
	if len(indexExpr) != 3 {
		return nil, fmt.Errorf("invalid index expression")
	}

	// We don't need to evaluate obj and key separately since GetItemForm will do it

	// Get current value
	currentValue, err := GetItemForm(core.ListValue{indexExpr[1], indexExpr[2]}, ctx)
	if err != nil {
		return nil, err
	}

	// Evaluate augment value
	evalAugValue, err := Eval(augValue, ctx)
	if err != nil {
		return nil, err
	}

	// Perform operation
	opExpr := core.ListValue{
		core.SymbolValue(baseOp),
		currentValue,
		evalAugValue,
	}
	result, err := Eval(opExpr, ctx)
	if err != nil {
		return nil, err
	}

	// Set the new value
	return SetItemForm(core.ListValue{indexExpr[1], indexExpr[2], result}, ctx)
}

// augmentedPropertyAssign handles obj.prop += value
func augmentedPropertyAssign(dotExpr core.ListValue, baseOp string, augValue core.Value, ctx *core.Context) (core.Value, error) {
	// dotExpr is (. obj "prop")
	if len(dotExpr) < 3 {
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

	// Perform operation
	opExpr := core.ListValue{
		core.SymbolValue(baseOp),
		currentValue,
		evalAugValue,
	}
	result, err := Eval(opExpr, ctx)
	if err != nil {
		return nil, err
	}

	// Create assignment expression and evaluate it
	assignExpr := core.ListValue{
		core.SymbolValue("="),
		dotExpr,
		result,
	}
	return AssignForm(assignExpr[1:], ctx)
}

// RegisterAugmentedAssignment registers all augmented assignment operators
func RegisterAugmentedAssignment() {
	for op := range augmentedOps {
		// Capture op in closure
		operator := op
		RegisterSpecialForm(operator, func(args core.ListValue, ctx *core.Context) (core.Value, error) {
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
