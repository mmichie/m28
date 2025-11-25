// Package operators provides arithmetic and comparison operators for M28
package operators

import (
	"sync"

	"github.com/mmichie/m28/core"
)

// Global operator registry for fast lookup without context traversal
var (
	operatorRegistry   = make(map[string]core.Value)
	operatorRegistryMu sync.RWMutex
)

func init() {
	// Set the hook so context.Lookup can check the operator registry
	core.GetOperatorFunc = GetOperator
}

// GetOperator returns an operator from the global registry (fast path)
// Returns (operator, true) if found, (nil, false) otherwise
func GetOperator(name string) (core.Value, bool) {
	operatorRegistryMu.RLock()
	defer operatorRegistryMu.RUnlock()
	op, exists := operatorRegistry[name]
	return op, exists
}

// registerOperator adds an operator to both context and global registry
func registerOperator(ctx *core.Context, name string, op core.Value) {
	ctx.Define(name, op)

	operatorRegistryMu.Lock()
	operatorRegistry[name] = op
	operatorRegistryMu.Unlock()
}

// RegisterAll registers all operators
func RegisterAll(ctx *core.Context) {
	// Arithmetic operators
	registerOperator(ctx, "+", core.NewBuiltinFunction(Add()))
	registerOperator(ctx, "-", core.NewBuiltinFunction(Subtract()))
	registerOperator(ctx, "*", core.NewBuiltinFunction(Multiply()))
	registerOperator(ctx, "/", core.NewBuiltinFunction(Divide()))
	registerOperator(ctx, "//", core.NewBuiltinFunction(FloorDivide()))
	registerOperator(ctx, "%", core.NewBuiltinFunction(Modulo()))
	registerOperator(ctx, "**", core.NewBuiltinFunction(Power()))
	registerOperator(ctx, "@", core.NewBuiltinFunction(MatMul()))

	// Comparison operators
	registerOperator(ctx, "==", core.NewBuiltinFunction(Equal()))
	registerOperator(ctx, "!=", core.NewBuiltinFunction(NotEqual()))
	registerOperator(ctx, "<", core.NewBuiltinFunction(LessThan()))
	registerOperator(ctx, "<=", core.NewBuiltinFunction(LessThanOrEqual()))
	registerOperator(ctx, ">", core.NewBuiltinFunction(GreaterThan()))
	registerOperator(ctx, ">=", core.NewBuiltinFunction(GreaterThanOrEqual()))

	// Logical operators
	registerOperator(ctx, "not", core.NewBuiltinFunction(Not()))
	registerOperator(ctx, "and", core.NewBuiltinFunction(And()))
	registerOperator(ctx, "or", core.NewBuiltinFunction(Or()))
	registerOperator(ctx, "in", core.NewBuiltinFunction(In()))
	registerOperator(ctx, "not in", core.NewBuiltinFunction(NotIn()))
	registerOperator(ctx, "is", core.NewBuiltinFunction(Is()))
	registerOperator(ctx, "is not", core.NewBuiltinFunction(IsNot()))

	// Bitwise operators
	registerOperator(ctx, "<<", core.NewBuiltinFunction(LeftShift()))
	registerOperator(ctx, ">>", core.NewBuiltinFunction(RightShift()))
	registerOperator(ctx, "&", core.NewBuiltinFunction(BitwiseAnd()))
	registerOperator(ctx, "|", core.NewBuiltinFunction(BitwiseOr()))
	registerOperator(ctx, "^", core.NewBuiltinFunction(BitwiseXor()))
	registerOperator(ctx, "~", core.NewBuiltinFunction(BitwiseInvert()))
}
