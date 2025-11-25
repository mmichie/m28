package eval

import (
	"fmt"
	"testing"

	"github.com/mmichie/m28/core"
)

func TestDefForm_Complete(t *testing.T) {
	ctx := setupTestContext()

	// Define a function that adds two numbers
	// (def add (a b) (+ a b))
	params := core.NewList(core.SymbolValue("a"), core.SymbolValue("b"))

	// Register + operator manually since we can't import builtin
	ctx.Define("+", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, fmt.Errorf("+ requires 2 arguments")
		}
		a, aOk := args[0].(core.NumberValue)
		b, bOk := args[1].(core.NumberValue)
		if !aOk || !bOk {
			return nil, fmt.Errorf("+ requires numeric arguments")
		}
		return core.NumberValue(float64(a) + float64(b)), nil
	}))

	body := core.NewList(core.SymbolValue("+"), core.SymbolValue("a"), core.SymbolValue("b"))
	args := core.NewList(core.SymbolValue("add"), params, body)

	result, err := DefForm(args, ctx)
	if err != nil {
		t.Fatalf("DefForm() error = %v", err)
	}

	// Result should be the function
	if result == nil {
		t.Fatal("DefForm() returned nil")
	}

	// Verify function is defined in context
	fn, err := ctx.Lookup("add")
	if err != nil {
		t.Fatalf("Function not defined in context: %v", err)
	}

	// Verify it's callable
	if fn == nil {
		t.Fatal("Function is nil")
	}
}

func TestLambdaForm_Complete(t *testing.T) {
	ctx := setupTestContext()

	// Create a lambda that doubles a number
	// (lambda (x) (* x 2))
	ctx.Define("*", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, fmt.Errorf("* requires 2 arguments")
		}
		a, aOk := args[0].(core.NumberValue)
		b, bOk := args[1].(core.NumberValue)
		if !aOk || !bOk {
			return nil, fmt.Errorf("* requires numeric arguments")
		}
		return core.NumberValue(float64(a) * float64(b)), nil
	}))

	params := core.NewList(core.SymbolValue("x"))
	body := core.NewList(core.SymbolValue("*"), core.SymbolValue("x"), core.NumberValue(2))
	args := core.NewList(params, body)

	result, err := lambdaForm(args, ctx)
	if err != nil {
		t.Fatalf("lambdaForm() error = %v", err)
	}

	// Result should be a function
	if result == nil {
		t.Fatal("lambdaForm() returned nil")
	}
}

func TestUserFunction_String(t *testing.T) {
	ctx := setupTestContext()

	// Create a simple function
	fn := &UserFunction{
		BaseObject: *core.NewBaseObject(core.FunctionType),
		name:       "test_func",
		params:     []core.SymbolValue{"a", "b"},
		body:       core.NumberValue(42),
		env:        ctx,
	}

	str := fn.String()
	if str == "" {
		t.Error("Function String() returned empty string")
	}

	// Should contain function name or indicate it's a function
	if len(str) < 3 {
		t.Errorf("Function String() = %q, seems too short", str)
	}
}

func TestUserFunction_Type(t *testing.T) {
	ctx := setupTestContext()

	fn := &UserFunction{
		BaseObject: *core.NewBaseObject(core.FunctionType),
		name:       "test",
		params:     []core.SymbolValue{},
		body:       core.Nil,
		env:        ctx,
	}

	if fn.Type() != core.FunctionType {
		t.Errorf("Function Type() = %v, want %v", fn.Type(), core.FunctionType)
	}
}

func TestDefForm_Errors(t *testing.T) {
	ctx := setupTestContext()

	tests := []struct {
		name      string
		args      *core.ListValue
		shouldErr bool
	}{
		{
			name:      "no arguments",
			args:      core.NewList(),
			shouldErr: true,
		},
		{
			name:      "one argument",
			args:      core.NewList(core.SymbolValue("foo")),
			shouldErr: true,
		},
		{
			name: "non-symbol name",
			args: core.NewList(
				core.NumberValue(123),
				core.NewList(),
				core.Nil,
			),
			shouldErr: true,
		},
		{
			name: "non-list params",
			args: core.NewList(
				core.SymbolValue("foo"),
				core.NumberValue(123),
				core.Nil,
			),
			shouldErr: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			_, err := DefForm(tt.args, ctx)
			if tt.shouldErr {
				if err == nil {
					t.Error("DefForm() expected error, got nil")
				}
			} else {
				if err != nil {
					t.Errorf("DefForm() unexpected error = %v", err)
				}
			}
		})
	}
}

func TestLambdaForm_Errors(t *testing.T) {
	ctx := setupTestContext()

	tests := []struct {
		name      string
		args      *core.ListValue
		shouldErr bool
	}{
		{
			name:      "no arguments",
			args:      core.NewList(),
			shouldErr: true,
		},
		{
			name:      "one argument",
			args:      core.NewList(core.NewList()),
			shouldErr: true,
		},
		{
			name: "non-list params",
			args: core.NewList(
				core.NumberValue(123),
				core.Nil,
			),
			shouldErr: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			_, err := lambdaForm(tt.args, ctx)
			if tt.shouldErr {
				if err == nil {
					t.Error("lambdaForm() expected error, got nil")
				}
			} else {
				if err != nil {
					t.Errorf("lambdaForm() unexpected error = %v", err)
				}
			}
		})
	}
}

func TestReturnValue_Type(t *testing.T) {
	retVal := &ReturnValue{Value: core.NumberValue(42)}

	if retVal.Type() != core.Type("return") {
		t.Errorf("ReturnValue Type() = %v, want return", retVal.Type())
	}
}

func TestReturnValue_String(t *testing.T) {
	retVal := &ReturnValue{Value: core.NumberValue(42)}

	str := retVal.String()
	if str == "" {
		t.Error("ReturnValue String() returned empty string")
	}
}

func TestBreakValue_Type(t *testing.T) {
	breakVal := &BreakValue{}

	if breakVal.Type() != core.Type("break") {
		t.Errorf("BreakValue Type() = %v, want break", breakVal.Type())
	}
}

func TestBreakValue_String(t *testing.T) {
	breakVal := &BreakValue{}

	str := breakVal.String()
	if str == "" {
		t.Error("BreakValue String() returned empty string")
	}
}

func TestContinueValue_Type(t *testing.T) {
	contVal := &ContinueValue{}

	if contVal.Type() != core.Type("continue") {
		t.Errorf("ContinueValue Type() = %v, want continue", contVal.Type())
	}
}

func TestContinueValue_String(t *testing.T) {
	contVal := &ContinueValue{}

	str := contVal.String()
	if str == "" {
		t.Error("ContinueValue String() returned empty string")
	}
}

func TestWalrusForm_Basic(t *testing.T) {
	ctx := setupTestContext()

	// Walrus operator: (x := 42)
	// This should assign and return the value
	args := core.NewList(core.SymbolValue("x"), core.NumberValue(42))

	result, err := WalrusForm(args, ctx)
	if err != nil {
		t.Fatalf("WalrusForm() error = %v", err)
	}

	// Should return the assigned value
	if result != core.NumberValue(42) {
		t.Errorf("WalrusForm() = %v, want 42", result)
	}

	// Variable should be defined
	x, err := ctx.Lookup("x")
	if err != nil {
		t.Fatalf("Variable not defined: %v", err)
	}

	if x != core.NumberValue(42) {
		t.Errorf("x = %v, want 42", x)
	}
}
