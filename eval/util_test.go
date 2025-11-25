package eval

import (
	"strings"
	"testing"

	"github.com/mmichie/m28/core"
)

func TestDoForm_Util(t *testing.T) {
	tests := []struct {
		name     string
		exprs    []core.Value
		expected core.Value
	}{
		{
			name:     "empty do returns nil",
			exprs:    []core.Value{},
			expected: core.Nil,
		},
		{
			name:     "single expression",
			exprs:    []core.Value{core.NumberValue(42)},
			expected: core.NumberValue(42),
		},
		{
			name: "multiple expressions return last",
			exprs: []core.Value{
				core.NumberValue(1),
				core.NumberValue(2),
				core.NumberValue(3),
			},
			expected: core.NumberValue(3),
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			ctx := setupTestContext()
			args := core.NewList(tt.exprs...)

			result, err := DoForm(args, ctx)
			if err != nil {
				t.Fatalf("DoForm() error = %v", err)
			}

			if result != tt.expected {
				t.Errorf("DoForm() = %v, want %v", result, tt.expected)
			}
		})
	}
}

func TestDefForm(t *testing.T) {
	ctx := setupTestContext()

	// Define a simple function: (def add (a b) (+ a b))
	params := core.NewList(core.SymbolValue("a"), core.SymbolValue("b"))
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
	fnStr := fn.String()
	if !strings.Contains(fnStr, "add") && !strings.Contains(fnStr, "function") {
		t.Errorf("Function string = %v, expected to contain 'add' or 'function'", fnStr)
	}
}

func TestAssignForm_Simple(t *testing.T) {
	ctx := setupTestContext()

	// Simple assignment: (= x 42)
	args := core.NewList(core.SymbolValue("x"), core.NumberValue(42))

	result, err := AssignForm(args, ctx)
	if err != nil {
		t.Fatalf("AssignForm() error = %v", err)
	}

	if result != core.NumberValue(42) {
		t.Errorf("AssignForm() = %v, want 42", result)
	}

	// Verify variable is set
	x, err := ctx.Lookup("x")
	if err != nil {
		t.Fatalf("Variable not defined: %v", err)
	}

	if x != core.NumberValue(42) {
		t.Errorf("x = %v, want 42", x)
	}
}

func TestAssignForm_Errors(t *testing.T) {
	tests := []struct {
		name     string
		args     *core.ListValue
		errMatch string
	}{
		{
			name:     "no arguments",
			args:     core.NewList(),
			errMatch: "at least 2 arguments",
		},
		{
			name:     "one argument",
			args:     core.NewList(core.SymbolValue("x")),
			errMatch: "at least 2 arguments",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			ctx := setupTestContext()
			_, err := AssignForm(tt.args, ctx)
			if err == nil {
				t.Fatal("AssignForm() expected error, got nil")
			}
			if !strings.Contains(err.Error(), tt.errMatch) {
				t.Errorf("AssignForm() error = %v, want error containing %q", err, tt.errMatch)
			}
		})
	}
}

func TestReturnForm(t *testing.T) {
	ctx := setupTestContext()

	// Return with value
	args := core.NewList(core.NumberValue(42))

	result, err := ReturnForm(args, ctx)
	if err != nil {
		t.Fatalf("ReturnForm() error = %v", err)
	}

	// Should be a ReturnValue
	retVal, ok := result.(*ReturnValue)
	if !ok {
		t.Fatalf("ReturnForm() returned %T, want *ReturnValue", result)
	}

	// Verify return value
	if retVal.Value != core.NumberValue(42) {
		t.Errorf("Return value = %v, want 42", retVal.Value)
	}
}

func TestReturnForm_NoValue(t *testing.T) {
	ctx := setupTestContext()

	// Return without value
	args := core.NewList()

	result, err := ReturnForm(args, ctx)
	if err != nil {
		t.Fatalf("ReturnForm() error = %v", err)
	}

	// Should be a ReturnValue with nil value
	retVal, ok := result.(*ReturnValue)
	if !ok {
		t.Fatalf("ReturnForm() returned %T, want *ReturnValue", result)
	}

	if retVal.Value != core.Nil {
		t.Errorf("Return value = %v, want nil", retVal.Value)
	}
}

func TestUnwrapLocated(t *testing.T) {
	tests := []struct {
		name     string
		input    core.Value
		expected core.Value
	}{
		{
			name:     "unwrap located value",
			input:    core.LocatedValue{Value: core.NumberValue(42), Location: nil},
			expected: core.NumberValue(42),
		},
		{
			name: "unwrap nested located values",
			input: core.LocatedValue{
				Value: core.LocatedValue{
					Value:    core.StringValue("test"),
					Location: nil,
				},
				Location: nil,
			},
			expected: core.StringValue("test"),
		},
		{
			name:     "non-located value returns itself",
			input:    core.NumberValue(100),
			expected: core.NumberValue(100),
		},
		{
			name:     "nil returns nil",
			input:    core.Nil,
			expected: core.Nil,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := unwrapLocated(tt.input)
			if result != tt.expected {
				t.Errorf("unwrapLocated() = %v, want %v", result, tt.expected)
			}
		})
	}
}

func TestIsTruthyWithErrors(t *testing.T) {
	ctx := setupTestContext()

	tests := []struct {
		name     string
		value    core.Value
		expected bool
	}{
		{
			name:     "true is truthy",
			value:    core.BoolValue(true),
			expected: true,
		},
		{
			name:     "false is falsy",
			value:    core.BoolValue(false),
			expected: false,
		},
		{
			name:     "non-zero number is truthy",
			value:    core.NumberValue(42),
			expected: true,
		},
		{
			name:     "zero is falsy",
			value:    core.NumberValue(0),
			expected: false,
		},
		{
			name:     "non-empty string is truthy",
			value:    core.StringValue("hello"),
			expected: true,
		},
		{
			name:     "empty string is falsy",
			value:    core.StringValue(""),
			expected: false,
		},
		{
			name:     "nil is falsy",
			value:    core.Nil,
			expected: false,
		},
		{
			name:     "non-empty list is truthy",
			value:    core.NewList(core.NumberValue(1)),
			expected: true,
		},
		{
			name:     "empty list is falsy",
			value:    core.NewList(),
			expected: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := isTruthyWithErrors(tt.value, ctx)
			if err != nil {
				t.Fatalf("isTruthyWithErrors() error = %v", err)
			}

			if result != tt.expected {
				t.Errorf("isTruthyWithErrors(%v) = %v, want %v", tt.value, result, tt.expected)
			}
		})
	}
}

func TestQuoteForm_Basic(t *testing.T) {
	ctx := setupTestContext()

	// Quote a symbol - should not evaluate it
	args := core.NewList(core.SymbolValue("undefined_var"))

	result, err := quoteForm(args, ctx)
	if err != nil {
		t.Fatalf("quoteForm() error = %v", err)
	}

	// Should return the symbol itself, not try to look it up
	if sym, ok := result.(core.SymbolValue); !ok {
		t.Errorf("quoteForm() returned %T, want core.SymbolValue", result)
	} else if string(sym) != "undefined_var" {
		t.Errorf("quoteForm() = %q, want \"undefined_var\"", string(sym))
	}
}

func TestQuoteForm_Error(t *testing.T) {
	ctx := setupTestContext()

	// Quote with no arguments
	args := core.NewList()

	_, err := quoteForm(args, ctx)
	if err == nil {
		t.Fatal("quoteForm() expected error for no arguments, got nil")
	}

	if !strings.Contains(err.Error(), "requires 1 argument") && !strings.Contains(err.Error(), "exactly 1 argument") {
		t.Errorf("quoteForm() error = %v, want error about argument count", err)
	}
}

func TestErrArgCount(t *testing.T) {
	err := ErrArgCount("test requires 2 arguments")
	if err == nil {
		t.Fatal("ErrArgCount() returned nil")
	}

	if !strings.Contains(err.Error(), "test requires 2 arguments") {
		t.Errorf("ErrArgCount() = %v, want error containing argument message", err)
	}
}
