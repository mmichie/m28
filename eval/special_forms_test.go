package eval

import (
	"strings"
	"testing"

	"github.com/mmichie/m28/core"
)

func TestIfForm(t *testing.T) {
	tests := []struct {
		name        string
		condition   core.Value
		trueBranch  core.Value
		falseBranch core.Value
		expected    core.Value
	}{
		{
			name:        "true condition returns true branch",
			condition:   core.BoolValue(true),
			trueBranch:  core.NumberValue(42),
			falseBranch: core.NumberValue(0),
			expected:    core.NumberValue(42),
		},
		{
			name:        "false condition returns false branch",
			condition:   core.BoolValue(false),
			trueBranch:  core.NumberValue(42),
			falseBranch: core.NumberValue(0),
			expected:    core.NumberValue(0),
		},
		{
			name:        "truthy number condition",
			condition:   core.NumberValue(1),
			trueBranch:  core.StringValue("yes"),
			falseBranch: core.StringValue("no"),
			expected:    core.StringValue("yes"),
		},
		{
			name:        "falsy zero condition",
			condition:   core.NumberValue(0),
			trueBranch:  core.StringValue("yes"),
			falseBranch: core.StringValue("no"),
			expected:    core.StringValue("no"),
		},
		{
			name:        "nil condition is falsy",
			condition:   core.Nil,
			trueBranch:  core.StringValue("yes"),
			falseBranch: core.StringValue("no"),
			expected:    core.StringValue("no"),
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			ctx := setupTestContext()

			// Create if form: (if condition trueBranch falseBranch)
			args := core.NewList(tt.condition, tt.trueBranch, tt.falseBranch)

			result, err := ifForm(args, ctx)
			if err != nil {
				t.Fatalf("ifForm() error = %v", err)
			}

			if result != tt.expected {
				t.Errorf("ifForm() = %v, want %v", result, tt.expected)
			}
		})
	}
}

func TestIfForm_WithoutElse(t *testing.T) {
	ctx := setupTestContext()

	// Test if without else branch
	// (if true 42) should return 42
	args := core.NewList(core.BoolValue(true), core.NumberValue(42))
	result, err := ifForm(args, ctx)
	if err != nil {
		t.Fatalf("ifForm() error = %v", err)
	}
	if result != core.NumberValue(42) {
		t.Errorf("ifForm() = %v, want 42", result)
	}

	// (if false 42) should return nil
	args = core.NewList(core.BoolValue(false), core.NumberValue(42))
	result, err = ifForm(args, ctx)
	if err != nil {
		t.Fatalf("ifForm() error = %v", err)
	}
	if result != core.Nil {
		t.Errorf("ifForm() = %v, want nil", result)
	}
}

func TestIfForm_Errors(t *testing.T) {
	ctx := setupTestContext()

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
			args:     core.NewList(core.BoolValue(true)),
			errMatch: "at least 2 arguments",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			_, err := ifForm(tt.args, ctx)
			if err == nil {
				t.Fatal("ifForm() expected error, got nil")
			}
			if !strings.Contains(err.Error(), tt.errMatch) {
				t.Errorf("ifForm() error = %v, want error containing %q", err, tt.errMatch)
			}
		})
	}
}

func TestAssignForm(t *testing.T) {
	tests := []struct {
		name     string
		varName  string
		value    core.Value
		expected core.Value
	}{
		{
			name:     "assign number",
			varName:  "x",
			value:    core.NumberValue(42),
			expected: core.NumberValue(42),
		},
		{
			name:     "assign string",
			varName:  "name",
			value:    core.StringValue("test"),
			expected: core.StringValue("test"),
		},
		{
			name:     "assign nil",
			varName:  "result",
			value:    core.Nil,
			expected: core.Nil,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			ctx := setupTestContext()

			// Create assign form: (= varName value)
			args := core.NewList(core.SymbolValue(tt.varName), tt.value)

			result, err := assignForm(args, ctx)
			if err != nil {
				t.Fatalf("assignForm() error = %v", err)
			}

			if result != tt.expected {
				t.Errorf("assignForm() = %v, want %v", result, tt.expected)
			}

			// Verify the variable is set in context
			lookup, err := ctx.Lookup(tt.varName)
			if err != nil {
				t.Fatalf("ctx.Lookup() error = %v", err)
			}
			if lookup != tt.expected {
				t.Errorf("variable %s = %v, want %v", tt.varName, lookup, tt.expected)
			}
		})
	}
}

func TestQuoteForm(t *testing.T) {
	tests := []struct {
		name     string
		arg      core.Value
		expected core.Value
	}{
		{
			name:     "quote symbol",
			arg:      core.SymbolValue("x"),
			expected: core.SymbolValue("x"),
		},
		{
			name:     "quote list",
			arg:      core.NewList(core.SymbolValue("foo"), core.NumberValue(1)),
			expected: core.NewList(core.SymbolValue("foo"), core.NumberValue(1)),
		},
		{
			name:     "quote number",
			arg:      core.NumberValue(42),
			expected: core.NumberValue(42),
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			ctx := setupTestContext()

			// Create quote form: (quote arg)
			args := core.NewList(tt.arg)

			result, err := quoteForm(args, ctx)
			if err != nil {
				t.Fatalf("quoteForm() error = %v", err)
			}

			// For complex types like lists, compare string representations
			if result.String() != tt.expected.String() {
				t.Errorf("quoteForm() = %v, want %v", result, tt.expected)
			}
		})
	}
}

func TestDoForm(t *testing.T) {
	ctx := setupTestContext()

	// Create do form that sets multiple variables and returns last value
	// (do (= x 1) (= y 2) (= z 3) z)
	args := core.NewList(
		core.NewList(core.SymbolValue("="), core.SymbolValue("x"), core.NumberValue(1)),
		core.NewList(core.SymbolValue("="), core.SymbolValue("y"), core.NumberValue(2)),
		core.NewList(core.SymbolValue("="), core.SymbolValue("z"), core.NumberValue(3)),
		core.SymbolValue("z"),
	)

	result, err := doForm(args, ctx)
	if err != nil {
		t.Fatalf("doForm() error = %v", err)
	}

	// Should return the last value
	if result != core.NumberValue(3) {
		t.Errorf("doForm() = %v, want 3", result)
	}

	// Verify all variables were set
	x, _ := ctx.Lookup("x")
	if x != core.NumberValue(1) {
		t.Errorf("x = %v, want 1", x)
	}

	y, _ := ctx.Lookup("y")
	if y != core.NumberValue(2) {
		t.Errorf("y = %v, want 2", y)
	}

	z, _ := ctx.Lookup("z")
	if z != core.NumberValue(3) {
		t.Errorf("z = %v, want 3", z)
	}
}

func TestDoForm_Empty(t *testing.T) {
	ctx := setupTestContext()

	// Empty do form should return nil
	args := core.NewList()

	result, err := doForm(args, ctx)
	if err != nil {
		t.Fatalf("doForm() error = %v", err)
	}

	if result != core.Nil {
		t.Errorf("doForm() = %v, want nil", result)
	}
}

func TestLambdaForm(t *testing.T) {
	ctx := setupTestContext()

	// Create lambda: (lambda (x) (* x 2))
	params := core.NewList(core.SymbolValue("x"))
	body := core.NewList(core.SymbolValue("*"), core.SymbolValue("x"), core.NumberValue(2))
	args := core.NewList(params, body)

	result, err := lambdaForm(args, ctx)
	if err != nil {
		t.Fatalf("lambdaForm() error = %v", err)
	}

	// Result should be a function (could be UserFunction or GeneratorFunction)
	// Just verify it's callable by checking its string representation
	resultStr := result.String()
	if !strings.Contains(resultStr, "lambda") && !strings.Contains(resultStr, "function") {
		t.Errorf("lambda result string = %v, expected to contain 'lambda' or 'function'", resultStr)
	}
}

func TestListLiteralForm(t *testing.T) {
	ctx := setupTestContext()
	ctx.Define("x", core.NumberValue(10))

	// Create list literal with evaluated contents: [1, 2, x]
	args := core.NewList(
		core.NumberValue(1),
		core.NumberValue(2),
		core.SymbolValue("x"),
	)

	result, err := listLiteralForm(args, ctx)
	if err != nil {
		t.Fatalf("listLiteralForm() error = %v", err)
	}

	list, ok := result.(*core.ListValue)
	if !ok {
		t.Fatalf("listLiteralForm() returned %T, want *core.ListValue", result)
	}

	if list.Len() != 3 {
		t.Fatalf("list length = %d, want 3", list.Len())
	}

	// Check values
	items := list.Items()
	if items[0] != core.NumberValue(1) {
		t.Errorf("list[0] = %v, want 1", items[0])
	}
	if items[1] != core.NumberValue(2) {
		t.Errorf("list[1] = %v, want 2", items[1])
	}
	if items[2] != core.NumberValue(10) {
		t.Errorf("list[2] = %v, want 10 (evaluated x)", items[2])
	}
}

func TestTupleLiteralForm(t *testing.T) {
	ctx := setupTestContext()
	ctx.Define("y", core.StringValue("hello"))

	// Create tuple literal with evaluated contents: (1, 2, y)
	args := core.NewList(
		core.NumberValue(1),
		core.NumberValue(2),
		core.SymbolValue("y"),
	)

	result, err := tupleLiteralForm(args, ctx)
	if err != nil {
		t.Fatalf("tupleLiteralForm() error = %v", err)
	}

	tuple, ok := result.(core.TupleValue)
	if !ok {
		t.Fatalf("tupleLiteralForm() returned %T, want core.TupleValue", result)
	}

	if len(tuple) != 3 {
		t.Fatalf("tuple length = %d, want 3", len(tuple))
	}

	// Check values
	if tuple[0] != core.NumberValue(1) {
		t.Errorf("tuple[0] = %v, want 1", tuple[0])
	}
	if tuple[1] != core.NumberValue(2) {
		t.Errorf("tuple[1] = %v, want 2", tuple[1])
	}
	if tuple[2] != core.StringValue("hello") {
		t.Errorf("tuple[2] = %v, want \"hello\" (evaluated y)", tuple[2])
	}
}

func TestRaiseForm(t *testing.T) {
	ctx := setupTestContext()

	// Create raise form: (raise "test error")
	args := core.NewList(core.StringValue("test error"))

	_, err := raiseForm(args, ctx)
	if err == nil {
		t.Fatal("raiseForm() expected error, got nil")
	}

	if !strings.Contains(err.Error(), "test error") {
		t.Errorf("raiseForm() error = %v, want error containing 'test error'", err)
	}
}

func TestRegisterSpecialForm_ThroughEval(t *testing.T) {
	// Save original state
	originalForms := make(map[string]SpecialFormHandler)
	for k, v := range specialForms {
		originalForms[k] = v
	}
	defer func() {
		// Restore original state
		specialForms = originalForms
	}()

	// Test registering a new special form
	called := false
	testHandler := func(args *core.ListValue, ctx *core.Context) (core.Value, error) {
		called = true
		return core.NumberValue(99), nil
	}

	RegisterSpecialForm("test-special", testHandler)

	// Verify it was registered
	if handler, ok := specialForms["test-special"]; !ok {
		t.Fatal("Special form was not registered")
	} else if handler == nil {
		t.Fatal("Special form handler is nil")
	}

	// Verify it can be called through Eval
	ctx := setupTestContext()
	testList := core.NewList(core.SymbolValue("test-special"))
	result, err := Eval(testList, ctx)
	if err != nil {
		t.Fatalf("Eval() error = %v", err)
	}

	if !called {
		t.Error("Special form handler was not called")
	}

	if result != core.NumberValue(99) {
		t.Errorf("Special form returned %v, want 99", result)
	}
}
