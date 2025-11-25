package eval

import (
	"testing"

	"github.com/mmichie/m28/core"
)

// setupTestContext creates a basic context for testing
// We avoid importing builtin here to prevent import cycles
func setupTestContext() *core.Context {
	return core.NewContext(nil)
}

func TestEval_SelfEvaluating(t *testing.T) {
	tests := []struct {
		name     string
		input    core.Value
		expected core.Value
	}{
		{
			name:     "number evaluates to itself",
			input:    core.NumberValue(42),
			expected: core.NumberValue(42),
		},
		{
			name:     "float evaluates to itself",
			input:    core.NumberValue(3.14),
			expected: core.NumberValue(3.14),
		},
		{
			name:     "string evaluates to itself",
			input:    core.StringValue("hello"),
			expected: core.StringValue("hello"),
		},
		{
			name:     "empty string evaluates to itself",
			input:    core.StringValue(""),
			expected: core.StringValue(""),
		},
		{
			name:     "true evaluates to itself",
			input:    core.BoolValue(true),
			expected: core.BoolValue(true),
		},
		{
			name:     "false evaluates to itself",
			input:    core.BoolValue(false),
			expected: core.BoolValue(false),
		},
		{
			name:     "nil evaluates to itself",
			input:    core.Nil,
			expected: core.Nil,
		},
	}

	ctx := setupTestContext()

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := Eval(tt.input, ctx)
			if err != nil {
				t.Fatalf("Eval() error = %v", err)
			}
			if result != tt.expected {
				t.Errorf("Eval() = %v, want %v", result, tt.expected)
			}
		})
	}
}

func TestEval_SymbolLookup(t *testing.T) {
	tests := []struct {
		name      string
		symbol    string
		setValue  core.Value
		expected  core.Value
		shouldErr bool
	}{
		{
			name:     "lookup existing variable",
			symbol:   "x",
			setValue: core.NumberValue(42),
			expected: core.NumberValue(42),
		},
		{
			name:     "lookup string variable",
			symbol:   "name",
			setValue: core.StringValue("test"),
			expected: core.StringValue("test"),
		},
		{
			name:      "lookup nonexistent variable",
			symbol:    "undefined",
			shouldErr: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			ctx := setupTestContext()
			if tt.setValue != nil {
				ctx.Define(tt.symbol, tt.setValue)
			}

			result, err := Eval(core.SymbolValue(tt.symbol), ctx)
			if tt.shouldErr {
				if err == nil {
					t.Errorf("Eval() expected error, got nil")
				}
				return
			}

			if err != nil {
				t.Fatalf("Eval() error = %v", err)
			}
			if result != tt.expected {
				t.Errorf("Eval() = %v, want %v", result, tt.expected)
			}
		})
	}
}

func TestEval_EmptyList(t *testing.T) {
	ctx := setupTestContext()
	result, err := Eval(core.NewList(), ctx)
	if err != nil {
		t.Fatalf("Eval() error = %v", err)
	}

	// Empty list should evaluate to a fresh empty list
	if result == nil {
		t.Fatal("Eval() returned nil")
	}
	list, ok := result.(*core.ListValue)
	if !ok {
		t.Fatalf("Eval() returned %T, want *core.ListValue", result)
	}
	if list.Len() != 0 {
		t.Errorf("Eval() returned list with length %d, want 0", list.Len())
	}
}

func TestEval_Dict(t *testing.T) {
	ctx := setupTestContext()

	// Create a dict with some values
	dict := core.NewDict()
	dict.SetValue(core.StringValue("key1"), core.NumberValue(42))
	dict.SetValue(core.StringValue("key2"), core.StringValue("value"))

	result, err := Eval(dict, ctx)
	if err != nil {
		t.Fatalf("Eval() error = %v", err)
	}

	// Result should be a fresh copy of the dict
	resultDict, ok := result.(*core.DictValue)
	if !ok {
		t.Fatalf("Eval() returned %T, want *core.DictValue", result)
	}

	// Verify contents are copied
	val1, found1 := resultDict.GetValue(core.StringValue("key1"))
	if !found1 {
		t.Error("Expected key1 to be in result dict")
	}
	if val1 != core.NumberValue(42) {
		t.Errorf("key1 value = %v, want 42", val1)
	}

	val2, found2 := resultDict.GetValue(core.StringValue("key2"))
	if !found2 {
		t.Error("Expected key2 to be in result dict")
	}
	if val2 != core.StringValue("value") {
		t.Errorf("key2 value = %v, want \"value\"", val2)
	}

	// Verify it's a copy, not the same object
	if resultDict == dict {
		t.Error("Eval() returned same dict object instead of copy")
	}
}

func TestEval_Set(t *testing.T) {
	ctx := setupTestContext()

	// Create a set with some values
	set := core.NewSet()
	set.Add(core.NumberValue(1))
	set.Add(core.NumberValue(2))
	set.Add(core.NumberValue(3))

	result, err := Eval(set, ctx)
	if err != nil {
		t.Fatalf("Eval() error = %v", err)
	}

	// Result should be a fresh copy of the set
	resultSet, ok := result.(*core.SetValue)
	if !ok {
		t.Fatalf("Eval() returned %T, want *core.SetValue", result)
	}

	// Verify contents are copied
	if !resultSet.Contains(core.NumberValue(1)) {
		t.Error("Expected 1 to be in result set")
	}
	if !resultSet.Contains(core.NumberValue(2)) {
		t.Error("Expected 2 to be in result set")
	}
	if !resultSet.Contains(core.NumberValue(3)) {
		t.Error("Expected 3 to be in result set")
	}

	// Verify it's a copy, not the same object
	if resultSet == set {
		t.Error("Eval() returned same set object instead of copy")
	}
}

func TestEval_LocatedValue(t *testing.T) {
	ctx := setupTestContext()

	// Create a located value
	location := &core.SourceLocation{
		File:   "test.py",
		Line:   10,
		Column: 5,
	}
	located := core.LocatedValue{
		Value:    core.NumberValue(42),
		Location: location,
	}

	result, err := Eval(located, ctx)
	if err != nil {
		t.Fatalf("Eval() error = %v", err)
	}

	// Result should be the unwrapped value
	if result != core.NumberValue(42) {
		t.Errorf("Eval() = %v, want 42", result)
	}

	// Location should have been pushed and popped
	if ctx.CurrentLocation() != nil {
		t.Error("Expected location to be popped from stack")
	}
}

func TestEval_EvalCountIncrement(t *testing.T) {
	ctx := setupTestContext()
	initialCount := ctx.EvalCount

	// Evaluate something simple
	_, err := Eval(core.NumberValue(42), ctx)
	if err != nil {
		t.Fatalf("Eval() error = %v", err)
	}

	// Eval count should have incremented
	if ctx.EvalCount != initialCount+1 {
		t.Errorf("EvalCount = %d, want %d", ctx.EvalCount, initialCount+1)
	}
}

func TestEval_NilContext(t *testing.T) {
	// Test that Eval handles nil context gracefully for self-evaluating values
	result, err := Eval(core.NumberValue(42), nil)
	if err != nil {
		t.Fatalf("Eval() with nil context error = %v", err)
	}
	if result != core.NumberValue(42) {
		t.Errorf("Eval() = %v, want 42", result)
	}
}

func TestEval_UnknownValueType(t *testing.T) {
	ctx := setupTestContext()

	// Test with a custom value type that doesn't match any case
	// In the current implementation, unknown types evaluate to themselves
	customValue := &core.RangeValue{Start: 0, Stop: 10, Step: 1}

	result, err := Eval(customValue, ctx)
	if err != nil {
		t.Fatalf("Eval() error = %v", err)
	}

	// Unknown types should evaluate to themselves
	if result != customValue {
		t.Errorf("Eval() returned different value for unknown type")
	}
}

func TestRegisterSpecialForm(t *testing.T) {
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
		return core.Nil, nil
	}

	RegisterSpecialForm("test-form", testHandler)

	// Verify it was registered
	if handler, ok := specialForms["test-form"]; !ok {
		t.Error("Special form was not registered")
	} else if handler == nil {
		t.Error("Special form handler is nil")
	}

	// Verify it can be called
	ctx := setupTestContext()
	testList := core.NewList(core.SymbolValue("test-form"))
	_, err := Eval(testList, ctx)
	if err != nil {
		t.Fatalf("Eval() error = %v", err)
	}

	if !called {
		t.Error("Special form handler was not called")
	}
}

func TestRegisterSpecialForm_Duplicate(t *testing.T) {
	// Save original state
	originalForms := make(map[string]SpecialFormHandler)
	for k, v := range specialForms {
		originalForms[k] = v
	}
	defer func() {
		// Restore original state
		specialForms = originalForms
	}()

	// Test that duplicate registration doesn't panic in non-strict mode
	StrictDuplicateChecking = false
	defer func() { StrictDuplicateChecking = false }()

	handler1 := func(args *core.ListValue, ctx *core.Context) (core.Value, error) {
		return core.NumberValue(1), nil
	}
	handler2 := func(args *core.ListValue, ctx *core.Context) (core.Value, error) {
		return core.NumberValue(2), nil
	}

	RegisterSpecialForm("dup-test", handler1)
	RegisterSpecialForm("dup-test", handler2) // Should not panic

	// The second handler should win
	ctx := setupTestContext()
	testList := core.NewList(core.SymbolValue("dup-test"))
	result, err := Eval(testList, ctx)
	if err != nil {
		t.Fatalf("Eval() error = %v", err)
	}

	// Should use the second handler
	if result != core.NumberValue(2) {
		t.Errorf("Got handler result %v, want 2", result)
	}
}
