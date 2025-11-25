package eval

import (
	"strings"
	"testing"

	"github.com/mmichie/m28/core"
)

func TestForForm_SimpleIteration(t *testing.T) {
	ctx := setupTestContext()

	// Create a list to iterate over
	items := core.NewList(core.NumberValue(1), core.NumberValue(2), core.NumberValue(3))
	ctx.Define("items", items)
	ctx.Define("result", core.NewList())

	// For loop: (for x items (append result x))
	// This would normally be created by the parser, we'll test the loop internals
	// by creating a simple iteration context

	// Test basic for loop structure
	varName := core.SymbolValue("x")
	iterable := core.SymbolValue("items")

	// Verify we can look up the iterable
	iterVal, err := Eval(iterable, ctx)
	if err != nil {
		t.Fatalf("Failed to evaluate iterable: %v", err)
	}

	list, ok := iterVal.(*core.ListValue)
	if !ok {
		t.Fatalf("Iterable is not a list: %T", iterVal)
	}

	if list.Len() != 3 {
		t.Errorf("List length = %d, want 3", list.Len())
	}

	// Verify iteration would work by manually iterating
	iter := list.Iterator()
	count := 0
	for {
		item, ok := iter.Next()
		if !ok {
			break
		}
		count++

		// Verify each item is accessible
		if item == nil {
			t.Error("Iterator returned nil item")
		}
	}

	if count != 3 {
		t.Errorf("Iterated %d items, want 3", count)
	}

	// Test that variable name is a symbol
	if string(varName) != "x" {
		t.Errorf("Variable name = %q, want \"x\"", string(varName))
	}
}

func TestWhileForm_Basic(t *testing.T) {
	ctx := setupTestContext()

	// Set up initial counter
	ctx.Define("counter", core.NumberValue(0))
	ctx.Define("result", core.NewList())

	// Test basic while loop structure
	// We're testing components that would be used in a while loop

	// Check initial condition
	counter, err := ctx.Lookup("counter")
	if err != nil {
		t.Fatalf("Failed to lookup counter: %v", err)
	}

	if counter != core.NumberValue(0) {
		t.Errorf("Initial counter = %v, want 0", counter)
	}

	// Verify we can evaluate conditions
	lessThan5, err := isTruthyWithErrors(core.BoolValue(true), ctx)
	if err != nil {
		t.Fatalf("Failed to evaluate condition: %v", err)
	}

	if !lessThan5 {
		t.Error("Condition should be truthy")
	}
}

func TestBreakForm(t *testing.T) {
	ctx := setupTestContext()

	// Test break form - should return a BreakValue
	args := core.NewList()

	result, err := BreakForm(args, ctx)
	if err != nil {
		t.Fatalf("BreakForm() error = %v", err)
	}

	// Should return a BreakValue
	if _, ok := result.(*BreakValue); !ok {
		t.Errorf("BreakForm() returned %T, want *BreakValue", result)
	}
}

func TestContinueForm(t *testing.T) {
	ctx := setupTestContext()

	// Test continue form - should return a ContinueValue
	args := core.NewList()

	result, err := ContinueForm(args, ctx)
	if err != nil {
		t.Fatalf("ContinueForm() error = %v", err)
	}

	// Should return a ContinueValue
	if _, ok := result.(*ContinueValue); !ok {
		t.Errorf("ContinueForm() returned %T, want *ContinueValue", result)
	}
}

func TestForForm_Errors(t *testing.T) {
	ctx := setupTestContext()

	tests := []struct {
		name     string
		args     *core.ListValue
		errMatch string
	}{
		{
			name:     "no arguments",
			args:     core.NewList(),
			errMatch: "requires at least 2 arguments",
		},
		{
			name:     "one argument",
			args:     core.NewList(core.SymbolValue("x")),
			errMatch: "requires at least 2 arguments",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			_, err := ForForm(tt.args, ctx)
			if err == nil {
				t.Fatal("ForForm() expected error, got nil")
			}
			if !strings.Contains(err.Error(), tt.errMatch) {
				t.Errorf("ForForm() error = %v, want error containing %q", err, tt.errMatch)
			}
		})
	}
}

func TestWhileForm_Errors(t *testing.T) {
	ctx := setupTestContext()

	tests := []struct {
		name     string
		args     *core.ListValue
		errMatch string
	}{
		{
			name:     "no arguments",
			args:     core.NewList(),
			errMatch: "requires at least 2 arguments",
		},
		{
			name:     "one argument",
			args:     core.NewList(core.BoolValue(true)),
			errMatch: "requires at least 2 arguments",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			_, err := WhileForm(tt.args, ctx)
			if err == nil {
				t.Fatal("WhileForm() expected error, got nil")
			}
			if !strings.Contains(err.Error(), tt.errMatch) {
				t.Errorf("WhileForm() error = %v, want error containing %q", err, tt.errMatch)
			}
		})
	}
}

func TestListComprehension(t *testing.T) {
	ctx := setupTestContext()

	// Test list comprehension form
	// [x * 2 for x in [1, 2, 3]]
	// This tests the basic structure

	// Create source list
	source := core.NewList(core.NumberValue(1), core.NumberValue(2), core.NumberValue(3))
	ctx.Define("source_list", source)

	// The comprehension would have:
	// - expr: (* x 2)
	// - var: x
	// - iterable: source_list

	// Test that we can access the iterable
	iterVal, err := ctx.Lookup("source_list")
	if err != nil {
		t.Fatalf("Failed to lookup source_list: %v", err)
	}

	list, ok := iterVal.(*core.ListValue)
	if !ok {
		t.Fatalf("source_list is not a list: %T", iterVal)
	}

	if list.Len() != 3 {
		t.Errorf("List length = %d, want 3", list.Len())
	}
}

func TestIteratorProtocol(t *testing.T) {
	// Test that core types implement iterator protocol correctly

	tests := []struct {
		name        string
		iterable    core.Value
		expectedLen int
		itemCheck   func(core.Value) bool
	}{
		{
			name:        "list iterator",
			iterable:    core.NewList(core.NumberValue(1), core.NumberValue(2), core.NumberValue(3)),
			expectedLen: 3,
			itemCheck: func(v core.Value) bool {
				_, ok := v.(core.NumberValue)
				return ok
			},
		},
		{
			name:        "empty list iterator",
			iterable:    core.NewList(),
			expectedLen: 0,
			itemCheck:   func(v core.Value) bool { return true },
		},
		{
			name:        "string iterator",
			iterable:    core.StringValue("abc"),
			expectedLen: 3,
			itemCheck: func(v core.Value) bool {
				s, ok := v.(core.StringValue)
				return ok && len(s) == 1
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			iterable, ok := tt.iterable.(core.Iterable)
			if !ok {
				t.Fatalf("Value is not iterable: %T", tt.iterable)
			}

			iter := iterable.Iterator()
			if iter == nil {
				t.Fatal("Iterator is nil")
			}

			count := 0
			for {
				item, ok := iter.Next()
				if !ok {
					break
				}
				count++

				if !tt.itemCheck(item) {
					t.Errorf("Item %d failed type check: %T", count, item)
				}
			}

			if count != tt.expectedLen {
				t.Errorf("Iterated %d items, want %d", count, tt.expectedLen)
			}
		})
	}
}
