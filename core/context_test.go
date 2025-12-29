package core

import (
	"strings"
	"testing"
)

// ============== Context Creation Tests ==============

func TestNewContext(t *testing.T) {
	ctx := NewContext(nil)

	if ctx == nil {
		t.Fatal("NewContext should not return nil")
	}

	if ctx.Outer != nil {
		t.Error("Root context should have nil outer")
	}
}

func TestNewContextWithOuter(t *testing.T) {
	outer := NewContext(nil)
	inner := NewContext(outer)

	if inner.Outer != outer {
		t.Error("Inner context should reference outer")
	}
}

// ============== Define and Lookup Tests ==============

func TestContextDefine(t *testing.T) {
	ctx := NewContext(nil)

	ctx.Define("x", NumberValue(42))

	val, err := ctx.Lookup("x")
	if err != nil {
		t.Errorf("Lookup failed: %v", err)
	}

	if num, ok := val.(NumberValue); !ok || float64(num) != 42 {
		t.Errorf("Expected 42, got %v", val)
	}
}

func TestContextDefineOverwrite(t *testing.T) {
	ctx := NewContext(nil)

	ctx.Define("x", NumberValue(1))
	ctx.Define("x", NumberValue(2))

	val, err := ctx.Lookup("x")
	if err != nil {
		t.Errorf("Lookup failed: %v", err)
	}

	if num, ok := val.(NumberValue); !ok || float64(num) != 2 {
		t.Errorf("Expected 2, got %v", val)
	}
}

func TestContextLookupNotFound(t *testing.T) {
	ctx := NewContext(nil)

	_, err := ctx.Lookup("nonexistent")
	if err == nil {
		t.Error("Expected error for non-existent variable")
	}
}

func TestContextLookupInOuter(t *testing.T) {
	outer := NewContext(nil)
	outer.Define("x", NumberValue(42))

	inner := NewContext(outer)

	// Should find x in outer
	val, err := inner.Lookup("x")
	if err != nil {
		t.Errorf("Lookup failed: %v", err)
	}

	if num, ok := val.(NumberValue); !ok || float64(num) != 42 {
		t.Errorf("Expected 42, got %v", val)
	}
}

func TestContextShadowing(t *testing.T) {
	outer := NewContext(nil)
	outer.Define("x", NumberValue(1))

	inner := NewContext(outer)
	inner.Define("x", NumberValue(2))

	// Inner should return its own value
	val, err := inner.Lookup("x")
	if err != nil {
		t.Errorf("Lookup failed: %v", err)
	}

	if num, ok := val.(NumberValue); !ok || float64(num) != 2 {
		t.Errorf("Expected 2, got %v", val)
	}

	// Outer should still have its value
	val, err = outer.Lookup("x")
	if err != nil {
		t.Errorf("Lookup failed: %v", err)
	}

	if num, ok := val.(NumberValue); !ok || float64(num) != 1 {
		t.Errorf("Expected 1, got %v", val)
	}
}

// ============== Set Tests ==============

func TestContextSet(t *testing.T) {
	ctx := NewContext(nil)
	ctx.Define("x", NumberValue(1))

	err := ctx.Set("x", NumberValue(2))
	if err != nil {
		t.Errorf("Set failed: %v", err)
	}

	val, _ := ctx.Lookup("x")
	if num, ok := val.(NumberValue); !ok || float64(num) != 2 {
		t.Errorf("Expected 2, got %v", val)
	}
}

func TestContextSetInOuter(t *testing.T) {
	outer := NewContext(nil)
	outer.Define("x", NumberValue(1))

	inner := NewContext(outer)

	// Set should update outer's variable
	err := inner.Set("x", NumberValue(2))
	if err != nil {
		t.Errorf("Set failed: %v", err)
	}

	// Check that outer was updated
	val, _ := outer.Lookup("x")
	if num, ok := val.(NumberValue); !ok || float64(num) != 2 {
		t.Errorf("Expected outer to be updated to 2, got %v", val)
	}
}

func TestContextSetNotFound(t *testing.T) {
	ctx := NewContext(nil)

	err := ctx.Set("nonexistent", NumberValue(1))
	if err == nil {
		t.Error("Expected error when setting non-existent variable")
	}
}

// ============== Delete Tests ==============

func TestContextDelete(t *testing.T) {
	ctx := NewContext(nil)
	ctx.Define("x", NumberValue(1))

	err := ctx.Delete("x")
	if err != nil {
		t.Errorf("Delete failed: %v", err)
	}

	_, err = ctx.Lookup("x")
	if err == nil {
		t.Error("x should be deleted")
	}
}

func TestContextDeleteNotFound(t *testing.T) {
	ctx := NewContext(nil)

	err := ctx.Delete("nonexistent")
	if err == nil {
		t.Error("Expected error when deleting non-existent variable")
	}
}

// ============== Global/Nonlocal Tests ==============

func TestContextDeclareGlobal(t *testing.T) {
	ctx := NewContext(nil)

	ctx.DeclareGlobal("x")

	if !ctx.IsGlobal("x") {
		t.Error("x should be declared global")
	}

	if ctx.IsGlobal("y") {
		t.Error("y should not be global")
	}
}

func TestContextDeclareNonlocal(t *testing.T) {
	outer := NewContext(nil)
	outer.Define("x", NumberValue(1))

	inner := NewContext(outer)

	err := inner.DeclareNonlocal("x")
	if err != nil {
		t.Errorf("DeclareNonlocal failed: %v", err)
	}

	if !inner.IsNonlocal("x") {
		t.Error("x should be declared nonlocal")
	}
}

func TestContextDeclareNonlocalNotFound(t *testing.T) {
	ctx := NewContext(nil)

	err := ctx.DeclareNonlocal("x")
	if err == nil {
		t.Error("Expected error when declaring nonlocal for non-existent variable")
	}
}

// ============== Call Stack Tests ==============

func TestContextCallStack(t *testing.T) {
	ctx := NewContext(nil)

	ctx.PushStack("func1", "file1.m28", 10, 5)
	ctx.PushStack("func2", "file2.m28", 20, 3)

	stack := ctx.GetCallStack()
	if len(stack) != 2 {
		t.Errorf("Expected stack size 2, got %d", len(stack))
	}

	if stack[0].Function != "func1" {
		t.Errorf("Expected first function 'func1', got '%s'", stack[0].Function)
	}

	if stack[1].Function != "func2" {
		t.Errorf("Expected second function 'func2', got '%s'", stack[1].Function)
	}

	ctx.PopStack()
	stack = ctx.GetCallStack()
	if len(stack) != 1 {
		t.Errorf("Expected stack size 1 after pop, got %d", len(stack))
	}
}

func TestContextFormatStackTrace(t *testing.T) {
	ctx := NewContext(nil)
	ctx.PushStack("myFunc", "test.m28", 10, 5)

	trace := ctx.FormatStackTrace()

	if !strings.Contains(trace, "myFunc") {
		t.Errorf("Stack trace should contain function name, got: %s", trace)
	}

	if !strings.Contains(trace, "test.m28") {
		t.Errorf("Stack trace should contain file name, got: %s", trace)
	}
}

// ============== Location Stack Tests ==============

func TestContextLocationStack(t *testing.T) {
	ctx := NewContext(nil)

	loc1 := &SourceLocation{File: "file1.m28", Line: 10, Column: 5}
	loc2 := &SourceLocation{File: "file2.m28", Line: 20, Column: 3}

	ctx.PushLocation(loc1)
	ctx.PushLocation(loc2)

	current := ctx.CurrentLocation()
	if current != loc2 {
		t.Error("Current location should be loc2")
	}

	ctx.PopLocation()
	current = ctx.CurrentLocation()
	if current != loc1 {
		t.Error("Current location should be loc1 after pop")
	}

	ctx.PopLocation()
	current = ctx.CurrentLocation()
	if current != nil {
		t.Error("Current location should be nil after all pops")
	}
}

// ============== GetAllSymbols Tests ==============

func TestContextGetAllSymbols(t *testing.T) {
	ctx := NewContext(nil)
	ctx.Define("a", NumberValue(1))
	ctx.Define("b", NumberValue(2))
	ctx.Define("c", NumberValue(3))

	symbols := ctx.GetAllSymbols()

	if len(symbols) < 3 {
		t.Errorf("Expected at least 3 symbols, got %d", len(symbols))
	}

	// Check that our defined symbols are present
	found := map[string]bool{}
	for _, s := range symbols {
		found[s] = true
	}

	if !found["a"] || !found["b"] || !found["c"] {
		t.Error("All defined symbols should be in GetAllSymbols")
	}
}

func TestContextGetAllSymbolsIncludesOuter(t *testing.T) {
	outer := NewContext(nil)
	outer.Define("outer_var", NumberValue(1))

	inner := NewContext(outer)
	inner.Define("inner_var", NumberValue(2))

	symbols := inner.GetAllSymbols()

	found := map[string]bool{}
	for _, s := range symbols {
		found[s] = true
	}

	if !found["outer_var"] {
		t.Error("GetAllSymbols should include outer context symbols")
	}

	if !found["inner_var"] {
		t.Error("GetAllSymbols should include inner context symbols")
	}
}

// ============== WithMetadata Tests ==============

func TestContextWithMetadata(t *testing.T) {
	ctx := NewContext(nil)
	metadata := NewIRMetadata()

	newCtx := ctx.WithMetadata(metadata)

	if newCtx == ctx {
		t.Error("WithMetadata should return a new context")
	}

	if newCtx.Metadata != metadata {
		t.Error("New context should have the metadata")
	}
}

// ============== TraceEntry Tests ==============

func TestTraceEntry(t *testing.T) {
	entry := TraceEntry{
		Function: "myFunc",
		File:     "test.m28",
		Line:     42,
		Column:   10,
	}

	if entry.Function != "myFunc" {
		t.Errorf("Expected function 'myFunc', got '%s'", entry.Function)
	}

	if entry.Line != 42 {
		t.Errorf("Expected line 42, got %d", entry.Line)
	}
}
