package core

import (
	"testing"
)

func TestIRMetadata_Locations(t *testing.T) {
	metadata := NewIRMetadata()

	// Create some hashable values (numbers, strings, symbols, bools)
	// Note: ListValue, DictValue are NOT hashable and can't be map keys
	val1 := NumberValue(42)
	val2 := StringValue("hello")
	val3 := SymbolValue("foo")

	// Set locations
	loc1 := &SourceLocation{File: "test1.m28", Line: 10, Column: 5}
	loc2 := &SourceLocation{File: "test2.m28", Line: 20, Column: 15}
	loc3 := &SourceLocation{File: "test3.m28", Line: 30, Column: 1}

	metadata.SetLocation(val1, loc1)
	metadata.SetLocation(val2, loc2)
	metadata.SetLocation(val3, loc3)

	// Get locations back
	if got := metadata.GetLocation(val1); got != loc1 {
		t.Errorf("Expected loc1, got %v", got)
	}

	if got := metadata.GetLocation(val2); got != loc2 {
		t.Errorf("Expected loc2, got %v", got)
	}

	if got := metadata.GetLocation(val3); got != loc3 {
		t.Errorf("Expected loc3, got %v", got)
	}

	// Non-existent value should return nil
	val4 := NumberValue(99)
	if got := metadata.GetLocation(val4); got != nil {
		t.Errorf("Expected nil for non-existent value, got %v", got)
	}
}

func TestIRMetadata_Comments(t *testing.T) {
	metadata := NewIRMetadata()

	val := SymbolValue("foo")
	comments := []string{"This is a comment", "Another comment"}

	metadata.SetComments(val, comments)

	got := metadata.GetComments(val)
	if len(got) != 2 {
		t.Fatalf("Expected 2 comments, got %d", len(got))
	}

	if got[0] != "This is a comment" {
		t.Errorf("Expected 'This is a comment', got '%s'", got[0])
	}

	if got[1] != "Another comment" {
		t.Errorf("Expected 'Another comment', got '%s'", got[1])
	}
}

func TestIRMetadata_SyntaxKind(t *testing.T) {
	metadata := NewIRMetadata()

	// Use hashable values (symbols, not lists)
	val1 := SymbolValue("def")
	val2 := SymbolValue("+")

	// SyntaxKind 0 = Lisp, 1 = Python (from ast package)
	metadata.SetSyntaxKind(val1, 0) // Lisp
	metadata.SetSyntaxKind(val2, 1) // Python

	kind1, ok1 := metadata.GetSyntaxKind(val1)
	if !ok1 {
		t.Error("Expected to find syntax kind for val1")
	}
	if kind1 != 0 {
		t.Errorf("Expected kind 0 (Lisp), got %d", kind1)
	}

	kind2, ok2 := metadata.GetSyntaxKind(val2)
	if !ok2 {
		t.Error("Expected to find syntax kind for val2")
	}
	if kind2 != 1 {
		t.Errorf("Expected kind 1 (Python), got %d", kind2)
	}

	// Non-existent value
	val3 := NumberValue(42)
	_, ok3 := metadata.GetSyntaxKind(val3)
	if ok3 {
		t.Error("Expected no syntax kind for non-existent value")
	}
}

func TestIRMetadata_TypeAnnotations(t *testing.T) {
	metadata := NewIRMetadata()

	val := SymbolValue("x")
	typeInfo := map[string]string{"name": "int"}

	metadata.SetTypeAnnotation(val, typeInfo)

	got := metadata.GetTypeAnnotation(val)
	if got == nil {
		t.Fatal("Expected type annotation, got nil")
	}

	typeMap, ok := got.(map[string]string)
	if !ok {
		t.Fatalf("Expected map[string]string, got %T", got)
	}

	if typeMap["name"] != "int" {
		t.Errorf("Expected type 'int', got '%s'", typeMap["name"])
	}
}

func TestIRMetadata_Clear(t *testing.T) {
	metadata := NewIRMetadata()

	val := NumberValue(42)
	loc := &SourceLocation{File: "test.m28", Line: 1, Column: 1}
	metadata.SetLocation(val, loc)

	if metadata.Size() != 1 {
		t.Errorf("Expected size 1, got %d", metadata.Size())
	}

	metadata.Clear()

	if metadata.Size() != 0 {
		t.Errorf("Expected size 0 after clear, got %d", metadata.Size())
	}

	if got := metadata.GetLocation(val); got != nil {
		t.Errorf("Expected nil after clear, got %v", got)
	}
}

func TestIRMetadata_Merge(t *testing.T) {
	metadata1 := NewIRMetadata()
	metadata2 := NewIRMetadata()

	val1 := NumberValue(1)
	val2 := NumberValue(2)
	val3 := NumberValue(3)

	loc1 := &SourceLocation{File: "test1.m28", Line: 1, Column: 1}
	loc2 := &SourceLocation{File: "test2.m28", Line: 2, Column: 2}
	loc3 := &SourceLocation{File: "test3.m28", Line: 3, Column: 3}

	metadata1.SetLocation(val1, loc1)
	metadata1.SetLocation(val2, loc2)

	metadata2.SetLocation(val3, loc3)

	// Merge metadata2 into metadata1
	metadata1.Merge(metadata2)

	// metadata1 should now have all three locations
	if metadata1.Size() != 3 {
		t.Errorf("Expected size 3 after merge, got %d", metadata1.Size())
	}

	if got := metadata1.GetLocation(val1); got != loc1 {
		t.Error("val1 location lost after merge")
	}

	if got := metadata1.GetLocation(val2); got != loc2 {
		t.Error("val2 location lost after merge")
	}

	if got := metadata1.GetLocation(val3); got != loc3 {
		t.Error("val3 location not added in merge")
	}
}

func TestIRMetadata_NilSafety(t *testing.T) {
	var metadata *IRMetadata // nil metadata

	val := NumberValue(42)
	loc := &SourceLocation{File: "test.m28", Line: 1, Column: 1}

	// All operations should be safe on nil metadata
	metadata.SetLocation(val, loc)
	metadata.SetComments(val, []string{"comment"})
	metadata.SetSyntaxKind(val, 0)
	metadata.SetTypeAnnotation(val, "int")

	if got := metadata.GetLocation(val); got != nil {
		t.Errorf("Expected nil from nil metadata, got %v", got)
	}

	if got := metadata.GetComments(val); got != nil {
		t.Errorf("Expected nil from nil metadata, got %v", got)
	}

	if _, ok := metadata.GetSyntaxKind(val); ok {
		t.Error("Expected false from nil metadata")
	}

	if got := metadata.GetTypeAnnotation(val); got != nil {
		t.Errorf("Expected nil from nil metadata, got %v", got)
	}

	if size := metadata.Size(); size != 0 {
		t.Errorf("Expected size 0 from nil metadata, got %d", size)
	}

	// Clear and Merge should also be safe
	metadata.Clear()
	metadata.Merge(NewIRMetadata())
}

func TestContext_WithMetadata(t *testing.T) {
	// Create global context (creates its own metadata)
	globalCtx := NewContext(nil)

	if globalCtx.Metadata == nil {
		t.Fatal("Expected global context to create metadata")
	}

	originalMetadata := globalCtx.Metadata

	// Create child context (inherits metadata)
	childCtx := NewContext(globalCtx)

	if childCtx.Metadata != originalMetadata {
		t.Error("Expected child context to inherit parent's metadata")
	}

	// Test WithMetadata
	newMetadata := NewIRMetadata()
	ctxWithNew := globalCtx.WithMetadata(newMetadata)

	if ctxWithNew.Metadata != newMetadata {
		t.Error("Expected WithMetadata to set new metadata")
	}

	// Original context should be unchanged
	if globalCtx.Metadata != originalMetadata {
		t.Error("WithMetadata should not modify original context")
	}
}
