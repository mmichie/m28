package ast

import (
	"testing"

	"github.com/mmichie/m28/core"
)

func TestIdentifier(t *testing.T) {
	loc := &core.SourceLocation{File: "test.m28", Line: 1, Column: 1}
	ident := NewIdentifier("foo", loc, SyntaxLisp)

	// Test basic properties
	if ident.Name != "foo" {
		t.Errorf("Expected name 'foo', got '%s'", ident.Name)
	}

	if ident.Type() != core.SymbolType {
		t.Errorf("Expected SymbolType, got %v", ident.Type())
	}

	if ident.String() != "foo" {
		t.Errorf("Expected string 'foo', got '%s'", ident.String())
	}

	// Test AST properties
	if ident.Location() != loc {
		t.Error("Location mismatch")
	}

	if ident.SyntaxKind() != SyntaxLisp {
		t.Errorf("Expected SyntaxLisp, got %v", ident.SyntaxKind())
	}

	// Test ToIR conversion
	ir := ident.ToIR()
	if sym, ok := ir.(core.SymbolValue); !ok || string(sym) != "foo" {
		t.Errorf("Expected SymbolValue('foo'), got %v", ir)
	}
}

func TestLiteral(t *testing.T) {
	loc := &core.SourceLocation{File: "test.m28", Line: 2, Column: 5}

	// Test number literal
	numLit := NewLiteral(core.NumberValue(42), loc, SyntaxLisp)
	if numLit.Type() != core.NumberType {
		t.Errorf("Expected NumberType, got %v", numLit.Type())
	}

	ir := numLit.ToIR()
	if num, ok := ir.(core.NumberValue); !ok || num != 42 {
		t.Errorf("Expected NumberValue(42), got %v", ir)
	}

	// Test string literal
	strLit := NewLiteral(core.StringValue("hello"), loc, SyntaxLisp)
	if strLit.Type() != core.StringType {
		t.Errorf("Expected StringType, got %v", strLit.Type())
	}

	// Test bool literal
	boolLit := NewLiteral(core.True, loc, SyntaxLisp)
	if boolLit.Type() != core.BoolType {
		t.Errorf("Expected BoolType, got %v", boolLit.Type())
	}
}

func TestSExpr(t *testing.T) {
	loc := &core.SourceLocation{File: "test.m28", Line: 3, Column: 1}

	// Create (+ 1 2)
	elements := []ASTNode{
		NewIdentifier("+", loc, SyntaxLisp),
		NewLiteral(core.NumberValue(1), loc, SyntaxLisp),
		NewLiteral(core.NumberValue(2), loc, SyntaxLisp),
	}

	sexpr := NewSExpr(elements, loc, SyntaxLisp)

	if sexpr.Type() != core.ListType {
		t.Errorf("Expected ListType, got %v", sexpr.Type())
	}

	if sexpr.String() != "(+ 1 2)" {
		t.Errorf("Expected '(+ 1 2)', got '%s'", sexpr.String())
	}

	// Test ToIR conversion
	ir := sexpr.ToIR()
	list, ok := ir.(core.ListValue)
	if !ok {
		t.Fatalf("Expected ListValue, got %T", ir)
	}

	if len(list) != 3 {
		t.Errorf("Expected 3 elements, got %d", len(list))
	}

	// Check elements
	if sym, ok := list[0].(core.SymbolValue); !ok || string(sym) != "+" {
		t.Errorf("Expected first element to be '+', got %v", list[0])
	}

	if num, ok := list[1].(core.NumberValue); !ok || num != 1 {
		t.Errorf("Expected second element to be 1, got %v", list[1])
	}

	if num, ok := list[2].(core.NumberValue); !ok || num != 2 {
		t.Errorf("Expected third element to be 2, got %v", list[2])
	}
}

func TestDefForm(t *testing.T) {
	loc := &core.SourceLocation{File: "test.m28", Line: 5, Column: 1}

	// Create (def double (x) (* x 2))
	params := []Parameter{
		{Name: "x", Type: nil, Default: nil},
	}

	body := NewSExpr([]ASTNode{
		NewIdentifier("*", loc, SyntaxLisp),
		NewIdentifier("x", loc, SyntaxLisp),
		NewLiteral(core.NumberValue(2), loc, SyntaxLisp),
	}, loc, SyntaxLisp)

	def := NewDefForm("double", params, body, nil, nil, false, loc, SyntaxLisp)

	if def.Name != "double" {
		t.Errorf("Expected name 'double', got '%s'", def.Name)
	}

	// Test ToIR conversion
	ir := def.ToIR()
	list, ok := ir.(core.ListValue)
	if !ok {
		t.Fatalf("Expected ListValue, got %T", ir)
	}

	if len(list) != 4 {
		t.Errorf("Expected 4 elements (def name params body), got %d", len(list))
	}

	// Check def symbol
	if sym, ok := list[0].(core.SymbolValue); !ok || string(sym) != "def" {
		t.Errorf("Expected 'def', got %v", list[0])
	}

	// Check function name
	if sym, ok := list[1].(core.SymbolValue); !ok || string(sym) != "double" {
		t.Errorf("Expected 'double', got %v", list[1])
	}

	// Check params list
	paramList, ok := list[2].(core.ListValue)
	if !ok {
		t.Fatalf("Expected params to be ListValue, got %T", list[2])
	}
	if len(paramList) != 1 {
		t.Errorf("Expected 1 parameter, got %d", len(paramList))
	}
	if sym, ok := paramList[0].(core.SymbolValue); !ok || string(sym) != "x" {
		t.Errorf("Expected parameter 'x', got %v", paramList[0])
	}

	// Check body
	bodyList, ok := list[3].(core.ListValue)
	if !ok {
		t.Fatalf("Expected body to be ListValue, got %T", list[3])
	}
	if len(bodyList) != 3 {
		t.Errorf("Expected body to have 3 elements, got %d", len(bodyList))
	}
}

func TestDefFormWithTypes(t *testing.T) {
	loc := &core.SourceLocation{File: "test.py", Line: 1, Column: 1}

	// Create def add(x: int, y: int) -> int: return x + y
	params := []Parameter{
		{Name: "x", Type: &TypeInfo{Name: "int"}, Default: nil},
		{Name: "y", Type: &TypeInfo{Name: "int"}, Default: nil},
	}

	body := NewSExpr([]ASTNode{
		NewIdentifier("+", loc, SyntaxPython),
		NewIdentifier("x", loc, SyntaxPython),
		NewIdentifier("y", loc, SyntaxPython),
	}, loc, SyntaxPython)

	returnType := &TypeInfo{Name: "int"}
	def := NewDefForm("add", params, body, returnType, nil, false, loc, SyntaxPython)

	if def.ReturnType == nil {
		t.Fatal("Expected return type to be set")
	}

	if def.ReturnType.Name != "int" {
		t.Errorf("Expected return type 'int', got '%s'", def.ReturnType.Name)
	}

	if def.SyntaxKind() != SyntaxPython {
		t.Errorf("Expected SyntaxPython, got %v", def.SyntaxKind())
	}

	// ToIR should still produce standard Lisp form
	// (type annotations are preserved in metadata, not IR)
	ir := def.ToIR()
	list, ok := ir.(core.ListValue)
	if !ok {
		t.Fatalf("Expected ListValue, got %T", ir)
	}

	if sym, ok := list[0].(core.SymbolValue); !ok || string(sym) != "def" {
		t.Errorf("Expected 'def', got %v", list[0])
	}
}

func TestAssignForm(t *testing.T) {
	loc := &core.SourceLocation{File: "test.m28", Line: 10, Column: 1}

	// Create (= x 10)
	target := NewIdentifier("x", loc, SyntaxLisp)
	value := NewLiteral(core.NumberValue(10), loc, SyntaxLisp)
	assign := NewAssignForm(target, value, loc, SyntaxLisp)

	if assign.String() != "(= x 10)" {
		t.Errorf("Expected '(= x 10)', got '%s'", assign.String())
	}

	// Test ToIR conversion
	ir := assign.ToIR()
	list, ok := ir.(core.ListValue)
	if !ok {
		t.Fatalf("Expected ListValue, got %T", ir)
	}

	if len(list) != 3 {
		t.Errorf("Expected 3 elements, got %d", len(list))
	}

	if sym, ok := list[0].(core.SymbolValue); !ok || string(sym) != "=" {
		t.Errorf("Expected '=', got %v", list[0])
	}
}

func TestIfForm(t *testing.T) {
	loc := &core.SourceLocation{File: "test.m28", Line: 15, Column: 1}

	// Create (if (> x 0) x (- x))
	condition := NewSExpr([]ASTNode{
		NewIdentifier(">", loc, SyntaxLisp),
		NewIdentifier("x", loc, SyntaxLisp),
		NewLiteral(core.NumberValue(0), loc, SyntaxLisp),
	}, loc, SyntaxLisp)

	thenBranch := NewIdentifier("x", loc, SyntaxLisp)

	elseBranch := NewSExpr([]ASTNode{
		NewIdentifier("-", loc, SyntaxLisp),
		NewIdentifier("x", loc, SyntaxLisp),
	}, loc, SyntaxLisp)

	ifForm := NewIfForm(condition, thenBranch, elseBranch, loc, SyntaxLisp)

	// Test ToIR conversion
	ir := ifForm.ToIR()
	list, ok := ir.(core.ListValue)
	if !ok {
		t.Fatalf("Expected ListValue, got %T", ir)
	}

	if len(list) != 4 {
		t.Errorf("Expected 4 elements (if condition then else), got %d", len(list))
	}

	if sym, ok := list[0].(core.SymbolValue); !ok || string(sym) != "if" {
		t.Errorf("Expected 'if', got %v", list[0])
	}
}

func TestTypeInfo(t *testing.T) {
	// Simple type
	intType := &TypeInfo{Name: "int"}
	if intType.String() != "int" {
		t.Errorf("Expected 'int', got '%s'", intType.String())
	}

	// Generic type
	listIntType := &TypeInfo{
		Name:    "List",
		Generic: []*TypeInfo{{Name: "int"}},
	}
	if listIntType.String() != "List[int]" {
		t.Errorf("Expected 'List[int]', got '%s'", listIntType.String())
	}

	// Dict type
	dictType := &TypeInfo{
		Name: "Dict",
		Generic: []*TypeInfo{
			{Name: "str"},
			{Name: "int"},
		},
	}
	if dictType.String() != "Dict[str, int]" {
		t.Errorf("Expected 'Dict[str, int]', got '%s'", dictType.String())
	}

	// Optional type
	optType := &TypeInfo{
		Name:       "str",
		IsOptional: true,
	}
	if optType.String() != "Optional[str]" {
		t.Errorf("Expected 'Optional[str]', got '%s'", optType.String())
	}
}
