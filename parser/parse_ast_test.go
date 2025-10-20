package parser

import (
	"testing"

	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/core/ast"
)

func TestParseToAST_SimpleLiteral(t *testing.T) {
	p := NewParser()
	p.SetFilename("test.m28")

	astNode, metadata, err := p.ParseToAST("42")
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	// Check we got a literal
	lit, ok := astNode.(*ast.Literal)
	if !ok {
		t.Fatalf("Expected Literal, got %T", astNode)
	}

	// Check value
	num, ok := lit.Value.(core.NumberValue)
	if !ok || num != 42 {
		t.Errorf("Expected NumberValue(42), got %v", lit.Value)
	}

	// Check location
	loc := lit.Location()
	if loc == nil {
		t.Fatal("Expected location to be set")
	}
	if loc.File != "test.m28" {
		t.Errorf("Expected file 'test.m28', got '%s'", loc.File)
	}
	if loc.Line != 1 {
		t.Errorf("Expected line 1, got %d", loc.Line)
	}

	// Check metadata was created
	if metadata == nil {
		t.Fatal("Expected metadata to be returned")
	}
}

func TestParseToAST_Identifier(t *testing.T) {
	p := NewParser()
	p.SetFilename("test.m28")

	astNode, _, err := p.ParseToAST("foo")
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	// Check we got an identifier
	ident, ok := astNode.(*ast.Identifier)
	if !ok {
		t.Fatalf("Expected Identifier, got %T", astNode)
	}

	if ident.Name != "foo" {
		t.Errorf("Expected name 'foo', got '%s'", ident.Name)
	}

	// Check location
	loc := ident.Location()
	if loc == nil {
		t.Fatal("Expected location to be set")
	}
}

func TestParseToAST_Assignment(t *testing.T) {
	p := NewParser()
	p.SetFilename("test.m28")

	astNode, _, err := p.ParseToAST("x = 10")
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	// Check we got an assignment
	assign, ok := astNode.(*ast.AssignForm)
	if !ok {
		t.Fatalf("Expected AssignForm, got %T", astNode)
	}

	// Check target
	target, ok := assign.Target.(*ast.Identifier)
	if !ok {
		t.Fatalf("Expected target to be Identifier, got %T", assign.Target)
	}
	if target.Name != "x" {
		t.Errorf("Expected target 'x', got '%s'", target.Name)
	}

	// Check value
	value, ok := assign.Value.(*ast.Literal)
	if !ok {
		t.Fatalf("Expected value to be Literal, got %T", assign.Value)
	}
	num, ok := value.Value.(core.NumberValue)
	if !ok || num != 10 {
		t.Errorf("Expected value 10, got %v", value.Value)
	}

	// Check location
	loc := assign.Location()
	if loc == nil {
		t.Fatal("Expected location to be set")
	}
}

func TestParseToAST_SExpr(t *testing.T) {
	p := NewParser()
	p.SetFilename("test.m28")

	astNode, _, err := p.ParseToAST("(+ 1 2)")
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	// Check we got an S-expression
	sexpr, ok := astNode.(*ast.SExpr)
	if !ok {
		t.Fatalf("Expected SExpr, got %T", astNode)
	}

	// Check elements
	if len(sexpr.Elements) != 3 {
		t.Fatalf("Expected 3 elements, got %d", len(sexpr.Elements))
	}

	// Check operator
	op, ok := sexpr.Elements[0].(*ast.Identifier)
	if !ok || op.Name != "+" {
		t.Errorf("Expected operator '+', got %v", sexpr.Elements[0])
	}

	// Check operands
	arg1, ok := sexpr.Elements[1].(*ast.Literal)
	if !ok {
		t.Fatalf("Expected arg1 to be Literal, got %T", sexpr.Elements[1])
	}
	if num, ok := arg1.Value.(core.NumberValue); !ok || num != 1 {
		t.Errorf("Expected arg1 to be 1, got %v", arg1.Value)
	}

	arg2, ok := sexpr.Elements[2].(*ast.Literal)
	if !ok {
		t.Fatalf("Expected arg2 to be Literal, got %T", sexpr.Elements[2])
	}
	if num, ok := arg2.Value.(core.NumberValue); !ok || num != 2 {
		t.Errorf("Expected arg2 to be 2, got %v", arg2.Value)
	}
}

func TestParseToAST_PythonicCall(t *testing.T) {
	p := NewParser()
	p.SetFilename("test.m28")

	astNode, _, err := p.ParseToAST("print(42)")
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	// Check we got an S-expression (Pythonic call desugared)
	sexpr, ok := astNode.(*ast.SExpr)
	if !ok {
		t.Fatalf("Expected SExpr, got %T", astNode)
	}

	// Check elements: (print 42)
	if len(sexpr.Elements) != 2 {
		t.Fatalf("Expected 2 elements, got %d", len(sexpr.Elements))
	}

	// Check function name
	fn, ok := sexpr.Elements[0].(*ast.Identifier)
	if !ok || fn.Name != "print" {
		t.Errorf("Expected function 'print', got %v", sexpr.Elements[0])
	}

	// Check argument
	arg, ok := sexpr.Elements[1].(*ast.Literal)
	if !ok {
		t.Fatalf("Expected arg to be Literal, got %T", sexpr.Elements[1])
	}
	if num, ok := arg.Value.(core.NumberValue); !ok || num != 42 {
		t.Errorf("Expected arg to be 42, got %v", arg.Value)
	}
}

func TestParseToAST_VectorLiteral(t *testing.T) {
	p := NewParser()
	p.SetFilename("test.m28")

	astNode, _, err := p.ParseToAST("[1, 2, 3]")
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	// Check we got an S-expression: (list-literal 1 2 3)
	sexpr, ok := astNode.(*ast.SExpr)
	if !ok {
		t.Fatalf("Expected SExpr, got %T", astNode)
	}

	// Check elements
	if len(sexpr.Elements) != 4 { // list-literal + 3 elements
		t.Fatalf("Expected 4 elements, got %d", len(sexpr.Elements))
	}

	// Check it's a list-literal form
	listLit, ok := sexpr.Elements[0].(*ast.Identifier)
	if !ok || listLit.Name != "list-literal" {
		t.Errorf("Expected 'list-literal', got %v", sexpr.Elements[0])
	}
}

func TestParseToAST_ToIR(t *testing.T) {
	p := NewParser()
	p.SetFilename("test.m28")

	// Parse to AST
	astNode, _, err := p.ParseToAST("(+ 1 2)")
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	// Lower to IR
	ir := astNode.ToIR()

	// Check IR is correct
	list, ok := ir.(*core.ListValue)
	if !ok {
		t.Fatalf("Expected ListValue, got %T", ir)
	}

	items := list.Items()
	if len(items) != 3 {
		t.Fatalf("Expected 3 elements in IR, got %d", len(items))
	}

	// Check elements
	if sym, ok := items[0].(core.SymbolValue); !ok || string(sym) != "+" {
		t.Errorf("Expected '+', got %v", items[0])
	}

	if num, ok := items[1].(core.NumberValue); !ok || num != 1 {
		t.Errorf("Expected 1, got %v", items[1])
	}

	if num, ok := items[2].(core.NumberValue); !ok || num != 2 {
		t.Errorf("Expected 2, got %v", items[2])
	}
}
