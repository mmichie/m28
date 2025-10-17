package parser

import (
	"testing"

	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/core/ast"
)

// Helper function to create a parser from source code
func parseSource(source string) (*PythonParser, []ast.ASTNode, error) {
	tokenizer := NewPythonTokenizer(source)
	tokens, err := tokenizer.Tokenize()
	if err != nil {
		return nil, nil, err
	}

	parser := NewPythonParser(tokens)
	nodes, err := parser.Parse()
	return parser, nodes, err
}

// ============================================================================
// Simple Statement Tests
// ============================================================================

func TestParseLiteral(t *testing.T) {
	tests := []struct {
		source   string
		expected core.Value
	}{
		{"42", core.NumberValue(42)},
		{"3.14", core.NumberValue(3.14)},
		{`"hello"`, core.StringValue("hello")},
		{"True", core.BoolValue(true)},
		{"False", core.BoolValue(false)},
		{"None", core.None},
	}

	for _, tt := range tests {
		_, nodes, err := parseSource(tt.source)
		if err != nil {
			t.Fatalf("Parse error for %q: %v", tt.source, err)
		}

		if len(nodes) != 1 {
			t.Fatalf("Expected 1 node for %q, got %d", tt.source, len(nodes))
		}

		literal, ok := nodes[0].(*ast.Literal)
		if !ok {
			t.Fatalf("Expected Literal for %q, got %T", tt.source, nodes[0])
		}

		if literal.Value != tt.expected {
			t.Errorf("Expected value %v for %q, got %v", tt.expected, tt.source, literal.Value)
		}
	}
}

func TestParseIdentifier(t *testing.T) {
	_, nodes, err := parseSource("foo")
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	if len(nodes) != 1 {
		t.Fatalf("Expected 1 node, got %d", len(nodes))
	}

	ident, ok := nodes[0].(*ast.Identifier)
	if !ok {
		t.Fatalf("Expected Identifier, got %T", nodes[0])
	}

	if ident.Name != "foo" {
		t.Errorf("Expected name 'foo', got %q", ident.Name)
	}
}

func TestParseSimpleArithmetic(t *testing.T) {
	tests := []struct {
		source string
		op     string
	}{
		{"1 + 2", "+"},
		{"5 - 3", "-"},
		{"2 * 3", "*"},
		{"10 / 2", "/"},
		{"10 % 3", "%"},
	}

	for _, tt := range tests {
		_, nodes, err := parseSource(tt.source)
		if err != nil {
			t.Fatalf("Parse error for %q: %v", tt.source, err)
		}

		if len(nodes) != 1 {
			t.Fatalf("Expected 1 node for %q, got %d", tt.source, len(nodes))
		}

		sexpr, ok := nodes[0].(*ast.SExpr)
		if !ok {
			t.Fatalf("Expected SExpr for %q, got %T", tt.source, nodes[0])
		}

		if len(sexpr.Elements) != 3 {
			t.Fatalf("Expected 3 elements for %q, got %d", tt.source, len(sexpr.Elements))
		}

		opNode, ok := sexpr.Elements[0].(*ast.Identifier)
		if !ok {
			t.Fatalf("Expected operator to be Identifier for %q, got %T", tt.source, sexpr.Elements[0])
		}

		if opNode.Name != tt.op {
			t.Errorf("Expected operator %q for %q, got %q", tt.op, tt.source, opNode.Name)
		}
	}
}

func TestParseComparison(t *testing.T) {
	tests := []struct {
		source string
		op     string
	}{
		{"x == y", "=="},
		{"x != y", "!="},
		{"x < y", "<"},
		{"x <= y", "<="},
		{"x > y", ">"},
		{"x >= y", ">="},
	}

	for _, tt := range tests {
		_, nodes, err := parseSource(tt.source)
		if err != nil {
			t.Fatalf("Parse error for %q: %v", tt.source, err)
		}

		sexpr, ok := nodes[0].(*ast.SExpr)
		if !ok {
			t.Fatalf("Expected SExpr for %q, got %T", tt.source, nodes[0])
		}

		opNode, ok := sexpr.Elements[0].(*ast.Identifier)
		if !ok {
			t.Fatalf("Expected operator to be Identifier for %q", tt.source)
		}

		if opNode.Name != tt.op {
			t.Errorf("Expected operator %q for %q, got %q", tt.op, tt.source, opNode.Name)
		}
	}
}

func TestParseAssignment(t *testing.T) {
	_, nodes, err := parseSource("x = 42")
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	if len(nodes) != 1 {
		t.Fatalf("Expected 1 node, got %d", len(nodes))
	}

	assign, ok := nodes[0].(*ast.AssignForm)
	if !ok {
		t.Fatalf("Expected AssignForm, got %T", nodes[0])
	}

	target, ok := assign.Target.(*ast.Identifier)
	if !ok {
		t.Fatalf("Expected target to be Identifier, got %T", assign.Target)
	}

	if target.Name != "x" {
		t.Errorf("Expected target 'x', got %q", target.Name)
	}

	value, ok := assign.Value.(*ast.Literal)
	if !ok {
		t.Fatalf("Expected value to be Literal, got %T", assign.Value)
	}

	if value.Value != core.NumberValue(42) {
		t.Errorf("Expected value 42, got %v", value.Value)
	}
}

func TestParseAugmentedAssignment(t *testing.T) {
	tests := []struct {
		source string
		op     string
	}{
		{"x += 1", "+"},
		{"x -= 1", "-"},
		{"x *= 2", "*"},
		{"x /= 2", "/"},
	}

	for _, tt := range tests {
		_, nodes, err := parseSource(tt.source)
		if err != nil {
			t.Fatalf("Parse error for %q: %v", tt.source, err)
		}

		assign, ok := nodes[0].(*ast.AssignForm)
		if !ok {
			t.Fatalf("Expected AssignForm for %q, got %T", tt.source, nodes[0])
		}

		// The value should be an SExpr representing the operation
		sexpr, ok := assign.Value.(*ast.SExpr)
		if !ok {
			t.Fatalf("Expected value to be SExpr for %q, got %T", tt.source, assign.Value)
		}

		opNode, ok := sexpr.Elements[0].(*ast.Identifier)
		if !ok {
			t.Fatalf("Expected operator to be Identifier for %q", tt.source)
		}

		if opNode.Name != tt.op {
			t.Errorf("Expected operator %q for %q, got %q", tt.op, tt.source, opNode.Name)
		}
	}
}

func TestParseReturnStatement(t *testing.T) {
	tests := []struct {
		source string
		hasVal bool
	}{
		{"return", false},
		{"return 42", true},
		{"return x + y", true},
	}

	for _, tt := range tests {
		_, nodes, err := parseSource(tt.source)
		if err != nil {
			t.Fatalf("Parse error for %q: %v", tt.source, err)
		}

		returnStmt, ok := nodes[0].(*ast.ReturnForm)
		if !ok {
			t.Fatalf("Expected ReturnForm for %q, got %T", tt.source, nodes[0])
		}

		if tt.hasVal && returnStmt.Value == nil {
			t.Errorf("Expected return value for %q", tt.source)
		}
		if !tt.hasVal && returnStmt.Value != nil {
			t.Errorf("Expected no return value for %q", tt.source)
		}
	}
}

func TestParseBreakContinuePass(t *testing.T) {
	tests := []struct {
		source   string
		expected interface{}
	}{
		{"break", &ast.BreakForm{}},
		{"continue", &ast.ContinueForm{}},
		{"pass", &ast.PassForm{}},
	}

	for _, tt := range tests {
		_, nodes, err := parseSource(tt.source)
		if err != nil {
			t.Fatalf("Parse error for %q: %v", tt.source, err)
		}

		if len(nodes) != 1 {
			t.Fatalf("Expected 1 node for %q, got %d", tt.source, len(nodes))
		}

		nodeType := nodes[0]
		switch tt.expected.(type) {
		case *ast.BreakForm:
			if _, ok := nodeType.(*ast.BreakForm); !ok {
				t.Errorf("Expected BreakForm for %q, got %T", tt.source, nodeType)
			}
		case *ast.ContinueForm:
			if _, ok := nodeType.(*ast.ContinueForm); !ok {
				t.Errorf("Expected ContinueForm for %q, got %T", tt.source, nodeType)
			}
		case *ast.PassForm:
			if _, ok := nodeType.(*ast.PassForm); !ok {
				t.Errorf("Expected PassForm for %q, got %T", tt.source, nodeType)
			}
		}
	}
}

func TestParseRaiseStatement(t *testing.T) {
	tests := []struct {
		source   string
		hasExc   bool
		hasCause bool
	}{
		{"raise", false, false},
		{"raise ValueError", true, false},
		{"raise ValueError from cause", true, true},
	}

	for _, tt := range tests {
		_, nodes, err := parseSource(tt.source)
		if err != nil {
			t.Fatalf("Parse error for %q: %v", tt.source, err)
		}

		raiseStmt, ok := nodes[0].(*ast.RaiseForm)
		if !ok {
			t.Fatalf("Expected RaiseForm for %q, got %T", tt.source, nodes[0])
		}

		if tt.hasExc && raiseStmt.Exception == nil {
			t.Errorf("Expected exception for %q", tt.source)
		}
		if !tt.hasExc && raiseStmt.Exception != nil {
			t.Errorf("Expected no exception for %q", tt.source)
		}
		if tt.hasCause && raiseStmt.Cause == nil {
			t.Errorf("Expected cause for %q", tt.source)
		}
		if !tt.hasCause && raiseStmt.Cause != nil {
			t.Errorf("Expected no cause for %q", tt.source)
		}
	}
}

// ============================================================================
// Postfix Expression Tests
// ============================================================================

func TestParseFunctionCall(t *testing.T) {
	_, nodes, err := parseSource("foo(1, 2, 3)")
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	sexpr, ok := nodes[0].(*ast.SExpr)
	if !ok {
		t.Fatalf("Expected SExpr, got %T", nodes[0])
	}

	// Should be (foo 1 2 3)
	if len(sexpr.Elements) != 4 {
		t.Fatalf("Expected 4 elements (callee + 3 args), got %d", len(sexpr.Elements))
	}

	callee, ok := sexpr.Elements[0].(*ast.Identifier)
	if !ok {
		t.Fatalf("Expected callee to be Identifier, got %T", sexpr.Elements[0])
	}

	if callee.Name != "foo" {
		t.Errorf("Expected callee 'foo', got %q", callee.Name)
	}
}

func TestParseAttributeAccess(t *testing.T) {
	_, nodes, err := parseSource("obj.method")
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	sexpr, ok := nodes[0].(*ast.SExpr)
	if !ok {
		t.Fatalf("Expected SExpr, got %T", nodes[0])
	}

	// Should be (. obj "method")
	if len(sexpr.Elements) != 3 {
		t.Fatalf("Expected 3 elements, got %d", len(sexpr.Elements))
	}

	dot, ok := sexpr.Elements[0].(*ast.Identifier)
	if !ok || dot.Name != "." {
		t.Errorf("Expected '.' operator")
	}
}

func TestParseSubscript(t *testing.T) {
	_, nodes, err := parseSource("list[0]")
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	sexpr, ok := nodes[0].(*ast.SExpr)
	if !ok {
		t.Fatalf("Expected SExpr, got %T", nodes[0])
	}

	// Should be ([] list 0)
	if len(sexpr.Elements) != 3 {
		t.Fatalf("Expected 3 elements, got %d", len(sexpr.Elements))
	}

	brackets, ok := sexpr.Elements[0].(*ast.Identifier)
	if !ok || brackets.Name != "[]" {
		t.Errorf("Expected '[]' operator")
	}
}

func TestParseChainedCalls(t *testing.T) {
	_, nodes, err := parseSource("obj.method(1, 2).other[3]")
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	// Should parse as nested operations
	if nodes[0] == nil {
		t.Fatal("Expected a node")
	}

	// Just verify it parses without error
	// Detailed structure checking would be complex
}

// ============================================================================
// List/Dict/Set Literal Tests
// ============================================================================

func TestParseListLiteral(t *testing.T) {
	tests := []struct {
		source   string
		expected int
	}{
		{"[]", 0},
		{"[1]", 1},
		{"[1, 2, 3]", 3},
		{"[1, 2, 3,]", 3}, // Trailing comma
	}

	for _, tt := range tests {
		_, nodes, err := parseSource(tt.source)
		if err != nil {
			t.Fatalf("Parse error for %q: %v", tt.source, err)
		}

		literal, ok := nodes[0].(*ast.Literal)
		if !ok {
			t.Fatalf("Expected Literal for %q, got %T", tt.source, nodes[0])
		}

		list, ok := literal.Value.(core.ListValue)
		if !ok {
			t.Fatalf("Expected ListValue for %q, got %T", tt.source, literal.Value)
		}

		if len(list) != tt.expected {
			t.Errorf("Expected %d elements for %q, got %d", tt.expected, tt.source, len(list))
		}
	}
}

func TestParseDictLiteral(t *testing.T) {
	tests := []struct {
		source   string
		expected int
	}{
		{"{}", 0},
		{`{"a": 1}`, 1},
		{`{"a": 1, "b": 2}`, 2},
	}

	for _, tt := range tests {
		_, nodes, err := parseSource(tt.source)
		if err != nil {
			t.Fatalf("Parse error for %q: %v", tt.source, err)
		}

		literal, ok := nodes[0].(*ast.Literal)
		if !ok {
			t.Fatalf("Expected Literal for %q, got %T", tt.source, nodes[0])
		}

		dict, ok := literal.Value.(*core.DictValue)
		if !ok {
			t.Fatalf("Expected DictValue for %q, got %T", tt.source, literal.Value)
		}

		if dict.Size() != tt.expected {
			t.Errorf("Expected %d elements for %q, got %d", tt.expected, tt.source, dict.Size())
		}
	}
}

func TestParseSetLiteral(t *testing.T) {
	tests := []struct {
		source   string
		expected int
	}{
		{"{1}", 1},
		{"{1, 2, 3}", 3},
	}

	for _, tt := range tests {
		_, nodes, err := parseSource(tt.source)
		if err != nil {
			t.Fatalf("Parse error for %q: %v", tt.source, err)
		}

		literal, ok := nodes[0].(*ast.Literal)
		if !ok {
			t.Fatalf("Expected Literal for %q, got %T", tt.source, nodes[0])
		}

		set, ok := literal.Value.(*core.SetValue)
		if !ok {
			t.Fatalf("Expected SetValue for %q, got %T", tt.source, literal.Value)
		}

		if set.Size() != tt.expected {
			t.Errorf("Expected %d elements for %q, got %d", tt.expected, tt.source, set.Size())
		}
	}
}

// ============================================================================
// Multiple Statement Tests
// ============================================================================

func TestParseMultipleStatements(t *testing.T) {
	source := `x = 1
y = 2
z = 3`

	_, nodes, err := parseSource(source)
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	if len(nodes) != 3 {
		t.Fatalf("Expected 3 statements, got %d", len(nodes))
	}

	for i, node := range nodes {
		if _, ok := node.(*ast.AssignForm); !ok {
			t.Errorf("Statement %d: expected AssignForm, got %T", i, node)
		}
	}
}
