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

	parser := NewPythonParser(tokens, "<test>", source)
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
		augOp  string // The augmented assignment operator (+=, -=, etc.)
	}{
		{"x += 1", "+="},
		{"x -= 1", "-="},
		{"x *= 2", "*="},
		{"x /= 2", "/="},
	}

	for _, tt := range tests {
		_, nodes, err := parseSource(tt.source)
		if err != nil {
			t.Fatalf("Parse error for %q: %v", tt.source, err)
		}

		// Augmented assignment is now parsed as an SExpr: (+= x 1)
		sexpr, ok := nodes[0].(*ast.SExpr)
		if !ok {
			t.Fatalf("Expected SExpr for %q, got %T", tt.source, nodes[0])
		}

		if len(sexpr.Elements) != 3 {
			t.Fatalf("Expected 3 elements in SExpr for %q, got %d", tt.source, len(sexpr.Elements))
		}

		opNode, ok := sexpr.Elements[0].(*ast.Identifier)
		if !ok {
			t.Fatalf("Expected operator to be Identifier for %q, got %T", tt.source, sexpr.Elements[0])
		}

		if opNode.Name != tt.augOp {
			t.Errorf("Expected augmented operator %q for %q, got %q", tt.augOp, tt.source, opNode.Name)
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

	// Should be (get-item list 0)
	if len(sexpr.Elements) != 3 {
		t.Fatalf("Expected 3 elements, got %d", len(sexpr.Elements))
	}

	getItem, ok := sexpr.Elements[0].(*ast.Identifier)
	if !ok || getItem.Name != "get-item" {
		t.Errorf("Expected 'get-item' operator, got %v", getItem.Name)
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

		// Empty lists remain as Literal nodes, non-empty lists are SExpr
		if tt.expected == 0 {
			literal, ok := nodes[0].(*ast.Literal)
			if !ok {
				t.Fatalf("Expected Literal for empty list %q, got %T", tt.source, nodes[0])
			}

			list, ok := literal.Value.(*core.ListValue)
			if !ok {
				t.Fatalf("Expected ListValue for %q, got %T", tt.source, literal.Value)
			}

			if list.Len() != 0 {
				t.Errorf("Expected empty list for %q, got %d elements", tt.source, list.Len())
			}
		} else {
			// List literals are now SExpr nodes representing (list-literal ...)
			sexpr, ok := nodes[0].(*ast.SExpr)
			if !ok {
				t.Fatalf("Expected SExpr for %q, got %T", tt.source, nodes[0])
			}

			// Check it starts with "list-literal"
			if len(sexpr.Elements) < 1 {
				t.Fatalf("Expected at least 1 element in SExpr for %q", tt.source)
			}

			ident, ok := sexpr.Elements[0].(*ast.Identifier)
			if !ok || ident.Name != "list-literal" {
				t.Fatalf("Expected first element to be 'list-literal' for %q, got %v", tt.source, sexpr.Elements[0])
			}

			// Number of elements is total - 1 (excluding list-literal symbol)
			if len(sexpr.Elements)-1 != tt.expected {
				t.Errorf("Expected %d elements for %q, got %d", tt.expected, tt.source, len(sexpr.Elements)-1)
			}
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

		// Empty dicts remain as Literal nodes, non-empty dicts are SExpr
		if tt.expected == 0 {
			literal, ok := nodes[0].(*ast.Literal)
			if !ok {
				t.Fatalf("Expected Literal for empty dict %q, got %T", tt.source, nodes[0])
			}

			dict, ok := literal.Value.(*core.DictValue)
			if !ok {
				t.Fatalf("Expected DictValue for %q, got %T", tt.source, literal.Value)
			}

			if dict.Size() != 0 {
				t.Errorf("Expected empty dict for %q, got %d elements", tt.source, dict.Size())
			}
		} else {
			// Dict literals are now SExpr nodes representing (dict-literal k1 v1 k2 v2 ...)
			sexpr, ok := nodes[0].(*ast.SExpr)
			if !ok {
				t.Fatalf("Expected SExpr for %q, got %T", tt.source, nodes[0])
			}

			// Check it starts with "dict-literal"
			if len(sexpr.Elements) < 1 {
				t.Fatalf("Expected at least 1 element in SExpr for %q", tt.source)
			}

			ident, ok := sexpr.Elements[0].(*ast.Identifier)
			if !ok || ident.Name != "dict-literal" {
				t.Fatalf("Expected first element to be 'dict-literal' for %q, got %v", tt.source, sexpr.Elements[0])
			}

			// Number of key-value pairs is (total - 1) / 2
			numPairs := (len(sexpr.Elements) - 1) / 2
			if numPairs != tt.expected {
				t.Errorf("Expected %d pairs for %q, got %d", tt.expected, tt.source, numPairs)
			}
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

		// Set literals are now SExpr nodes representing (set (list-literal ...))
		sexpr, ok := nodes[0].(*ast.SExpr)
		if !ok {
			t.Fatalf("Expected SExpr for %q, got %T", tt.source, nodes[0])
		}

		// Check it starts with "set"
		if len(sexpr.Elements) != 2 {
			t.Fatalf("Expected 2 elements in set SExpr for %q, got %d", tt.source, len(sexpr.Elements))
		}

		ident, ok := sexpr.Elements[0].(*ast.Identifier)
		if !ok || ident.Name != "set" {
			t.Fatalf("Expected first element to be 'set' for %q, got %v", tt.source, sexpr.Elements[0])
		}

		// Second element should be (list-literal ...)
		listLiteral, ok := sexpr.Elements[1].(*ast.SExpr)
		if !ok {
			t.Fatalf("Expected second element to be SExpr for %q, got %T", tt.source, sexpr.Elements[1])
		}

		listIdent, ok := listLiteral.Elements[0].(*ast.Identifier)
		if !ok || listIdent.Name != "list-literal" {
			t.Fatalf("Expected 'list-literal' in set for %q, got %v", tt.source, listLiteral.Elements[0])
		}

		// Number of elements is total - 1 (excluding list-literal symbol)
		numElements := len(listLiteral.Elements) - 1
		if numElements != tt.expected {
			t.Errorf("Expected %d elements for %q, got %d", tt.expected, tt.source, numElements)
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

// ============================================================================
// Block Statement Tests
// ============================================================================

func TestParseIfStatement(t *testing.T) {
	source := `if x > 0:
    print("positive")`

	_, nodes, err := parseSource(source)
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	if len(nodes) != 1 {
		t.Fatalf("Expected 1 node, got %d", len(nodes))
	}

	ifStmt, ok := nodes[0].(*ast.IfForm)
	if !ok {
		t.Fatalf("Expected IfForm, got %T", nodes[0])
	}

	if ifStmt.Condition == nil {
		t.Error("Expected condition")
	}
	if ifStmt.ThenBranch == nil {
		t.Error("Expected then branch")
	}
}

func TestParseIfElseStatement(t *testing.T) {
	source := `if x > 0:
    print("positive")
else:
    print("non-positive")`

	_, nodes, err := parseSource(source)
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	ifStmt, ok := nodes[0].(*ast.IfForm)
	if !ok {
		t.Fatalf("Expected IfForm, got %T", nodes[0])
	}

	if ifStmt.ElseBranch == nil {
		t.Error("Expected else branch")
	}
}

func TestParseIfElifElse(t *testing.T) {
	source := `if x > 0:
    print("positive")
elif x < 0:
    print("negative")
else:
    print("zero")`

	_, nodes, err := parseSource(source)
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	ifStmt, ok := nodes[0].(*ast.IfForm)
	if !ok {
		t.Fatalf("Expected IfForm, got %T", nodes[0])
	}

	// elif should be nested as another IfForm
	if ifStmt.ElseBranch == nil {
		t.Error("Expected else branch (containing elif)")
	}
}

func TestParseForLoop(t *testing.T) {
	source := `for i in range(10):
    print(i)`

	_, nodes, err := parseSource(source)
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	forLoop, ok := nodes[0].(*ast.ForForm)
	if !ok {
		t.Fatalf("Expected ForForm, got %T", nodes[0])
	}

	if forLoop.Variable != "i" {
		t.Errorf("Expected variable 'i', got %q", forLoop.Variable)
	}
	if len(forLoop.Body) == 0 {
		t.Error("Expected non-empty body")
	}
}

func TestParseForLoopWithElse(t *testing.T) {
	source := `for i in range(10):
    print(i)
else:
    print("done")`

	_, nodes, err := parseSource(source)
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	forLoop, ok := nodes[0].(*ast.ForForm)
	if !ok {
		t.Fatalf("Expected ForForm, got %T", nodes[0])
	}

	if len(forLoop.ElseBody) == 0 {
		t.Error("Expected else body")
	}
}

func TestParseWhileLoop(t *testing.T) {
	source := `while x > 0:
    x = x - 1`

	_, nodes, err := parseSource(source)
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	whileLoop, ok := nodes[0].(*ast.WhileForm)
	if !ok {
		t.Fatalf("Expected WhileForm, got %T", nodes[0])
	}

	if whileLoop.Condition == nil {
		t.Error("Expected condition")
	}
	if len(whileLoop.Body) == 0 {
		t.Error("Expected non-empty body")
	}
}

func TestParseSimpleFunctionDef(t *testing.T) {
	source := `def foo(x, y):
    return x + y`

	_, nodes, err := parseSource(source)
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	defForm, ok := nodes[0].(*ast.DefForm)
	if !ok {
		t.Fatalf("Expected DefForm, got %T", nodes[0])
	}

	if defForm.Name != "foo" {
		t.Errorf("Expected name 'foo', got %q", defForm.Name)
	}
	if len(defForm.Params) != 2 {
		t.Errorf("Expected 2 parameters, got %d", len(defForm.Params))
	}
	if defForm.Body == nil {
		t.Error("Expected body")
	}
}

func TestParseFunctionDefWithTypes(t *testing.T) {
	source := `def add(x: int, y: int) -> int:
    return x + y`

	_, nodes, err := parseSource(source)
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	defForm, ok := nodes[0].(*ast.DefForm)
	if !ok {
		t.Fatalf("Expected DefForm, got %T", nodes[0])
	}

	if defForm.Params[0].Type == nil {
		t.Error("Expected type annotation on first parameter")
	}
	if defForm.ReturnType == nil {
		t.Error("Expected return type annotation")
	}
	if defForm.ReturnType.Name != "int" {
		t.Errorf("Expected return type 'int', got %q", defForm.ReturnType.Name)
	}
}

func TestParseFunctionDefWithDefaults(t *testing.T) {
	source := `def foo(x, y=10):
    return x + y`

	_, nodes, err := parseSource(source)
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	defForm, ok := nodes[0].(*ast.DefForm)
	if !ok {
		t.Fatalf("Expected DefForm, got %T", nodes[0])
	}

	if defForm.Params[1].Default == nil {
		t.Error("Expected default value on second parameter")
	}
}

func TestParseFunctionDefWithDecorator(t *testing.T) {
	source := `@decorator
def foo():
    pass`

	_, nodes, err := parseSource(source)
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	defForm, ok := nodes[0].(*ast.DefForm)
	if !ok {
		t.Fatalf("Expected DefForm, got %T", nodes[0])
	}

	if len(defForm.Decorators) != 1 {
		t.Errorf("Expected 1 decorator, got %d", len(defForm.Decorators))
	}
}

func TestParseSimpleClass(t *testing.T) {
	source := `class Foo:
    pass`

	_, nodes, err := parseSource(source)
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	classForm, ok := nodes[0].(*ast.ClassForm)
	if !ok {
		t.Fatalf("Expected ClassForm, got %T", nodes[0])
	}

	if classForm.Name != "Foo" {
		t.Errorf("Expected name 'Foo', got %q", classForm.Name)
	}
}

func TestParseClassWithBase(t *testing.T) {
	source := `class Foo(Bar):
    pass`

	_, nodes, err := parseSource(source)
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	classForm, ok := nodes[0].(*ast.ClassForm)
	if !ok {
		t.Fatalf("Expected ClassForm, got %T", nodes[0])
	}

	if len(classForm.Bases) != 1 {
		t.Errorf("Expected 1 base class, got %d", len(classForm.Bases))
	}
}

func TestParseClassWithDecorator(t *testing.T) {
	source := `@dataclass
class Foo:
    pass`

	_, nodes, err := parseSource(source)
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	classForm, ok := nodes[0].(*ast.ClassForm)
	if !ok {
		t.Fatalf("Expected ClassForm, got %T", nodes[0])
	}

	if len(classForm.Decorators) != 1 {
		t.Errorf("Expected 1 decorator, got %d", len(classForm.Decorators))
	}
}

func TestParseTryExcept(t *testing.T) {
	source := `try:
    risky()
except ValueError:
    handle()`

	_, nodes, err := parseSource(source)
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	tryForm, ok := nodes[0].(*ast.TryForm)
	if !ok {
		t.Fatalf("Expected TryForm, got %T", nodes[0])
	}

	if len(tryForm.TryBody) == 0 {
		t.Error("Expected try body")
	}
	if len(tryForm.ExceptClauses) != 1 {
		t.Errorf("Expected 1 except clause, got %d", len(tryForm.ExceptClauses))
	}
	if tryForm.ExceptClauses[0].ExceptionType != "ValueError" {
		t.Errorf("Expected exception type 'ValueError', got %q", tryForm.ExceptClauses[0].ExceptionType)
	}
}

func TestParseTryExceptAsVariable(t *testing.T) {
	source := `try:
    risky()
except ValueError as e:
    handle(e)`

	_, nodes, err := parseSource(source)
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	tryForm, ok := nodes[0].(*ast.TryForm)
	if !ok {
		t.Fatalf("Expected TryForm, got %T", nodes[0])
	}

	if tryForm.ExceptClauses[0].Variable != "e" {
		t.Errorf("Expected variable 'e', got %q", tryForm.ExceptClauses[0].Variable)
	}
}

func TestParseTryExceptFinally(t *testing.T) {
	source := `try:
    risky()
except:
    handle()
finally:
    cleanup()`

	_, nodes, err := parseSource(source)
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	tryForm, ok := nodes[0].(*ast.TryForm)
	if !ok {
		t.Fatalf("Expected TryForm, got %T", nodes[0])
	}

	if len(tryForm.FinallyBody) == 0 {
		t.Error("Expected finally body")
	}
}

func TestParseWithStatement(t *testing.T) {
	source := `with open("file.txt") as f:
    data = f.read()`

	_, nodes, err := parseSource(source)
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	withForm, ok := nodes[0].(*ast.WithForm)
	if !ok {
		t.Fatalf("Expected WithForm, got %T", nodes[0])
	}

	if len(withForm.Items) != 1 {
		t.Errorf("Expected 1 context manager, got %d", len(withForm.Items))
	}
	if withForm.Items[0].Variable != "f" {
		t.Errorf("Expected variable 'f', got %q", withForm.Items[0].Variable)
	}
	if len(withForm.Body) == 0 {
		t.Error("Expected non-empty body")
	}
}

func TestParseWithMultipleContexts(t *testing.T) {
	source := `with open("in.txt") as f, open("out.txt") as g:
    g.write(f.read())`

	_, nodes, err := parseSource(source)
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	withForm, ok := nodes[0].(*ast.WithForm)
	if !ok {
		t.Fatalf("Expected WithForm, got %T", nodes[0])
	}

	if len(withForm.Items) != 2 {
		t.Errorf("Expected 2 context managers, got %d", len(withForm.Items))
	}
}

// ============================================================================
// Integration Tests
// ============================================================================

func TestParseCompleteProgram(t *testing.T) {
	source := `def factorial(n: int) -> int:
    if n <= 1:
        return 1
    return n * factorial(n - 1)

result = factorial(5)
print(result)`

	_, nodes, err := parseSource(source)
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	if len(nodes) != 3 {
		t.Fatalf("Expected 3 statements, got %d", len(nodes))
	}

	// First should be function definition
	if _, ok := nodes[0].(*ast.DefForm); !ok {
		t.Errorf("Expected first node to be DefForm, got %T", nodes[0])
	}

	// Second should be assignment
	if _, ok := nodes[1].(*ast.AssignForm); !ok {
		t.Errorf("Expected second node to be AssignForm, got %T", nodes[1])
	}

	// Third should be expression (function call)
	if nodes[2] == nil {
		t.Error("Expected third node")
	}
}

func TestParseClassWithMethods(t *testing.T) {
	source := `class Counter:
    def __init__(self, start: int = 0):
        self.value = start

    def increment(self):
        self.value = self.value + 1
        return self.value`

	_, nodes, err := parseSource(source)
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	classForm, ok := nodes[0].(*ast.ClassForm)
	if !ok {
		t.Fatalf("Expected ClassForm, got %T", nodes[0])
	}

	if len(classForm.Body) != 2 {
		t.Errorf("Expected 2 methods in class body, got %d", len(classForm.Body))
	}
}

// ============================================================================
// Comprehension Tests
// ============================================================================

func TestParseListComprehension(t *testing.T) {
	source := `[x * x for x in range(10)]`

	_, nodes, err := parseSource(source)
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	comp, ok := nodes[0].(*ast.ComprehensionForm)
	if !ok {
		t.Fatalf("Expected ComprehensionForm, got %T", nodes[0])
	}

	if comp.Kind != ast.ListComp {
		t.Errorf("Expected ListComp, got %v", comp.Kind)
	}

	if comp.Variable != "x" {
		t.Errorf("Expected variable 'x', got %q", comp.Variable)
	}

	if comp.Condition != nil {
		t.Error("Expected no condition")
	}
}

func TestParseListComprehensionWithCondition(t *testing.T) {
	source := `[x * x for x in range(10) if x % 2 == 0]`

	_, nodes, err := parseSource(source)
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	comp, ok := nodes[0].(*ast.ComprehensionForm)
	if !ok {
		t.Fatalf("Expected ComprehensionForm, got %T", nodes[0])
	}

	if comp.Kind != ast.ListComp {
		t.Errorf("Expected ListComp, got %v", comp.Kind)
	}

	if comp.Condition == nil {
		t.Error("Expected condition")
	}
}

func TestParseDictComprehension(t *testing.T) {
	source := `{x: x * 2 for x in range(10)}`

	_, nodes, err := parseSource(source)
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	comp, ok := nodes[0].(*ast.ComprehensionForm)
	if !ok {
		t.Fatalf("Expected ComprehensionForm, got %T", nodes[0])
	}

	if comp.Kind != ast.DictComp {
		t.Errorf("Expected DictComp, got %v", comp.Kind)
	}

	if comp.Variable != "x" {
		t.Errorf("Expected variable 'x', got %q", comp.Variable)
	}

	if comp.KeyExpr == nil || comp.ValueExpr == nil {
		t.Error("Expected both key and value expressions")
	}
}

func TestParseSetComprehension(t *testing.T) {
	source := `{x * 2 for x in numbers if x > 0}`

	_, nodes, err := parseSource(source)
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	comp, ok := nodes[0].(*ast.ComprehensionForm)
	if !ok {
		t.Fatalf("Expected ComprehensionForm, got %T", nodes[0])
	}

	if comp.Kind != ast.SetComp {
		t.Errorf("Expected SetComp, got %v", comp.Kind)
	}

	if comp.Variable != "x" {
		t.Errorf("Expected variable 'x', got %q", comp.Variable)
	}
}

func TestParseGeneratorExpression(t *testing.T) {
	source := `(x for x in range(100) if x % 3 == 0)`

	_, nodes, err := parseSource(source)
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	comp, ok := nodes[0].(*ast.ComprehensionForm)
	if !ok {
		t.Fatalf("Expected ComprehensionForm, got %T", nodes[0])
	}

	if comp.Kind != ast.GeneratorComp {
		t.Errorf("Expected GeneratorComp, got %v", comp.Kind)
	}

	if comp.Variable != "x" {
		t.Errorf("Expected variable 'x', got %q", comp.Variable)
	}
}
