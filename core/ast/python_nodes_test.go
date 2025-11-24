package ast

import (
	"testing"

	"github.com/mmichie/m28/core"
)

// unwrap is a test helper that unwraps LocatedValue
func unwrapPython(v core.Value) core.Value {
	if lv, ok := v.(core.LocatedValue); ok {
		return lv.Unwrap()
	}
	return v
}

// ============================================================================
// Control Flow Tests
// ============================================================================

func TestBreakForm(t *testing.T) {
	loc := &core.SourceLocation{Line: 1, Column: 1}
	breakStmt := NewBreakForm(loc, SyntaxPython)

	// Check construction
	if breakStmt.Location() != loc {
		t.Error("Location not set correctly")
	}
	if breakStmt.SyntaxKind() != SyntaxPython {
		t.Error("SyntaxKind should be Python")
	}

	// Check ToIR
	ir := breakStmt.ToIR()
	list, ok := ir.(*core.ListValue)
	if !ok {
		t.Fatalf("Expected ListValue, got %T", ir)
	}
	items := list.Items()
	if len(items) != 1 {
		t.Fatalf("Expected 1 element, got %d", len(items))
	}
	if sym, ok := items[0].(core.SymbolValue); !ok || string(sym) != "break" {
		t.Errorf("Expected 'break' symbol, got %v", items[0])
	}
}

func TestContinueForm(t *testing.T) {
	loc := &core.SourceLocation{Line: 1, Column: 1}
	continueStmt := NewContinueForm(loc, SyntaxPython)

	// Check ToIR
	ir := continueStmt.ToIR()
	list, ok := ir.(*core.ListValue)
	if !ok {
		t.Fatalf("Expected ListValue, got %T", ir)
	}
	items := list.Items()
	if len(items) != 1 {
		t.Fatalf("Expected 1 element, got %d", len(items))
	}
	if sym, ok := items[0].(core.SymbolValue); !ok || string(sym) != "continue" {
		t.Errorf("Expected 'continue' symbol, got %v", items[0])
	}
}

func TestReturnForm_WithValue(t *testing.T) {
	loc := &core.SourceLocation{Line: 1, Column: 1}
	value := NewLiteral(core.NumberValue(42), loc, SyntaxPython)
	returnStmt := NewReturnForm(value, loc, SyntaxPython)

	// Check ToIR: (return 42)
	ir := returnStmt.ToIR()
	list, ok := ir.(*core.ListValue)
	if !ok {
		t.Fatalf("Expected ListValue, got %T", ir)
	}
	items := list.Items()
	if len(items) != 2 {
		t.Fatalf("Expected 2 elements, got %d", len(items))
	}
	if sym, ok := unwrapPython(items[0]).(core.SymbolValue); !ok || string(sym) != "return" {
		t.Errorf("Expected 'return' symbol, got %v", items[0])
	}
	if num, ok := unwrapPython(items[1]).(core.NumberValue); !ok || float64(num) != 42 {
		t.Errorf("Expected 42, got %v", items[1])
	}
}

func TestReturnForm_Bare(t *testing.T) {
	loc := &core.SourceLocation{Line: 1, Column: 1}
	returnStmt := NewReturnForm(nil, loc, SyntaxPython)

	// Check ToIR: (return None)
	ir := returnStmt.ToIR()
	list, ok := ir.(*core.ListValue)
	if !ok {
		t.Fatalf("Expected ListValue, got %T", ir)
	}
	items := list.Items()
	if len(items) != 2 {
		t.Fatalf("Expected 2 elements, got %d", len(items))
	}
	if items[1] != core.None {
		t.Errorf("Expected None, got %v", items[1])
	}
}

func TestRaiseForm_Simple(t *testing.T) {
	loc := &core.SourceLocation{Line: 1, Column: 1}
	exception := NewIdentifier("ValueError", loc, SyntaxPython)
	raiseStmt := NewRaiseForm(exception, nil, loc, SyntaxPython)

	// Check ToIR: (raise ValueError)
	ir := raiseStmt.ToIR()
	list, ok := ir.(*core.ListValue)
	if !ok {
		t.Fatalf("Expected ListValue, got %T", ir)
	}
	items := list.Items()
	if len(items) != 2 {
		t.Fatalf("Expected 2 elements, got %d", len(items))
	}
	if sym, ok := unwrapPython(items[0]).(core.SymbolValue); !ok || string(sym) != "raise" {
		t.Errorf("Expected 'raise' symbol, got %v", items[0])
	}
	if sym, ok := unwrapPython(items[1]).(core.SymbolValue); !ok || string(sym) != "ValueError" {
		t.Errorf("Expected 'ValueError' symbol, got %v", items[1])
	}
}

func TestRaiseForm_WithCause(t *testing.T) {
	loc := &core.SourceLocation{Line: 1, Column: 1}
	exception := NewIdentifier("ValueError", loc, SyntaxPython)
	cause := NewIdentifier("KeyError", loc, SyntaxPython)
	raiseStmt := NewRaiseForm(exception, cause, loc, SyntaxPython)

	// Check ToIR: (raise ValueError from KeyError)
	ir := raiseStmt.ToIR()
	list, ok := ir.(*core.ListValue)
	if !ok {
		t.Fatalf("Expected ListValue, got %T", ir)
	}
	items := list.Items()
	if len(items) != 4 {
		t.Fatalf("Expected 4 elements, got %d", len(items))
	}
	if sym, ok := items[2].(core.SymbolValue); !ok || string(sym) != "from" {
		t.Errorf("Expected 'from' symbol at position 2, got %v", items[2])
	}
}

func TestRaiseForm_Bare(t *testing.T) {
	loc := &core.SourceLocation{Line: 1, Column: 1}
	raiseStmt := NewRaiseForm(nil, nil, loc, SyntaxPython)

	// Check ToIR: (raise)
	ir := raiseStmt.ToIR()
	list, ok := ir.(*core.ListValue)
	if !ok {
		t.Fatalf("Expected ListValue, got %T", ir)
	}
	items := list.Items()
	if len(items) != 1 {
		t.Fatalf("Expected 1 element, got %d", len(items))
	}
}

func TestPassForm(t *testing.T) {
	loc := &core.SourceLocation{Line: 1, Column: 1}
	passStmt := NewPassForm(loc, SyntaxPython)

	// Check ToIR: should be None
	ir := passStmt.ToIR()
	if ir != core.None {
		t.Errorf("Expected None, got %v", ir)
	}
}

// ============================================================================
// BlockForm Tests
// ============================================================================

func TestBlockForm(t *testing.T) {
	loc := &core.SourceLocation{Line: 1, Column: 1}
	stmt1 := NewLiteral(core.NumberValue(1), loc, SyntaxPython)
	stmt2 := NewLiteral(core.NumberValue(2), loc, SyntaxPython)
	stmt3 := NewLiteral(core.NumberValue(3), loc, SyntaxPython)
	block := NewBlockForm([]ASTNode{stmt1, stmt2, stmt3}, loc, SyntaxPython)

	// Check ToIR: (do 1 2 3)
	ir := block.ToIR()
	list, ok := ir.(*core.ListValue)
	if !ok {
		t.Fatalf("Expected ListValue, got %T", ir)
	}
	items := list.Items()
	if len(items) != 4 {
		t.Fatalf("Expected 4 elements (do + 3 stmts), got %d", len(items))
	}
	if sym, ok := items[0].(core.SymbolValue); !ok || string(sym) != "do" {
		t.Errorf("Expected 'do' symbol, got %v", items[0])
	}
}

// ============================================================================
// Loop Tests
// ============================================================================

func TestForForm_Simple(t *testing.T) {
	loc := &core.SourceLocation{Line: 1, Column: 1}
	iterable := NewIdentifier("items", loc, SyntaxPython)
	bodyStmt := NewIdentifier("body", loc, SyntaxPython)
	forLoop := NewForForm("x", iterable, []ASTNode{bodyStmt}, nil, loc, SyntaxPython)

	// Check ToIR: (for x items (do body))
	ir := forLoop.ToIR()
	list, ok := ir.(*core.ListValue)
	if !ok {
		t.Fatalf("Expected ListValue, got %T", ir)
	}
	items := list.Items()
	if len(items) != 4 {
		t.Fatalf("Expected 4 elements, got %d", len(items))
	}
	if sym, ok := items[0].(core.SymbolValue); !ok || string(sym) != "for" {
		t.Errorf("Expected 'for' symbol, got %v", items[0])
	}
	if sym, ok := items[1].(core.SymbolValue); !ok || string(sym) != "x" {
		t.Errorf("Expected 'x' variable, got %v", items[1])
	}
}

func TestForForm_WithElse(t *testing.T) {
	loc := &core.SourceLocation{Line: 1, Column: 1}
	iterable := NewIdentifier("items", loc, SyntaxPython)
	bodyStmt := NewIdentifier("body", loc, SyntaxPython)
	elseStmt := NewIdentifier("else_body", loc, SyntaxPython)
	forLoop := NewForForm("x", iterable, []ASTNode{bodyStmt}, []ASTNode{elseStmt}, loc, SyntaxPython)

	// Check ToIR has 5 elements (includes else block)
	ir := forLoop.ToIR()
	list, ok := ir.(*core.ListValue)
	if !ok {
		t.Fatalf("Expected ListValue, got %T", ir)
	}
	items := list.Items()
	if len(items) != 5 {
		t.Fatalf("Expected 5 elements (for + var + iter + body + else), got %d", len(items))
	}
}

func TestWhileForm_Simple(t *testing.T) {
	loc := &core.SourceLocation{Line: 1, Column: 1}
	condition := NewIdentifier("condition", loc, SyntaxPython)
	bodyStmt := NewIdentifier("body", loc, SyntaxPython)
	whileLoop := NewWhileForm(condition, []ASTNode{bodyStmt}, nil, loc, SyntaxPython)

	// Check ToIR: (while condition (do body))
	ir := whileLoop.ToIR()
	list, ok := ir.(*core.ListValue)
	if !ok {
		t.Fatalf("Expected ListValue, got %T", ir)
	}
	items := list.Items()
	if len(items) != 3 {
		t.Fatalf("Expected 3 elements, got %d", len(items))
	}
	if sym, ok := items[0].(core.SymbolValue); !ok || string(sym) != "while" {
		t.Errorf("Expected 'while' symbol, got %v", items[0])
	}
}

func TestWhileForm_WithElse(t *testing.T) {
	loc := &core.SourceLocation{Line: 1, Column: 1}
	condition := NewIdentifier("condition", loc, SyntaxPython)
	bodyStmt := NewIdentifier("body", loc, SyntaxPython)
	elseStmt := NewIdentifier("else_body", loc, SyntaxPython)
	whileLoop := NewWhileForm(condition, []ASTNode{bodyStmt}, []ASTNode{elseStmt}, loc, SyntaxPython)

	// Check ToIR has 4 elements (includes else block)
	ir := whileLoop.ToIR()
	list, ok := ir.(*core.ListValue)
	if !ok {
		t.Fatalf("Expected ListValue, got %T", ir)
	}
	items := list.Items()
	if len(items) != 4 {
		t.Fatalf("Expected 4 elements (while + cond + body + else), got %d", len(items))
	}
}

// ============================================================================
// Comprehension Tests
// ============================================================================

func TestComprehensionForm_ListComp_NoFilter(t *testing.T) {
	loc := &core.SourceLocation{Line: 1, Column: 1}
	// [x*2 for x in items]
	element := NewIdentifier("x", loc, SyntaxPython) // simplified
	iterable := NewIdentifier("items", loc, SyntaxPython)
	comp := NewComprehensionForm(ListComp, element, nil, nil, "x", iterable, nil, loc, SyntaxPython)

	// Check ToIR: (list-comp x x items)
	ir := comp.ToIR()
	list, ok := ir.(*core.ListValue)
	if !ok {
		t.Fatalf("Expected ListValue, got %T", ir)
	}
	if list.Len() != 4 {
		t.Fatalf("Expected 4 elements, got %d", list.Len())
	}
	if sym, ok := list.Items()[0].(core.SymbolValue); !ok || string(sym) != "list-comp" {
		t.Errorf("Expected 'list-comp' symbol, got %v", list.Items()[0])
	}

	// Check variable symbol
	if sym, ok := list.Items()[2].(core.SymbolValue); !ok || string(sym) != "x" {
		t.Errorf("Expected variable 'x' symbol, got %v", list.Items()[2])
	}
}

func TestComprehensionForm_ListComp_WithFilter(t *testing.T) {
	loc := &core.SourceLocation{Line: 1, Column: 1}
	// [x for x in items if x > 0]
	element := NewIdentifier("x", loc, SyntaxPython)
	iterable := NewIdentifier("items", loc, SyntaxPython)
	condition := NewIdentifier("condition", loc, SyntaxPython)
	comp := NewComprehensionForm(ListComp, element, nil, nil, "x", iterable, condition, loc, SyntaxPython)

	// Check ToIR: (list-comp x x items condition)
	ir := comp.ToIR()
	list, ok := ir.(*core.ListValue)
	if !ok {
		t.Fatalf("Expected ListValue, got %T", ir)
	}
	if list.Len() != 5 {
		t.Fatalf("Expected 5 elements (with filter), got %d", list.Len())
	}
}

func TestComprehensionForm_DictComp(t *testing.T) {
	loc := &core.SourceLocation{Line: 1, Column: 1}
	// {k: v for k, v in items}
	keyExpr := NewIdentifier("k", loc, SyntaxPython)
	valueExpr := NewIdentifier("v", loc, SyntaxPython)
	iterable := NewIdentifier("items", loc, SyntaxPython)
	comp := NewComprehensionForm(DictComp, nil, keyExpr, valueExpr, "k", iterable, nil, loc, SyntaxPython)

	// Check ToIR: (dict-comp key-expr value-expr var iterable)
	ir := comp.ToIR()
	list, ok := ir.(*core.ListValue)
	if !ok {
		t.Fatalf("Expected ListValue, got %T", ir)
	}
	if list.Len() != 5 {
		t.Fatalf("Expected 5 elements (dict-comp, key-expr, value-expr, var, iterable), got %d", list.Len())
	}
	if sym, ok := list.Items()[0].(core.SymbolValue); !ok || string(sym) != "dict-comp" {
		t.Errorf("Expected 'dict-comp' symbol, got %v", list.Items()[0])
	}
	// Verify variable is a symbol
	if sym, ok := list.Items()[3].(core.SymbolValue); !ok || string(sym) != "k" {
		t.Errorf("Expected variable 'k', got %v", list.Items()[3])
	}
}

func TestComprehensionForm_SetComp(t *testing.T) {
	loc := &core.SourceLocation{Line: 1, Column: 1}
	// {x for x in items}
	element := NewIdentifier("x", loc, SyntaxPython)
	iterable := NewIdentifier("items", loc, SyntaxPython)
	comp := NewComprehensionForm(SetComp, element, nil, nil, "x", iterable, nil, loc, SyntaxPython)

	// Check ToIR
	ir := comp.ToIR()
	list, ok := ir.(*core.ListValue)
	if !ok {
		t.Fatalf("Expected ListValue, got %T", ir)
	}
	if sym, ok := list.Items()[0].(core.SymbolValue); !ok || string(sym) != "set-comp" {
		t.Errorf("Expected 'set-comp' symbol, got %v", list.Items()[0])
	}
}

func TestComprehensionForm_GeneratorComp(t *testing.T) {
	loc := &core.SourceLocation{Line: 1, Column: 1}
	// (x for x in items)
	element := NewIdentifier("x", loc, SyntaxPython)
	iterable := NewIdentifier("items", loc, SyntaxPython)
	comp := NewComprehensionForm(GeneratorComp, element, nil, nil, "x", iterable, nil, loc, SyntaxPython)

	// Check ToIR
	ir := comp.ToIR()
	list, ok := ir.(*core.ListValue)
	if !ok {
		t.Fatalf("Expected ListValue, got %T", ir)
	}
	if sym, ok := list.Items()[0].(core.SymbolValue); !ok || string(sym) != "gen-comp" {
		t.Errorf("Expected 'gen-comp' symbol, got %v", list.Items()[0])
	}
}

// ============================================================================
// With Statement Tests
// ============================================================================

func TestWithForm_Single(t *testing.T) {
	loc := &core.SourceLocation{Line: 1, Column: 1}
	// with open("file") as f: body
	context := NewIdentifier("open_expr", loc, SyntaxPython)
	bodyStmt := NewIdentifier("body", loc, SyntaxPython)
	items := []WithItem{{Context: context, Variable: "f"}}
	withStmt := NewWithForm(items, []ASTNode{bodyStmt}, loc, SyntaxPython)

	// Check ToIR: (with open_expr f (do body))
	ir := withStmt.ToIR()
	list, ok := ir.(*core.ListValue)
	if !ok {
		t.Fatalf("Expected ListValue, got %T", ir)
	}
	if list.Len() != 4 {
		t.Fatalf("Expected 4 elements, got %d", list.Len())
	}
	if sym, ok := list.Items()[0].(core.SymbolValue); !ok || string(sym) != "with" {
		t.Errorf("Expected 'with' symbol, got %v", list.Items()[0])
	}
	if sym, ok := list.Items()[2].(core.SymbolValue); !ok || string(sym) != "f" {
		t.Errorf("Expected 'f' variable, got %v", list.Items()[2])
	}
}

func TestWithForm_NoVariable(t *testing.T) {
	loc := &core.SourceLocation{Line: 1, Column: 1}
	// with context: body
	context := NewIdentifier("context", loc, SyntaxPython)
	bodyStmt := NewIdentifier("body", loc, SyntaxPython)
	items := []WithItem{{Context: context, Variable: ""}}
	withStmt := NewWithForm(items, []ASTNode{bodyStmt}, loc, SyntaxPython)

	// Check ToIR: (with context None (do body))
	ir := withStmt.ToIR()
	list, ok := ir.(*core.ListValue)
	if !ok {
		t.Fatalf("Expected ListValue, got %T", ir)
	}
	if list.Items()[2] != core.None {
		t.Errorf("Expected None for missing variable, got %v", list.Items()[2])
	}
}

func TestWithForm_Multiple(t *testing.T) {
	loc := &core.SourceLocation{Line: 1, Column: 1}
	// with x as a, y as b: body
	ctx1 := NewIdentifier("x", loc, SyntaxPython)
	ctx2 := NewIdentifier("y", loc, SyntaxPython)
	bodyStmt := NewIdentifier("body", loc, SyntaxPython)
	items := []WithItem{
		{Context: ctx1, Variable: "a"},
		{Context: ctx2, Variable: "b"},
	}
	withStmt := NewWithForm(items, []ASTNode{bodyStmt}, loc, SyntaxPython)

	// Check ToIR: (with x a (with y b (do body)))
	ir := withStmt.ToIR()
	list, ok := ir.(*core.ListValue)
	if !ok {
		t.Fatalf("Expected ListValue, got %T", ir)
	}
	if sym, ok := list.Items()[0].(core.SymbolValue); !ok || string(sym) != "with" {
		t.Errorf("Expected 'with' symbol, got %v", list.Items()[0])
	}

	// Check nested structure
	innerWith, ok := list.Items()[3].(*core.ListValue)
	if !ok {
		t.Fatalf("Expected nested with, got %T", list.Items()[3])
	}
	if sym, ok := innerWith.Items()[0].(core.SymbolValue); !ok || string(sym) != "with" {
		t.Errorf("Expected nested 'with' symbol, got %v", innerWith.Items()[0])
	}
}

// ============================================================================
// Try Statement Tests
// ============================================================================

func TestTryForm_SimpleExcept(t *testing.T) {
	loc := &core.SourceLocation{Line: 1, Column: 1}
	// try: body except ValueError: handler
	tryBody := []ASTNode{NewIdentifier("risky", loc, SyntaxPython)}
	exceptClauses := []ExceptClause{
		{
			ExceptionType: "ValueError",
			Variable:      "",
			Body:          []ASTNode{NewIdentifier("handler", loc, SyntaxPython)},
		},
	}
	tryStmt := NewTryForm(tryBody, exceptClauses, nil, nil, loc, SyntaxPython)

	// Check ToIR: (try (do risky) (except ValueError (do handler)))
	ir := tryStmt.ToIR()
	list, ok := ir.(*core.ListValue)
	if !ok {
		t.Fatalf("Expected ListValue, got %T", ir)
	}
	if list.Len() != 3 {
		t.Fatalf("Expected 3 elements, got %d", list.Len())
	}
	if sym, ok := list.Items()[0].(core.SymbolValue); !ok || string(sym) != "try" {
		t.Errorf("Expected 'try' symbol, got %v", list.Items()[0])
	}

	// Check except clause
	exceptClause, ok := list.Items()[2].(*core.ListValue)
	if !ok {
		t.Fatalf("Expected except clause ListValue, got %T", list.Items()[2])
	}
	if sym, ok := exceptClause.Items()[0].(core.SymbolValue); !ok || string(sym) != "except" {
		t.Errorf("Expected 'except' symbol, got %v", exceptClause.Items()[0])
	}
}

func TestTryForm_ExceptWithVariable(t *testing.T) {
	loc := &core.SourceLocation{Line: 1, Column: 1}
	// try: body except ValueError as e: handler
	tryBody := []ASTNode{NewIdentifier("risky", loc, SyntaxPython)}
	exceptClauses := []ExceptClause{
		{
			ExceptionType: "ValueError",
			Variable:      "e",
			Body:          []ASTNode{NewIdentifier("handler", loc, SyntaxPython)},
		},
	}
	tryStmt := NewTryForm(tryBody, exceptClauses, nil, nil, loc, SyntaxPython)

	// Check ToIR includes variable
	ir := tryStmt.ToIR()
	list, ok := ir.(*core.ListValue)
	if !ok {
		t.Fatalf("Expected ListValue, got %T", ir)
	}

	exceptClause, ok := list.Items()[2].(*core.ListValue)
	if !ok {
		t.Fatalf("Expected except clause ListValue, got %T", list.Items()[2])
	}
	// Should be: (except ValueError as e (do handler))
	if exceptClause.Len() != 5 {
		t.Fatalf("Expected 5 elements in except (except Type as var body), got %d", exceptClause.Len())
	}
	// Check "as" keyword is at position 2
	if asKeyword, ok := exceptClause.Items()[2].(core.SymbolValue); !ok || string(asKeyword) != "as" {
		t.Fatalf("Expected 'as' keyword at position 2, got %v", exceptClause.Items()[2])
	}
	// Check variable name is at position 3
	if varName, ok := exceptClause.Items()[3].(core.SymbolValue); !ok || string(varName) != "e" {
		t.Fatalf("Expected variable 'e' at position 3, got %v", exceptClause.Items()[3])
	}
}

func TestTryForm_BareExcept(t *testing.T) {
	loc := &core.SourceLocation{Line: 1, Column: 1}
	// try: body except: handler
	tryBody := []ASTNode{NewIdentifier("risky", loc, SyntaxPython)}
	exceptClauses := []ExceptClause{
		{
			ExceptionType: "",
			Variable:      "",
			Body:          []ASTNode{NewIdentifier("handler", loc, SyntaxPython)},
		},
	}
	tryStmt := NewTryForm(tryBody, exceptClauses, nil, nil, loc, SyntaxPython)

	// Check ToIR: (try (do risky) (except (do handler)))
	ir := tryStmt.ToIR()
	list, ok := ir.(*core.ListValue)
	if !ok {
		t.Fatalf("Expected ListValue, got %T", ir)
	}

	exceptClause, ok := list.Items()[2].(*core.ListValue)
	if !ok {
		t.Fatalf("Expected except clause ListValue, got %T", list.Items()[2])
	}
	if exceptClause.Len() != 2 {
		t.Fatalf("Expected 2 elements in bare except (except body), got %d", exceptClause.Len())
	}
}

func TestTryForm_WithFinally(t *testing.T) {
	loc := &core.SourceLocation{Line: 1, Column: 1}
	// try: body finally: cleanup
	tryBody := []ASTNode{NewIdentifier("risky", loc, SyntaxPython)}
	finallyBody := []ASTNode{NewIdentifier("cleanup", loc, SyntaxPython)}
	tryStmt := NewTryForm(tryBody, nil, nil, finallyBody, loc, SyntaxPython)

	// Check ToIR includes finally
	ir := tryStmt.ToIR()
	list, ok := ir.(*core.ListValue)
	if !ok {
		t.Fatalf("Expected ListValue, got %T", ir)
	}

	// Should have try body and finally clause
	found := false
	for _, item := range list.Items() {
		if itemList, ok := item.(*core.ListValue); ok && itemList.Len() > 0 {
			if sym, ok := itemList.Items()[0].(core.SymbolValue); ok && string(sym) == "finally" {
				found = true
				break
			}
		}
	}
	if !found {
		t.Error("Expected to find 'finally' clause in IR")
	}
}

func TestTryForm_WithElse(t *testing.T) {
	loc := &core.SourceLocation{Line: 1, Column: 1}
	// try: body except: handler else: success
	tryBody := []ASTNode{NewIdentifier("risky", loc, SyntaxPython)}
	exceptClauses := []ExceptClause{
		{
			ExceptionType: "",
			Body:          []ASTNode{NewIdentifier("handler", loc, SyntaxPython)},
		},
	}
	elseBody := []ASTNode{NewIdentifier("success", loc, SyntaxPython)}
	tryStmt := NewTryForm(tryBody, exceptClauses, elseBody, nil, loc, SyntaxPython)

	// Check ToIR includes else
	ir := tryStmt.ToIR()
	list, ok := ir.(*core.ListValue)
	if !ok {
		t.Fatalf("Expected ListValue, got %T", ir)
	}

	found := false
	for _, item := range list.Items() {
		if itemList, ok := item.(*core.ListValue); ok && itemList.Len() > 0 {
			if sym, ok := itemList.Items()[0].(core.SymbolValue); ok && string(sym) == "else" {
				found = true
				break
			}
		}
	}
	if !found {
		t.Error("Expected to find 'else' clause in IR")
	}
}

func TestTryForm_MultipleExcept(t *testing.T) {
	loc := &core.SourceLocation{Line: 1, Column: 1}
	// try: body except ValueError: handler1 except KeyError: handler2
	tryBody := []ASTNode{NewIdentifier("risky", loc, SyntaxPython)}
	exceptClauses := []ExceptClause{
		{
			ExceptionType: "ValueError",
			Body:          []ASTNode{NewIdentifier("handler1", loc, SyntaxPython)},
		},
		{
			ExceptionType: "KeyError",
			Body:          []ASTNode{NewIdentifier("handler2", loc, SyntaxPython)},
		},
	}
	tryStmt := NewTryForm(tryBody, exceptClauses, nil, nil, loc, SyntaxPython)

	// Check ToIR has multiple except clauses
	ir := tryStmt.ToIR()
	list, ok := ir.(*core.ListValue)
	if !ok {
		t.Fatalf("Expected ListValue, got %T", ir)
	}

	exceptCount := 0
	for _, item := range list.Items() {
		if itemList, ok := item.(*core.ListValue); ok && itemList.Len() > 0 {
			if sym, ok := itemList.Items()[0].(core.SymbolValue); ok && string(sym) == "except" {
				exceptCount++
			}
		}
	}
	if exceptCount != 2 {
		t.Errorf("Expected 2 except clauses, got %d", exceptCount)
	}
}

// ============================================================================
// Class Definition Tests
// ============================================================================

func TestClassForm_Simple(t *testing.T) {
	loc := &core.SourceLocation{Line: 1, Column: 1}
	// class Foo: pass
	body := []ASTNode{NewPassForm(loc, SyntaxPython)}
	classStmt := NewClassForm("Foo", nil, body, nil, nil, nil, nil, loc, SyntaxPython)

	// Check construction
	if classStmt.Name != "Foo" {
		t.Errorf("Expected name 'Foo', got %s", classStmt.Name)
	}

	// Check ToIR: (class Foo () ...)
	ir := classStmt.ToIR()
	list, ok := ir.(*core.ListValue)
	if !ok {
		t.Fatalf("Expected ListValue, got %T", ir)
	}
	if sym, ok := list.Items()[0].(core.SymbolValue); !ok || string(sym) != "class" {
		t.Errorf("Expected 'class' symbol, got %v", list.Items()[0])
	}
	if sym, ok := list.Items()[1].(core.SymbolValue); !ok || string(sym) != "Foo" {
		t.Errorf("Expected 'Foo' class name, got %v", list.Items()[1])
	}
}

func TestClassForm_WithBase(t *testing.T) {
	loc := &core.SourceLocation{Line: 1, Column: 1}
	// class Foo(Base): pass
	base := NewIdentifier("Base", loc, SyntaxPython)
	body := []ASTNode{NewPassForm(loc, SyntaxPython)}
	classStmt := NewClassForm("Foo", []ASTNode{base}, body, nil, nil, nil, nil, loc, SyntaxPython)

	// Check ToIR includes base class
	ir := classStmt.ToIR()
	list, ok := ir.(*core.ListValue)
	if !ok {
		t.Fatalf("Expected ListValue, got %T", ir)
	}

	// list.Items()[2] should be the bases list
	bases, ok := list.Items()[2].(*core.ListValue)
	if !ok {
		t.Fatalf("Expected bases ListValue, got %T", list.Items()[2])
	}
	if bases.Len() != 1 {
		t.Errorf("Expected 1 base class, got %d", bases.Len())
	}
}

func TestClassForm_WithDecorator(t *testing.T) {
	loc := &core.SourceLocation{Line: 1, Column: 1}
	// @decorator class Foo: pass
	decorator := NewIdentifier("decorator", loc, SyntaxPython)
	body := []ASTNode{NewPassForm(loc, SyntaxPython)}
	classStmt := NewClassForm("Foo", nil, body, []ASTNode{decorator}, nil, nil, nil, loc, SyntaxPython)

	// Check ToIR wraps in assignment: (= Foo (decorator (def-class Foo ...)))
	ir := classStmt.ToIR()
	list, ok := ir.(*core.ListValue)
	if !ok {
		t.Fatalf("Expected ListValue, got %T", ir)
	}
	if sym, ok := list.Items()[0].(core.SymbolValue); !ok || string(sym) != "=" {
		t.Errorf("Expected '=' assignment for decorated class, got %v", list.Items()[0])
	}
}

func TestClassForm_WithMultipleDecorators(t *testing.T) {
	loc := &core.SourceLocation{Line: 1, Column: 1}
	// @decorator1 @decorator2 class Foo: pass
	decorator1 := NewIdentifier("decorator1", loc, SyntaxPython)
	decorator2 := NewIdentifier("decorator2", loc, SyntaxPython)
	body := []ASTNode{NewPassForm(loc, SyntaxPython)}
	classStmt := NewClassForm("Foo", nil, body, []ASTNode{decorator1, decorator2}, nil, nil, nil, loc, SyntaxPython)

	// Check ToIR nests decorators
	ir := classStmt.ToIR()
	list, ok := ir.(*core.ListValue)
	if !ok {
		t.Fatalf("Expected ListValue, got %T", ir)
	}
	if sym, ok := list.Items()[0].(core.SymbolValue); !ok || string(sym) != "=" {
		t.Errorf("Expected '=' for decorated class, got %v", list.Items()[0])
	}
}

// ============================================================================
// ComprehensionKind String Test
// ============================================================================

func TestComprehensionKind_String(t *testing.T) {
	tests := []struct {
		kind     ComprehensionKind
		expected string
	}{
		{ListComp, "list"},
		{DictComp, "dict"},
		{SetComp, "set"},
		{GeneratorComp, "generator"},
	}

	for _, tt := range tests {
		if tt.kind.String() != tt.expected {
			t.Errorf("Expected %q, got %q", tt.expected, tt.kind.String())
		}
	}
}
