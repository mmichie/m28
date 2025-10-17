package ast

import (
	"strings"

	"github.com/mmichie/m28/core"
)

// Python-specific AST nodes
// These nodes represent Python constructs that don't have direct Lisp equivalents

// ============================================================================
// Control Flow Statements
// ============================================================================

// BreakForm represents the break statement
type BreakForm struct {
	BaseNode
}

// NewBreakForm creates a new break statement
func NewBreakForm(loc *core.SourceLocation, syntax SyntaxKind) *BreakForm {
	return &BreakForm{
		BaseNode: BaseNode{
			Loc:    loc,
			Syntax: syntax,
		},
	}
}

// Type implements core.Value.Type
func (b *BreakForm) Type() core.Type {
	return core.ListType
}

// String implements core.Value.String
func (b *BreakForm) String() string {
	return "(break)"
}

// ToIR lowers break to IR
func (b *BreakForm) ToIR() core.Value {
	return core.ListValue{core.SymbolValue("break")}
}

// ContinueForm represents the continue statement
type ContinueForm struct {
	BaseNode
}

// NewContinueForm creates a new continue statement
func NewContinueForm(loc *core.SourceLocation, syntax SyntaxKind) *ContinueForm {
	return &ContinueForm{
		BaseNode: BaseNode{
			Loc:    loc,
			Syntax: syntax,
		},
	}
}

// Type implements core.Value.Type
func (c *ContinueForm) Type() core.Type {
	return core.ListType
}

// String implements core.Value.String
func (c *ContinueForm) String() string {
	return "(continue)"
}

// ToIR lowers continue to IR
func (c *ContinueForm) ToIR() core.Value {
	return core.ListValue{core.SymbolValue("continue")}
}

// ReturnForm represents the return statement
type ReturnForm struct {
	BaseNode
	Value ASTNode // nil for bare return
}

// NewReturnForm creates a new return statement
func NewReturnForm(value ASTNode, loc *core.SourceLocation, syntax SyntaxKind) *ReturnForm {
	return &ReturnForm{
		BaseNode: BaseNode{
			Loc:    loc,
			Syntax: syntax,
		},
		Value: value,
	}
}

// Type implements core.Value.Type
func (r *ReturnForm) Type() core.Type {
	return core.ListType
}

// String implements core.Value.String
func (r *ReturnForm) String() string {
	if r.Value == nil {
		return "(return)"
	}
	return "(return " + r.Value.String() + ")"
}

// ToIR lowers return to IR
func (r *ReturnForm) ToIR() core.Value {
	if r.Value == nil {
		return core.ListValue{core.SymbolValue("return"), core.None}
	}
	return core.ListValue{core.SymbolValue("return"), r.Value.ToIR()}
}

// RaiseForm represents the raise statement
type RaiseForm struct {
	BaseNode
	Exception ASTNode // nil for bare raise (re-raise)
	Cause     ASTNode // nil unless 'raise X from Y'
}

// NewRaiseForm creates a new raise statement
func NewRaiseForm(exception, cause ASTNode, loc *core.SourceLocation, syntax SyntaxKind) *RaiseForm {
	return &RaiseForm{
		BaseNode: BaseNode{
			Loc:    loc,
			Syntax: syntax,
		},
		Exception: exception,
		Cause:     cause,
	}
}

// Type implements core.Value.Type
func (r *RaiseForm) Type() core.Type {
	return core.ListType
}

// String implements core.Value.String
func (r *RaiseForm) String() string {
	if r.Exception == nil {
		return "(raise)"
	}
	if r.Cause != nil {
		return "(raise " + r.Exception.String() + " from " + r.Cause.String() + ")"
	}
	return "(raise " + r.Exception.String() + ")"
}

// ToIR lowers raise to IR
func (r *RaiseForm) ToIR() core.Value {
	if r.Exception == nil {
		// Bare raise (re-raise current exception)
		return core.ListValue{core.SymbolValue("raise")}
	}
	if r.Cause != nil {
		// raise X from Y
		return core.ListValue{
			core.SymbolValue("raise"),
			r.Exception.ToIR(),
			core.SymbolValue("from"),
			r.Cause.ToIR(),
		}
	}
	// raise X
	return core.ListValue{core.SymbolValue("raise"), r.Exception.ToIR()}
}

// PassForm represents the pass statement
type PassForm struct {
	BaseNode
}

// NewPassForm creates a new pass statement
func NewPassForm(loc *core.SourceLocation, syntax SyntaxKind) *PassForm {
	return &PassForm{
		BaseNode: BaseNode{
			Loc:    loc,
			Syntax: syntax,
		},
	}
}

// Type implements core.Value.Type
func (p *PassForm) Type() core.Type {
	return core.NilType
}

// String implements core.Value.String
func (p *PassForm) String() string {
	return "(pass)"
}

// ToIR lowers pass to IR (represented as None)
func (p *PassForm) ToIR() core.Value {
	return core.None
}

// ============================================================================
// Block Statement
// ============================================================================

// BlockForm represents a sequence of statements (do block)
type BlockForm struct {
	BaseNode
	Statements []ASTNode
}

// NewBlockForm creates a new block statement
func NewBlockForm(statements []ASTNode, loc *core.SourceLocation, syntax SyntaxKind) *BlockForm {
	return &BlockForm{
		BaseNode: BaseNode{
			Loc:    loc,
			Syntax: syntax,
		},
		Statements: statements,
	}
}

// Type implements core.Value.Type
func (b *BlockForm) Type() core.Type {
	return core.ListType
}

// String implements core.Value.String
func (b *BlockForm) String() string {
	parts := make([]string, len(b.Statements))
	for i, stmt := range b.Statements {
		parts[i] = stmt.String()
	}
	return "(do " + strings.Join(parts, " ") + ")"
}

// ToIR lowers block to IR (do ...)
func (b *BlockForm) ToIR() core.Value {
	ir := make(core.ListValue, 0, len(b.Statements)+1)
	ir = append(ir, core.SymbolValue("do"))
	for _, stmt := range b.Statements {
		ir = append(ir, stmt.ToIR())
	}
	return ir
}

// ============================================================================
// Loop Statements
// ============================================================================

// ForForm represents a for loop
type ForForm struct {
	BaseNode
	Variable string
	Iterable ASTNode
	Body     []ASTNode
	ElseBody []ASTNode // Optional else clause (Python feature)
}

// NewForForm creates a new for loop
func NewForForm(variable string, iterable ASTNode, body, elseBody []ASTNode, loc *core.SourceLocation, syntax SyntaxKind) *ForForm {
	return &ForForm{
		BaseNode: BaseNode{
			Loc:    loc,
			Syntax: syntax,
		},
		Variable: variable,
		Iterable: iterable,
		Body:     body,
		ElseBody: elseBody,
	}
}

// Type implements core.Value.Type
func (f *ForForm) Type() core.Type {
	return core.ListType
}

// String implements core.Value.String
func (f *ForForm) String() string {
	return "(for " + f.Variable + " " + f.Iterable.String() + " ...)"
}

// ToIR lowers for loop to IR
func (f *ForForm) ToIR() core.Value {
	// Build body block
	bodyIR := make(core.ListValue, 0, len(f.Body)+1)
	bodyIR = append(bodyIR, core.SymbolValue("do"))
	for _, stmt := range f.Body {
		bodyIR = append(bodyIR, stmt.ToIR())
	}

	// Basic for loop: (for var iterable body)
	result := core.ListValue{
		core.SymbolValue("for"),
		core.SymbolValue(f.Variable),
		f.Iterable.ToIR(),
		bodyIR,
	}

	// If there's an else clause, we need to handle it
	// Python's for...else runs if loop completes without break
	// This could be: (for var iterable body else-body)
	if len(f.ElseBody) > 0 {
		elseIR := make(core.ListValue, 0, len(f.ElseBody)+1)
		elseIR = append(elseIR, core.SymbolValue("do"))
		for _, stmt := range f.ElseBody {
			elseIR = append(elseIR, stmt.ToIR())
		}
		result = append(result, elseIR)
	}

	return result
}

// WhileForm represents a while loop
type WhileForm struct {
	BaseNode
	Condition ASTNode
	Body      []ASTNode
	ElseBody  []ASTNode // Optional else clause
}

// NewWhileForm creates a new while loop
func NewWhileForm(condition ASTNode, body, elseBody []ASTNode, loc *core.SourceLocation, syntax SyntaxKind) *WhileForm {
	return &WhileForm{
		BaseNode: BaseNode{
			Loc:    loc,
			Syntax: syntax,
		},
		Condition: condition,
		Body:      body,
		ElseBody:  elseBody,
	}
}

// Type implements core.Value.Type
func (w *WhileForm) Type() core.Type {
	return core.ListType
}

// String implements core.Value.String
func (w *WhileForm) String() string {
	return "(while " + w.Condition.String() + " ...)"
}

// ToIR lowers while loop to IR
func (w *WhileForm) ToIR() core.Value {
	// Build body block
	bodyIR := make(core.ListValue, 0, len(w.Body)+1)
	bodyIR = append(bodyIR, core.SymbolValue("do"))
	for _, stmt := range w.Body {
		bodyIR = append(bodyIR, stmt.ToIR())
	}

	// Basic while loop: (while condition body)
	result := core.ListValue{
		core.SymbolValue("while"),
		w.Condition.ToIR(),
		bodyIR,
	}

	// Handle else clause
	if len(w.ElseBody) > 0 {
		elseIR := make(core.ListValue, 0, len(w.ElseBody)+1)
		elseIR = append(elseIR, core.SymbolValue("do"))
		for _, stmt := range w.ElseBody {
			elseIR = append(elseIR, stmt.ToIR())
		}
		result = append(result, elseIR)
	}

	return result
}

// ============================================================================
// Comprehensions
// ============================================================================

// ComprehensionKind represents the type of comprehension
type ComprehensionKind int

const (
	ListComp ComprehensionKind = iota
	DictComp
	SetComp
	GeneratorComp
)

func (ck ComprehensionKind) String() string {
	switch ck {
	case ListComp:
		return "list"
	case DictComp:
		return "dict"
	case SetComp:
		return "set"
	case GeneratorComp:
		return "generator"
	default:
		return "unknown"
	}
}

// ComprehensionForm represents list/dict/set comprehensions and generator expressions
type ComprehensionForm struct {
	BaseNode
	Kind      ComprehensionKind
	Element   ASTNode // For list/set/generator: the expression
	KeyExpr   ASTNode // For dict: key expression
	ValueExpr ASTNode // For dict: value expression
	Variable  string  // Loop variable
	Iterable  ASTNode // What to iterate over
	Condition ASTNode // Optional filter (nil if none)
}

// NewComprehensionForm creates a new comprehension
func NewComprehensionForm(kind ComprehensionKind, element, keyExpr, valueExpr ASTNode,
	variable string, iterable, condition ASTNode, loc *core.SourceLocation, syntax SyntaxKind) *ComprehensionForm {
	return &ComprehensionForm{
		BaseNode: BaseNode{
			Loc:    loc,
			Syntax: syntax,
		},
		Kind:      kind,
		Element:   element,
		KeyExpr:   keyExpr,
		ValueExpr: valueExpr,
		Variable:  variable,
		Iterable:  iterable,
		Condition: condition,
	}
}

// Type implements core.Value.Type
func (c *ComprehensionForm) Type() core.Type {
	return core.ListType
}

// String implements core.Value.String
func (c *ComprehensionForm) String() string {
	return "(" + c.Kind.String() + "-comp ...)"
}

// ToIR lowers comprehension to IR
func (c *ComprehensionForm) ToIR() core.Value {
	switch c.Kind {
	case ListComp:
		// [x*x for x in range(10) if x % 2 == 0]
		// → (list-comp (lambda (x) (* x x)) (range 10) (lambda (x) (== (% x 2) 0)))
		elemLambda := core.ListValue{
			core.SymbolValue("lambda"),
			core.ListValue{core.SymbolValue(c.Variable)},
			c.Element.ToIR(),
		}

		if c.Condition != nil {
			filterLambda := core.ListValue{
				core.SymbolValue("lambda"),
				core.ListValue{core.SymbolValue(c.Variable)},
				c.Condition.ToIR(),
			}
			return core.ListValue{
				core.SymbolValue("list-comp"),
				elemLambda,
				c.Iterable.ToIR(),
				filterLambda,
			}
		}

		return core.ListValue{
			core.SymbolValue("list-comp"),
			elemLambda,
			c.Iterable.ToIR(),
		}

	case DictComp:
		// {k: v*2 for k, v in items}
		// → (dict-comp (lambda (k v) k) (lambda (k v) (* v 2)) items)
		// Note: This assumes variable is "k,v" or similar - parser needs to handle unpacking
		keyLambda := core.ListValue{
			core.SymbolValue("lambda"),
			core.ListValue{core.SymbolValue(c.Variable)},
			c.KeyExpr.ToIR(),
		}
		valueLambda := core.ListValue{
			core.SymbolValue("lambda"),
			core.ListValue{core.SymbolValue(c.Variable)},
			c.ValueExpr.ToIR(),
		}

		if c.Condition != nil {
			filterLambda := core.ListValue{
				core.SymbolValue("lambda"),
				core.ListValue{core.SymbolValue(c.Variable)},
				c.Condition.ToIR(),
			}
			return core.ListValue{
				core.SymbolValue("dict-comp"),
				keyLambda,
				valueLambda,
				c.Iterable.ToIR(),
				filterLambda,
			}
		}

		return core.ListValue{
			core.SymbolValue("dict-comp"),
			keyLambda,
			valueLambda,
			c.Iterable.ToIR(),
		}

	case SetComp:
		// {x for x in data if x > 10}
		// → (set-comp (lambda (x) x) data (lambda (x) (> x 10)))
		elemLambda := core.ListValue{
			core.SymbolValue("lambda"),
			core.ListValue{core.SymbolValue(c.Variable)},
			c.Element.ToIR(),
		}

		if c.Condition != nil {
			filterLambda := core.ListValue{
				core.SymbolValue("lambda"),
				core.ListValue{core.SymbolValue(c.Variable)},
				c.Condition.ToIR(),
			}
			return core.ListValue{
				core.SymbolValue("set-comp"),
				elemLambda,
				c.Iterable.ToIR(),
				filterLambda,
			}
		}

		return core.ListValue{
			core.SymbolValue("set-comp"),
			elemLambda,
			c.Iterable.ToIR(),
		}

	case GeneratorComp:
		// (x*x for x in range(10))
		// → (gen-comp (lambda (x) (* x x)) (range 10))
		elemLambda := core.ListValue{
			core.SymbolValue("lambda"),
			core.ListValue{core.SymbolValue(c.Variable)},
			c.Element.ToIR(),
		}

		if c.Condition != nil {
			filterLambda := core.ListValue{
				core.SymbolValue("lambda"),
				core.ListValue{core.SymbolValue(c.Variable)},
				c.Condition.ToIR(),
			}
			return core.ListValue{
				core.SymbolValue("gen-comp"),
				elemLambda,
				c.Iterable.ToIR(),
				filterLambda,
			}
		}

		return core.ListValue{
			core.SymbolValue("gen-comp"),
			elemLambda,
			c.Iterable.ToIR(),
		}

	default:
		return core.None
	}
}

// ============================================================================
// Context Managers
// ============================================================================

// WithItem represents a single context manager in a with statement
type WithItem struct {
	Context  ASTNode
	Variable string // Empty string if no 'as' clause
}

// WithForm represents a with statement (context manager)
type WithForm struct {
	BaseNode
	Items []WithItem
	Body  []ASTNode
}

// NewWithForm creates a new with statement
func NewWithForm(items []WithItem, body []ASTNode, loc *core.SourceLocation, syntax SyntaxKind) *WithForm {
	return &WithForm{
		BaseNode: BaseNode{
			Loc:    loc,
			Syntax: syntax,
		},
		Items: items,
		Body:  body,
	}
}

// Type implements core.Value.Type
func (w *WithForm) Type() core.Type {
	return core.ListType
}

// String implements core.Value.String
func (w *WithForm) String() string {
	return "(with ...)"
}

// ToIR lowers with statement to IR
func (w *WithForm) ToIR() core.Value {
	// Build body block
	bodyIR := make(core.ListValue, 0, len(w.Body)+1)
	bodyIR = append(bodyIR, core.SymbolValue("do"))
	for _, stmt := range w.Body {
		bodyIR = append(bodyIR, stmt.ToIR())
	}

	// For now, handle simple case: one context manager
	// (with context-expr var body)
	if len(w.Items) == 1 {
		item := w.Items[0]
		if item.Variable != "" {
			return core.ListValue{
				core.SymbolValue("with"),
				item.Context.ToIR(),
				core.SymbolValue(item.Variable),
				bodyIR,
			}
		}
		return core.ListValue{
			core.SymbolValue("with"),
			item.Context.ToIR(),
			core.None,
			bodyIR,
		}
	}

	// Multiple context managers: nest them
	// with x as a, y as b: body
	// → (with x a (with y b body))
	result := bodyIR
	for i := len(w.Items) - 1; i >= 0; i-- {
		item := w.Items[i]
		if item.Variable != "" {
			result = core.ListValue{
				core.SymbolValue("with"),
				item.Context.ToIR(),
				core.SymbolValue(item.Variable),
				result,
			}
		} else {
			result = core.ListValue{
				core.SymbolValue("with"),
				item.Context.ToIR(),
				core.None,
				result,
			}
		}
	}

	return result
}

// ============================================================================
// Exception Handling
// ============================================================================

// ExceptClause represents an except clause in a try statement
type ExceptClause struct {
	ExceptionType string // Empty string for bare except
	Variable      string // Empty string if no 'as' clause
	Body          []ASTNode
}

// TryForm represents a try/except/finally statement
type TryForm struct {
	BaseNode
	TryBody       []ASTNode
	ExceptClauses []ExceptClause
	ElseBody      []ASTNode // Optional (runs if no exception)
	FinallyBody   []ASTNode // Optional
}

// NewTryForm creates a new try statement
func NewTryForm(tryBody []ASTNode, exceptClauses []ExceptClause,
	elseBody, finallyBody []ASTNode, loc *core.SourceLocation, syntax SyntaxKind) *TryForm {
	return &TryForm{
		BaseNode: BaseNode{
			Loc:    loc,
			Syntax: syntax,
		},
		TryBody:       tryBody,
		ExceptClauses: exceptClauses,
		ElseBody:      elseBody,
		FinallyBody:   finallyBody,
	}
}

// Type implements core.Value.Type
func (t *TryForm) Type() core.Type {
	return core.ListType
}

// String implements core.Value.String
func (t *TryForm) String() string {
	return "(try ...)"
}

// ToIR lowers try statement to IR
func (t *TryForm) ToIR() core.Value {
	// Build try body
	tryIR := make(core.ListValue, 0, len(t.TryBody)+1)
	tryIR = append(tryIR, core.SymbolValue("do"))
	for _, stmt := range t.TryBody {
		tryIR = append(tryIR, stmt.ToIR())
	}

	// Start building result: (try body ...)
	result := core.ListValue{core.SymbolValue("try"), tryIR}

	// Add except clauses
	for _, except := range t.ExceptClauses {
		exceptBody := make(core.ListValue, 0, len(except.Body)+1)
		exceptBody = append(exceptBody, core.SymbolValue("do"))
		for _, stmt := range except.Body {
			exceptBody = append(exceptBody, stmt.ToIR())
		}

		if except.ExceptionType != "" && except.Variable != "" {
			// except ValueError as e: ...
			result = append(result, core.ListValue{
				core.SymbolValue("except"),
				core.SymbolValue(except.ExceptionType),
				core.SymbolValue(except.Variable),
				exceptBody,
			})
		} else if except.ExceptionType != "" {
			// except ValueError: ...
			result = append(result, core.ListValue{
				core.SymbolValue("except"),
				core.SymbolValue(except.ExceptionType),
				exceptBody,
			})
		} else {
			// except: ... (bare except)
			result = append(result, core.ListValue{
				core.SymbolValue("except"),
				exceptBody,
			})
		}
	}

	// Add else clause if present
	if len(t.ElseBody) > 0 {
		elseIR := make(core.ListValue, 0, len(t.ElseBody)+1)
		elseIR = append(elseIR, core.SymbolValue("do"))
		for _, stmt := range t.ElseBody {
			elseIR = append(elseIR, stmt.ToIR())
		}
		result = append(result, core.ListValue{core.SymbolValue("else"), elseIR})
	}

	// Add finally clause if present
	if len(t.FinallyBody) > 0 {
		finallyIR := make(core.ListValue, 0, len(t.FinallyBody)+1)
		finallyIR = append(finallyIR, core.SymbolValue("do"))
		for _, stmt := range t.FinallyBody {
			finallyIR = append(finallyIR, stmt.ToIR())
		}
		result = append(result, core.ListValue{core.SymbolValue("finally"), finallyIR})
	}

	return result
}

// ============================================================================
// Class Definitions
// ============================================================================

// ClassForm represents a class definition
type ClassForm struct {
	BaseNode
	Name       string
	Bases      []ASTNode // Base classes
	Body       []ASTNode // Methods and attributes
	Decorators []ASTNode // Class decorators
}

// NewClassForm creates a new class definition
func NewClassForm(name string, bases, body, decorators []ASTNode, loc *core.SourceLocation, syntax SyntaxKind) *ClassForm {
	return &ClassForm{
		BaseNode: BaseNode{
			Loc:    loc,
			Syntax: syntax,
		},
		Name:       name,
		Bases:      bases,
		Body:       body,
		Decorators: decorators,
	}
}

// Type implements core.Value.Type
func (c *ClassForm) Type() core.Type {
	return core.ListType
}

// String implements core.Value.String
func (c *ClassForm) String() string {
	return "(def-class " + c.Name + " ...)"
}

// ToIR lowers class definition to IR
func (c *ClassForm) ToIR() core.Value {
	// Build base class list
	basesIR := make(core.ListValue, 0, len(c.Bases))
	for _, base := range c.Bases {
		basesIR = append(basesIR, base.ToIR())
	}

	// Build body
	bodyIR := make(core.ListValue, 0, len(c.Body))
	for _, item := range c.Body {
		bodyIR = append(bodyIR, item.ToIR())
	}

	// Basic class: (def-class Name (Base1 Base2) method1 method2 ...)
	result := core.ListValue{
		core.SymbolValue("def-class"),
		core.SymbolValue(c.Name),
		basesIR,
	}
	result = append(result, bodyIR...)

	// Handle decorators
	// @decorator class Foo: ... → (= Foo (decorator (def-class Foo ...)))
	for i := len(c.Decorators) - 1; i >= 0; i-- {
		decorator := c.Decorators[i]
		result = core.ListValue{
			decorator.ToIR(),
			result,
		}
	}

	// If there are decorators, wrap in assignment
	if len(c.Decorators) > 0 {
		result = core.ListValue{
			core.SymbolValue("="),
			core.SymbolValue(c.Name),
			result,
		}
	}

	return result
}
