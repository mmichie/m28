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

// AssertForm represents the assert statement
type AssertForm struct {
	BaseNode
	Condition ASTNode // The condition to assert
	Message   ASTNode // Optional message (nil if not provided)
}

// NewAssertForm creates a new assert statement
func NewAssertForm(condition, message ASTNode, loc *core.SourceLocation, syntax SyntaxKind) *AssertForm {
	return &AssertForm{
		BaseNode: BaseNode{
			Loc:    loc,
			Syntax: syntax,
		},
		Condition: condition,
		Message:   message,
	}
}

// Type implements core.Value.Type
func (a *AssertForm) Type() core.Type {
	return core.ListType
}

// String implements core.Value.String
func (a *AssertForm) String() string {
	if a.Message == nil {
		return "(assert " + a.Condition.String() + ")"
	}
	return "(assert " + a.Condition.String() + " " + a.Message.String() + ")"
}

// ToIR lowers assert to IR
func (a *AssertForm) ToIR() core.Value {
	if a.Message == nil {
		return core.ListValue{core.SymbolValue("assert"), a.Condition.ToIR()}
	}
	return core.ListValue{core.SymbolValue("assert"), a.Condition.ToIR(), a.Message.ToIR()}
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
	Variable  string   // Deprecated: use Variables instead
	Variables []string // Loop variables (for tuple unpacking)
	Iterable  ASTNode
	Body      []ASTNode
	ElseBody  []ASTNode // Optional else clause (Python feature)
}

// NewForForm creates a new for loop with a single variable (backward compatible)
func NewForForm(variable string, iterable ASTNode, body, elseBody []ASTNode, loc *core.SourceLocation, syntax SyntaxKind) *ForForm {
	return &ForForm{
		BaseNode: BaseNode{
			Loc:    loc,
			Syntax: syntax,
		},
		Variable:  variable,
		Variables: []string{variable},
		Iterable:  iterable,
		Body:      body,
		ElseBody:  elseBody,
	}
}

// NewForFormMulti creates a new for loop with multiple variables (tuple unpacking)
func NewForFormMulti(variables []string, iterable ASTNode, body, elseBody []ASTNode, loc *core.SourceLocation, syntax SyntaxKind) *ForForm {
	// Set Variable to first var for backward compatibility
	var firstVar string
	if len(variables) > 0 {
		firstVar = variables[0]
	}

	return &ForForm{
		BaseNode: BaseNode{
			Loc:    loc,
			Syntax: syntax,
		},
		Variable:  firstVar,
		Variables: variables,
		Iterable:  iterable,
		Body:      body,
		ElseBody:  elseBody,
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

	var result core.ListValue

	if len(f.Variables) == 1 {
		// Single variable: (for var iterable body)
		result = core.ListValue{
			core.SymbolValue("for"),
			core.SymbolValue(f.Variables[0]),
			f.Iterable.ToIR(),
			bodyIR,
		}
	} else {
		// Multiple variables (tuple unpacking): (for var1 var2 ... in iterable body)
		result = core.ListValue{core.SymbolValue("for")}

		// Add all variables
		for _, v := range f.Variables {
			result = append(result, core.SymbolValue(v))
		}

		// Add 'in' keyword
		result = append(result, core.SymbolValue("in"))

		// Add iterable and body
		result = append(result, f.Iterable.ToIR())
		result = append(result, bodyIR)
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

// ComprehensionClause represents a single for clause in a comprehension
// e.g., "for x in items if x > 0"
type ComprehensionClause struct {
	Variable  string  // Loop variable
	Iterable  ASTNode // What to iterate over
	Condition ASTNode // Optional filter (nil if none)
}

// ComprehensionForm represents list/dict/set comprehensions and generator expressions
// Supports nested comprehensions via multiple clauses
type ComprehensionForm struct {
	BaseNode
	Kind      ComprehensionKind
	Element   ASTNode               // For list/set/generator: the expression
	KeyExpr   ASTNode               // For dict: key expression
	ValueExpr ASTNode               // For dict: value expression
	Clauses   []ComprehensionClause // One or more for clauses

	// Deprecated fields - kept for backward compatibility
	Variable  string  // Use Clauses instead
	Iterable  ASTNode // Use Clauses instead
	Condition ASTNode // Use Clauses instead
}

// NewComprehensionForm creates a new comprehension (legacy single-clause version)
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
		Clauses: []ComprehensionClause{
			{
				Variable:  variable,
				Iterable:  iterable,
				Condition: condition,
			},
		},
	}
}

// NewComprehensionFormMulti creates a new comprehension with multiple clauses (for nested comprehensions)
func NewComprehensionFormMulti(kind ComprehensionKind, element, keyExpr, valueExpr ASTNode,
	clauses []ComprehensionClause, loc *core.SourceLocation, syntax SyntaxKind) *ComprehensionForm {
	// For backward compatibility, set Variable/Iterable/Condition to first clause
	var variable string
	var iterable, condition ASTNode
	if len(clauses) > 0 {
		variable = clauses[0].Variable
		iterable = clauses[0].Iterable
		condition = clauses[0].Condition
	}

	return &ComprehensionForm{
		BaseNode: BaseNode{
			Loc:    loc,
			Syntax: syntax,
		},
		Kind:      kind,
		Element:   element,
		KeyExpr:   keyExpr,
		ValueExpr: valueExpr,
		Clauses:   clauses,
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
		// Single clause: [x*x for x in range(10) if x % 2 == 0]
		// → (list-comp (* x x) x (range 10) (== (% x 2) 0))
		//
		// Multiple clauses: [item for row in matrix for item in row]
		// → (list-comp item ((row matrix) (item row)))

		if len(c.Clauses) > 1 {
			// Multi-clause (nested) comprehension
			clausesIR := make(core.ListValue, 0, len(c.Clauses))
			for _, clause := range c.Clauses {
				clauseIR := core.ListValue{
					core.SymbolValue(clause.Variable),
					clause.Iterable.ToIR(),
				}
				if clause.Condition != nil {
					clauseIR = append(clauseIR, clause.Condition.ToIR())
				}
				clausesIR = append(clausesIR, clauseIR)
			}

			return core.ListValue{
				core.SymbolValue("list-comp"),
				c.Element.ToIR(),
				clausesIR,
			}
		}

		// Single clause (backward compatible)
		// Use first clause if Variable not set properly
		var variable string
		var iterable, condition ASTNode
		if len(c.Clauses) > 0 {
			// Prefer clauses (new format)
			variable = c.Clauses[0].Variable
			iterable = c.Clauses[0].Iterable
			condition = c.Clauses[0].Condition
		} else {
			// Fall back to deprecated fields
			variable = c.Variable
			iterable = c.Iterable
			condition = c.Condition
		}

		if condition != nil {
			return core.ListValue{
				core.SymbolValue("list-comp"),
				c.Element.ToIR(),
				core.SymbolValue(variable),
				iterable.ToIR(),
				condition.ToIR(),
			}
		}

		return core.ListValue{
			core.SymbolValue("list-comp"),
			c.Element.ToIR(),
			core.SymbolValue(variable),
			iterable.ToIR(),
		}

	case DictComp:
		// Single: {k: v*2 for k in items}
		// → (dict-comp k (* v 2) k items)
		//
		// Multiple: {k: v for row in matrix for k, v in row.items()}
		// → (dict-comp k v ((row matrix) ((k v) (row.items))))

		if len(c.Clauses) > 1 {
			// Multi-clause (nested) dict comprehension
			clausesIR := make(core.ListValue, 0, len(c.Clauses))
			for _, clause := range c.Clauses {
				clauseIR := core.ListValue{
					core.SymbolValue(clause.Variable),
					clause.Iterable.ToIR(),
				}
				if clause.Condition != nil {
					clauseIR = append(clauseIR, clause.Condition.ToIR())
				}
				clausesIR = append(clausesIR, clauseIR)
			}

			return core.ListValue{
				core.SymbolValue("dict-comp"),
				c.KeyExpr.ToIR(),
				c.ValueExpr.ToIR(),
				clausesIR,
			}
		}

		// Single clause (backward compatible)
		// Format: (dict-comp key-expr value-expr var iterable [condition])
		var variable string
		var iterable, condition ASTNode
		if len(c.Clauses) > 0 {
			variable = c.Clauses[0].Variable
			iterable = c.Clauses[0].Iterable
			condition = c.Clauses[0].Condition
		} else {
			variable = c.Variable
			iterable = c.Iterable
			condition = c.Condition
		}

		if condition != nil {
			return core.ListValue{
				core.SymbolValue("dict-comp"),
				c.KeyExpr.ToIR(),
				c.ValueExpr.ToIR(),
				core.SymbolValue(variable),
				iterable.ToIR(),
				condition.ToIR(),
			}
		}

		return core.ListValue{
			core.SymbolValue("dict-comp"),
			c.KeyExpr.ToIR(),
			c.ValueExpr.ToIR(),
			core.SymbolValue(variable),
			iterable.ToIR(),
		}

	case SetComp:
		// Single: {x for x in data if x > 10}
		// → (set-comp x x data (> x 10))
		//
		// Multiple: {item for row in matrix for item in row}
		// → (set-comp item ((row matrix) (item row)))

		if len(c.Clauses) > 1 {
			// Multi-clause (nested) set comprehension
			clausesIR := make(core.ListValue, 0, len(c.Clauses))
			for _, clause := range c.Clauses {
				clauseIR := core.ListValue{
					core.SymbolValue(clause.Variable),
					clause.Iterable.ToIR(),
				}
				if clause.Condition != nil {
					clauseIR = append(clauseIR, clause.Condition.ToIR())
				}
				clausesIR = append(clausesIR, clauseIR)
			}

			return core.ListValue{
				core.SymbolValue("set-comp"),
				c.Element.ToIR(),
				clausesIR,
			}
		}

		// Single clause (backward compatible)
		// Format: (set-comp expr var iterable [condition])
		var variable string
		var iterable, condition ASTNode
		if len(c.Clauses) > 0 {
			variable = c.Clauses[0].Variable
			iterable = c.Clauses[0].Iterable
			condition = c.Clauses[0].Condition
		} else {
			variable = c.Variable
			iterable = c.Iterable
			condition = c.Condition
		}

		if condition != nil {
			return core.ListValue{
				core.SymbolValue("set-comp"),
				c.Element.ToIR(),
				core.SymbolValue(variable),
				iterable.ToIR(),
				condition.ToIR(),
			}
		}

		return core.ListValue{
			core.SymbolValue("set-comp"),
			c.Element.ToIR(),
			core.SymbolValue(variable),
			iterable.ToIR(),
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
	Keywords   []ASTNode // Keyword arguments (e.g., metaclass=...)
}

// NewClassForm creates a new class definition
func NewClassForm(name string, bases, body, decorators, keywords []ASTNode, loc *core.SourceLocation, syntax SyntaxKind) *ClassForm {
	return &ClassForm{
		BaseNode: BaseNode{
			Loc:    loc,
			Syntax: syntax,
		},
		Name:       name,
		Bases:      bases,
		Body:       body,
		Decorators: decorators,
		Keywords:   keywords,
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

	// Basic class: (class Name [Base1 Base2] method1 method2 ...)
	result := core.ListValue{
		core.SymbolValue("class"),
		core.SymbolValue(c.Name),
		basesIR,
	}
	result = append(result, bodyIR...)

	// Handle decorators
	// @decorator class Foo: ... → (= Foo (decorator (class Foo ...)))
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
