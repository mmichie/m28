package ast

import (
	"strings"

	"github.com/mmichie/m28/core"
)

// parseVariablePattern converts a variable string into a pattern structure for unpacking.
// Examples:
//
//	"x" → SymbolValue("x")
//	"(x, y)" → ListValue([SymbolValue("x"), SymbolValue("y")])
//	"(x, (y, z))" → ListValue([SymbolValue("x"), ListValue([SymbolValue("y"), SymbolValue("z")])])
func parseVariablePattern(varStr string) core.Value {
	varStr = strings.TrimSpace(varStr)

	// If no parentheses, it's a simple symbol
	if !strings.HasPrefix(varStr, "(") {
		return core.SymbolValue(varStr)
	}

	// Remove outer parentheses
	if strings.HasPrefix(varStr, "(") && strings.HasSuffix(varStr, ")") {
		varStr = varStr[1 : len(varStr)-1]
	}

	// Split by comma, respecting nested parentheses
	parts := []string{}
	current := strings.Builder{}
	depth := 0

	for _, ch := range varStr {
		switch ch {
		case '(':
			depth++
			current.WriteRune(ch)
		case ')':
			depth--
			current.WriteRune(ch)
		case ',':
			if depth == 0 {
				// Top-level comma - split here
				parts = append(parts, strings.TrimSpace(current.String()))
				current.Reset()
			} else {
				current.WriteRune(ch)
			}
		case ' ', '\t':
			// Skip whitespace at depth 0, keep it otherwise
			if depth > 0 || current.Len() > 0 {
				current.WriteRune(ch)
			}
		default:
			current.WriteRune(ch)
		}
	}

	// Add last part
	if current.Len() > 0 {
		parts = append(parts, strings.TrimSpace(current.String()))
	}

	// Recursively parse each part
	patterns := make([]core.Value, len(parts))
	for i, part := range parts {
		patterns[i] = parseVariablePattern(part)
	}

	return core.NewList(patterns...)
}

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
	return core.NewList(core.SymbolValue("break"))
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
	return core.NewList(core.SymbolValue("continue"))
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
		return core.NewList(core.SymbolValue("return"), core.None)
	}
	return core.NewList(core.SymbolValue("return"), r.Value.ToIR())
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
		return core.NewList(core.SymbolValue("raise"))
	}
	if r.Cause != nil {
		// raise X from Y
		return core.NewList(
			core.SymbolValue("raise"),
			r.Exception.ToIR(),
			core.SymbolValue("from"),
			r.Cause.ToIR(),
		)
	}
	// raise X
	return core.NewList(core.SymbolValue("raise"), r.Exception.ToIR())
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
		return core.NewList(core.SymbolValue("assert"), a.Condition.ToIR())
	}
	return core.NewList(core.SymbolValue("assert"), a.Condition.ToIR(), a.Message.ToIR())
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
	ir := make([]core.Value, 0, len(b.Statements)+1)
	ir = append(ir, core.SymbolValue("do"))
	for _, stmt := range b.Statements {
		ir = append(ir, stmt.ToIR())
	}
	return core.NewList(ir...)
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
	bodyIR := make([]core.Value, 0, len(f.Body)+1)
	bodyIR = append(bodyIR, core.SymbolValue("do"))
	for _, stmt := range f.Body {
		bodyIR = append(bodyIR, stmt.ToIR())
	}

	var result []core.Value

	if len(f.Variables) == 1 {
		// Single variable: (for var iterable body)
		// Variable might be nested tuple like "(x, y)"
		varPattern := parseVariablePattern(f.Variables[0])
		result = []core.Value{
			core.SymbolValue("for"),
			varPattern,
			f.Iterable.ToIR(),
			core.NewList(bodyIR...),
		}
	} else {
		// Multiple variables (tuple unpacking): (for (quote (var1 var2 ...)) iterable body)
		// Wrap variables in a quoted list to prevent evaluation
		// (patterns use ListValue, not TupleValue)
		varList := make([]core.Value, 0, len(f.Variables))
		for _, v := range f.Variables {
			varList = append(varList, parseVariablePattern(v))
		}

		// Quote the variable list so it doesn't get evaluated as a function call
		quotedVarList := core.NewList(core.SymbolValue("quote"), core.NewList(varList...))

		result = []core.Value{
			core.SymbolValue("for"),
			quotedVarList,
			f.Iterable.ToIR(),
			core.NewList(bodyIR...),
		}
	}

	// If there's an else clause, we need to handle it
	// Python's for...else runs if loop completes without break
	// This could be: (for var iterable body else-body)
	if len(f.ElseBody) > 0 {
		elseIR := make([]core.Value, 0, len(f.ElseBody)+1)
		elseIR = append(elseIR, core.SymbolValue("do"))
		for _, stmt := range f.ElseBody {
			elseIR = append(elseIR, stmt.ToIR())
		}
		result = append(result, core.NewList(elseIR...))
	}

	return core.NewList(result...)
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
	bodyIR := make([]core.Value, 0, len(w.Body)+1)
	bodyIR = append(bodyIR, core.SymbolValue("do"))
	for _, stmt := range w.Body {
		bodyIR = append(bodyIR, stmt.ToIR())
	}

	// Basic while loop: (while condition body)
	result := []core.Value{
		core.SymbolValue("while"),
		w.Condition.ToIR(),
		core.NewList(bodyIR...),
	}

	// Handle else clause
	if len(w.ElseBody) > 0 {
		elseIR := make([]core.Value, 0, len(w.ElseBody)+1)
		elseIR = append(elseIR, core.SymbolValue("do"))
		for _, stmt := range w.ElseBody {
			elseIR = append(elseIR, stmt.ToIR())
		}
		result = append(result, core.NewList(elseIR...))
	}

	return core.NewList(result...)
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
			clausesIR := make([]core.Value, 0, len(c.Clauses))
			for _, clause := range c.Clauses {
				clauseIR := []core.Value{
					core.SymbolValue(clause.Variable),
					clause.Iterable.ToIR(),
				}
				if clause.Condition != nil {
					clauseIR = append(clauseIR, clause.Condition.ToIR())
				}
				clausesIR = append(clausesIR, core.NewList(clauseIR...))
			}

			return core.NewList(
				core.SymbolValue("list-comp"),
				c.Element.ToIR(),
				core.NewList(clausesIR...),
			)
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
			return core.NewList(
				core.SymbolValue("list-comp"),
				c.Element.ToIR(),
				core.SymbolValue(variable),
				iterable.ToIR(),
				condition.ToIR(),
			)
		}

		return core.NewList(
			core.SymbolValue("list-comp"),
			c.Element.ToIR(),
			core.SymbolValue(variable),
			iterable.ToIR(),
		)

	case DictComp:
		// Single: {k: v*2 for k in items}
		// → (dict-comp k (* v 2) k items)
		//
		// Multiple: {k: v for row in matrix for k, v in row.items()}
		// → (dict-comp k v ((row matrix) ((k v) (row.items))))

		if len(c.Clauses) > 1 {
			// Multi-clause (nested) dict comprehension
			clausesIR := make([]core.Value, 0, len(c.Clauses))
			for _, clause := range c.Clauses {
				clauseIR := []core.Value{
					core.SymbolValue(clause.Variable),
					clause.Iterable.ToIR(),
				}
				if clause.Condition != nil {
					clauseIR = append(clauseIR, clause.Condition.ToIR())
				}
				clausesIR = append(clausesIR, core.NewList(clauseIR...))
			}

			return core.NewList(
				core.SymbolValue("dict-comp"),
				c.KeyExpr.ToIR(),
				c.ValueExpr.ToIR(),
				core.NewList(clausesIR...),
			)
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
			return core.NewList(
				core.SymbolValue("dict-comp"),
				c.KeyExpr.ToIR(),
				c.ValueExpr.ToIR(),
				core.SymbolValue(variable),
				iterable.ToIR(),
				condition.ToIR(),
			)
		}

		return core.NewList(
			core.SymbolValue("dict-comp"),
			c.KeyExpr.ToIR(),
			c.ValueExpr.ToIR(),
			core.SymbolValue(variable),
			iterable.ToIR(),
		)

	case SetComp:
		// Single: {x for x in data if x > 10}
		// → (set-comp x x data (> x 10))
		//
		// Multiple: {item for row in matrix for item in row}
		// → (set-comp item ((row matrix) (item row)))

		if len(c.Clauses) > 1 {
			// Multi-clause (nested) set comprehension
			clausesIR := make([]core.Value, 0, len(c.Clauses))
			for _, clause := range c.Clauses {
				clauseIR := []core.Value{
					core.SymbolValue(clause.Variable),
					clause.Iterable.ToIR(),
				}
				if clause.Condition != nil {
					clauseIR = append(clauseIR, clause.Condition.ToIR())
				}
				clausesIR = append(clausesIR, core.NewList(clauseIR...))
			}

			return core.NewList(
				core.SymbolValue("set-comp"),
				c.Element.ToIR(),
				core.NewList(clausesIR...),
			)
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
			return core.NewList(
				core.SymbolValue("set-comp"),
				c.Element.ToIR(),
				core.SymbolValue(variable),
				iterable.ToIR(),
				condition.ToIR(),
			)
		}

		return core.NewList(
			core.SymbolValue("set-comp"),
			c.Element.ToIR(),
			core.SymbolValue(variable),
			iterable.ToIR(),
		)

	case GeneratorComp:
		// (x*x for x in range(10))
		// → (gen-comp (lambda (x) (* x x)) (range 10))
		// or for tuple unpacking:
		// ((k, v) for k, v in items)
		// → (gen-comp (lambda ((k v)) (tuple-literal k v)) items)

		// Get variable, iterable, and condition from clauses or deprecated fields
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

		// Parse variable pattern to handle tuple unpacking
		varPattern := parseVariablePattern(variable)

		elemLambda := core.NewList(
			core.SymbolValue("lambda"),
			core.NewList(varPattern),
			c.Element.ToIR(),
		)

		if condition != nil {
			filterLambda := core.NewList(
				core.SymbolValue("lambda"),
				core.NewList(varPattern),
				condition.ToIR(),
			)
			return core.NewList(
				core.SymbolValue("gen-comp"),
				elemLambda,
				iterable.ToIR(),
				filterLambda,
			)
		}

		return core.NewList(
			core.SymbolValue("gen-comp"),
			elemLambda,
			iterable.ToIR(),
		)

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
	bodyIR := make([]core.Value, 0, len(w.Body)+1)
	bodyIR = append(bodyIR, core.SymbolValue("do"))
	for _, stmt := range w.Body {
		bodyIR = append(bodyIR, stmt.ToIR())
	}

	// For now, handle simple case: one context manager
	// (with context-expr var body)
	if len(w.Items) == 1 {
		item := w.Items[0]
		if item.Variable != "" {
			return core.NewList(
				core.SymbolValue("with"),
				item.Context.ToIR(),
				core.SymbolValue(item.Variable),
				core.NewList(bodyIR...),
			)
		}
		return core.NewList(
			core.SymbolValue("with"),
			item.Context.ToIR(),
			core.None,
			core.NewList(bodyIR...),
		)
	}

	// Multiple context managers: nest them
	// with x as a, y as b: body
	// → (with x a (with y b body))
	result := core.NewList(bodyIR...)
	for i := len(w.Items) - 1; i >= 0; i-- {
		item := w.Items[i]
		if item.Variable != "" {
			result = core.NewList(
				core.SymbolValue("with"),
				item.Context.ToIR(),
				core.SymbolValue(item.Variable),
				result,
			)
		} else {
			result = core.NewList(
				core.SymbolValue("with"),
				item.Context.ToIR(),
				core.None,
				result,
			)
		}
	}

	return result
}

// ============================================================================
// Exception Handling
// ============================================================================

// ExceptClause represents an except clause in a try statement
type ExceptClause struct {
	ExceptionType     string  // Empty string for bare except (deprecated - use ExceptionTypeExpr)
	ExceptionTypeExpr ASTNode // The actual exception type expression (identifier, tuple, etc.)
	Variable          string  // Empty string if no 'as' clause
	Body              []ASTNode
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
	tryIR := make([]core.Value, 0, len(t.TryBody)+1)
	tryIR = append(tryIR, core.SymbolValue("do"))
	for _, stmt := range t.TryBody {
		tryIR = append(tryIR, stmt.ToIR())
	}

	// Start building result: (try body ...)
	result := []core.Value{core.SymbolValue("try"), core.NewList(tryIR...)}

	// Add except clauses
	for _, except := range t.ExceptClauses {
		exceptBody := make([]core.Value, 0, len(except.Body)+1)
		exceptBody = append(exceptBody, core.SymbolValue("do"))
		for _, stmt := range except.Body {
			exceptBody = append(exceptBody, stmt.ToIR())
		}

		// Determine the exception type IR value
		var excTypeIR core.Value
		if except.ExceptionTypeExpr != nil {
			// Use the AST node (handles tuples, identifiers, etc.)
			excTypeIR = except.ExceptionTypeExpr.ToIR()
		} else if except.ExceptionType != "" {
			// Fallback to string (backward compatibility)
			excTypeIR = core.SymbolValue(except.ExceptionType)
		}

		if excTypeIR != nil && except.Variable != "" {
			// except ValueError as e: ... or except (ValueError, TypeError) as e: ...
			result = append(result, core.NewList(
				core.SymbolValue("except"),
				excTypeIR,
				core.SymbolValue("as"),
				core.SymbolValue(except.Variable),
				core.NewList(exceptBody...),
			))
		} else if excTypeIR != nil {
			// except ValueError: ... or except (ValueError, TypeError): ...
			result = append(result, core.NewList(
				core.SymbolValue("except"),
				excTypeIR,
				core.NewList(exceptBody...),
			))
		} else {
			// except: ... (bare except)
			result = append(result, core.NewList(
				core.SymbolValue("except"),
				core.NewList(exceptBody...),
			))
		}
	}

	// Add else clause if present
	if len(t.ElseBody) > 0 {
		elseIR := make([]core.Value, 0, len(t.ElseBody)+1)
		elseIR = append(elseIR, core.SymbolValue("do"))
		for _, stmt := range t.ElseBody {
			elseIR = append(elseIR, stmt.ToIR())
		}
		result = append(result, core.NewList(core.SymbolValue("else"), core.NewList(elseIR...)))
	}

	// Add finally clause if present
	if len(t.FinallyBody) > 0 {
		finallyIR := make([]core.Value, 0, len(t.FinallyBody)+1)
		finallyIR = append(finallyIR, core.SymbolValue("do"))
		for _, stmt := range t.FinallyBody {
			finallyIR = append(finallyIR, stmt.ToIR())
		}
		result = append(result, core.NewList(core.SymbolValue("finally"), core.NewList(finallyIR...)))
	}

	return core.NewList(result...)
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
	basesIR := make([]core.Value, 0, len(c.Bases))
	for _, base := range c.Bases {
		basesIR = append(basesIR, base.ToIR())
	}

	// Build body
	bodyIR := make([]core.Value, 0, len(c.Body))
	for _, item := range c.Body {
		bodyIR = append(bodyIR, item.ToIR())
	}

	// Build keywords (e.g., metaclass=ABCMeta)
	keywordsIR := make([]core.Value, 0, len(c.Keywords))
	for _, kw := range c.Keywords {
		keywordsIR = append(keywordsIR, kw.ToIR())
	}

	// Basic class: (class Name [Base1 Base2] {keyword args} method1 method2 ...)
	result := []core.Value{
		core.SymbolValue("class"),
		core.SymbolValue(c.Name),
		core.NewList(basesIR...),
		core.NewList(keywordsIR...), // Add keywords as 4th element
	}
	result = append(result, bodyIR...)

	// Handle decorators
	// @decorator class Foo: ... → (= Foo (decorator (class Foo ...)))
	for i := len(c.Decorators) - 1; i >= 0; i-- {
		decorator := c.Decorators[i]
		result = []core.Value{
			decorator.ToIR(),
			core.NewList(result...),
		}
	}

	// If there are decorators, wrap in assignment
	if len(c.Decorators) > 0 {
		result = []core.Value{
			core.SymbolValue("="),
			core.SymbolValue(c.Name),
			core.NewList(result...),
		}
	}

	return core.NewList(result...)
}

// ============================================================================
// Match/Case Statement (Python 3.10+)
// ============================================================================

// CaseClause represents a single case in a match statement
type CaseClause struct {
	Pattern ASTNode   // The pattern to match (can be literal, identifier, etc.)
	Guard   ASTNode   // Optional if guard (can be nil)
	Body    []ASTNode // Statements to execute if matched
}

// MatchForm represents a match statement
type MatchForm struct {
	BaseNode
	Subject ASTNode      // The value being matched
	Cases   []CaseClause // List of case clauses
}

// NewMatchForm creates a new match statement
func NewMatchForm(subject ASTNode, cases []CaseClause, loc *core.SourceLocation, syntax SyntaxKind) *MatchForm {
	return &MatchForm{
		BaseNode: BaseNode{
			Loc:    loc,
			Syntax: syntax,
		},
		Subject: subject,
		Cases:   cases,
	}
}

// Type implements core.Value.Type
func (m *MatchForm) Type() core.Type {
	return core.ListType
}

// String implements core.Value.String
func (m *MatchForm) String() string {
	return "(match ...)"
}

// ToIR lowers match statement to IR
// For now, we'll convert to a series of if/elif statements
func (m *MatchForm) ToIR() core.Value {
	// (let ((subject-var <subject>))
	//   (if (match-pattern? subject-var pattern1)
	//     body1
	//     (if (match-pattern? subject-var pattern2)
	//       body2
	//       ...)))

	// For simplicity, we'll just convert to a series of if statements
	// More sophisticated pattern matching can be added later

	if len(m.Cases) == 0 {
		return core.SymbolValue("nil")
	}

	// Generate a let binding for the subject
	subjectVar := core.SymbolValue("__match_subject__")
	subjectIR := m.Subject.ToIR()

	// Build nested if/elif chain
	var buildCases func(int) core.Value
	buildCases = func(idx int) core.Value {
		if idx >= len(m.Cases) {
			// No more cases, return nil
			return core.SymbolValue("nil")
		}

		caseClause := m.Cases[idx]

		// Build case body
		bodyIR := make([]core.Value, 0, len(caseClause.Body)+1)
		bodyIR = append(bodyIR, core.SymbolValue("do"))
		for _, stmt := range caseClause.Body {
			bodyIR = append(bodyIR, stmt.ToIR())
		}

		// Build condition
		// For now, simple pattern matching:
		// - Identifier starting with _ is wildcard (always matches)
		// - Otherwise, use equality check
		var condition core.Value

		if ident, ok := caseClause.Pattern.(*Identifier); ok && ident.Name == "_" {
			// Wildcard pattern - always true
			condition = core.BoolValue(true)
		} else {
			// Equality check
			patternIR := caseClause.Pattern.ToIR()
			condition = core.NewList(
				core.SymbolValue("=="),
				subjectVar,
				patternIR,
			)
		}

		// Add guard if present
		if caseClause.Guard != nil {
			guardIR := caseClause.Guard.ToIR()
			condition = core.NewList(
				core.SymbolValue("and"),
				condition,
				guardIR,
			)
		}

		// Build if statement
		elseClause := buildCases(idx + 1)

		return core.NewList(
			core.SymbolValue("if"),
			condition,
			core.NewList(bodyIR...),
			elseClause,
		)
	}

	// Wrap in let binding
	ifChain := buildCases(0)

	return core.NewList(
		core.SymbolValue("let"),
		core.NewList(
			core.NewList(subjectVar, subjectIR),
		),
		ifChain,
	)
}
