package ast

import (
	"fmt"
	"strings"

	"github.com/mmichie/m28/core"
)

// Identifier represents a symbol/variable name
// Examples: x, foo, print, +
type Identifier struct {
	BaseNode
	Name string
}

// Type implements core.Value.Type
func (i *Identifier) Type() core.Type {
	return core.SymbolType
}

// String implements core.Value.String
func (i *Identifier) String() string {
	return i.Name
}

// ToIR implements ASTNode.ToIR
func (i *Identifier) ToIR() core.Value {
	return core.SymbolValue(i.Name)
}

// NewIdentifier creates a new identifier node
func NewIdentifier(name string, loc *core.SourceLocation, syntax SyntaxKind) *Identifier {
	return &Identifier{
		BaseNode: BaseNode{
			Loc:    loc,
			Syntax: syntax,
		},
		Name: name,
	}
}

// Literal represents a literal value (number, string, bool, nil)
// Examples: 42, "hello", True, None
type Literal struct {
	BaseNode
	Value core.Value
}

// Type implements core.Value.Type
func (l *Literal) Type() core.Type {
	return l.Value.Type()
}

// String implements core.Value.String
func (l *Literal) String() string {
	return l.Value.String()
}

// ToIR implements ASTNode.ToIR
func (l *Literal) ToIR() core.Value {
	return l.Value
}

// NewLiteral creates a new literal node
func NewLiteral(value core.Value, loc *core.SourceLocation, syntax SyntaxKind) *Literal {
	return &Literal{
		BaseNode: BaseNode{
			Loc:    loc,
			Syntax: syntax,
		},
		Value: value,
	}
}

// SExpr represents an S-expression (function call, special form, list)
// Examples: (+ 1 2), (def foo (x) (* x 2)), (if x y z)
type SExpr struct {
	BaseNode
	Elements []ASTNode
}

// Type implements core.Value.Type
func (s *SExpr) Type() core.Type {
	return core.ListType
}

// String implements core.Value.String
func (s *SExpr) String() string {
	parts := make([]string, len(s.Elements))
	for i, e := range s.Elements {
		parts[i] = e.String()
	}
	return "(" + strings.Join(parts, " ") + ")"
}

// ToIR implements ASTNode.ToIR
func (s *SExpr) ToIR() core.Value {
	vals := make(core.ListValue, len(s.Elements))
	for i, elem := range s.Elements {
		vals[i] = elem.ToIR()
	}
	return vals
}

// NewSExpr creates a new S-expression node
func NewSExpr(elements []ASTNode, loc *core.SourceLocation, syntax SyntaxKind) *SExpr {
	return &SExpr{
		BaseNode: BaseNode{
			Loc:    loc,
			Syntax: syntax,
		},
		Elements: elements,
	}
}

// DefForm represents a function definition
// Examples:
//
//	Lisp:   (def foo (x y) (+ x y))
//	Python: def foo(x: int, y: int) -> int: return x + y
type DefForm struct {
	BaseNode
	Name       string      // Function name
	Params     []Parameter // Parameters with optional types and defaults
	Body       ASTNode     // Function body
	ReturnType *TypeInfo   // Return type annotation (optional)
	Decorators []ASTNode   // Function decorators (e.g., @property, @staticmethod)
	IsAsync    bool        // Whether this is an async function
}

// Type implements core.Value.Type
func (d *DefForm) Type() core.Type {
	return core.ListType
}

// String implements core.Value.String
func (d *DefForm) String() string {
	// Show in Lisp syntax for consistency
	params := make([]string, len(d.Params))
	for i, p := range d.Params {
		params[i] = p.Name
		if p.Type != nil {
			params[i] += ":" + p.Type.String()
		}
	}

	result := fmt.Sprintf("(def %s (%s) %s)",
		d.Name,
		strings.Join(params, " "),
		d.Body.String(),
	)

	if d.ReturnType != nil {
		result = fmt.Sprintf("%s -> %s", result, d.ReturnType.String())
	}

	return result
}

// ToIR implements ASTNode.ToIR
func (d *DefForm) ToIR() core.Value {
	// Build (def name (params) body) or (async def name (params) body)
	// Parameters with defaults are represented as (param default-value)
	params := make(core.ListValue, len(d.Params))
	for i, p := range d.Params {
		if p.Default != nil {
			// Parameter with default: (param-name default-value)
			params[i] = core.ListValue{
				core.SymbolValue(p.Name),
				p.Default.ToIR(),
			}
		} else {
			// Parameter without default: just the symbol
			params[i] = core.SymbolValue(p.Name)
		}
	}

	var result core.Value
	if d.IsAsync {
		// Build (async def name (params) body)
		result = core.ListValue{
			core.SymbolValue("async"),
			core.SymbolValue("def"),
			core.SymbolValue(d.Name),
			params,
			d.Body.ToIR(),
		}
	} else {
		// Build (def name (params) body)
		result = core.ListValue{
			core.SymbolValue("def"),
			core.SymbolValue(d.Name),
			params,
			d.Body.ToIR(),
		}
	}

	// Handle decorators
	// @decorator1 @decorator2 def foo(): pass
	// → (= foo (decorator1 (decorator2 (def foo () ...))))
	if len(d.Decorators) > 0 {
		// Apply decorators from bottom to top (innermost to outermost)
		for i := len(d.Decorators) - 1; i >= 0; i-- {
			decorator := d.Decorators[i]
			result = core.ListValue{
				decorator.ToIR(),
				result,
			}
		}

		// Wrap in assignment
		result = core.ListValue{
			core.SymbolValue("="),
			core.SymbolValue(d.Name),
			result,
		}
	}

	return result
}

// NewDefForm creates a new function definition node
func NewDefForm(name string, params []Parameter, body ASTNode, returnType *TypeInfo, decorators []ASTNode, isAsync bool, loc *core.SourceLocation, syntax SyntaxKind) *DefForm {
	return &DefForm{
		BaseNode: BaseNode{
			Loc:    loc,
			Syntax: syntax,
		},
		Name:       name,
		Params:     params,
		Body:       body,
		ReturnType: returnType,
		Decorators: decorators,
		IsAsync:    isAsync,
	}
}

// AssignForm represents an assignment statement
// Examples:
//
//	Lisp:   (= x 10)
//	Python: x = 10
type AssignForm struct {
	BaseNode
	Target ASTNode // What to assign to (usually Identifier)
	Value  ASTNode // Value to assign
}

// Type implements core.Value.Type
func (a *AssignForm) Type() core.Type {
	return core.ListType
}

// String implements core.Value.String
func (a *AssignForm) String() string {
	return fmt.Sprintf("(= %s %s)", a.Target.String(), a.Value.String())
}

// ToIR implements ASTNode.ToIR
func (a *AssignForm) ToIR() core.Value {
	return core.ListValue{
		core.SymbolValue("="),
		a.Target.ToIR(),
		a.Value.ToIR(),
	}
}

// NewAssignForm creates a new assignment node
func NewAssignForm(target ASTNode, value ASTNode, loc *core.SourceLocation, syntax SyntaxKind) *AssignForm {
	return &AssignForm{
		BaseNode: BaseNode{
			Loc:    loc,
			Syntax: syntax,
		},
		Target: target,
		Value:  value,
	}
}

// IfForm represents a conditional expression
// Examples:
//
//	Lisp:   (if condition then-branch else-branch)
//	Python: if condition: then_branch else: else_branch
type IfForm struct {
	BaseNode
	Condition  ASTNode // Condition to test
	ThenBranch ASTNode // Branch when condition is true
	ElseBranch ASTNode // Branch when condition is false (optional)
}

// Type implements core.Value.Type
func (i *IfForm) Type() core.Type {
	return core.ListType
}

// String implements core.Value.String
func (i *IfForm) String() string {
	if i.ElseBranch != nil {
		return fmt.Sprintf("(if %s %s %s)",
			i.Condition.String(),
			i.ThenBranch.String(),
			i.ElseBranch.String(),
		)
	}
	return fmt.Sprintf("(if %s %s)",
		i.Condition.String(),
		i.ThenBranch.String(),
	)
}

// ToIR implements ASTNode.ToIR
func (i *IfForm) ToIR() core.Value {
	result := core.ListValue{
		core.SymbolValue("if"),
		i.Condition.ToIR(),
		i.ThenBranch.ToIR(),
	}
	if i.ElseBranch != nil {
		result = append(result, i.ElseBranch.ToIR())
	}
	return result
}

// NewIfForm creates a new if expression node
func NewIfForm(condition ASTNode, thenBranch ASTNode, elseBranch ASTNode, loc *core.SourceLocation, syntax SyntaxKind) *IfForm {
	return &IfForm{
		BaseNode: BaseNode{
			Loc:    loc,
			Syntax: syntax,
		},
		Condition:  condition,
		ThenBranch: thenBranch,
		ElseBranch: elseBranch,
	}
}
