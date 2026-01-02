// Package ast provides the abstract syntax tree types for M28.
// This layer sits between parsing and evaluation, enabling:
// - Multiple frontend languages (Python, Lisp, DSLs)
// - Source location tracking for error messages
// - Type annotation preservation
// - AST-based optimizations and tooling
package ast

import (
	"github.com/mmichie/m28/core"
)

// SyntaxKind identifies which frontend produced this AST node
type SyntaxKind int

const (
	// SyntaxLisp indicates M28 Lisp syntax: (def foo (x) (* x 2))
	SyntaxLisp SyntaxKind = iota
	// SyntaxPython indicates Python syntax: def foo(x): return x * 2
	SyntaxPython
	// SyntaxJSON indicates JSON/configuration DSL syntax
	SyntaxJSON
	// SyntaxCustom indicates a custom DSL
	SyntaxCustom
)

func (sk SyntaxKind) String() string {
	switch sk {
	case SyntaxLisp:
		return "Lisp"
	case SyntaxPython:
		return "Python"
	case SyntaxJSON:
		return "JSON"
	case SyntaxCustom:
		return "Custom"
	default:
		return "Unknown"
	}
}

// TypeInfo represents type annotations (for gradual typing)
// Examples:
//
//	int                    → TypeInfo{Name: "int"}
//	List[int]              → TypeInfo{Name: "List", Generic: []{Name: "int"}}
//	Dict[str, int]         → TypeInfo{Name: "Dict", Generic: []{Name: "str"}, {Name: "int"}}
//	Optional[str]          → TypeInfo{Name: "str", IsOptional: true}
type TypeInfo struct {
	Name       string      // "int", "str", "List", "Dict", etc.
	Generic    []*TypeInfo // For List[int], Dict[str, int]
	IsOptional bool        // For Optional[T]
}

func (ti *TypeInfo) String() string {
	if ti == nil {
		return "Any"
	}

	result := ti.Name

	if len(ti.Generic) > 0 {
		result += "["
		for i, g := range ti.Generic {
			if i > 0 {
				result += ", "
			}
			result += g.String()
		}
		result += "]"
	}

	if ti.IsOptional {
		result = "Optional[" + result + "]"
	}

	return result
}

// ASTNode represents a node in the abstract syntax tree
// All AST nodes implement this interface
type ASTNode interface {
	core.Value // Still a Value for backwards compatibility during migration

	// Location returns the source location of this node
	Location() *core.SourceLocation

	// SyntaxKind returns which frontend produced this node
	SyntaxKind() SyntaxKind

	// Comments returns any comments attached to this node
	Comments() []string

	// TypeAnnotation returns the type annotation, if any
	TypeAnnotation() *TypeInfo

	// ToIR lowers this AST node to core.Value for evaluation
	// This is where desugaring happens (Pythonic → Lisp)
	ToIR() core.Value
}

// BaseNode provides common fields that all AST nodes embed
type BaseNode struct {
	Loc     *core.SourceLocation // Source location (file, line, column)
	Syntax  SyntaxKind           // Which syntax produced this
	Comment []string             // Attached comments
	Type    *TypeInfo            // Type annotation (optional)
}

// Location implements ASTNode.Location
func (b *BaseNode) Location() *core.SourceLocation {
	return b.Loc
}

// SyntaxKind implements ASTNode.SyntaxKind
func (b *BaseNode) SyntaxKind() SyntaxKind {
	return b.Syntax
}

// Comments implements ASTNode.Comments
func (b *BaseNode) Comments() []string {
	return b.Comment
}

// TypeAnnotation implements ASTNode.TypeAnnotation
func (b *BaseNode) TypeAnnotation() *TypeInfo {
	return b.Type
}

// Parameter represents a function parameter with optional type annotation and default value
type Parameter struct {
	Name           string    // Parameter name
	Type           *TypeInfo // Type annotation (optional)
	Default        ASTNode   // Default value (optional)
	IsVarArgs      bool      // True for *args parameters
	IsKwargs       bool      // True for **kwargs parameters
	PositionalOnly bool      // True for parameters before / separator (PEP 570)
	KeywordOnly    bool      // True for parameters after * separator (PEP 3102)
}
