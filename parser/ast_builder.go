package parser

import (
	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/core/ast"
)

// AST builder helpers that create AST nodes with source location tracking

// makeLocation creates a SourceLocation from a token
func (p *Parser) makeLocation(tok Token) *core.SourceLocation {
	return &core.SourceLocation{
		File:   p.filename,
		Line:   tok.Line,
		Column: tok.Col,
	}
}

// makeLocationFromCurrent creates a SourceLocation from the current token
func (p *Parser) makeLocationFromCurrent() *core.SourceLocation {
	return p.makeLocation(p.currentToken())
}

// makeIdentifier creates an Identifier AST node from a token
func (p *Parser) makeIdentifier(name string, tok Token) *ast.Identifier {
	return ast.NewIdentifier(name, p.makeLocation(tok), ast.SyntaxLisp)
}

// makeLiteral creates a Literal AST node from a value and token
func (p *Parser) makeLiteral(value core.Value, tok Token) *ast.Literal {
	return ast.NewLiteral(value, p.makeLocation(tok), ast.SyntaxLisp)
}

// makeSExpr creates an SExpr AST node from elements and a location token
func (p *Parser) makeSExpr(elements []ast.ASTNode, tok Token) *ast.SExpr {
	return ast.NewSExpr(elements, p.makeLocation(tok), ast.SyntaxLisp)
}

// makeDefForm creates a DefForm AST node
func (p *Parser) makeDefForm(name string, params []ast.Parameter, body ast.ASTNode, returnType *ast.TypeInfo, tok Token) *ast.DefForm {
	return ast.NewDefForm(name, params, body, returnType, p.makeLocation(tok), ast.SyntaxLisp)
}

// makeAssignForm creates an AssignForm AST node
func (p *Parser) makeAssignForm(target ast.ASTNode, value ast.ASTNode, tok Token) *ast.AssignForm {
	return ast.NewAssignForm(target, value, p.makeLocation(tok), ast.SyntaxLisp)
}

// makeIfForm creates an IfForm AST node
func (p *Parser) makeIfForm(condition ast.ASTNode, thenBranch ast.ASTNode, elseBranch ast.ASTNode, tok Token) *ast.IfForm {
	return ast.NewIfForm(condition, thenBranch, elseBranch, p.makeLocation(tok), ast.SyntaxLisp)
}
