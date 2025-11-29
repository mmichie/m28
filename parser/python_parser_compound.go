package parser

import (
	"fmt"
	"strings"

	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/core/ast"
)

// This file contains compound statement parsing methods for the Python parser.
// It includes:
// - Block parsing and decorators
// - Control flow statements (if, for, while, match)
// - Function definitions (def, async def, parameters, type annotations)
// - Class definitions
// - Exception handling (try/except/finally)
// - Context managers (with)

func (p *PythonParser) parseBlock() []ast.ASTNode {
	p.expect(TOKEN_COLON)

	// Support single-line statements: if condition: return value
	if !p.check(TOKEN_NEWLINE) {
		// Parse a single statement on the same line
		stmt := p.parseStatement()
		if stmt != nil {
			return []ast.ASTNode{stmt}
		}
		return []ast.ASTNode{}
	}

	p.expect(TOKEN_NEWLINE)
	p.expect(TOKEN_INDENT)

	statements := []ast.ASTNode{}

	for !p.check(TOKEN_DEDENT) && !p.isAtEnd() {
		// Skip empty lines
		for p.check(TOKEN_NEWLINE) {
			p.advance()
		}

		if p.check(TOKEN_DEDENT) {
			break
		}

		stmt := p.parseStatement()
		if stmt != nil {
			statements = append(statements, stmt)
		} else if len(p.errors) > 0 {
			// parseStatement returned nil and we have errors - stop parsing
			break
		}

		// If we hit an error, synchronize
		if p.panic {
			p.synchronize()
		}
	}

	p.expect(TOKEN_DEDENT)
	return statements
}

// parseDecorators parses: (@decorator_expr NEWLINE)*
// PEP 614 (Python 3.9+): decorators can be arbitrary expressions, not just dotted names
func (p *PythonParser) parseDecorators() []ast.ASTNode {
	decorators := []ast.ASTNode{}

	for p.check(TOKEN_AT) {
		p.advance() // consume @

		// PEP 614: Parse any expression as decorator
		// This allows: @func, @obj.method(), @lambda x: x, @a or b, @(a := b), etc.
		decorator := p.parseExpression()

		decorators = append(decorators, decorator)
		p.expect(TOKEN_NEWLINE)
	}

	return decorators
}

// ============================================================================
// Compound Statement Parsing
// ============================================================================

// parseIfStatement parses: if expr: block (elif expr: block)* (else: block)?
func (p *PythonParser) parseIfStatement() ast.ASTNode {
	// Accept both if and elif (elif is used in recursive calls)
	tok := p.peek()
	if p.check(TOKEN_IF) {
		p.advance()
	} else if p.check(TOKEN_ELIF) {
		p.advance()
	} else {
		p.error(fmt.Sprintf("expected 'if' or 'elif', got %v", p.peek().Type))
		return nil
	}

	condition := p.parseExpression()
	thenBranch := p.parseBlock()

	var elseBranch []ast.ASTNode

	// Check for elif (treated as nested if)
	if p.check(TOKEN_ELIF) {
		elseIf := p.parseIfStatement() // Recursive - will handle elif
		elseBranch = []ast.ASTNode{elseIf}
	} else if p.check(TOKEN_ELSE) {
		p.advance()
		elseBranch = p.parseBlock()
	}

	// Convert blocks to single nodes
	var thenNode ast.ASTNode
	if len(thenBranch) == 1 {
		thenNode = thenBranch[0]
	} else {
		thenNode = ast.NewBlockForm(thenBranch, p.makeLocation(tok), ast.SyntaxPython)
	}

	var elseNode ast.ASTNode
	if len(elseBranch) == 1 {
		elseNode = elseBranch[0]
	} else if len(elseBranch) > 1 {
		elseNode = ast.NewBlockForm(elseBranch, p.makeLocation(tok), ast.SyntaxPython)
	}

	return ast.NewIfForm(condition, thenNode, elseNode, p.makeLocation(tok), ast.SyntaxPython)
}

// splitLoopVariables splits a loop variable string into individual variables
// Handles:
//   - "(x, y, z)" -> ["x", "y", "z"]
//   - "x" -> ["x"]
//   - "(x, (y, z))" -> ["x", "(y, z)"] // nested tuples preserved
func splitLoopVariables(varStr string) []string {
	varStr = strings.TrimSpace(varStr)

	// If no parentheses, return as-is
	if !strings.HasPrefix(varStr, "(") {
		return []string{varStr}
	}

	// Remove outer parentheses: "(x, y)" -> "x, y"
	if strings.HasPrefix(varStr, "(") && strings.HasSuffix(varStr, ")") {
		varStr = varStr[1 : len(varStr)-1]
	}

	// Split by comma, respecting nested parentheses
	variables := []string{}
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
				variables = append(variables, strings.TrimSpace(current.String()))
				current.Reset()
			} else {
				current.WriteRune(ch)
			}
		default:
			current.WriteRune(ch)
		}
	}

	// Add last variable
	if current.Len() > 0 {
		variables = append(variables, strings.TrimSpace(current.String()))
	}

	return variables
}

// parseForStatement parses: for var[, var2, ...] in iterable: block (else: block)?
func (p *PythonParser) parseForStatement() ast.ASTNode {
	tok := p.expect(TOKEN_FOR)

	// Parse loop variables using parseLoopVariables to support:
	// - Single variable: for x in ...
	// - Tuple unpacking: for x, y in ...
	// - Nested tuples: for key,(begin,end) in ...
	variable := p.parseLoopVariables()

	p.expect(TOKEN_IN)

	// Parse iterable expression
	// Special handling for implicit tuple syntax: for x in 1, 2, 3:
	iterable := p.parseForIterable()

	// Parse body
	body := p.parseBlock()

	// Check for optional else clause
	var elseBody []ast.ASTNode
	if p.check(TOKEN_ELSE) {
		p.advance()
		elseBody = p.parseBlock()
	}

	// Split variable string into individual variables
	// e.g. "(index, name)" -> ["index", "name"]
	//      "x" -> ["x"]
	variables := splitLoopVariables(variable)

	if len(variables) == 1 {
		return ast.NewForForm(variables[0], iterable, body, elseBody, p.makeLocation(tok), ast.SyntaxPython)
	} else {
		return ast.NewForFormMulti(variables, iterable, body, elseBody, p.makeLocation(tok), ast.SyntaxPython)
	}
}

// parseForIterable parses the iterable expression in a for loop
// Handles implicit tuple syntax: for x in 1, 2, 3: means for x in (1, 2, 3):
// Parses comma-separated expressions until reaching ':'
func (p *PythonParser) parseForIterable() ast.ASTNode {
	startTok := p.peek()

	// Parse first expression (may include star unpacking)
	exprs := []ast.ASTNode{p.parseListElement()}

	// Check for comma-separated values (implicit tuple with possible star unpacking)
	// Example: for x in *a, *b, *c:
	// Continue collecting expressions until we hit a colon
	for p.check(TOKEN_COMMA) {
		p.advance() // consume comma

		// If we hit a colon after the comma, we have a trailing comma - stop here
		if p.check(TOKEN_COLON) {
			break
		}

		// Parse each element, supporting star unpacking
		exprs = append(exprs, p.parseListElement())
	}

	// If we got multiple expressions, create an implicit tuple
	if len(exprs) == 1 {
		return exprs[0]
	}

	// Create tuple-literal form (similar to parseExpressionStatement)
	tupleSym := ast.NewIdentifier("tuple-literal", p.makeLocation(startTok), ast.SyntaxPython)
	allElements := append([]ast.ASTNode{tupleSym}, exprs...)
	return ast.NewSExpr(allElements, p.makeLocation(startTok), ast.SyntaxPython)
}

// parseWhileStatement parses: while expr: block (else: block)?
func (p *PythonParser) parseWhileStatement() ast.ASTNode {
	tok := p.expect(TOKEN_WHILE)

	// Parse condition
	condition := p.parseExpression()

	// Parse body
	body := p.parseBlock()

	// Check for optional else clause
	var elseBody []ast.ASTNode
	if p.check(TOKEN_ELSE) {
		p.advance()
		elseBody = p.parseBlock()
	}

	return ast.NewWhileForm(condition, body, elseBody, p.makeLocation(tok), ast.SyntaxPython)
}

// parseAsyncStatement parses: async (def|for|with)
func (p *PythonParser) parseAsyncStatement() ast.ASTNode {
	p.expect(TOKEN_ASYNC)

	switch p.peek().Type {
	case TOKEN_DEF:
		return p.parseDefStatement(nil, true)
	case TOKEN_FOR:
		// async for - for now, treat like regular for
		return p.parseForStatement()
	case TOKEN_WITH:
		// async with - use async context manager protocol
		return p.parseWithStatementWithAsync(true)
	default:
		p.error(fmt.Sprintf("Expected 'def', 'for', or 'with' after 'async', got %v", p.peek().Type))
		return nil
	}
}

// parseDefStatement parses: (@decorator)* (async)? def name(params) (-> type)?: block
func (p *PythonParser) parseDefStatement(decorators []ast.ASTNode, isAsync bool) ast.ASTNode {
	tok := p.expect(TOKEN_DEF)

	// Parse function name
	nameTok := p.expect(TOKEN_IDENTIFIER)
	name := nameTok.Lexeme

	// Log Pythonic function definition desugaring
	core.Log.Trace(core.SubsystemParser, "Desugaring Pythonic function definition", "file", p.filename, "function", name, "line", tok.Line, "col", tok.Col, "is_async", isAsync)

	// Parse optional PEP 695 type parameters: def func[T, U](params):
	// Type parameters are for runtime (Python 3.12+) but we can parse and ignore them
	if p.check(TOKEN_LBRACKET) {
		p.advance()

		// Parse comma-separated list of type parameter names
		if !p.check(TOKEN_RBRACKET) {
			for {
				// Each type parameter can be:
				// - Simple name: T
				// - Name with constraint: T: SomeType
				// - Name with default: T = SomeType
				// For now, we just consume the tokens without processing

				_ = p.expect(TOKEN_IDENTIFIER) // type parameter name

				// Check for constraint (: Type) or default (= Type)
				if p.check(TOKEN_COLON) {
					p.advance()
					_ = p.parseTypeName() // parse the constraint type
				} else if p.check(TOKEN_ASSIGN) {
					p.advance()
					_ = p.parseTypeName() // parse the default type
				}

				// Check for more type parameters
				if !p.check(TOKEN_COMMA) {
					break
				}
				p.advance() // consume comma
			}
		}

		p.expect(TOKEN_RBRACKET)
	}

	// Parse parameters
	p.expect(TOKEN_LPAREN)
	params := p.parseParameters()
	p.expect(TOKEN_RPAREN)

	// Validate parameters - check for invalid star-only patterns
	p.validateParameters(params, tok)

	// Parse optional return type annotation
	var returnType *ast.TypeInfo
	if p.check(TOKEN_ARROW) {
		p.advance()
		returnType = p.parseTypeAnnotation()
	}

	// Parse body - can be either indented block or inline statement
	p.expect(TOKEN_COLON)

	var bodyNode ast.ASTNode
	if p.check(TOKEN_NEWLINE) || p.check(TOKEN_SEMICOLON) {
		// Indented block
		p.advance() // consume newline
		p.expect(TOKEN_INDENT)

		statements := []ast.ASTNode{}
		for !p.check(TOKEN_DEDENT) && !p.isAtEnd() {
			// Skip empty lines
			for p.check(TOKEN_NEWLINE) {
				p.advance()
			}

			if p.check(TOKEN_DEDENT) {
				break
			}

			stmt := p.parseStatement()
			if stmt != nil {
				statements = append(statements, stmt)
			} else if len(p.errors) > 0 {
				break
			}

			if p.panic {
				p.synchronize()
			}
		}

		p.expect(TOKEN_DEDENT)

		if len(statements) == 1 {
			bodyNode = statements[0]
		} else {
			bodyNode = ast.NewBlockForm(statements, p.makeLocation(tok), ast.SyntaxPython)
		}
	} else {
		// Inline statement: def f(): pass
		bodyNode = p.parseStatement()
	}

	return ast.NewDefForm(name, params, bodyNode, returnType, decorators, isAsync, p.makeLocation(tok), ast.SyntaxPython)
}

// parseParameters parses: (param (: type)? (= default)?, ...)*
// Also handles *args and **kwargs
func (p *PythonParser) parseParameters() []ast.Parameter {
	params := []ast.Parameter{}

	if p.check(TOKEN_RPAREN) {
		return params // Empty parameter list
	}

	for {
		// Handle positional-only separator (/)
		if shouldContinue, shouldBreak := p.handlePositionalOnlySeparator(&params); shouldBreak {
			break
		} else if shouldContinue {
			continue
		}

		// Handle *args or keyword-only separator (*)
		if param, ok := p.parseVarArgsParameter(); ok {
			params = append(params, param)
			if p.shouldBreakParameterLoop() {
				break
			}
			continue
		}

		// Handle **kwargs
		if param, ok := p.parseKwargsParameter(); ok {
			params = append(params, param)
			if p.shouldBreakParameterLoop() {
				break
			}
			continue
		}

		// Parse regular parameter
		param := p.parseRegularParameter()
		params = append(params, param)

		if p.shouldBreakParameterLoop() {
			break
		}
	}

	return params
}

// handlePositionalOnlySeparator handles the / separator for positional-only parameters
// Returns (shouldContinue, shouldBreak) to control the parameter parsing loop
func (p *PythonParser) handlePositionalOnlySeparator(params *[]ast.Parameter) (bool, bool) {
	if !p.check(TOKEN_SLASH) {
		return false, false // Didn't see /, continue normal processing
	}

	p.advance() // consume /

	// If no comma after /, we're done with parameters
	if !p.check(TOKEN_COMMA) {
		return false, true // Break the loop
	}
	p.advance() // consume comma

	// If rparen after comma, we're done
	if p.check(TOKEN_RPAREN) {
		return false, true // Break the loop
	}

	return true, false // Continue to next iteration
}

// parseVarArgsParameter parses *args or bare * (keyword-only separator)
// Returns (parameter, true) if found, (zero-value, false) otherwise
func (p *PythonParser) parseVarArgsParameter() (ast.Parameter, bool) {
	if !p.check(TOKEN_STAR) {
		return ast.Parameter{}, false
	}

	p.advance() // consume *

	// Check if this is just a keyword-only separator (bare *)
	if p.check(TOKEN_COMMA) || p.check(TOKEN_RPAREN) {
		param := ast.Parameter{Name: "*"}
		// Don't consume the comma here - let shouldBreakParameterLoop() handle it
		return param, true
	}

	// *args case
	nameTok := p.expect(TOKEN_IDENTIFIER)
	param := ast.Parameter{
		Name:      "*" + nameTok.Lexeme, // Prefix with * to mark as varargs
		IsVarArgs: true,
	}

	// Parse optional type annotation
	if p.check(TOKEN_COLON) {
		p.advance()
		param.Type = p.parseTypeAnnotation()
	}

	return param, true
}

// parseKwargsParameter parses **kwargs
// Returns (parameter, true) if found, (zero-value, false) otherwise
func (p *PythonParser) parseKwargsParameter() (ast.Parameter, bool) {
	if !p.check(TOKEN_DOUBLESTAR) {
		return ast.Parameter{}, false
	}

	p.advance() // consume **
	nameTok := p.expect(TOKEN_IDENTIFIER)
	param := ast.Parameter{
		Name:     "**" + nameTok.Lexeme, // Prefix with ** to mark as kwargs
		IsKwargs: true,
	}

	// Parse optional type annotation
	if p.check(TOKEN_COLON) {
		p.advance()
		param.Type = p.parseTypeAnnotation()
	}

	return param, true
}

// parseRegularParameter parses a regular parameter with optional type and default
func (p *PythonParser) parseRegularParameter() ast.Parameter {
	nameTok := p.expect(TOKEN_IDENTIFIER)
	param := ast.Parameter{Name: nameTok.Lexeme}

	// Parse optional type annotation
	if p.check(TOKEN_COLON) {
		p.advance()
		param.Type = p.parseTypeAnnotation()
	}

	// Parse optional default value
	if p.check(TOKEN_ASSIGN) {
		p.advance()
		param.Default = p.parseExpression()
	}

	return param
}

// shouldBreakParameterLoop determines if we should exit the parameter parsing loop
func (p *PythonParser) shouldBreakParameterLoop() bool {
	// If no comma, we're done
	if !p.check(TOKEN_COMMA) {
		return true
	}

	p.advance() // consume comma

	// Allow trailing comma
	return p.check(TOKEN_RPAREN)
}

// validateParameters checks for invalid parameter patterns
// Python requires at least one keyword-only parameter after a bare *
func (p *PythonParser) validateParameters(params []ast.Parameter, tok Token) {
	// Find bare * (keyword-only separator without a name)
	barStarIndex := -1
	for i, param := range params {
		if param.Name == "*" && !param.IsVarArgs {
			barStarIndex = i
			break
		}
	}

	if barStarIndex == -1 {
		return // No bare *, nothing to validate
	}

	// Check if there's at least one keyword-only parameter after the bare *
	hasKeywordOnlyParam := false
	for i := barStarIndex + 1; i < len(params); i++ {
		param := params[i]
		// Skip **kwargs - it doesn't count as a keyword-only parameter
		if param.IsKwargs || (len(param.Name) > 2 && param.Name[:2] == "**") {
			continue
		}
		// Found a regular keyword-only parameter
		hasKeywordOnlyParam = true
		break
	}

	if !hasKeywordOnlyParam {
		p.error("named arguments must follow bare *")
	}
}

// parseTypeAnnotation parses a type annotation
// In Python, type annotations can be arbitrary expressions: int, str, List[int], 1/0, etc.
func (p *PythonParser) parseTypeAnnotation() *ast.TypeInfo {
	// Type annotations in Python are arbitrary expressions
	// They can be: identifiers, dotted names, generics, unions, or even complex expressions like 1/0
	// We parse the expression and extract a type name for the TypeInfo

	// Try to parse as a simple type name first (common case optimization)
	if p.check(TOKEN_IDENTIFIER) || p.check(TOKEN_STRING) || p.check(TOKEN_NIL) ||
		p.check(TOKEN_TRUE) || p.check(TOKEN_FALSE) || p.check(TOKEN_LBRACKET) {
		typeName := p.parseTypeName()

		// Handle generic types like dict[str, object] or list[int]
		if p.check(TOKEN_LBRACKET) {
			p.advance() // consume [
			p.parseTypeArguments()
			p.expect(TOKEN_RBRACKET)
		}

		// Handle union types with | (Python 3.10+)
		for p.check(TOKEN_PIPE) {
			p.advance() // consume |
			_ = p.parseTypeName()
			if p.check(TOKEN_LBRACKET) {
				p.advance()
				p.parseTypeArguments()
				p.expect(TOKEN_RBRACKET)
			}
		}

		return &ast.TypeInfo{Name: typeName}
	}

	// Fall back to parsing any expression (for cases like 1/0, function calls, etc.)
	// We don't actually use the expression, just consume the tokens
	_ = p.parseExpression()

	// Return a generic type name since we can't extract a meaningful name from arbitrary expressions
	return &ast.TypeInfo{Name: "object"}
}

// parseTypeName parses a potentially dotted type name like "typing.Optional" or "events.AbstractEventLoop"
// Also handles string literals for forward references like 'IO[AnyStr]'
// Returns the full dotted name as a string
func (p *PythonParser) parseTypeName() string {
	// Type annotations can be:
	// - Identifiers: int, str, MyClass
	// - String literals (forward references): 'MyClass', "IO[AnyStr]"
	// - Special constants: None, True, False
	// - List literals: [int, str] (for union-like annotations)
	var typeName string
	if p.check(TOKEN_STRING) {
		// Forward reference - quoted string like 'IO[AnyStr]'
		// We just extract the string value
		stringTok := p.advance()
		typeName = stringTok.Lexeme
		// Remove quotes if present
		if len(typeName) >= 2 && (typeName[0] == '"' || typeName[0] == '\'') {
			typeName = typeName[1 : len(typeName)-1]
		}
		return typeName // String forward references don't have dotted attribute access
	} else if p.check(TOKEN_LBRACKET) {
		// List literal as type annotation like [int, str]
		// Parse it but return a placeholder type name
		p.advance() // consume [
		// Parse list contents - can be type names separated by commas
		if !p.check(TOKEN_RBRACKET) {
			for {
				_ = p.parseTypeName() // recursive parse of element types
				if !p.check(TOKEN_COMMA) {
					break
				}
				p.advance() // consume comma
			}
		}
		p.expect(TOKEN_RBRACKET)
		return "list" // return generic "list" type name
	} else if p.check(TOKEN_IDENTIFIER) {
		nameTok := p.advance()
		typeName = nameTok.Lexeme
	} else if p.check(TOKEN_NIL) {
		p.advance()
		typeName = "None"
	} else if p.check(TOKEN_TRUE) {
		p.advance()
		typeName = "True"
	} else if p.check(TOKEN_FALSE) {
		p.advance()
		typeName = "False"
	} else {
		p.error("Expected type name in type annotation")
		return "object" // fallback
	}

	// Handle dotted names like typing.Optional, events.AbstractEventLoop
	for p.check(TOKEN_DOT) {
		p.advance() // consume .
		if !p.check(TOKEN_IDENTIFIER) {
			p.error("Expected identifier after dot in type name")
			break
		}
		attrTok := p.advance()
		typeName += "." + attrTok.Lexeme
	}

	return typeName
}

// parseTypeArguments parses the contents of generic type brackets
// e.g., "str, object" in dict[str, object]
// Also handles complex nested types like typing.Callable[[], typing.Awaitable]
func (p *PythonParser) parseTypeArguments() {
	if p.check(TOKEN_RBRACKET) {
		return // empty brackets like []
	}

	for {
		// Check if we hit a closing bracket (for empty nested lists like [])
		if p.check(TOKEN_RBRACKET) {
			return
		}

		// Check for ellipsis ... in type arguments (e.g., Callable[..., Any])
		if p.check(TOKEN_ELLIPSIS) {
			p.advance() // consume ...
		} else if p.check(TOKEN_LBRACKET) {
			// Check if this type argument starts with [ (empty list notation like [])
			// This is used in Callable[[], ReturnType] where [] represents no parameters
			p.advance()
			p.parseTypeArguments()
			p.expect(TOKEN_RBRACKET)
		} else {
			// Parse one type argument - use parseTypeName to handle dotted names
			_ = p.parseTypeName()

			// Handle nested generics like List[Dict[str, int]] or Callable[[], Awaitable]
			if p.check(TOKEN_LBRACKET) {
				p.advance()
				p.parseTypeArguments()
				p.expect(TOKEN_RBRACKET)
			}

			// Handle union types within type arguments
			for p.check(TOKEN_PIPE) {
				p.advance() // consume |
				_ = p.parseTypeName()
				if p.check(TOKEN_LBRACKET) {
					p.advance()
					p.parseTypeArguments()
					p.expect(TOKEN_RBRACKET)
				}
			}
		}

		// Check for more arguments
		if !p.check(TOKEN_COMMA) {
			break
		}
		p.advance() // consume comma

		// After comma, check if we're at the end (trailing comma case)
		if p.check(TOKEN_RBRACKET) {
			return
		}
	}
}

// parseClassStatement parses: (@decorator)* class name (bases)?: block
func (p *PythonParser) parseClassStatement(decorators []ast.ASTNode) ast.ASTNode {
	tok := p.expect(TOKEN_CLASS)

	// Parse class name
	nameTok := p.expect(TOKEN_IDENTIFIER)
	name := nameTok.Lexeme

	// Parse optional PEP 695 type parameters: class Name[T, U](Base):
	// Type parameters are for runtime (Python 3.12+) but we can parse and ignore them
	if p.check(TOKEN_LBRACKET) {
		p.advance()

		// Parse comma-separated list of type parameter names
		if !p.check(TOKEN_RBRACKET) {
			for {
				// Each type parameter can be:
				// - Simple name: T
				// - Name with constraint: T: SomeType
				// - Name with default: T = SomeType
				// For now, we just consume the tokens without processing

				_ = p.expect(TOKEN_IDENTIFIER) // type parameter name

				// Check for constraint (: Type) or default (= Type)
				if p.check(TOKEN_COLON) {
					p.advance()
					_ = p.parseTypeName() // parse the constraint type
				} else if p.check(TOKEN_ASSIGN) {
					p.advance()
					_ = p.parseTypeName() // parse the default type
				}

				// Check for more type parameters
				if !p.check(TOKEN_COMMA) {
					break
				}
				p.advance() // consume comma
			}
		}

		p.expect(TOKEN_RBRACKET)
	}

	// Parse optional base classes and keyword arguments (like metaclass=)
	var bases []ast.ASTNode
	var keywords []ast.ASTNode  // keyword arguments like metaclass=...
	var starBases ast.ASTNode   // *args unpacking
	var kwargUnpack ast.ASTNode // **kwargs unpacking

	if p.check(TOKEN_LPAREN) {
		p.advance()

		if !p.check(TOKEN_RPAREN) {
			for {
				// Skip newlines in multi-line base class list
				for p.check(TOKEN_NEWLINE) {
					p.advance()
				}

				// Check if we've reached the end (trailing comma case)
				if p.check(TOKEN_RPAREN) {
					break
				}

				// Check for **kwargs unpacking
				if p.check(TOKEN_DOUBLESTAR) {
					p.advance()
					kwargUnpack = p.parseExpression()
				} else if p.check(TOKEN_STAR) {
					// Check for *args unpacking
					p.advance()
					starBases = p.parseExpression()
				} else if p.check(TOKEN_IDENTIFIER) && p.current+1 < len(p.tokens) && p.tokens[p.current+1].Type == TOKEN_ASSIGN {
					// Check for keyword argument: IDENTIFIER = expression
					nameTok := p.advance()
					p.expect(TOKEN_ASSIGN)
					value := p.parseExpression()

					// Store as a keyword-value pair
					kwPair := ast.NewSExpr([]ast.ASTNode{
						ast.NewLiteral(core.StringValue(nameTok.Lexeme), p.makeLocation(nameTok), ast.SyntaxPython),
						value,
					}, p.makeLocation(nameTok), ast.SyntaxPython)
					keywords = append(keywords, kwPair)
				} else {
					// Regular base class
					bases = append(bases, p.parseExpression())
				}

				if !p.check(TOKEN_COMMA) {
					break
				}
				p.advance()
			}
		}

		p.expect(TOKEN_RPAREN)
	}

	// Parse body
	body := p.parseBlock()

	return ast.NewClassForm(name, bases, body, decorators, keywords, starBases, kwargUnpack, p.makeLocation(tok), ast.SyntaxPython)
}

// parseTryStatement parses: try: block (except (type (as var))?: block)+ (else: block)? (finally: block)?
func (p *PythonParser) parseTryStatement() ast.ASTNode {
	tok := p.expect(TOKEN_TRY)

	// Parse try body
	tryBody := p.parseBlock()

	// Parse except clauses (including except* for exception groups)
	var exceptClauses []ast.ExceptClause

	for p.check(TOKEN_EXCEPT) {
		p.advance() // consume EXCEPT

		// Check for except* (exception groups, PEP 654 - Python 3.11+)
		isExceptStar := false
		if p.check(TOKEN_STAR) {
			p.advance() // consume *
			isExceptStar = true
		}

		var exceptType string
		var exceptVar string

		var typeExpr ast.ASTNode
		// Check if there's an exception type
		if !p.check(TOKEN_COLON) {
			// Exception type can be an identifier, parenthesized identifier, or tuple
			typeExpr = p.parseExpression()

			// For backward compatibility, also store as string
			if ident, ok := typeExpr.(*ast.Identifier); ok {
				exceptType = ident.Name
			} else {
				// For complex types, leave exceptType empty (will use ExceptionTypeExpr instead)
				exceptType = ""
			}

			// Check for 'as variable'
			if p.check(TOKEN_AS) {
				p.advance()
				varTok := p.expect(TOKEN_IDENTIFIER)
				exceptVar = varTok.Lexeme
			}
		}

		// Parse except body
		exceptBody := p.parseBlock()

		exceptClauses = append(exceptClauses, ast.ExceptClause{
			ExceptionType:     exceptType,
			ExceptionTypeExpr: typeExpr,
			Variable:          exceptVar,
			IsExceptStar:      isExceptStar,
			Body:              exceptBody,
		})
	}

	// Parse optional else clause
	var elseBody []ast.ASTNode
	if p.check(TOKEN_ELSE) {
		p.advance()
		elseBody = p.parseBlock()
	}

	// Parse optional finally clause
	var finallyBody []ast.ASTNode
	if p.check(TOKEN_FINALLY) {
		p.advance()
		finallyBody = p.parseBlock()
	}

	return ast.NewTryForm(tryBody, exceptClauses, elseBody, finallyBody, p.makeLocation(tok), ast.SyntaxPython)
}

// parseWithStatement parses: with expr (as var)?(, expr (as var)?)* : block
// Also supports Python 3.10+ parenthesized form: with (expr as var, expr as var): block
func (p *PythonParser) parseWithStatement() ast.ASTNode {
	return p.parseWithStatementWithAsync(false)
}

// parseWithStatementWithAsync parses with statements, optionally as async
func (p *PythonParser) parseWithStatementWithAsync(isAsync bool) ast.ASTNode {
	tok := p.expect(TOKEN_WITH)

	// Check for parenthesized form (Python 3.10+)
	// with (manager() as x, manager2() as y): ...
	isParenthesized := p.check(TOKEN_LPAREN)
	if isParenthesized {
		p.advance() // consume (

		// Skip any newlines after opening paren
		for p.check(TOKEN_NEWLINE) {
			p.advance()
		}
	}

	// Parse context managers
	var items []ast.WithItem

	for {
		// Parse context expression
		context := p.parseExpression()

		var variable string
		var target ast.ASTNode
		if p.check(TOKEN_AS) {
			p.advance()
			// Check if target is a tuple (starts with LPAREN) or simple identifier
			if p.check(TOKEN_LPAREN) {
				// Parse tuple unpacking: (a, b, c)
				target = p.parsePrimary()
			} else {
				// Simple identifier
				varTok := p.expect(TOKEN_IDENTIFIER)
				variable = varTok.Lexeme
			}
		}

		items = append(items, ast.WithItem{
			Context:  context,
			Variable: variable,
			Target:   target,
		})

		// Skip newlines in parenthesized form
		if isParenthesized {
			for p.check(TOKEN_NEWLINE) {
				p.advance()
			}
		}

		if !p.check(TOKEN_COMMA) {
			break
		}
		p.advance() // consume comma

		// Skip newlines after comma in parenthesized form
		if isParenthesized {
			for p.check(TOKEN_NEWLINE) {
				p.advance()
			}
			// Check for trailing comma before closing paren
			if p.check(TOKEN_RPAREN) {
				break
			}
		}
	}

	// If parenthesized, expect closing paren
	if isParenthesized {
		// Skip any trailing newlines before closing paren
		for p.check(TOKEN_NEWLINE) {
			p.advance()
		}
		p.expect(TOKEN_RPAREN)
	}

	// Parse body
	body := p.parseBlock()

	if isAsync {
		return ast.NewAsyncWithForm(items, body, p.makeLocation(tok), ast.SyntaxPython)
	}
	return ast.NewWithForm(items, body, p.makeLocation(tok), ast.SyntaxPython)
}

// parseMatchStatement parses: match subject: case pattern: block ...
func (p *PythonParser) parseMatchStatement() ast.ASTNode {
	tok := p.expect(TOKEN_IDENTIFIER) // "match" as soft keyword
	if tok.Lexeme != "match" {
		p.error("Expected 'match'")
		return nil
	}

	// Parse subject expression
	subject := p.parseExpression()

	// Expect colon
	p.expect(TOKEN_COLON)

	// Parse block of case clauses
	var cases []ast.CaseClause

	// Expect NEWLINE and INDENT
	p.expect(TOKEN_NEWLINE)
	p.expect(TOKEN_INDENT)

	// Parse case clauses
	for p.check(TOKEN_IDENTIFIER) && p.peek().Lexeme == "case" {
		p.advance() // consume 'case'

		// Parse pattern
		pattern := p.parsePattern()

		// Check for optional guard (if condition)
		var guard ast.ASTNode
		if p.check(TOKEN_IF) {
			p.advance()
			guard = p.parseExpression()
		}

		// Parse case body (parseBlock expects and consumes the colon itself)
		body := p.parseBlock()

		cases = append(cases, ast.CaseClause{
			Pattern: pattern,
			Guard:   guard,
			Body:    body,
		})
	}

	// Expect DEDENT
	p.expect(TOKEN_DEDENT)

	return ast.NewMatchForm(subject, cases, p.makeLocation(tok), ast.SyntaxPython)
}

// parsePattern parses a pattern in a case clause
// Patterns can be:
// - Literal values: 1, "hello", True, None
// - Wildcard: _
// - Class patterns: Point(x, y), ast.Expr(expr)
// - Or patterns: pattern1 | pattern2
// For now, we handle simple patterns and class patterns
func (p *PythonParser) parsePattern() ast.ASTNode {
	// Check for literal patterns
	tok := p.peek()

	switch tok.Type {
	case TOKEN_NUMBER, TOKEN_STRING:
		p.advance()
		return ast.NewLiteral(tok.Value, p.makeLocation(tok), ast.SyntaxPython)

	case TOKEN_TRUE:
		p.advance()
		return ast.NewLiteral(core.True, p.makeLocation(tok), ast.SyntaxPython)

	case TOKEN_FALSE:
		p.advance()
		return ast.NewLiteral(core.False, p.makeLocation(tok), ast.SyntaxPython)

	case TOKEN_NIL:
		p.advance()
		return ast.NewLiteral(core.Nil, p.makeLocation(tok), ast.SyntaxPython)

	case TOKEN_IDENTIFIER:
		// Could be:
		// - Wildcard: _
		// - Variable binding
		// - Class pattern: ClassName(...) or ast.Expr(...)
		ident := p.advance()
		var node ast.ASTNode = ast.NewIdentifier(ident.Lexeme, p.makeLocation(ident), ast.SyntaxPython)

		// Handle dotted attribute access (e.g., ast.Expr)
		for p.check(TOKEN_DOT) {
			node = p.parseAttribute(node)
		}

		// Check for class pattern (function call syntax)
		if p.check(TOKEN_LPAREN) {
			// Parse as a call pattern (treat like function call)
			// ClassName(pattern1, pattern2, ...) or ast.Expr(pattern1, ...)
			return p.parseCall(node)
		}

		// Simple identifier or wildcard
		return node

	case TOKEN_LPAREN:
		// Tuple pattern or grouped pattern
		return p.parseParenthesized()

	case TOKEN_LBRACKET:
		// List pattern
		return p.parseListLiteral()

	default:
		p.error(fmt.Sprintf("Unexpected token in pattern: %v", tok.Type))
		return nil
	}
}

// ============================================================================
// F-String Support
// ============================================================================

// interpretFStringEscapes interprets Python escape sequences in an f-string literal part
// This handles \n, \t, \\, etc. in the non-expression parts of f-strings
