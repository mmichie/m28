package modules

import (
	"fmt"
	"os"
	"sync"

	"github.com/mmichie/m28/core/ast"
	"github.com/mmichie/m28/parser"
)

// PythonModuleTranspiler transpiles Python source to M28 AST
type PythonModuleTranspiler struct {
	// In-memory cache: file path → AST
	cache map[string]ast.ASTNode
	mu    sync.RWMutex
}

var (
	globalTranspiler *PythonModuleTranspiler
	transpilerOnce   sync.Once
)

// GetPythonTranspiler returns the global transpiler instance
func GetPythonTranspiler() *PythonModuleTranspiler {
	transpilerOnce.Do(func() {
		globalTranspiler = &PythonModuleTranspiler{
			cache: make(map[string]ast.ASTNode),
		}
	})
	return globalTranspiler
}

// Transpile parses a Python file and returns its AST as a block
func (t *PythonModuleTranspiler) Transpile(pyPath string) (ast.ASTNode, error) {
	// Check cache first
	t.mu.RLock()
	if cached, ok := t.cache[pyPath]; ok {
		t.mu.RUnlock()
		return cached, nil
	}
	t.mu.RUnlock()

	// Read the Python source file
	source, err := os.ReadFile(pyPath)
	if err != nil {
		return nil, fmt.Errorf("failed to read Python file '%s': %w", pyPath, err)
	}

	// Tokenize Python source
	tokenizer := parser.NewPythonTokenizer(string(source))
	tokens, err := tokenizer.Tokenize()
	if err != nil {
		return nil, fmt.Errorf("failed to tokenize Python module '%s': %w", pyPath, err)
	}

	// Parse tokens into AST
	pythonParser := parser.NewPythonParser(tokens)
	nodes, err := pythonParser.Parse()
	if err != nil {
		return nil, fmt.Errorf("failed to parse Python module '%s': %w", pyPath, err)
	}

	// Wrap multiple statements in a BlockForm (do block)
	var astNode ast.ASTNode
	if len(nodes) == 0 {
		// Empty module
		astNode = &ast.BlockForm{Statements: []ast.ASTNode{}}
	} else if len(nodes) == 1 {
		astNode = nodes[0]
	} else {
		astNode = &ast.BlockForm{Statements: nodes}
	}

	// Cache the result
	t.mu.Lock()
	t.cache[pyPath] = astNode
	t.mu.Unlock()

	return astNode, nil
}

// ClearCache clears the transpiler cache
func (t *PythonModuleTranspiler) ClearCache() {
	t.mu.Lock()
	defer t.mu.Unlock()
	t.cache = make(map[string]ast.ASTNode)
}

// RemoveFromCache removes a specific file from the cache
func (t *PythonModuleTranspiler) RemoveFromCache(pyPath string) {
	t.mu.Lock()
	defer t.mu.Unlock()
	delete(t.cache, pyPath)
}
