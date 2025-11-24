package modules

import (
	"fmt"
	"os"
	"sync"
	"time"

	"github.com/mmichie/m28/core"
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
	core.Log.Info(core.SubsystemParser, "Parsing file started", "file", pyPath)
	startTotal := time.Now()

	// Check cache first
	t.mu.RLock()
	if cached, ok := t.cache[pyPath]; ok {
		t.mu.RUnlock()
		core.Log.Debug(core.SubsystemParser, "File found in parse cache", "file", pyPath, "cache_status", "hit")
		return cached, nil
	}
	t.mu.RUnlock()

	core.Log.Debug(core.SubsystemParser, "File not in parse cache", "file", pyPath, "cache_status", "miss")

	// Read the Python source file
	startRead := time.Now()
	source, err := os.ReadFile(pyPath)
	if err != nil {
		core.Log.Error(core.SubsystemParser, "Failed to read file", "file", pyPath, "error", err)
		return nil, fmt.Errorf("failed to read Python file '%s': %w", pyPath, err)
	}
	readTime := time.Since(startRead)
	core.Log.Debug(core.SubsystemParser, "File read completed", "file", pyPath, "size_bytes", len(source), "read_time", readTime)

	// Tokenize Python source
	startTokenize := time.Now()
	tokenizer := parser.NewPythonTokenizer(string(source))
	tokens, err := tokenizer.Tokenize()
	if err != nil {
		core.Log.Error(core.SubsystemParser, "Tokenization failed", "file", pyPath, "error", err)
		return nil, fmt.Errorf("failed to tokenize Python module '%s': %w", pyPath, err)
	}
	tokenizeTime := time.Since(startTokenize)
	core.Log.Debug(core.SubsystemParser, "Tokenization completed", "file", pyPath, "token_count", len(tokens), "tokenize_time", tokenizeTime)

	// Parse tokens into AST
	startParse := time.Now()
	core.Log.Debug(core.SubsystemParser, "AST parsing started", "file", pyPath, "tokens", len(tokens))
	pythonParser := parser.NewPythonParser(tokens, pyPath, string(source))
	nodes, err := pythonParser.Parse()
	if err != nil {
		core.Log.Error(core.SubsystemParser, "AST parsing failed", "file", pyPath, "error", err)
		return nil, fmt.Errorf("failed to parse Python module '%s': %w", pyPath, err)
	}
	parseTime := time.Since(startParse)
	core.Log.Debug(core.SubsystemParser, "AST parsing completed", "file", pyPath, "node_count", len(nodes), "parse_time", parseTime)

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

	totalTime := time.Since(startTotal)

	core.Log.Info(core.SubsystemParser, "Parsing file completed successfully", "file", pyPath, "total_time", totalTime, "nodes", len(nodes), "tokens", len(tokens))

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
