package integration_tests

import (
	"testing"

	"github.com/mmichie/m28/builtin"
	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/eval"
	"github.com/mmichie/m28/parser"
	"github.com/mmichie/m28/special_forms"
)

var globalCtx *core.Context

func init() {
	// Register all special forms (only needs to be done once)
	special_forms.RegisterAllForms()

	// Create a global context with all builtins registered
	globalCtx = core.NewContext(nil)
	builtin.RegisterAllBuiltins(globalCtx)
}

// TestPythonToIRIntegration tests the full pipeline: Python source → Tokenizer → Parser → IR → Evaluator
func TestPythonToIRIntegration(t *testing.T) {
	tests := []struct {
		name     string
		source   string
		expected string
	}{
		{
			name:     "Simple assignment",
			source:   "x = 42",
			expected: "42",
		},
		{
			name:     "Simple function",
			source:   "def add(a, b):\n    return a + b",
			expected: "<function add(a b)>",
		},
		{
			name:     "Function call",
			source:   "def double(x):\n    return x * 2\n\nresult = double(21)",
			expected: "42",
		},
		{
			name:     "If statement",
			source:   "x = 10\nif x > 5:\n    result = \"big\"\nelse:\n    result = \"small\"",
			expected: "\"big\"",
		},
		{
			name:     "List comprehension",
			source:   "result = [x * x for x in range(5)]",
			expected: "[0, 1, 4, 9, 16]",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Tokenize
			tokenizer := parser.NewPythonTokenizer(tt.source)
			tokens, err := tokenizer.Tokenize()
			if err != nil {
				t.Fatalf("Tokenization error: %v", err)
			}

			// Parse
			pythonParser := parser.NewPythonParser(tokens, "<test>", tt.source)
			nodes, err := pythonParser.Parse()
			if err != nil {
				t.Fatalf("Parse error: %v", err)
			}

			// Lower to IR and evaluate
			ctx := core.NewContext(globalCtx)
			var result core.Value

			for _, node := range nodes {
				ir := node.ToIR()
				result, err = eval.Eval(ir, ctx)
				if err != nil {
					t.Fatalf("Evaluation error: %v", err)
				}
			}

			// Check result
			if tt.name == "Simple assignment" {
				// For assignment, check the variable value
				result, _ = ctx.Lookup("x")
			} else if tt.name == "Function call" || tt.name == "If statement" || tt.name == "List comprehension" {
				// For these, check the 'result' variable
				result, _ = ctx.Lookup("result")
			}

			if result == nil {
				t.Fatalf("Result is nil")
			}

			resultStr := result.String()
			if resultStr != tt.expected {
				t.Errorf("Expected %q, got %q", tt.expected, resultStr)
			}
		})
	}
}

// TestPythonForLoop tests for loop execution
func TestPythonForLoop(t *testing.T) {
	source := `total = 0
for i in range(5):
    total = total + i`

	// Tokenize
	tokenizer := parser.NewPythonTokenizer(source)
	tokens, err := tokenizer.Tokenize()
	if err != nil {
		t.Fatalf("Tokenization error: %v", err)
	}

	// Parse
	pythonParser := parser.NewPythonParser(tokens, "<test>", source)
	nodes, err := pythonParser.Parse()
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	// Evaluate
	ctx := core.NewContext(globalCtx)
	for _, node := range nodes {
		ir := node.ToIR()
		_, err = eval.Eval(ir, ctx)
		if err != nil {
			t.Fatalf("Evaluation error: %v", err)
		}
	}

	// Check result
	result, err := ctx.Lookup("total")
	if err != nil {
		t.Fatal("Variable 'total' not found")
	}

	if result.String() != "10" {
		t.Errorf("Expected total to be 10, got %v", result.String())
	}
}

// TestPythonClassDefinition tests basic class definition
func TestPythonClassDefinition(t *testing.T) {
	source := `class Counter:
    def __init__(self, start):
        self.value = start

    def increment(self):
        self.value = self.value + 1
        return self.value`

	// Tokenize
	tokenizer := parser.NewPythonTokenizer(source)
	tokens, err := tokenizer.Tokenize()
	if err != nil {
		t.Fatalf("Tokenization error: %v", err)
	}

	// Parse
	pythonParser := parser.NewPythonParser(tokens, "<test>", source)
	nodes, err := pythonParser.Parse()
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	if len(nodes) != 1 {
		t.Fatalf("Expected 1 node, got %d", len(nodes))
	}

	// Lower to IR
	ir := nodes[0].ToIR()

	if ir == nil {
		t.Fatal("ToIR() returned nil")
	}

	// Evaluate
	ctx := core.NewContext(globalCtx)
	_, err = eval.Eval(ir, ctx)
	if err != nil {
		t.Fatalf("Evaluation error: %v", err)
	}

	// Check that Counter class exists
	counterClass, err := ctx.Lookup("Counter")
	if err != nil {
		t.Fatal("Class 'Counter' not found")
	}

	if counterClass == nil {
		t.Fatal("Counter class is nil")
	}
}
