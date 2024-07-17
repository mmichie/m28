package m28

import (
	"strings"
	"testing"
)

func TestInterpreter(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected string
		hasError bool
	}{
		{"Addition", "(+ 1 2)", "3", false},
		{"Subtraction", "(- 5 3)", "2", false},
		{"Multiplication", "(* 2 3)", "6", false},
		{"Division", "(/ 6 2)", "3", false},
		{"Nested Arithmetic", "(+ (* 2 3) (- 10 5))", "11", false},
		{"Define Variable", "(define x 5)", "x", false},
		{"Access Defined Variable", "x", "5", false},
		{"Lambda Function", "((lambda (x) (* x x)) 4)", "16", false},
		{"Conditional", "(if (> 5 3) 'yes 'no)", "yes", false},
		{"Define Function", `
			(define safe-divide 
			  (lambda (x y)
			    (if (= y 0)
			        'division-by-zero-error
			        (/ x y))))
		`, "safe-divide", false},
		{"Call Defined Function", "(safe-divide 10 2)", "5", false},
		{"Safe Division by Zero", "(safe-divide 1 0)", "division-by-zero-error", false},
	}

	interpreter := New()

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := interpreter.Execute(tt.input)

			if tt.hasError {
				if err == nil {
					t.Errorf("Expected an error, but got none")
				}
			} else {
				if err != nil {
					t.Errorf("Unexpected error: %v", err)
				} else {
					if strings.TrimSpace(result) != tt.expected {
						t.Errorf("Expected %s, but got %s", tt.expected, result)
					}
				}
			}
		})
	}
}
