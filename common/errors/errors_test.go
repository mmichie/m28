package errors_test

import (
	"errors"
	"fmt"
	"strings"
	"testing"

	m28errors "github.com/mmichie/m28/common/errors"
)

func TestM28Error_Error(t *testing.T) {
	tests := []struct {
		name     string
		err      *m28errors.M28Error
		expected string
	}{
		{
			name: "error with function",
			err: &m28errors.M28Error{
				Type:     m28errors.TypeError,
				Function: "abs",
				Message:  "expected number, got string",
			},
			expected: "TypeError: abs: expected number, got string",
		},
		{
			name: "error without function",
			err: &m28errors.M28Error{
				Type:    m28errors.ValueError,
				Message: "invalid value",
			},
			expected: "ValueError: invalid value",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := tt.err.Error(); got != tt.expected {
				t.Errorf("Error() = %v, want %v", got, tt.expected)
			}
		})
	}
}

func TestM28Error_Is(t *testing.T) {
	typeErr1 := &m28errors.M28Error{Type: m28errors.TypeError, Message: "test"}
	typeErr2 := &m28errors.M28Error{Type: m28errors.TypeError, Message: "different"}
	valueErr := &m28errors.M28Error{Type: m28errors.ValueError, Message: "test"}

	tests := []struct {
		name   string
		err    *m28errors.M28Error
		target error
		want   bool
	}{
		{
			name:   "same error type",
			err:    typeErr1,
			target: typeErr2,
			want:   true,
		},
		{
			name:   "different error type",
			err:    typeErr1,
			target: valueErr,
			want:   false,
		},
		{
			name:   "nil target",
			err:    typeErr1,
			target: nil,
			want:   false,
		},
		{
			name:   "non-M28Error target",
			err:    typeErr1,
			target: fmt.Errorf("regular error"),
			want:   false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := tt.err.Is(tt.target); got != tt.want {
				t.Errorf("Is() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestM28Error_WithDetail(t *testing.T) {
	err := &m28errors.M28Error{
		Type:    m28errors.TypeError,
		Message: "test error",
	}

	// Add details
	err.WithDetail("expected", "number").WithDetail("got", "string")

	if err.Details == nil {
		t.Fatal("Details should not be nil")
	}

	if err.Details["expected"] != "number" {
		t.Errorf("expected detail = %v, want 'number'", err.Details["expected"])
	}

	if err.Details["got"] != "string" {
		t.Errorf("got detail = %v, want 'string'", err.Details["got"])
	}
}

func TestNewTypeError(t *testing.T) {
	err := m28errors.NewTypeError("len", "sequence", "number")

	if err.Type != m28errors.TypeError {
		t.Errorf("Type = %v, want TypeError", err.Type)
	}

	if err.Function != "len" {
		t.Errorf("Function = %v, want 'len'", err.Function)
	}

	expected := "TypeError: len: expected sequence, got number"
	if err.Error() != expected {
		t.Errorf("Error() = %v, want %v", err.Error(), expected)
	}
}

func TestNewTypeErrorf(t *testing.T) {
	err := m28errors.NewTypeErrorf("foo", "unsupported operand type(s) for +: '%s' and '%s'", "int", "str")

	expected := "TypeError: foo: unsupported operand type(s) for +: 'int' and 'str'"
	if err.Error() != expected {
		t.Errorf("Error() = %v, want %v", err.Error(), expected)
	}
}

func TestNewArgumentError(t *testing.T) {
	tests := []struct {
		name     string
		function string
		expected int
		got      int
		want     string
	}{
		{
			name:     "single argument",
			function: "abs",
			expected: 1,
			got:      3,
			want:     "ArgumentError: abs: takes exactly 1 argument (3 given)",
		},
		{
			name:     "multiple arguments",
			function: "pow",
			expected: 2,
			got:      1,
			want:     "ArgumentError: pow: takes exactly 2 arguments (1 given)",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			err := m28errors.NewArgumentError(tt.function, tt.expected, tt.got)
			if err.Error() != tt.want {
				t.Errorf("Error() = %v, want %v", err.Error(), tt.want)
			}

			// Check details
			if err.Details["expected"] != tt.expected {
				t.Errorf("expected detail = %v, want %v", err.Details["expected"], tt.expected)
			}
			if err.Details["got"] != tt.got {
				t.Errorf("got detail = %v, want %v", err.Details["got"], tt.got)
			}
		})
	}
}

func TestNewArgumentRangeError(t *testing.T) {
	tests := []struct {
		name string
		min  int
		max  int
		got  int
		want string
	}{
		{
			name: "range",
			min:  1,
			max:  3,
			got:  5,
			want: "ArgumentError: range: takes 1 to 3 arguments (5 given)",
		},
		{
			name: "exact (falls back to NewArgumentError)",
			min:  2,
			max:  2,
			got:  1,
			want: "ArgumentError: range: takes exactly 2 arguments (1 given)",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			err := m28errors.NewArgumentRangeError("range", tt.min, tt.max, tt.got)
			if err.Error() != tt.want {
				t.Errorf("Error() = %v, want %v", err.Error(), tt.want)
			}
		})
	}
}

func TestNewAttributeError(t *testing.T) {
	err := m28errors.NewAttributeError("list", "foo")

	expected := "AttributeError: 'list' object has no attribute 'foo'"
	if err.Error() != expected {
		t.Errorf("Error() = %v, want %v", err.Error(), expected)
	}

	if err.Details["type"] != "list" {
		t.Errorf("type detail = %v, want 'list'", err.Details["type"])
	}

	if err.Details["attribute"] != "foo" {
		t.Errorf("attribute detail = %v, want 'foo'", err.Details["attribute"])
	}
}

func TestNewKeyError(t *testing.T) {
	err := m28errors.NewKeyError("missing_key")

	expected := "KeyError: key not found: missing_key"
	if err.Error() != expected {
		t.Errorf("Error() = %v, want %v", err.Error(), expected)
	}
}

func TestNewIndexError(t *testing.T) {
	err := m28errors.NewIndexError(10, 5)

	expected := "IndexError: index 10 out of range (length 5)"
	if err.Error() != expected {
		t.Errorf("Error() = %v, want %v", err.Error(), expected)
	}
}

func TestNewZeroDivisionError(t *testing.T) {
	err := m28errors.NewZeroDivisionError("divide")

	expected := "ZeroDivisionError: divide: division by zero"
	if err.Error() != expected {
		t.Errorf("Error() = %v, want %v", err.Error(), expected)
	}
}

func TestWrap(t *testing.T) {
	tests := []struct {
		name     string
		err      error
		errType  m28errors.ErrorType
		function string
		want     string
	}{
		{
			name:     "wrap standard error",
			err:      fmt.Errorf("something went wrong"),
			errType:  m28errors.RuntimeError,
			function: "process",
			want:     "RuntimeError: process: something went wrong",
		},
		{
			name:     "wrap nil error",
			err:      nil,
			errType:  m28errors.RuntimeError,
			function: "process",
			want:     "",
		},
		{
			name: "wrap M28Error preserves type",
			err: &m28errors.M28Error{
				Type:    m28errors.TypeError,
				Message: "original error",
			},
			errType:  m28errors.RuntimeError, // This should be ignored
			function: "wrapper",
			want:     "TypeError: wrapper: original error",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			wrapped := m28errors.Wrap(tt.err, tt.errType, tt.function)

			if tt.err == nil {
				if wrapped != nil {
					t.Errorf("Wrap() = %v, want nil", wrapped)
				}
				return
			}

			if wrapped.Error() != tt.want {
				t.Errorf("Error() = %v, want %v", wrapped.Error(), tt.want)
			}
		})
	}
}

// TestErrorsIsCompatibility ensures our errors work with errors.Is
func TestErrorsIsCompatibility(t *testing.T) {
	typeErr := &m28errors.M28Error{Type: m28errors.TypeError}
	sameType := &m28errors.M28Error{Type: m28errors.TypeError}
	diffType := &m28errors.M28Error{Type: m28errors.ValueError}

	if !errors.Is(typeErr, sameType) {
		t.Error("errors.Is should return true for same error type")
	}

	if errors.Is(typeErr, diffType) {
		t.Error("errors.Is should return false for different error type")
	}
}

// TestErrorFormatting verifies consistent error message formatting
func TestErrorFormatting(t *testing.T) {
	tests := []struct {
		name    string
		creator func() *m28errors.M28Error
		checks  []string // Substrings that should be in the error
	}{
		{
			name:    "TypeError format",
			creator: func() *m28errors.M28Error { return m28errors.NewTypeError("len", "sequence", "number") },
			checks:  []string{"TypeError:", "len:", "expected sequence", "got number"},
		},
		{
			name:    "ArgumentError singular",
			creator: func() *m28errors.M28Error { return m28errors.NewArgumentError("abs", 1, 3) },
			checks:  []string{"ArgumentError:", "abs:", "takes exactly 1 argument", "(3 given)"},
		},
		{
			name:    "ArgumentError plural",
			creator: func() *m28errors.M28Error { return m28errors.NewArgumentError("pow", 2, 1) },
			checks:  []string{"ArgumentError:", "pow:", "takes exactly 2 arguments", "(1 given)"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			err := tt.creator()
			errStr := err.Error()

			for _, check := range tt.checks {
				if !strings.Contains(errStr, check) {
					t.Errorf("Error message %q should contain %q", errStr, check)
				}
			}
		})
	}
}
