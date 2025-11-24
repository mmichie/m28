package suggestions

import (
	"reflect"
	"testing"
)

func TestFindSimilarNames(t *testing.T) {
	tests := []struct {
		name        string
		target      string
		candidates  []string
		maxDistance int
		maxResults  int
		expected    []string
	}{
		{
			name:        "exact match exists",
			target:      "variable",
			candidates:  []string{"variable", "varable", "var"},
			maxDistance: 2,
			maxResults:  3,
			expected:    []string{"variable", "varable"}, // variable=0, varable=1, var=5
		},
		{
			name:        "close typo",
			target:      "varaible",
			candidates:  []string{"variable", "valuable", "var"},
			maxDistance: 2,
			maxResults:  3,
			expected:    []string{"variable"}, // distance 2
		},
		{
			name:        "multiple similar names",
			target:      "prit",
			candidates:  []string{"print", "printer", "sprint", "printf"},
			maxDistance: 2,
			maxResults:  3,
			expected:    []string{"print", "printf", "sprint"}, // print=1, printf=2, sprint=2
		},
		{
			name:        "no matches within distance",
			target:      "foo",
			candidates:  []string{"variable", "function", "class"},
			maxDistance: 2,
			maxResults:  3,
			expected:    nil,
		},
		{
			name:        "empty candidates",
			target:      "foo",
			candidates:  []string{},
			maxDistance: 2,
			maxResults:  3,
			expected:    nil,
		},
		{
			name:        "limit results",
			target:      "test",
			candidates:  []string{"test", "tests", "tester", "testing"},
			maxDistance: 2,
			maxResults:  2,
			expected:    []string{"test", "tests"}, // only first 2
		},
		{
			name:        "sort by distance",
			target:      "len",
			candidates:  []string{"length", "lean", "lens"},
			maxDistance: 2,
			maxResults:  3,
			expected:    []string{"lean", "lens"}, // distance 1 each, alphabetical
		},
		{
			name:        "common python variable typo",
			target:      "coutner",
			candidates:  []string{"counter", "count", "country"},
			maxDistance: 2,
			maxResults:  3,
			expected:    []string{"counter"}, // distance 2 (missing n, wrong position)
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := FindSimilarNames(tt.target, tt.candidates, tt.maxDistance, tt.maxResults)
			if !reflect.DeepEqual(result, tt.expected) {
				t.Errorf("FindSimilarNames(%q, %v, %d, %d) = %v, want %v",
					tt.target, tt.candidates, tt.maxDistance, tt.maxResults, result, tt.expected)
			}
		})
	}
}

func TestFindSimilarNamesCaseInsensitive(t *testing.T) {
	tests := []struct {
		name        string
		target      string
		candidates  []string
		maxDistance int
		maxResults  int
		expected    []string
	}{
		{
			name:        "case insensitive match",
			target:      "VARIABLE",
			candidates:  []string{"variable", "Variable", "VARABLE"},
			maxDistance: 2,
			maxResults:  3,
			expected:    []string{"Variable", "VARABLE"}, // both match case-insensitively, Variable closer alphabetically
		},
		{
			name:        "mixed case typo",
			target:      "Prit",
			candidates:  []string{"print", "PRINT", "Print"},
			maxDistance: 2,
			maxResults:  1,
			expected:    []string{"Print"}, // exact match wins
		},
		{
			name:        "preserve original case in results",
			target:      "foo",
			candidates:  []string{"FOO", "Foo", "fOo"},
			maxDistance: 0,
			maxResults:  1,
			expected:    []string{"fOo"}, // exact match (case-insensitive returns first alphabetically)
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := FindSimilarNamesCaseInsensitive(tt.target, tt.candidates, tt.maxDistance, tt.maxResults)
			if !reflect.DeepEqual(result, tt.expected) {
				t.Errorf("FindSimilarNamesCaseInsensitive(%q, %v, %d, %d) = %v, want %v",
					tt.target, tt.candidates, tt.maxDistance, tt.maxResults, result, tt.expected)
			}
		})
	}
}

func TestFormatSuggestion(t *testing.T) {
	tests := []struct {
		name        string
		suggestions []string
		expected    string
	}{
		{
			name:        "no suggestions",
			suggestions: []string{},
			expected:    "",
		},
		{
			name:        "single suggestion",
			suggestions: []string{"print"},
			expected:    "Did you mean 'print'?",
		},
		{
			name:        "two suggestions",
			suggestions: []string{"print", "printf"},
			expected:    "Did you mean 'print' or 'printf'?",
		},
		{
			name:        "three suggestions",
			suggestions: []string{"print", "printf", "sprint"},
			expected:    "Did you mean 'print', 'printf', or 'sprint'?",
		},
		{
			name:        "four suggestions",
			suggestions: []string{"a", "b", "c", "d"},
			expected:    "Did you mean 'a', 'b', 'c', or 'd'?",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := FormatSuggestion(tt.suggestions)
			if result != tt.expected {
				t.Errorf("FormatSuggestion(%v) = %q, want %q",
					tt.suggestions, result, tt.expected)
			}
		})
	}
}
