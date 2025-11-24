package suggestions

import "testing"

func TestLevenshteinDistance(t *testing.T) {
	tests := []struct {
		name     string
		s1       string
		s2       string
		expected int
	}{
		{
			name:     "identical strings",
			s1:       "hello",
			s2:       "hello",
			expected: 0,
		},
		{
			name:     "empty strings",
			s1:       "",
			s2:       "",
			expected: 0,
		},
		{
			name:     "one empty string",
			s1:       "hello",
			s2:       "",
			expected: 5,
		},
		{
			name:     "single character difference",
			s1:       "hello",
			s2:       "hallo",
			expected: 1,
		},
		{
			name:     "insertion",
			s1:       "hello",
			s2:       "hellos",
			expected: 1,
		},
		{
			name:     "deletion",
			s1:       "hello",
			s2:       "helo",
			expected: 1,
		},
		{
			name:     "multiple changes",
			s1:       "kitten",
			s2:       "sitting",
			expected: 3,
		},
		{
			name:     "completely different",
			s1:       "abc",
			s2:       "xyz",
			expected: 3,
		},
		{
			name:     "case sensitivity",
			s1:       "Hello",
			s2:       "hello",
			expected: 1,
		},
		{
			name:     "variable name typo",
			s1:       "my_variable",
			s2:       "my_varaible",
			expected: 2, // swap i and a
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := LevenshteinDistance(tt.s1, tt.s2)
			if result != tt.expected {
				t.Errorf("LevenshteinDistance(%q, %q) = %d, want %d",
					tt.s1, tt.s2, result, tt.expected)
			}
		})
	}
}
