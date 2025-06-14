package validation_test

import (
	"testing"

	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// Test helpers to create values
func num(n float64) core.Value              { return core.NumberValue(n) }
func str(s string) core.Value               { return core.StringValue(s) }
func boolean(b bool) core.Value             { return core.BoolValue(b) }
func list(values ...core.Value) core.Value  { return core.ListValue(values) }
func tuple(values ...core.Value) core.Value { return core.TupleValue(values) }

func TestArgs_Exact(t *testing.T) {
	tests := []struct {
		name     string
		args     []core.Value
		expected int
		wantErr  bool
		errMsg   string
	}{
		{
			name:     "exact match",
			args:     []core.Value{num(42)},
			expected: 1,
			wantErr:  false,
		},
		{
			name:     "too few",
			args:     []core.Value{},
			expected: 1,
			wantErr:  true,
			errMsg:   "ArgumentError: test: takes exactly 1 argument (0 given)",
		},
		{
			name:     "too many",
			args:     []core.Value{num(1), num(2), num(3)},
			expected: 2,
			wantErr:  true,
			errMsg:   "ArgumentError: test: takes exactly 2 arguments (3 given)",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			v := validation.NewArgs("test", tt.args)
			err := v.Exact(tt.expected)

			if (err != nil) != tt.wantErr {
				t.Errorf("Exact() error = %v, wantErr %v", err, tt.wantErr)
			}

			if err != nil && tt.errMsg != "" && err.Error() != tt.errMsg {
				t.Errorf("Exact() error = %v, want %v", err.Error(), tt.errMsg)
			}
		})
	}
}

func TestArgs_Range(t *testing.T) {
	tests := []struct {
		name    string
		args    []core.Value
		min     int
		max     int
		wantErr bool
		errMsg  string
	}{
		{
			name:    "within range",
			args:    []core.Value{num(1), num(2)},
			min:     1,
			max:     3,
			wantErr: false,
		},
		{
			name:    "at min",
			args:    []core.Value{num(1)},
			min:     1,
			max:     3,
			wantErr: false,
		},
		{
			name:    "at max",
			args:    []core.Value{num(1), num(2), num(3)},
			min:     1,
			max:     3,
			wantErr: false,
		},
		{
			name:    "below min",
			args:    []core.Value{},
			min:     1,
			max:     3,
			wantErr: true,
			errMsg:  "ArgumentError: test: takes 1 to 3 arguments (0 given)",
		},
		{
			name:    "above max",
			args:    []core.Value{num(1), num(2), num(3), num(4)},
			min:     1,
			max:     3,
			wantErr: true,
			errMsg:  "ArgumentError: test: takes 1 to 3 arguments (4 given)",
		},
		{
			name:    "exact range (delegates to Exact)",
			args:    []core.Value{num(1)},
			min:     2,
			max:     2,
			wantErr: true,
			errMsg:  "ArgumentError: test: takes exactly 2 arguments (1 given)",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			v := validation.NewArgs("test", tt.args)
			err := v.Range(tt.min, tt.max)

			if (err != nil) != tt.wantErr {
				t.Errorf("Range() error = %v, wantErr %v", err, tt.wantErr)
			}

			if err != nil && tt.errMsg != "" && err.Error() != tt.errMsg {
				t.Errorf("Range() error = %v, want %v", err.Error(), tt.errMsg)
			}
		})
	}
}

func TestArgs_Min(t *testing.T) {
	tests := []struct {
		name    string
		args    []core.Value
		min     int
		wantErr bool
		errMsg  string
	}{
		{
			name:    "enough args",
			args:    []core.Value{num(1), num(2)},
			min:     2,
			wantErr: false,
		},
		{
			name:    "more than enough",
			args:    []core.Value{num(1), num(2), num(3)},
			min:     2,
			wantErr: false,
		},
		{
			name:    "not enough",
			args:    []core.Value{num(1)},
			min:     2,
			wantErr: true,
			errMsg:  "ArgumentError: test: at least 2 arguments required",
		},
		{
			name:    "not enough singular",
			args:    []core.Value{},
			min:     1,
			wantErr: true,
			errMsg:  "ArgumentError: test: at least 1 argument required",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			v := validation.NewArgs("test", tt.args)
			err := v.Min(tt.min)

			if (err != nil) != tt.wantErr {
				t.Errorf("Min() error = %v, wantErr %v", err, tt.wantErr)
			}

			if err != nil && tt.errMsg != "" && err.Error() != tt.errMsg {
				t.Errorf("Min() error = %v, want %v", err.Error(), tt.errMsg)
			}
		})
	}
}

func TestArgs_GetNumber(t *testing.T) {
	tests := []struct {
		name    string
		args    []core.Value
		index   int
		want    float64
		wantErr bool
		errMsg  string
	}{
		{
			name:    "valid number",
			args:    []core.Value{num(42.5)},
			index:   0,
			want:    42.5,
			wantErr: false,
		},
		{
			name:    "not a number",
			args:    []core.Value{str("hello")},
			index:   0,
			want:    0,
			wantErr: true,
			errMsg:  "TypeError: test: expected number, got string",
		},
		{
			name:    "index out of bounds",
			args:    []core.Value{},
			index:   0,
			want:    0,
			wantErr: true,
			errMsg:  "ArgumentError: test: takes exactly 1 argument (0 given)",
		},
		{
			name:    "negative index",
			args:    []core.Value{num(1)},
			index:   -1,
			want:    0,
			wantErr: true,
			errMsg:  "ArgumentError: test: takes exactly 0 arguments (1 given)",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			v := validation.NewArgs("test", tt.args)
			got, err := v.GetNumber(tt.index)

			if (err != nil) != tt.wantErr {
				t.Errorf("GetNumber() error = %v, wantErr %v", err, tt.wantErr)
			}

			if got != tt.want {
				t.Errorf("GetNumber() = %v, want %v", got, tt.want)
			}

			if err != nil && tt.errMsg != "" && err.Error() != tt.errMsg {
				t.Errorf("GetNumber() error = %v, want %v", err.Error(), tt.errMsg)
			}
		})
	}
}

func TestArgs_GetInt(t *testing.T) {
	tests := []struct {
		name    string
		args    []core.Value
		index   int
		want    int
		wantErr bool
		errMsg  string
	}{
		{
			name:    "valid int",
			args:    []core.Value{num(42)},
			index:   0,
			want:    42,
			wantErr: false,
		},
		{
			name:    "float value",
			args:    []core.Value{num(42.5)},
			index:   0,
			want:    0,
			wantErr: true,
			errMsg:  "ValueError: test: argument 1 must be an integer",
		},
		{
			name:    "not a number",
			args:    []core.Value{str("hello")},
			index:   0,
			want:    0,
			wantErr: true,
			errMsg:  "TypeError: test: expected number, got string",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			v := validation.NewArgs("test", tt.args)
			got, err := v.GetInt(tt.index)

			if (err != nil) != tt.wantErr {
				t.Errorf("GetInt() error = %v, wantErr %v", err, tt.wantErr)
			}

			if got != tt.want {
				t.Errorf("GetInt() = %v, want %v", got, tt.want)
			}

			if err != nil && tt.errMsg != "" && err.Error() != tt.errMsg {
				t.Errorf("GetInt() error = %v, want %v", err.Error(), tt.errMsg)
			}
		})
	}
}

func TestArgs_GetString(t *testing.T) {
	tests := []struct {
		name    string
		args    []core.Value
		index   int
		want    string
		wantErr bool
		errMsg  string
	}{
		{
			name:    "valid string",
			args:    []core.Value{str("hello")},
			index:   0,
			want:    "hello",
			wantErr: false,
		},
		{
			name:    "not a string",
			args:    []core.Value{num(42)},
			index:   0,
			want:    "",
			wantErr: true,
			errMsg:  "TypeError: test: expected string, got number",
		},
		{
			name:    "empty string",
			args:    []core.Value{str("")},
			index:   0,
			want:    "",
			wantErr: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			v := validation.NewArgs("test", tt.args)
			got, err := v.GetString(tt.index)

			if (err != nil) != tt.wantErr {
				t.Errorf("GetString() error = %v, wantErr %v", err, tt.wantErr)
			}

			if got != tt.want {
				t.Errorf("GetString() = %v, want %v", got, tt.want)
			}

			if err != nil && tt.errMsg != "" && err.Error() != tt.errMsg {
				t.Errorf("GetString() error = %v, want %v", err.Error(), tt.errMsg)
			}
		})
	}
}

func TestArgs_GetBool(t *testing.T) {
	tests := []struct {
		name    string
		args    []core.Value
		index   int
		want    bool
		wantErr bool
		errMsg  string
	}{
		{
			name:    "true value",
			args:    []core.Value{boolean(true)},
			index:   0,
			want:    true,
			wantErr: false,
		},
		{
			name:    "false value",
			args:    []core.Value{boolean(false)},
			index:   0,
			want:    false,
			wantErr: false,
		},
		{
			name:    "not a bool",
			args:    []core.Value{num(1)},
			index:   0,
			want:    false,
			wantErr: true,
			errMsg:  "TypeError: test: expected bool, got number",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			v := validation.NewArgs("test", tt.args)
			got, err := v.GetBool(tt.index)

			if (err != nil) != tt.wantErr {
				t.Errorf("GetBool() error = %v, wantErr %v", err, tt.wantErr)
			}

			if got != tt.want {
				t.Errorf("GetBool() = %v, want %v", got, tt.want)
			}

			if err != nil && tt.errMsg != "" && err.Error() != tt.errMsg {
				t.Errorf("GetBool() error = %v, want %v", err.Error(), tt.errMsg)
			}
		})
	}
}

func TestArgs_GetList(t *testing.T) {
	testList := list(num(1), num(2), num(3))

	tests := []struct {
		name    string
		args    []core.Value
		index   int
		wantLen int
		wantErr bool
		errMsg  string
	}{
		{
			name:    "valid list",
			args:    []core.Value{testList},
			index:   0,
			wantLen: 3,
			wantErr: false,
		},
		{
			name:    "not a list",
			args:    []core.Value{tuple(num(1), num(2))},
			index:   0,
			wantLen: 0,
			wantErr: true,
			errMsg:  "TypeError: test: expected list, got tuple",
		},
		{
			name:    "empty list",
			args:    []core.Value{list()},
			index:   0,
			wantLen: 0,
			wantErr: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			v := validation.NewArgs("test", tt.args)
			got, err := v.GetList(tt.index)

			if (err != nil) != tt.wantErr {
				t.Errorf("GetList() error = %v, wantErr %v", err, tt.wantErr)
			}

			if !tt.wantErr && len(got) != tt.wantLen {
				t.Errorf("GetList() returned list of length %v, want %v", len(got), tt.wantLen)
			}

			if err != nil && tt.errMsg != "" && err.Error() != tt.errMsg {
				t.Errorf("GetList() error = %v, want %v", err.Error(), tt.errMsg)
			}
		})
	}
}

func TestArgs_GetSequence(t *testing.T) {
	tests := []struct {
		name    string
		args    []core.Value
		index   int
		wantErr bool
		errMsg  string
	}{
		{
			name:    "list is sequence",
			args:    []core.Value{list(num(1), num(2))},
			index:   0,
			wantErr: false,
		},
		{
			name:    "tuple is sequence",
			args:    []core.Value{tuple(num(1), num(2))},
			index:   0,
			wantErr: false,
		},
		{
			name:    "string is sequence",
			args:    []core.Value{str("hello")},
			index:   0,
			wantErr: false,
		},
		{
			name:    "number is not sequence",
			args:    []core.Value{num(42)},
			index:   0,
			wantErr: true,
			errMsg:  "TypeError: test: expected sequence, got number",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			v := validation.NewArgs("test", tt.args)
			_, err := v.GetSequence(tt.index)

			if (err != nil) != tt.wantErr {
				t.Errorf("GetSequence() error = %v, wantErr %v", err, tt.wantErr)
			}

			if err != nil && tt.errMsg != "" && err.Error() != tt.errMsg {
				t.Errorf("GetSequence() error = %v, want %v", err.Error(), tt.errMsg)
			}
		})
	}
}

func TestArgs_GetNumberOrDefault(t *testing.T) {
	tests := []struct {
		name       string
		args       []core.Value
		index      int
		defaultVal float64
		want       float64
		wantErr    bool
	}{
		{
			name:       "valid number",
			args:       []core.Value{num(42)},
			index:      0,
			defaultVal: 10,
			want:       42,
			wantErr:    false,
		},
		{
			name:       "out of bounds returns default",
			args:       []core.Value{},
			index:      0,
			defaultVal: 10,
			want:       10,
			wantErr:    false,
		},
		{
			name:       "not a number returns error",
			args:       []core.Value{str("hello")},
			index:      0,
			defaultVal: 10,
			want:       0,
			wantErr:    true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			v := validation.NewArgs("test", tt.args)
			got, err := v.GetNumberOrDefault(tt.index, tt.defaultVal)

			if (err != nil) != tt.wantErr {
				t.Errorf("GetNumberOrDefault() error = %v, wantErr %v", err, tt.wantErr)
			}

			if got != tt.want {
				t.Errorf("GetNumberOrDefault() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestArgs_ExtractNumbers(t *testing.T) {
	tests := []struct {
		name    string
		args    []core.Value
		want    []float64
		wantErr bool
		errMsg  string
	}{
		{
			name:    "all numbers",
			args:    []core.Value{num(1), num(2.5), num(3)},
			want:    []float64{1, 2.5, 3},
			wantErr: false,
		},
		{
			name:    "empty args",
			args:    []core.Value{},
			want:    []float64{},
			wantErr: false,
		},
		{
			name:    "contains non-number",
			args:    []core.Value{num(1), str("two"), num(3)},
			want:    nil,
			wantErr: true,
			errMsg:  "TypeError: test: all arguments must be numbers, argument 2 is string",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			v := validation.NewArgs("test", tt.args)
			got, err := v.ExtractNumbers()

			if (err != nil) != tt.wantErr {
				t.Errorf("ExtractNumbers() error = %v, wantErr %v", err, tt.wantErr)
			}

			if !tt.wantErr {
				if len(got) != len(tt.want) {
					t.Errorf("ExtractNumbers() returned %d numbers, want %d", len(got), len(tt.want))
				}
				for i, n := range got {
					if n != tt.want[i] {
						t.Errorf("ExtractNumbers()[%d] = %v, want %v", i, n, tt.want[i])
					}
				}
			}

			if err != nil && tt.errMsg != "" && err.Error() != tt.errMsg {
				t.Errorf("ExtractNumbers() error = %v, want %v", err.Error(), tt.errMsg)
			}
		})
	}
}

func TestArgs_ExtractStrings(t *testing.T) {
	tests := []struct {
		name    string
		args    []core.Value
		want    []string
		wantErr bool
		errMsg  string
	}{
		{
			name:    "all strings",
			args:    []core.Value{str("hello"), str("world")},
			want:    []string{"hello", "world"},
			wantErr: false,
		},
		{
			name:    "contains non-string",
			args:    []core.Value{str("hello"), num(42)},
			want:    nil,
			wantErr: true,
			errMsg:  "TypeError: test: all arguments must be strings, argument 2 is number",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			v := validation.NewArgs("test", tt.args)
			got, err := v.ExtractStrings()

			if (err != nil) != tt.wantErr {
				t.Errorf("ExtractStrings() error = %v, wantErr %v", err, tt.wantErr)
			}

			if !tt.wantErr {
				if len(got) != len(tt.want) {
					t.Errorf("ExtractStrings() returned %d strings, want %d", len(got), len(tt.want))
				}
				for i, s := range got {
					if s != tt.want[i] {
						t.Errorf("ExtractStrings()[%d] = %v, want %v", i, s, tt.want[i])
					}
				}
			}

			if err != nil && tt.errMsg != "" && err.Error() != tt.errMsg {
				t.Errorf("ExtractStrings() error = %v, want %v", err.Error(), tt.errMsg)
			}
		})
	}
}

func TestArgs_UtilityMethods(t *testing.T) {
	args := []core.Value{num(1), str("hello"), boolean(true)}
	v := validation.NewArgs("test", args)

	// Test Get
	if got := v.Get(0); got != args[0] {
		t.Errorf("Get(0) = %v, want %v", got, args[0])
	}

	if got := v.Get(10); got != nil {
		t.Errorf("Get(10) = %v, want nil", got)
	}

	if got := v.Get(-1); got != nil {
		t.Errorf("Get(-1) = %v, want nil", got)
	}

	// Test Count
	if got := v.Count(); got != 3 {
		t.Errorf("Count() = %v, want 3", got)
	}

	// Test All
	all := v.All()
	if len(all) != len(args) {
		t.Errorf("All() returned %d args, want %d", len(all), len(args))
	}
}

// TestRealWorldExample shows how the validation package simplifies real function implementations
func TestRealWorldExample(t *testing.T) {
	// Example: implementing abs() function
	absFunc := func(args []core.Value) (core.Value, error) {
		v := validation.NewArgs("abs", args)

		// Validate argument count
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		// Extract number
		n, err := v.GetNumber(0)
		if err != nil {
			return nil, err
		}

		// Implement logic
		if n < 0 {
			return num(-n), nil
		}
		return num(n), nil
	}

	// Test the function
	tests := []struct {
		name    string
		args    []core.Value
		want    float64
		wantErr bool
		errMsg  string
	}{
		{
			name:    "positive number",
			args:    []core.Value{num(42)},
			want:    42,
			wantErr: false,
		},
		{
			name:    "negative number",
			args:    []core.Value{num(-42)},
			want:    42,
			wantErr: false,
		},
		{
			name:    "wrong type",
			args:    []core.Value{str("42")},
			want:    0,
			wantErr: true,
			errMsg:  "TypeError: abs: expected number, got string",
		},
		{
			name:    "too many args",
			args:    []core.Value{num(42), num(10)},
			want:    0,
			wantErr: true,
			errMsg:  "ArgumentError: abs: takes exactly 1 argument (2 given)",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := absFunc(tt.args)

			if (err != nil) != tt.wantErr {
				t.Errorf("absFunc() error = %v, wantErr %v", err, tt.wantErr)
			}

			if err != nil {
				if tt.errMsg != "" && err.Error() != tt.errMsg {
					t.Errorf("absFunc() error = %v, want %v", err.Error(), tt.errMsg)
				}
			} else {
				if n, ok := got.(core.NumberValue); ok {
					if float64(n) != tt.want {
						t.Errorf("absFunc() = %v, want %v", float64(n), tt.want)
					}
				} else {
					t.Errorf("absFunc() returned non-number: %v", got)
				}
			}
		})
	}
}
