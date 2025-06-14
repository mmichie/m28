package types

import (
	"strings"
	"testing"

	"github.com/mmichie/m28/core"
)

func TestAsNumber(t *testing.T) {
	tests := []struct {
		name    string
		input   core.Value
		wantNum float64
		wantOk  bool
	}{
		{"number value", core.NumberValue(42.5), 42.5, true},
		{"integer number", core.NumberValue(10), 10.0, true},
		{"string value", core.StringValue("hello"), 0, false},
		{"nil value", core.Nil, 0, false},
		{"list value", core.ListValue([]core.Value{}), 0, false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gotNum, gotOk := AsNumber(tt.input)
			if gotNum != tt.wantNum || gotOk != tt.wantOk {
				t.Errorf("AsNumber(%v) = %v, %v; want %v, %v",
					tt.input, gotNum, gotOk, tt.wantNum, tt.wantOk)
			}
		})
	}
}

func TestAsString(t *testing.T) {
	tests := []struct {
		name    string
		input   core.Value
		wantStr string
		wantOk  bool
	}{
		{"string value", core.StringValue("hello"), "hello", true},
		{"empty string", core.StringValue(""), "", true},
		{"number value", core.NumberValue(42), "", false},
		{"nil value", core.Nil, "", false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gotStr, gotOk := AsString(tt.input)
			if gotStr != tt.wantStr || gotOk != tt.wantOk {
				t.Errorf("AsString(%v) = %v, %v; want %v, %v",
					tt.input, gotStr, gotOk, tt.wantStr, tt.wantOk)
			}
		})
	}
}

func TestAsBool(t *testing.T) {
	tests := []struct {
		name   string
		input  core.Value
		wantB  bool
		wantOk bool
	}{
		{"true value", core.BoolValue(true), true, true},
		{"false value", core.BoolValue(false), false, true},
		{"number value", core.NumberValue(1), false, false},
		{"string value", core.StringValue("true"), false, false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gotB, gotOk := AsBool(tt.input)
			if gotB != tt.wantB || gotOk != tt.wantOk {
				t.Errorf("AsBool(%v) = %v, %v; want %v, %v",
					tt.input, gotB, gotOk, tt.wantB, tt.wantOk)
			}
		})
	}
}

func TestAsList(t *testing.T) {
	list := core.ListValue([]core.Value{core.NumberValue(1), core.NumberValue(2)})

	tests := []struct {
		name   string
		input  core.Value
		wantOk bool
	}{
		{"list value", list, true},
		{"empty list", core.ListValue([]core.Value{}), true},
		{"tuple value", core.TupleValue([]core.Value{}), false},
		{"string value", core.StringValue("list"), false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gotList, gotOk := AsList(tt.input)
			if gotOk != tt.wantOk {
				t.Errorf("AsList(%v) ok = %v; want %v", tt.input, gotOk, tt.wantOk)
			}
			if gotOk && tt.name == "list value" {
				if len(gotList) != 2 {
					t.Errorf("AsList(%v) returned list with wrong length", tt.input)
				}
			}
		})
	}
}

func TestAsDict(t *testing.T) {
	dict := core.NewDict()

	tests := []struct {
		name   string
		input  core.Value
		wantOk bool
	}{
		{"dict value", dict, true},
		{"list value", core.ListValue([]core.Value{}), false},
		{"string value", core.StringValue("dict"), false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			_, gotOk := AsDict(tt.input)
			if gotOk != tt.wantOk {
				t.Errorf("AsDict(%v) ok = %v; want %v", tt.input, gotOk, tt.wantOk)
			}
		})
	}
}

func TestRequireNumber(t *testing.T) {
	tests := []struct {
		name    string
		input   core.Value
		context string
		wantNum float64
		wantErr bool
		errMsg  string
	}{
		{
			name:    "valid number",
			input:   core.NumberValue(42.5),
			context: "test",
			wantNum: 42.5,
			wantErr: false,
		},
		{
			name:    "string instead of number",
			input:   core.StringValue("hello"),
			context: "add",
			wantErr: true,
			errMsg:  "add: expected number, got string",
		},
		{
			name:    "nil instead of number",
			input:   core.Nil,
			context: "multiply",
			wantErr: true,
			errMsg:  "multiply: expected number, got nil",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gotNum, err := RequireNumber(tt.input, tt.context)
			if (err != nil) != tt.wantErr {
				t.Errorf("RequireNumber(%v, %q) error = %v, wantErr %v",
					tt.input, tt.context, err, tt.wantErr)
				return
			}
			if err != nil && !strings.Contains(err.Error(), tt.errMsg) {
				t.Errorf("RequireNumber(%v, %q) error = %v, want error containing %q",
					tt.input, tt.context, err, tt.errMsg)
			}
			if !tt.wantErr && gotNum != tt.wantNum {
				t.Errorf("RequireNumber(%v, %q) = %v, want %v",
					tt.input, tt.context, gotNum, tt.wantNum)
			}
		})
	}
}

func TestRequireString(t *testing.T) {
	tests := []struct {
		name    string
		input   core.Value
		context string
		wantStr string
		wantErr bool
		errMsg  string
	}{
		{
			name:    "valid string",
			input:   core.StringValue("hello"),
			context: "concat",
			wantStr: "hello",
			wantErr: false,
		},
		{
			name:    "number instead of string",
			input:   core.NumberValue(42),
			context: "upper",
			wantErr: true,
			errMsg:  "upper: expected string, got number",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gotStr, err := RequireString(tt.input, tt.context)
			if (err != nil) != tt.wantErr {
				t.Errorf("RequireString(%v, %q) error = %v, wantErr %v",
					tt.input, tt.context, err, tt.wantErr)
				return
			}
			if err != nil && !strings.Contains(err.Error(), tt.errMsg) {
				t.Errorf("RequireString(%v, %q) error = %v, want error containing %q",
					tt.input, tt.context, err, tt.errMsg)
			}
			if !tt.wantErr && gotStr != tt.wantStr {
				t.Errorf("RequireString(%v, %q) = %v, want %v",
					tt.input, tt.context, gotStr, tt.wantStr)
			}
		})
	}
}

func TestRequireList(t *testing.T) {
	list := core.ListValue([]core.Value{core.NumberValue(1)})

	tests := []struct {
		name    string
		input   core.Value
		context string
		wantErr bool
		errMsg  string
	}{
		{
			name:    "valid list",
			input:   list,
			context: "append",
			wantErr: false,
		},
		{
			name:    "tuple instead of list",
			input:   core.TupleValue([]core.Value{}),
			context: "extend",
			wantErr: true,
			errMsg:  "extend: expected list, got tuple",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			_, err := RequireList(tt.input, tt.context)
			if (err != nil) != tt.wantErr {
				t.Errorf("RequireList(%v, %q) error = %v, wantErr %v",
					tt.input, tt.context, err, tt.wantErr)
				return
			}
			if err != nil && !strings.Contains(err.Error(), tt.errMsg) {
				t.Errorf("RequireList(%v, %q) error = %v, want error containing %q",
					tt.input, tt.context, err, tt.errMsg)
			}
		})
	}
}

func TestAsIterable(t *testing.T) {
	// Create test values
	list := core.ListValue([]core.Value{})
	tuple := core.TupleValue([]core.Value{})
	str := core.StringValue("hello")
	dict := core.NewDict()
	set := core.NewSet()
	rng := &core.RangeValue{Start: 0, Stop: 10, Step: 1}

	tests := []struct {
		name   string
		input  core.Value
		wantOk bool
	}{
		{"list is iterable", list, true},
		{"tuple is iterable", tuple, true},
		{"string is iterable", str, false}, // StringValue doesn't implement Iterable directly
		{"dict is iterable", dict, false},  // DictValue doesn't implement Iterable directly
		{"set is iterable", set, true},
		{"range is iterable", rng, true},
		{"number not iterable", core.NumberValue(42), false},
		{"nil not iterable", core.Nil, false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			_, gotOk := AsIterable(tt.input)
			if gotOk != tt.wantOk {
				t.Errorf("AsIterable(%v) ok = %v; want %v", tt.input, gotOk, tt.wantOk)
			}
		})
	}
}

func TestRequireIterable(t *testing.T) {
	tests := []struct {
		name    string
		input   core.Value
		context string
		wantErr bool
		errMsg  string
	}{
		{
			name:    "valid iterable",
			input:   core.ListValue([]core.Value{}),
			context: "map",
			wantErr: false,
		},
		{
			name:    "number not iterable",
			input:   core.NumberValue(42),
			context: "for loop",
			wantErr: true,
			errMsg:  "for loop: expected iterable, got number",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			_, err := RequireIterable(tt.input, tt.context)
			if (err != nil) != tt.wantErr {
				t.Errorf("RequireIterable(%v, %q) error = %v, wantErr %v",
					tt.input, tt.context, err, tt.wantErr)
				return
			}
			if err != nil && !strings.Contains(err.Error(), tt.errMsg) {
				t.Errorf("RequireIterable(%v, %q) error = %v, want error containing %q",
					tt.input, tt.context, err, tt.errMsg)
			}
		})
	}
}
