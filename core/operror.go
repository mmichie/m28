package core

import "fmt"

// NewBinaryOpError builds the CPython-exact TypeError for an unsupported binary
// arithmetic/bitwise operation, e.g.
//
//	unsupported operand type(s) for +: 'int' and 'str'
//
// It uses the operands' Python type names (int/str/NoneType, not the internal
// number/string/nil) and returns a *TypeError so the top-level display renders
// it as "TypeError: ...", not wrapped as an EvalError.
func NewBinaryOpError(op string, left, right Value) *TypeError {
	// CPython spells the ** error "for ** or pow():".
	if op == "**" {
		op = "** or pow()"
	}
	return &TypeError{Message: fmt.Sprintf(
		"unsupported operand type(s) for %s: '%s' and '%s'",
		op, GetPythonTypeName(left), GetPythonTypeName(right))}
}

// NewComparisonError builds the CPython-exact TypeError for an unsupported
// ordering comparison, e.g.
//
//	'<' not supported between instances of 'int' and 'str'
func NewComparisonError(op string, left, right Value) *TypeError {
	return &TypeError{Message: fmt.Sprintf(
		"'%s' not supported between instances of '%s' and '%s'",
		op, GetPythonTypeName(left), GetPythonTypeName(right))}
}

// NewConcatError builds the CPython-exact TypeError for concatenating a wrong
// type onto a sequence, e.g.
//
//	can only concatenate str (not "int") to str
func NewConcatError(seqType string, right Value) *TypeError {
	// bytes/bytearray use a different CPython template than str/list.
	if seqType == "bytes" || seqType == "bytearray" {
		return &TypeError{Message: fmt.Sprintf(
			"can't concat %s to %s", GetPythonTypeName(right), seqType)}
	}
	return &TypeError{Message: fmt.Sprintf(
		"can only concatenate %s (not \"%s\") to %s",
		seqType, GetPythonTypeName(right), seqType)}
}
