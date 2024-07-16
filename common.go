package m28

import (
	"fmt"
	"strings"
)

// IsTruthy determines if a value is considered true in Lisp
func IsTruthy(v LispValue) bool {
	switch v := v.(type) {
	case nil:
		return false
	case bool:
		return v
	case float64:
		return v != 0
	case string:
		return v != ""
	case LispList:
		return len(v) > 0
	default:
		return true
	}
}

// EqualValues compares two Lisp values for equality
func EqualValues(a, b LispValue) bool {
	switch va := a.(type) {
	case float64:
		if vb, ok := b.(float64); ok {
			return va == vb
		}
	case string:
		if vb, ok := b.(string); ok {
			return va == vb
		}
	case LispSymbol:
		if vb, ok := b.(LispSymbol); ok {
			return va == vb
		}
	case LispList:
		if vb, ok := b.(LispList); ok {
			if len(va) != len(vb) {
				return false
			}
			for i := range va {
				if !EqualValues(va[i], vb[i]) {
					return false
				}
			}
			return true
		}
	}
	return false
}

// PrintValue converts a LispValue to a string representation
func PrintValue(val LispValue) string {
	switch v := val.(type) {
	case LispSymbol:
		return string(v)
	case float64:
		return fmt.Sprintf("%g", v)
	case string:
		return fmt.Sprintf("%q", v)
	case LispList:
		elements := make([]string, len(v))
		for i, elem := range v {
			elements[i] = PrintValue(elem)
		}
		return "(" + strings.Join(elements, " ") + ")"
	case LispFunc:
		return "#<function>"
	case *Lambda:
		return "#<lambda>"
	case bool:
		if v {
			return "#t"
		}
		return "#f"
	case nil:
		return "nil"
	default:
		return fmt.Sprintf("%v", v)
	}
}
