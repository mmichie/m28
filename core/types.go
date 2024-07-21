package core

import (
	"fmt"
	"math"
	"strings"
)

// LispValue represents any Lisp value
type LispValue interface{}

// LispSymbol represents a Lisp symbol
type LispSymbol string

// LispList represents a Lisp list
type LispList []LispValue

// BuiltinFunc represents a built-in function
type BuiltinFunc func([]LispValue, Environment) (LispValue, error)

// Nil represents the nil value in Lisp
type Nil struct{}

// Environment interface defines the methods for managing variable bindings
type Environment interface {
	Get(symbol LispSymbol) (LispValue, bool)
	Set(symbol LispSymbol, value LispValue)
	Define(symbol LispSymbol, value LispValue)
	SetMutable(symbol LispSymbol, value LispValue) bool
	NewEnvironment(outer Environment) Environment
}

// Evaluator interface defines the method for evaluating Lisp expressions
type Evaluator interface {
	Eval(expr LispValue, env Environment) (LispValue, error)
}

// Parser interface defines the method for parsing Lisp code
type Parser interface {
	Parse(input string) (LispValue, error)
}

// Lambda represents a lambda function
type Lambda struct {
	Params    []LispSymbol
	RestParam LispSymbol
	Body      LispValue
	Env       Environment
	Closure   Environment
}

// Macro represents a Lisp macro
type Macro struct {
	Params []LispSymbol
	Body   LispValue
	Env    Environment
}

// Quasiquote represents a quasiquoted expression
type Quasiquote struct {
	Expr LispValue
}

// Unquote represents an unquoted expression
type Unquote struct {
	Expr LispValue
}

// UnquoteSplicing represents an unquote-splicing expression
type UnquoteSplicing struct {
	Expr LispValue
}

// BuiltinFuncs maps function names to their implementations
var BuiltinFuncs = make(map[LispSymbol]BuiltinFunc)

// RegisterBuiltin registers a builtin function
func RegisterBuiltin(name string, fn BuiltinFunc) {
	BuiltinFuncs[LispSymbol(name)] = fn
}

// IsTruthy determines if a value is considered true in Lisp
func IsTruthy(v LispValue) bool {
	switch v := v.(type) {
	case nil, Nil:
		return false
	case bool:
		return v
	case float64:
		return v != 0 && !math.IsNaN(v)
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
			if math.IsNaN(va) && math.IsNaN(vb) {
				return true
			}
			if math.IsInf(va, 1) && math.IsInf(vb, 1) {
				return true
			}
			if math.IsInf(va, -1) && math.IsInf(vb, -1) {
				return true
			}
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
	case bool:
		if vb, ok := b.(bool); ok {
			return va == vb
		}
	case nil, *struct{}:
		return false
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
		if math.IsNaN(v) {
			return "+nan.0"
		}
		if math.IsInf(v, 1) {
			return "+inf.0"
		}
		if math.IsInf(v, -1) {
			return "-inf.0"
		}
		return fmt.Sprintf("%g", v)
	case string:
		return fmt.Sprintf("%q", v)
	case LispList:
		elements := make([]string, len(v))
		for i, elem := range v {
			elements[i] = PrintValue(elem)
		}
		return "(" + strings.Join(elements, " ") + ")"
	case BuiltinFunc:
		return "#<builtin-function>"
	case *Lambda:
		return "#<lambda>"
	case bool:
		if v {
			return "t"
		}
		return "nil"
	case Nil:
		return "nil"
	default:
		return fmt.Sprintf("%v", v)
	}
}

func PrintValueWithoutQuotes(val LispValue) string {
	switch v := val.(type) {
	case string:
		return v
	default:
		return PrintValue(val)
	}
}
