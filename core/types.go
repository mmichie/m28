package core

import (
	"fmt"
	"math"
	"reflect"
	"strings"
	"sync"
)

type Evaluator interface {
	Eval(expr LispValue, env Environment) (LispValue, error)
	Apply(fn LispValue, args []LispValue, env Environment) (LispValue, error)
}

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

// Parser interface defines the method for parsing Lisp code
type Parser interface {
	Parse(input string) (LispValue, error)
}

type OptionalParam struct {
	Name         LispSymbol
	DefaultValue LispValue
}

type Lambda struct {
	Params    []LispSymbol
	Optional  []OptionalParam
	Rest      LispSymbol
	KeyParams map[LispSymbol]LispValue
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
	case LispList:
		vb, ok := b.(LispList)
		if !ok {
			return false
		}
		if len(va) != len(vb) {
			return false
		}
		for i := range va {
			if !EqualValues(va[i], vb[i]) {
				return false
			}
		}
		return true

	case float64:
		vb, ok := b.(float64)
		if !ok {
			return false
		}
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

	case string:
		vb, ok := b.(string)
		if !ok {
			return false
		}
		return va == vb

	case LispSymbol:
		vb, ok := b.(LispSymbol)
		if !ok {
			return false
		}
		return va == vb

	case bool:
		vb, ok := b.(bool)
		if !ok {
			return false
		}
		return va == vb

	case nil:
		return b == nil

	case Nil:
		switch b.(type) {
		case Nil:
			return true
		case LispList:
			return len(b.(LispList)) == 0
		default:
			return false
		}
	default:
		// For any other types, use reflect.DeepEqual as a fallback
		return reflect.DeepEqual(a, b)
	}
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
		if len(v) == 0 {
			return "nil"
		}
		elements := make([]string, 0, len(v))
		for i, elem := range v {
			if i == len(v)-2 && !IsList(v[i+1]) {
				// This is a dotted pair at the end
				elements = append(elements, PrintValue(elem))
				elements = append(elements, ".", PrintValue(v[i+1]))
				break
			}
			elements = append(elements, PrintValue(elem))
		}
		return "(" + strings.Join(elements, " ") + ")"
	case BuiltinFunc:
		return "#<builtin-function>"
	case *Lambda:
		return "#<lambda>"
	case *LispHashTable:
		return "#<HASH-TABLE>"
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

// Helper function to check if a value is a list
func IsList(v LispValue) bool {
	_, isList := v.(LispList)
	return isList
}

func PrintValueWithoutQuotes(val LispValue) string {
	switch v := val.(type) {
	case string:
		return v
	default:
		return PrintValue(val)
	}
}

// EqValues compares two Lisp values for equality (identity)
func EqValues(a, b LispValue) bool {
	switch va := a.(type) {
	case LispSymbol:
		if vb, ok := b.(LispSymbol); ok {
			return va == vb
		}
	case float64:
		if vb, ok := b.(float64); ok {
			return va == vb
		}
	case string:
		if vb, ok := b.(string); ok {
			return va == vb
		}
	case bool:
		if vb, ok := b.(bool); ok {
			return va == vb
		}
	case nil:
		return b == nil
	case Nil:
		_, ok := b.(Nil)
		return ok
	case LispList:
		if vb, ok := b.(LispList); ok {
			return &va == &vb // Compare pointers for lists
		}
	}
	return false
}

// LispHashTable represents a Lisp hash table
type LispHashTable struct {
	data map[LispValue]LispValue
	mu   sync.RWMutex
}

// NewLispHashTable creates a new LispHashTable
func NewLispHashTable() *LispHashTable {
	return &LispHashTable{
		data: make(map[LispValue]LispValue),
	}
}

// Get retrieves a value from the hash table
func (ht *LispHashTable) Get(key LispValue) (LispValue, bool) {
	ht.mu.RLock()
	defer ht.mu.RUnlock()
	val, ok := ht.data[key]
	return val, ok
}

// Set sets a value in the hash table
func (ht *LispHashTable) Set(key, value LispValue) {
	ht.mu.Lock()
	defer ht.mu.Unlock()
	ht.data[key] = value
}

// Delete removes a key-value pair from the hash table
func (ht *LispHashTable) Delete(key LispValue) {
	ht.mu.Lock()
	defer ht.mu.Unlock()
	delete(ht.data, key)
}
