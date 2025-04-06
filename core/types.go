package core

import (
	"fmt"
	"math"
	"reflect"
	"sort"
	"strings"
	"sync"
)

// LispValue represents any Lisp value
type LispValue interface{}

// LispSymbol represents a Lisp symbol
type LispSymbol string

// LispList represents a Lisp list
type LispList []LispValue

// BuiltinFunc represents a built-in function
type BuiltinFunc func([]LispValue, Environment) (LispValue, error)

// PythonicBool represents a Python-style boolean
type PythonicBool bool

// PythonicNone represents Python's None value
type PythonicNone struct{}

// PythonicSet represents a Python-style set
type PythonicSet struct {
	data map[LispValue]struct{}
	mu   sync.RWMutex
}

// Environment interface defines the methods for managing variable bindings
type Environment interface {
	Get(symbol LispSymbol) (LispValue, bool)
	Set(symbol LispSymbol, value LispValue)
	Define(symbol LispSymbol, value LispValue)
	SetMutable(symbol LispSymbol, value LispValue) bool
	NewEnvironment(outer Environment) Environment
	String() string
	StringWithDepth(depth int) string
}

// Evaluator interface defines the methods for evaluating Lisp expressions
type Evaluator interface {
	Eval(expr LispValue, env Environment) (LispValue, error)
	Apply(fn LispValue, args []LispValue, env Environment) (LispValue, error)
}

// Lambda represents a Python-like lambda function
type Lambda struct {
	Params        []LispSymbol
	Body          LispValue
	Env           Environment
	Closure       Environment
	DefaultValues map[LispSymbol]LispValue
}

// OptionalParam represents an optional parameter in a Lambda
type OptionalParam struct {
	Name         LispSymbol
	DefaultValue LispValue
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

// NewPythonicSet creates a new PythonicSet
func NewPythonicSet() *PythonicSet {
	return &PythonicSet{
		data: make(map[LispValue]struct{}),
	}
}

// Add adds an element to the PythonicSet
func (s *PythonicSet) Add(value LispValue) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.data[value] = struct{}{}
}

// Helper method for PythonicSet to get sorted elements
func (s *PythonicSet) sortedElements() []LispValue {
	elements := make([]LispValue, 0, len(s.data))
	for elem := range s.data {
		elements = append(elements, elem)
	}
	sort.Slice(elements, func(i, j int) bool {
		return Compare(elements[i], elements[j]) < 0
	})
	return elements
}

// Add Size method to PythonicSet
func (s *PythonicSet) Size() int {
	s.mu.RLock()
	defer s.mu.RUnlock()
	return len(s.data)
}

// Contains checks if an element is in the PythonicSet
func (s *PythonicSet) Contains(value LispValue) bool {
	s.mu.RLock()
	defer s.mu.RUnlock()
	_, ok := s.data[value]
	return ok
}

// Remove removes an element from the PythonicSet
func (s *PythonicSet) Remove(value LispValue) {
	s.mu.Lock()
	defer s.mu.Unlock()
	delete(s.data, value)
}

// Data returns the underlying map of the PythonicSet
func (s *PythonicSet) Data() map[LispValue]struct{} {
	s.mu.RLock()
	defer s.mu.RUnlock()
	return s.data
}

// IsTruthy determines if a value is considered true in Pythonic Lisp
func IsTruthy(v LispValue) bool {
	switch v := v.(type) {
	case nil, PythonicNone:
		return false
	case PythonicBool:
		return bool(v)
	case float64:
		return v != 0 && !math.IsNaN(v)
	case string:
		return v != ""
	case LispList:
		return len(v) > 0
	case *PythonicDict:
		return len(v.data) > 0
	case *PythonicSet:
		return len(v.data) > 0
	default:
		return true
	}
}

// EqualValues compares two Lisp values for equality
func EqualValues(a, b LispValue) bool {
	switch va := a.(type) {
	case LispList:
		vb, ok := b.(LispList)
		if !ok || len(va) != len(vb) {
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
		return ok && va == vb
	case LispSymbol:
		vb, ok := b.(LispSymbol)
		return ok && va == vb
	case PythonicBool:
		vb, ok := b.(PythonicBool)
		return ok && va == vb
	case PythonicNone:
		_, ok := b.(PythonicNone)
		return ok
	case *PythonicDict:
		vb, ok := b.(*PythonicDict)
		if !ok || len(va.data) != len(vb.data) {
			return false
		}
		for k, v := range va.data {
			vbv, ok := vb.Get(k)
			if !ok || !EqualValues(v, vbv) {
				return false
			}
		}
		return true
	case *PythonicSet:
		vb, ok := b.(*PythonicSet)
		if !ok || len(va.data) != len(vb.data) {
			return false
		}
		for k := range va.data {
			if !vb.Contains(k) {
				return false
			}
		}
		return true
	default:
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
			return "float('nan')"
		}
		if math.IsInf(v, 1) {
			return "float('inf')"
		}
		if math.IsInf(v, -1) {
			return "float('-inf')"
		}
		return fmt.Sprintf("%g", v)
	case string:
		return fmt.Sprintf("%q", v)
	case LispList:
		elements := make([]string, len(v))
		for i, elem := range v {
			elements[i] = PrintValue(elem)
		}
		return "[" + strings.Join(elements, ", ") + "]"
	case PythonicBool:
		if v {
			return "True"
		}
		return "False"
	case PythonicNone:
		return "None"
	case *PythonicDict:
		pairs := make([]string, 0, len(v.data))
		for k, val := range v.data {
			pairs = append(pairs, fmt.Sprintf("%s: %s", PrintValue(k), PrintValue(val)))
		}
		return "{" + strings.Join(pairs, ", ") + "}"
	case *PythonicSet:
		elements := make([]string, 0, len(v.data))
		for k := range v.data {
			elements = append(elements, PrintValue(k))
		}
		return "{" + strings.Join(elements, ", ") + "}"
	case BuiltinFunc:
		return "#<builtin-function>"
	case *Lambda:
		return "#<lambda>"
	default:
		return fmt.Sprintf("%v", v)
	}
}

// EqValues compares two Lisp values for identity equality
func EqValues(a, b LispValue) bool {
	switch va := a.(type) {
	case LispSymbol:
		vb, ok := b.(LispSymbol)
		return ok && va == vb
	case float64:
		vb, ok := b.(float64)
		return ok && va == vb
	case string:
		vb, ok := b.(string)
		return ok && va == vb
	case PythonicBool:
		vb, ok := b.(PythonicBool)
		return ok && va == vb
	case PythonicNone:
		_, ok := b.(PythonicNone)
		return ok
	case *PythonicDict:
		vb, ok := b.(*PythonicDict)
		return ok && va == vb
	case *PythonicSet:
		vb, ok := b.(*PythonicSet)
		return ok && va == vb
	default:
		return a == b
	}
}

// PrintValueWithoutQuotes converts a LispValue to a string representation without quotes for strings
func PrintValueWithoutQuotes(val LispValue) string {
	if str, ok := val.(string); ok {
		return str
	}
	return PrintValue(val)
}

// IsList checks if a value is a list
func IsList(v LispValue) bool {
	_, isList := v.(LispList)
	return isList || v == nil
}

// Compare compares two LispValues and returns an integer indicating their relative order.
// Returns -1 if a < b, 0 if a == b, and 1 if a > b.
func Compare(a, b LispValue) int {
	switch va := a.(type) {
	case float64:
		if vb, ok := b.(float64); ok {
			if va < vb {
				return -1
			} else if va > vb {
				return 1
			}
			return 0
		}
	case string:
		if vb, ok := b.(string); ok {
			return strings.Compare(va, vb)
		}
	case LispSymbol:
		if vb, ok := b.(LispSymbol); ok {
			return strings.Compare(string(va), string(vb))
		}
	case PythonicBool:
		if vb, ok := b.(PythonicBool); ok {
			if va == vb {
				return 0
			} else if va {
				return 1
			} else {
				return -1
			}
		}
	case LispList:
		if vb, ok := b.(LispList); ok {
			for i := 0; i < len(va) && i < len(vb); i++ {
				if cmp := Compare(va[i], vb[i]); cmp != 0 {
					return cmp
				}
			}
			if len(va) < len(vb) {
				return -1
			} else if len(va) > len(vb) {
				return 1
			}
			return 0
		}
	case *PythonicDict:
		if vb, ok := b.(*PythonicDict); ok {
			if va.Size() != vb.Size() {
				if va.Size() < vb.Size() {
					return -1
				}
				return 1
			}
			// Compare keys in sorted order
			aKeys := va.sortedKeys()
			bKeys := vb.sortedKeys()
			for i := 0; i < len(aKeys); i++ {
				if cmp := Compare(aKeys[i], bKeys[i]); cmp != 0 {
					return cmp
				}
				aVal, _ := va.Get(aKeys[i])
				bVal, _ := vb.Get(bKeys[i])
				if cmp := Compare(aVal, bVal); cmp != 0 {
					return cmp
				}
			}
			return 0
		}
	case *PythonicSet:
		if vb, ok := b.(*PythonicSet); ok {
			if va.Size() != vb.Size() {
				if va.Size() < vb.Size() {
					return -1
				}
				return 1
			}
			// Compare elements in sorted order
			aElems := va.sortedElements()
			bElems := vb.sortedElements()
			for i := 0; i < len(aElems); i++ {
				if cmp := Compare(aElems[i], bElems[i]); cmp != 0 {
					return cmp
				}
			}
			return 0
		}
	}

	// For incomparable types, we'll use their string representations
	sa := PrintValue(a)
	sb := PrintValue(b)
	return strings.Compare(sa, sb)
}
