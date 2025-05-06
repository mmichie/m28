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

// Applicable represents a value that can be applied to arguments (function-like)
type Applicable interface {
	Apply(Evaluator, []LispValue, Environment) (LispValue, error)
}

// LispSymbol represents a Lisp symbol
type LispSymbol string

// LispList represents a Lisp list
// LispList represents a list of values
type LispList []LispValue

// LispListLiteral represents a list literal (created with [1 2 3] syntax)
// This helps the evaluator distinguish between function calls and list literals
type LispListLiteral []LispValue

// LispComprehension represents a list comprehension (created with [expr for var in iterable if condition] syntax)
type LispComprehension struct {
	Expression LispValue  // The expression to evaluate for each item
	Variable   LispSymbol // The iteration variable
	Iterable   LispValue  // The iterable to loop over
	Condition  LispValue  // Optional filter condition (nil if no condition)
}

// LispTuple represents a Python-style tuple (immutable sequence)
type LispTuple []LispValue

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

// ContextManager interface defines the methods for the context manager protocol
type ContextManager interface {
	Enter() (LispValue, error) // __enter__ method
	Exit(exc LispValue) error  // __exit__ method
}

// Generator represents a generator function that can be resumed and yield values
type Generator struct {
	Function     *Lambda     // Generator function
	Args         []LispValue // Arguments to the generator function
	State        Environment // Current state environment
	IsDone       bool        // Flag indicating if the generator is exhausted
	CurrentValue LispValue   // Last yielded value
	IsStarted    bool        // Flag indicating if the generator has been started
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
	// Add a shared state environment for closures
	SharedEnv Environment
	// Add a unique ID to identify each Lambda instance
	InstanceID int64
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

// SpecialFormMarker represents a special form registered in the environment
type SpecialFormMarker struct {
	Name LispSymbol
}

// IsBuiltinSpecialForm checks if a symbol is a built-in special form
func IsBuiltinSpecialForm(symbol LispSymbol) bool {
	// List of all special forms - maintain this in sync with special_forms/special_forms.go
	specialForms := map[LispSymbol]bool{
		// Control Flow
		"if": true, "elif": true, "else": true,
		"for": true, "while": true, "try": true,
		"break": true, "continue": true, "pass": true,

		// Function and Class Definition
		"def": true, "define": true, "class": true, "lambda": true,

		// Module Management
		"import": true,

		// Exception Handling
		"raise": true, "assert": true, "defexception": true,

		// Variable Scope
		"global": true, "nonlocal": true,

		// Misc
		"with": true, "begin": true, "return": true, "yield": true, "del": true,
	}

	_, ok := specialForms[symbol]
	return ok
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
	case LispListLiteral:
		return len(v) > 0
	case LispTuple:
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
		// Check if b is a LispList
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
		// Check if b is a LispListLiteral
		if vb, ok := b.(LispListLiteral); ok {
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
		return false
	case LispListLiteral:
		// Check if b is a LispList
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
		// Check if b is also a LispListLiteral
		if vb, ok := b.(LispListLiteral); ok {
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
		return false
	case LispTuple:
		// Check if b is a LispTuple
		if vb, ok := b.(LispTuple); ok {
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
		return false
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
	case LispListLiteral:
		elements := make([]string, len(v))
		for i, elem := range v {
			elements[i] = PrintValue(elem)
		}
		return "[" + strings.Join(elements, ", ") + "]"
	case LispTuple:
		elements := make([]string, len(v))
		for i, elem := range v {
			elements[i] = PrintValue(elem)
		}
		// Add trailing comma for single-element tuples to distinguish from parenthesized expressions
		if len(v) == 1 {
			return "(" + elements[0] + ",)"
		}
		return "(" + strings.Join(elements, ", ") + ")"
	case LispComprehension:
		// Format as [expr for var in iterable if condition]
		result := "[" + PrintValue(v.Expression) + " for " + string(v.Variable) + " in " + PrintValue(v.Iterable)
		if v.Condition != nil {
			result += " if " + PrintValue(v.Condition)
		}
		result += "]"
		return result
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
	case LispList:
		// For a list, check if it's the same object
		vb, ok := b.(LispList)
		return ok && &va == &vb
	case LispListLiteral:
		// For a list literal, check if it's the same object
		vb, ok := b.(LispListLiteral)
		return ok && &va == &vb
	case LispTuple:
		// For a tuple, check if it's the same object
		vb, ok := b.(LispTuple)
		return ok && &va == &vb
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
	_, isListLiteral := v.(LispListLiteral)
	_, isTuple := v.(LispTuple)
	return isList || isListLiteral || isTuple || v == nil
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
		} else if vb, ok := b.(LispListLiteral); ok {
			// Convert LispListLiteral to LispList for comparison
			vbList := LispList(vb)
			for i := 0; i < len(va) && i < len(vbList); i++ {
				if cmp := Compare(va[i], vbList[i]); cmp != 0 {
					return cmp
				}
			}
			if len(va) < len(vbList) {
				return -1
			} else if len(va) > len(vbList) {
				return 1
			}
			return 0
		}
	case LispListLiteral:
		// Convert LispListLiteral to LispList for comparison
		vaList := LispList(va)
		if vb, ok := b.(LispList); ok {
			for i := 0; i < len(vaList) && i < len(vb); i++ {
				if cmp := Compare(vaList[i], vb[i]); cmp != 0 {
					return cmp
				}
			}
			if len(vaList) < len(vb) {
				return -1
			} else if len(vaList) > len(vb) {
				return 1
			}
			return 0
		} else if vb, ok := b.(LispListLiteral); ok {
			vbList := LispList(vb)
			for i := 0; i < len(vaList) && i < len(vbList); i++ {
				if cmp := Compare(vaList[i], vbList[i]); cmp != 0 {
					return cmp
				}
			}
			if len(vaList) < len(vbList) {
				return -1
			} else if len(vaList) > len(vbList) {
				return 1
			}
			return 0
		}
	case LispTuple:
		if vb, ok := b.(LispTuple); ok {
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
