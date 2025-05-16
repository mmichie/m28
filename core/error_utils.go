package core

import "fmt"

// Error utility functions for adapters

// ErrWrongArgCount formats an error message for wrong argument count
func ErrWrongArgCount(funcName string, expected, got int) error {
	return fmt.Errorf("%s expects %d arguments, got %d", funcName, expected, got)
}

// ErrTypeMismatch formats an error message for type mismatch
func ErrTypeMismatch(expected, got string) error {
	return fmt.Errorf("expected %s, got %s", expected, got)
}

// ErrIndexOutOfRange formats an error message for index out of range
func ErrIndexOutOfRange(index, length int) error {
	return fmt.Errorf("index %d out of range (length %d)", index, length)
}

// TypeOf returns a string representation of a value's type
func TypeOf(value LispValue) string {
	if value == nil {
		return "nil"
	}

	switch v := value.(type) {
	case float64:
		return "number"
	case string:
		return "string"
	case LispSymbol:
		return "symbol"
	case LispList:
		return "list"
	case LispListLiteral:
		return "list"
	case LispTuple:
		return "tuple"
	case *PythonicDict:
		return "dict"
	case *PythonicSet:
		return "set"
	case *Lambda:
		return "function"
	case PythonicBool:
		return "boolean"
	case PythonicNone:
		return "None"
	case *PythonicObject:
		if v.Class != nil {
			return v.Class.Name
		}
		return "object"
	default:
		return fmt.Sprintf("%T", v)
	}
}
