package eval

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// GetItemForm implements the get-item special form for index access
// (get-item obj key) -> value at index/key
func GetItemForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("get-item requires exactly 2 arguments, got %d", len(args))
	}

	// Evaluate the object
	obj, err := Eval(args[0], ctx)
	if err != nil {
		return nil, fmt.Errorf("error evaluating object: %v", err)
	}

	// Evaluate the key/index
	key, err := Eval(args[1], ctx)
	if err != nil {
		return nil, fmt.Errorf("error evaluating key: %v", err)
	}

	// Handle different object types
	switch v := obj.(type) {
	case core.ListValue:
		// List indexing requires numeric index
		idx, ok := key.(core.NumberValue)
		if !ok {
			return nil, fmt.Errorf("list indices must be integers, not %s", key.Type())
		}
		
		index := int(idx)
		if index < 0 {
			index = len(v) + index
		}
		
		if index < 0 || index >= len(v) {
			return nil, &core.IndexError{Index: index, Length: len(v)}
		}
		
		return v[index], nil

	case core.TupleValue:
		// Tuple indexing
		idx, ok := key.(core.NumberValue)
		if !ok {
			return nil, fmt.Errorf("tuple indices must be integers, not %s", key.Type())
		}
		
		index := int(idx)
		if index < 0 {
			index = len(v) + index
		}
		
		if index < 0 || index >= len(v) {
			return nil, &core.IndexError{Index: index, Length: len(v)}
		}
		
		return v[index], nil

	case *core.DictValue:
		// Dictionary key access
		// Convert key to string
		keyStr := ""
		switch k := key.(type) {
		case core.StringValue:
			keyStr = string(k)
		case core.NumberValue:
			keyStr = fmt.Sprintf("%g", float64(k))
		case core.SymbolValue:
			keyStr = string(k)
		default:
			keyStr = key.String()
		}
		
		// Check if key exists
		if val, exists := v.Get(keyStr); exists {
			return val, nil
		}
		
		// If not found, return KeyError
		return nil, &core.KeyError{Key: key}

	case core.StringValue:
		// String indexing
		idx, ok := key.(core.NumberValue)
		if !ok {
			return nil, fmt.Errorf("string indices must be integers, not %s", key.Type())
		}
		
		str := string(v)
		index := int(idx)
		if index < 0 {
			index = len(str) + index
		}
		
		if index < 0 || index >= len(str) {
			return nil, &core.IndexError{Index: index, Length: len(str)}
		}
		
		return core.StringValue(str[index:index+1]), nil

	default:
		return nil, fmt.Errorf("'%s' object is not subscriptable", obj.Type())
	}
}

// RegisterIndexing registers the indexing special forms
func RegisterIndexing() {
	RegisterSpecialForm("get-item", GetItemForm)
}