package builtin

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/mmichie/m28/common/builders"
	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// RegisterPathOps registers path operation functions
func RegisterPathOps(ctx *core.Context) {
	ctx.Define("get-path", core.NewNamedBuiltinFunction("get-path", GetPathBuilder()))
	ctx.Define("set-path", core.NewNamedBuiltinFunction("set-path", SetPathBuilder()))
	ctx.Define("update-path", core.NewNamedBuiltinFunction("update-path", UpdatePathBuilder()))
	ctx.Define("has-path?", core.NewNamedBuiltinFunction("has-path?", HasPathBuilder()))
}

// parsePath converts a path (string or list) into a slice of path segments
// String paths like "user.address.city" are split by dots
// Numeric segments are converted to integers
func parsePath(pathValue core.Value) ([]interface{}, error) {
	switch p := pathValue.(type) {
	case core.StringValue:
		// Parse string path: "users.0.name" -> ["users", 0, "name"]
		if string(p) == "" {
			return []interface{}{}, nil
		}

		parts := strings.Split(string(p), ".")
		segments := make([]interface{}, len(parts))

		for i, part := range parts {
			// Try to parse as integer for list indexing
			if num, err := strconv.Atoi(part); err == nil {
				segments[i] = num
			} else {
				segments[i] = part
			}
		}
		return segments, nil

	case *core.ListValue:
		// Use list path as-is: ["user", 0, "name"]
		segments := make([]interface{}, p.Len())
		for i, seg := range p.Items() {
			switch s := seg.(type) {
			case core.StringValue:
				segments[i] = string(s)
			case core.NumberValue:
				segments[i] = int(s)
			default:
				return nil, fmt.Errorf("path segments must be strings or integers, got %s", seg.Type())
			}
		}
		return segments, nil

	default:
		return nil, fmt.Errorf("path must be a string or list, got %s", pathValue.Type())
	}
}

// getAtPath retrieves a value from nested data structures following a path
func getAtPath(data core.Value, segments []interface{}) (core.Value, bool) {
	current := data

	for _, segment := range segments {
		switch seg := segment.(type) {
		case string:
			// Try dict access first
			if dict, ok := current.(*core.DictValue); ok {
				value, exists := dict.GetValue(core.StringValue(seg))
				if !exists {
					return nil, false
				}
				current = value
				continue
			}

			// Try object attribute access
			if obj, ok := current.(core.Object); ok {
				value, exists := obj.GetAttr(seg)
				if !exists {
					return nil, false
				}
				current = value
				continue
			}

			return nil, false

		case int:
			// Try list/tuple index access
			if list, ok := current.(*core.ListValue); ok {
				if seg < 0 || seg >= list.Len() {
					return nil, false
				}
				current = list.Items()[seg]
				continue
			}

			if tuple, ok := current.(core.TupleValue); ok {
				if seg < 0 || seg >= len(tuple) {
					return nil, false
				}
				current = tuple[seg]
				continue
			}

			return nil, false

		default:
			return nil, false
		}
	}

	return current, true
}

// setAtPath creates a new data structure with the value set at the given path
// This is immutable - it returns a new structure without modifying the original
func setAtPath(data core.Value, segments []interface{}, value core.Value) (core.Value, error) {
	if len(segments) == 0 {
		return value, nil
	}

	segment := segments[0]
	remaining := segments[1:]

	switch seg := segment.(type) {
	case string:
		// Handle dict
		if dict, ok := data.(*core.DictValue); ok {
			newDict := core.NewDict()
			// Copy all existing key-value pairs
			for _, k := range dict.Keys() {
				val, _ := dict.Get(k)
				origKey, hasOrig := dict.OriginalKeys()[0], false
				for _, ok := range dict.OriginalKeys() {
					if core.ValueToKey(ok) == k {
						origKey = ok
						hasOrig = true
						break
					}
				}
				if hasOrig {
					newDict.SetValue(origKey, val)
				} else {
					newDict.Set(k, val)
				}
			}

			// Get current value at this segment
			current, _ := dict.GetValue(core.StringValue(seg))

			// Recursively set on remaining path
			newValue, err := setAtPath(current, remaining, value)
			if err != nil {
				return nil, err
			}

			newDict.SetValue(core.StringValue(seg), newValue)
			return newDict, nil
		}

		// Handle object - for now, we don't support immutable updates on objects
		if _, ok := data.(core.Object); ok {
			return nil, fmt.Errorf("immutable update of object attributes not yet supported")
		}

		return nil, fmt.Errorf("cannot use string key '%s' on %s", seg, data.Type())

	case int:
		// Handle list
		if list, ok := data.(*core.ListValue); ok {
			if seg < 0 || seg >= list.Len() {
				return nil, fmt.Errorf("list index out of range: %d", seg)
			}

			// Clone list
			newList := make([]core.Value, list.Len())
			copy(newList, list.Items())

			// Get current value at index
			current := list.Items()[seg]

			// Recursively set on remaining path
			newValue, err := setAtPath(current, remaining, value)
			if err != nil {
				return nil, err
			}

			newList[seg] = newValue
			return core.NewList(newList...), nil
		}

		return nil, fmt.Errorf("cannot use integer index %d on %s", seg, data.Type())

	default:
		return nil, fmt.Errorf("invalid path segment type")
	}
}

// GetPathBuilder implements the get-path function
func GetPathBuilder() builders.BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("get-path", args)
		if err := v.Range(2, 3); err != nil {
			return nil, err
		}

		data := v.Get(0)
		pathValue := v.Get(1)
		var defaultValue core.Value = core.Nil
		if v.Count() == 3 {
			defaultValue = v.Get(2)
		}

		// Parse the path
		segments, err := parsePath(pathValue)
		if err != nil {
			return nil, err
		}

		// Get value at path
		result, found := getAtPath(data, segments)
		if !found {
			return defaultValue, nil
		}

		return result, nil
	}
}

// SetPathBuilder implements the set-path function (immutable)
func SetPathBuilder() builders.BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("set-path", args)
		if err := v.Exact(3); err != nil {
			return nil, err
		}

		data := v.Get(0)
		pathValue := v.Get(1)
		value := v.Get(2)

		// Parse the path
		segments, err := parsePath(pathValue)
		if err != nil {
			return nil, err
		}

		// Set value at path (returns new structure)
		return setAtPath(data, segments, value)
	}
}

// UpdatePathBuilder implements the update-path function
func UpdatePathBuilder() builders.BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("update-path", args)
		if err := v.Exact(3); err != nil {
			return nil, err
		}

		data := v.Get(0)
		pathValue := v.Get(1)
		fn := v.Get(2)

		// Ensure fn is callable
		callable, ok := types.AsCallable(fn)
		if !ok {
			return nil, fmt.Errorf("update-path requires a callable, got %s", fn.Type())
		}

		// Parse the path
		segments, err := parsePath(pathValue)
		if err != nil {
			return nil, err
		}

		// Get current value at path
		currentValue, found := getAtPath(data, segments)
		if !found {
			return nil, fmt.Errorf("path not found: %v", pathValue)
		}

		// Apply function to current value
		newValue, err := callable.Call([]core.Value{currentValue}, ctx)
		if err != nil {
			return nil, err
		}

		// Set new value at path (returns new structure)
		return setAtPath(data, segments, newValue)
	}
}

// HasPathBuilder implements the has-path? function
func HasPathBuilder() builders.BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("has-path?", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		data := v.Get(0)
		pathValue := v.Get(1)

		// Parse the path
		segments, err := parsePath(pathValue)
		if err != nil {
			return nil, err
		}

		// Check if path exists
		_, found := getAtPath(data, segments)
		return core.BoolValue(found), nil
	}
}
