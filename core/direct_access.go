package core

import "fmt"

// This file provides helper functions and utilities for direct property access
// It's designed to provide a more intuitive and convenient API for object property access

// GetAttr retrieves an attribute from an object using the object protocol
// It's a simpler wrapper around the optimized FastGetPropFrom function
func GetAttr(obj LispValue, name string) (LispValue, error) {
	if val, exists := FastGetPropFrom(obj, name); exists {
		return val, nil
	}
	return nil, ErrDotNoPropertyf(name)
}

// SetAttr sets an attribute on an object using the object protocol
// It's a simpler wrapper around the optimized FastSetPropOn function
func SetAttr(obj LispValue, name string, value LispValue) error {
	return FastSetPropOn(obj, name, value)
}

// HasAttr checks if an object has a given attribute
func HasAttr(obj LispValue, name string) bool {
	_, exists := FastGetPropFrom(obj, name)
	return exists
}

// GetAttrDefault retrieves an attribute, returning a default value if not found
func GetAttrDefault(obj LispValue, name string, defaultValue LispValue) LispValue {
	if val, exists := FastGetPropFrom(obj, name); exists {
		return val
	}
	return defaultValue
}

// GetNestedAttr retrieves a nested attribute from an object chain
// For example: GetNestedAttr(obj, ["a", "b", "c"]) is equivalent to obj.a.b.c
func GetNestedAttr(obj LispValue, path []string) (LispValue, error) {
	if len(path) == 0 {
		return obj, nil
	}

	current := obj
	currentPath := ""

	// Navigate the object path
	for i, part := range path {
		currentPath = currentPath + "." + part

		// Get the current part using the optimized helper
		if val, exists := FastGetPropFrom(current, part); exists {
			// If this is the last part, return it
			if i == len(path)-1 {
				return val, nil
			}

			// Otherwise, continue to the next part
			current = val
		} else {
			return nil, fmt.Errorf("attribute '%s' not found in path: %s", part, currentPath)
		}
	}

	return current, nil
}

// SetNestedAttr sets a nested attribute in an object chain
func SetNestedAttr(obj LispValue, path []string, value LispValue) error {
	if len(path) == 0 {
		return fmt.Errorf("empty attribute path")
	}

	if len(path) == 1 {
		// Direct attribute set using optimized function
		return SetAttr(obj, path[0], value)
	}

	// For nested attributes, we need to navigate to the parent object
	current := obj
	currentPath := ""

	// Navigate to the parent object
	for i := 0; i < len(path)-1; i++ {
		part := path[i]
		currentPath = currentPath + "." + part

		// Get the current part using the optimized helper
		if val, exists := FastGetPropFrom(current, part); exists {
			current = val
		} else {
			return fmt.Errorf("attribute '%s' not found in path: %s", part, currentPath)
		}
	}

	// Set the attribute on the parent object using optimized function
	return SetAttr(current, path[len(path)-1], value)
}

// GetItem retrieves an item from a sequence or mapping by key/index
func GetItem(obj LispValue, key LispValue) (LispValue, error) {
	// Handle different types of containers
	switch container := obj.(type) {
	case LispList:
		// For lists, key should be a numeric index
		idx, ok := key.(float64)
		if !ok {
			return nil, fmt.Errorf("list indices must be numbers, got %T", key)
		}

		intIdx := int(idx)
		if intIdx < 0 || intIdx >= len(container) {
			return nil, fmt.Errorf("list index out of range: %d (length %d)", intIdx, len(container))
		}

		return container[intIdx], nil

	case LispListLiteral:
		// For list literals, key should be a numeric index
		idx, ok := key.(float64)
		if !ok {
			return nil, fmt.Errorf("list indices must be numbers, got %T", key)
		}

		intIdx := int(idx)
		if intIdx < 0 || intIdx >= len(container) {
			return nil, fmt.Errorf("list index out of range: %d (length %d)", intIdx, len(container))
		}

		return container[intIdx], nil

	case LispTuple:
		// For tuples, key should be a numeric index
		idx, ok := key.(float64)
		if !ok {
			return nil, fmt.Errorf("tuple indices must be numbers, got %T", key)
		}

		intIdx := int(idx)
		if intIdx < 0 || intIdx >= len(container) {
			return nil, fmt.Errorf("tuple index out of range: %d (length %d)", intIdx, len(container))
		}

		return container[intIdx], nil

	case *PythonicDict:
		// For dictionaries, use the Get method
		val, exists := container.Get(key)
		if !exists {
			return nil, fmt.Errorf("key not found in dict: %v", key)
		}
		return val, nil

	case string:
		// For strings, key should be a numeric index
		idx, ok := key.(float64)
		if !ok {
			return nil, fmt.Errorf("string indices must be numbers, got %T", key)
		}

		intIdx := int(idx)
		if intIdx < 0 || intIdx >= len(container) {
			return nil, fmt.Errorf("string index out of range: %d (length %d)", intIdx, len(container))
		}

		return string(container[intIdx]), nil

	default:
		// Try to use the __getitem__ method if available
		if val, exists := GetPropFrom(obj, "__getitem__"); exists {
			// Call the method with the key
			switch fn := val.(type) {
			case BuiltinFunc:
				return fn([]LispValue{key}, nil)
			case Applicable:
				// We need an evaluator for this, but we don't have one here
				// This is a limitation of this approach
				return nil, fmt.Errorf("cannot call __getitem__ without an evaluator")
			}
		}

		return nil, fmt.Errorf("object of type %T does not support item access", obj)
	}
}

// SetItem sets an item in a sequence or mapping by key/index
func SetItem(obj LispValue, key LispValue, value LispValue) error {
	// Handle different types of containers
	switch container := obj.(type) {
	case LispList:
		// For lists, key should be a numeric index
		idx, ok := key.(float64)
		if !ok {
			return fmt.Errorf("list indices must be numbers, got %T", key)
		}

		intIdx := int(idx)
		if intIdx < 0 || intIdx >= len(container) {
			return fmt.Errorf("list index out of range: %d (length %d)", intIdx, len(container))
		}

		container[intIdx] = value
		return nil

	case LispListLiteral:
		// List literals should be immutable, but we'll allow this for consistency
		idx, ok := key.(float64)
		if !ok {
			return fmt.Errorf("list indices must be numbers, got %T", key)
		}

		intIdx := int(idx)
		if intIdx < 0 || intIdx >= len(container) {
			return fmt.Errorf("list index out of range: %d (length %d)", intIdx, len(container))
		}

		container[intIdx] = value
		return nil

	case *PythonicDict:
		// For dictionaries, use the Set method
		return container.Set(key, value)

	default:
		// Try to use the __setitem__ method if available
		if val, exists := GetPropFrom(obj, "__setitem__"); exists {
			// Call the method with the key and value
			switch fn := val.(type) {
			case BuiltinFunc:
				_, err := fn([]LispValue{key, value}, nil)
				return err
			case Applicable:
				// We need an evaluator for this, but we don't have one here
				// This is a limitation of this approach
				return fmt.Errorf("cannot call __setitem__ without an evaluator")
			}
		}

		return fmt.Errorf("object of type %T does not support item assignment", obj)
	}
}

// DelItem deletes an item from a sequence or mapping by key/index
func DelItem(obj LispValue, key LispValue) error {
	// Handle different types of containers
	switch container := obj.(type) {
	case *PythonicDict:
		// For dictionaries, use the Delete method
		container.Delete(key)
		return nil

	case *PythonicSet:
		// For sets, use the Remove method
		container.Remove(key)
		return nil

	default:
		// Try to use the __delitem__ method if available
		if val, exists := GetPropFrom(obj, "__delitem__"); exists {
			// Call the method with the key
			switch fn := val.(type) {
			case BuiltinFunc:
				_, err := fn([]LispValue{key}, nil)
				return err
			case Applicable:
				// We need an evaluator for this, but we don't have one here
				// This is a limitation of this approach
				return fmt.Errorf("cannot call __delitem__ without an evaluator")
			}
		}

		return fmt.Errorf("object of type %T does not support item deletion", obj)
	}
}

// Dir returns a list of attributes available on an object
func Dir(obj LispValue) ([]string, error) {
	// Use different approaches depending on the object type
	switch typedObj := obj.(type) {
	case *PythonicDict:
		// For dictionaries, return all keys and methods
		// Use the Size method instead of direct data access
		keys := make([]string, 0, typedObj.Size())

		// Add all dictionary keys using proper accessor method
		dictKeys := typedObj.SortedKeys()
		for _, k := range dictKeys {
			if strKey, ok := k.(string); ok {
				keys = append(keys, strKey)
			} else {
				// Convert non-string keys to string representation
				keys = append(keys, fmt.Sprintf("%v", k))
			}
		}

		// Add all dictionary methods
		for methodName := range typedObj.methods {
			keys = append(keys, methodName)
		}

		return keys, nil

	case *PythonicObject:
		// For objects, return all instance attributes and class methods
		attrs := make([]string, 0)

		// Add instance attributes
		if typedObj.Attributes != nil {
			// Use proper accessor methods instead of direct data access
			attrKeys := typedObj.Attributes.SortedKeys()
			for _, k := range attrKeys {
				if strKey, ok := k.(string); ok {
					attrs = append(attrs, strKey)
				}
			}
		}

		// Add class methods
		if typedObj.Class != nil {
			for methodName := range typedObj.Class.Methods {
				attrs = append(attrs, methodName)
			}

			// Add class attributes
			for attrName := range typedObj.Class.Attributes {
				attrs = append(attrs, attrName)
			}
		}

		return attrs, nil

	case ObjProtocol:
		// For ObjProtocol objects, we can't easily list all properties
		// This is a limitation of the current design
		return []string{"properties not enumerable"}, nil

	default:
		// For any other type, provide a simple type message
		return []string{fmt.Sprintf("type: %T", obj)}, nil
	}
}
