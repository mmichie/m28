# Dictionary Implementation Fixes for M28

This document outlines the issues identified in the M28 dictionary implementation and proposes solutions to make dictionaries fully functional. The goal is to ensure that dictionaries work reliably with all the documented features and properly integrate with the object protocol system.

## Current Status and Issues

After analyzing the codebase, we've identified several issues with dictionary implementation:

### 1. Missing Functionality

- `dict.has_key` method is documented but not implemented
- The equivalent of Python's `__contains__` (checking if a key exists) is missing
- No implementation of `contains?` method referenced in examples
- Dictionary iteration protocols are incomplete

### 2. Core Implementation Issues (`python_dict.go`)

- Inconsistent usage of mutex locks across methods
- `sortedKeys()` and `SortedKeys()` are duplicative (private and public versions)
- The `Iterate` method doesn't handle concurrent modifications safely
- Legacy interface methods (`HasProperty`, `GetProperty`, `SetProperty`) don't fully integrate with the newer object protocol
- Missing documentation for many methods

### 3. Dictionary Operations Issues (`dict_ops.go`)

- `dictItemsFunc` doesn't sort keys while the internal method does
- `dictValuesFunc` doesn't preserve key order, making values order unpredictable
- `dictClearFunc` creates a completely new dictionary instead of clearing the existing one
- `dictUpdateFunc` error message indicates it requires at least 2 arguments, but it should only require 1 dictionary plus update values

### 4. Object Protocol Integration

- Line 270 in `python_dict.go` references `AsObject` implementation in `module_adapter.go`, but its implementation isn't clear
- `GetMember` method has issues with pseudo-properties - it checks for "length", "len", "size", "count" but these aren't consistently implemented
- No `HasMember` implementation for the object protocol

### 5. Dot Notation Access

- Dot notation for property access is mentioned in documentation but appears inconsistently implemented
- Dot notation for method calls isn't working reliably
- The object protocol implementation for dictionaries doesn't fully support dot notation standards

### 6. Test Issues

- Test coverage is minimal, focusing on very basic operations
- No tests for error conditions or edge cases
- Advanced features mentioned in documentation lack proper testing

## Proposed Solutions

### 1. Complete Dictionary Method Implementation

```go
// Add missing has_key method
func (d *PythonicDict) HasKey(key LispValue) bool {
    d.mu.RLock()
    defer d.mu.RUnlock()
    _, exists := d.data[key]
    return exists
}

// Add contains method
func (d *PythonicDict) Contains(key LispValue) bool {
    return d.HasKey(key)
}

// Expose as dict builtins
core.RegisterBuiltin("dict.has_key", func(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
    if len(args) != 2 {
        return nil, fmt.Errorf("dict.has_key requires exactly 2 arguments")
    }
    
    dict, ok := args[0].(*core.PythonicDict)
    if !ok {
        return nil, fmt.Errorf("dict.has_key requires a dict as first argument, got %T", args[0])
    }
    
    return dict.HasKey(args[1]), nil
})

core.RegisterBuiltin("dict.contains?", func(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
    if len(args) != 2 {
        return nil, fmt.Errorf("dict.contains? requires exactly 2 arguments")
    }
    
    dict, ok := args[0].(*core.PythonicDict)
    if !ok {
        return nil, fmt.Errorf("dict.contains? requires a dict as first argument, got %T", args[0])
    }
    
    return dict.Contains(args[1]), nil
})
```

### 2. Fix Dictionary Operation Methods

```go
// Fix dictItemsFunc to sort keys
func dictItemsFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
    if len(args) != 1 {
        return nil, fmt.Errorf("dict.items() requires exactly one argument")
    }

    dict, ok := args[0].(*core.PythonicDict)
    if !ok {
        return nil, fmt.Errorf("dict.items() requires a dict as argument, got %T", args[0])
    }

    // Get sorted keys for consistent ordering
    keys := dict.SortedKeys()
    items := make(core.LispList, len(keys))
    
    for i, key := range keys {
        value, _ := dict.Get(key)
        items[i] = core.LispList{key, value}
    }

    return items, nil
}

// Fix dictValuesFunc to preserve key order
func dictValuesFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
    if len(args) != 1 {
        return nil, fmt.Errorf("dict.values() requires exactly one argument")
    }

    dict, ok := args[0].(*core.PythonicDict)
    if !ok {
        return nil, fmt.Errorf("dict.values() requires a dict as argument, got %T", args[0])
    }

    // Get sorted keys for consistent ordering
    keys := dict.SortedKeys()
    values := make(core.LispList, len(keys))
    
    for i, key := range keys {
        value, _ := dict.Get(key)
        values[i] = value
    }

    return values, nil
}

// Fix dictClearFunc to clear in place
func dictClearFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
    if len(args) != 1 {
        return nil, fmt.Errorf("dict.clear() requires exactly one argument")
    }

    dict, ok := args[0].(*core.PythonicDict)
    if !ok {
        return nil, fmt.Errorf("dict.clear() requires a dict as argument, got %T", args[0])
    }

    // Clear the dictionary in place
    dict.mu.Lock()
    dict.data = make(map[LispValue]LispValue)
    dict.mu.Unlock()

    return core.PythonicNone{}, nil
}
```

### 3. Object Protocol Integration

```go
// Add HasMember method for dictionaries
func (d *PythonicDict) HasMember(name string) bool {
    // First check for methods
    if d.HasMethod(name) {
        return true
    }
    
    // Then check for common dictionary pseudo-properties
    switch name {
    case "length", "len", "size", "count":
        return true
    }
    
    // Finally check for attributes
    _, exists := d.Get(name)
    return exists
}

// Ensure consistent implementation of ObjProtocol interface
var _ core.ObjProtocol = (*PythonicDict)(nil)
```

### 4. Safe Iteration with Lock Protection

```go
// Improved Iterate method with better safety
func (d *PythonicDict) Iterate(f func(key, value LispValue) error) error {
    d.mu.RLock()
    // Create a copy of the keys and values to iterate
    keys := make([]core.LispValue, 0, len(d.data))
    values := make([]core.LispValue, 0, len(d.data))
    
    for k, v := range d.data {
        keys = append(keys, k)
        values = append(values, v)
    }
    d.mu.RUnlock()
    
    // Now iterate using the copies
    for i := range keys {
        if err := f(keys[i], values[i]); err != nil {
            return err
        }
    }
    return nil
}
```

### 5. Improve Dot Notation Support

```go
// In evaluator.go or special_forms.go

// Enhanced evalDotAccess to handle dictionaries better
func (e *Evaluator) evalDotAccess(expr LispValue, env Environment) (LispValue, error) {
    // [...existing code...]
    
    // Special handling for dictionaries
    if dict, ok := object.(*core.PythonicDict); ok {
        // For method calls
        if e.isMethodCall(expr) {
            // Handle dictionary method calls
            // [implementation]
        }
        
        // For property access
        value, exists := dict.Get(member)
        if exists {
            return value, nil
        }
        
        // For common pseudo-properties
        switch member {
        case "length", "len", "size", "count":
            return float64(dict.Size()), nil
        }
    }
    
    // [...continue with existing code...]
}
```

### 6. Comprehensive Testing

Create a comprehensive test suite to validate all dictionary functionality:

```lisp
# Comprehensive dictionary test
(= test_dict {})
(dict.set test_dict "name" "John")
(dict.set test_dict "age" 30)

# Test basic operations
(print "Dict after creation:" test_dict)
(print "Has key 'name':" (dict.has_key test_dict "name"))
(print "Has key 'email':" (dict.has_key test_dict "email"))
(print "Contains 'name':" (dict.contains? test_dict "name"))

# Test dict.get with defaults
(print "Get existing key:" (dict.get test_dict "name"))
(print "Get missing key with default:" (dict.get test_dict "email" "unknown"))

# Test other methods
(print "Keys:" (dict.keys test_dict))
(print "Values:" (dict.values test_dict))
(print "Items:" (dict.items test_dict))

# Test mutating operations
(dict.update test_dict {"email": "john@example.com"})
(print "After update:" test_dict)

(= popped_value (dict.pop test_dict "age"))
(print "Popped value:" popped_value)
(print "After pop:" test_dict)

(dict.clear test_dict)
(print "After clear:" test_dict)
```

## Implementation Strategy

To fully fix dictionaries, we recommend the following implementation strategy:

1. Fix internal dictionary operations first (Get, Set, Delete, Iterate)
2. Implement missing dictionary methods (has_key, contains?)
3. Fix object protocol integration
4. Improve dictionary serialization and representation
5. Enhance dot notation support for dictionaries
6. Create comprehensive test suite
7. Update documentation to match the implementation

## Expected Benefits

Once these fixes are implemented, dictionaries will:

1. Support all the methods mentioned in documentation
2. Properly integrate with the object protocol for dot notation
3. Be thread-safe with proper locking
4. Have consistent behavior across all operations
5. Work with the various access patterns mentioned in examples
6. Provide better error messages for misuse

## Conclusion

The dictionary implementation in M28 has a solid foundation but requires several fixes to fully meet the documented capabilities. The proposed solutions focus on completing missing functionality, fixing integration with the object protocol, improving safety, and ensuring consistent behavior. These changes will make dictionaries a reliable and powerful feature in the M28 language, similar to Python's dictionary implementation.

## References

1. Python Dictionary Documentation - https://docs.python.org/3/library/stdtypes.html#mapping-types-dict
2. M28 Dictionary Documentation - /docs/features/dictionaries.md
3. M28 Python-Style Dictionaries - /docs/features/python-style-dicts.md