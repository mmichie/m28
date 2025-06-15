# Container Protocols Implementation Plan

## Overview
Implement `__getitem__`, `__setitem__`, `__delitem__`, and `__contains__` protocols to enable custom indexing and container operations for user-defined types.

## Current State
- Indexable protocol is already defined in `/core/protocols/protocols.go`
- Basic indexing works via special forms (`get-item`, `set-item`)
- `__getitem__` is partially supported in `indexing.go`
- Dunder method utilities exist in `/common/types/dunder.go`

## Implementation Tasks

### 1. Extend Dunder Method Utilities
Add to `/common/types/dunder.go`:
- `CallGetItem(obj, key, ctx)` - already exists
- `CallSetItem(obj, key, value, ctx)` - already exists
- `CallDelItem(obj, key, ctx)` - already exists
- Ensure proper error handling and type validation

### 2. Create Indexable Protocol Adapters
In `/core/protocols/indexable.go`:
- `ListIndexable` - adapter for ListValue
- `DictIndexable` - adapter for DictValue
- `TupleIndexable` - adapter for TupleValue (read-only)
- `StringIndexable` - adapter for StringValue (read-only)
- `RangeIndexable` - adapter for RangeValue (read-only)

### 3. Update Indexing Operations
Modify `/eval/indexing.go`:
- Update `GetItemForm` to check protocols first
- Update `SetItemForm` to check protocols first
- Add `DelItemForm` for del operations
- Use three-tier dispatch: dunder → protocol → type-specific

### 4. Integrate with Evaluator
Update dot notation and indexing evaluation:
- Ensure `a[b]` syntax uses GetItemForm
- Ensure `a[b] = c` syntax uses SetItemForm
- Add support for `del a[b]` syntax

### 5. Slice Object Enhancement
- Ensure slice objects work with protocol-based indexing
- Support extended slicing for custom types

### 6. Testing
Create comprehensive tests:
- Protocol adapter tests
- Custom type with indexing support
- Edge cases and error conditions
- Integration with existing types

## Example Usage

```python
# Custom vector type with indexing
class Vector:
    def __init__(self, *components):
        self.data = list(components)
    
    def __getitem__(self, index):
        return self.data[index]
    
    def __setitem__(self, index, value):
        self.data[index] = value
    
    def __delitem__(self, index):
        del self.data[index]
    
    def __len__(self):
        return len(self.data)
    
    def __contains__(self, item):
        return item in self.data

# Usage
v = Vector(1, 2, 3)
print(v[0])     # 1
v[1] = 5        # Sets second element to 5
del v[2]        # Removes third element
print(3 in v)   # False
```

## Migration Strategy

1. Start with read-only operations (`__getitem__`)
2. Add write operations (`__setitem__`, `__delitem__`)
3. Ensure backward compatibility
4. Update documentation with examples

## Expected Benefits

1. **Custom Types**: Enable indexing for user-defined classes
2. **Consistency**: Unified protocol for all indexable types
3. **Extensibility**: Easy to add new container types
4. **Python Compatibility**: Match Python's container protocols